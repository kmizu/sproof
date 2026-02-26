package sproof.syntax

import sproof.core.*

/** Elaboration error. */
case class ElabError(message: String)

/** Result of elaboration: a global environment + function bodies + defspec proofs. */
case class ElabResult(
  env:      GlobalEnv,
  defs:     Map[String, Term],
  /** (params: List[(name, type)], propTerm, proof) — params needed to build the proof context */
  defspecs: Map[String, (List[(String, Term)], Term, SProof)],
)

/** Elaborator: converts surface AST to core terms with De Bruijn indices.
  *
  * Processes declarations in order, building up GlobalEnv incrementally.
  * - SInductive -> IndDef in GlobalEnv
  * - SDef       -> Term body (with de Bruijn indices) in defs map
  * - SDefspec   -> (proposition Term, SProof) in defspecs map
  *
  * Type annotations map: when a param `(inst: Add)` declares a struct type,
  * we record `"inst" -> "Add"` so that `inst.field(args)` can be elaborated
  * as field accessor calls (dictionary passing style).
  */
object Elaborator:

  /** Name environment for de Bruijn index resolution. Head = most recently bound. */
  private type NameEnv = List[String]

  /** Maps variable name to struct name (for dot-notation field access). */
  private type TypeAnns = Map[String, String]

  def elaborate(decls: List[SDecl]): Either[ElabError, ElabResult] =
    var env      = GlobalEnv.empty
    var defs     = Map.empty[String, Term]
    var defspecs = Map.empty[String, (List[(String, Term)], Term, SProof)]

    for decl <- decls do
      decl match
        case SDecl.SInductive(name, params, ctors) =>
          if env.lookupInd(name).isDefined then
            return Left(ElabError(s"Duplicate inductive type: $name"))
          val indDef = elabInductive(name, params, ctors, env)
          env = env.addInd(indDef)

        case SDecl.SDef(name, params, retTpe, body) =>
          if defs.contains(name) then
            return Left(ElabError(s"Duplicate definition: $name"))
          elabDef(name, params, retTpe, body, env) match
            case Left(err)   => return Left(err)
            case Right(term) =>
              defs = defs + (name -> term)
              val fullTpe = term match
                case Term.Fix(_, tpe, _) => tpe
                case _                   => elabType(retTpe, env, Nil).getOrElse(Term.Meta(-1))
              env = env.addDef(DefEntry(name, fullTpe, term))

        case SDecl.SDefspec(name, params, prop, proof) =>
          if defspecs.contains(name) then
            return Left(ElabError(s"Duplicate defspec: $name"))
          val nameEnv = params.reverse.map(_.name)
          val typeAnns = buildTypeAnns(params, env)
          // Elaborate param types (with accumulating context for dependent types)
          val elabParamTypes: Either[ElabError, List[(String, Term)]] =
            params.zipWithIndex.foldLeft[Either[ElabError, List[(String, Term)]]](Right(Nil)) {
              case (acc, (p, i)) => acc.flatMap { lst =>
                val tNameEnv = params.take(i).reverse.map(_.name)
                elabType(p.tpe, env, tNameEnv).map(tpe => lst :+ (p.name, tpe))
              }
            }
          elabParamTypes match
            case Left(err) => return Left(err)
            case Right(elabParams) =>
              elabType(prop, env, nameEnv, typeAnns) match
                case Left(err)    => return Left(err)
                case Right(propT) => defspecs = defspecs + (name -> (elabParams, propT, proof))

        case SDecl.SStructure(name, fields) =>
          if env.lookupInd(name).isDefined || env.lookupStruct(name).isDefined then
            return Left(ElabError(s"Duplicate structure name: $name"))
          elabStructure(name, fields, env) match
            case Left(err) => return Left(err)
            case Right((indDef, structDef, accessors)) =>
              env = env.addInd(indDef).addStruct(structDef)
              for acc <- accessors do
                env = env.addDef(acc)
                defs = defs + (acc.name -> acc.body)

        case SDecl.SInstance(name, structName, bindings) =>
          if defs.contains(name) then
            return Left(ElabError(s"Duplicate definition: $name"))
          elabInstance(name, structName, bindings, env) match
            case Left(err)   => return Left(err)
            case Right(entry) =>
              defs = defs + (name -> entry.body)
              env = env.addDef(entry)

        case SDecl.SOperator(lhsParam, opSymbol, rhsParam, retTpe, body) =>
          if env.lookupOperator(opSymbol).isDefined then
            return Left(ElabError(s"Duplicate operator: $opSymbol"))
          val defName = s"__opr_${mangleOp(opSymbol)}"
          val asDef = SDecl.SDef(defName, List(lhsParam, rhsParam), retTpe, body)
          asDef match
            case SDecl.SDef(n, ps, rt, bd) =>
              elabDef(n, ps, rt, bd, env) match
                case Left(err)   => return Left(err)
                case Right(term) =>
                  val fullTpe = term match
                    case Term.Fix(_, tpe, _) => tpe
                    case _                   => elabType(rt, env, Nil).getOrElse(Term.Meta(-1))
                  val entry = DefEntry(defName, fullTpe, term)
                  defs = defs + (defName -> term)
                  env = env.addDef(entry).addOperator(opSymbol, defName)
            case _ => ()

        case SDecl.SAttr(attr, inner) =>
          // Process the inner decl normally, then if attr == "simp", mark it in env
          val namesBefore = defs.keySet
          // Recurse: elaborate the inner decl
          inner match
            case SDecl.SDef(name, params, retTpe, body) =>
              if defs.contains(name) then
                return Left(ElabError(s"Duplicate definition: $name"))
              elabDef(name, params, retTpe, body, env) match
                case Left(err)   => return Left(err)
                case Right(term) =>
                  defs = defs + (name -> term)
                  val fullTpe = term match
                    case Term.Fix(_, tpe, _) => tpe
                    case _                   => elabType(retTpe, env, Nil).getOrElse(Term.Meta(-1))
                  var entry = DefEntry(name, fullTpe, term)
                  env = env.addDef(entry)
                  if attr == "simp" then env = env.addToSimpSet(name)
            case _ =>
              // For non-def declarations, just process inner (attrs ignored)
              ()

        case SDecl.SCheck(_) =>
          // #check is processed by the CLI, not elaborator; skip here
          ()

    Right(ElabResult(env, defs, defspecs))

  /** Public API: elaborate a surface type given a GlobalEnv and a Context.
    * Builds a nameEnv from the context entries.
    */
  def elabTypePublic(
    tpe: SType,
    env: GlobalEnv,
    ctx: sproof.core.Context,
  ): Either[ElabError, Term] =
    val nameEnv = ctx.entries.map(_.name).toList
    elabType(tpe, env, nameEnv)

  /** Public API: elaborate a surface expression given a GlobalEnv and a nameEnv. */
  def elabExprPublic(
    expr:    SExpr,
    env:     GlobalEnv,
    nameEnv: List[String],
  ): Either[ElabError, Term] =
    elabExpr(expr, env, nameEnv, "")

  // ---- Helpers ----

  /** Build type annotations map from param declarations (for struct field access). */
  private def buildTypeAnns(params: List[SParam], env: GlobalEnv): TypeAnns =
    params.flatMap { p =>
      p.tpe match
        case SType.STVar(typeName) if env.lookupStruct(typeName).isDefined =>
          Some(p.name -> typeName)
        case _ => None
    }.toMap

  // ---- Inductive elaboration ----

  private def elabInductive(
    name:   String,
    params: List[SParam],
    ctors:  List[SCtor],
    env:    GlobalEnv,
  ): IndDef =
    val envWithSelf = env.addInd(IndDef(name, Nil, Nil, 0))
    val ctorDefs = ctors.map { ctor =>
      val argTpes = ctor.argParams.map { p =>
        elabType(p.tpe, envWithSelf, Nil).getOrElse(Term.Meta(-1))
      }
      CtorDef(ctor.name, argTpes)
    }
    IndDef(name, Nil, ctorDefs, 0)

  // ---- Def elaboration ----

  private def elabDef(
    name:   String,
    params: List[SParam],
    retTpe: SType,
    body:   SExpr,
    env:    GlobalEnv,
  ): Either[ElabError, Term] =
    // Validate + elaborate param types with accumulating nameEnv (dependent Pi support).
    // params[i]'s type is elaborated with params[0..i-1] in scope.
    val elabParamTpesE: Either[ElabError, List[Term]] =
      params.zipWithIndex.foldLeft(Right(Nil): Either[ElabError, List[Term]]) {
        case (acc, (p, i)) =>
          val typeNameEnv = params.take(i).reverse.map(_.name)
          acc.flatMap(lst => elabType(p.tpe, env, typeNameEnv).map(t => lst :+ t))
      }

    // Return type is elaborated with all params in scope.
    val retTypeNameEnv = params.reverse.map(_.name)

    // Name environment for the body: params reversed so innermost = lowest index.
    val nameEnv = params.reverse.map(_.name)

    // Type annotations from params (for struct field access via dot notation).
    val typeAnns = buildTypeAnns(params, env)

    for
      bodyT     <- elabExpr(body, env, nameEnv, name, typeAnns)
      retTpeT   <- elabType(retTpe, env, retTypeNameEnv)
      paramTpes <- elabParamTpesE
    yield
      if params.isEmpty then
        bodyT
      else
        val lamsBody = params.zip(paramTpes).foldRight(bodyT) { case ((p, tpe), acc) =>
          Term.Lam(p.name, tpe, acc)
        }
        val fixTpe = params.zip(paramTpes).foldRight(retTpeT) { case ((p, tpe), cod) =>
          Term.Pi(p.name, tpe, cod)
        }
        Term.Fix(name, fixTpe, lamsBody)

  // ---- Type elaboration ----

  private def elabType(
    tpe:      SType,
    env:      GlobalEnv,
    nameEnv:  NameEnv,
    typeAnns: TypeAnns = Map.empty,
  ): Either[ElabError, Term] =
    tpe match
      case SType.STVar(name) =>
        if env.lookupInd(name).isDefined then
          Right(Term.Ind(name, Nil, Nil))
        else nameEnv.indexOf(name) match
          case -1 => Left(ElabError(s"Unknown type: $name"))
          case i  => Right(Term.Var(i))

      case SType.STApp(fn, arg) =>
        for
          fnT  <- elabType(fn, env, nameEnv, typeAnns)
          argT <- elabType(arg, env, nameEnv, typeAnns)
        yield Term.App(fnT, argT)

      case SType.STArrow(dom, cod) =>
        for
          domT <- elabType(dom, env, nameEnv, typeAnns)
          codT <- elabType(cod, env, "_" :: nameEnv, typeAnns)
        yield Term.Pi("_", domT, codT)

      case SType.STPi(name, dom, cod) =>
        for
          domT <- elabType(dom, env, nameEnv, typeAnns)
          codT <- elabType(cod, env, name :: nameEnv, typeAnns)
        yield Term.Pi(name, domT, codT)

      case SType.STUni(level) =>
        Right(Term.Uni(level))

      case SType.STEq(lhs, rhs) =>
        for
          lhsT <- elabExpr(lhs, env, nameEnv, "", typeAnns)
          rhsT <- elabExpr(rhs, env, nameEnv, "", typeAnns)
        yield Term.App(Term.App(Term.Ind("Eq", Nil, Nil), lhsT), rhsT)

  // ---- Expression elaboration ----

  private def elabExpr(
    expr:     SExpr,
    env:      GlobalEnv,
    nameEnv:  NameEnv,
    selfName: String,
    typeAnns: TypeAnns = Map.empty,
  ): Either[ElabError, Term] =
    expr match
      case SExpr.SEVar(name) =>
        nameEnv.indexOf(name) match
          case -1 =>
            if name == selfName then
              Right(Term.Var(nameEnv.length))
            else env.lookupDef(name) match
              case Some(defEntry) => Right(defEntry.body)
              case None           => Left(ElabError(s"Unknown variable: $name"))
          case i => Right(Term.Var(i))

      case SExpr.SEApp(fn, args) =>
        fn match
          case SExpr.SEVar(_) =>
            elabExpr(fn, env, nameEnv, selfName, typeAnns).flatMap { fnTerm =>
              args.foldLeft(Right(fnTerm): Either[ElabError, Term]) { (acc, arg) =>
                for
                  f <- acc
                  a <- elabExpr(arg, env, nameEnv, selfName, typeAnns)
                yield Term.App(f, a)
              }
            }
          case _ =>
            for
              fnT <- elabExpr(fn, env, nameEnv, selfName, typeAnns)
              result <- args.foldLeft(Right(fnT): Either[ElabError, Term]) { (acc, arg) =>
                for
                  f <- acc
                  a <- elabExpr(arg, env, nameEnv, selfName, typeAnns)
                yield Term.App(f, a)
              }
            yield result

      case SExpr.SELam(params, body) =>
        val newEnv = params.reverse.map(_.name) ++ nameEnv
        val newTypeAnns = typeAnns ++ buildTypeAnns(params, env)
        for
          bodyT <- elabExpr(body, env, newEnv, selfName, newTypeAnns)
        yield params.foldRight(bodyT) { (p, acc) =>
          val tpe = elabType(p.tpe, env, nameEnv).getOrElse(Term.Meta(-1))
          Term.Lam(p.name, tpe, acc)
        }

      case SExpr.SECon(varOrTypeName, fieldOrCtorName, args) =>
        env.lookupInd(varOrTypeName) match
          case Some(indDef) =>
            // Constructor application: Type.ctor(args)
            if !indDef.ctors.exists(_.name == fieldOrCtorName) then
              Left(ElabError(s"Unknown constructor: $varOrTypeName.$fieldOrCtorName"))
            else
              val argsResult = args.map(elabExpr(_, env, nameEnv, selfName, typeAnns))
              argsResult.collectFirst { case Left(e) => e } match
                case Some(err) => Left(err)
                case None =>
                  Right(Term.Con(fieldOrCtorName, varOrTypeName, argsResult.map(_.toOption.get)))

          case None =>
            // Dot notation field access: varName.fieldName(args)
            typeAnns.get(varOrTypeName) match
              case None =>
                Left(ElabError(s"Unknown inductive type or struct variable: $varOrTypeName"))
              case Some(structName) =>
                env.lookupStruct(structName) match
                  case None =>
                    Left(ElabError(s"Internal: struct $structName not in env"))
                  case Some(structDef) =>
                    if !structDef.fields.exists(_._1 == fieldOrCtorName) then
                      Left(ElabError(s"Unknown field: $fieldOrCtorName on struct $structName"))
                    else
                      val accessorName = s"${structName}_${fieldOrCtorName}"
                      env.lookupDef(accessorName) match
                        case None =>
                          Left(ElabError(s"Internal: missing accessor $accessorName"))
                        case Some(accEntry) =>
                          // Resolve receiver variable
                          val receiverE: Either[ElabError, Term] =
                            nameEnv.indexOf(varOrTypeName) match
                              case -1 =>
                                env.lookupDef(varOrTypeName) match
                                  case None  => Left(ElabError(s"Unknown variable: $varOrTypeName"))
                                  case Some(d) => Right(d.body)
                              case i  => Right(Term.Var(i))
                          for
                            receiver <- receiverE
                            argTerms <- args.foldLeft(Right(Nil): Either[ElabError, List[Term]]) { (acc, a) =>
                              for lst <- acc; t <- elabExpr(a, env, nameEnv, selfName, typeAnns) yield lst :+ t
                            }
                          yield
                            // App(App(App(accessor, receiver), arg1), arg2, ...)
                            argTerms.foldLeft(Term.App(accEntry.body, receiver): Term)(Term.App.apply)

      case SExpr.SEMatch(scrutinee, cases) =>
        for
          scrT   <- elabExpr(scrutinee, env, nameEnv, selfName, typeAnns)
          casesT <- elabMatchCases(cases, env, nameEnv, selfName, typeAnns)
        yield Term.Mat(scrT, casesT, Term.Meta(-1))

      case SExpr.SInfix(lhs, op, rhs) =>
        env.lookupOperator(op) match
          case None          => Left(ElabError(s"Unknown operator: $op (no operator declaration found)"))
          case Some(defName) =>
            env.lookupDef(defName) match
              case None       => Left(ElabError(s"Internal: operator $op maps to unknown def $defName"))
              case Some(opEntry) =>
                for
                  lT <- elabExpr(lhs, env, nameEnv, selfName, typeAnns)
                  rT <- elabExpr(rhs, env, nameEnv, selfName, typeAnns)
                yield Term.App(Term.App(opEntry.body, lT), rT)

      case SExpr.SEList(elems) =>
        // Desugar [e1, e2, e3] -> cons(e1, cons(e2, cons(e3, nil)))
        // Requires "List" inductive with "nil" and "cons" constructors in scope.
        val nilExpr: SExpr = SExpr.SECon("List", "nil", Nil)
        val desugared = elems.foldRight(nilExpr) { (elem, acc) =>
          SExpr.SECon("List", "cons", List(elem, acc))
        }
        elabExpr(desugared, env, nameEnv, selfName, typeAnns)

      case SExpr.SEInt(n) =>
        elabIntLiteral(n, env)

      case SExpr.SELet(name, value, body) =>
        // let x := value; body  =>  Term.Let(x, inferredType, valueTerm, bodyTerm)
        // We elaborate value first to get the core term, then extend nameEnv for body.
        for
          valueTerm <- elabExpr(value, env, nameEnv, selfName, typeAnns)
          // Extend the name environment: name is now at index 0
          newEnv     = name :: nameEnv
          bodyTerm  <- elabExpr(body, env, newEnv, selfName, typeAnns)
        yield Term.Let(name, Term.Meta(-1), valueTerm, bodyTerm)

  // ---- Structure / Instance / Operator helpers ----

  private def elabStructure(
    name:   String,
    fields: List[SParam],
    env:    GlobalEnv,
  ): Either[ElabError, (IndDef, StructDef, List[DefEntry])] =
    val fieldTypesE: Either[ElabError, List[(String, Term)]] =
      fields.foldLeft[Either[ElabError, List[(String, Term)]]](Right(Nil)) { (acc, p) =>
        acc.flatMap { lst =>
          elabType(p.tpe, env, Nil).map(tpe => lst :+ (p.name, tpe))
        }
      }

    fieldTypesE.map { fieldTypes =>
      val n = fieldTypes.length
      val mkCtor = CtorDef("mk", fieldTypes.map(_._2))
      val indDef = IndDef(name, Nil, List(mkCtor), 0)
      val structDef = StructDef(name, fieldTypes)
      val indTpe = Term.Ind(name, Nil, Nil)
      val accessors = fieldTypes.zipWithIndex.map { case ((fieldName, fieldTpe), i) =>
        val accessorName = s"${name}_${fieldName}"
        val body = Term.Fix(
          accessorName,
          Term.Pi("d", indTpe, fieldTpe),
          Term.Lam("d", indTpe,
            Term.Mat(
              Term.Var(0),
              List(MatchCase("mk", n, Term.Var(n - 1 - i))),
              Term.Meta(-1),
            )
          ),
        )
        DefEntry(accessorName, Term.Pi("d", indTpe, fieldTpe), body)
      }
      (indDef, structDef, accessors)
    }

  private def elabInstance(
    name:       String,
    structName: String,
    bindings:   List[(String, SExpr)],
    env:        GlobalEnv,
  ): Either[ElabError, DefEntry] =
    env.lookupStruct(structName) match
      case None => Left(ElabError(s"Unknown structure: $structName"))
      case Some(structDef) =>
        val fieldNames = structDef.fields.map(_._1)
        val bindingMap = bindings.toMap
        val missing = fieldNames.filterNot(bindingMap.contains)
        if missing.nonEmpty then
          return Left(ElabError(s"Missing fields in instance $name: ${missing.mkString(", ")}"))
        val extra = bindings.map(_._1).filterNot(fieldNames.contains)
        if extra.nonEmpty then
          return Left(ElabError(s"Unknown fields in instance $name: ${extra.mkString(", ")}"))
        val argTermsE: Either[ElabError, List[Term]] =
          fieldNames.foldLeft[Either[ElabError, List[Term]]](Right(Nil)) { (acc, fn) =>
            acc.flatMap { lst =>
              elabExpr(bindingMap(fn), env, Nil, "") match
                case Left(err) => Left(err)
                case Right(t)  => Right(lst :+ t)
            }
          }
        argTermsE.map { argTerms =>
          val instTpe = Term.Ind(structName, Nil, Nil)
          val body    = Term.Con("mk", structName, argTerms)
          DefEntry(name, instTpe, body)
        }

  private def mangleOp(sym: String): String =
    sym.map {
      case '+' => "plus"
      case '*' => "times"
      case '-' => "minus"
      case '/' => "div"
      case c   => c.toString
    }.mkString

  private def elabMatchCases(
    cases:    List[SMatchCase],
    env:      GlobalEnv,
    nameEnv:  NameEnv,
    selfName: String,
    typeAnns: TypeAnns = Map.empty,
  ): Either[ElabError, List[MatchCase]] =
    val results = cases.map { mc =>
      val (typeName, ctorName) = mc.ctor.split('.') match
        case Array(t, c) => (t, c)
        case _           => ("", mc.ctor)
      val bindingCount = mc.bindings.length
      val extendedEnv  = mc.bindings.reverse ++ nameEnv
      elabExpr(mc.body, env, extendedEnv, selfName, typeAnns).map { bodyT =>
        MatchCase(ctorName, bindingCount, bodyT)
      }
    }
    results.collectFirst { case Left(e) => e } match
      case Some(err) => Left(err)
      case None      => Right(results.map(_.toOption.get))

  // ---- Numeric literal elaboration ----

  /** Elaborate an integer literal to Nat/Int constructors.
    *
    * - n >= 0 with only Nat in scope: build nested Nat.succ(..Nat.zero..)
    * - n == 0 with Int in scope: Int.zero
    * - n > 0 with Int in scope: Int.pos(natN-1)
    * - n < 0 with Int in scope: Int.neg(nat|n|-1)
    * - n >= 0 with Nat in scope (Int also present): Nat encoding
    */
  private def elabIntLiteral(n: Int, env: GlobalEnv): Either[ElabError, Term] =
    val hasNat = env.lookupInd("Nat").isDefined
    val hasInt = env.lookupInd("Int").isDefined

    def mkNat(k: Int): Term =
      if k <= 0 then Term.Con("zero", "Nat", Nil)
      else Term.Con("succ", "Nat", List(mkNat(k - 1)))

    if n < 0 then
      if hasInt then
        Right(Term.Con("neg", "Int", List(mkNat(-n - 1))))
      else
        Left(ElabError(s"Negative literal $n requires Int type in scope"))
    else if n == 0 then
      if hasNat then Right(mkNat(0))
      else if hasInt then Right(Term.Con("zero", "Int", Nil))
      else Left(ElabError("Numeric literal 0 requires Nat or Int type in scope"))
    else // n > 0
      if hasNat then Right(mkNat(n))
      else if hasInt then Right(Term.Con("pos", "Int", List(mkNat(n - 1))))
      else Left(ElabError(s"Numeric literal $n requires Nat or Int type in scope"))

end Elaborator
