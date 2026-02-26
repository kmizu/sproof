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
  */
object Elaborator:

  /** Name environment for de Bruijn index resolution. Head = most recently bound. */
  private type NameEnv = List[String]

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
              // Also add to global env so later defs can reference it
              val tpeTerm = elabType(retTpe, env, Nil) match
                case Right(t) => t
                case Left(_)  => Term.Meta(-1) // fallback
              env = env.addDef(DefEntry(name, tpeTerm, term))

        case SDecl.SDefspec(name, params, prop, proof) =>
          if defspecs.contains(name) then
            return Left(ElabError(s"Duplicate defspec: $name"))
          val nameEnv = params.reverse.map(_.name)
          // Elaborate param types (non-dependent: each type is in the outer env)
          val elabParamTypes: Either[ElabError, List[(String, Term)]] =
            params.foldLeft[Either[ElabError, List[(String, Term)]]](Right(Nil)) {
              (acc, p) => acc.flatMap { lst =>
                elabType(p.tpe, env, Nil).map(tpe => lst :+ (p.name, tpe))
              }
            }
          elabParamTypes match
            case Left(err) => return Left(err)
            case Right(elabParams) =>
              elabType(prop, env, nameEnv) match
                case Left(err)    => return Left(err)
                case Right(propT) => defspecs = defspecs + (name -> (elabParams, propT, proof))

    Right(ElabResult(env, defs, defspecs))

  // ---- Inductive elaboration ----

  private def elabInductive(
    name:   String,
    params: List[SParam],
    ctors:  List[SCtor],
    env:    GlobalEnv,
  ): IndDef =
    val ctorDefs = ctors.map { ctor =>
      val argTpes = ctor.argParams.map { p =>
        elabType(p.tpe, env, Nil).getOrElse(Term.Meta(-1))
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
    // Validate parameter types
    for p <- params do
      elabType(p.tpe, env, Nil) match
        case Left(err) => return Left(err)
        case _         => ()

    // Build name environment: first param at highest index
    val nameEnv = params.reverse.map(_.name)
    // Also include the function name itself for recursive calls (at outermost)
    val nameEnvWithSelf = nameEnv :+ name
    elabExpr(body, env, nameEnv, name)

  // ---- Type elaboration ----

  private def elabType(tpe: SType, env: GlobalEnv, nameEnv: NameEnv): Either[ElabError, Term] =
    tpe match
      case SType.STVar(name) =>
        // Check if it's a known inductive type
        if env.lookupInd(name).isDefined then
          Right(Term.Ind(name, Nil, Nil))
        // Check if it's a bound variable (in Pi types, etc.)
        else nameEnv.indexOf(name) match
          case -1 => Left(ElabError(s"Unknown type: $name"))
          case i  => Right(Term.Var(i))

      case SType.STApp(fn, arg) =>
        for
          fnT  <- elabType(fn, env, nameEnv)
          argT <- elabType(arg, env, nameEnv)
        yield Term.App(fnT, argT)

      case SType.STArrow(dom, cod) =>
        for
          domT <- elabType(dom, env, nameEnv)
          codT <- elabType(cod, env, "_" :: nameEnv)
        yield Term.Pi("_", domT, codT)

      case SType.STPi(name, dom, cod) =>
        for
          domT <- elabType(dom, env, nameEnv)
          codT <- elabType(cod, env, name :: nameEnv)
        yield Term.Pi(name, domT, codT)

      case SType.STUni(level) =>
        Right(Term.Uni(level))

      case SType.STEq(lhs, rhs) =>
        // Equality proposition: elaborate both sides as expressions.
        // Use 2-arg form: App(App(Ind("Eq",...), lhs), rhs) — type arg is inferred later.
        // We use Ind("Eq",...) as the head; the bidirectional checker has a special case for refl.
        for
          lhsT <- elabExpr(lhs, env, nameEnv, "")
          rhsT <- elabExpr(rhs, env, nameEnv, "")
        yield Term.App(Term.App(Term.Ind("Eq", Nil, Nil), lhsT), rhsT)

  // ---- Expression elaboration ----

  private def elabExpr(
    expr:     SExpr,
    env:      GlobalEnv,
    nameEnv:  NameEnv,
    selfName: String,
  ): Either[ElabError, Term] =
    expr match
      case SExpr.SEVar(name) =>
        // Check local bindings
        nameEnv.indexOf(name) match
          case -1 =>
            // Check global defs
            if env.lookupDef(name).isDefined then
              Right(Term.Var(nameEnv.length)) // will be resolved by Fix or global ref
            else if name == selfName then
              Right(Term.Var(nameEnv.length)) // self-reference placeholder
            else
              Left(ElabError(s"Unknown variable: $name"))
          case i => Right(Term.Var(i))

      case SExpr.SEApp(fn, args) =>
        fn match
          case SExpr.SEVar(name) =>
            // Elaborate function reference
            val fnResult = elabExpr(fn, env, nameEnv, selfName)
            fnResult.flatMap { fnTerm =>
              args.foldLeft(Right(fnTerm): Either[ElabError, Term]) { (acc, arg) =>
                for
                  f <- acc
                  a <- elabExpr(arg, env, nameEnv, selfName)
                yield Term.App(f, a)
              }
            }
          case _ =>
            for
              fnT <- elabExpr(fn, env, nameEnv, selfName)
              result <- args.foldLeft(Right(fnT): Either[ElabError, Term]) { (acc, arg) =>
                for
                  f <- acc
                  a <- elabExpr(arg, env, nameEnv, selfName)
                yield Term.App(f, a)
              }
            yield result

      case SExpr.SELam(params, body) =>
        val newEnv = params.reverse.map(_.name) ++ nameEnv
        for
          bodyT <- elabExpr(body, env, newEnv, selfName)
        yield params.foldRight(bodyT) { (p, acc) =>
          val tpe = elabType(p.tpe, env, nameEnv).getOrElse(Term.Meta(-1))
          Term.Lam(p.name, tpe, acc)
        }

      case SExpr.SECon(typeName, ctorName, args) =>
        // Verify constructor exists
        env.lookupInd(typeName) match
          case None => Left(ElabError(s"Unknown inductive type: $typeName"))
          case Some(indDef) =>
            if !indDef.ctors.exists(_.name == ctorName) then
              Left(ElabError(s"Unknown constructor: $typeName.$ctorName"))
            else
              val argsResult = args.map(elabExpr(_, env, nameEnv, selfName))
              val firstErr = argsResult.collectFirst { case Left(e) => e }
              firstErr match
                case Some(err) => Left(err)
                case None =>
                  Right(Term.Con(ctorName, typeName, argsResult.map(_.toOption.get)))

      case SExpr.SEMatch(scrutinee, cases) =>
        for
          scrT <- elabExpr(scrutinee, env, nameEnv, selfName)
          casesT <- elabMatchCases(cases, env, nameEnv, selfName)
        yield Term.Mat(scrT, casesT, Term.Meta(-1)) // return type inferred later

  private def elabMatchCases(
    cases:    List[SMatchCase],
    env:      GlobalEnv,
    nameEnv:  NameEnv,
    selfName: String,
  ): Either[ElabError, List[MatchCase]] =
    val results = cases.map { mc =>
      // Parse the constructor name (may be "Nat.zero" or "Nat.succ")
      val (typeName, ctorName) = mc.ctor.split('.') match
        case Array(t, c) => (t, c)
        case _           => ("", mc.ctor)

      val bindingCount = mc.bindings.length
      val extendedEnv = mc.bindings.reverse ++ nameEnv

      elabExpr(mc.body, env, extendedEnv, selfName).map { bodyT =>
        MatchCase(ctorName, bindingCount, bodyT)
      }
    }

    val firstErr = results.collectFirst { case Left(e) => e }
    firstErr match
      case Some(err) => Left(err)
      case None      => Right(results.map(_.toOption.get))

end Elaborator
