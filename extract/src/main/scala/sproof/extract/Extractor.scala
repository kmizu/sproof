package sproof.extract

import sproof.core.{Term, GlobalEnv, IndDef, CtorDef, Param}

/** Extracts sproof core terms to Scala 3 source code.
 *
 *  The extractor erases proof-irrelevant information (type universes, propositions)
 *  and generates idiomatic Scala 3 code from the dependently-typed term representation.
 *
 *  Key design decisions:
 *  - `Term.Uni` is erased to `Any` (type universes have no runtime meaning)
 *  - `defspec` (theorem statements) are erased to `Unit` / `()`
 *  - De Bruijn indices are resolved using a `ctx` name list
 *  - Inductive types become Scala 3 `enum` declarations
 *  - `Term.Fix` becomes a `def` with recursion (named by the Fix binder)
 */
/** Names of inductive types that are mapped to Scala built-in types.
 *
 *  These types are NOT emitted as `enum` declarations; instead the
 *  Scala primitive is used directly.  Constructor expressions and
 *  pattern matches are translated to arithmetic.
 *
 *  | sproof        | Scala             |
 *  |---------------|-------------------|
 *  | Int           | Int               |
 *  | Int.zero      | 0                 |
 *  | Int.pos(n)    | n + 1             |
 *  | Int.neg(n)    | -(n + 1)          |
 *  | match (Int)   | if / else if / else |
 */
val builtinInductives: Set[String] = Set("Int")

object Extractor:

  // ---- public API ----

  /** Extract a full sproof program to Scala 3 source. */
  def extractProgram(env: GlobalEnv): String =
    val indParts = env.inductives.values.toList
      .sortBy(_.name)
      .map(extractInductive)
    val defParts = env.defs.values.toList
      .sortBy(_.name)
      .map(d => extractDef(d.name, d.tpe, d.body))
    val runtimeParts =
      if hasDefaultIoShape(env) then List(extractIoRuntime)
      else Nil
    (indParts ++ defParts ++ runtimeParts).mkString("\n\n")

  /** Extract an inductive type definition to a Scala 3 `enum`.
   *
   *  Builtin inductive types (e.g. `Int`) are NOT emitted as enums;
   *  a comment is produced instead and Scala's built-in type is used.
   *
   *  Uniform parameters with `Uni` type become Scala type parameters (e.g. `[A]`).
   *  Index parameters (`IndDef.indices`) are fully erased — they appear neither in the
   *  enum header nor in constructor argument lists.
   *
   *  Example:
   *  {{{
   *    IndDef("Nat", [], [CtorDef("zero",[]), CtorDef("succ",[Nat])], 0)
   *    // produces:
   *    enum Nat:
   *      case Zero
   *      case Succ(n: Nat)
   *
   *    IndDef("Vec", [A:Uni], [nil,cons(Nat,A,Vec[A])], 0, indices=[n:Nat])
   *    // produces:
   *    enum Vec[A]:
   *      case Nil
   *      case Cons(arg0: A, arg1: Vec[A])
   *  }}}
   */
  def extractInductive(indDef: IndDef): String =
    val name = indDef.name
    if builtinInductives.contains(name) then
      s"// $name is mapped to Scala's built-in $name"
    else
      // Params with Uni type become Scala type parameters [A, B, ...]
      val typeParams = indDef.params.collect { case Param(n, Term.Uni(_)) => n }
      val typeParamStr = if typeParams.isEmpty then "" else s"[${typeParams.mkString(", ")}]"
      val header = s"enum $name$typeParamStr:"
      val ctors  = indDef.ctors.map(c => extractCtor(indDef, c, typeParams))
      if ctors.isEmpty then s"$header\n  case Empty"
      else ctors.map(c => s"  $c").mkString(s"$header\n", "\n", "")

  /** Extract a function definition.
   *
   *  Peels off leading `Lam` binders to collect parameters, then renders the body.
   *  Example: `def add(n: Nat)(m: Nat): Nat = n match { ... }`
   */
  def extractDef(name: String, tpe: Term, body: Term): String =
    val (params, retTpe, bodyTerm) = peelLambdas(body, tpe, Nil)
    val paramStr = params.map((n, t) => s"($n: $t)").mkString("")
    val retStr   = termToScalaType(retTpe)
    val ctx      = params.map(_._1)
    val bodyStr  = termToScalaExpr(bodyTerm, ctx)
    s"def $name$paramStr: $retStr = $bodyStr"

  /** Extract a theorem (defspec) — erased to `Unit` at runtime.
   *
   *  Theorem statements carry no computational content, so we erase them.
   */
  def extractDefspec(name: String, params: List[(String, Term)]): String =
    val paramStr = params.map((n, t) => s"($n: ${termToScalaType(t)})").mkString("")
    s"def $name$paramStr: Unit = ()"

  /** Convert a Term to a Scala 3 type expression.
   *
   *  @param t          the type term to convert
   *  @param paramNames names of enclosing inductive type parameters, innermost first.
   *                    Used to resolve `Var(i)` references inside constructor arg types.
   *                    E.g. for Vec[A], paramNames=["A"], so Var(0) → "A".
   */
  def termToScalaType(t: Term, paramNames: List[String] = Nil): String = t match
    case Term.Uni(_)             => "Any"
    case Term.Ind(name, _, _)    => name
    case Term.Var(i)             => paramNames.lift(i).getOrElse(s"T$i")  // resolve or fallback
    case Term.Pi(x, dom, cod)    =>
      if Term.freeIn(0, cod) then
        // Dependent Pi: rendered as generic function type (simplified)
        s"[${sanitizeName(x)} <: ${termToScalaType(dom, paramNames)}] =>> ${termToScalaType(cod, paramNames)}"
      else
        s"${termToScalaType(dom, paramNames)} => ${termToScalaType(cod, paramNames)}"
    case Term.App(f, a)          => s"${termToScalaType(f, paramNames)}[${termToScalaType(a, paramNames)}]"
    case Term.Lam(x, _, b)      => termToScalaType(b, paramNames)   // type-level lambda (erased)
    case Term.Let(_, _, _, b)   => termToScalaType(b, paramNames)
    case Term.Con(_, ind, _)    => ind
    case Term.Fix(name, _, _)   => name
    case Term.Meta(_)            => "Any"
    case Term.Mat(_, _, rt)     => termToScalaType(rt, paramNames)

  /** Convert a Term to a Scala 3 expression.
   *
   *  @param t   the core term to extract
   *  @param ctx name list for De Bruijn resolution; head = index 0 (most recent binder)
   */
  def termToScalaExpr(t: Term, ctx: List[String] = Nil): String = t match
    case Term.Var(i) =>
      ctx.lift(i).getOrElse(s"_v$i")

    case Term.App(f, a) =>
      val fStr = termToScalaExpr(f, ctx)
      val aStr = termToScalaExpr(a, ctx)
      // Detect constructor-style application for nicer output
      s"$fStr($aStr)"

    case Term.Lam(x, _, b) =>
      val safe = sanitizeName(x)
      val bStr = termToScalaExpr(b, safe :: ctx)
      s"($safe => $bStr)"

    case Term.Let(x, _, d, b) =>
      val safe = sanitizeName(x)
      val dStr = termToScalaExpr(d, ctx)
      val bStr = termToScalaExpr(b, safe :: ctx)
      s"{ val $safe = $dStr; $bStr }"

    case Term.Pi(x, _, _) =>
      // Pi terms in expression position are types — erase to Any
      "Any"

    case Term.Uni(_) => "()"  // universe in expression position → unit

    case Term.Ind(name, _, _) => name   // reference to inductive type constructor

    case Term.Con(name, indRef, args) =>
      // Builtin constructors are translated to arithmetic literals.
      if builtinInductives.contains(indRef) then
        extractBuiltinCon(name, indRef, args, ctx)
      else
        val ctorName = pascalCase(name)
        val fullName = s"$indRef.$ctorName"
        if args.isEmpty then fullName
        else
          val argStrs = args.map(termToScalaExpr(_, ctx))
          s"$fullName(${argStrs.mkString(", ")})"

    case Term.Mat(scrutinee, cases, retTpe) =>
      // Check if the scrutinee's type is a builtin inductive.
      val indName = retTpe match
        case Term.Ind(n, _, _) if builtinInductives.contains(n) => Some(n)
        case _ =>
          // Infer from the first case's ctor owning type when retTpe is opaque.
          cases.headOption.flatMap(mc => builtinInductives.find(_ => true).filter(_ =>
            // crude: if ANY case ctor is from a builtin, treat as builtin match
            cases.forall(c => List("zero","pos","neg").contains(c.ctor))
          ))
      indName match
        case Some(ind) => extractBuiltinMatch(scrutinee, cases, ind, ctx)
        case None      => extractMatch(scrutinee, cases, ctx)

    case Term.Fix(name, _, body) =>
      // Fix point: render as a local recursive def
      val safe = sanitizeName(name)
      val bodyStr = termToScalaExpr(body, safe :: ctx)
      s"{ def $safe: Any = $bodyStr; $safe }"

    case Term.Meta(id) => s"???"  // unsolved metavariable

  // ---- private helpers ----

  /** Translate a builtin constructor to a Scala arithmetic expression.
   *
   *  | Constructor      | Scala output  |
   *  |-----------------|---------------|
   *  | Int.zero        | 0             |
   *  | Int.pos(n)      | n + 1         |
   *  | Int.neg(n)      | -(n + 1)      |
   */
  private def extractBuiltinCon(
    name:   String,
    indRef: String,
    args:   List[Term],
    ctx:    List[String],
  ): String = (indRef, name) match
    case ("Int", "zero")          => "0"
    case ("Int", "pos") if args.size == 1 =>
      val n = termToScalaExpr(args.head, ctx)
      s"$n + 1"
    case ("Int", "neg") if args.size == 1 =>
      val n = termToScalaExpr(args.head, ctx)
      s"-($n + 1)"
    case _ =>
      // Fallback: emit as normal constructor
      val ctorName = pascalCase(name)
      val fullName = s"$indRef.$ctorName"
      if args.isEmpty then fullName
      else s"$fullName(${args.map(termToScalaExpr(_, ctx)).mkString(", ")})"

  /** Translate a match on a builtin Int to an if/else chain.
   *
   *  Expects exactly three cases in order: zero, pos(n), neg(n).
   *  Emits:
   *  {{{
   *    if (scrut == 0) body0
   *    else if (scrut > 0) { val n = scrut - 1; bodyP }
   *    else { val n = -scrut - 1; bodyN }
   *  }}}
   */
  private def extractBuiltinMatch(
    scrutinee: Term,
    cases:     List[sproof.core.MatchCase],
    ind:       String,
    ctx:       List[String],
  ): String =
    val sStr = termToScalaExpr(scrutinee, ctx)
    // Partition cases by ctor name (order-insensitive)
    val byName = cases.map(mc => mc.ctor -> mc).toMap
    def arm(mc: sproof.core.MatchCase, binder: String => String): String =
      if mc.bindings == 0 then termToScalaExpr(mc.body, ctx)
      else
        val bindName  = s"_n"
        val extCtx    = bindName :: ctx
        val bodyStr   = termToScalaExpr(mc.body, extCtx)
        s"{ val $bindName = ${binder(sStr)}; $bodyStr }"

    (byName.get("zero"), byName.get("pos"), byName.get("neg")) match
      case (Some(z), Some(p), Some(n)) =>
        val zBody = termToScalaExpr(z.body, ctx)
        val pBody = arm(p, s => s"$s - 1")
        val nBody = arm(n, s => s"-$s - 1")
        s"(if ($sStr == 0) $zBody else if ($sStr > 0) $pBody else $nBody)"
      case _ =>
        // Fallback to normal match if case structure is unexpected
        extractMatch(scrutinee, cases, ctx)

  /** Render a match expression. */
  private def extractMatch(
    scrutinee: Term,
    cases:     List[sproof.core.MatchCase],
    ctx:       List[String],
  ): String =
    val sStr = termToScalaExpr(scrutinee, ctx)
    if cases.isEmpty then s"($sStr match {})"
    else
      val caseStrs = cases.map { mc =>
        val bindNames = (0 until mc.bindings).toList.map(i => s"_arg${mc.bindings - 1 - i}")
        // In the case body, new binders are at the head of ctx (innermost = Var(0))
        val extCtx    = bindNames.reverse ++ ctx
        val bodyStr   = termToScalaExpr(mc.body, extCtx)
        val ctorName  = pascalCase(mc.ctor)
        if bindNames.isEmpty then
          s"    case _.$ctorName => $bodyStr"
        else
          s"    case _.$ctorName(${bindNames.mkString(", ")}) => $bodyStr"
      }
      s"($sStr match {\n${caseStrs.mkString("\n")}\n  })"

  /** Extract a constructor's Scala `case` line for an enum.
   *
   *  Constructor argument names are generated as `arg0`, `arg1`, etc. (after index erasure).
   *  Index args — those whose type equals one of `indDef.indices[i].tpe` — are erased entirely.
   *  Remaining data arg types are resolved using `allParamNames` for Var references.
   *
   *  `allParamNames` mirrors the De Bruijn nameEnv used during elaboration of ctor arg types:
   *    (params ++ indices).map(_.name).reverse
   *  meaning the innermost index param has De Bruijn index 0, then the type params follow.
   *
   *  Example for Vec.cons(m: Nat, head: A, tail: Vec(A, m)):
   *    indDef.params  = [A: Type], indices = [n: Nat]
   *    allParamNames  = ["n", "A"]   (index n = Var(0), type param A = Var(1))
   *    indexTypes     = {Ind("Nat")} → Nat-typed arg m is erased
   *    result: case Cons(arg0: A, arg1: Vec[A])
   */
  private def extractCtor(indDef: IndDef, ctor: CtorDef, typeParamNames: List[String]): String =
    val ctorName   = pascalCase(ctor.name)
    // Full nameEnv for Var resolution — must match what elabInductive uses:
    //   (params ++ indices).map(_.name).reverse  (indices are innermost = lowest index)
    val allParamNames: List[String] = (indDef.params ++ indDef.indices).map(_.name).reverse
    // Index arg types: args whose type matches an index param type are erased
    val indexTypes: Set[Term] = indDef.indices.map(_.tpe).toSet
    val dataArgTpes = ctor.argTpes.filterNot(indexTypes.contains)
    if dataArgTpes.isEmpty then s"case $ctorName"
    else
      val args = dataArgTpes.zipWithIndex.map { (tpe, i) =>
        val argName = s"arg$i"
        val resolved = termToScalaType(tpe, allParamNames)
        val tpeStr = resolved match
          case n if allParamNames.contains(n) && !typeParamNames.contains(n) =>
            // Resolved to an index param name (e.g. "n") — shouldn't appear in types; use Any
            "Any"
          case t => t
        s"$argName: $tpeStr"
      }
      s"case $ctorName(${args.mkString(", ")})"

  /** Peel leading `Lam` binders off `body` collecting (paramName, scalaType) pairs.
   *
   *  Simultaneously peels corresponding `Pi` binders off `tpe` to get the return type.
   *  Returns (params, returnType, innerBody).
   */
  private def peelLambdas(
    body:   Term,
    tpe:    Term,
    params: List[(String, String)],
  ): (List[(String, String)], Term, Term) = (body, tpe) match
    case (Term.Lam(x, paramTpe, b), Term.Pi(_, _, cod)) =>
      val safe     = sanitizeName(x)
      val tpeStr   = termToScalaType(paramTpe)
      peelLambdas(b, cod, params :+ (safe, tpeStr))
    case (Term.Lam(x, paramTpe, b), _) =>
      val safe   = sanitizeName(x)
      val tpeStr = termToScalaType(paramTpe)
      peelLambdas(b, tpe, params :+ (safe, tpeStr))
    case _ =>
      (params, tpe, body)

  /** Convert a sproof name to PascalCase for enum case names.
   *
   *  "zero" → "Zero", "succ" → "Succ", already-Pascal strings unchanged.
   */
  private def pascalCase(s: String): String =
    if s.isEmpty then s
    else s.head.toUpper.toString + s.tail

  /** Sanitize a binder name to a valid Scala identifier. */
  private def sanitizeName(s: String): String =
    if s.isEmpty || s == "_" then "_"
    else s.filter(c => c.isLetterOrDigit || c == '_')
      .ensuring(_.nonEmpty, "_")

  /** Detect the default IO script shape used by stdlib/Effect.sproof. */
  private def hasDefaultIoShape(env: GlobalEnv): Boolean =
    env.lookupInd("IO") match
      case None => false
      case Some(ioDef) =>
        val names = ioDef.ctors.map(_.name).toSet
        names == Set("pure", "read_int", "print_int", "bind")

  /** Runtime interpreter for extracted IO scripts.
   *
   *  Kept outside the kernel and generated only when IO is present.
   */
  private def extractIoRuntime: String =
    """|object IORuntime:
       |  def run(script: IO): Int =
       |    runWith(
       |      script,
       |      () =>
       |        scala.io.StdIn.readLine() match
       |          case null => 0
       |          case s    => s.trim.toInt,
       |      (value: Int) => println(value),
       |    )
       |
       |  def runWith(
       |    script:   IO,
       |    readInt:  () => Int,
       |    printInt: Int => Unit,
       |  ): Int =
       |    script match
       |      case IO.Pure(value) =>
       |        value
       |      case IO.Read_int =>
       |        readInt()
       |      case IO.Print_int(value) =>
       |        printInt(value)
       |        value
       |      case IO.Bind(action, k) =>
       |        val result = runWith(action, readInt, printInt)
       |        runWith(k(result), readInt, printInt)
       |
       |  final case class Trace(
       |    result:          Int,
       |    consumedInputs:  List[Int],
       |    printedValues:   List[Int],
       |    remainingInputs: List[Int],
       |  )
       |
       |  def runWithTrace(script: IO, inputs: List[Int]): Trace =
       |    def eval(
       |      current:    IO,
       |      remaining:  List[Int],
       |      consumedRv: List[Int],
       |      printedRv:  List[Int],
       |    ): (Int, List[Int], List[Int], List[Int]) =
       |      current match
       |        case IO.Pure(value) =>
       |          (value, remaining, consumedRv, printedRv)
       |        case IO.Read_int =>
       |          remaining match
       |            case x :: rest => (x, rest, x :: consumedRv, printedRv)
       |            case Nil       => (0, Nil, consumedRv, printedRv)
       |        case IO.Print_int(value) =>
       |          (value, remaining, consumedRv, value :: printedRv)
       |        case IO.Bind(action, k) =>
       |          val (v, rem1, con1, out1) = eval(action, remaining, consumedRv, printedRv)
       |          eval(k(v), rem1, con1, out1)
       |
       |    val (result, remaining, consumedRv, printedRv) = eval(script, inputs, Nil, Nil)
       |    Trace(result, consumedRv.reverse, printedRv.reverse, remaining)
       |""".stripMargin
