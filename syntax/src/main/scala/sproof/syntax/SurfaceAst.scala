package sproof.syntax

/** Surface-level declarations (before elaboration). */
enum SDecl:
  case SInductive(name: String, params: List[SParam], ctors: List[SCtor])
  case SDef(name: String, params: List[SParam], retTpe: SType, body: SExpr)
  case SDefspec(name: String, params: List[SParam], prop: SType, proof: SProof)
  /** Type class / record type: structure Foo { field: Type } */
  case SStructure(name: String, fields: List[SParam])
  /** Type class instance: instance instName: StructName { field = expr } */
  case SInstance(name: String, structName: String, bindings: List[(String, SExpr)])
  /** Typed binary operator (type annotations mandatory):
   *  operator (x: T1) + (y: T2): T3 = body */
  case SOperator(lhsParam: SParam, opSymbol: String, rhsParam: SParam, retTpe: SType, body: SExpr)
  /** #check expr — interactively check/print the type of an expression */
  case SCheck(expr: SExpr)
  /** @[attr] decl — attribute annotation on a declaration (e.g. @[simp]) */
  case SAttr(attr: String, decl: SDecl)

/** Surface parameter (name: type). */
case class SParam(name: String, tpe: SType)

/** Surface constructor. */
case class SCtor(name: String, argParams: List[SParam], retTpe: SType)

/** Surface type expressions. */
enum SType:
  case STVar(name: String)
  case STApp(fn: SType, arg: SType)
  case STArrow(dom: SType, cod: SType)
  case STPi(name: String, dom: SType, cod: SType)
  case STUni(level: Int)
  case STEq(lhs: SExpr, rhs: SExpr)              // a = b (propositional equality)

/** Surface expressions. */
enum SExpr:
  case SEVar(name: String)
  case SEApp(fn: SExpr, args: List[SExpr])
  case SELam(params: List[SParam], body: SExpr)
  case SEMatch(scrutinee: SExpr, cases: List[SMatchCase])
  case SECon(typeName: String, ctorName: String, args: List[SExpr])
  /** Infix operator expression: x + y */
  case SInfix(lhs: SExpr, op: String, rhs: SExpr)
  /** List literal: [e1, e2, e3] — desugars to nested cons/nil in elaborator */
  case SEList(elems: List[SExpr])
  /** Integer literal: 0, 1, 2, -1, -2 — desugars to Nat/Int constructors */
  case SEInt(n: Int)
  /** let x := value; body — local binding (desugars to Term.Let) */
  case SELet(name: String, value: SExpr, body: SExpr)

/** Match case in surface syntax. */
case class SMatchCase(ctor: String, bindings: List[String], body: SExpr)

/** Proof terms / tactics. */
enum SProof:
  case SBy(tactic: STactic)
  case STerm(expr: SExpr)

/** Surface tactics. */
enum STactic:
  case STrivial
  case STriv
  /** rfl — alias for trivial (reflexivity, familiar from Lean/Coq) */
  case SRfl
  case SAssume(names: List[String])
  case SApply(expr: SExpr)
  /** exact e — provide the exact proof term (checked against goal type) */
  case SExact(expr: SExpr)
  case SSimplify(lemmas: List[String])
  case SSimp(lemmas: List[String])
  case SInduction(varName: String, cases: List[STacticCase])
  case SSorry
  /** have h: T = { proof } ; cont_tactic */
  case SHave(name: String, tpe: SType, proof: SProof, cont: STactic)
  /** rewrite [lemma1, lemma2] or rewrite lemma */
  case SRewrite(lemmas: List[String])
  /** rw [...] — alias for rewrite (shorter Lean-style name) */
  case SRw(lemmas: List[String])
  /** calc { step1 step2 ... } */
  case SCalc(steps: List[SCalcStep])
  /** sequence of tactics: t1; t2; t3 (run in order, passing proof state) */
  case SSeq(tactics: List[STactic])

/** A step in a calc block: lhs = rhs { proof }. lhs=None means _ (carry forward). */
case class SCalcStep(lhs: Option[SExpr], rhs: SExpr, proof: SProof)

/** A case in an induction tactic. */
case class STacticCase(ctorName: String, extraBindings: List[String], tactic: STactic)
