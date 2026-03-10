package sroof.syntax

/** Surface-level declarations (before elaboration). */
enum SDecl:
  case SInductive(name: String, params: List[SParam], ctors: List[SCtor], indices: List[SParam] = Nil)
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
  case STExpr(expr: SExpr)                       // value expression used as type argument (e.g. Regex.eps in Accept(Regex.eps, s))

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
  /** Type ascription: (e : T) — explicit type annotation */
  case SEAscr(expr: SExpr, tpe: SType)
  /** if P then e1 else e2 — conditional expression */
  case SEIf(cond: SExpr, thenBr: SExpr, elseBr: SExpr)

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
  /** try t — attempt tactic t, succeed even if t fails */
  case STry(tactic: STactic)
  /** first | t1 | t2 | t3 — try each tactic in order, take first success */
  case SFirst(tactics: List[STactic])
  /** repeat t — repeat t until it fails */
  case SRepeat(tactic: STactic)
  /** all_goals t — apply tactic to all remaining goals */
  case SAllGoals(tactic: STactic)
  /** skip — no-op tactic */
  case SSkip
  /** assumption — find a hypothesis in context that matches goal exactly */
  case SAssumption
  /** contradiction — find False or contradictory hypotheses */
  case SContradiction
  /** cases x { case c1 => t1 ... } — case split without induction hypothesis */
  case SCases(varName: String, cases: List[STacticCase])
  /** decide — close a goal by boolean computation (NbE evaluation to Bool.true) */
  case SDecide
  /** split / constructor — split a single-constructor goal into one subgoal per field */
  case SSplit
  case SConstructor
  /** left — apply the first constructor (for disjunctions / Either) */
  case SLeft
  /** right — apply the second constructor */
  case SRight
  /** use e / exists e — provide a witness for the first constructor argument */
  case SUse(expr: SExpr)
  case SExists(expr: SExpr)
  /** by_contra h — introduce the negation of the goal as hypothesis h */
  case SByContra(varName: String)
  /** tauto — close a propositional goal by trying trivial, assumption, contradiction */
  case STauto
  /** obtain [a, b] := h ; cont — destruct a single-constructor hypothesis */
  case SObtain(bindings: List[String], hypName: String, cont: STactic)
  /** specialize h arg1 arg2 ; cont — apply a hypothesis to arguments */
  case SSpecialize(hypName: String, args: List[SExpr], cont: STactic)

/** A step in a calc block: lhs = rhs { proof }. lhs=None means _ (carry forward). */
case class SCalcStep(lhs: Option[SExpr], rhs: SExpr, proof: SProof)

/** A case in an induction tactic. */
case class STacticCase(ctorName: String, extraBindings: List[String], tactic: STactic)
