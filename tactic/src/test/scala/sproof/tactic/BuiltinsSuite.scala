package sproof.tactic

import sproof.core.{Term, Context, Param, Ctor}
import munit.FunSuite

/** Tests for built-in tactics: trivial, assume, apply.
 *
 *  TDD: tests written before implementation.
 *  Philosophy: "defspec ... program = { ... }" — a proof program that type-checks
 *  is a valid proof. Type error = proof failure.
 */
class BuiltinsSuite extends FunSuite:

  val empty: Context = Context.empty

  // Helper: run a tactic proving a goal
  def prove(ctx: Context, target: Term)(t: TacticM[Unit]): Either[TacticError, Term] =
    TacticM.prove(ctx, target)(t)

  // ======== trivial ========

  test("trivial closes Eq(T, a, a) — reflexivity"):
    // Goal: Eq Type1 Type0 Type0
    val goal = Eq.mkType(Term.Uni(1), Term.Uni(0), Term.Uni(0))
    val result = prove(empty, goal)(Builtins.trivial)
    assert(result.isRight, s"Expected Right, got $result")

  test("trivial closes Eq after beta-reduction"):
    // LHS = (λx:Type0. x) Type0, which beta-reduces to Type0
    val lhs  = Term.App(Term.Lam("x", Term.Uni(0), Term.Var(0)), Term.Uni(0))
    val goal = Eq.mkType(Term.Uni(1), lhs, Term.Uni(0))
    val result = prove(empty, goal)(Builtins.trivial)
    assert(result.isRight, s"Expected Right after beta: $result")

  test("trivial fails on Eq(T, a, b) where a ≢ b"):
    val goal = Eq.mkType(Term.Uni(2), Term.Uni(0), Term.Uni(1))
    val result = prove(empty, goal)(Builtins.trivial)
    assert(result.isLeft, s"Expected Left on non-trivial equality, got $result")

  test("trivial fails on non-equality goal"):
    val result = prove(empty, Term.Uni(0))(Builtins.trivial)
    assert(result.isLeft, "trivial should fail on a non-equality goal")

  test("trivial proof term is Con(refl, ...)"):
    val goal = Eq.mkType(Term.Uni(1), Term.Uni(0), Term.Uni(0))
    val result = prove(empty, goal)(Builtins.trivial)
    result match
      case Right(term) =>
        assert(Eq.isRefl(term), s"Expected refl term, got $term")
      case Left(err) => fail(s"Expected Right, got $err")

  // ======== assume ========

  test("assume on Pi goal introduces binder into context"):
    // Goal: Pi(x: Type0, Type0)  →  after assume "x", goal is Type0 with x:Type0 in ctx
    val piGoal = Term.Pi("x", Term.Uni(0), Term.Uni(0))
    val result = prove(empty, piGoal) {
      for
        _       <- Builtins.assume("x")
        gp      <- TacticM.currentGoal
        (mv, _)  = gp
        // After assume "x": ctx has x:Type0, goal is Type0
        // Var(0) = x : Type0 — correctly typed in extended context
        _       <- TacticM.solveGoalWith(mv, Term.Var(0))
      yield ()
    }
    assert(result.isRight, s"assume on Pi should succeed: $result")

  test("assume on non-Pi goal fails"):
    val result = prove(empty, Term.Uni(0))(Builtins.assume("x"))
    assert(result.isLeft, "assume should fail on non-Pi goal")

  test("assume multiple names peels off multiple Pi binders"):
    // Goal: Pi(x: Type0, Pi(y: Type0, Type0))
    val goal = Term.Pi("x", Term.Uni(0), Term.Pi("y", Term.Uni(0), Term.Uni(0)))
    val result = prove(empty, goal) {
      for
        _       <- Builtins.assume("x")
        _       <- Builtins.assume("y")
        gp      <- TacticM.currentGoal
        (mv, _)  = gp
        // After assume "x" then "y": ctx has y:Type0, x:Type0; goal is Type0
        // Var(0) = y : Type0
        _       <- TacticM.solveGoalWith(mv, Term.Var(0))
      yield ()
    }
    assert(result.isRight, s"Double assume should succeed: $result")

  // ======== apply ========

  test("apply f : A→B on goal B produces subgoal A"):
    // Context: f : Type0 → Type0; Goal: Type0
    // After apply(f), new goal is Type0 (the domain)
    val fTpe = Term.Pi("_", Term.Uni(0), Term.Uni(0))
    val ctx  = empty.extend("f", fTpe)
    val f    = Term.Var(0)  // f in context

    val result = prove(ctx, Term.Uni(0)) {
      for
        _       <- Builtins.apply_(f)
        // Subgoal: Type0 (the argument type)
        gp      <- TacticM.currentGoal
        (mv, g)  = gp
        _        = assertEquals(g.target, Term.Uni(0), "Subgoal should be the domain")
        _       <- TacticM.solveGoalWith(mv, Term.Uni(0))
      yield ()
    }
    assert(result.isRight, s"apply should succeed: $result")

  test("apply fails if codomain doesn't match goal"):
    // f : Type0 → Type1, goal is Type0 (not Type1)
    val fTpe = Term.Pi("_", Term.Uni(0), Term.Uni(1))
    val ctx  = empty.extend("f", fTpe)
    val f    = Term.Var(0)
    val result = prove(ctx, Term.Uni(0))(Builtins.apply_(f))
    assert(result.isLeft, "apply with wrong codomain should fail")

  test("apply fails if term is not a function"):
    // f : Type0, goal is Type0
    val ctx = empty.extend("f", Term.Uni(0))
    val f   = Term.Var(0)
    val result = prove(ctx, Term.Uni(0))(Builtins.apply_(f))
    assert(result.isLeft, "apply on non-function should fail")

  // ======== triv (alias for trivial) ========

  test("triv is an alias for trivial"):
    val goal = Eq.mkType(Term.Uni(1), Term.Uni(0), Term.Uni(0))
    val r1   = prove(empty, goal)(Builtins.trivial)
    val r2   = prove(empty, goal)(Builtins.triv)
    assertEquals(r1.isRight, r2.isRight)

  // ======== negative: proofs that must NOT go through ========

  test("SOUNDNESS: trivial must not prove distinct types equal"):
    // Type0 ≠ Type1 — trivial must reject this
    val unsound = Eq.mkType(Term.Uni(2), Term.Uni(0), Term.Uni(1))
    val result  = prove(empty, unsound)(Builtins.trivial)
    assert(result.isLeft, "Trivial must not prove false equality!")

  test("SOUNDNESS: assume must not work on concrete (non-Pi) type"):
    val result = prove(empty, Term.Uni(0))(Builtins.assume("x"))
    assert(result.isLeft, "assume must not close a Uni goal!")

  // ======== rewrite ========

  test("rewrite with hypothesis matching goal directly"):
    // Context: h : Eq(Type1, Type0, Type0)
    // Goal: Eq(Type1, Type0, Type0)
    // rewrite [h] should close the goal by using h directly
    given sproof.core.GlobalEnv = sproof.core.GlobalEnv.empty
    val eqType = Eq.mkType(Term.Uni(1), Term.Uni(0), Term.Uni(0))
    val ctx    = empty.extend("h", eqType)
    val result = prove(ctx, eqType)(Builtins.rewrite(List("h")))
    assert(result.isRight, s"rewrite should close matching goal: $result")

  test("rewrite fails when lemma not found"):
    given sproof.core.GlobalEnv = sproof.core.GlobalEnv.empty
    val goal = Eq.mkType(Term.Uni(1), Term.Uni(0), Term.Uni(0))
    val result = prove(empty, goal)(Builtins.rewrite(List("nonexistent")))
    assert(result.isLeft, "rewrite with unknown lemma should fail")

  test("rewrite fails when lemma is not an equality"):
    given sproof.core.GlobalEnv = sproof.core.GlobalEnv.empty
    val ctx    = empty.extend("h", Term.Uni(0))
    val goal   = Eq.mkType(Term.Uni(1), Term.Uni(0), Term.Uni(0))
    val result = prove(ctx, goal)(Builtins.rewrite(List("h")))
    assert(result.isLeft, "rewrite with non-equality lemma should fail")

  test("simplify uses priority ordering from rewrite DB specs"):
    given sproof.core.GlobalEnv = sproof.core.GlobalEnv.empty
    val eqType = Eq.mkType(Term.Uni(1), Term.Uni(0), Term.Uni(0))
    // Context indices: hHigh = Var(0), hLow = Var(1)
    val ctx = empty.extend("hLow", eqType).extend("hHigh", eqType)
    val result = prove(ctx, eqType)(Builtins.simplify(List("hLow__p1", "hHigh__p10")))
    result match
      case Right(term) => assertEquals(term, Term.Var(0))
      case Left(err)   => fail(s"expected successful simplify, got $err")

  test("simplify supports backward direction with __rev for direct symmetry"):
    given sproof.core.GlobalEnv = sproof.core.GlobalEnv.empty
    val nat   = Term.Ind("Nat", Nil, Nil)
    val zero  = Term.Con("zero", "Nat", Nil)
    val one   = Term.Con("succ", "Nat", List(zero))
    val eqFwd = Eq.mkType(nat, zero, one)
    val goal  = Eq.mkType(nat, one, zero)
    val ctx   = empty.extend("h", eqFwd)
    val result = prove(ctx, goal)(Builtins.simplify(List("h__rev")))
    assert(result.isRight, s"expected backward rewrite to close direct symmetric goal: $result")
