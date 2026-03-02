package sproof.kernel

import sproof.core.{Term, Context, Param, Ctor, MatchCase}
import sproof.tactic.{Eq, TacticM, Builtins}
import munit.FunSuite

/** Tests for the trusted kernel.
 *
 *  The kernel is the ONLY trusted component.  Every completed proof goes
 *  through `Kernel.check` before being accepted.  Tests ensure:
 *  1. Valid proofs pass
 *  2. Invalid / unsound proofs are rejected
 */
class KernelSuite extends FunSuite:

  val empty: Context = Context.empty

  // --- basic well-typed terms ---

  test("Uni(0) : Uni(1) — universes type-check"):
    assert(Kernel.check(empty, Term.Uni(0), Term.Uni(1)).isRight)

  test("identity function has correct Pi type"):
    val id = Term.Lam("x", Term.Uni(0), Term.Var(0))
    val tp = Term.Pi("x", Term.Uni(0), Term.Uni(0))
    assert(Kernel.check(empty, id, tp).isRight)

  test("refl term has equality type"):
    // refl(Type0) : Eq Type1 Type0 Type0
    val refl  = Eq.mkRefl(Term.Uni(0))
    val eqTpe = Eq.mkType(Term.Uni(1), Term.Uni(0), Term.Uni(0))
    assert(Kernel.check(empty, refl, eqTpe).isRight)

  // --- defspec validation (proving = type-checking the proof program) ---

  test("trivial proof of reflexivity type-checks in kernel"):
    // Goal: Eq Type1 Type0 Type0
    val eqTpe = Eq.mkType(Term.Uni(1), Term.Uni(0), Term.Uni(0))
    val proofTerm = TacticM.prove(empty, eqTpe)(Builtins.trivial).getOrElse(fail("tactic failed"))
    assert(Kernel.check(empty, proofTerm, eqTpe).isRight, "proof term must type-check in kernel")

  test("assume proof of Pi type type-checks in kernel"):
    // Goal: Pi(x: Type0, Type0) — identity function
    // After assume "x", goal is Type0 in ctx[x:Type0].
    // Solve with Var(0) (= x), which has type Type0 in that context.
    // Expected proof: λx:Type0. x
    val piTpe = Term.Pi("x", Term.Uni(0), Term.Uni(0))
    val proofTerm = TacticM.prove(empty, piTpe) {
      for
        _       <- Builtins.assume("x")
        gp      <- TacticM.currentGoal
        (mv, _)  = gp
        // Var(0) = x : Type0 — correctly typed in the extended context
        _       <- TacticM.solveGoalWith(mv, Term.Var(0))
      yield ()
    }.getOrElse(fail("tactic failed"))
    // Expected proof term: λx:Type0. x
    assertEquals(proofTerm, Term.Lam("x", Term.Uni(0), Term.Var(0)))
    assert(Kernel.check(empty, proofTerm, piTpe).isRight)

  // --- negative: ill-typed terms must be rejected ---

  test("SOUNDNESS: refl on a ≠ b is rejected"):
    // refl(Type0) should NOT have type Eq Type2 Type0 Type1
    val refl      = Eq.mkRefl(Term.Uni(0))
    val falseTpe  = Eq.mkType(Term.Uni(2), Term.Uni(0), Term.Uni(1))
    assert(Kernel.check(empty, refl, falseTpe).isLeft, "Kernel must reject false equality proof!")

  test("SOUNDNESS: wrong universe level is rejected"):
    // Type1 does NOT have type Type0
    assert(Kernel.check(empty, Term.Uni(1), Term.Uni(0)).isLeft)

  test("SOUNDNESS: unbound variable is rejected"):
    // Var(0) in empty context
    assert(Kernel.check(empty, Term.Var(0), Term.Uni(0)).isLeft)

  test("SOUNDNESS: De Bruijn escape in lambda body is rejected"):
    // λx:Type0. Var(1) tries to reference an out-of-scope variable.
    val badProof = Term.Lam("x", Term.Uni(0), Term.Var(1))
    val claim    = Term.Pi("x", Term.Uni(0), Term.Uni(0))
    assert(Kernel.check(empty, badProof, claim).isLeft)

  test("SOUNDNESS: equality universe witness mismatch is rejected"):
    // Eq Type0 Type0 Type0 is ill-typed because Type0 : Type1, not Type0.
    val badEqType = Eq.mkType(Term.Uni(0), Term.Uni(0), Term.Uni(0))
    val refl      = Eq.mkRefl(Term.Uni(0))
    assert(Kernel.check(empty, refl, badEqType).isLeft)

  test("SOUNDNESS: downward cumulativity is rejected in kernel verification"):
    // Type2 does NOT inhabit Type1.
    assert(Kernel.check(empty, Term.Uni(2), Term.Uni(1)).isLeft)

  // --- kernel is independent of checker internals ---

  test("kernel re-checks: does not trust tactic output blindly"):
    // Manufacture a 'proof' that wouldn't type-check: just Uni(0)
    // Claim it proves Eq Type1 Type0 Type1 (false)
    val falseTpe = Eq.mkType(Term.Uni(1), Term.Uni(0), Term.Uni(1))
    assert(Kernel.check(empty, Term.Uni(0), falseTpe).isLeft)
