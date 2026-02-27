package sproof

import munit.FunSuite
import sproof.tactic.TacticError

/** Feature 5: TacticError.hint — contextual hints for LLM proof repair.
  *
  * Each TacticError case returns a human/LLM-readable hint suggesting
  * what to try next.
  */
class TacticHintSuite extends FunSuite:

  test("TacticError.Custom has hint method") {
    val err = TacticError.Custom("trivial: not definitionally equal: x ≢ y")
    // hint is Option[String]
    val hint: Option[String] = err.hint
    // For a trivial failure, hint should suggest simplify
    assert(hint.isDefined, s"Custom trivial error should have a hint")
    assert(
      hint.get.contains("simplify") || hint.get.contains("simp"),
      s"hint should mention simplify: ${hint.get}"
    )
  }

  test("TacticError.NoGoals has hint") {
    val err = TacticError.NoGoals
    val hint = err.hint
    assert(hint.isDefined, "NoGoals should have a hint")
  }

  test("TacticError.NotAnEquality has hint") {
    val err = TacticError.NotAnEquality("Nat")
    val hint = err.hint
    assert(hint.isDefined, "NotAnEquality should have a hint")
    assert(
      hint.get.contains("=") || hint.get.contains("equality"),
      s"hint should mention equality: ${hint.get}"
    )
  }

  test("TacticError.NotAPi has hint") {
    val err = TacticError.NotAPi("Nat")
    val hint = err.hint
    assert(hint.isDefined, "NotAPi should have a hint")
  }

  test("hint appears in proof-state sexp error output") {
    val source =
      """|inductive Nat { case zero: Nat  case succ(n: Nat): Nat }
         |def plus(n: Nat, m: Nat): Nat {
         |  match n {
         |    case Nat.zero => m
         |    case Nat.succ(k) => Nat.succ(plus(k, m))
         |  }
         |}
         |defspec bad(n: Nat): plus(n, Nat.zero) = n { by trivial }
         |""".stripMargin
    val result = Main.processSource(source, "hint-test.sproof")
    assert(result.isLeft, "expected failure")
    val msg = result.left.toOption.get
    // The sexp should contain a (hint ...) field
    assert(msg.contains("(hint"), s"proof-state sexp should include (hint ...):\n$msg")
  }

end TacticHintSuite
