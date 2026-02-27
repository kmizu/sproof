package sproof

import munit.FunSuite

/** Feature 4: sorry emits warnings listing all unproved goals. */
class SorryWarnSuite extends FunSuite:

  test("sorry in proof succeeds but returns warning") {
    val source =
      """|inductive Nat { case zero: Nat  case succ(n: Nat): Nat }
         |defspec unfinished(n: Nat): n = n { by sorry }
         |""".stripMargin
    // sorry should still produce a Right (proof "passes") but with a warning
    val result = Main.processSource(source, "sorry.sproof")
    assert(result.isRight, s"sorry proof should succeed (unsoundly): $result")
  }

  test("processSourceWithWarnings: sorry defspec appears in warnings") {
    val source =
      """|inductive Nat { case zero: Nat  case succ(n: Nat): Nat }
         |defspec unfinished(n: Nat): n = n { by sorry }
         |""".stripMargin
    val result = Main.processSourceWithWarnings(source, "sorry.sproof")
    assert(result.isRight, s"expected Right: $result")
    val (_, _, warnings) = result.toOption.get
    assert(warnings.nonEmpty, s"expected sorry warning, got empty warnings")
    assert(
      warnings.exists(_.contains("sorry") || warnings.exists(_.contains("unfinished"))),
      s"warning should mention 'sorry' or defspec name: $warnings"
    )
  }

  test("processSourceWithWarnings: no-sorry proof has no warnings") {
    val source =
      """|inductive Nat { case zero: Nat  case succ(n: Nat): Nat }
         |defspec refl(n: Nat): n = n { by trivial }
         |""".stripMargin
    val result = Main.processSourceWithWarnings(source, "clean.sproof")
    assert(result.isRight, s"expected Right: $result")
    val (_, _, warnings) = result.toOption.get
    assert(warnings.isEmpty, s"expected no warnings for clean proof, got: $warnings")
  }

  test("processSourceWithWarnings: multiple sorry defspecs all warned") {
    val source =
      """|inductive Nat { case zero: Nat  case succ(n: Nat): Nat }
         |defspec s1(n: Nat): n = n { by sorry }
         |defspec s2(n: Nat): n = n { by sorry }
         |""".stripMargin
    val result = Main.processSourceWithWarnings(source, "multi-sorry.sproof")
    assert(result.isRight, s"expected Right: $result")
    val (_, _, warnings) = result.toOption.get
    assert(warnings.length >= 2, s"expected 2 warnings, got: $warnings")
  }

end SorryWarnSuite
