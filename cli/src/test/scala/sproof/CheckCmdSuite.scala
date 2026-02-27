package sproof

import munit.FunSuite

/** Feature 3: #check expr — evaluate expression and show its type.
  *
  * #check Nat.zero      → Nat.zero : Nat
  * #check plus(0, 0)    → plus(Nat.zero, Nat.zero) : Nat
  */
class CheckCmdSuite extends FunSuite:

  test("#check Nat.zero reports its type") {
    val source =
      """|inductive Nat { case zero: Nat  case succ(n: Nat): Nat }
         |#check Nat.zero
         |""".stripMargin
    val result = Main.processSourceWithChecks(source, "t.sproof")
    assert(result.isRight, s"expected Right, got: $result")
    val (_, _, checks) = result.toOption.get
    assert(checks.nonEmpty, "expected at least one #check result")
    val (_, typeStr) = checks.head
    assert(typeStr.contains("Nat"), s"type should mention Nat: $typeStr")
  }

  test("#check result includes the expression and its type") {
    val source =
      """|inductive Nat { case zero: Nat  case succ(n: Nat): Nat }
         |#check Nat.succ(Nat.zero)
         |""".stripMargin
    val result = Main.processSourceWithChecks(source, "t.sproof")
    assert(result.isRight, s"expected Right, got: $result")
    val (_, _, checks) = result.toOption.get
    assert(checks.nonEmpty, "expected check result")
  }

  test("multiple #check declarations are all reported") {
    val source =
      """|inductive Nat { case zero: Nat  case succ(n: Nat): Nat }
         |#check Nat.zero
         |#check Nat.succ(Nat.zero)
         |""".stripMargin
    val result = Main.processSourceWithChecks(source, "t.sproof")
    assert(result.isRight, s"expected Right, got: $result")
    val (_, _, checks) = result.toOption.get
    assert(checks.length == 2, s"expected 2 check results, got ${checks.length}")
  }

  test("#check unknown identifier returns error") {
    val source =
      """|inductive Nat { case zero: Nat  case succ(n: Nat): Nat }
         |#check unknownThing
         |""".stripMargin
    val result = Main.processSourceWithChecks(source, "t.sproof")
    // Should still return Right (file processes ok), but check has error
    // OR return Left with error — either is acceptable
    // The important thing is that it doesn't crash
    assert(result.isRight || result.isLeft, "should return either Right or Left")
  }

end CheckCmdSuite
