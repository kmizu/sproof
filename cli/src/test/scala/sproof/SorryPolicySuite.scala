package sproof

import munit.FunSuite

class SorryPolicySuite extends FunSuite:

  test("parseCheckOptions: default warning mode without flags"):
    val parsed = Main.parseCheckOptions(List("examples/nat.sproof"))
    assertEquals(
      parsed,
      Right(Main.CheckCliOptions("examples/nat.sproof", json = false, failOnSorry = false)),
    )

  test("parseCheckOptions: accepts --json and --fail-on-sorry in any order"):
    val a = Main.parseCheckOptions(List("--json", "--fail-on-sorry", "f.sproof"))
    val b = Main.parseCheckOptions(List("--fail-on-sorry", "--json", "f.sproof"))
    val expected = Right(Main.CheckCliOptions("f.sproof", json = true, failOnSorry = true))
    assertEquals(a, expected)
    assertEquals(b, expected)

  test("parseCheckOptions: unknown option is rejected"):
    val parsed = Main.parseCheckOptions(List("--unknown", "f.sproof"))
    assert(parsed.isLeft, s"expected parse failure, got: $parsed")

  test("processSourceJson: fail-on-sorry keeps clean proof successful"):
    val source =
      """|inductive Nat { case zero: Nat  case succ(n: Nat): Nat }
         |defspec refl(n: Nat): n = n { by trivial }
         |""".stripMargin
    val json = Main.processSourceJson(source, "clean.sproof", failOnSorry = true)
    assert(json.contains("\"ok\":true"), s"clean proofs must pass in fail mode:\n$json")
    assert(json.contains("\"sorryDiagnostics\":[]"), s"clean proof should have no sorry diagnostics:\n$json")

