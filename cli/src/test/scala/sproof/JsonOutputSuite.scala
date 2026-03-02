package sproof

import munit.FunSuite

/** Schema v2 regression tests for `check --json`. */
class JsonOutputSuite extends FunSuite:

  test("v2 success snapshot is stable"):
    val source =
      """|inductive Nat { case zero: Nat  case succ(n: Nat): Nat }
         |def id(n: Nat): Nat = n
         |defspec refl(n: Nat): n = n { by trivial }
         |""".stripMargin
    val json = Main.processSourceJson(source, "t.sproof")
    val expected =
      """{"schemaVersion":2,"ok":true,"phase":"check","file":"t.sproof","result":{"inductives":1,"defs":1,"defspecs":1},"warnings":[],"sorryDiagnostics":[],"diagnostics":[],"checks":[],"error":null}"""
    assertEquals(json, expected)

  test("v2 warning snapshot includes structured warning object"):
    val source =
      """|inductive Nat { case zero: Nat  case succ(n: Nat): Nat }
         |defspec unfinished(n: Nat): n = n { by sorry }
         |""".stripMargin
    val json = Main.processSourceJson(source, "sorry.sproof")
    val expected =
      """{"schemaVersion":2,"ok":true,"phase":"check","file":"sorry.sproof","result":{"inductives":1,"defs":0,"defspecs":1},"warnings":[{"severity":"warning","code":"sorry","message":"warning: 'unfinished' uses sorry (1 occurrence(s)) — proof is unsound"}],"sorryDiagnostics":[{"code":"sorry.direct","defspec":"unfinished","transitive":false,"occurrences":1,"dependsOn":[],"message":"warning: 'unfinished' uses sorry (1 occurrence(s)) — proof is unsound"}],"diagnostics":[],"checks":[],"error":null}"""
    assertEquals(json, expected)

  test("v2 parse error has structured diagnostics"):
    val json = Main.processSourceJson("@@@ garbage @@@", "bad.sproof")
    assert(json.contains("\"schemaVersion\":2"), s"must include schemaVersion:\n$json")
    assert(json.contains("\"ok\":false"), s"must be failure:\n$json")
    assert(json.contains("\"phase\":\"parse\""), s"must be parse-phase failure:\n$json")
    assert(json.contains("\"diagnostics\":"), s"must include diagnostics array:\n$json")
    assert(json.contains("\"result\":null"), s"must include nullable result field:\n$json")

  test("v2 proof error keeps proof-state text in error payload"):
    val source =
      """|inductive Nat { case zero: Nat  case succ(n: Nat): Nat }
         |defspec bad(n: Nat): n = Nat.zero { by trivial }
         |""".stripMargin
    val json = Main.processSourceJson(source, "bad-proof.sproof")
    assert(json.contains("\"schemaVersion\":2"), s"must include schemaVersion:\n$json")
    assert(json.contains("\"phase\":\"proof\""), s"must have proof phase:\n$json")
    assert(json.contains("proof-state"), s"proof failure should include proof-state details:\n$json")

  test("v2 includes #check results as checks array"):
    val source =
      """|inductive Nat { case zero: Nat  case succ(n: Nat): Nat }
         |#check Nat.zero
         |""".stripMargin
    val json = Main.processSourceJson(source, "checks.sproof")
    assert(json.contains("\"checks\":["), s"checks array must exist:\n$json")
    assert(json.contains("\"ok\":true"), s"check entry should succeed:\n$json")
    assert(json.contains("\"type\":\"Nat\""), s"check entry should contain inferred type:\n$json")

  test("processSourceJson: includes machine-readable sorry diagnostics") {
    val source =
      """|inductive Nat { case zero: Nat  case succ(n: Nat): Nat }
         |defspec unfinished(n: Nat): n = n { by sorry }
         |""".stripMargin
    val json = Main.processSourceJson(source, "sorry.sproof")
    assert(json.contains("\"ok\":true"), s"warning mode should still succeed:\n$json")
    assert(json.contains("\"warnings\""), s"should include warnings:\n$json")
    assert(json.contains("\"sorryDiagnostics\""), s"should include sorryDiagnostics:\n$json")
    assert(json.contains("\"code\":\"sorry.direct\""), s"should classify direct sorry:\n$json")
  }

  test("processSourceJson: fail-on-sorry returns policy error") {
    val source =
      """|inductive Nat { case zero: Nat  case succ(n: Nat): Nat }
         |defspec unfinished(n: Nat): n = n { by sorry }
         |""".stripMargin
    val json = Main.processSourceJson(source, "sorry.sproof", failOnSorry = true)
    assert(json.contains("\"ok\":false"), s"fail mode must fail:\n$json")
    assert(json.contains("\"phase\":\"policy\""), s"phase should be policy:\n$json")
    assert(json.contains("\"sorryDiagnostics\""), s"policy error should retain diagnostics:\n$json")
  }

end JsonOutputSuite
