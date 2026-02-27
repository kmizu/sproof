package sproof

import munit.FunSuite

/** Feature 2: --json flag outputs machine-readable JSON.
  *
  * sproof check --json file.sproof
  *   success → {"ok":true,"inductives":N,"defs":N,"defspecs":N}
  *   failure → {"ok":false,"error":"...sexp...","phase":"proof|parse|elab"}
  */
class JsonOutputSuite extends FunSuite:

  test("processSourceJson: success returns ok:true with counts") {
    val source =
      """|inductive Nat { case zero: Nat  case succ(n: Nat): Nat }
         |def id(n: Nat): Nat = n
         |defspec refl(n: Nat): n = n { by trivial }
         |""".stripMargin
    val json = Main.processSourceJson(source, "t.sproof")
    assert(json.contains("\"ok\":true"),  s"should be ok:true:\n$json")
    assert(json.contains("\"inductives\""), s"should have inductives count:\n$json")
    assert(json.contains("\"defs\""),       s"should have defs count:\n$json")
    assert(json.contains("\"defspecs\""),   s"should have defspecs count:\n$json")
  }

  test("processSourceJson: failure returns ok:false with error") {
    val source =
      """|inductive Nat { case zero: Nat  case succ(n: Nat): Nat }
         |defspec bad(n: Nat): n = Nat.zero { by trivial }
         |""".stripMargin
    val json = Main.processSourceJson(source, "t.sproof")
    assert(json.contains("\"ok\":false"), s"should be ok:false:\n$json")
    assert(json.contains("\"error\""),    s"should have error field:\n$json")
  }

  test("processSourceJson: parse error returns ok:false with phase:parse") {
    val json = Main.processSourceJson("@@@ garbage @@@", "bad.sproof")
    assert(json.contains("\"ok\":false"), s"should be ok:false:\n$json")
    assert(json.contains("\"phase\":\"parse\""), s"should have phase:parse:\n$json")
  }

  test("processSourceJson: proof error returns phase:proof") {
    val source =
      """|inductive Nat { case zero: Nat  case succ(n: Nat): Nat }
         |defspec bad(n: Nat): n = Nat.zero { by trivial }
         |""".stripMargin
    val json = Main.processSourceJson(source, "t.sproof")
    assert(json.contains("\"phase\":\"proof\""), s"should have phase:proof:\n$json")
  }

  test("processSourceJson: inductives count is correct") {
    val source =
      """|inductive Nat { case zero: Nat  case succ(n: Nat): Nat }
         |inductive Bool { case true: Bool  case false: Bool }
         |""".stripMargin
    val json = Main.processSourceJson(source, "t.sproof")
    assert(json.contains("\"inductives\":2"), s"should have 2 inductives:\n$json")
  }

  test("processSourceJson: error field contains proof-state sexp on proof failure") {
    val source =
      """|inductive Nat { case zero: Nat  case succ(n: Nat): Nat }
         |defspec bad(n: Nat): n = Nat.zero { by trivial }
         |""".stripMargin
    val json = Main.processSourceJson(source, "t.sproof")
    assert(json.contains("proof-state"), s"error should contain proof-state:\n$json")
  }

end JsonOutputSuite
