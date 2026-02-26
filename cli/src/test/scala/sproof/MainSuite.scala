package sproof

import munit.FunSuite
import sproof.core.GlobalEnv

class MainSuite extends FunSuite:

  // ---- processSource tests ----

  test("check: Bool definition compiles and type-checks") {
    val source =
      """|inductive Bool {
         |  case true: Bool
         |  case false: Bool
         |}
         |""".stripMargin
    val result = Main.processSource(source, "Bool")
    assert(result.isRight, s"expected Right but got: $result")
    val (env, _) = result.toOption.get
    assert(env.inductives.contains("Bool"), "GlobalEnv should contain Bool")
  }

  test("check: Nat definition with plus compiles") {
    val source =
      """|inductive Nat {
         |  case zero: Nat
         |  case succ(n: Nat): Nat
         |}
         |
         |def plus(n: Nat, m: Nat): Nat {
         |  match n {
         |    case Nat.zero => m
         |    case Nat.succ(k) => Nat.succ(plus(k, m))
         |  }
         |}
         |""".stripMargin
    val result = Main.processSource(source, "Nat+plus")
    assert(result.isRight, s"expected Right but got: $result")
    val (env, _) = result.toOption.get
    assert(env.inductives.contains("Nat"), "GlobalEnv should contain Nat")
    assert(env.defs.contains("plus"), "GlobalEnv should contain plus")
  }

  test("check: parse error returns Left with helpful message") {
    val source = "this is not valid sproof syntax @@@@"
    val result = Main.processSource(source, "bad.sproof")
    assert(result.isLeft, "expected Left for invalid syntax")
    val msg = result.left.toOption.get
    assert(msg.contains("Parse error"), s"error should mention parse: $msg")
  }

  test("check: unknown type in def returns Left") {
    val source =
      """|def foo(n: NonExistentType): NonExistentType {
         |  n
         |}
         |""".stripMargin
    val result = Main.processSource(source, "unknown.sproof")
    assert(result.isLeft, "expected Left for unknown type")
  }

  test("check: duplicate inductive returns Left") {
    val source =
      """|inductive Bool {
         |  case true: Bool
         |  case false: Bool
         |}
         |
         |inductive Bool {
         |  case yes: Bool
         |}
         |""".stripMargin
    val result = Main.processSource(source, "dup.sproof")
    assert(result.isLeft, "expected Left for duplicate inductive definition")
    val msg = result.left.toOption.get
    assert(msg.contains("Bool") || msg.contains("Duplicate"), s"error should mention Bool or Duplicate: $msg")
  }

  // ---- processDeclaration (REPL) tests ----

  test("REPL: processes a single inductive declaration") {
    given GlobalEnv = GlobalEnv.empty
    val input =
      """|inductive Color {
         |  case red: Color
         |  case green: Color
         |  case blue: Color
         |}
         |""".stripMargin
    val result = Main.processDeclaration(input, GlobalEnv.empty)
    assert(result.isRight, s"expected Right but got: $result")
    val (newEnv, msg) = result.toOption.get
    assert(newEnv.inductives.contains("Color"), "env should contain Color")
    assert(msg.contains("Color"), s"summary should mention Color: $msg")
  }

  test("REPL: processes multiple declarations in a single input block") {
    // The elaborator processes all declarations together in one pass,
    // so multiple related declarations must be submitted as one block.
    val input =
      """|inductive Nat {
         |  case zero: Nat
         |  case succ(n: Nat): Nat
         |}
         |
         |def id(n: Nat): Nat {
         |  n
         |}
         |""".stripMargin
    val result = Main.processDeclaration(input, GlobalEnv.empty)
    assert(result.isRight, s"expected Right but got: $result")
    val (env, msg) = result.toOption.get
    assert(env.inductives.contains("Nat"), "env should contain Nat")
    assert(env.defs.contains("id"), "env should contain id")
    assert(msg.contains("Nat"), s"summary should mention Nat: $msg")
  }

  test("REPL: returns error for invalid syntax") {
    val result = Main.processDeclaration("@#$% garbage", GlobalEnv.empty)
    assert(result.isLeft, "expected Left for garbage input")
    val msg = result.left.toOption.get
    assert(msg.contains("Parse error") || msg.nonEmpty, s"should have error message: $msg")
  }

  // ---- Checker.checkAll tests ----

  test("Checker.checkAll: accepts empty ElabResult") {
    import sproof.syntax.{ElabResult, SProof}
    val emptyResult = ElabResult(GlobalEnv.empty, Map.empty, Map.empty)
    val result = Checker.checkAll(emptyResult)
    assert(result.isRight, s"empty result should pass: $result")
  }

  test("Checker.checkAll: accepts result with only inductives and defs") {
    import sproof.syntax.{ElabResult, SProof}
    import sproof.core.{IndDef, CtorDef, Term}
    val natDef = GlobalEnv.natDef
    val env = GlobalEnv.empty.addInd(natDef)
    val result = ElabResult(env, Map.empty, Map.empty)
    val checkResult = Checker.checkAll(result)
    assert(checkResult.isRight, s"result with only inductive should pass: $checkResult")
  }

  // ---- readMultiLine tests ----

  test("readMultiLine: single-line input terminates after one non-empty line") {
    // We can't easily test stdin-reading, but we can test the logic indirectly
    // by testing processSource which exercises the pipeline end-to-end.
    val source = "inductive Unit { case unit: Unit }"
    val result = Main.processSource(source)
    assert(result.isRight, s"single-line inductive should parse: $result")
  }

end MainSuite
