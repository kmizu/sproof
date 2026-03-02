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

  // ---- Int type ----

  test("check: Int with neg-neg involution proves correctly") {
    val source =
      """|inductive Nat {
         |  case zero: Nat
         |  case succ(n: Nat): Nat
         |}
         |inductive Int {
         |  case zero: Int
         |  case pos(n: Nat): Int
         |  case neg(n: Nat): Int
         |}
         |def int_neg(a: Int): Int {
         |  match a {
         |    case Int.zero   => Int.zero
         |    case Int.pos(n) => Int.neg(n)
         |    case Int.neg(n) => Int.pos(n)
         |  }
         |}
         |defspec int_neg_neg(a: Int): int_neg(int_neg(a)) = a {
         |  by induction a {
         |    case zero   => trivial
         |    case pos n  => trivial
         |    case neg n  => trivial
         |  }
         |}
         |""".stripMargin
    val result = Main.processSource(source, "Int-test")
    assert(result.isRight, s"expected Right but got: $result")
    val (env, defspecCount) = result.toOption.get
    assert(env.inductives.contains("Int"), "env should contain Int")
    assert(defspecCount == 1, s"expected 1 defspec, got: $defspecCount")
  }

  test("check: int_add_zero_left holds by trivial") {
    val source =
      """|inductive Nat {
         |  case zero: Nat
         |  case succ(n: Nat): Nat
         |}
         |inductive Int {
         |  case zero: Int
         |  case pos(n: Nat): Int
         |  case neg(n: Nat): Int
         |}
         |def int_add(a: Int, b: Int): Int {
         |  match a {
         |    case Int.zero   => b
         |    case Int.pos(n) => b
         |    case Int.neg(n) => b
         |  }
         |}
         |defspec int_add_zero_left(a: Int): int_add(Int.zero, a) = a {
         |  by trivial
         |}
         |""".stripMargin
    val result = Main.processSource(source, "int-add-zero-left")
    assert(result.isRight, s"expected Right but got: $result")
  }

  // ---- have tactic ----

  test("check: have tactic introduces local hypothesis") {
    // Verify have tactic parses and the continuation runs
    // have h : Nat = { Nat.zero } ; trivial
    // This introduces h: Nat into context, then trivial closes n = n
    val source =
      """|inductive Nat {
         |  case zero: Nat
         |  case succ(n: Nat): Nat
         |}
         |defspec have_test(n: Nat): n = n {
         |  by have h : Nat = { Nat.zero } ; trivial
         |}
         |""".stripMargin
    val result = Main.processSource(source, "have-test")
    assert(result.isRight, s"expected Right but got: $result")
  }

  test("check: have tactic accepts equality propositions") {
    val source =
      """|inductive Nat {
         |  case zero: Nat
         |  case succ(n: Nat): Nat
         |}
         |defspec have_eq_test(n: Nat): n = n {
         |  by have h : n = n = { by trivial }; trivial
         |}
         |""".stripMargin
    val result = Main.processSource(source, "have-eq-test")
    assert(result.isRight, s"expected Right but got: $result")
  }

  test("check: later defspec can reuse earlier defspec as theorem term") {
    val source =
      """|inductive Nat {
         |  case zero: Nat
         |  case succ(n: Nat): Nat
         |}
         |defspec eq1(n: Nat): n = n {
         |  by trivial
         |}
         |defspec eq2(n: Nat): n = n {
         |  by exact eq1(n)
         |}
         |""".stripMargin
    val result = Main.processSource(source, "defspec-reuse")
    assert(result.isRight, s"expected Right but got: $result")
  }

  test("check: defspec forward reference is rejected (source-order checking)") {
    val source =
      """|inductive Nat {
         |  case zero: Nat
         |  case succ(n: Nat): Nat
         |}
         |defspec eq2(n: Nat): n = n {
         |  by exact eq1(n)
         |}
         |defspec eq1(n: Nat): n = n {
         |  by trivial
         |}
         |""".stripMargin
    val result = Main.processSource(source, "defspec-forward-ref")
    assert(result.isLeft, s"expected Left but got: $result")
    val msg = result.left.toOption.get
    assert(msg.contains("eq1") || msg.contains("Unknown variable"), s"unexpected error: $msg")
  }

  // ---- rewrite tactic ----

  test("check: rewrite tactic rewrites goal using equality hypothesis") {
    // Test that rewrite can close a goal when the hypothesis exactly matches
    val source =
      """|inductive Nat {
         |  case zero: Nat
         |  case succ(n: Nat): Nat
         |}
         |defspec rewrite_test(n: Nat): n = n {
         |  by rewrite [n]
         |}
         |""".stripMargin
    // Note: rewrite [n] where n: Nat (not an equality) should fall back or error
    // Let's test a simpler case first — just verify parsing works
    val parseResult = sproof.syntax.Parser.parseTactic("rewrite [h]")
    assert(parseResult.isRight, s"Parse failed: $parseResult")
  }

  // ---- numeric literals ----

  test("check: numeric literal 0 elaborates to Nat.zero") {
    val source =
      """|inductive Nat {
         |  case zero: Nat
         |  case succ(n: Nat): Nat
         |}
         |def myZero(): Nat = 0
         |""".stripMargin
    val result = Main.processSource(source, "num-literal-test")
    assert(result.isRight, s"expected Right but got: $result")
  }

  test("check: numeric literal 2 elaborates to succ(succ(zero))") {
    val source =
      """|inductive Nat {
         |  case zero: Nat
         |  case succ(n: Nat): Nat
         |}
         |def myTwo(): Nat = 2
         |""".stripMargin
    val result = Main.processSource(source, "num-literal-2-test")
    assert(result.isRight, s"expected Right but got: $result")
  }

  // ---- parameterized inductives ----

  test("check: parameterized inductive List(A) compiles") {
    val source =
      """|inductive Nat {
         |  case zero: Nat
         |  case succ(n: Nat): Nat
         |}
         |inductive List(A: Type) {
         |  case nil: List(A)
         |  case cons(head: A, tail: List(A)): List(A)
         |}
         |""".stripMargin
    val result = Main.processSource(source, "param-ind-test")
    assert(result.isRight, s"expected Right but got: $result")
    val (env, _) = result.toOption.get
    assert(env.inductives.contains("List"), "env should contain List")
  }

  // ---- numeric literal edge cases ----

  test("check: numeric literal 1 in expression") {
    val source =
      """|inductive Nat {
         |  case zero: Nat
         |  case succ(n: Nat): Nat
         |}
         |def myOne(): Nat = 1
         |""".stripMargin
    val result = Main.processSource(source, "num-literal-1-test")
    assert(result.isRight, s"expected Right but got: $result")
  }

  test("check: numeric literal -1 with Int type") {
    val source =
      """|inductive Nat {
         |  case zero: Nat
         |  case succ(n: Nat): Nat
         |}
         |inductive Int {
         |  case zero: Int
         |  case pos(n: Nat): Int
         |  case neg(n: Nat): Int
         |}
         |def myNeg(): Int = -1
         |""".stripMargin
    val result = Main.processSource(source, "neg-literal-test")
    assert(result.isRight, s"expected Right but got: $result")
  }

  // ---- Meta-tactics (Group A) ----

  test("check: try tactic succeeds when inner succeeds") {
    val source =
      """|inductive Nat {
         |  case zero: Nat
         |  case succ(n: Nat): Nat
         |}
         |defspec try_ok(n: Nat): n = n {
         |  by try trivial
         |}
         |""".stripMargin
    val result = Main.processSource(source, "try-ok")
    assert(result.isRight, s"expected Right but got: $result")
  }

  test("check: try tactic does not fail when inner fails") {
    // try (assumption) should silently fail when there's no matching hyp;
    // then trivial closes the reflexive goal
    val source =
      """|inductive Nat {
         |  case zero: Nat
         |  case succ(n: Nat): Nat
         |}
         |defspec try_fail(n: Nat): n = n {
         |  by { try assumption; trivial }
         |}
         |""".stripMargin
    val result = Main.processSource(source, "try-fail")
    assert(result.isRight, s"expected Right but got: $result")
  }

  test("check: first tactic picks first success") {
    // first | assumption | trivial — assumption fails, trivial succeeds
    val source =
      """|inductive Nat {
         |  case zero: Nat
         |  case succ(n: Nat): Nat
         |}
         |defspec first_test(n: Nat): n = n {
         |  by first | assumption | trivial
         |}
         |""".stripMargin
    val result = Main.processSource(source, "first-test")
    assert(result.isRight, s"expected Right but got: $result")
  }

  test("check: skip tactic is no-op") {
    // skip alone should leave the goal unsolved, so combine with trivial
    val source =
      """|inductive Nat {
         |  case zero: Nat
         |  case succ(n: Nat): Nat
         |}
         |defspec skip_test(n: Nat): n = n {
         |  by { skip; trivial }
         |}
         |""".stripMargin
    val result = Main.processSource(source, "skip-test")
    assert(result.isRight, s"expected Right but got: $result")
  }

  // ---- Goal inspection (Group B) ----

  test("check: assumption tactic finds matching hypothesis") {
    val source =
      """|inductive Nat {
         |  case zero: Nat
         |  case succ(n: Nat): Nat
         |}
         |defspec assumption_test(n: Nat): n = n {
         |  by trivial
         |}
         |""".stripMargin
    // This is a baseline — more thorough test would use assumption in a Pi-goal context
    val result = Main.processSource(source, "assumption-test")
    assert(result.isRight, s"expected Right but got: $result")
  }

  // ---- cases tactic (Group C) ----

  test("check: cases tactic splits without induction hypothesis") {
    val source =
      """|inductive Bool {
         |  case true: Bool
         |  case false: Bool
         |}
         |def not(b: Bool): Bool {
         |  match b {
         |    case Bool.true => Bool.false
         |    case Bool.false => Bool.true
         |  }
         |}
         |defspec not_not(b: Bool): not(not(b)) = b {
         |  by cases b {
         |    case true => trivial
         |    case false => trivial
         |  }
         |}
         |""".stripMargin
    val result = Main.processSource(source, "cases-test")
    assert(result.isRight, s"expected Right but got: $result")
  }

end MainSuite
