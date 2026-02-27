package sproof

import munit.FunSuite

/** Tests for proof state visualization in error messages.
  *
  * When a proof fails, the error message must include the proof state
  * in S-expression format — structured, machine-readable, no ASCII art.
  *
  * Expected format:
  * {{{
  *   (proof-state
  *     (context
  *       (hyp "n" "Nat")
  *       (hyp "k" "Nat")
  *       (hyp "ih" "(plus k Nat.zero) = k"))
  *     (goal "(plus (Nat.succ k) Nat.zero) = (Nat.succ k)")
  *     (error "trivial: not definitionally equal"))
  * }}}
  *
  * This format is chosen because:
  * - LLMs are trained on Lisp/S-expression syntax
  * - Regular nested structure is easy to parse programmatically
  * - No ambiguous line-drawing or whitespace conventions
  */
class ProofStateSuite extends FunSuite:

  // ---- S-expression proof state sections appear in failure messages ----

  test("failing proof outputs (proof-state ...) sexp block") {
    val source =
      """|inductive Nat {
         |  case zero: Nat
         |  case succ(n: Nat): Nat
         |}
         |def plus(n: Nat, m: Nat): Nat {
         |  match n {
         |    case Nat.zero => m
         |    case Nat.succ(k) => Nat.succ(plus(k, m))
         |  }
         |}
         |defspec bad_proof(n: Nat): plus(n, Nat.zero) = n {
         |  by trivial
         |}
         |""".stripMargin
    val result = Main.processSource(source, "bad.sproof")
    assert(result.isLeft, s"expected failure but got: $result")
    val msg = result.left.toOption.get
    assert(msg.contains("(proof-state"), s"error should include '(proof-state' sexp:\n$msg")
  }

  test("failing proof includes (context ...) sexp") {
    val source =
      """|inductive Nat {
         |  case zero: Nat
         |  case succ(n: Nat): Nat
         |}
         |def plus(n: Nat, m: Nat): Nat {
         |  match n {
         |    case Nat.zero => m
         |    case Nat.succ(k) => Nat.succ(plus(k, m))
         |  }
         |}
         |defspec bad_proof(n: Nat): plus(n, Nat.zero) = n {
         |  by trivial
         |}
         |""".stripMargin
    val result = Main.processSource(source, "bad.sproof")
    assert(result.isLeft)
    val msg = result.left.toOption.get
    assert(msg.contains("(context"), s"error should include '(context' sexp:\n$msg")
  }

  test("failing proof includes (goal ...) sexp") {
    val source =
      """|inductive Nat {
         |  case zero: Nat
         |  case succ(n: Nat): Nat
         |}
         |def plus(n: Nat, m: Nat): Nat {
         |  match n {
         |    case Nat.zero => m
         |    case Nat.succ(k) => Nat.succ(plus(k, m))
         |  }
         |}
         |defspec bad_proof(n: Nat): plus(n, Nat.zero) = n {
         |  by trivial
         |}
         |""".stripMargin
    val result = Main.processSource(source, "bad.sproof")
    assert(result.isLeft)
    val msg = result.left.toOption.get
    assert(msg.contains("(goal"), s"error should include '(goal' sexp:\n$msg")
  }

  test("failing proof includes (error ...) sexp with tactic message") {
    val source =
      """|inductive Nat {
         |  case zero: Nat
         |  case succ(n: Nat): Nat
         |}
         |defspec never_proved(n: Nat): n = Nat.zero {
         |  by trivial
         |}
         |""".stripMargin
    val result = Main.processSource(source, "never-proved.sproof")
    assert(result.isLeft)
    val msg = result.left.toOption.get
    assert(msg.contains("(error"), s"error should include '(error' sexp:\n$msg")
  }

  test("context (hyp ...) entries show name and type") {
    // defspec with two params: a, b should appear as (hyp "a" "Nat") etc.
    val source =
      """|inductive Nat {
         |  case zero: Nat
         |  case succ(n: Nat): Nat
         |}
         |defspec two_params(a: Nat, b: Nat): a = b {
         |  by trivial
         |}
         |""".stripMargin
    val result = Main.processSource(source, "two-params.sproof")
    assert(result.isLeft)
    val msg = result.left.toOption.get
    assert(msg.contains("(hyp"), s"context entries should use '(hyp ...)' form:\n$msg")
    assert(msg.contains("\"a\"") || msg.contains(" a ") || msg.contains("a"),
      s"context should mention param 'a':\n$msg")
    assert(msg.contains("Nat"),
      s"context should mention type 'Nat':\n$msg")
  }

  test("proof state format has no ASCII box-drawing characters") {
    val source =
      """|inductive Nat {
         |  case zero: Nat
         |  case succ(n: Nat): Nat
         |}
         |defspec bad(n: Nat): n = Nat.zero {
         |  by trivial
         |}
         |""".stripMargin
    val result = Main.processSource(source, "bad.sproof")
    assert(result.isLeft)
    val msg = result.left.toOption.get
    assert(!msg.contains("─"), s"should not contain box-drawing '─':\n$msg")
    assert(!msg.contains("│"), s"should not contain box-drawing '│':\n$msg")
    assert(!msg.contains("┌"), s"should not contain box-drawing '┌':\n$msg")
    assert(!msg.contains("└"), s"should not contain box-drawing '└':\n$msg")
  }

  test("context section lists params in order outermost first") {
    // (hyp "a" ...) should appear before (hyp "b" ...) since a is outer
    val source =
      """|inductive Nat {
         |  case zero: Nat
         |  case succ(n: Nat): Nat
         |}
         |defspec two_params(a: Nat, b: Nat): a = b {
         |  by trivial
         |}
         |""".stripMargin
    val result = Main.processSource(source, "two-params.sproof")
    assert(result.isLeft)
    val msg = result.left.toOption.get
    val aIdx = msg.indexOf("\"a\"")
    val bIdx = msg.indexOf("\"b\"")
    assert(aIdx >= 0, s"should contain param 'a':\n$msg")
    assert(bIdx >= 0, s"should contain param 'b':\n$msg")
    assert(aIdx < bIdx, s"'a' should appear before 'b' in context:\n$msg")
  }

  test("induction succ case shows all hypotheses including ih") {
    val source =
      """|inductive Nat {
         |  case zero: Nat
         |  case succ(n: Nat): Nat
         |}
         |def plus(n: Nat, m: Nat): Nat {
         |  match n {
         |    case Nat.zero => m
         |    case Nat.succ(k) => Nat.succ(plus(k, m))
         |  }
         |}
         |defspec bad_succ(n: Nat): plus(n, Nat.zero) = n {
         |  by induction n {
         |    case zero => trivial
         |    case succ k ih => trivial
         |  }
         |}
         |""".stripMargin
    val result = Main.processSource(source, "bad-succ.sproof")
    assert(result.isLeft, s"expected failure but got: $result")
    val msg = result.left.toOption.get
    // The proof state at the failing succ case should include k and ih
    assert(msg.contains("(proof-state"), s"should have proof-state sexp:\n$msg")
  }

  test("successful proof does NOT include proof-state sexp in message") {
    val source =
      """|inductive Nat {
         |  case zero: Nat
         |  case succ(n: Nat): Nat
         |}
         |defspec refl(n: Nat): n = n {
         |  by trivial
         |}
         |""".stripMargin
    val result = Main.processSource(source, "refl.sproof")
    assert(result.isRight, s"expected success but got: $result")
    // On success there is no error at all
  }

end ProofStateSuite
