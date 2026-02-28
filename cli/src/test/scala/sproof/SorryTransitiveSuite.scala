package sproof

import munit.FunSuite

/** Tests for transitive sorry detection.
 *
 *  If defspec A uses sorry, and defspec B references A (via simplify [A]),
 *  B should also be flagged as transitively unsound even though B itself
 *  doesn't directly use sorry.
 */
class SorryTransitiveSuite extends FunSuite:

  test("defspec referencing a sorry defspec gets transitive warning"):
    // Use a trivially-provable lemma that also uses sorry,
    // then reference it from another defspec that also uses sorry for its own proof.
    // The transitive detection should flag both.
    val source =
      """|inductive Nat { case zero: Nat  case succ(n: Nat): Nat }
         |defspec base_lemma(n: Nat): n = n { by sorry }
         |defspec depends_on_base(n: Nat): n = n {
         |  by simplify [base_lemma]
         |}
         |""".stripMargin
    val result = Main.processSourceWithWarnings(source, "transitive.sproof")
    assert(result.isRight, s"expected Right: $result")
    val (_, _, warnings) = result.toOption.get
    // depends_on_base should have a transitive sorry warning
    assert(
      warnings.exists(_.contains("depends_on_base")),
      s"expected transitive sorry warning for depends_on_base: $warnings"
    )

  test("clean defspec referencing clean defspec has no sorry warning"):
    val source =
      """|inductive Nat { case zero: Nat  case succ(n: Nat): Nat }
         |def plus(n: Nat, m: Nat): Nat {
         |  match n {
         |    case Nat.zero    => m
         |    case Nat.succ(k) => Nat.succ(plus(k, m))
         |  }
         |}
         |defspec plus_zero_left(m: Nat): plus(Nat.zero, m) = m {
         |  by trivial
         |}
         |defspec also_clean(m: Nat): plus(Nat.zero, m) = m {
         |  by simplify [plus_zero_left]
         |}
         |""".stripMargin
    val result = Main.processSourceWithWarnings(source, "clean.sproof")
    assert(result.isRight, s"expected Right: $result")
    val (_, _, warnings) = result.toOption.get
    assert(warnings.isEmpty, s"expected no warnings, got: $warnings")

  test("direct sorry defspec is still warned"):
    val source =
      """|inductive Nat { case zero: Nat  case succ(n: Nat): Nat }
         |defspec bad(n: Nat): n = n { by sorry }
         |""".stripMargin
    val result = Main.processSourceWithWarnings(source, "direct.sproof")
    assert(result.isRight, s"expected Right: $result")
    val (_, _, warnings) = result.toOption.get
    assert(warnings.exists(_.contains("bad")), s"expected sorry warning: $warnings")
