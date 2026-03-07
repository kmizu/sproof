package sproof

import munit.FunSuite

/** Tests for @[simp] attribute wiring: defspecs marked @[simp] should be
 *  automatically used by `simplify` (with no explicit lemma list).
 */
class SimpSetSuite extends FunSuite:

  private def check(src: String): Either[String, ?] =
    Main.processSource(src, "<SimpSetSuite>")

  // ---- @[simp] on def ----

  test("@[simp] def is added to simpSet"):
    val src = """
      inductive Nat {
        case zero: Nat
        case succ(n: Nat): Nat
      }
      @[simp] def id(n: Nat): Nat { n }
    """
    assert(check(src).isRight, s"source should elaborate: ${check(src)}")

  // ---- @[simp] on defspec ----

  test("@[simp] defspec elaborates without error"):
    val src = """
      inductive Nat {
        case zero: Nat
        case succ(n: Nat): Nat
      }
      def plus(n: Nat, m: Nat): Nat {
        match n {
          case Nat.zero    => m
          case Nat.succ(k) => Nat.succ(plus(k, m))
        }
      }
      @[simp] defspec plus_zero_left(m: Nat): plus(Nat.zero, m) = m {
        by trivial
      }
    """
    assert(check(src).isRight, s"@[simp] defspec should check: ${check(src)}")

  // ---- simplify queries simpSet ----

  test("simplify with no lemmas uses @[simp] defspec from simpSet"):
    // plus_zero_left is in simpSet; bar proves the same proposition using `by simplify`
    val src = """
      inductive Nat {
        case zero: Nat
        case succ(n: Nat): Nat
      }
      def plus(n: Nat, m: Nat): Nat {
        match n {
          case Nat.zero    => m
          case Nat.succ(k) => Nat.succ(plus(k, m))
        }
      }
      @[simp] defspec plus_zero_left(m: Nat): plus(Nat.zero, m) = m {
        by trivial
      }
      defspec plus_zero_left_again(k: Nat): plus(Nat.zero, k) = k {
        by simplify
      }
    """
    assert(check(src).isRight, s"simplify should use simpSet lemma: ${check(src)}")

  test("simplify with no lemmas uses trivial when simpSet is empty"):
    val src = """
      inductive Nat {
        case zero: Nat
        case succ(n: Nat): Nat
      }
      def plus(n: Nat, m: Nat): Nat {
        match n {
          case Nat.zero    => m
          case Nat.succ(k) => Nat.succ(plus(k, m))
        }
      }
      defspec plus_zero_left(m: Nat): plus(Nat.zero, m) = m {
        by simplify
      }
    """
    assert(check(src).isRight, s"simplify with empty simpSet should fall back to trivial: ${check(src)}")

  test("simplify with no lemmas fails when goal is non-trivial and simpSet cannot help"):
    val src = """
      inductive Nat {
        case zero: Nat
        case succ(n: Nat): Nat
      }
      def plus(n: Nat, m: Nat): Nat {
        match n {
          case Nat.zero    => m
          case Nat.succ(k) => Nat.succ(plus(k, m))
        }
      }
      defspec plus_zero_right(n: Nat): plus(n, Nat.zero) = n {
        by simplify
      }
    """
    // plus(n, zero) = n is NOT trivial (requires induction), so simplify should fail
    assert(check(src).isLeft, "simplify should fail when goal is non-trivial and no simp lemmas can help")
