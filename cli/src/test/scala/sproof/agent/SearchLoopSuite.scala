package sproof.agent

import munit.FunSuite
import sproof.Main

/** Tests for the proof agent's search loop.
 *
 *  Strategy: run the agent on .sproof source with sorry proofs
 *  and verify it produces ok:true with no warnings.
 */
class SearchLoopSuite extends FunSuite:

  // Helper: run agent on source, return repaired source
  private def repairSource(src: String): String =
    FileRepairer.repair(src, "<test>")

  // Helper: check that repaired source verifies cleanly
  private def verifies(src: String): Boolean =
    Main.processSourceWithWarnings(src, "<test>") match
      case Right((_, _, warnings)) => warnings.isEmpty
      case Left(_)                 => false

  test("agent proves trivial sorry: plus(0, n) = n"):
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
      defspec plus_zero_left(n: Nat): plus(Nat.zero, n) = n {
        by sorry
      }
    """
    val repaired = repairSource(src)
    assert(verifies(repaired), s"repaired source failed:\n$repaired")

  test("agent proves induction sorry: plus(n, 0) = n"):
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
      defspec plus_zero(n: Nat): plus(n, Nat.zero) = n {
        by sorry
      }
    """
    val repaired = repairSource(src)
    assert(verifies(repaired), s"repaired source failed:\n$repaired")

  test("agent proves mult_zero: mult(n, 0) = 0"):
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
      def mult(n: Nat, m: Nat): Nat {
        match n {
          case Nat.zero    => Nat.zero
          case Nat.succ(k) => plus(m, mult(k, m))
        }
      }
      defspec mult_zero(n: Nat): mult(n, Nat.zero) = Nat.zero {
        by sorry
      }
    """
    val repaired = repairSource(src)
    assert(verifies(repaired), s"repaired source failed:\n$repaired")

  test("agent leaves non-sorry proofs untouched"):
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
      defspec plus_zero_left(n: Nat): plus(Nat.zero, n) = n {
        by trivial
      }
    """
    val repaired = repairSource(src)
    assert(verifies(repaired))
    assert(repaired.contains("by trivial"), "non-sorry proof should be unchanged")

  test("agent reports success for already-valid file"):
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
      defspec plus_zero_left(n: Nat): plus(Nat.zero, n) = n {
        by trivial
      }
      defspec plus_zero(n: Nat): plus(n, Nat.zero) = n {
        by induction n {
          case zero    => trivial
          case succ k ih => simplify [ih]
        }
      }
    """
    val repaired = repairSource(src)
    assert(verifies(repaired))
