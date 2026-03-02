package sproof.agent

import munit.FunSuite
import sproof.Main
import sproof.core.{Context, GlobalEnv, Term}
import sproof.tactic.Eq

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

  test("agent proves plus_succ_right: plus(n, succ(m)) = succ(plus(n, m))"):
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
      defspec plus_succ_right(n: Nat, m: Nat): plus(n, Nat.succ(m)) = Nat.succ(plus(n, m)) {
        by sorry
      }
    """
    val repaired = repairSource(src)
    assert(verifies(repaired), s"repaired source failed:\n$repaired")

  test("agent repairs multiple sorry defspecs in one file"):
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
      defspec plus_zero(n: Nat): plus(n, Nat.zero) = n {
        by sorry
      }
    """
    val repaired = repairSource(src)
    assert(verifies(repaired), s"repaired source failed:\n$repaired")
    assert(!repaired.contains("by sorry"), "all sorrys should be replaced")

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

  test("searchWithConfig enforces deterministic maxNodes limit"):
    given GlobalEnv = GlobalEnv.withNat
    val nat  = Term.Ind("Nat", Nil, Nil)
    val ctx  = Context.empty.extend("n", nat)
    val goal = Eq.mkType(nat, Term.Var(0), Term.Con("zero", "Nat", Nil))

    val config = SearchLoop.SearchConfig(maxDepth = 1, maxNodes = 1)
    val r1 = SearchLoop.searchWithConfig(ctx, goal, config)
    val r2 = SearchLoop.searchWithConfig(ctx, goal, config)

    assertEquals(r1.found, r2.found, "search should be deterministic with same limits")
    assertEquals(r1.stats.exploredNodes, 1, "maxNodes=1 should cap explored nodes")
    assert(r1.stats.uniqueCandidates >= 2, s"expected >1 candidate for this goal: ${r1.stats}")
    assert(r1.stats.hitNodeLimit, s"expected node-limit flag: ${r1.stats}")

  test("ranked candidates are sorted by score and de-duplicated"):
    given GlobalEnv = GlobalEnv.withNat
    val nat   = Term.Ind("Nat", Nil, Nil)
    val ctx   = Context.empty.extend("n", nat)
    val goal  = Eq.mkType(nat, Term.Var(0), Term.Var(0))
    val ranked = TacticGen.rankedCandidates(ctx, goal, maxDepth = 1)

    assert(ranked.nonEmpty, "ranked candidates should not be empty")
    assertEquals(ranked.map(_.key).distinct.size, ranked.size)
    val nonIncreasing = ranked.zip(ranked.drop(1)).forall { case (a, b) => a.score >= b.score }
    assert(nonIncreasing, s"candidate scores should be non-increasing: $ranked")
