package sproof

import munit.FunSuite

class IncrementalSuite extends FunSuite:

  private val baseSource =
    """|inductive Nat {
       |  case zero: Nat
       |  case succ(n: Nat): Nat
       |}
       |defspec refl(n: Nat): n = n {
       |  by trivial
       |}
       |""".stripMargin

  test("no-change re-run hits parse/elab/proof caches") {
    Main.resetIncrementalCache()

    val first = Main.processSourceWithIncrementalStats(baseSource, "inc-nochange.sproof")
    assert(first.isRight, s"first run should succeed: $first")
    val firstStats = first.toOption.get._4
    assert(!firstStats.parseCacheHit, s"first parse should miss cache: $firstStats")
    assert(!firstStats.elabCacheHit, s"first elab should miss cache: $firstStats")
    assert(!firstStats.proofCacheHit, s"first proof should miss cache: $firstStats")

    val second = Main.processSourceWithIncrementalStats(baseSource, "inc-nochange.sproof")
    assert(second.isRight, s"second run should succeed: $second")
    val secondStats = second.toOption.get._4
    assert(secondStats.parseCacheHit, s"second parse should hit cache: $secondStats")
    assert(secondStats.elabCacheHit, s"second elab should hit cache: $secondStats")
    assert(secondStats.proofCacheHit, s"second proof should hit cache: $secondStats")
  }

  test("non-semantic edit reuses downstream caches") {
    Main.resetIncrementalCache()

    val src1 = baseSource
    val src2 =
      """|// added comment
         |inductive Nat {
         |  case zero: Nat
         |  case succ(n: Nat): Nat
         |}
         |defspec refl(n: Nat): n = n {
         |  by trivial
         |}
         |""".stripMargin

    val first = Main.processSourceWithIncrementalStats(src1, "inc-comment.sproof")
    assert(first.isRight, s"first run should succeed: $first")

    val second = Main.processSourceWithIncrementalStats(src2, "inc-comment.sproof")
    assert(second.isRight, s"second run should succeed: $second")
    val stats = second.toOption.get._4
    assert(!stats.parseCacheHit, s"source changed so parse cache should miss: $stats")
    assert(stats.elabCacheHit, s"decl-level cache should allow elab reuse: $stats")
    assert(stats.proofCacheHit, s"program-level cache should allow proof reuse: $stats")
  }

  test("declaration change invalidates downstream cache and preserves correctness") {
    Main.resetIncrementalCache()

    val srcOk = baseSource
    val srcBad =
      """|inductive Nat {
         |  case zero: Nat
         |  case succ(n: Nat): Nat
         |}
         |defspec refl(n: Nat): n = Nat.zero {
         |  by trivial
         |}
         |""".stripMargin

    val first = Main.processSourceWithIncrementalStats(srcOk, "inc-invalid.sproof")
    assert(first.isRight, s"first run should succeed: $first")

    val second = Main.processSourceWithIncrementalStats(srcBad, "inc-invalid.sproof")
    assert(second.isLeft, s"changed declaration should fail and not reuse stale success: $second")
    val message = second.left.toOption.get
    assert(message.contains("Proof of 'refl' failed"), s"should report real proof failure:\n$message")
  }

