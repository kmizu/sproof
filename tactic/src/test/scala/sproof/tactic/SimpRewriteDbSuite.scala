package sproof.tactic

import munit.FunSuite
import sproof.core.Term
import sproof.tactic.SimpRewriteDb.RewriteDirection

class SimpRewriteDbSuite extends FunSuite:

  private val a = Term.Con("a", "Nat", Nil)
  private val b = Term.Con("b", "Nat", Nil)
  private val c = Term.Con("c", "Nat", Nil)

  test("orderSpecs sorts by explicit priority, then source order"):
    val ordered = SimpRewriteDb.orderSpecs(List("h1", "h2__p10", "h3__p2", "h4"))
    assertEquals(ordered.map(_.lemmaName), List("h2", "h3", "h1", "h4"))
    assertEquals(ordered.map(_.priority), List(10, 2, 0, 0))

  test("parseSpec supports backward direction marker"):
    val spec = SimpRewriteDb.parseSpec("ih__rev__p3", order = 0)
    assertEquals(spec.lemmaName, "ih")
    assertEquals(spec.direction, RewriteDirection.Backward)
    assertEquals(spec.priority, 3)

  test("normalize applies highest-priority rule first"):
    val rules = List(
      SimpRewriteDb.RewriteRule("low", lhs = a, rhs = c, priority = 1, order = 0),
      SimpRewriteDb.RewriteRule("high", lhs = a, rhs = b, priority = 10, order = 1),
    )
    val rewritten = SimpRewriteDb.normalize(a, rules)
    assertEquals(rewritten, b)

  test("normalize terminates on rewrite loop using seen-term guard"):
    val rules = List(
      SimpRewriteDb.RewriteRule("a_to_b", lhs = a, rhs = b, priority = 1, order = 0),
      SimpRewriteDb.RewriteRule("b_to_a", lhs = b, rhs = a, priority = 1, order = 1),
    )
    val rewritten = SimpRewriteDb.normalize(a, rules, maxSteps = 100)
    assertEquals(rewritten, b)

