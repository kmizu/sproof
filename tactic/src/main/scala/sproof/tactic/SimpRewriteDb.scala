package sproof.tactic

import scala.annotation.tailrec
import sproof.core.{Term, MatchCase, Param, Ctor}

/** Ordered rewrite-rule database for `simp`.
 *
 *  Rule spec syntax in tactic source:
 *    - `h`             : forward rewrite using hypothesis `h`
 *    - `h__rev`        : backward rewrite
 *    - `h__p10`        : explicit priority 10
 *    - `h__rev__p10`   : backward rewrite with explicit priority
 */
object SimpRewriteDb:

  enum RewriteDirection:
    case Forward
    case Backward

  import RewriteDirection.*

  final case class RuleSpec(
    raw: String,
    lemmaName: String,
    direction: RewriteDirection,
    priority: Int,
    order: Int,
  )

  final case class RewriteRule(
    name: String,
    lhs: Term,
    rhs: Term,
    direction: RewriteDirection = Forward,
    priority: Int = 0,
    order: Int = 0,
  ):
    def from: Term = direction match
      case Forward  => lhs
      case Backward => rhs
    def to: Term = direction match
      case Forward  => rhs
      case Backward => lhs

  def parseSpec(raw: String, order: Int): RuleSpec =
    val parts = raw.split("__").toList
    val lemma = parts.headOption.getOrElse(raw)
    val direction =
      if parts.exists(_ == "rev") then Backward
      else Forward
    val priority =
      parts.collectFirst {
        case p if p.startsWith("p") && p.length > 1 && p.drop(1).forall(_.isDigit) =>
          p.drop(1).toInt
      }.getOrElse(0)
    RuleSpec(raw, lemma, direction, priority, order)

  def orderSpecs(specs: List[String]): List[RuleSpec] =
    specs.zipWithIndex
      .map((raw, i) => parseSpec(raw, i))
      .sortBy(spec => (-spec.priority, spec.order))

  /** Normalize a term with ordered rewrite rules.
   *
   *  Loop guard:
   *  - max rewrite step budget
   *  - visited-term cycle detection
   */
  def normalize(term: Term, rules: List[RewriteRule], maxSteps: Int = 64): Term =
    val orderedRules = rules.sortBy(r => (-r.priority, r.order))

    @tailrec
    def loop(current: Term, seen: Set[Term], remaining: Int): Term =
      if remaining <= 0 then current
      else
        rewriteOnce(current, orderedRules) match
          case None => current
          case Some(next) if seen.contains(next) =>
            current
          case Some(next) =>
            loop(next, seen + next, remaining - 1)

    loop(term, Set(term), maxSteps)

  private def rewriteOnce(term: Term, rules: List[RewriteRule]): Option[Term] =
    rules.collectFirst { case rule if term == rule.from => rule.to }
      .orElse(rewriteChildren(term, rules))

  private def rewriteChildren(term: Term, rules: List[RewriteRule]): Option[Term] = term match
    case Term.App(f, a) =>
      rewriteOnce(f, rules).map(Term.App(_, a))
        .orElse(rewriteOnce(a, rules).map(Term.App(f, _)))
    case Term.Lam(x, tpe, body) =>
      rewriteOnce(tpe, rules).map(Term.Lam(x, _, body))
        .orElse(rewriteOnce(body, rules).map(Term.Lam(x, tpe, _)))
    case Term.Pi(x, dom, cod) =>
      rewriteOnce(dom, rules).map(Term.Pi(x, _, cod))
        .orElse(rewriteOnce(cod, rules).map(Term.Pi(x, dom, _)))
    case Term.Let(x, tpe, dfn, body) =>
      rewriteOnce(tpe, rules).map(Term.Let(x, _, dfn, body))
        .orElse(rewriteOnce(dfn, rules).map(Term.Let(x, tpe, _, body)))
        .orElse(rewriteOnce(body, rules).map(Term.Let(x, tpe, dfn, _)))
    case Term.Ind(name, params, ctors) =>
      rewriteParams(params, rules).map(ps => Term.Ind(name, ps, ctors))
        .orElse(rewriteCtors(ctors, rules).map(cs => Term.Ind(name, params, cs)))
    case Term.Con(name, ref, args) =>
      rewriteFirst(args, rules).map(updated => Term.Con(name, ref, updated))
    case Term.Fix(name, tpe, body) =>
      rewriteOnce(tpe, rules).map(Term.Fix(name, _, body))
        .orElse(rewriteOnce(body, rules).map(Term.Fix(name, tpe, _)))
    case Term.Mat(scrutinee, cases, returnTpe) =>
      rewriteOnce(scrutinee, rules).map(Term.Mat(_, cases, returnTpe))
        .orElse(rewriteCases(cases, rules).map(cs => Term.Mat(scrutinee, cs, returnTpe)))
        .orElse(rewriteOnce(returnTpe, rules).map(Term.Mat(scrutinee, cases, _)))
    case Term.Var(_) | Term.Uni(_) | Term.Meta(_) =>
      None

  private def rewriteParams(params: List[Param], rules: List[RewriteRule]): Option[List[Param]] =
    params.zipWithIndex.collectFirst {
      case (p, i) if rewriteOnce(p.tpe, rules).nonEmpty =>
        val rewritten = rewriteOnce(p.tpe, rules).get
        params.updated(i, p.copy(tpe = rewritten))
    }

  private def rewriteCtors(ctors: List[Ctor], rules: List[RewriteRule]): Option[List[Ctor]] =
    ctors.zipWithIndex.collectFirst {
      case (c, i) if rewriteOnce(c.tpe, rules).nonEmpty =>
        val rewritten = rewriteOnce(c.tpe, rules).get
        ctors.updated(i, c.copy(tpe = rewritten))
    }

  private def rewriteCases(cases: List[MatchCase], rules: List[RewriteRule]): Option[List[MatchCase]] =
    cases.zipWithIndex.collectFirst {
      case (mc, i) if rewriteOnce(mc.body, rules).nonEmpty =>
        val rewritten = rewriteOnce(mc.body, rules).get
        cases.updated(i, mc.copy(body = rewritten))
    }

  private def rewriteFirst(terms: List[Term], rules: List[RewriteRule]): Option[List[Term]] =
    terms.zipWithIndex.collectFirst {
      case (t, i) if rewriteOnce(t, rules).nonEmpty =>
        val rewritten = rewriteOnce(t, rules).get
        terms.updated(i, rewritten)
    }

