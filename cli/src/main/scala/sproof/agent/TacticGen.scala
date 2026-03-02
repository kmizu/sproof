package sproof.agent

import sproof.core.{Term, Context, GlobalEnv, IndDef}
import sproof.syntax.{STactic, STacticCase}
import sproof.tactic.Eq

/** Generates scored tactic candidates for proof search.
 *
 *  Depth levels:
 *  - depth 0: single-step tactics (`trivial`, `assumption`, `simplify[h]`)
 *  - depth 1: structural induction candidates
 *
 *  Heuristics:
 *  - goal-aware induction variable ranking (prefer vars appearing in goal)
 *  - recursive constructors prioritize IH-enabled branches
 *  - fallback induction candidates are appended after primary combinations
 *
 *  Candidates are scored and de-duplicated deterministically.
 */
object TacticGen:

  final case class ScoredCandidate(
    tactic: STactic,
    score: Int,
    key: String,
  )

  /** Generate ordered tactic candidates for a goal. */
  def candidates(ctx: Context, target: Term, maxDepth: Int = 1)(using env: GlobalEnv): List[STactic] =
    rankedCandidates(ctx, target, maxDepth).map(_.tactic)

  /** Generate scored candidates sorted by descending score. */
  def rankedCandidates(ctx: Context, target: Term, maxDepth: Int = 1)(using env: GlobalEnv): List[ScoredCandidate] =
    dedupeCandidates(rawCandidates(ctx, target, maxDepth))

  private[agent] def rawCandidates(
    ctx: Context,
    target: Term,
    maxDepth: Int = 1,
  )(using env: GlobalEnv): List[ScoredCandidate] =
    val d0 = depth0(ctx, target)
    val d1 =
      if maxDepth >= 1 then
        val rankedVars = rankedInductionVars(ctx, target)
        val primary = rankedVars.flatMap { (varName, indDef) =>
          buildInductionCandidates(varName, indDef, d0.map(_.tactic)).map { tac =>
            ScoredCandidate(tac, scoreInduction(indDef), tacticKey(tac))
          }
        }
        val fallback = fallbackInductionCandidates(rankedVars).map { (tac, indDef) =>
          ScoredCandidate(tac, scoreFallbackInduction(indDef), tacticKey(tac))
        }
        primary ++ fallback
      else Nil
    d0 ++ d1

  /** Ranked inductive variables used for induction/cases heuristics. */
  private[agent] def rankedInductionVars(ctx: Context, target: Term)(using env: GlobalEnv): List[(String, IndDef)] =
    ctx.entries.zipWithIndex
      .flatMap { case (entry, idx) =>
        extractIndName(entry.tpe).flatMap(env.lookupInd).map { ind =>
          val occurrenceScore = countVarOccurrences(target, idx)
          val recursiveScore  = ind.ctors.count(_.argTpes.nonEmpty)
          // Higher occurrence in goal first, then recursive richness.
          val score = occurrenceScore * 100 + recursiveScore
          (entry.name, ind, score)
        }
      }
      .sortBy { case (_, _, score) => -score }
      .map { case (name, ind, _) => (name, ind) }

  private[agent] def dedupeCandidates(cands: List[ScoredCandidate]): List[ScoredCandidate] =
    dedupeAndSort(cands)

  // ---- Depth 0: single-step tactics ----

  private def depth0(ctx: Context, target: Term)(using env: GlobalEnv): List[ScoredCandidate] =
    val trivial = ScoredCandidate(STactic.STrivial, scoreTrivial(target), tacticKey(STactic.STrivial))
    val assumption = ScoredCandidate(STactic.SAssumption, scoreAssumption(ctx, target), tacticKey(STactic.SAssumption))
    val simpRules = eqHypNames(ctx).map { h =>
      val tac = STactic.SSimplify(List(h))
      ScoredCandidate(tac, scoreSimplify(h), tacticKey(tac))
    }
    trivial :: assumption :: simpRules

  // ---- Depth 1: induction candidates ----

  /** For each constructor, build STacticCase options and take cartesian combinations. */
  private def buildInductionCandidates(
    varName: String,
    indDef: IndDef,
    subTactics: List[STactic],
  ): List[STactic] =
    val perCtorOptions: List[List[STacticCase]] = indDef.ctors.map { ctor =>
      if ctor.argTpes.isEmpty then
        subTactics.map(t => STacticCase(ctor.name, Nil, t))
      else
        val argNames  = ctor.argTpes.indices.map(i => s"_arg$i").toList
        val withIH    = STacticCase(ctor.name, argNames :+ "ih", STactic.SSimplify(List("ih")))
        val withoutIH = subTactics.map(t => STacticCase(ctor.name, argNames, t))
        // IH-first ordering: prioritize branches where induction hypothesis may help.
        withIH :: withoutIH
    }
    cartesian(perCtorOptions).map(cases => STactic.SInduction(varName, cases))

  private def fallbackInductionCandidates(
    rankedVars: List[(String, IndDef)],
  ): List[(STactic, IndDef)] =
    rankedVars.map { (varName, indDef) =>
      val cases = indDef.ctors.map { ctor =>
        if ctor.argTpes.isEmpty then
          STacticCase(ctor.name, Nil, STactic.STrivial)
        else
          val argNames = ctor.argTpes.indices.map(i => s"_arg$i").toList
          STacticCase(ctor.name, argNames :+ "ih", STactic.SSimplify(List("ih")))
      }
      (STactic.SInduction(varName, cases), indDef)
    }

  // ---- Scoring ----

  private def scoreTrivial(target: Term): Int =
    Eq.extract(target) match
      case Some((_, lhs, rhs)) if lhs == rhs => 1000
      case Some(_)                           => 500
      case None                              => 100

  private def scoreAssumption(ctx: Context, target: Term): Int =
    val exactHit = ctx.entries.exists(e => e.tpe == target)
    if exactHit then 950 else 400

  private def scoreSimplify(name: String): Int =
    700 + math.min(name.length, 30)

  private def scoreInduction(indDef: IndDef): Int =
    300 - indDef.ctors.length

  private def scoreFallbackInduction(indDef: IndDef): Int =
    200 - indDef.ctors.length

  // ---- Helpers ----

  private def dedupeAndSort(cands: List[ScoredCandidate]): List[ScoredCandidate] =
    val bestByKey = scala.collection.mutable.LinkedHashMap.empty[String, ScoredCandidate]
    cands.foreach { cand =>
      bestByKey.get(cand.key) match
        case Some(prev) if prev.score >= cand.score => ()
        case _                                      => bestByKey.update(cand.key, cand)
    }
    bestByKey.values.toList.sortBy(c => (-c.score, c.key))

  private def tacticKey(tac: STactic): String =
    tac.toString

  private def countVarOccurrences(term: Term, targetIdx: Int): Int =
    def go(depth: Int, t: Term): Int = t match
      case Term.Var(i) =>
        if i == targetIdx + depth then 1 else 0
      case Term.App(f, a) =>
        go(depth, f) + go(depth, a)
      case Term.Lam(_, tpe, body) =>
        go(depth, tpe) + go(depth + 1, body)
      case Term.Pi(_, dom, cod) =>
        go(depth, dom) + go(depth + 1, cod)
      case Term.Let(_, tpe, dfn, body) =>
        go(depth, tpe) + go(depth, dfn) + go(depth + 1, body)
      case Term.Ind(_, params, ctors) =>
        params.map(p => go(depth, p.tpe)).sum + ctors.map(c => go(depth, c.tpe)).sum
      case Term.Con(_, _, args) =>
        args.map(go(depth, _)).sum
      case Term.Fix(_, tpe, body) =>
        go(depth, tpe) + go(depth + 1, body)
      case Term.Mat(scrutinee, cases, returnTpe) =>
        go(depth, scrutinee) +
          cases.map(c => go(depth + c.bindings, c.body)).sum +
          go(depth, returnTpe)
      case Term.Uni(_) | Term.Meta(_) =>
        0
    go(0, term)

  /** Names of equality hypotheses in the context. */
  private def eqHypNames(ctx: Context): List[String] =
    ctx.entries.collect { case e if Eq.extract(e.tpe).isDefined => e.name }

  private def extractIndName(t: Term): Option[String] = t match
    case Term.Ind(name, _, _) => Some(name)
    case _                    => None

  /** Cartesian product of lists. */
  private def cartesian[A](lists: List[List[A]]): List[List[A]] = lists match
    case Nil          => List(Nil)
    case head :: tail =>
      for x <- head; rest <- cartesian(tail) yield x :: rest
