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
      if maxDepth >= 1 then depth1(ctx, target, d0.map(_.tactic))
      else Nil
    d0 ++ d1

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

  private def depth1(ctx: Context, target: Term, subTactics: List[STactic])(using env: GlobalEnv): List[ScoredCandidate] =
    inductiveVars(ctx).flatMap { (varName, indDef) =>
      buildInductionCandidates(varName, indDef, subTactics).map { tac =>
        ScoredCandidate(tac, scoreInduction(indDef), tacticKey(tac))
      }
    }

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
        withIH :: withoutIH
    }
    cartesian(perCtorOptions).map(cases => STactic.SInduction(varName, cases))

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

  /** Names of equality hypotheses in the context. */
  private def eqHypNames(ctx: Context): List[String] =
    ctx.entries.collect { case e if Eq.extract(e.tpe).isDefined => e.name }

  /** Pairs of (varName, IndDef) for inductive-typed variables in context. */
  private def inductiveVars(ctx: Context)(using env: GlobalEnv): List[(String, IndDef)] =
    ctx.entries.flatMap { e =>
      extractIndName(e.tpe).flatMap(env.lookupInd).map(e.name -> _)
    }

  private def extractIndName(t: Term): Option[String] = t match
    case Term.Ind(name, _, _) => Some(name)
    case _                    => None

  /** Cartesian product of lists. */
  private def cartesian[A](lists: List[List[A]]): List[List[A]] = lists match
    case Nil          => List(Nil)
    case head :: tail =>
      for x <- head; rest <- cartesian(tail) yield x :: rest
