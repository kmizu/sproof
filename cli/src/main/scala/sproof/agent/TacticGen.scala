package sproof.agent

import sproof.core.{Term, Context, GlobalEnv, IndDef, CtorDef}
import sproof.syntax.{STactic, STacticCase}
import sproof.tactic.Eq

/** Generates candidate STactic values for a given proof goal.
 *
 *  Two levels of depth:
 *   - Depth 0: trivial, assumption, simplify[h] for each equality hypothesis
 *   - Depth 1: induction on each inductive variable, with depth-0 sub-proofs
 *
 *  Designed for the proof agent loop: ordered by expected success probability.
 */
object TacticGen:

  /** Generate candidate tactics for a goal, shallowest first. */
  def candidates(ctx: Context, target: Term)(using env: GlobalEnv): List[STactic] =
    val d0 = depth0(ctx, target)
    val d1 = depth1(ctx, target, d0)
    d0 ++ d1

  // ---- Depth 0: single-step tactics ----

  private def depth0(ctx: Context, target: Term)(using env: GlobalEnv): List[STactic] =
    List(STactic.STrivial, STactic.SAssumption) ++
    eqHypNames(ctx).map(h => STactic.SSimplify(List(h)))

  // ---- Depth 1: induction with depth-0 sub-proofs ----

  private def depth1(ctx: Context, target: Term, subTactics: List[STactic])(using env: GlobalEnv): List[STactic] =
    inductiveVars(ctx).flatMap { (varName, indDef) =>
      buildInductionCandidates(varName, indDef, subTactics)
    }

  /** For each inductive type constructor, build STacticCase options.
   *
   *  - Non-recursive ctor (e.g. zero): try each sub-tactic
   *  - Recursive ctor (e.g. succ k): try with IH — bindings = [k, ih], tactic = simplify[ih] or each sub-tactic
   */
  private def buildInductionCandidates(
    varName: String,
    indDef:  IndDef,
    subTactics: List[STactic],
  ): List[STactic] =
    // For each constructor, generate a list of STacticCase options
    val perCtorOptions: List[List[STacticCase]] = indDef.ctors.map { ctor =>
      if ctor.argTpes.isEmpty then
        // Non-recursive: try each sub-tactic
        subTactics.map(t => STacticCase(ctor.name, Nil, t))
      else
        // Recursive: try with IH (simplify[ih]) + sub-tactics without IH
        val argNames  = ctor.argTpes.indices.map(i => s"_arg$i").toList
        val withIH    = STacticCase(ctor.name, argNames :+ "ih", STactic.SSimplify(List("ih")))
        val withoutIH = subTactics.map(t => STacticCase(ctor.name, argNames, t))
        withIH :: withoutIH
    }
    // Cross product: pick one option per constructor
    cartesian(perCtorOptions).map(cases => STactic.SInduction(varName, cases))

  // ---- Helpers ----

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
