package sproof.agent

import sproof.core.{Term, Context, GlobalEnv}
import sproof.syntax.{SProof, STactic}
import sproof.Checker

/** Searches for a proof of a goal by trying generated tactic candidates.
 *
 *  Tries each candidate in order; returns the first STactic that succeeds.
 *  Returns None if no candidate closes the goal within the candidate list.
 */
object SearchLoop:

  /** Try to find a proof of `prop` in context `ctx`.
   *
   *  @param ctx    the typing context (parameters of the defspec)
   *  @param prop   the proposition to prove
   *  @param env    the global environment
   *  @return Some(tactic) if a proof is found, None otherwise
   */
  def search(ctx: Context, prop: Term)(using env: GlobalEnv): Option[STactic] =
    val candidates = TacticGen.candidates(ctx, prop)
    candidates.find { tac =>
      Checker.executeProof(ctx, prop, SProof.SBy(tac)) match
        case Right(_) => true
        case Left(_)  => false
    }
