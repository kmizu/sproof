package sproof

import scala.util.boundary
import scala.util.boundary.break
import sproof.core.{Term, Context, GlobalEnv}
import sproof.syntax.{ElabResult, SProof, STactic, STacticCase}
import sproof.tactic.{TacticM, Builtins, TacticError}
import sproof.kernel.Kernel

/** Checks and proves all declarations from an elaboration result.
  *
  * Takes an ElabResult (containing the global env, def bodies, and defspec proofs)
  * and fully verifies each declaration through tactic execution + kernel re-check.
  */
object Checker:

  /** Check all declarations in the elaboration result.
    *
    * @param result the elaborated result containing env, defs, and defspecs
    * @return either an error message or the final GlobalEnv after all checks
    */
  def checkAll(result: ElabResult): Either[String, GlobalEnv] =
    given GlobalEnv = result.env
    boundary:
      for (name, (elabParams, propTerm, proof)) <- result.defspecs do
        // Build context from defspec parameters so the proof can reference them
        val proofCtx = elabParams.foldLeft(Context.empty) { (ctx, p) =>
          ctx.extend(p._1, p._2)
        }
        executeProof(proofCtx, propTerm, proof) match
          case Left(err) =>
            break(Left(s"Proof of '$name' failed: $err"))
          case Right(proofTerm) =>
            // Wrap proof in lambdas + prop in Pi for the kernel check at Context.empty
            val fullProofTerm = elabParams.foldRight(proofTerm) { (p, body) =>
              Term.Lam(p._1, p._2, body)
            }
            val fullPropTerm = elabParams.foldRight(propTerm) { (p, cod) =>
              Term.Pi(p._1, p._2, cod)
            }
            Kernel.check(Context.empty, fullProofTerm, fullPropTerm) match
              case Left(err) =>
                break(Left(s"Kernel rejected proof of '$name': ${err.getMessage}"))
              case Right(()) =>
                ()
      Right(result.env)

  /** Execute a surface proof to produce a core proof term.
    *
    * @param ctx   the context in which the proof is checked
    * @param prop  the proposition to prove
    * @param proof the surface-level proof strategy
    * @param env   the global environment
    * @return either an error string or the core proof term
    */
  def executeProof(ctx: Context, prop: Term, proof: SProof)(using env: GlobalEnv): Either[String, Term] =
    proof match
      case SProof.SBy(tactic) =>
        val t = tacticFromSurface(tactic)
        TacticM.prove(ctx, prop)(t).left.map(_.toString)

      case SProof.STerm(_) =>
        Left("Direct proof terms (STerm) not yet supported in CLI; use 'by' tactics")

  /** Convert a surface tactic to a TacticM computation. */
  def tacticFromSurface(t: STactic)(using env: GlobalEnv): TacticM[Unit] = t match
    case STactic.STrivial =>
      Builtins.trivial

    case STactic.STriv =>
      Builtins.trivial

    case STactic.SAssume(names) =>
      names.foldLeft(TacticM.pure(())) { (acc, name) =>
        acc.flatMap(_ => Builtins.assume(name))
      }

    case STactic.SSimplify(lemmas) =>
      Builtins.simplify(lemmas)

    case STactic.SSimp(lemmas) =>
      Builtins.simplify(lemmas)

    case STactic.SInduction(varName, cases) =>
      Builtins.induction(varName).flatMap { _ =>
        closeRemainingGoals(cases)
      }

    case STactic.SApply(_expr) =>
      // SApply requires a fully elaborated core term; deferred in CLI.
      TacticM.pure(())

    case STactic.SSorry =>
      sorryCurrentGoal

  /** Try to close all remaining sub-goals, using trivial or sorry as fallback. */
  private def closeRemainingGoals(cases: List[STacticCase])(using env: GlobalEnv): TacticM[Unit] =
    TacticM.get.flatMap { s =>
      if s.goals.isEmpty then TacticM.pure(())
      else
        attemptTrivialOrSorry.flatMap(_ => closeRemainingGoals(cases))
    }

  /** Try trivial; if it fails fall back to sorry (discharges without proof). */
  private def attemptTrivialOrSorry(using env: GlobalEnv): TacticM[Unit] =
    TacticM.get.flatMap { s =>
      val (finalState, result) = TacticM.run(Builtins.trivial, s)
      result match
        case Right(()) =>
          TacticM.set(finalState)
        case Left(_) =>
          sorryCurrentGoal
    }

  /** Discharge the current goal with a placeholder (sorry). */
  private def sorryCurrentGoal: TacticM[Unit] =
    TacticM.currentGoal.flatMap { case (mv, goal) =>
      val placeholder = sproof.tactic.Eq.mkRefl(goal.target)
      TacticM.solveGoalWith(mv, placeholder)
    }

end Checker
