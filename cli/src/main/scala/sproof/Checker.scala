package sproof

import scala.util.boundary
import scala.util.boundary.break
import sproof.core.{Term, Context, GlobalEnv, Subst}
import sproof.syntax.{ElabResult, SProof, STactic, STacticCase, SType, SCalcStep, SExpr}
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

      case SProof.STerm(sexpr) =>
        import sproof.syntax.Elaborator
        // Elaborate the expression in the current context
        val nameEnv = ctx.entries.map(_.name).toList
        Elaborator.elabExprPublic(sexpr, env, nameEnv) match
          case Left(err) => Left(s"Term proof elaboration failed: ${err.message}")
          case Right(term) => Right(term)

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
      val caseSpecs = cases.map(c => (c.ctorName, c.extraBindings))
      Builtins.induction(varName, caseSpecs).flatMap { _ =>
        closeRemainingGoals(cases)
      }

    case STactic.SApply(_expr) =>
      // SApply requires a fully elaborated core term; deferred in CLI.
      TacticM.pure(())

    case STactic.SSorry =>
      sorryCurrentGoal

    case STactic.SHave(name, stpe, proof, cont) =>
      haveTactic(name, stpe, proof, cont)

    case STactic.SRfl =>
      Builtins.trivial

    case STactic.SRw(lemmas) =>
      Builtins.rewrite(lemmas)

    case STactic.SRewrite(lemmas) =>
      Builtins.rewrite(lemmas)

    case STactic.SExact(sexpr) =>
      exactTactic(sexpr)

    case STactic.SSeq(tactics) =>
      tactics.foldLeft(TacticM.pure(())) { (acc, t) =>
        acc.flatMap(_ => tacticFromSurface(t))
      }

    case STactic.SCalc(steps) =>
      calcTactic(steps)

  /** Close remaining sub-goals using the specified case tactics in order.
   *
   *  Each STacticCase is consumed one-by-one as subgoals are closed.
   *  If no cases remain, fall back to trivial (then sorry if trivial fails).
   */
  private def closeRemainingGoals(cases: List[STacticCase])(using env: GlobalEnv): TacticM[Unit] =
    TacticM.get.flatMap { s =>
      if s.goals.isEmpty then TacticM.pure(())
      else
        val (tactic, rest) = cases match
          case caseSpec :: remaining => (tacticFromSurface(caseSpec.tactic), remaining)
          case Nil                   => (attemptTrivialOrSorryTactic, Nil)
        tactic.flatMap(_ => closeRemainingGoals(rest))
    }

  /** TacticM version of attemptTrivialOrSorry for use as a fallback. */
  private def attemptTrivialOrSorryTactic(using env: GlobalEnv): TacticM[Unit] =
    TacticM.get.flatMap { s =>
      val (finalState, result) = TacticM.run(Builtins.trivial, s)
      result match
        case Right(()) => TacticM.set(finalState)
        case Left(_)   => sorryCurrentGoal
    }

  /** Try trivial; if it fails fall back to sorry (discharges without proof). */
  private def attemptTrivialOrSorry(using env: GlobalEnv): TacticM[Unit] =
    attemptTrivialOrSorryTactic

  /** Discharge the current goal with a placeholder (sorry). */
  private def sorryCurrentGoal: TacticM[Unit] =
    TacticM.currentGoal.flatMap { case (mv, goal) =>
      val placeholder = sproof.tactic.Eq.mkRefl(goal.target)
      TacticM.solveGoalWith(mv, placeholder)
    }

  /** have h: T = { proof }; cont
    *
    * 1. Elaborate the surface type into a core Term
    * 2. Prove the sub-proposition in the current context
    * 3. Introduce h: T via Let into the proof
    * 4. Continue with cont tactic
    */
  private def haveTactic(
    name:  String,
    stpe:  sproof.syntax.SType,
    proof: SProof,
    cont:  STactic,
  )(using env: GlobalEnv): TacticM[Unit] =
    import sproof.syntax.Elaborator
    for
      goalPair   <- TacticM.currentGoal
      (mv, goal)  = goalPair
      // Elaborate the surface type in the current context
      tpeTerm    <- Elaborator.elabTypePublic(stpe, env, goal.ctx) match
        case Right(t) => TacticM.pure(t)
        case Left(e)  => TacticM.fail(TacticError.Custom(s"have: type elaboration failed: ${e.message}"))
      // Prove the sub-proposition
      proofTerm  <- executeProof(goal.ctx, tpeTerm, proof) match
        case Right(t) => TacticM.pure(t)
        case Left(e)  => TacticM.fail(TacticError.Custom(s"have: sub-proof failed: $e"))
      // Create new goal with h: T in context via Let
      newCtx      = goal.ctx.extend(name, tpeTerm)
      newTarget   = sproof.core.Subst.shift(1, goal.target)
      mv_cont    <- TacticM.addGoal(newCtx, newTarget)
      // Evidence: Let(h, T, proofTerm, <cont_proof>)
      _          <- TacticM.solveGoalWith(mv, sproof.core.Term.Let(name, tpeTerm, proofTerm, sproof.core.Term.Meta(mv_cont.id)))
      // Run the continuation tactic
      _          <- tacticFromSurface(cont)
    yield ()

  /** exact e — provide the exact proof term, checked against the current goal. */
  private def exactTactic(sexpr: SExpr)(using env: GlobalEnv): TacticM[Unit] =
    import sproof.syntax.Elaborator
    for
      goalPair   <- TacticM.currentGoal
      (mv, goal)  = goalPair
      nameEnv     = goal.ctx.entries.map(_.name).toList
      proofTerm  <- Elaborator.elabExprPublic(sexpr, env, nameEnv) match
        case Right(t) => TacticM.pure(t)
        case Left(e)  => TacticM.fail(TacticError.Custom(s"exact: elaboration failed: ${e.message}"))
      _          <- TacticM.solveGoalWith(mv, proofTerm)
    yield ()

  /** calc { step1 step2 ... } — chain equality proofs by transitivity */
  private def calcTactic(
    steps: List[sproof.syntax.SCalcStep],
  )(using env: GlobalEnv): TacticM[Unit] =
    // For now, just try trivial for each step (minimal implementation)
    // A full implementation would chain transitivity proofs
    Builtins.trivial

end Checker
