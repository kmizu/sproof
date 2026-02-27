package sproof.tactic

import cats.data.{EitherT, State}
import sproof.core.{Term, Context}

// Type alias to avoid kind-projector in Scala 3
private type StatePS[A] = State[ProofState, A]

/** TacticM: the proof construction monad.
 *
 *  A tactic is a computation that transforms a ProofState, possibly failing
 *  with a TacticError.  Built from cats.data.EitherT + State.
 *
 *  Usage pattern ("defspec ... program = { ... }"):
 *  {{{
 *    TacticM.prove(ctx, goalType) {
 *      Builtins.trivial   // or any sequence of tactics
 *    }
 *  }}}
 */
type TacticM[A] = EitherT[StatePS, TacticError, A]

object TacticM:

  // ---- Primitives ----

  def pure[A](a: A): TacticM[A] =
    EitherT.pure[StatePS, TacticError](a)

  def fail[A](err: TacticError): TacticM[A] =
    EitherT.leftT[StatePS, A](err)

  def get: TacticM[ProofState] =
    EitherT.liftF[StatePS, TacticError, ProofState](State.get)

  def set(s: ProofState): TacticM[Unit] =
    EitherT.liftF[StatePS, TacticError, Unit](State.set(s))

  def modify(f: ProofState => ProofState): TacticM[Unit] =
    EitherT.liftF[StatePS, TacticError, Unit](State.modify(f))

  /** Lift an Either into TacticM. */
  def liftEither[A](e: Either[TacticError, A]): TacticM[A] =
    EitherT.fromEither[StatePS](e)

  // ---- Goal operations ----

  /** Return the current (first) goal, or fail with NoGoals. */
  def currentGoal: TacticM[(MetaVar, Goal)] =
    get.flatMap { s =>
      s.goals.headOption match
        case Some(g) => pure(g)
        case None    => fail(TacticError.NoGoals)
    }

  /** Solve a specific goal with a proof term.  Removes it from the goal list. */
  def solveGoalWith(mv: MetaVar, term: Term): TacticM[Unit] =
    modify { s =>
      s.copy(
        goals    = s.goals.filterNot(_._1 == mv),
        evidence = s.evidence + (mv -> term),
      )
    }

  /** Add a new subgoal and return its fresh MetaVar. */
  def addGoal(ctx: Context, target: Term): TacticM[MetaVar] =
    for
      s  <- get
      mv  = MetaVar(s.nextMeta)
      _  <- set(s.copy(
               goals    = s.goals :+ (mv -> Goal(ctx, target)),
               nextMeta = s.nextMeta + 1,
             ))
    yield mv

  // ---- Runner ----

  /** Run a tactic on a given proof state. */
  def run[A](tactic: TacticM[A], initial: ProofState): (ProofState, Either[TacticError, A]) =
    val (finalState, result) = tactic.value.run(initial).value
    (finalState, result)

  /** Prove `target` using `tactic`.  Returns the evidence term on success.
   *
   *  This is the top-level entry point for "defspec ... program = { ... }".
   *  The returned term is subsequently re-checked by the trusted Kernel.
   *
   *  After all tactics complete, all solved metavariables are substituted into
   *  the main proof term (analogous to unification variable instantiation).
   */
  def prove(ctx: Context, target: Term)(tactic: TacticM[Unit]): Either[TacticError, Term] =
    proveWithState(ctx, target)(tactic).left.map(_._1)

  /** Like `prove`, but returns the final `ProofState` alongside any `TacticError`.
   *
   *  On failure, the returned ProofState captures the unsolved goals and evidence
   *  at the point of failure — useful for generating diagnostic output.
   */
  def proveWithState(ctx: Context, target: Term)(tactic: TacticM[Unit]): Either[(TacticError, ProofState), Term] =
    val mv           = MetaVar(0)
    val initialGoal  = Goal(ctx, target)
    val initialState = ProofState(List(mv -> initialGoal), Map.empty, 1)
    val (finalState, result) = run(tactic, initialState)
    result match
      case Left(err) => Left((err, finalState))
      case Right(_) =>
        if finalState.goals.nonEmpty then
          Left((TacticError.Custom(
            s"Proof incomplete: ${finalState.goals.length} goal(s) remaining"
          ), finalState))
        else
          finalState.evidence.get(mv) match
            case Some(term) => Right(substEvidence(finalState.evidence, term))
            case None       => Left((TacticError.Custom("Main goal was not solved"), finalState))

  /** Substitute all solved metavariables into a term (fixpoint until stable). */
  private def substEvidence(ev: Map[MetaVar, Term], t: Term): Term =
    import sproof.core.Term as T
    def go(t: T): T = t match
      case T.Meta(i)              => ev.get(MetaVar(i)).map(go).getOrElse(t)
      case T.Var(_) | T.Uni(_)    => t
      case T.App(f, a)            => T.App(go(f), go(a))
      case T.Lam(x, tp, b)       => T.Lam(x, go(tp), go(b))
      case T.Pi(x, d, c)         => T.Pi(x, go(d), go(c))
      case T.Let(x, tp, df, b)   => T.Let(x, go(tp), go(df), go(b))
      case T.Con(n, r, args)      => T.Con(n, r, args.map(go))
      case T.Fix(n, tp, b)       => T.Fix(n, go(tp), go(b))
      case T.Ind(n, ps, cs)      =>
        T.Ind(n,
          ps.map(p => sproof.core.Param(p.name, go(p.tpe))),
          cs.map(c => sproof.core.Ctor(c.name, go(c.tpe))))
      case T.Mat(s, cases, rt)   =>
        T.Mat(go(s), cases.map(mc => mc.copy(body = go(mc.body))), go(rt))
    go(t)
