package sproof.kernel

import sproof.core.{Term, Context, GlobalEnv}
import sproof.checker.{Bidirectional, TypeError}
import sproof.eval.{Quote, Eval, Env, Semantic}
import sproof.tactic.Eq

/** The trusted kernel: the sole source of proof validity in sproof.
 *
 *  Every completed proof MUST pass through `Kernel.check` before being
 *  accepted.  Tactics (TacticM, Builtins) are NOT trusted — they are
 *  proof-term generators.  The kernel independently re-verifies that the
 *  generated term has the claimed type.
 *
 *  Size target: < 100 lines of logic (this is the Trusted Computing Base).
 *
 *  "仕様を書いて証明プログラムが型エラーになれば失敗が明確":
 *  This object is the type-checker that enforces that invariant.
 */
object Kernel:

  /** Check that `proof` has type `claimedType` in context `ctx`.
   *
   *  Returns Right(()) on success, Left(TypeError) on failure.
   *  Special-cases the Eq/refl encoding used in Phase 2.
   */
  def check(ctx: Context, proof: Term, claimedType: Term)(using env: GlobalEnv): Either[TypeError, Unit] =
    // Special case: refl(a) must have type Eq T a a.
    // We do NOT whnf the claimed type here because our NbE evaluation of Ind("Eq",...)
    // loses the constructor name — use the raw syntactic structure of claimedType instead.
    (proof, Eq.extract(claimedType)) match
      case (Term.Con("refl", "Eq", List(a)), Some(triple)) =>
        val (tpe, lhs, rhs) = triple
        val env = buildEnv(ctx)
        // refl(a) : Eq T a a  iff  a ≡ lhs ≡ rhs (definitionally)
        if Quote.convEqual(ctx.size, env, a, lhs) &&
           Quote.convEqual(ctx.size, env, lhs, rhs) then
          // Also verify a : T, but skip when T is Meta(-1) (unknown, from 2-arg Eq form).
          // In that case, a was already type-checked by the bidirectional checker.
          tpe match
            case Term.Meta(_) => Right(())
            case _            => Bidirectional.check(ctx, a, tpe)
        else
          Left(TypeError.TypeMismatch(claimedType, proof, proof, ctx))

      case _ =>
        // General case: delegate to the bidirectional checker
        Bidirectional.check(ctx, proof, claimedType)

  /** Infer the type of a term; re-export for convenience. */
  def infer(ctx: Context, term: Term)(using env: GlobalEnv): Either[TypeError, Term] =
    Bidirectional.infer(ctx, term)

  private def buildEnv(ctx: Context): Env =
    (0 until ctx.size).toList.map(i => Semantic.freshVar(ctx.size - 1 - i))
