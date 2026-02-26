package sproof.checker

import sproof.core.{Term, Context, Subst, GlobalEnv, MatchCase}
import sproof.eval.{Semantic, Eval, Quote, Env}

/** Bidirectional type checker for predicative CIC.
 *
 *  Two modes:
 *  - check(ctx, term, expected): verify `term` has type `expected`
 *  - infer(ctx, term): synthesize the type of `term`
 *
 *  Definitional equality is decided by NbE (eval + quote + alpha-eq).
 *
 *  Both modes take `using env: GlobalEnv` to resolve constructor and inductive
 *  type information.  The companion object of `GlobalEnv` provides a default
 *  empty environment so existing callers compile unchanged.
 */
object Bidirectional:

  /** Check that `term` has type `expected` in context `ctx`. */
  def check(ctx: Context, term: Term, expected: Term)(using env: GlobalEnv): Either[TypeError, Unit] =
    // When expected is already a syntactic Pi, use it directly to avoid NbE round-trip:
    // Eval.eval(Ind(...)) → SNeu(NVar(-1), Nil) → quote → Var(wrong), which mangles inductive
    // type names in Pi domains.  Pi is already a value (whnf), so no reduction is needed.
    val normExpected = expected match
      case _: Term.Pi => expected
      case _          => whnf(ctx, expected)
    (term, normExpected) match
      // η-expansion: check a lambda against a Pi type
      case (Term.Lam(x, annTp, body), Term.Pi(_, dom, cod)) =>
        for
          _ <- inferUniverse(ctx, annTp)
          _ <- convCheck(ctx, annTp, dom, term)
          extCtx = ctx.extend(x, annTp)
          _ <- check(extCtx, body, Subst.open(cod, Term.Var(0)))
        yield ()

      // Let-in: check defn first, then extend context
      case (Term.Let(x, tp, defn, body), _) =>
        for
          _ <- inferUniverse(ctx, tp)
          _ <- check(ctx, defn, tp)
          extCtx = ctx.extendDef(x, tp, defn)
          _ <- check(extCtx, body, Subst.shift(1, expected))
        yield ()

      // Fall through: infer type and check conversion
      case _ =>
        for
          actual <- infer(ctx, term)
          _      <- convCheck(ctx, actual, expected, term)
        yield ()

  /** Infer (synthesize) the type of `term` in context `ctx`. */
  def infer(ctx: Context, term: Term)(using env: GlobalEnv): Either[TypeError, Term] = term match
    case Term.Var(i) =>
      ctx.lookup(i).toRight(TypeError.UnboundVariable(i, ctx))

    case Term.Uni(l) =>
      if l > 100 then Left(TypeError.UniverseOverflow(l))
      else Right(Term.Uni(l + 1))

    case Term.Pi(x, dom, cod) =>
      for
        l1    <- inferUniverse(ctx, dom)
        extCtx = ctx.extend(x, dom)
        l2    <- inferUniverse(extCtx, cod)
      yield Term.Uni(math.max(l1, l2))

    case Term.App(fn, arg) =>
      for
        fnTp   <- infer(ctx, fn)
        piNorm  = whnf(ctx, fnTp)
        result <- piNorm match
          case Term.Pi(_, dom, cod) =>
            check(ctx, arg, dom).map(_ => Subst.subst(arg, cod))
          case _ =>
            Left(TypeError.NotAFunction(fn, fnTp, ctx))
      yield result

    case Term.Lam(x, tpe, body) =>
      for
        _     <- inferUniverse(ctx, tpe)
        extCtx = ctx.extend(x, tpe)
        bTp   <- infer(extCtx, body)
      yield Term.Pi(x, tpe, bTp)

    case Term.Let(x, tp, defn, body) =>
      for
        _      <- inferUniverse(ctx, tp)
        _      <- check(ctx, defn, tp)
        extCtx  = ctx.extendDef(x, tp, defn)
        bTp    <- infer(extCtx, body)
      yield Subst.subst(defn, bTp)

    case Term.Fix(name, tpe, body) =>
      for
        _     <- inferUniverse(ctx, tpe)
        extCtx = ctx.extend(name, tpe)
        _     <- check(extCtx, body, Subst.shift(1, tpe))
      yield tpe

    case Term.Con("refl", "Eq", List(a)) =>
      // Special case: refl(a) : Eq a a (2-arg form, no explicit type arg).
      // This matches the 2-arg Eq encoding used by the Elaborator for propositions.
      // The type arg is omitted; bidirectional equality check uses only lhs/rhs.
      Right(Term.App(Term.App(Term.Ind("Eq", Nil, Nil), a), a))

    case Term.Con(name, indRef, args) =>
      IndChecker.inferCon(ctx, name, indRef, args)

    case Term.Meta(id) =>
      Left(TypeError.UnresolvedMeta(id))

    case Term.Ind(name, _, _) =>
      env.lookupInd(name) match
        case Some(indDef) => Right(Term.Uni(indDef.universe))
        case None         => Right(Term.Uni(0))  // fallback for anonymous/unknown Ind

    case Term.Mat(scrutinee, cases, returnTpe) =>
      IndChecker.checkMat(ctx, scrutinee, cases, returnTpe)

  // ---- Helpers ----

  /** Infer the universe level of a type expression `t`. Returns l if t : Type_l. */
  def inferUniverse(ctx: Context, t: Term)(using env: GlobalEnv): Either[TypeError, Int] =
    infer(ctx, t).flatMap { tp =>
      whnf(ctx, tp) match
        case Term.Uni(l) => Right(l)
        case _           => Left(TypeError.NotAType(t, ctx))
    }

  /** Weak-head normal form via NbE round-trip. */
  def whnf(ctx: Context, t: Term): Term =
    Quote.normalize(ctx.size, buildEnv(ctx), t)

  /** Check definitional equality of `actual` and `expected`. */
  private def convCheck(ctx: Context, actual: Term, expected: Term, term: Term): Either[TypeError, Unit] =
    val env = buildEnv(ctx)
    if Quote.convEqual(ctx.size, env, actual, expected) then Right(())
    else Left(TypeError.TypeMismatch(expected, actual, term, ctx))

  /** Build the NbE environment from a typing context (each var becomes a neutral). */
  private def buildEnv(ctx: Context): Env =
    (0 until ctx.size).toList.map(i => Semantic.freshVar(ctx.size - 1 - i))
