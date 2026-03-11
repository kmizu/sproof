package sroof.checker

import sroof.core.{Term, Context, Subst, GlobalEnv, MatchCase}
import sroof.eval.{Semantic, Eval, Quote, Env, EnvBuilder}

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
          _ <- check(extCtx, body, cod)
        yield ()

      // Let-in: check defn first, then extend context
      case (Term.Let(x, tp, defn, body), _) =>
        for
          _ <- inferUniverse(ctx, tp)
          _ <- check(ctx, defn, tp)
          extCtx = ctx.extendDef(x, tp, defn)
          _ <- check(extCtx, body, Subst.shift(1, expected))
        yield ()

      // Mat with placeholder return type (Meta(-1)): use expected as the return type.
      // Elaborated match expressions use Meta(-1) as a sentinel meaning "to be determined
      // from context".  Rather than failing when whnf hits the sentinel, substitute the
      // concrete expected type and check the case bodies against it.
      case (Term.Mat(scrutinee, cases, Term.Meta(id)), _) if id < 0 =>
        IndChecker.checkMat(ctx, scrutinee, cases, expected).map(_ => ())

      // Con in check mode: when the expected type is an applied parameterized inductive
      // that is registered in GlobalEnv, extract param values from the expected type
      // and use them for dependent checking.
      // This enables `Sigma.mk(a, b)` to type-check against `Sigma(A)(B)`.
      // Note: Eq is NOT in GlobalEnv (it is a built-in), so this path is skipped for refl.
      case (Term.Con(name, indRef, args), _)
           if env.lookupInd(indRef).exists(_.params.nonEmpty) =>
        val paramVals = IndChecker.extractParamsFromExpected(normExpected, indRef)
        if paramVals.nonEmpty then
          IndChecker.inferConWithParams(ctx, name, indRef, args, paramVals).flatMap { actual =>
            convCheck(ctx, actual, expected, term)
          }
        else
          for
            actual <- infer(ctx, term)
            _      <- convCheck(ctx, actual, expected, term)
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
        // Optimization: if fnTp is already syntactically a Pi, skip the NbE round-trip.
        // whnf(Pi(...)) would quote the Pi body through fresh variables, introducing Mat
        // terms into the inferred type that break convCheck against a syntactic expected type.
        // This mirrors the same shortcut already present in `check` for the expected type.
        piNorm  = fnTp match
                    case _: Term.Pi => fnTp
                    case _          => whnf(ctx, fnTp)
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
        case Some(indDef) =>
          // Parameterized inductives return a Pi type: List : Type → Type, etc.
          val base: Term = Term.Uni(indDef.universe)
          val withParams = indDef.params.foldRight(base) { (p, acc) =>
            Term.Pi(p.name, p.tpe, acc)
          }
          Right(withParams)
        case None => Right(Term.Uni(0))  // fallback for anonymous/unknown Ind (e.g. Eq)

    case Term.Mat(scrutinee, cases, returnTpe) =>
      IndChecker.checkMat(ctx, scrutinee, cases, returnTpe)

  // ---- Helpers ----

  /** Infer the universe level of a type expression `t`. Returns l if t : Type_l.
   *
   *  Special case: Eq type applications `Eq a b` or `Eq a` are treated as Prop-level
   *  (universe 0).  The bidirectional checker cannot infer through `App(Ind("Eq",...), ...)`
   *  because `Ind("Eq",...)` is NOT registered in GlobalEnv (it is a built-in), so it
   *  types as `Uni(0)` (not a Pi type).  This shortcut avoids the NotAFunction error.
   *
   *  For user-defined parameterized inductives (List, PolyList, Sigma, etc.), their
   *  `Ind(name)` now properly returns a Pi type, so `App(Ind("List"), A)` infers
   *  through the normal App case and returns `Uni(0)`.
   */
  def inferUniverse(ctx: Context, t: Term)(using env: GlobalEnv): Either[TypeError, Int] =
    t match
      case Term.App(Term.App(Term.Ind("Eq", _, _), _), _) => Right(0)
      case Term.App(Term.Ind("Eq", _, _), _)              => Right(0)
      // Bare Ind(name) used as a type: always return the declared universe.
      // This preserves backward compat for inductives like Vec used as bare types
      // (without applying type params/indices), even though infer(Ind(name)) now
      // returns a Pi type for parameterized inductives.
      case Term.Ind(name, _, _) =>
        env.lookupInd(name) match
          case Some(indDef) => Right(indDef.universe)
          case None         => Right(0)
      case _ =>
        infer(ctx, t).flatMap { tp =>
          whnf(ctx, tp) match
            case Term.Uni(l) => Right(l)
            case _           => Left(TypeError.NotAType(t, ctx))
        }

  /** Weak-head normal form via NbE round-trip. */
  def whnf(ctx: Context, t: Term): Term =
    Quote.normalize(ctx.size, EnvBuilder.fromContext(ctx), t)

  /** Check definitional equality of `actual` and `expected` (with universe cumulativity). */
  private def convCheck(ctx: Context, actual: Term, expected: Term, term: Term): Either[TypeError, Unit] =
    val env = EnvBuilder.fromContext(ctx)
    // Universe cumulativity: Type_i ≤ Type_j when i ≤ j
    val normActual   = whnf(ctx, actual)
    val normExpected = whnf(ctx, expected)
    (normActual, normExpected) match
      case (Term.Uni(l1), Term.Uni(l2)) if l1 <= l2 => Right(())
      case _ =>
        if Quote.convEqual(ctx.size, env, actual, expected) then Right(())
        else Left(TypeError.TypeMismatch(expected, actual, term, ctx))

