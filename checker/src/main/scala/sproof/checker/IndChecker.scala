package sproof.checker

import sproof.core.{Term, Context, Subst, GlobalEnv, IndDef, CtorDef, MatchCase}

/** Type-checking rules for inductive types: constructors (Con) and eliminators (Mat).
 *
 *  This module is NOT part of the trusted kernel.
 *  Every proof term it produces is re-checked by `Kernel.check`.
 *
 *  Design note: both methods take `using env: GlobalEnv` as a contextual parameter
 *  so they can look up constructor types and inductive definitions.
 */
object IndChecker:

  // ---- Con: constructor type inference ----

  /** Infer the type of a constructor application `Con(name, indRef, args)`.
   *
   *  Returns `Ind(indRef, ...)` if all argument types check out.
   */
  def inferCon(
    ctx:    Context,
    name:   String,
    indRef: String,
    args:   List[Term],
  )(using env: GlobalEnv): Either[TypeError, Term] =
    env.lookupInd(indRef) match
      case None =>
        Left(TypeError.Custom(s"Unknown inductive type '$indRef'"))
      case Some(indDef) =>
        indDef.ctors.find(_.name == name) match
          case None =>
            Left(TypeError.Custom(s"Unknown constructor '$name' for inductive type '$indRef'"))
          case Some(ctorDef) =>
            if args.length != ctorDef.argTpes.length then
              Left(TypeError.Custom(
                s"Constructor '$name' expects ${ctorDef.argTpes.length} argument(s), " +
                s"got ${args.length}"
              ))
            else
              checkArgs(ctx, args, ctorDef.argTpes).map { _ =>
                // The type of any constructor is the inductive type itself
                Term.Ind(indRef, indDef.params, Nil)
              }

  // ---- Mat: match expression type checking ----

  /** Check a match expression `Mat(scrutinee, cases, returnTpe)`.
   *
   *  - Infers the type of the scrutinee and looks up its inductive definition.
   *  - Verifies that the cases are exactly the constructors of that type (exhaustive).
   *  - Checks each case body against `returnTpe` in the appropriately extended context.
   *  - Returns `returnTpe` as the overall type.
   */
  def checkMat(
    ctx:       Context,
    scrutinee: Term,
    cases:     List[MatchCase],
    returnTpe: Term,
  )(using env: GlobalEnv): Either[TypeError, Term] =
    for
      scrutTpe <- Bidirectional.infer(ctx, scrutinee)
      // NOTE: do NOT call whnf here — Eval.eval converts Ind(...) to a neutral SNeu(NVar(-1), Nil),
      // destroying the inductive type name.  Use the raw inferred type directly.
      result   <- extractEq(scrutTpe) match
                    case Some((_, lhs, rhs)) =>
                      // J-rule: Eq elimination.  returnTpe is the motive function P.
                      // Overall type = App(P, rhs).  Branch body checked against App(P, lhs).
                      checkEqElim(ctx, lhs, rhs, cases, returnTpe)
                    case None =>
                      for
                        indName <- extractIndName(scrutTpe)
                        indDef  <- env.lookupInd(indName).toRight(
                                     TypeError.Custom(s"Unknown inductive type '$indName'")
                                   )
                        _       <- checkCoverage(cases, indDef)
                        _       <- checkCases(ctx, scrutinee, cases, indDef, returnTpe)
                      yield returnTpe
    yield result

  // ---- private helpers ----

  /** Check each argument against its expected type (pairwise). */
  private def checkArgs(
    ctx:  Context,
    args: List[Term],
    tpes: List[Term],
  )(using env: GlobalEnv): Either[TypeError, Unit] =
    args.zip(tpes).foldLeft[Either[TypeError, Unit]](Right(())) { case (acc, (arg, tpe)) =>
      acc.flatMap(_ => Bidirectional.check(ctx, arg, tpe))
    }

  /** Extract the inductive type name from a (possibly normalized) type term. */
  private def extractIndName(t: Term): Either[TypeError, String] = t match
    case Term.Ind(name, _, _) => Right(name)
    case _ => Left(TypeError.Custom(
      s"Scrutinee must have an inductive type; got: ${t.show}"
    ))

  /** Extract (tpe, lhs, rhs) from an Eq type term (2-arg or 3-arg form). */
  private def extractEq(t: Term): Option[(Term, Term, Term)] = t match
    case Term.App(Term.App(Term.App(Term.Ind("Eq", _, _), tpe), lhs), rhs) =>
      Some((tpe, lhs, rhs))
    case Term.App(Term.App(Term.Ind("Eq", _, _), lhs), rhs) =>
      Some((Term.Meta(-1), lhs, rhs))
    case _ =>
      None

  /** Check a J-rule (Eq elimination) match.
   *
   *  `Mat(scrutinee, [MatchCase("refl", 1, body)], motiveFunc)`
   *  where `scrutinee : Eq T lhs rhs` and `motiveFunc : T → Type`.
   *
   *  - Branch body is checked against `App(motiveFunc, lhs)` (shifted for the refl witness).
   *  - Overall type returned is `App(motiveFunc, rhs)`.
   */
  private def checkEqElim(
    ctx:       Context,
    lhs:       Term,
    rhs:       Term,
    cases:     List[MatchCase],
    motiveFunc: Term,
  )(using env: GlobalEnv): Either[TypeError, Term] =
    if cases.length != 1 || cases.head.ctor != "refl" || cases.head.bindings != 1 then
      Left(TypeError.Custom(
        s"J-rule (Eq elimination) match must have exactly one 'refl' case with 1 binding; " +
        s"got cases: ${cases.map(c => s"${c.ctor}/${c.bindings}").mkString(", ")}"
      ))
    else
      val body = cases.head.body
      for
        lhsTpe       <- Bidirectional.infer(ctx, lhs)
        branchCtx     = ctx.extend("x", lhsTpe)
        // Branch body type = App(motiveFunc, lhs) shifted for the refl witness binder
        branchBodyTpe = Subst.shift(1, Term.App(motiveFunc, lhs))
        _            <- Bidirectional.check(branchCtx, body, branchBodyTpe)
      yield Term.App(motiveFunc, rhs)

  /** Verify that the match cases cover exactly the constructors (no missing, no extra). */
  private def checkCoverage(
    cases:  List[MatchCase],
    indDef: IndDef,
  ): Either[TypeError, Unit] =
    val ctorNames = indDef.ctors.map(_.name).toSet
    val caseNames = cases.map(_.ctor).toSet
    val missing   = ctorNames -- caseNames
    val extra     = caseNames -- ctorNames
    if missing.isEmpty && extra.isEmpty then Right(())
    else
      val msgs = List(
        Option.when(missing.nonEmpty)(s"missing: ${missing.mkString(", ")}"),
        Option.when(extra.nonEmpty)(s"unexpected: ${extra.mkString(", ")}"),
      ).flatten
      Left(TypeError.Custom(
        s"Non-exhaustive match for '${indDef.name}': ${msgs.mkString("; ")}"
      ))

  /** Check the body of each match case against `returnTpe`.
   *
   *  @param scrutinee the original scrutinee term (used to specialize returnTpe)
   */
  private def checkCases(
    ctx:       Context,
    scrutinee: Term,
    cases:     List[MatchCase],
    indDef:    IndDef,
    returnTpe: Term,
  )(using env: GlobalEnv): Either[TypeError, Unit] =
    cases.foldLeft[Either[TypeError, Unit]](Right(())) { case (acc, mc) =>
      acc.flatMap { _ =>
        indDef.ctors.find(_.name == mc.ctor) match
          case None =>
            Left(TypeError.Custom(s"Unknown constructor '${mc.ctor}' in match"))
          case Some(ctorDef) =>
            if mc.bindings != ctorDef.argTpes.length then
              Left(TypeError.Custom(
                s"Case '${mc.ctor}' declares ${mc.bindings} binding(s) " +
                s"but constructor has ${ctorDef.argTpes.length} argument(s)"
              ))
            else
              val n = ctorDef.argTpes.length
              // Extend context with constructor argument bindings.
              // foldLeft prepends each arg type; last arg in argTpes → Var(0).
              val extCtx = ctorDef.argTpes.foldLeft(ctx) { (c, argTpe) =>
                c.extend("_", argTpe)
              }
              // Build the constructor application term in the extended context.
              // Ctor args are Var(n-1)..Var(0) (most recent = Var(0)).
              val ctorApp = Term.Con(ctorDef.name, indDef.name,
                (0 until n).toList.map(i => Term.Var(n - 1 - i)))
              // Specialize returnTpe for this constructor:
              //   replace Var(scrutineeIdx) with ctorApp, shift all other free vars by n.
              // If scrutinee is not a Var, fall back to plain shift (conservative).
              val specializedRetTpe = scrutinee match
                case Term.Var(k) => specializeForCase(returnTpe, k, ctorApp, n)
                case _           => Subst.shift(n, returnTpe)
              Bidirectional.check(extCtx, mc.body, specializedRetTpe)
      }
    }

  /** Specialize `returnTpe` for a match case branch.
   *
   *  Replaces `Var(varIdx)` (the scrutinee) with `ctorApp` (the constructor
   *  application in the extended context) and shifts all other free variables by `n`
   *  (to account for the `n` new constructor-arg bindings).
   *
   *  Unlike `specializeGoal` in Builtins, this does NOT remove the old binding;
   *  it only shifts other free variables up by `n`.
   */
  private def specializeForCase(t: Term, varIdx: Int, ctorApp: Term, n: Int): Term =
    import sproof.core.{Param, Ctor}
    def go(depth: Int, t: Term): Term = t match
      case Term.Var(i) =>
        val absI = i - depth
        if absI < 0 then Term.Var(i)                           // bound inside depth binders
        else if absI == varIdx then Subst.shift(depth, ctorApp) // replace scrutinee
        else Term.Var(i + n)                                    // other free vars: shift +n
      case Term.App(f, a)          => Term.App(go(depth, f), go(depth, a))
      case Term.Lam(x, tp, b)     => Term.Lam(x, go(depth, tp), go(depth + 1, b))
      case Term.Pi(x, d, c)       => Term.Pi(x, go(depth, d), go(depth + 1, c))
      case Term.Let(x, tp, df, b) => Term.Let(x, go(depth, tp), go(depth, df), go(depth + 1, b))
      case Term.Uni(_)             => t
      case Term.Meta(_)            => t
      case Term.Ind(nm, ps, cs)   =>
        Term.Ind(nm,
          ps.map(p => Param(p.name, go(depth, p.tpe))),
          cs.map(c => Ctor(c.name, go(depth, c.tpe))))
      case Term.Con(nm, r, args)  => Term.Con(nm, r, args.map(go(depth, _)))
      case Term.Fix(nm, tp, b)    => Term.Fix(nm, go(depth, tp), go(depth + 1, b))
      case Term.Mat(s, cases, rt) =>
        Term.Mat(
          go(depth, s),
          cases.map(mc => mc.copy(body = go(depth + mc.bindings, mc.body))),
          go(depth, rt),
        )
    go(0, t)
