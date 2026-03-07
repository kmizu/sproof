package sproof.tactic

import sproof.core.{Term, Context, Subst, GlobalEnv, IndDef, CtorDef, MatchCase, Param, Ctor, DefEntry}
import sproof.checker.Bidirectional
import sproof.eval.{Quote, Eval, Env, Semantic, Neutral}
import sproof.tactic.SimpRewriteDb.RewriteDirection

/** Built-in tactics for sproof.
 *
 *  Naming philosophy: full English words as primary names,
 *  obvious abbreviations as aliases.  No cryptic shortcuts.
 *
 *    trivial  / triv    — close a reflexive equality goal
 *    assume   (no abbr) — introduce a Pi-bound variable
 *    apply_   (no abbr) — apply a function to reduce the goal
 *
 *  These tactics are NOT part of the trusted Kernel.  Every proof term
 *  they produce is subsequently re-checked by Kernel.check.
 */
object Builtins:

  // Eq is a built-in inductive family handled specially in the kernel checker.
  // Tactic-side case splitting still needs a constructor shape for subgoal generation.
  private val eqIndDef: IndDef = IndDef(
    name = "Eq",
    params = Nil,
    ctors = List(CtorDef("refl", List(Term.Meta(-1)))),
    universe = 0,
  )

  // ---- trivial / triv ----

  /** Close an equality goal `Eq T a b` when `a` and `b` are definitionally equal.
   *
   *  Corresponds to `rfl` / `reflexivity` in Coq, but with a name that
   *  communicates intent: this goal is *trivially* true.
   */
  val trivial: TacticM[Unit] =
    for
      goalPair   <- TacticM.currentGoal
      (mv, goal)  = goalPair
      // Extract Eq structure from the raw term — do NOT whnf the outer goal because
      // NbE evaluation of Ind("Eq",...) loses the name and breaks pattern matching.
      // We normalise only the two sides (lhs, rhs) for the definitional equality check.
      result     <- Eq.extract(goal.target) match
        case None =>
          TacticM.fail(TacticError.NotAnEquality(goal.target.show))
        case Some(triple) =>
          val (_, lhs, rhs) = triple
          val env = buildEnvWithDefs(goal.ctx)
          if Quote.convEqual(goal.ctx.size, env, lhs, rhs) then
            TacticM.solveGoalWith(mv, Eq.mkRefl(lhs))
          else
            TacticM.fail(TacticError.Custom(
              s"trivial: not definitionally equal: ${lhs.show} ≢ ${rhs.show}"
            ))
    yield result

  /** Alias for `trivial`. */
  val triv: TacticM[Unit] = trivial

  // ---- assume ----

  /** Introduce the outermost Pi binder as a local variable.
   *
   *  `assume "x"` on goal `Pi(y, A, B)` replaces the goal with `B`
   *  in an extended context that contains `x : A`.
   *
   *  Corresponds to `intro` / `intros` in Coq, but with a name that
   *  makes the action explicit: we *assume* `x : A`.
   */
  def assume(varName: String): TacticM[Unit] =
    for
      goalPair   <- TacticM.currentGoal
      (mv, goal)  = goalPair
      target      = Bidirectional.whnf(goal.ctx, goal.target)
      result     <- target match
        case Term.Pi(_, dom, cod) =>
          val newCtx    = goal.ctx.extend(varName, dom)
          // cod has Var(0) = the Pi-bound variable, which in newCtx is varName
          val newTarget = cod
          for
            mv_new <- TacticM.addGoal(newCtx, newTarget)
            // Evidence: λvarName. <proof of body> — body filled by mv_new
            _      <- TacticM.solveGoalWith(mv, Term.Lam(varName, dom, Term.Meta(mv_new.id)))
          yield ()
        case _ =>
          TacticM.fail(TacticError.NotAPi(goal.target.show))
    yield result

  // ---- apply ----

  /** Apply a function term to reduce the current goal.
   *
   *  `apply_ f` where `f : A → B` and the goal is `B`:
   *    - Unifies the codomain of `f` with the goal
   *    - Replaces the goal with the domain of `f`
   *    - Evidence for the original goal will be `f <evidence-for-A>`
   *
   *  For dependent Pi, we require the codomain to be a closed type
   *  (non-dependent arrow) to keep Phase 2 simple.
   */
  def apply_(fn: Term): TacticM[Unit] =
    for
      goalPair   <- TacticM.currentGoal
      (mv, goal)  = goalPair
      fnTpe      <- Bidirectional.infer(goal.ctx, fn) match
        case Right(t) => TacticM.pure(t)
        case Left(e)  => TacticM.fail(TacticError.TypeCheckFailed(e))
      result     <- Bidirectional.whnf(goal.ctx, fnTpe) match
        case Term.Pi(_, dom, cod) =>
          // The codomain after substituting the argument (not yet known).
          // For non-dependent cod, Var(0) does not appear; substitute Uni(0) as dummy.
          val codClosed = Subst.subst(Term.Uni(0), cod)
          val env       = buildEnvWithDefs(goal.ctx)
          if Quote.convEqual(goal.ctx.size, env, codClosed, goal.target) then
            for
              argMv <- TacticM.addGoal(goal.ctx, dom)
              // Evidence: fn applied to the (future) argument
              // We record a placeholder; the argument metavar is argMv
              _     <- TacticM.solveGoalWith(mv, Term.App(fn, Term.Meta(argMv.id)))
            yield ()
          else
            TacticM.fail(TacticError.GoalMismatch(
              expected = goal.target.show,
              got      = codClosed.show,
            ))
        case nonPi =>
          TacticM.fail(TacticError.NotAFunction(fn.show))
    yield result

  // ---- induction ----

  /** Perform structural induction on a named variable.
   *
   *  `induction "n"` where `n : Nat` in the current context:
   *  1. Finds `n` in the context (by name) at some De Bruijn index `i`
   *  2. Looks up the inductive type definition from GlobalEnv
   *  3. For each constructor:
   *     - Non-recursive (e.g., `zero`): subgoal with `n` substituted by `zero`
   *     - Recursive (e.g., `succ k`): subgoal with `n` substituted by `succ(k)`,
   *       extended context adds `k : T`, and optionally `ih : P(k)` via Fix
   *  4. If any case has an IH requested (extraBindings > ctorDef.argTpes.length),
   *     the proof term is wrapped in a Fix to provide the recursive hypothesis.
   *     Otherwise: plain `Mat(Var(i), matchCases, returnTpe)`.
   *
   *  NOTE: Context variable must have an inductive type present in GlobalEnv.
   */
  def induction(varName: String, caseSpecs: List[(String, List[String])] = Nil)(using env: GlobalEnv): TacticM[Unit] =
    for
      goalPair     <- TacticM.currentGoal
      (mv, goal)    = goalPair
      varIdxTpe    <- TacticM.liftEither(findVarByName(goal.ctx, varName))
      (varIdx, varTpe) = varIdxTpe
      indName      <- TacticM.liftEither(extractIndNameRaw(varTpe))
      indDef       <- TacticM.liftEither(resolveIndDef(indName))
      // Determine if any case requests IH (extraBindings > ctor arg count)
      needsIH       = caseSpecs.exists { case (ctorName, bindings) =>
                        indDef.ctors.find(_.name == ctorName)
                          .exists(ctor => bindings.length > ctor.argTpes.length)
                      }
      _            <- (if needsIH
                       then inductionWithIH(mv, goal, varIdx, varTpe, indDef, caseSpecs)
                       else plainInduction(mv, goal, varIdx, indDef))
    yield ()

  private def plainInduction(
    mv:     MetaVar,
    goal:   Goal,
    varIdx: Int,
    indDef: IndDef,
  )(using env: GlobalEnv): TacticM[Unit] =
    for
      matchCases <- generateInductionCases(goal.ctx, varIdx, goal.target, indDef)
      matTerm     = Term.Mat(Term.Var(varIdx), matchCases.map(_._3), goal.target)
      _          <- TacticM.solveGoalWith(mv, matTerm)
    yield ()

  /** Induction with induction hypothesis (Fix-wrapped proof term).
   *
   *  Proof term structure:
   *  {{{
   *    App(
   *      Fix("_rec", Pi("_n", T, P),
   *        Lam("_n", T,
   *          Mat(Var(0), [zero_case, succ_Let_case], P))),
   *      Var(varIdx))
   *  }}}
   *  where P = propWithVar0 (the motive, goal with Var(0) as induction var).
   *  For the succ case: Let("ih", P, App(Var(2), Var(0)), Meta(mv_succ)).
   */
  private def inductionWithIH(
    mv:        MetaVar,
    goal:      Goal,
    varIdx:    Int,
    varTpe:    Term,
    indDef:    IndDef,
    caseSpecs: List[(String, List[String])],
  )(using env: GlobalEnv): TacticM[Unit] =
    // propWithVar0 = the motive body: goal.target with the induction variable (Var(varIdx))
    // replaced by Var(0) (_n), and variables below varIdx shifted +1 for the _n binder.
    // For varIdx=0 (single-variable context): propWithVar0 = goal.target unchanged.
    val propWithVar0 = computeMotiveBody(goal.target, varIdx)
    // propForMat = propWithVar0 shifted for the _rec binder inside Fix+Lam body.
    // Inside Lam("_n", ...) body (Fix body): _n=Var(0), _rec=Var(1), ctx_minus at Var(2+).
    // propWithVar0 has _n=Var(0), ctx_minus at Var(1+) — shift Var(1+) by 1 for _rec.
    val propForMat   = Subst.shiftFrom(1, 1, propWithVar0)
    val ctx_minus    = removeFromContext(goal.ctx, varIdx)
    for
      fixCasesData <- buildFixCases(ctx_minus, varIdx, varTpe, goal.target, propWithVar0, indDef, caseSpecs)
      fixCases      = fixCasesData.map(_._1)
      // Fix("_rec", Pi("_n", T, P), Lam("_n", T, Mat(Var(0), cases, propForMat)))
      // propForMat (not propWithVar0) for Mat return type: ctx_minus vars are at Var(2+) in Lam body.
      fixTerm       = Term.Fix("_rec",
                        Term.Pi("_n", varTpe, propWithVar0),
                        Term.Lam("_n", varTpe,
                          Term.Mat(Term.Var(0), fixCases, propForMat)))
      proofTerm     = Term.App(fixTerm, Term.Var(varIdx))
      _            <- TacticM.solveGoalWith(mv, proofTerm)
    yield ()

  /** Build Fix-style match cases for each constructor.
   *  Returns list of (MatchCase, subCtx, subGoal).
   */
  private def buildFixCases(
    ctx_minus:   Context,
    varIdx:      Int,
    varTpe:      Term,
    goal:        Term,
    propWithVar0: Term,
    indDef:      IndDef,
    caseSpecs:   List[(String, List[String])],
  )(using env: GlobalEnv): TacticM[List[(MatchCase, Context, Term)]] =
    indDef.ctors.foldLeft(TacticM.pure(List.empty[(MatchCase, Context, Term)])) { (acc, ctorDef) =>
      acc.flatMap { cases =>
        val extraBindings = caseSpecs
          .collectFirst { case (name, bindings) if name == ctorDef.name => bindings }
          .getOrElse(Nil)
        val hasIH = extraBindings.length > ctorDef.argTpes.length
        buildFixCase(ctx_minus, varIdx, varTpe, goal, propWithVar0, indDef, ctorDef, hasIH, extraBindings).map { triple =>
          cases :+ triple
        }
      }
    }

  /** Build a single Fix-style match case for one constructor.
   *
   *  Non-recursive (no IH): plain MatchCase with meta placeholder.
   *  Recursive (IH): Let-wrapped body; IH = _rec applied to the recursive arg.
   *
   *  Context structure inside the Fix>Lam>Mat body for a ctor with n args:
   *    Var(0..n-1) = ctor args (last arg = Var(0))
   *    Var(n)      = _n  (Lam binder)
   *    Var(n+1)    = _rec (Fix binder)
   *    Var(n+2+)   = outer context vars (ctx_minus)
   *
   *  The sub-goal context mirrors this exactly (ctx_minus + [_rec] + [_n] + ctor_args [+ ih])
   *  so that proof terms generated by tactics have correct De Bruijn indices.
   *
   *  For the IH Let: Let("ih", ihType, App(Var(n+1), Var(0)), Meta(mv))
   *    where Var(n+1) = _rec and Var(0) = the last (recursive) ctor arg in Match body.
   */
  private def buildFixCase(
    ctx_minus:    Context,
    varIdx:       Int,
    varTpe:       Term,
    goal:         Term,
    propWithVar0: Term,
    indDef:       IndDef,
    ctorDef:      CtorDef,
    hasIH:        Boolean,
    extraBindings: List[String] = Nil,
  )(using env: GlobalEnv): TacticM[(MatchCase, Context, Term)] =
    val n            = ctorDef.argTpes.length
    val ctorArgVars  = (0 until n).toList.map(i => Term.Var(n - 1 - i))
    val ctorTerm     = Term.Con(ctorDef.name, indDef.name, ctorArgVars)
    // specialGoalBase: goal in (ctx_minus + n_ctor_args) context.
    val specialGoalBase = specializeGoal(goal, varIdx, ctorTerm, n)
    // The sub-goal context has _rec and _n between ctx_minus and ctor_args.
    // Shift ctx_minus vars in specialGoalBase by 2 to account for _rec and _n.
    val specialGoalAdjusted = Subst.shiftFrom(n, 2, specialGoalBase)
    // Use user-supplied binding names for ctor args; fall back to "_" if not provided
    val argNames     = extraBindings.take(n).padTo(n, "_")
    // Build sub-goal context including _rec and _n so proof term indices match Fix+Lam+Mat body.
    val recType      = Term.Pi("_n", varTpe, propWithVar0)
    val ctxWithRec   = ctx_minus.extend("_rec", recType)
    val ctxWithRecN  = ctxWithRec.extend("_n", varTpe)
    val ctorCtx      = ctorDef.argTpes.zip(argNames).foldLeft(ctxWithRecN)((c, pair) => c.extend(pair._2, pair._1))
    if !hasIH then
      for mv <- TacticM.addGoal(ctorCtx, specialGoalAdjusted)
      yield (MatchCase(ctorDef.name, n, Term.Meta(mv.id)), ctorCtx, specialGoalAdjusted)
    else
      // IH type P(k) in ctorCtx (and in the Match case body):
      // propWithVar0 has _n=Var(0), ctx_minus at Var(1+).
      // In ctorCtx: k=Var(0..n-1), _n=Var(n), _rec=Var(n+1), ctx_minus at Var(n+2+).
      // P(k) = propWithVar0 with Var(0)=_n→k=Var(0), ctx_minus shifted Var(1+)→Var(n+2+).
      val ihType       = Subst.shiftFrom(1, n + 1, propWithVar0)
      // _rec is at Var(n+1) in the Match case body (n ctor arg binders, then _n, then _rec).
      val recFuncRef   = Term.Var(n + 1)
      val recArgRef    = Term.Var(0)           // k: the (last) recursive ctor arg
      val ihDef        = Term.App(recFuncRef, recArgRef)
      // Sub-goal context: [ih: shift(1,ihType), ctor_args, _n, _rec, ctx_minus...]
      val ihTypeInSub  = Subst.shift(1, ihType)
      val subCtx       = ctorCtx.extend("ih", ihTypeInSub)
      val specialGoalInSub = Subst.shift(1, specialGoalAdjusted)
      for mv <- TacticM.addGoal(subCtx, specialGoalInSub)
      yield
        val letBody = Term.Let("ih", ihType, ihDef, Term.Meta(mv.id))
        (MatchCase(ctorDef.name, n, letBody), subCtx, specialGoalInSub)

  /** Generate subgoals and MatchCase placeholders for induction on a variable.
   *
   *  Returns a list of (subCtx, subGoal, matchCase) for each constructor.
   *  The match cases contain `Term.Meta` placeholders that get filled by subgoal proofs.
   */
  private def generateInductionCases(
    ctx:    Context,
    varIdx: Int,
    goal:   Term,
    indDef: IndDef,
  )(using env: GlobalEnv): TacticM[List[(Context, Term, MatchCase)]] =
    // Remove varIdx from context: entries with index < varIdx stay, entries > varIdx shift down.
    // For the common case (varIdx = 0), ctx_minus is just ctx without the first entry.
    val ctx_minus = removeFromContext(ctx, varIdx)
    indDef.ctors.foldLeft(TacticM.pure(List.empty[(Context, Term, MatchCase)])) { (acc, ctorDef) =>
      acc.flatMap { cases =>
        generateCtorCase(ctx_minus, ctx, varIdx, goal, indDef, ctorDef).map { triple =>
          cases :+ triple
        }
      }
    }

  /** Generate the subgoal and MatchCase for a single constructor.
   *
   *  For constructor `ctor(arg1: A1, ..., argN: AN)`:
   *  - Ctor args are De Bruijn variables Var(N-1)..Var(0) in the extended context
   *  - The subgoal goal = specializeGoal(G, varIdx, ctorTerm, N)
   *    where the induction variable (Var(varIdx)) is replaced by the constructor application
   *  - The subgoal context = ctx_minus extended with [arg1: A1, ..., argN: AN]
   */
  private def generateCtorCase(
    ctx_minus: Context,    // ctx with the induction variable removed
    ctx:       Context,    // original context (with the induction variable)
    varIdx:    Int,        // De Bruijn index of the induction variable in ctx
    goal:      Term,       // original goal (may mention Var(varIdx))
    indDef:    IndDef,
    ctorDef:   CtorDef,
  )(using env: GlobalEnv): TacticM[(Context, Term, MatchCase)] =
    val n       = ctorDef.argTpes.length  // number of ctor args (= bindings in MatchCase)
    // Build constructor arg references: Var(0), Var(1), ..., Var(n-1)
    // In the extended context, the most recently bound (innermost) ctor arg = Var(0).
    // (The args list is ordered left-to-right; the first arg is the outermost = Var(n-1).)
    val ctorArgVars = (0 until n).toList.map(i => Term.Var(n - 1 - i))
    val ctorTerm    = Term.Con(ctorDef.name, indDef.name, ctorArgVars)
    // Specialize the goal: replace Var(varIdx) with ctorTerm, adjusting all other variables
    // correctly for the n new ctor-arg bindings added to the context.
    val specialGoal = specializeGoal(goal, varIdx, ctorTerm, n)
    // Build the extended context: ctx_minus + ctor args (foldLeft prepends each, so last arg = Var(0))
    // (Names are "_" here since generateCtorCase is called without user-provided binding names)
    val extCtx = ctorDef.argTpes.foldLeft(ctx_minus) { (c, argTpe) =>
      c.extend("_", argTpe)
    }
    for
      mv <- TacticM.addGoal(extCtx, specialGoal)
    yield (extCtx, specialGoal, MatchCase(ctorDef.name, n, Term.Meta(mv.id)))

  /** Remove the variable at De Bruijn index `idx` from the context.
   *
   *  For idx = 0: removes the most recently bound variable (head of entries).
   *  For idx > 0: removes deeper entries (less common, handled conservatively).
   */
  private def removeFromContext(ctx: Context, idx: Int): Context =
    // Reconstruct context without entry at position idx
    // entries are ordered head = index 0 (most recent)
    val entriesList = ctx.entries.toList
    if idx < 0 || idx >= entriesList.length then ctx
    else
      val before = entriesList.take(idx)   // entries with De Bruijn index < idx (less recent)
      val after  = entriesList.drop(idx + 1) // entries with De Bruijn index > idx (more recent)
      // Rebuild: newer entries (smaller indices) first, older entries reconstructed below
      // 'after' entries have indices 0..idx-1 (they were above the removed entry)
      // 'before' entries had indices idx+1..size-1 (they are below the removed entry)
      // After removal, their indices shift: idx+1 → idx, idx+2 → idx+1, etc.
      // For simplicity, rebuild from scratch (correct for non-dependent context entries)
      val newCtx1 = after.foldRight(Context.empty) { (e, c) => c.extend(e.name, e.tpe) }
      before.foldRight(newCtx1) { (e, c) => c.extend(e.name, Subst.shift(-1, e.tpe)) }

  /** Specialize `goal` by replacing the induction variable (at De Bruijn index `varIdx`
   *  in the original context) with `ctorTerm`, while accounting for `n` new ctor-arg
   *  bindings that have been prepended to the context.
   *
   *  The new context is: [arg_n-1 : A_n-1, ..., arg_0 : A_0] ++ ctx_minus
   *  So every free variable in `goal` must be adjusted:
   *    - Var(i) where (i - depth) < 0        : bound inside inner binders → unchanged
   *    - Var(i) where (i - depth) == varIdx  : replace with Subst.shift(depth, ctorTerm)
   *    - Var(i) where (i - depth) < varIdx   : these variables are "above" varIdx in the
   *                                             original context; they need to shift up by n
   *                                             to skip over the new ctor bindings
   *    - Var(i) where (i - depth) > varIdx   : these are "below" varIdx; net shift = n - 1
   *                                             (+n for new ctor bindings, -1 for removed var)
   */
  private def specializeGoal(goal: Term, varIdx: Int, ctorTerm: Term, n: Int): Term =
    def go(depth: Int, t: Term): Term = t match
      case Term.Var(i) =>
        val absI = i - depth
        if absI < 0 then Term.Var(i)                          // bound inside depth binders
        else if absI == varIdx then Subst.shift(depth, ctorTerm) // replace induction var
        else if absI < varIdx then Term.Var(i + n)             // vars above varIdx: shift +n
        else Term.Var(i - 1 + n)                               // vars below varIdx: shift +(n-1)
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
    go(0, goal)

  // ---- simplify ----

  /** Simplify the current goal, optionally using named equality hypotheses as rewrite rules.
   *
   *  `simplify []` / `simplify` — when `@[simp]` lemmas are in scope uses those; otherwise
   *    delegates to `trivial` (NbE definitional equality).
   *
   *  `simplify [ih]` where `ih : lhs = rhs` — applies the J-rule (congruence):
   *    given goal `Eq(f(lhs), f(rhs))`, constructs the J-rule proof term:
   *    `Mat(ih, [refl(x) => refl(f(lhs_shifted))], P)` where `P(x) = Eq(f(lhs), f(x))`.
   *    Currently handles single-constructor-application wrappers, e.g. `succ(lhs) = succ(rhs)`.
   *    Also handles global defspecs that are not in the local context (e.g. @[simp] lemmas).
   *
   *  Falls back to `trivial` if the pattern is not recognised.
   */
  def simplify(lemmas: List[String] = Nil)(using env: GlobalEnv): TacticM[Unit] =
    val effectiveLemmas = if lemmas.isEmpty then env.simpSet.toList else lemmas
    effectiveLemmas match
      case Nil => trivial
      case _ =>
        val orderedSpecs = SimpRewriteDb.orderSpecs(effectiveLemmas)
        for
          goalPair  <- TacticM.currentGoal
          (mv, goal) = goalPair
          result    <- trySimplifySpecs(mv, goal, orderedSpecs)
        yield result

  private def trySimplifySpecs(
    mv: MetaVar,
    goal: Goal,
    specs: List[SimpRewriteDb.RuleSpec],
  )(using env: GlobalEnv): TacticM[Unit] =
    specs match
      case Nil => trivial
      case spec :: rest =>
        TacticM.get.flatMap { state =>
          val (newState, result) =
            TacticM.run(simplifyWithIH(mv, goal, spec.lemmaName, spec.direction), state)
          result match
            case Right(()) => TacticM.set(newState)
            case Left(_)   => trySimplifySpecs(mv, goal, rest)
        }

  /** Try to close the goal using the J-rule with hypothesis `ihName`. */
  private def simplifyWithIH(
    mv: MetaVar,
    goal: Goal,
    ihName: String,
    direction: RewriteDirection = RewriteDirection.Forward,
  )(using env: GlobalEnv): TacticM[Unit] =
    findVarByName(goal.ctx, ihName) match
      case Left(_) => tryGlobalLemmaAsIH(mv, goal, ihName, direction)  // not in ctx: try global env
      case Right((ihIdx, _)) =>
        // Use the raw stored type (not the over-shifted version from findVarByName)
        // because buildFixCase stores ih.tpe already shifted for the extended context.
        val ihTypeRaw = goal.ctx.entries(ihIdx).tpe
        Eq.extract(ihTypeRaw) match
          case None => trivial  // ih not an equality: fall back
          case Some((eqTpe, lhsIh, rhsIh)) =>
            Eq.extract(goal.target) match
              case None => trivial  // goal not an equality: fall back
              case Some((_, lhsGoal, rhsGoal)) =>
                // Normalise all sides for comparison and J-rule construction
                val envForCtx  = buildEnvWithDefs(goal.ctx)
                val n          = goal.ctx.size
                val (orientedLhsIh, orientedRhsIh) = direction match
                  case RewriteDirection.Forward  => (lhsIh, rhsIh)
                  case RewriteDirection.Backward => (rhsIh, lhsIh)
                val normLhs    = Quote.normalize(n, envForCtx, lhsGoal)
                val normRhs    = Quote.normalize(n, envForCtx, rhsGoal)
                val normLhsIh  = Quote.normalize(n, envForCtx, orientedLhsIh)
                val normRhsIh  = Quote.normalize(n, envForCtx, orientedRhsIh)
                // Fast path: goal LHS ≡ ih LHS and goal RHS ≡ ih RHS
                // e.g. mult(succ_k, 0) normalises to mult(k,0), ih: mult(k,0)=0
                if Quote.convEqual(n, envForCtx, normLhs, normLhsIh) &&
                   Quote.convEqual(n, envForCtx, normRhs, normRhsIh) then
                  direction match
                    case RewriteDirection.Forward =>
                      TacticM.solveGoalWith(mv, Term.Var(ihIdx))
                    case RewriteDirection.Backward =>
                      TacticM.solveGoalWith(mv, symmetryProof(ihIdx, eqTpe, lhsIh))
                else
                  direction match
                    case RewriteDirection.Forward =>
                      buildJRuleProof(mv, goal, ihIdx, lhsIh, normLhs, normRhs)
                    case RewriteDirection.Backward =>
                      // Backward J-rule is intentionally conservative in Phase 1:
                      // direct symmetry is supported; complex congruence falls back.
                      trivial

  /** Build a J-rule proof for goal `Eq(normLhs, normRhs)` given `ih` at `ihIdx`.
   *
   *  Handles any-arity Con where exactly one argument position differs:
   *    normLhs = Con(name, ref, [a0, ..., l, ..., an])
   *    normRhs = Con(name, ref, [a0, ..., r, ..., an])
   *  where position k has l ≠ r (all others definitionally equal).
   *
   *  Motive: P(x) = Eq(Con(name, args_l_shifted), Con(name, args_r_with_x_at_k)).
   *  Proof: Mat(Var(ihIdx), [MatchCase("refl", 1, refl(Con(name, args_l_shifted)))], P).
   */
  private def buildJRuleProof(
    mv:      MetaVar,
    goal:    Goal,
    ihIdx:   Int,
    lhsIh:  Term,
    normLhs: Term,
    normRhs: Term,
  )(using env: GlobalEnv): TacticM[Unit] =
    (normLhs, normRhs) match
      case (Term.Con(lname, lref, largs), Term.Con(rname, rref, rargs))
           if lname == rname && lref == rref && largs.length == rargs.length && largs.nonEmpty =>
        val n         = goal.ctx.size
        val envForCtx = buildEnvWithDefs(goal.ctx)
        // Find the unique argument position where largs and rargs differ.
        val diffPositions = largs.zip(rargs).zipWithIndex.collect {
          case ((la, ra), k) if !Quote.convEqual(n, envForCtx, la, ra) => k
        }
        diffPositions match
          case List(k) =>
            // Shift all lhs args by 1 for use inside the motive lambda binder.
            val shiftedLArgs = largs.map(Subst.shift(1, _))
            // rhs args: position k becomes Var(0); all others shifted by 1.
            val motiveRArgs  = rargs.zipWithIndex.map { (a, i) =>
              if i == k then Term.Var(0) else Subst.shift(1, a)
            }
            val motiveBody = Term.App(
              Term.App(Term.Ind("Eq", Nil, Nil),
                Term.Con(lname, lref, shiftedLArgs)),
              Term.Con(rname, rref, motiveRArgs))
            // Determine the motive binder type.  For parameterized inductives (e.g. List(A)),
            // `Term.Ind(lref, Nil, Nil)` is wrong — we need `List(A)` as the binder type.
            // When rargs(k) normalises to a Var we can look up its exact type in the context
            // (ctx.apply returns the type already well-formed in the current context) and shift
            // by 1 for the extra motive binder.  Otherwise fall back to the bare Ind.
            val motiveBinderTpe: Term = rargs(k) match
              case Term.Var(idx) =>
                goal.ctx.lookup(idx).getOrElse(Term.Ind(lref, Nil, Nil))
              case _ =>
                Term.Ind(lref, Nil, Nil)
            val motiveFunc = Term.Lam("x", motiveBinderTpe, motiveBody)
            // Branch body: refl of the fully-shifted LHS constructor.
            val body      = Term.Con("refl", "Eq", List(Term.Con(lname, lref, shiftedLArgs)))
            val proofTerm = Term.Mat(
              Term.Var(ihIdx),
              List(MatchCase("refl", 1, body)),
              motiveFunc)
            TacticM.solveGoalWith(mv, proofTerm)
          case _ =>
            // Zero or multiple varying positions: cannot construct J-rule; fall back.
            trivial
      case _ =>
        // Pattern not recognised: try trivial as fallback.
        trivial

  private def symmetryProof(ihIdx: Int, eqTpe: Term, lhs: Term): Term =
    val eqTpeInLam = Subst.shift(1, eqTpe)
    val lhsInLam   = Subst.shift(1, lhs)
    val motiveBody = Eq.mkType(eqTpeInLam, Term.Var(0), lhsInLam)
    val motive     = Term.Lam("x", eqTpeInLam, motiveBody)
    val branch     = Eq.mkRefl(lhsInLam)
    Term.Mat(Term.Var(ihIdx), List(MatchCase("refl", 1, branch)), motive)

  /** Apply a global defspec as a rewrite lemma when it is not in the local context.
   *
   *  Looks up `lemmaName` in `env.defs`, peels its Pi binders, finds matching context
   *  variables for each parameter (greedy: first match by definitional equality), and
   *  checks whether the instantiated proposition equals the current goal.
   *  Handles only non-dependent parameter types (the common case for stdlib lemmas).
   */
  private def tryGlobalLemmaAsIH(
    mv:        MetaVar,
    goal:      Goal,
    lemmaName: String,
    direction: RewriteDirection = RewriteDirection.Forward,
  )(using env: GlobalEnv): TacticM[Unit] =
    env.lookupDef(lemmaName).fold(trivial) { defEntry =>
      instantiateGlobalLemma(defEntry, goal) match
        case None => trivial
        case Some((proofTerm, propTerm)) =>
          Eq.extract(propTerm) match
            case None => trivial
            case Some((_, lhsLemma, rhsLemma)) =>
              Eq.extract(goal.target) match
                case None => trivial
                case Some((_, lhsGoal, rhsGoal)) =>
                  val n      = goal.ctx.size
                  val envCtx = buildEnvWithDefs(goal.ctx)
                  val (orientedLhs, orientedRhs) = direction match
                    case RewriteDirection.Forward  => (lhsLemma, rhsLemma)
                    case RewriteDirection.Backward => (rhsLemma, lhsLemma)
                  val normGoalLhs = Quote.normalize(n, envCtx, lhsGoal)
                  val normGoalRhs = Quote.normalize(n, envCtx, rhsGoal)
                  val normLemLhs  = Quote.normalize(n, envCtx, orientedLhs)
                  val normLemRhs  = Quote.normalize(n, envCtx, orientedRhs)
                  if Quote.convEqual(n, envCtx, normGoalLhs, normLemLhs) &&
                     Quote.convEqual(n, envCtx, normGoalRhs, normLemRhs) then
                    TacticM.solveGoalWith(mv, proofTerm)
                  else
                    trivial
    }

  /** Instantiate a global defspec body by matching its Pi binders against context variables.
   *
   *  Returns `(proofTerm, propTerm)` where proofTerm is `App(...body..., args...)` and
   *  propTerm is the Eq proposition with all Pi binders replaced by the chosen args.
   *
   *  Substitution order: innermost binder first (args.reverse) so De Bruijn indices
   *  decrement correctly at each step.  Only non-dependent arg types are supported.
   */
  private def instantiateGlobalLemma(
    defEntry: DefEntry,
    goal:     Goal,
  )(using env: GlobalEnv): Option[(Term, Term)] =
    def peelPis(t: Term, acc: List[Term]): (List[Term], Term) = t match
      case Term.Pi(_, argType, body) => peelPis(body, argType :: acc)
      case other                     => (acc.reverse, other)
    val (argTypes, coreProp) = peelPis(defEntry.tpe, Nil)
    if argTypes.isEmpty then
      Some((defEntry.body, defEntry.tpe))
    else
      val n      = goal.ctx.size
      val envCtx = buildEnvWithDefs(goal.ctx)
      // For each Pi arg type, find the first context variable matching by definitional equality.
      val candidatesOpt: Option[List[Term]] =
        argTypes.foldLeft(Option(List.empty[Term])) { (accOpt, argType) =>
          accOpt.flatMap { acc =>
            goal.ctx.entries.zipWithIndex.collectFirst {
              case (entry, idx)
                  if Quote.convEqual(
                    n, envCtx,
                    Quote.normalize(n, envCtx, Subst.shift(idx + 1, entry.tpe)),
                    Quote.normalize(n, envCtx, argType),
                  ) => Term.Var(idx)
            }.map(acc :+ _)
          }
        }
      candidatesOpt.map { args =>
        val proofTerm = args.foldLeft(defEntry.body)(Term.App.apply)
        // Substitute innermost binder first so each Subst.subst correctly lowers
        // remaining De Bruijn indices: innermost Var(0) gets the last arg, then the
        // former Var(1) becomes Var(0) and gets the second-to-last arg, and so on.
        val propTerm = args.reverse.foldLeft(coreProp) { (t, arg) => Subst.subst(arg, t) }
        (proofTerm, propTerm)
      }

  /** Alias for `simplify` with no lemmas. */
  val simp: TacticM[Unit] = simplify(Nil)

  // ---- assumption ----

  /** Find a hypothesis in context whose type matches the goal exactly. */
  val assumption: TacticM[Unit] =
    for
      goalPair   <- TacticM.currentGoal
      (mv, goal)  = goalPair
      env         = buildEnvWithDefs(goal.ctx)
      found      <- goal.ctx.entries.zipWithIndex.collectFirst {
        case (entry, idx) =>
          val entryTpe = Subst.shift(idx + 1, entry.tpe)
          if Quote.convEqual(goal.ctx.size, env, entryTpe, goal.target)
          then Some(idx)
          else None
      }.flatten match
        case Some(idx) => TacticM.pure(Term.Var(idx))
        case None      => TacticM.fail(TacticError.Custom("assumption: no matching hypothesis found"))
      _          <- TacticM.solveGoalWith(mv, found)
    yield ()

  // ---- contradiction ----

  /** Find False or contradictory hypotheses in context to close the goal. */
  val contradiction: TacticM[Unit] =
    for
      goalPair   <- TacticM.currentGoal
      (mv, goal)  = goalPair
      // Look for a hypothesis of type False (inductive with no constructors)
      found      <- goal.ctx.entries.zipWithIndex.collectFirst {
        case (entry, idx) =>
          val tpe = Subst.shift(idx + 1, entry.tpe)
          tpe match
            case Term.Ind(name, _, ctors) if ctors.isEmpty =>
              Some(idx)
            case _ => None
      }.flatten match
        case Some(idx) =>
          // Eliminate False using empty match
          val falseTerm = Term.Var(idx)
          val proof = Term.Mat(falseTerm, Nil, goal.target)
          TacticM.solveGoalWith(mv, proof)
        case None =>
          TacticM.fail(TacticError.Custom("contradiction: no False hypothesis found"))
    yield ()

  // ---- cases ----

  /** Perform case split on a named variable (like induction but without IH). */
  def cases(varName: String, caseSpecs: List[(String, List[String])] = Nil)(using env: GlobalEnv): TacticM[Unit] =
    for
      goalPair     <- TacticM.currentGoal
      (mv, goal)    = goalPair
      varIdxTpe    <- TacticM.liftEither(findVarByName(goal.ctx, varName))
      (varIdx, varTpe) = varIdxTpe
      indName      <- TacticM.liftEither(extractIndNameRaw(varTpe))
      indDef       <- TacticM.liftEither(resolveIndDef(indName))
      _            <- plainInduction(mv, goal, varIdx, indDef)
    yield ()

  // ---- rewrite ----

  /** Rewrite the current goal using named equality lemmas.
    *
    * For each lemma `h : a = b` in the context,
    * if the goal is exactly `a = b`, close it with `h`.
    * Otherwise, use J-rule substitution via simplify infrastructure.
    */
  def rewrite(lemmas: List[String])(using env: GlobalEnv): TacticM[Unit] =
    val orderedSpecs = SimpRewriteDb.orderSpecs(lemmas)
    orderedSpecs match
      case Nil => trivial
      case _ =>
        for
          goalPair   <- TacticM.currentGoal
          (mv, goal)  = goalPair
          _          <- rewriteWithSpecs(mv, goal, orderedSpecs)
        yield ()

  private def rewriteWithSpecs(
    mv: MetaVar,
    goal: Goal,
    specs: List[SimpRewriteDb.RuleSpec],
  )(using env: GlobalEnv): TacticM[Unit] =
    specs match
      case Nil => TacticM.fail(TacticError.Custom("rewrite: all rewrite rules failed"))
      case spec :: rest =>
        TacticM.get.flatMap { state =>
          val (newState, result) =
            TacticM.run(rewriteWithLemma(mv, goal, spec.lemmaName, spec.direction), state)
          result match
            case Right(()) => TacticM.set(newState)
            case Left(_)   => rewriteWithSpecs(mv, goal, rest)
        }

  /** Rewrite goal using a single lemma. */
  private def rewriteWithLemma(
    mv:        MetaVar,
    goal:      Goal,
    lemmaName: String,
    direction: RewriteDirection = RewriteDirection.Forward,
  )(using env: GlobalEnv): TacticM[Unit] =
    findVarByName(goal.ctx, lemmaName) match
      case Left(err) =>
        TacticM.fail(err)
      case Right((ihIdx, ihType)) =>
        val envForCtx = buildEnvWithDefs(goal.ctx)
        Eq.extract(ihType) match
          case None =>
            TacticM.fail(TacticError.Custom(s"rewrite: $lemmaName is not an equality"))
          case Some((eqTpe, lhsIh, rhsIh)) =>
            // Check if the goal exactly matches the hypothesis type
            direction match
              case RewriteDirection.Forward =>
                if Quote.convEqual(goal.ctx.size, envForCtx, goal.target, ihType) then
                  // Goal is exactly the same equality — just use the hypothesis directly
                  TacticM.solveGoalWith(mv, Term.Var(ihIdx))
                else
                  // Try J-rule approach through simplify
                  simplifyWithIH(mv, goal, lemmaName, direction)
              case RewriteDirection.Backward =>
                val reversedIhType = Eq.mkType(eqTpe, rhsIh, lhsIh)
                if Quote.convEqual(goal.ctx.size, envForCtx, goal.target, reversedIhType) then
                  TacticM.solveGoalWith(mv, symmetryProof(ihIdx, eqTpe, lhsIh))
                else
                  simplifyWithIH(mv, goal, lemmaName, direction)

  // ---- helpers ----

  /** Find a variable by name in the context. Returns (index, type). */
  private def findVarByName(ctx: Context, name: String): Either[TacticError, (Int, Term)] =
    ctx.entries.zipWithIndex.collectFirst {
      case (e, i) if e.name == name =>
        val tpe = Subst.shift(i + 1, e.tpe)
        (i, tpe)
    }.toRight(TacticError.Custom(s"Variable '$name' not found in context"))

  /** Extract the inductive type name from a raw type term (no whnf). */
  private def extractIndNameRaw(t: Term): Either[TacticError, String] = t match
    case Term.Ind(name, _, _) => Right(name)
    // Inductive families such as Eq are represented as applied inductives:
    // Eq(A, lhs, rhs) = App(App(Ind("Eq"), lhs), rhs). Peel applications.
    case Term.App(fn, _)       => extractIndNameRaw(fn)
    case _ => Left(TacticError.Custom(
      s"Expected inductive type for induction, got: ${t.show}"
    ))

  private def resolveIndDef(indName: String)(using env: GlobalEnv): Either[TacticError, IndDef] =
    env.lookupInd(indName) match
      case Some(indDef) => Right(indDef)
      case None if indName == "Eq" => Right(eqIndDef)
      case None =>
        Left(TacticError.Custom(s"Unknown inductive type '$indName' in GlobalEnv"))

  /** Transform goal.target into the induction motive body.
   *
   *  Replaces Var(varIdx) with Var(0) (the _n binder of the Pi/Lam),
   *  and shifts variables below varIdx (at Var(i) for i < varIdx) up by 1
   *  to make room for the new _n binder at Var(0).
   *  Variables above varIdx (i > varIdx) are unchanged (net zero: -1 remove + 1 _n).
   *
   *  For varIdx=0: result equals goal.target unchanged.
   */
  private def computeMotiveBody(goalTarget: Term, varIdx: Int): Term =
    def go(depth: Int, t: Term): Term = t match
      case Term.Var(i) =>
        val absI = i - depth
        if absI < 0      then t                           // bound inside inner binders
        else if absI == varIdx then Term.Var(depth)       // induction var → _n at current depth
        else if absI < varIdx  then Term.Var(i + 1)       // vars above varIdx: shift +1 for _n
        else                        t                     // vars below varIdx: unchanged
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
    go(0, goalTarget)

  private def buildEnvWithDefs(ctx: Context): Env =
    ctx.entries.reverse.foldLeft(List.empty[Semantic]) { (partialEnv, entry) =>
      entry match
        case Context.Entry.Assum(_, _) =>
          Semantic.freshVar(partialEnv.size) :: partialEnv
        case Context.Entry.Def(_, _, defn) =>
          Eval.eval(partialEnv, defn) :: partialEnv
    }
