package sproof.tactic

import sproof.core.{Term, Context, Subst, GlobalEnv, IndDef, CtorDef, MatchCase, Param, Ctor}
import sproof.checker.Bidirectional
import sproof.eval.{Quote, Eval, Env, Semantic, Neutral}

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
      indDef       <- TacticM.liftEither(
                        env.lookupInd(indName).toRight(
                          TacticError.Custom(s"Unknown inductive type '$indName' in GlobalEnv")
                        )
                      )
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
    // propWithVar0 = the motive (goal with Var(varIdx) as Var(0)).
    // For varIdx=0 (common case with single-variable context): propWithVar0 = goal.target.
    val propWithVar0 = goal.target
    val ctx_minus    = removeFromContext(goal.ctx, varIdx)
    for
      fixCasesData <- buildFixCases(ctx_minus, varIdx, goal.target, propWithVar0, indDef, caseSpecs)
      fixCases      = fixCasesData.map(_._1)
      // Fix("_rec", Pi("_n", T, P), Lam("_n", T, Mat(Var(0), cases, P)))
      fixTerm       = Term.Fix("_rec",
                        Term.Pi("_n", varTpe, propWithVar0),
                        Term.Lam("_n", varTpe,
                          Term.Mat(Term.Var(0), fixCases, propWithVar0)))
      proofTerm     = Term.App(fixTerm, Term.Var(varIdx))
      _            <- TacticM.solveGoalWith(mv, proofTerm)
    yield ()

  /** Build Fix-style match cases for each constructor.
   *  Returns list of (MatchCase, subCtx, subGoal).
   */
  private def buildFixCases(
    ctx_minus:   Context,
    varIdx:      Int,
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
        buildFixCase(ctx_minus, varIdx, goal, propWithVar0, indDef, ctorDef, hasIH, extraBindings).map { triple =>
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
   *    Var(0)   = last ctor arg (k for succ)
   *    Var(1)   = _n  (Lam binder)
   *    Var(2)   = _rec (Fix binder)
   *    Var(3+)  = outer context vars
   *
   *  For the IH Let: Let("ih", P, App(Var(n+1), Var(0)), Meta(mv))
   *    where Var(n+1) = _rec and Var(0) = the last (recursive) ctor arg.
   */
  private def buildFixCase(
    ctx_minus:    Context,
    varIdx:       Int,
    goal:         Term,
    propWithVar0: Term,
    indDef:       IndDef,
    ctorDef:      CtorDef,
    hasIH:        Boolean,
    extraBindings: List[String] = Nil,
  )(using env: GlobalEnv): TacticM[(MatchCase, Context, Term)] =
    val n          = ctorDef.argTpes.length
    val ctorArgVars = (0 until n).toList.map(i => Term.Var(n - 1 - i))
    val ctorTerm   = Term.Con(ctorDef.name, indDef.name, ctorArgVars)
    val specialGoal = specializeGoal(goal, varIdx, ctorTerm, n)
    // Use user-supplied binding names for ctor args; fall back to "_" if not provided
    val argNames   = extraBindings.take(n).padTo(n, "_")
    val ctorCtx    = ctorDef.argTpes.zip(argNames).foldLeft(ctx_minus)((c, pair) => c.extend(pair._2, pair._1))
    if !hasIH then
      for mv <- TacticM.addGoal(ctorCtx, specialGoal)
      yield (MatchCase(ctorDef.name, n, Term.Meta(mv.id)), ctorCtx, specialGoal)
    else
      // IH type = propWithVar0 (P with Var(0) = the recursive ctor arg = Var(0) in case body ctx)
      // _rec is at Var(n+1) in the case body context (n ctor arg binders + 1 Lam binder above)
      // The recursive arg (last, = Var(0)) is passed to _rec.
      val ihType       = propWithVar0          // P(k): P with Var(0)=k in case body ctx
      val recFuncRef   = Term.Var(n + 1)       // _rec (at depth n+1 in case body)
      val recArgRef    = Term.Var(0)           // k: the (last) recursive ctor arg
      val ihDef        = Term.App(recFuncRef, recArgRef)
      // Sub-goal context: [ih: shift(1,P), k: T, ...ctx_minus...]
      val ihTypeInSub  = Subst.shift(1, ihType)   // shift for the ih binder
      val subCtx       = ctorCtx.extend("ih", ihTypeInSub)
      val specialGoalInSub = Subst.shift(1, specialGoal)
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
   *  `simplify []` / `simplify` — delegates to `trivial` (NbE definitional equality).
   *
   *  `simplify [ih]` where `ih : lhs = rhs` — applies the J-rule (congruence):
   *    given goal `Eq(f(lhs), f(rhs))`, constructs the J-rule proof term:
   *    `Mat(ih, [refl(x) => refl(f(lhs_shifted))], P)` where `P(x) = Eq(f(lhs), f(x))`.
   *    Currently handles single-constructor-application wrappers, e.g. `succ(lhs) = succ(rhs)`.
   *
   *  Falls back to `trivial` if the pattern is not recognised.
   */
  def simplify(lemmas: List[String] = Nil)(using env: GlobalEnv): TacticM[Unit] =
    lemmas match
      case Nil => trivial
      case ihName :: _ =>
        for
          goalPair    <- TacticM.currentGoal
          (mv, goal)   = goalPair
          result      <- simplifyWithIH(mv, goal, ihName)
        yield result

  /** Try to close the goal using the J-rule with hypothesis `ihName`. */
  private def simplifyWithIH(mv: MetaVar, goal: Goal, ihName: String)(using env: GlobalEnv): TacticM[Unit] =
    findVarByName(goal.ctx, ihName) match
      case Left(_) => trivial   // ih not found: fall back
      case Right((ihIdx, _)) =>
        // Use the raw stored type (not the over-shifted version from findVarByName)
        // because buildFixCase stores ih.tpe already shifted for the extended context.
        val ihTypeRaw = goal.ctx.entries(ihIdx).tpe
        Eq.extract(ihTypeRaw) match
          case None => trivial  // ih not an equality: fall back
          case Some((_, lhsIh, rhsIh)) =>
            Eq.extract(goal.target) match
              case None => trivial  // goal not an equality: fall back
              case Some((_, lhsGoal, rhsGoal)) =>
                // Normalise all sides for comparison and J-rule construction
                val envForCtx  = buildEnvWithDefs(goal.ctx)
                val n          = goal.ctx.size
                val normLhs    = Quote.normalize(n, envForCtx, lhsGoal)
                val normRhs    = Quote.normalize(n, envForCtx, rhsGoal)
                val normLhsIh  = Quote.normalize(n, envForCtx, lhsIh)
                val normRhsIh  = Quote.normalize(n, envForCtx, rhsIh)
                // Fast path: goal LHS ≡ ih LHS and goal RHS ≡ ih RHS
                // e.g. mult(succ_k, 0) normalises to mult(k,0), ih: mult(k,0)=0
                if Quote.convEqual(n, envForCtx, normLhs, normLhsIh) &&
                   Quote.convEqual(n, envForCtx, normRhs, normRhsIh) then
                  TacticM.solveGoalWith(mv, Term.Var(ihIdx))
                else
                  buildJRuleProof(mv, goal, ihIdx, lhsIh, normLhs, normRhs)

  /** Build a J-rule proof for goal `Eq(normLhs, normRhs)` given `ih` at `ihIdx`.
   *
   *  Pattern: normLhs = Con(name, ref, [l]) and normRhs = Con(name, ref, [r]).
   *  Motive: P(x) = Eq(Con(name, ref, [shift(1, l)]), Con(name, ref, [Var(0)])).
   *  Proof: Mat(Var(ihIdx), [MatchCase("refl", 1, refl(Con(name,[shift(1,l)])))], P).
   */
  private def buildJRuleProof(
    mv:     MetaVar,
    goal:   Goal,
    ihIdx:  Int,
    lhsIh:  Term,
    normLhs: Term,
    normRhs: Term,
  )(using env: GlobalEnv): TacticM[Unit] =
    (normLhs, normRhs) match
      case (Term.Con(lname, lref, List(l)), Term.Con(rname, rref, List(r)))
           if lname == rname && lref == rref =>
        // Motive: P(x) = Eq(Con(name,[shift(1,l)]), Con(name,[Var(0)]))
        val lhsInLam  = Subst.shift(1, l)
        val motiveBody = Term.App(
          Term.App(Term.Ind("Eq", Nil, Nil),
            Term.Con(lname, lref, List(lhsInLam))),
          Term.Con(rname, rref, List(Term.Var(0))))
        val motiveFunc = Term.Lam("x", Term.Ind(lref, Nil, Nil), motiveBody)
        // Branch body: refl(Con(name, [shift(1, l)]))
        val body      = Term.Con("refl", "Eq", List(Term.Con(lname, lref, List(lhsInLam))))
        // J-rule proof: Mat(ih, [refl case], motive)
        val proofTerm = Term.Mat(
          Term.Var(ihIdx),
          List(MatchCase("refl", 1, body)),
          motiveFunc)
        TacticM.solveGoalWith(mv, proofTerm)
      case _ =>
        // Pattern not recognised: try trivial as fallback
        trivial

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
      indDef       <- TacticM.liftEither(
                        env.lookupInd(indName).toRight(
                          TacticError.Custom(s"Unknown inductive type '$indName' in GlobalEnv")
                        )
                      )
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
    lemmas match
      case Nil => trivial
      case lemmaName :: rest =>
        for
          goalPair   <- TacticM.currentGoal
          (mv, goal)  = goalPair
          _          <- rewriteWithLemma(mv, goal, lemmaName)
          _          <- if rest.nonEmpty then rewrite(rest) else TacticM.pure(())
        yield ()

  /** Rewrite goal using a single lemma. */
  private def rewriteWithLemma(
    mv:        MetaVar,
    goal:      Goal,
    lemmaName: String,
  )(using env: GlobalEnv): TacticM[Unit] =
    findVarByName(goal.ctx, lemmaName) match
      case Left(err) =>
        TacticM.fail(err)
      case Right((ihIdx, ihType)) =>
        val envForCtx = buildEnvWithDefs(goal.ctx)
        Eq.extract(ihType) match
          case None =>
            TacticM.fail(TacticError.Custom(s"rewrite: $lemmaName is not an equality"))
          case Some((_, lhsIh, rhsIh)) =>
            // Check if the goal exactly matches the hypothesis type
            if Quote.convEqual(goal.ctx.size, envForCtx, goal.target, ihType) then
              // Goal is exactly the same equality — just use the hypothesis directly
              TacticM.solveGoalWith(mv, Term.Var(ihIdx))
            else
              // Try J-rule approach through simplify
              simplifyWithIH(mv, goal, lemmaName)

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
    case _ => Left(TacticError.Custom(
      s"Expected inductive type for induction, got: ${t.show}"
    ))

  private def buildEnvWithDefs(ctx: Context): Env =
    ctx.entries.reverse.foldLeft(List.empty[Semantic]) { (partialEnv, entry) =>
      entry match
        case Context.Entry.Assum(_, _) =>
          Semantic.freshVar(partialEnv.size) :: partialEnv
        case Context.Entry.Def(_, _, defn) =>
          Eval.eval(partialEnv, defn) :: partialEnv
    }
