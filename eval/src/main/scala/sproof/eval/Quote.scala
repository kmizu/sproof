package sproof.eval

import sproof.core.{Term, Param, Ctor, MatchCase}

/** Quote a semantic value back to a Term (read-back phase of NbE).
 *
 *  `size` = current context depth (= next available De Bruijn level).
 *  Fresh variables are SNeu(NVar(level)); quoted as Var(size - level - 1).
 */
object Quote:
  def quote(size: Int, s: Semantic): Term = s match
    case Semantic.SLam(x, tpe, body) =>
      val v    = Semantic.freshVar(size)
      Term.Lam(x, quote(size, tpe), quote(size + 1, body(v)))

    case Semantic.SPi(x, dom, cod) =>
      val v    = Semantic.freshVar(size)
      Term.Pi(x, quote(size, dom), quote(size + 1, cod(v)))

    case Semantic.SUni(l) =>
      Term.Uni(l)

    case Semantic.SCon(name, ref, args) =>
      Term.Con(name, ref, args.map(quote(size, _)))

    case Semantic.SNeu(head, spine) =>
      spine.foldLeft(quoteNeutral(size, head))((acc, arg) =>
        Term.App(acc, quote(size, arg)))

    case Semantic.SFixPoint(_, origTerm, _) =>
      // Return the original closed Fix term directly — no unfolding needed.
      origTerm

  private def quoteNeutral(size: Int, n: Neutral): Term = n match
    case Neutral.NVar(level) =>
      Term.Var(size - level - 1)  // convert De Bruijn level → index

    case Neutral.NApp(fn, arg) =>
      Term.App(quoteNeutral(size, fn), quote(size, arg))

    case Neutral.NMat(scrut, cases, _) =>
      val scrutTerm = quoteNeutral(size, scrut)
      // Reconstruct case body terms using the stored capturedEnv and termBody.
      // We do NOT call nc.body(freshVars) here because for recursive functions
      // that produces an infinite chain of stuck matches (StackOverflowError).
      // Instead, we quote each captured environment value and substitute it
      // into the original term body.
      val caseTerms = cases.map { nc =>
        val envTerms = nc.capturedEnv.map(quote(size + nc.bindings, _))
        val bodyTerm = substCapturedEnv(nc.termBody.body, nc.bindings, envTerms)
        MatchCase(nc.ctor, nc.bindings, bodyTerm)
      }
      Term.Mat(scrutTerm, caseTerms, Term.Uni(0))

    case Neutral.NFix(origTerm, arg) =>
      // A fixpoint applied to a neutral argument: just write it back as
      // App(origTerm, quoteNeutral(arg)).  origTerm is the closed Fix term.
      Term.App(origTerm, quoteNeutral(size, arg))

    case Neutral.NInd(name) =>
      Term.Ind(name, Nil, Nil)

  /** Normalize a term: eval then quote. */
  def normalize(size: Int, env: Env, t: Term): Term =
    quote(size, Eval.eval(env, t))

  /** Definitional equality: compare normal forms. */
  def convEqual(size: Int, env: Env, t1: Term, t2: Term): Boolean =
    alphaEq(quote(size, Eval.eval(env, t1)), quote(size, Eval.eval(env, t2)))

  /** Alpha-equality (syntactic equality modulo binder names). */
  def alphaEq(t1: Term, t2: Term): Boolean = (t1, t2) match
    case (Term.Var(i), Term.Var(j))                        => i == j
    case (Term.App(f1, a1), Term.App(f2, a2))              => alphaEq(f1, f2) && alphaEq(a1, a2)
    case (Term.Lam(_, tp1, b1), Term.Lam(_, tp2, b2))     => alphaEq(tp1, tp2) && alphaEq(b1, b2)
    case (Term.Pi(_, d1, c1), Term.Pi(_, d2, c2))          => alphaEq(d1, d2) && alphaEq(c1, c2)
    case (Term.Let(_, tp1, df1, b1), Term.Let(_, tp2, df2, b2)) =>
      alphaEq(tp1, tp2) && alphaEq(df1, df2) && alphaEq(b1, b2)
    case (Term.Uni(l1), Term.Uni(l2))                      => l1 == l2
    case (Term.Con(n1, r1, as1), Term.Con(n2, r2, as2))    =>
      n1 == n2 && r1 == r2 && as1.length == as2.length &&
      as1.zip(as2).forall(alphaEq(_, _))
    case (Term.Ind(n1, _, _), Term.Ind(n2, _, _))          => n1 == n2
    case (Term.Meta(i), Term.Meta(j))                      => i == j
    case (Term.Fix(_, tp1, b1), Term.Fix(_, tp2, b2))      => alphaEq(tp1, tp2) && alphaEq(b1, b2)
    case (Term.Mat(s1, cs1, r1), Term.Mat(s2, cs2, r2))    =>
      alphaEq(s1, s2) && alphaEq(r1, r2) &&
      cs1.length == cs2.length &&
      cs1.zip(cs2).forall { (mc1, mc2) =>
        mc1.ctor == mc2.ctor && mc1.bindings == mc2.bindings && alphaEq(mc1.body, mc2.body)
      }
    case _                                                  => false

  /** Substitute quoted captured-environment terms into a match-case body.
   *
   *  In `body` (relative to depth 0):
   *  - Var(i) with i < bindingCount are ctor-arg variables — leave unchanged.
   *  - Var(bindingCount + j) is replaced with envTerms(j) (shifted to account
   *    for intermediate binders traversed below depth 0).
   *
   *  This allows NMat quoting to avoid calling the semantic body closure, which
   *  would diverge for recursive functions like `plus`.
   */
  private def substCapturedEnv(body: Term, bindingCount: Int, envTerms: List[Term]): Term =
    import sproof.core.{Param, Ctor}
    def go(depth: Int, t: Term): Term = t match
      case Term.Var(i) =>
        val absI = i - depth          // index relative to substitution site
        if absI < 0 then t            // bound inside a binder within body: keep
        else if absI < bindingCount then t  // ctor arg: keep
        else
          val j = absI - bindingCount
          if j < envTerms.length then sproof.core.Subst.shift(depth, envTerms(j))
          else t                      // out of range: keep (shouldn't happen in well-typed code)
      case Term.App(f, a)          => Term.App(go(depth, f), go(depth, a))
      case Term.Lam(x, tp, b)     => Term.Lam(x, go(depth, tp), go(depth + 1, b))
      case Term.Pi(x, d, c)       => Term.Pi(x, go(depth, d), go(depth + 1, c))
      case Term.Let(x, tp, df, b) => Term.Let(x, go(depth, tp), go(depth, df), go(depth + 1, b))
      case Term.Uni(_)             => t
      case Term.Meta(_)            => t
      case Term.Ind(n, ps, cs)    =>
        Term.Ind(n,
          ps.map(p => Param(p.name, go(depth, p.tpe))),
          cs.map(c => Ctor(c.name, go(depth, c.tpe))))
      case Term.Con(n, r, args)   => Term.Con(n, r, args.map(go(depth, _)))
      case Term.Fix(n, tp, b)     => Term.Fix(n, go(depth, tp), go(depth + 1, b))
      case Term.Mat(s, cs, rt)    =>
        Term.Mat(go(depth, s),
          cs.map(mc => mc.copy(body = go(depth + mc.bindings, mc.body))),
          go(depth, rt))
    go(0, body)
