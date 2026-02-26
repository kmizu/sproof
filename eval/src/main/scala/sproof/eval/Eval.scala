package sproof.eval

import sproof.core.{Term, Param, Ctor, MatchCase, Subst}

/** Environment: list of semantic values; head = De Bruijn index 0. */
type Env = List[Semantic]

object Eval:
  /** Evaluate a Term to a Semantic value under the given environment. */
  def eval(env: Env, t: Term): Semantic = t match
    case Term.Var(i) =>
      env.lift(i).getOrElse(throw RuntimeException(s"Unbound De Bruijn index $i (env size ${env.size})"))

    case Term.App(fn, arg) =>
      Semantic(eval(env, fn), eval(env, arg))

    case Term.Lam(x, tpe, body) =>
      Semantic.SLam(x, eval(env, tpe), v => eval(v :: env, body))

    case Term.Pi(x, dom, cod) =>
      Semantic.SPi(x, eval(env, dom), v => eval(v :: env, cod))

    case Term.Let(_, _, defn, body) =>
      eval(eval(env, defn) :: env, body)

    case Term.Uni(l) =>
      Semantic.SUni(l)

    case Term.Con(name, ref, args) =>
      Semantic.SCon(name, ref, args.map(eval(env, _)))

    case Term.Mat(scrutinee, cases, returnTpe) =>
      evalMatch(env, eval(env, scrutinee), cases, returnTpe)

    case Term.Fix(name, tpe, body) =>
      // Fix(f, T, body): body has Var(0) = f (self-reference).
      //
      // We represent fixpoints as SFixPoint rather than as an SLam.  SFixPoint stores
      // the ORIGINAL closed Fix term so Quote.quoteNeutral can read it back finitely,
      // and an applyFn closure for evaluating concrete applications.
      //
      // When applied to a NEUTRAL argument, Semantic.apply returns SNeu(NFix(origTerm, arg))
      // which quotes to App(origTerm, quoteNeutral(arg)) — always finite.
      //
      // When applied to a CONCRETE argument, applyFn is called; it evaluates the Fix body
      // with self = SFixPoint(...) so that recursive calls with neutral args again produce
      // SNeu(NFix(...)) rather than diverging.
      val origTerm = Term.Fix(name, tpe, body)
      def fixSem: Semantic =
        Semantic.SFixPoint(name, origTerm, argVal =>
          Semantic(eval(fixSem :: env, body), argVal)
        )
      fixSem

    case Term.Meta(id) =>
      throw RuntimeException(s"Unresolved meta variable ?$id during evaluation")

    case Term.Ind(name, _, _) =>
      // Type-level inductive: represent as a named neutral so distinct Ind types are non-equal.
      Semantic.SNeu(Neutral.NInd(name), Nil)

  private def evalMatch(env: Env, scrVal: Semantic, cases: List[MatchCase], returnTpe: Term): Semantic =
    scrVal match
      case Semantic.SCon(ctorName, _, args) =>
        cases.find(_.ctor == ctorName) match
          case Some(mc) =>
            // Extend env with constructor arguments (reversed, head = index 0)
            val extEnv = args.foldRight(env)(_ :: _)
            eval(extEnv, mc.body)
          case None =>
            throw RuntimeException(s"Non-exhaustive match: no case for constructor '$ctorName'")
      case Semantic.SNeu(h, sp) =>
        // Stuck match: record as a neutral.
        // We store both the semantic closure AND the original MatchCase + env size so
        // that Quote.quoteNeutral can read back the body without calling the closure
        // (calling the closure for recursive functions like `plus` would diverge).
        val neutralCases = cases.map { mc =>
          NeutralCase(mc.ctor, mc.bindings,
            argVals => { val extEnv = argVals.foldRight(env)(_ :: _); eval(extEnv, mc.body) },
            mc,
            env)
        }
        val retFn: Semantic => Semantic = _ => eval(env, returnTpe)
        Semantic.SNeu(Neutral.NMat(h, neutralCases, retFn), sp)
      case other =>
        throw RuntimeException(s"Pattern match on non-constructor value: $other")
