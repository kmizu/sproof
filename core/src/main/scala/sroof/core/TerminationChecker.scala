package sroof.core

/** Structural termination checker for Fix (recursive functions).
 *
 *  Ensures every recursive call is applied to a structurally smaller argument.
 *  "Structurally smaller" means a variable bound by a pattern match on the
 *  decreasing parameter (i.e., a constructor sub-component).
 *
 *  Algorithm:
 *  1. For Fix(f, tpe, body), the body has Var(0) = f (self-reference).
 *  2. Find all occurrences of f in body.
 *  3. For each recursive call f(a1)(a2)...(an), verify that at least one ai
 *     is a structurally smaller variable, and that all other arguments do not
 *     contain an unguarded reference to f.
 *
 *  This supports multi-argument functions where any argument may be the
 *  decreasing one (e.g. `matches(derive(r,c), t)` where t, not r, decreases).
 *  This is a simplified version of Coq's guard condition.
 */
object TerminationChecker:

  /** Check that a term satisfies the structural recursion guard.
   *
   *  Only Fix terms are checked; all other terms pass trivially.
   */
  def check(t: Term)(using env: GlobalEnv): Either[String, Unit] =
    t match
      case Term.Fix(name, tpe, body) =>
        // fixIdx = 0 in body (the self-reference De Bruijn index at depth 0)
        checkBody(name, fixIdx = 0, body)
      case _ =>
        Right(())

  /** Check the body of a Fix for guarded recursion.
   *
   *  `fixIdx` is the De Bruijn index of the fixpoint self-reference at the
   *  current depth. It increases as we go under binders.
   */
  private def checkBody(name: String, fixIdx: Int, body: Term): Either[String, Unit] =
    body match
      // Lambda: the main case. We look for the pattern:
      //   λx. match x { case C(y1..yn) => ...f(yi)... }
      case Term.Lam(_, _, lamBody) =>
        // Under this lambda, fixIdx shifts up by 1
        checkBody(name, fixIdx + 1, lamBody)

      // Match: binds constructor sub-components as smaller variables
      case Term.Mat(scrutinee, cases, _) =>
        // The scrutinee must be a variable for us to track what's "smaller"
        val scrutIdx = scrutinee match
          case Term.Var(i) => Some(i)
          case _           => None

        cases.foldLeft[Either[String, Unit]](Right(())) { (acc, mc) =>
          acc.flatMap { _ =>
            // Inside this case branch, fixIdx shifts by mc.bindings
            val newFixIdx = fixIdx + mc.bindings
            // The constructor-bound variables (Var(0)..Var(mc.bindings-1))
            // are structurally smaller than the scrutinee
            val smallerVars: Set[Int] = scrutIdx match
              case Some(_) => (0 until mc.bindings).toSet
              case None    => Set.empty
            checkGuarded(name, newFixIdx, mc.body, smallerVars)
          }
        }

      // If the body is just a reference to f with no lambda wrapping, that's
      // a bare self-reference (non-terminating)
      case Term.Var(i) if i == fixIdx =>
        Left(s"Termination check failed: '$name' is used as a bare value (no guarded recursion)")

      // App at top level (not inside a match) — check if it's a recursive call
      case Term.App(fn, _) if containsFixRef(fn, fixIdx) =>
        Left(s"Termination check failed: '$name' makes a recursive call outside of a match case")

      // Other terms at the top level — check that f doesn't escape
      case _ =>
        if containsUnguardedRef(body, fixIdx) then
          Left(s"Termination check failed: '$name' has unguarded recursive reference")
        else
          Right(())

  /** Peel a curried application into (function, args): App(App(f, a1), a2) → (f, [a1, a2]). */
  private def peelArgs(t: Term): (Term, List[Term]) =
    def go(t: Term, acc: List[Term]): (Term, List[Term]) = t match
      case Term.App(fn, arg) => go(fn, arg :: acc)
      case other             => (other, acc)
    go(t, Nil)

  /** Check that all recursive calls in `t` are to structurally smaller arguments.
   *
   *  `smallerVars` contains the De Bruijn indices (at current depth) of variables
   *  that are structurally smaller than the decreasing argument.
   *
   *  For a call f(a1)(a2)...(an), we accept if at least one ai is a structurally
   *  smaller variable and all other arguments don't contain unguarded fix references.
   *  This supports functions that recurse on any one of their arguments.
   */
  private def checkGuarded(name: String, fixIdx: Int, t: Term, smallerVars: Set[Int]): Either[String, Unit] =
    t match
      case Term.Var(i) if i == fixIdx =>
        // Bare reference to f without application — this is a higher-order escape
        Left(s"Termination check failed: '$name' escapes as a value in a match branch")

      case app @ Term.App(_, _) =>
        val (fn, args) = peelArgs(app)
        fn match
          case Term.Var(fIdx) if fIdx == fixIdx =>
            // Recursive call f(a1)(a2)...(an): at least one ai must be a smaller variable;
            // all remaining args must not contain unguarded fix references.
            val smallerIdx = args.indexWhere {
              case Term.Var(i) => smallerVars.contains(i)
              case _           => false
            }
            if smallerIdx < 0 then
              Left(s"Termination check failed: '$name' is called with a non-structurally-smaller argument")
            else
              val otherArgs = args.zipWithIndex.filterNot(_._2 == smallerIdx).map(_._1)
              otherArgs.foldLeft[Either[String, Unit]](Right(())) { (acc, a) =>
                acc.flatMap(_ => checkGuarded(name, fixIdx, a, smallerVars))
              }
          case _ =>
            // Non-recursive application: check fn and all args
            for
              _ <- checkGuarded(name, fixIdx, fn, smallerVars)
              _ <- args.foldLeft[Either[String, Unit]](Right(())) { (acc, a) =>
                     acc.flatMap(_ => checkGuarded(name, fixIdx, a, smallerVars))
                   }
            yield ()

      case Term.Lam(_, tp, body) =>
        for
          _ <- checkGuarded(name, fixIdx, tp, smallerVars)
          _ <- checkGuarded(name, fixIdx + 1, body, smallerVars.map(_ + 1))
        yield ()

      case Term.Pi(_, dom, cod) =>
        for
          _ <- checkGuarded(name, fixIdx, dom, smallerVars)
          _ <- checkGuarded(name, fixIdx + 1, cod, smallerVars.map(_ + 1))
        yield ()

      case Term.Let(_, tp, defn, body) =>
        for
          _ <- checkGuarded(name, fixIdx, tp, smallerVars)
          _ <- checkGuarded(name, fixIdx, defn, smallerVars)
          _ <- checkGuarded(name, fixIdx + 1, body, smallerVars.map(_ + 1))
        yield ()

      case Term.Con(_, _, args) =>
        args.foldLeft[Either[String, Unit]](Right(())) { (acc, arg) =>
          acc.flatMap(_ => checkGuarded(name, fixIdx, arg, smallerVars))
        }

      case Term.Mat(scrut, cases, rt) =>
        for
          _ <- checkGuarded(name, fixIdx, scrut, smallerVars)
          _ <- checkGuarded(name, fixIdx, rt, smallerVars)
          _ <- cases.foldLeft[Either[String, Unit]](Right(())) { (acc, mc) =>
            acc.flatMap { _ =>
              val n = mc.bindings
              val newFixIdx = fixIdx + n
              // Constructor-bound vars are smaller if scrutinee is smaller
              val scrutSmaller = scrut match
                case Term.Var(si) if smallerVars.contains(si) =>
                  (0 until n).toSet
                case _ => Set.empty[Int]
              val newSmaller = smallerVars.map(_ + n) ++ scrutSmaller
              checkGuarded(name, newFixIdx, mc.body, newSmaller)
            }
          }
        yield ()

      case Term.Fix(_, tp, body) =>
        for
          _ <- checkGuarded(name, fixIdx, tp, smallerVars)
          _ <- checkGuarded(name, fixIdx + 1, body, smallerVars.map(_ + 1))
        yield ()

      case _ => Right(())  // Var (not fixIdx), Uni, Meta, Ind

  /** Check if a term contains a reference to the fixpoint at the given index. */
  private def containsFixRef(t: Term, fixIdx: Int): Boolean = t match
    case Term.Var(i)            => i == fixIdx
    case Term.App(fn, arg)      => containsFixRef(fn, fixIdx) || containsFixRef(arg, fixIdx)
    case Term.Lam(_, tp, b)    => containsFixRef(tp, fixIdx) || containsFixRef(b, fixIdx + 1)
    case Term.Pi(_, d, c)      => containsFixRef(d, fixIdx) || containsFixRef(c, fixIdx + 1)
    case Term.Let(_, t, d, b)  => containsFixRef(t, fixIdx) || containsFixRef(d, fixIdx) || containsFixRef(b, fixIdx + 1)
    case Term.Con(_, _, args)   => args.exists(containsFixRef(_, fixIdx))
    case Term.Mat(s, cs, rt)   => containsFixRef(s, fixIdx) || containsFixRef(rt, fixIdx) || cs.exists(c => containsFixRef(c.body, fixIdx + c.bindings))
    case Term.Fix(_, tp, b)    => containsFixRef(tp, fixIdx) || containsFixRef(b, fixIdx + 1)
    case _                      => false

  /** Check if a term contains an unguarded reference to the fixpoint. */
  private def containsUnguardedRef(t: Term, fixIdx: Int): Boolean =
    containsFixRef(t, fixIdx)
