package sproof.tactic

import sproof.core.Term

/** Helpers for propositional equality (the Eq inductive type).
 *
 *  Encoding:
 *    Eq : Pi(T:Type_l, Pi(a:T, Pi(b:T, Type_l)))
 *    refl : Pi(T:Type_l, Pi(a:T, Eq T a a))
 *
 *  We represent:
 *    Eq T a b  as  App(App(App(Ind("Eq",...), T), a), b)
 *    refl a    as  Con("refl", "Eq", List(a))
 *
 *  The Ind node uses the name "Eq" as a sentinel; the actual well-typing
 *  of Eq as an inductive type is handled by the global environment (Phase 3+).
 *  For Phase 2, the Kernel directly recognises these forms.
 */
object Eq:

  private val eqInd: Term = Term.Ind("Eq", Nil, Nil)

  /** Construct the type `Eq tpe lhs rhs`. */
  def mkType(tpe: Term, lhs: Term, rhs: Term): Term =
    Term.App(Term.App(Term.App(eqInd, tpe), lhs), rhs)

  /** Construct the reflexivity proof `refl(lhs)`. */
  def mkRefl(lhs: Term): Term =
    Term.Con("refl", "Eq", List(lhs))

  /** Extract `(tpe, lhs, rhs)` from a term if it has the shape `Eq tpe lhs rhs`.
   *
   *  Handles both forms:
   *  - 3-arg: `App(App(App(Ind("Eq",...), tpe), lhs), rhs)` — explicit type
   *  - 2-arg: `App(App(Ind("Eq",...), lhs), rhs)` — type omitted (from Elaborator)
   *    In the 2-arg case, `tpe` is returned as `Meta(-1)` (unknown placeholder).
   */
  def extract(t: Term): Option[(Term, Term, Term)] = t match
    case Term.App(Term.App(Term.App(Term.Ind("Eq", _, _), tpe), lhs), rhs) =>
      Some((tpe, lhs, rhs))
    case Term.App(Term.App(Term.Ind("Eq", _, _), lhs), rhs) =>
      Some((Term.Meta(-1), lhs, rhs))  // 2-arg form: type is unknown
    case _ =>
      None

  /** Check whether a term is a `refl` constructor application. */
  def isRefl(t: Term): Boolean = t match
    case Term.Con("refl", "Eq", _) => true
    case _                         => false
