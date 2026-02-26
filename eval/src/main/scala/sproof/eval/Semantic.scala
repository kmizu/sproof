package sproof.eval

import sproof.core.Term

/** Semantic domain for Normalization by Evaluation (NbE).
 *
 *  Closures are Scala functions (Semantic => Semantic), giving efficient
 *  evaluation without explicit substitution at each step.
 */
enum Semantic:
  /** λ abstraction: name, domain type, and a Scala closure for the body. */
  case SLam(name: String, tpe: Semantic, body: Semantic => Semantic)

  /** Dependent function type (Pi): domain and a closure for the codomain. */
  case SPi(name: String, dom: Semantic, cod: Semantic => Semantic)

  /** Universe at level l. */
  case SUni(level: Int)

  /** Constructor value: fully applied. */
  case SCon(name: String, indRef: String, args: List[Semantic])

  /** Stuck computation: a neutral head applied to a spine of arguments. */
  case SNeu(head: Neutral, spine: List[Semantic])

  /** Semantic fixpoint value.
   *
   *  Wraps the ORIGINAL closed Fix term so that quoting is always finite.
   *  When applied to a neutral argument, produces SNeu(NFix(originalTerm, arg), []).
   *  When applied to a concrete argument, calls `applyFn` (which evaluates the body).
   *
   *  The key invariant: `originalTerm` is a closed Term.Fix; it never references
   *  free De Bruijn variables from an outer environment (all global defs are inlined
   *  by the Elaborator, so global Fix terms are always closed).
   */
  case SFixPoint(name: String, originalTerm: Term, applyFn: Semantic => Semantic)

  def isNeu: Boolean = this match
    case SNeu(_, _) => true
    case _          => false

/** Neutral terms: computations stuck on a free variable or unresolved case. */
enum Neutral:
  /** A free variable identified by De Bruijn *level* (outermost = 0). */
  case NVar(level: Int)

  /** Application of a neutral function to a semantic argument. */
  case NApp(fn: Neutral, arg: Semantic)

  /** Pattern match on a neutral scrutinee. */
  case NMat(
    scrutinee: Neutral,
    cases: List[NeutralCase],
    returnTpe: Semantic => Semantic,
  )

  /** Fixed point applied to a neutral first argument.
   *
   *  `originalTerm` is the closed Term.Fix for the fixpoint; quoting it
   *  produces `App(originalTerm, quoteNeutral(arg))` — always finite.
   */
  case NFix(originalTerm: Term, arg: Neutral)

  /** Inductive type constant (distinguished by name so different types are non-equal). */
  case NInd(name: String)

/** One case branch in a neutral match, represented as a closure over ctor args.
 *
 *  `termBody` stores the original MatchCase and `capturedEnv` is the evaluation
 *  environment at the time the NeutralCase was created.  `Quote.quoteNeutral`
 *  uses these to reconstruct the branch body term by quoting each env value and
 *  substituting — avoiding the infinite recursion that calling `body` would cause
 *  for recursive functions like `plus`.
 */
case class NeutralCase(
  ctor:        String,
  bindings:    Int,
  body:        List[Semantic] => Semantic,
  termBody:    sproof.core.MatchCase,
  capturedEnv: List[Semantic],
)

object Semantic:
  val Type0: Semantic = SUni(0)
  val Type1: Semantic = SUni(1)

  /** Create a fresh semantic variable at the given De Bruijn level. */
  def freshVar(level: Int): Semantic = SNeu(Neutral.NVar(level), Nil)

  /** Apply a semantic function to a semantic argument (call-by-value). */
  def apply(fn: Semantic, arg: Semantic): Semantic = fn match
    case SLam(_, _, body)                  => body(arg)
    case SNeu(h, sp)                       => SNeu(h, sp :+ arg)
    case SFixPoint(_, fixTerm, applyFn)    =>
      arg match
        case SNeu(h, sp) =>
          // Neutral argument: create a stuck NFix neutral rather than evaluating the body.
          // This prevents infinite recursion during quoting.
          SNeu(Neutral.NFix(fixTerm, h), sp)
        case _ =>
          // Concrete argument: evaluate via the closure (terminates for structural recursion).
          applyFn(arg)
    case other => throw RuntimeException(s"Cannot apply non-function: $other")
