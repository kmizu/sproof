package sproof.tactic

import sproof.checker.TypeError

/** Errors that can arise during tactic execution. */
enum TacticError(msg: String) extends Exception(msg):
  case NoGoals
    extends TacticError("No goals remaining")

  case GoalMismatch(expected: String, got: String)
    extends TacticError(s"Goal mismatch: expected $expected, got $got")

  case NotAnEquality(goalStr: String)
    extends TacticError(s"Goal is not a propositional equality: $goalStr")

  case NotAFunction(termStr: String)
    extends TacticError(s"Term is not a function: $termStr")

  case NotAPi(goalStr: String)
    extends TacticError(s"Goal is not a Pi type (cannot introduce): $goalStr")

  case TypeCheckFailed(err: TypeError)
    extends TacticError(s"Type check failed: ${err.getMessage}")

  case Custom(message: String)
    extends TacticError(message)

  /** Contextual hint for LLM proof repair. */
  def hint: Option[String] = this match
    case NoGoals =>
      Some("All goals are already closed. Remove the extra tactic.")
    case GoalMismatch(expected, got) =>
      Some(s"Expected goal shape '$expected' but got '$got'. Check tactic arguments.")
    case NotAnEquality(goal) =>
      Some(s"Goal '$goal' is not an equality. The goal must have the form 'A = B'.")
    case NotAFunction(_) =>
      Some("Try `apply f` where `f` is a function whose return type matches the goal.")
    case NotAPi(_) =>
      Some("Use `assume x` only when the goal is a Pi/forall type.")
    case TypeCheckFailed(_) =>
      Some("The proof term has a type error. Check that arguments match parameter types.")
    case Custom(msg) if msg.contains("not definitionally equal") =>
      Some("Try `simplify [lemma1, lemma2]` to unfold definitions, or `induction` to split by cases.")
    case Custom(msg) if msg.contains("not found") || msg.contains("unknown") =>
      Some("Check spelling of the lemma/variable name, or introduce it with `have`.")
    case Custom(msg) if msg.contains("Proof incomplete") =>
      Some("Some goals remain unsolved. Add tactics for each remaining case.")
    case Custom(_) =>
      None
