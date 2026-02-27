package sproof.tactic

import sproof.core.{Term, Context}

/** Formats a ProofState as an S-expression for LLM consumption.
  *
  * S-expression format is chosen because:
  * - LLMs are extensively trained on Lisp/S-expression syntax
  * - Regular nested structure is unambiguous and easy to parse
  * - No fragile whitespace or ASCII-art conventions
  *
  * Example output:
  * {{{
  *   (proof-state
  *     (context
  *       (hyp "n" "Nat")
  *       (hyp "k" "Nat")
  *       (hyp "ih" "(plus k Nat.zero) = k"))
  *     (goal "(plus (Nat.succ k) Nat.zero) = (Nat.succ k)")
  *     (error "trivial: not definitionally equal"))
  * }}}
  */
object ProofStatePretty:

  /** Format the first failing goal and its error as an S-expression. */
  def format(state: ProofState, err: TacticError): String =
    state.goals.headOption match
      case None =>
        s"""(proof-state (context) (goal "no goals") (error "${esc(err.getMessage)}"))"""
      case Some((_, goal)) =>
        val namesForTerm = goal.ctx.entries.map(_.name).toList
        val goalStr      = Term.show(goal.target, namesForTerm)
        val ctxLines     = formatContext(goal.ctx)
        val hintLine = err.hint.map(h => s"\n  (hint \"${esc(h)}\")").getOrElse("")
        s"""(proof-state
  (context
$ctxLines)
  (goal "${esc(goalStr)}")
  (error "${esc(err.getMessage)}")$hintLine)"""

  /** Format context entries as `(hyp "name" "type")` lines, outermost first.
    *
    * Note on De Bruijn indexing: some tactics (e.g., `induction`) store hypothesis
    * types that have been pre-shifted by 1 to account for their own binder.  This
    * means `e.tpe` for entry at position `i` is valid in the context that includes
    * entries[i] itself (i.e., names from `entries.drop(i)` rather than
    * `entries.drop(i+1)`).  Using `entries.drop(i)` as the name list handles both
    * the normal case (the extra name at position 0 simply goes unused) and the
    * pre-shifted case correctly.
    */
  private def formatContext(ctx: Context): String =
    if ctx.entries.isEmpty then "    (empty)"
    else
      val entries = ctx.entries
      // entries(0) = innermost (De Bruijn 0), entries.last = outermost
      // Display outermost-first: iterate from last to 0
      val lines = entries.zipWithIndex.reverse.map { case (e, i) =>
        val namesFromHere = entries.drop(i).map(_.name)
        val tpeStr        = Term.show(e.tpe, namesFromHere)
        s"""    (hyp "${esc(e.name)}" "${esc(tpeStr)}")"""
      }
      lines.mkString("\n")

  /** Escape backslashes, double-quotes, and newlines inside sexp strings. */
  private def esc(s: String): String =
    s.replace("\\", "\\\\")
     .replace("\"", "\\\"")
     .replace("\n", "\\n")
