package sroof.agent

import sroof.core.{Term, Context, GlobalEnv, DefEntry}
import sroof.syntax.{Parser, Elaborator, SProof, STactic}
import sroof.Checker

/** Repairs sorry proofs in a .sroof source file.
 *
 *  Algorithm:
 *   1. Parse and elaborate the source to find all sorry defspecs.
 *   2. For each sorry defspec (in source order), run SearchLoop to find a working tactic.
 *   3. After each success, add the proved DefEntry to the env so later defspecs can use it.
 *   4. Replace `by sorry` in the source text with the found tactic.
 *   5. Return the repaired source (or the original if nothing was found).
 */
object FileRepairer:

  /** Result of a single repair attempt. */
  case class RepairResult(
    defspecName: String,
    found:       Option[STactic],
  ):
    def succeeded: Boolean = found.isDefined

  /** Repair all sorry defspecs in the source and return the modified source.
   *
   *  If a proof is found for a sorry defspec, the `by sorry` text in the
   *  source is replaced with the found tactic rendered as source text.
   *  Defspecs whose proof was not found retain `by sorry`.
   */
  def repair(source: String, fileName: String = "<input>"): String =
    val results = tryRepair(source, fileName)
    results.foldLeft(source) { (src, result) =>
      result.found match
        case None      => src
        case Some(tac) => replaceSorryInDefspec(src, result.defspecName, tac)
    }

  /** Run the repair loop and return results for each sorry defspec.
   *
   *  Iterates defspecs in source order.  After each successful repair, the
   *  proved theorem is added to the GlobalEnv so that later defspecs can
   *  reference it via `simplify` or `exact`.
   */
  def tryRepair(source: String, fileName: String = "<input>"): List[RepairResult] =
    Parser.parseProgram(source) match
      case Left(_)     => Nil
      case Right(decls) =>
        Elaborator.elaborate(decls) match
          case Left(_)     => Nil
          case Right(result) =>
            // Iterate in source order so earlier proofs are available to later ones.
            val orderedNames =
              if result.defspecOrder.nonEmpty then result.defspecOrder.filter(result.defspecs.contains)
              else result.defspecs.keys.toList
            val (results, _) = orderedNames.foldLeft((List.empty[RepairResult], result.env)) {
              case ((acc, currentEnv), name) =>
                result.defspecs.get(name) match
                  case None => (acc, currentEnv)
                  case Some((elabParams, propTerm, proof)) =>
                    if !containsSorry(proof) then (acc, currentEnv)
                    else
                      val proofCtx = elabParams.foldLeft(Context.empty) { (ctx, p) =>
                        ctx.extend(p._1, p._2)
                      }
                      val found = SearchLoop.search(proofCtx, propTerm)(using currentEnv)
                      val nextEnv = found match
                        case None => currentEnv
                        case Some(tac) =>
                          // Re-execute to obtain the proof term for env accumulation.
                          Checker.executeProof(proofCtx, propTerm, SProof.SBy(tac))(using currentEnv) match
                            case Left(_) => currentEnv
                            case Right(proofTerm) =>
                              val fullProofTerm = elabParams.foldRight(proofTerm) { (p, body) =>
                                Term.Lam(p._1, p._2, body)
                              }
                              val fullPropTerm = elabParams.foldRight(propTerm) { (p, cod) =>
                                Term.Pi(p._1, p._2, cod)
                              }
                              currentEnv.addDef(DefEntry(name, fullPropTerm, fullProofTerm))
                      (acc :+ RepairResult(name, found), nextEnv)
            }
            results

  // ---- Source text manipulation ----

  /** Replace `by sorry` (or the whole proof block) for a named defspec. */
  private def replaceSorryInDefspec(source: String, name: String, tac: STactic): String =
    // Simple approach: find `defspec <name>` then replace the first `sorry` after it
    val defspecPattern = s"defspec\\s+${java.util.regex.Pattern.quote(name)}\\b"
    val re = defspecPattern.r
    re.findFirstMatchIn(source) match
      case None => source
      case Some(m) =>
        val afterDefspec = source.substring(m.start)
        val sorryRe = """(?s)(by\s+)sorry""".r
        sorryRe.findFirstMatchIn(afterDefspec) match
          case None => source
          case Some(sm) =>
            val absStart = m.start + sm.start(0)
            val absEnd   = m.start + sm.end(0)
            source.substring(0, absStart) +
              "by " + renderTactic(tac) +
              source.substring(absEnd)

  // ---- Tactic → source text rendering ----

  def renderTactic(t: STactic): String = t match
    case STactic.STrivial    => "trivial"
    case STactic.STriv       => "trivial"
    case STactic.SRfl        => "trivial"
    case STactic.SAssumption => "assumption"
    case STactic.SContradiction => "contradiction"
    case STactic.STauto      => "tauto"
    case STactic.SDecide     => "decide"
    case STactic.SSimplify(lemmas) =>
      if lemmas.isEmpty then "simplify" else s"simplify [${lemmas.mkString(", ")}]"
    case STactic.SSimp(lemmas) =>
      if lemmas.isEmpty then "simplify" else s"simplify [${lemmas.mkString(", ")}]"
    case STactic.SInduction(varName, cases, generalizing) =>
      val genSuffix = if generalizing.isEmpty then "" else s" generalizing ${generalizing.mkString(" ")}"
      val caseLines = cases.map { c =>
        val bindings = if c.extraBindings.isEmpty then "" else " " + c.extraBindings.mkString(" ")
        s"    case ${c.ctorName}$bindings => ${renderTactic(c.tactic)}"
      }
      s"induction $varName$genSuffix {\n${caseLines.mkString("\n")}\n  }"
    case STactic.SCases(varName, cases) =>
      val caseLines = cases.map { c =>
        val bindings = if c.extraBindings.isEmpty then "" else " " + c.extraBindings.mkString(" ")
        s"    case ${c.ctorName}$bindings => ${renderTactic(c.tactic)}"
      }
      s"cases $varName {\n${caseLines.mkString("\n")}\n  }"
    case STactic.SSeq(ts) => ts.map(renderTactic).mkString("; ")
    case STactic.SSorry    => "sorry"
    case STactic.SSkip     => "skip"
    case other             => other.toString

  // ---- sorry detection ----

  private def containsSorry(proof: SProof): Boolean = proof match
    case SProof.SBy(tac) => tacticHasSorry(tac)
    case SProof.STerm(_) => false

  private def tacticHasSorry(t: STactic): Boolean = t match
    case STactic.SSorry        => true
    case STactic.SSeq(ts)      => ts.exists(tacticHasSorry)
    case STactic.SInduction(_, cases, _) => cases.exists(c => tacticHasSorry(c.tactic))
    case STactic.SCases(_, cases)     => cases.exists(c => tacticHasSorry(c.tactic))
    case STactic.SFirst(ts)    => ts.exists(tacticHasSorry)
    case STactic.SRepeat(t)    => tacticHasSorry(t)
    case STactic.STry(t)       => tacticHasSorry(t)
    case STactic.SAllGoals(t)  => tacticHasSorry(t)
    case STactic.SHave(_, _, p, cont) => containsSorry(p) || tacticHasSorry(cont)
    case _                     => false
