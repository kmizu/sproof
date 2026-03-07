package sproof

import scala.util.matching.Regex

final case class DiagnosticPosition(line: Int, column: Int)
final case class DiagnosticRange(start: DiagnosticPosition, end: DiagnosticPosition)

/** Structured diagnostic payload shared by CLI text and JSON output. */
final case class Diagnostic(
  phase:            String,
  code:             String,
  message:          String,
  range:            Option[DiagnosticRange]  = None,
  expected:         Option[String]           = None,
  actual:           Option[String]           = None,
  hint:             Option[String]           = None,
  /** Human-readable rendering of the failing goal (e.g. "plus(n, zero) = n"). */
  goalTarget:       Option[String]           = None,
  /** Ordered list of tactics most likely to close the goal, from most to least likely. */
  tacticSuggestions: Option[List[String]]   = None,
)

object Diagnostics:
  private val ParseLocPattern = raw"\(line\s+(\d+),\s*column\s+(\d+)\)".r
  private val DefspecFailurePattern = raw"(?:Kernel rejected proof of|Proof of) '([^']+)'".r

  def fromFailure(source: String, phase: String, error: String): List[Diagnostic] =
    List(buildDiagnostic(source, phase, error))

  def formatForCli(diagnostics: List[Diagnostic]): String =
    diagnostics.map(formatOne).mkString("\n")

  private def formatOne(diag: Diagnostic): String =
    val b = new StringBuilder
    b.append(s"[${diag.phase}:${diag.code}] ${diag.message}")
    diag.range.foreach { r =>
      b.append(s"\n  range: ${r.start.line}:${r.start.column}-${r.end.line}:${r.end.column}")
    }
    diag.expected.foreach(e => b.append(s"\n  expected: $e"))
    diag.actual.foreach(a => b.append(s"\n  actual:   $a"))
    diag.hint.foreach(h => b.append(s"\n  hint:     $h"))
    b.toString

  private def buildDiagnostic(source: String, phase: String, error: String): Diagnostic =
    phase match
      case "parse" => parseDiagnostic(error)
      case _ =>
        val (expected, actual, term) = extractExpectedActualTerm(error)
        val code = codeFor(phase, error, expected, actual)
        val range = rangeFor(source, error, phase, term)
        val message = code match
          case "type_mismatch" => "Type mismatch"
          case "unknown_variable" => "Unknown variable"
          case "unknown_type" => "Unknown type"
          case "proof_error" => "Proof error"
          case _ => firstLine(error)
        val goalTarget = extractGoalTarget(error)
        val suggestions = suggestTactics(code, error, goalTarget)
        Diagnostic(
          phase = phase,
          code = code,
          message = message,
          range = range,
          expected = expected,
          actual = actual,
          hint = hintFor(code, expected, actual),
          goalTarget = goalTarget,
          tacticSuggestions = suggestions,
        )

  private def parseDiagnostic(error: String): Diagnostic =
    val range = ParseLocPattern
      .findFirstMatchIn(error)
      .map { m =>
        val line = m.group(1).toInt
        val col = m.group(2).toInt
        val caretWidth = """(?m)^\s*(\^+)\s*$""".r
          .findFirstMatchIn(error)
          .map(_.group(1).length)
          .getOrElse(1)
        DiagnosticRange(
          start = DiagnosticPosition(line, col),
          end = DiagnosticPosition(line, col + math.max(1, caretWidth) - 1),
        )
      }

    val hint = """(?m)^\s*expected (.+)$""".r
      .findFirstMatchIn(error)
      .map(m => s"Expected ${m.group(1)}.")
      .orElse(Some("Check syntax near the highlighted range."))

    Diagnostic(
      phase = "parse",
      code = "parse_error",
      message = "Parse error",
      range = range,
      hint = hint,
    )

  private def codeFor(
    phase: String,
    error: String,
    expected: Option[String],
    actual: Option[String],
  ): String =
    val lowered = error.toLowerCase
    if expected.nonEmpty && actual.nonEmpty then "type_mismatch"
    else if lowered.contains("unknown variable") then "unknown_variable"
    else if lowered.contains("unknown type") then "unknown_type"
    else if phase == "proof" then "proof_error"
    else "error"

  private def hintFor(
    code: String,
    expected: Option[String],
    actual: Option[String],
  ): Option[String] =
    code match
      case "type_mismatch" =>
        Some(s"Align the term type with the goal (${expected.getOrElse("?")} vs ${actual.getOrElse("?")}).")
      case "unknown_variable" =>
        Some("Check spelling or introduce the variable in scope (e.g. parameters/assume).")
      case "unknown_type" =>
        Some("Check the type name and confirm it is defined or imported.")
      case "proof_error" =>
        Some("Inspect proof-state and apply a tactic that matches the current goal.")
      case _ =>
        None

  private def firstLine(error: String): String =
    error.linesIterator.nextOption().getOrElse("Error")

  private def extractExpectedActualTerm(error: String): (Option[String], Option[String], Option[String]) =
    val expected = extractLabel(error, "expected:")
    val actual = extractLabel(error, "actual:")
    val term = extractLabel(error, "term:")
    (expected, actual, term)

  private def extractLabel(error: String, label: String): Option[String] =
    error.linesIterator
      .map(_.trim)
      .find(_.startsWith(label))
      .map(_.stripPrefix(label).trim)
      .filter(_.nonEmpty)

  private def rangeFor(
    source: String,
    error: String,
    phase: String,
    term: Option[String],
  ): Option[DiagnosticRange] =
    defspecRange(source, error)
      .orElse(term.flatMap(t => findNeedleRange(source, t)))
      .orElse(fallbackRange(source))

  private def defspecRange(source: String, error: String): Option[DiagnosticRange] =
    val nameOpt = DefspecFailurePattern.findFirstMatchIn(error).map(_.group(1))
    nameOpt.flatMap { name =>
      val pattern = ("(?m)^\\s*defspec\\s+" + Regex.quote(name) + "\\b").r
      pattern.findFirstMatchIn(source).map { m =>
        val start = m.start
        val end = source.indexOf('\n', start) match
          case -1 => source.length
          case i  => i
        rangeFromOffsets(source, start, math.max(start + 1, end))
      }
    }

  private def findNeedleRange(source: String, needle: String): Option[DiagnosticRange] =
    val clean = needle.trim
    if clean.isEmpty || clean.startsWith("#") then None
    else
      val idx = source.indexOf(clean)
      Option.when(idx >= 0)(rangeFromOffsets(source, idx, idx + clean.length))

  private def fallbackRange(source: String): Option[DiagnosticRange] =
    source.indexWhere(!_.isWhitespace) match
      case -1 => None
      case i  => Some(rangeFromOffsets(source, i, i + 1))

  private def rangeFromOffsets(source: String, startOffset: Int, endOffsetExclusive: Int): DiagnosticRange =
    DiagnosticRange(
      start = positionFromOffset(source, startOffset),
      end = positionFromOffset(source, math.max(startOffset + 1, endOffsetExclusive)),
    )

  private def positionFromOffset(source: String, offset: Int): DiagnosticPosition =
    val safe = math.max(0, math.min(offset, source.length))
    var line = 1
    var col = 1
    var i = 0
    while i < safe do
      if source.charAt(i) == '\n' then
        line += 1
        col = 1
      else
        col += 1
      i += 1
    DiagnosticPosition(line, col)

  /** Extract a human-readable goal target from a proof error message.
   *
   *  Handles "trivial: not definitionally equal: LHS ≢ RHS" pattern.
   */
  private def extractGoalTarget(error: String): Option[String] =
    raw"not definitionally equal:\s*(.+?)\s*≢\s*(.+?)(?:\s*\n|$$)".r
      .findFirstMatchIn(error)
      .map(m => s"${m.group(1).trim} = ${m.group(2).trim}")

  /** Suggest tactics based on the error code and message content. */
  private def suggestTactics(
    code: String, error: String, goalTarget: Option[String],
  ): Option[List[String]] =
    if code != "proof_error" then None
    else
      val lowered = error.toLowerCase
      val suggestions =
        if lowered.contains("not definitionally equal") then
          // Eq goal where sides differ: induction is most likely needed
          List("induction", "simplify [ih]", "assumption", "sorry")
        else if lowered.contains("not an equality") then
          // Goal is not Eq: likely a Pi, introduce the binder
          List("assume", "sorry")
        else if lowered.contains("not found in context") then
          List("assumption", "sorry")
        else
          List("trivial", "simplify", "induction", "sorry")
      Some(suggestions)

