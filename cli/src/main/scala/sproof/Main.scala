package sproof

import scala.io.{Source, StdIn}
import scala.collection.mutable
import scala.util.hashing.MurmurHash3
import sproof.core.{GlobalEnv, Context}
import sproof.syntax.{Parser, Elaborator, ElabResult, SDecl}
import sproof.tactic.TacticError
import sproof.extract.Extractor

/** Entry point for the sproof CLI.
  *
  * Supports two sub-commands:
  *   - `sproof check <file.sproof>` — parse, elaborate, type-check, and prove
  *   - `sproof repl`                — interactive REPL
  */
object Main:
  private final case class CheckFailure(
    phase: String,
    error: String,
    diagnostics: List[Diagnostic],
  )

  private[sproof] final case class CheckCliOptions(
    filePath: String,
    json: Boolean,
    failOnSorry: Boolean,
  )

  private[sproof] final case class SorryDiagnostic(
    code: String,
    defspec: String,
    transitive: Boolean,
    occurrences: Option[Int],
    dependsOn: List[String],
    message: String,
  )

  final case class IncrementalStats(
    parseCacheHit: Boolean,
    elabCacheHit:  Boolean,
    proofCacheHit: Boolean,
  )

  private final case class ParseCacheEntry(
    sourceHash: String,
    declHash: String,
    decls: List[SDecl],
  )

  private final case class ElabCacheEntry(
    declHash: String,
    programHash: String,
    result: ElabResult,
  )

  private final case class ProofCacheEntry(
    programHash: String,
    outcome: Either[String, (GlobalEnv, List[String], Int)],
  )

  // File-scoped incremental caches (used by repeated checks in the same JVM process).
  private val parseCache = mutable.Map.empty[String, ParseCacheEntry]
  private val elabCache  = mutable.Map.empty[String, ElabCacheEntry]
  private val proofCache = mutable.Map.empty[String, ProofCacheEntry]

  def resetIncrementalCache(): Unit =
    parseCache.clear()
    elabCache.clear()
    proofCache.clear()

  def main(args: Array[String]): Unit =
    args.toList match
      case "check" :: tail =>
        parseCheckOptions(tail) match
          case Left(err) =>
            System.err.println(s"Error: $err")
            printUsage()
            sys.exit(1)
          case Right(opts) =>
            if opts.json then runCheckJson(opts.filePath, opts.failOnSorry)
            else runCheck(opts.filePath, opts.failOnSorry)

      case "agent" :: filePath :: Nil =>
        runAgent(filePath)

      case "extract" :: filePath :: Nil =>
        runExtract(filePath)

      case "repl" :: Nil =>
        given GlobalEnv = GlobalEnv.empty
        runRepl()

      case _ =>
        printUsage()
        sys.exit(1)

  private[sproof] def parseCheckOptions(args: List[String]): Either[String, CheckCliOptions] =
    def loop(
      rest: List[String],
      json: Boolean,
      failOnSorry: Boolean,
      file: Option[String],
    ): Either[String, CheckCliOptions] =
      rest match
        case Nil =>
          file match
            case Some(path) => Right(CheckCliOptions(path, json, failOnSorry))
            case None       => Left("Missing <file.sproof> for check")
        case "--json" :: tail =>
          loop(tail, json = true, failOnSorry, file)
        case "--fail-on-sorry" :: tail =>
          loop(tail, json, failOnSorry = true, file)
        case opt :: _ if opt.startsWith("--") =>
          Left(s"Unknown option for check: $opt")
        case path :: tail =>
          file match
            case Some(_) => Left("Too many file arguments for check")
            case None    => loop(tail, json, failOnSorry, Some(path))
    loop(args, json = false, failOnSorry = false, file = None)

  // ---- Agent command ----

  private def runAgent(filePath: String): Unit =
    import sproof.agent.{FileRepairer, TacticGen}
    val source =
      try scala.io.Source.fromFile(filePath).mkString
      catch
        case e: java.io.FileNotFoundException =>
          System.err.println(s"Error: File not found: $filePath")
          sys.exit(1)
        case e: Exception =>
          System.err.println(s"Error reading file: ${e.getMessage}")
          sys.exit(1)

    println(s"sproof agent: analysing $filePath ...")
    val results = FileRepairer.tryRepair(source, filePath)

    if results.isEmpty then
      println("  No sorry proofs found.")
    else
      var allSolved = true
      results.foreach { r =>
        r.found match
          case Some(tac) =>
            println(s"  [OK] '${r.defspecName}' → by ${FileRepairer.renderTactic(tac)}")
          case None =>
            println(s"  [??] '${r.defspecName}' — no proof found (sorry retained)")
            allSolved = false
      }
      if allSolved then
        val repaired  = FileRepairer.repair(source, filePath)
        val outPath   = filePath.replaceAll("\\.sproof$", ".repaired.sproof")
        java.nio.file.Files.writeString(java.nio.file.Paths.get(outPath), repaired)
        println(s"  Repaired file written to: $outPath")
      else
        println("  Partial repair — some proofs remain as sorry.")
        val repaired = FileRepairer.repair(source, filePath)
        val outPath  = filePath.replaceAll("\\.sproof$", ".repaired.sproof")
        java.nio.file.Files.writeString(java.nio.file.Paths.get(outPath), repaired)
        println(s"  Partial repaired file written to: $outPath")

  // ---- Check --json command ----

  private def runCheckJson(filePath: String, failOnSorry: Boolean): Unit =
    val source =
      try Source.fromFile(filePath).mkString
      catch
        case e: java.io.FileNotFoundException =>
          println(s"""{"ok":false,"error":"File not found: $filePath","phase":"io"}""")
          sys.exit(1)
        case e: Exception =>
          println(s"""{"ok":false,"error":"${e.getMessage}","phase":"io"}""")
          sys.exit(1)
    val json = processSourceJson(source, filePath, failOnSorry = failOnSorry)
    println(json)
    if failOnSorry && json.contains("\"phase\":\"policy\"") then
      sys.exit(1)

  // ---- Extract command ----

  private def runExtract(filePath: String): Unit =
    val source =
      try Source.fromFile(filePath).mkString
      catch
        case e: java.io.FileNotFoundException =>
          System.err.println(s"Error: File not found: $filePath")
          sys.exit(1)
        case e: Exception =>
          System.err.println(s"Error reading file: ${e.getMessage}")
          sys.exit(1)

    processSource(source, filePath) match
      case Left(err) =>
        System.err.println(s"Error: $err")
        sys.exit(1)
      case Right((env, _)) =>
        println(Extractor.extractProgram(env))

  // ---- Check command ----

  private def runCheck(filePath: String, failOnSorry: Boolean): Unit =
    val source =
      try Source.fromFile(filePath).mkString
      catch
        case e: java.io.FileNotFoundException =>
          System.err.println(s"Error: File not found: $filePath")
          sys.exit(1)
        case e: Exception =>
          System.err.println(s"Error reading file: ${e.getMessage}")
          sys.exit(1)

    processSourceDetailed(source, filePath) match
      case Left(failure) =>
        System.err.println(s"Error: ${failure.error}")
        if failure.diagnostics.nonEmpty then
          System.err.println(Diagnostics.formatForCli(failure.diagnostics))
        sys.exit(1)
      case Right((env, specCount, warnings)) =>
        warnings.foreach(w => System.err.println(w))
        if failOnSorry && warnings.nonEmpty then
          System.err.println("Error: sorry policy violation (use of sorry is disallowed in this mode)")
          sys.exit(1)
        val indCount  = env.inductives.size
        val defCount  = env.defs.size
        println(s"OK: $filePath — $indCount inductive(s), $defCount definition(s), $specCount defspec(s)")

  /** Parse, elaborate, and check a source string.
    *
    * @param source   the source text
    * @param fileName file name for error reporting
    * @return the final GlobalEnv on success, or an error message
    */
  def processSource(source: String, fileName: String = "<input>"): Either[String, (GlobalEnv, Int)] =
    processSourceWithIncrementalStats(source, fileName)
      .map { case (env, defspecCount, _, _) => (env, defspecCount) }

  /** processSource with sorry warnings.
    *
    * @return (GlobalEnv, defspecCount, warnings) on success
    */
  def processSourceWithWarnings(
    source:   String,
    fileName: String = "<input>",
  ): Either[String, (GlobalEnv, Int, List[String])] =
    processSourceWithIncrementalStats(source, fileName)
      .map { case (env, defspecCount, warnings, _) => (env, defspecCount, warnings) }

  /** processSource with incremental cache stats.
    *
    * Used for tests and tooling that want to observe cache hit behavior.
    */
  def processSourceWithIncrementalStats(
    source:   String,
    fileName: String = "<input>",
  ): Either[String, (GlobalEnv, Int, List[String], IncrementalStats)] =
    val safeFileKey = if fileName.nonEmpty then fileName else "<input>"
    val sourceHash = hashString(source)
    var parseHit = false
    var elabHit = false
    var proofHit = false

    val (decls, declHash) =
      parseCache.get(safeFileKey) match
        case Some(entry) if entry.sourceHash == sourceHash =>
          parseHit = true
          (entry.decls, entry.declHash)
        case _ =>
          Parser.parseProgram(source) match
            case Left(parseErr) =>
              parseCache.remove(safeFileKey)
              elabCache.remove(safeFileKey)
              proofCache.remove(safeFileKey)
              return Left(s"Parse error in $safeFileKey:\n$parseErr")
            case Right(parsedDecls) =>
              val newDeclHash = declHashFor(parsedDecls)
              val prevDeclHash = parseCache.get(safeFileKey).map(_.declHash)
              parseCache.update(safeFileKey, ParseCacheEntry(sourceHash, newDeclHash, parsedDecls))
              if prevDeclHash.forall(_ != newDeclHash) then
                elabCache.remove(safeFileKey)
                proofCache.remove(safeFileKey)
              (parsedDecls, newDeclHash)

    val (elabResult, programHash) =
      elabCache.get(safeFileKey) match
        case Some(entry) if entry.declHash == declHash =>
          elabHit = true
          (entry.result, entry.programHash)
        case _ =>
          Elaborator.elaborate(decls) match
            case Left(elabErr) =>
              elabCache.remove(safeFileKey)
              proofCache.remove(safeFileKey)
              return Left(s"Elaboration error in $safeFileKey: ${elabErr.message}")
            case Right(result) =>
              val newProgramHash = programHashFor(result)
              val prevProgramHash = elabCache.get(safeFileKey).map(_.programHash)
              elabCache.update(safeFileKey, ElabCacheEntry(declHash, newProgramHash, result))
              if prevProgramHash.forall(_ != newProgramHash) then
                proofCache.remove(safeFileKey)
              (result, newProgramHash)

    val proofOutcome =
      proofCache.get(safeFileKey) match
        case Some(entry) if entry.programHash == programHash =>
          proofHit = true
          entry.outcome
        case _ =>
          given GlobalEnv = elabResult.env
          val computed = Checker
            .checkAllWithWarnings(elabResult)
            .map { case (env, warnings) =>
              (env, warnings, elabResult.defspecs.size)
            }
          proofCache.update(safeFileKey, ProofCacheEntry(programHash, computed))
          computed

    proofOutcome.map { case (env, warnings, defspecCount) =>
      val stats = IncrementalStats(parseHit, elabHit, proofHit)
      (env, defspecCount, warnings, stats)
    }

  /** processSource with #check results.
    *
    * @return (GlobalEnv, defspecCount, checkResults) on success
    *         checkResults: List[(exprStr, typeStr)]
    */
  def processSourceWithChecks(
    source:   String,
    fileName: String = "<input>",
  ): Either[String, (GlobalEnv, Int, List[(String, String)])] =
    import sproof.checker.Bidirectional
    for
      decls  <- Parser.parseProgram(source).left.map(e => s"Parse error in $fileName:\n$e")
      result <- Elaborator.elaborate(decls).left.map(e => s"Elaboration error in $fileName: ${e.message}")
      env    <- Checker.checkAll(result)
    yield
      given GlobalEnv = env
      val checkResults = result.checks.map { sexpr =>
        val exprStr = sexpr.toString
        Elaborator.elabExprPublic(sexpr, env, Nil) match
          case Left(err) =>
            (exprStr, s"error: ${err.message}")
          case Right(term) =>
            Bidirectional.infer(Context.empty, term) match
              case Left(err)  => (exprStr, s"error: ${err.getMessage}")
              case Right(tpe) => (term.show, tpe.show)
      }
      (env, result.defspecs.size, checkResults)

  /** processSource returning stable JSON schema v2.
    *
    * v2 shape always includes:
    * - schemaVersion (number)
    * - ok (boolean)
    * - phase (parse|elab|proof|policy|check)
    * - file (string)
    * - result ({inductives, defs, defspecs} | null)
    * - warnings (array)
    * - sorryDiagnostics (array)
    * - diagnostics (array)
    * - checks (array)
    * - error (string | null)
    */
  def processSourceJson(
    source: String,
    fileName: String = "<input>",
    failOnSorry: Boolean = false,
  ): String =
    import sproof.checker.Bidirectional

    def esc(s: String): String =
      s.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n")

    def warningJson(message: String): String =
      s"""{"severity":"warning","code":"sorry","message":"${esc(message)}"}"""

    def toJsonSorryDiagnostic(d: SorryDiagnostic): String =
      val occJson = d.occurrences match
        case Some(n) => n.toString
        case None    => "null"
      val depsJson = d.dependsOn.map(dep => s""""${esc(dep)}"""").mkString("[", ",", "]")
      s"""{"code":"${esc(d.code)}","defspec":"${esc(d.defspec)}","transitive":${d.transitive},"occurrences":$occJson,"dependsOn":$depsJson,"message":"${esc(d.message)}"}"""

    def diagnosticToJson(d: Diagnostic): String =
      val rangeJson = d.range match
        case Some(r) =>
          s"""{"start":{"line":${r.start.line},"column":${r.start.column}},"end":{"line":${r.end.line},"column":${r.end.column}}}"""
        case None => "null"
      val expectedJson = d.expected.map(v => s""""${esc(v)}"""").getOrElse("null")
      val actualJson = d.actual.map(v => s""""${esc(v)}"""").getOrElse("null")
      val hintJson = d.hint.map(v => s""""${esc(v)}"""").getOrElse("null")
      s"""{"phase":"${esc(d.phase)}","code":"${esc(d.code)}","message":"${esc(d.message)}","range":$rangeJson,"expected":$expectedJson,"actual":$actualJson,"hint":$hintJson}"""

    def failJson(phase: String, message: String, diagnostics: List[Diagnostic]): String =
      val diagJson = diagnostics.map(diagnosticToJson).mkString("[", ",", "]")
      s"""{"schemaVersion":2,"ok":false,"phase":"${esc(phase)}","file":"${esc(fileName)}","result":null,"warnings":[],"sorryDiagnostics":[],"diagnostics":$diagJson,"checks":[],"error":"${esc(message)}"}"""

    def policyDiagnosticJson(message: String): String =
      s"""{"phase":"policy","code":"policy_error","message":"${esc(message)}","range":null,"expected":null,"actual":null,"hint":null}"""

    Parser.parseProgram(source) match
      case Left(parseErr) =>
        val raw = parseErr.toString
        failJson("parse", raw, Diagnostics.fromFailure(source, "parse", raw))
      case Right(decls) =>
        Elaborator.elaborate(decls) match
          case Left(elabErr) =>
            val raw = elabErr.message
            failJson("elab", raw, Diagnostics.fromFailure(source, "elab", raw))
          case Right(result) =>
            given GlobalEnv = result.env
            Checker.checkAllWithWarnings(result) match
              case Left(proofErr) =>
                failJson("proof", proofErr, Diagnostics.fromFailure(source, "proof", proofErr))
              case Right((env, warnings)) =>
                val indCount   = env.inductives.size
                val defCount   = env.defs.size
                val specCount  = result.defspecs.size
                val warnJson   = warnings.map(warningJson).mkString("[", ",", "]")
                val sorryDiagnostics = warnings.map(parseSorryDiagnostic)
                val sorryDiagJson = sorryDiagnostics.map(toJsonSorryDiagnostic).mkString("[", ",", "]")
                val checkJson  = result.checks.map { sexpr =>
                  val exprStr = sexpr.toString
                  Elaborator.elabExprPublic(sexpr, env, Nil) match
                    case Left(err) =>
                      s"""{"expr":"${esc(exprStr)}","ok":false,"type":null,"error":"${esc(err.message)}"}"""
                    case Right(term) =>
                      Bidirectional.infer(Context.empty, term) match
                        case Left(err) =>
                          s"""{"expr":"${esc(term.show)}","ok":false,"type":null,"error":"${esc(err.getMessage)}"}"""
                        case Right(tpe) =>
                          s"""{"expr":"${esc(term.show)}","ok":true,"type":"${esc(tpe.show)}","error":null}"""
                }.mkString("[", ",", "]")
                if failOnSorry && warnings.nonEmpty then
                  val msg = "sorry policy violation (use of sorry is disallowed in this mode)"
                  val policyDiagJson = policyDiagnosticJson(msg)
                  s"""{"schemaVersion":2,"ok":false,"phase":"policy","file":"${esc(fileName)}","result":{"inductives":$indCount,"defs":$defCount,"defspecs":$specCount},"warnings":$warnJson,"sorryDiagnostics":$sorryDiagJson,"diagnostics":[$policyDiagJson],"checks":$checkJson,"error":"$msg"}"""
                else
                  s"""{"schemaVersion":2,"ok":true,"phase":"check","file":"${esc(fileName)}","result":{"inductives":$indCount,"defs":$defCount,"defspecs":$specCount},"warnings":$warnJson,"sorryDiagnostics":$sorryDiagJson,"diagnostics":[],"checks":$checkJson,"error":null}"""

  private def processSourceDetailed(
    source: String,
    fileName: String,
  ): Either[CheckFailure, (GlobalEnv, Int, List[String])] =
    Parser.parseProgram(source) match
      case Left(parseErr) =>
        val raw = parseErr.toString
        val message = s"Parse error in $fileName:\n$raw"
        Left(CheckFailure("parse", message, Diagnostics.fromFailure(source, "parse", raw)))
      case Right(decls) =>
        Elaborator.elaborate(decls) match
          case Left(elabErr) =>
            val message = s"Elaboration error in $fileName: ${elabErr.message}"
            Left(CheckFailure("elab", message, Diagnostics.fromFailure(source, "elab", elabErr.message)))
          case Right(result) =>
            given GlobalEnv = result.env
            Checker.checkAllWithWarnings(result) match
              case Left(proofErr) =>
                val message = proofErr
                Left(CheckFailure("proof", message, Diagnostics.fromFailure(source, "proof", proofErr)))
              case Right((env, warnings)) =>
                Right((env, result.defspecs.size, warnings))

  private def hashString(value: String): String =
    // Keep hashing portable across JVM and Scala Native (avoid java.security APIs).
    val h1 = MurmurHash3.stringHash(value, 0x9e3779b9)
    val h2 = MurmurHash3.stringHash(value.reverse, 0x85ebca6b)
    f"${h1.toLong & 0xffffffffL}%08x${h2.toLong & 0xffffffffL}%08x"

  private def declHashFor(decls: List[SDecl]): String =
    val payload = decls.map(_.toString).mkString("\u241F")
    hashString(payload)

  private def programHashFor(result: ElabResult): String =
    val inductivePart = result.env.inductives.toList.sortBy(_._1).map { case (name, ind) =>
      val params = ind.params.map(p => s"${p.name}:${p.tpe.show}").mkString(",")
      val indices = ind.indices.map(i => s"${i.name}:${i.tpe.show}").mkString(",")
      val ctors = ind.ctors.map(c => s"${c.name}:${c.argTpes.map(_.show).mkString("->")}").mkString(";")
      s"$name|$params|$indices|$ctors"
    }.mkString("||")
    val defsPart = result.defs.toList.sortBy(_._1).map { case (name, term) =>
      s"$name=${term.show}"
    }.mkString("||")
    val defspecPart = result.defspecs.toList.sortBy(_._1).map { case (name, (params, prop, proof)) =>
      val paramPart = params.map(p => s"${p._1}:${p._2.show}").mkString(",")
      s"$name|$paramPart|${prop.show}|${proof.toString}"
    }.mkString("||")
    val checksPart = result.checks.map(_.toString).mkString("||")
    hashString(s"$inductivePart###$defsPart###$defspecPart###$checksPart")

  // ---- REPL ----

  private def printHelp(): Unit =
    println(
      """|sproof REPL commands:
         |  :help, help   — show this help
         |  :quit, quit   — exit the REPL
         |  exit          — exit the REPL
         |
         |Enter declarations:
         |  inductive Nat { case zero: Nat  case succ(n: Nat): Nat }
         |  def plus(n: Nat, m: Nat): Nat { match n { ... } }
         |  defspec refl(n: Nat): n = n { by trivial }
         |
         |Multi-line input is supported: keep typing when a '{' is open.
         |""".stripMargin
    )

  def runRepl()(using env: GlobalEnv): Unit =
    var globalEnv = env
    println("sproof REPL — type :help for help, :quit to exit")

    var running = true
    while running do
      val input = readMultiLine("sproof> ")
      input.trim match
        case ":quit" | ":exit" | "quit" | "exit" =>
          running = false
          println("Goodbye.")

        case ":help" | "help" =>
          printHelp()

        case "" =>
          () // ignore blank input

        case decl =>
          processDeclaration(decl, globalEnv) match
            case Left(err) =>
              println(s"Error: $err")
            case Right((newEnv, msg)) =>
              globalEnv = newEnv
              println(msg)

  /** Process a single declaration string in the REPL context.
    *
    * @param input     the raw source text for one or more declarations
    * @param globalEnv the current global environment
    * @return a new GlobalEnv and a summary message, or an error string
    */
  def processDeclaration(input: String, globalEnv: GlobalEnv): Either[String, (GlobalEnv, String)] =
    given GlobalEnv = globalEnv
    for
      decls  <- Parser.parseProgram(input).left.map(e => s"Parse error:\n$e")
      result <- Elaborator.elaborate(decls).left.map(e => s"Elaboration error: ${e.message}")
      newEnv <- Checker.checkAll(result)
    yield
      val indNames   = newEnv.inductives.keys.toList.sorted
      val defNames   = newEnv.defs.keys.toList.sorted
      val specNames  = result.defspecs.keys.toList.sorted
      val summary    = buildSummary(indNames, defNames, specNames)
      (newEnv, summary)

  /** Summarise what is now defined in the environment. */
  private def buildSummary(indNames: List[String], defNames: List[String], specNames: List[String] = Nil): String =
    val parts = List(
      if indNames.nonEmpty  then Some(s"inductive: ${indNames.mkString(", ")}") else None,
      if defNames.nonEmpty  then Some(s"defined: ${defNames.mkString(", ")}") else None,
      if specNames.nonEmpty then Some(s"proved: ${specNames.mkString(", ")}") else None,
    ).flatten
    if parts.isEmpty then "OK (no definitions)"
    else "OK — " + parts.mkString("; ")

  // ---- Multi-line input reader ----

  /** Read input from stdin, accumulating lines until all open braces are closed.
    *
    * A line containing only whitespace after the brace balance reaches zero
    * terminates the input.
    *
    * @param prompt the primary prompt to show (continuation uses "     | ")
    * @return the accumulated input string
    */
  def readMultiLine(prompt: String): String =
    val sb       = new StringBuilder
    var depth    = 0
    var first    = true
    var continue = true

    while continue do
      val line = StdIn.readLine(if first then prompt else "     | ")
      if line == null then
        // EOF (Ctrl-D)
        continue = false
      else
        // Update brace depth
        depth += line.count(_ == '{') - line.count(_ == '}')
        sb.append(line).append('\n')
        first = false
        // Stop when braces are balanced (or went negative) and line is not blank
        if depth <= 0 && line.trim.nonEmpty then
          continue = false
        else if depth <= 0 && line.trim.isEmpty && !first then
          continue = false

    sb.toString

  private def printUsage(): Unit =
    println(
      """|Usage:
         |  sproof check <file.sproof>                          Parse, elaborate, and verify a sproof file.
         |  sproof check --json <file.sproof>                   Same, but output JSON (for tooling/agents).
         |  sproof check --fail-on-sorry <file.sproof>          Treat any sorry warning as an error (exit 1).
         |  sproof check --json --fail-on-sorry <file.sproof>   JSON output + fail policy.
         |  sproof extract <file.sproof>        Extract Scala 3 code from a verified sproof file.
         |  sproof agent <file.sproof>          Auto-repair sorry proofs using the proof agent.
         |  sproof repl                         Start the interactive REPL.
         |""".stripMargin
    )

  private val directSorryPattern =
    """warning: '([^']+)' uses sorry \((\d+) occurrence\(s\)\) — proof is unsound""".r
  private val transitiveSorryPattern =
    """warning: '([^']+)' depends on sorry-tainted defspec\(s\): (.+) — proof is transitively unsound""".r

  private[sproof] def parseSorryDiagnostic(warning: String): SorryDiagnostic =
    warning match
      case directSorryPattern(name, count) =>
        SorryDiagnostic(
          code = "sorry.direct",
          defspec = name,
          transitive = false,
          occurrences = Some(count.toInt),
          dependsOn = Nil,
          message = warning,
        )
      case transitiveSorryPattern(name, depsRaw) =>
        val deps = depsRaw.split(",").map(_.trim).filter(_.nonEmpty).toList
        SorryDiagnostic(
          code = "sorry.transitive",
          defspec = name,
          transitive = true,
          occurrences = None,
          dependsOn = deps,
          message = warning,
        )
      case _ =>
        SorryDiagnostic(
          code = "sorry.unknown",
          defspec = "<unknown>",
          transitive = true,
          occurrences = None,
          dependsOn = Nil,
          message = warning,
        )

end Main
