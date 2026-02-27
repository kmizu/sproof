package sproof

import scala.io.{Source, StdIn}
import sproof.core.{GlobalEnv, Context}
import sproof.syntax.{Parser, Elaborator, ElabResult}
import sproof.tactic.TacticError
import sproof.extract.Extractor

/** Entry point for the sproof CLI.
  *
  * Supports two sub-commands:
  *   - `sproof check <file.sproof>` — parse, elaborate, type-check, and prove
  *   - `sproof repl`                — interactive REPL
  */
object Main:

  def main(args: Array[String]): Unit =
    args.toList match
      case "check" :: filePath :: Nil =>
        runCheck(filePath)

      case "check" :: "--json" :: filePath :: Nil =>
        runCheckJson(filePath)

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

  private def runCheckJson(filePath: String): Unit =
    val source =
      try Source.fromFile(filePath).mkString
      catch
        case e: java.io.FileNotFoundException =>
          println(s"""{"ok":false,"error":"File not found: $filePath","phase":"io"}""")
          sys.exit(1)
        case e: Exception =>
          println(s"""{"ok":false,"error":"${e.getMessage}","phase":"io"}""")
          sys.exit(1)
    println(processSourceJson(source, filePath))

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

  private def runCheck(filePath: String): Unit =
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
      case Right((env, specCount)) =>
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
    for
      decls  <- Parser.parseProgram(source).left.map(e => s"Parse error in $fileName:\n$e")
      result <- Elaborator.elaborate(decls).left.map(e => s"Elaboration error in $fileName: ${e.message}")
      env    <- Checker.checkAll(result)
    yield (env, result.defspecs.size)

  /** processSource with sorry warnings.
    *
    * @return (GlobalEnv, defspecCount, warnings) on success
    */
  def processSourceWithWarnings(
    source:   String,
    fileName: String = "<input>",
  ): Either[String, (GlobalEnv, Int, List[String])] =
    for
      decls          <- Parser.parseProgram(source).left.map(e => s"Parse error in $fileName:\n$e")
      result         <- Elaborator.elaborate(decls).left.map(e => s"Elaboration error in $fileName: ${e.message}")
      envAndWarnings <- Checker.checkAllWithWarnings(result)
      (env, warnings) = envAndWarnings
    yield (env, result.defspecs.size, warnings)

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

  /** processSource returning JSON string (Feature 2).
    *
    * Output format:
    *   success: {"ok":true,"inductives":N,"defs":N,"defspecs":N}
    *   failure: {"ok":false,"error":"...","phase":"parse|elab|proof"}
    */
  def processSourceJson(source: String, fileName: String = "<input>"): String =
    def esc(s: String): String =
      s.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n")

    Parser.parseProgram(source) match
      case Left(parseErr) =>
        s"""{"ok":false,"error":"${esc(parseErr.toString)}","phase":"parse"}"""
      case Right(decls) =>
        Elaborator.elaborate(decls) match
          case Left(elabErr) =>
            s"""{"ok":false,"error":"${esc(elabErr.message)}","phase":"elab"}"""
          case Right(result) =>
            given GlobalEnv = result.env
            Checker.checkAllWithWarnings(result) match
              case Left(proofErr) =>
                s"""{"ok":false,"error":"${esc(proofErr)}","phase":"proof"}"""
              case Right((env, warnings)) =>
                val indCount  = env.inductives.size
                val defCount  = env.defs.size
                val specCount = result.defspecs.size
                val warnJson  = warnings.map(w => s""""${esc(w)}"""").mkString("[", ",", "]")
                s"""{"ok":true,"inductives":$indCount,"defs":$defCount,"defspecs":$specCount,"warnings":$warnJson}"""

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
         |  sproof check <file.sproof>          Parse, elaborate, and verify a sproof file.
         |  sproof check --json <file.sproof>   Same, but output JSON (for tooling/agents).
         |  sproof extract <file.sproof>        Extract Scala 3 code from a verified sproof file.
         |  sproof agent <file.sproof>          Auto-repair sorry proofs using the proof agent.
         |  sproof repl                         Start the interactive REPL.
         |""".stripMargin
    )

end Main
