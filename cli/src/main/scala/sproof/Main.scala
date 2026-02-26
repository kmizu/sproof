package sproof

import scala.io.{Source, StdIn}
import sproof.core.{GlobalEnv, Context}
import sproof.syntax.{Parser, Elaborator, ElabResult}
import sproof.tactic.TacticError

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

      case "repl" :: Nil =>
        given GlobalEnv = GlobalEnv.empty
        runRepl()

      case _ =>
        printUsage()
        sys.exit(1)

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
         |  defspec refl(n: Nat): n = n program = { by trivial }
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
         |  sproof check <file.sproof>   Parse, elaborate, and verify a sproof file.
         |  sproof repl                  Start the interactive REPL.
         |""".stripMargin
    )

end Main
