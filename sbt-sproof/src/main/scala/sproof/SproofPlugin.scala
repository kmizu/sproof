package sproof

import sbt._
import sbt.Keys._
import sbt.plugins.JvmPlugin

import scala.sys.process._
import java.io.File

object SproofPlugin extends AutoPlugin {
  override def requires = JvmPlugin
  override def trigger  = noTrigger  // opt-in: enablePlugins(SproofPlugin)

  object autoImport {
    // Tasks
    val sproofCheck   = taskKey[Unit]("Type-check all .sproof files in sproofSources")
    val sproofExtract = taskKey[Seq[File]]("Extract .sproof files to Scala 3 source (wired into compile)")
    val sproofRepl    = inputKey[Unit]("Start the sproof interactive REPL")

    // Settings
    val sproofSources  = settingKey[Seq[File]]("Directories containing .sproof source files")
    val sproofOutput   = settingKey[File]("Output directory for extracted Scala sources")
    val sproofVersion  = settingKey[String]("Version of the sproof CLI to use")
    val sproofJar      = settingKey[Option[File]]("Path to the sproof CLI uber-JAR (None = use `sproof` on PATH)")
    val sproofJvmOpts  = settingKey[Seq[String]]("JVM options when forking the sproof CLI")
  }

  import autoImport._

  // ---- Helpers ----

  /** Build the command prefix: `java [opts] -jar <jar>` or `["sproof"]`. */
  private def sproofCmd(jar: Option[File], jvmOpts: Seq[String]): Seq[String] =
    jar match {
      case Some(j) => Seq("java") ++ jvmOpts ++ Seq("-jar", j.getAbsolutePath)
      case None    => Seq("sproof")
    }

  /** Run a sproof CLI command, streaming output to the sbt logger. Returns exit code. */
  private def runSproof(
      args:    Seq[String],
      jar:     Option[File],
      jvmOpts: Seq[String],
      log:     Logger,
  ): Int = {
    val cmd = sproofCmd(jar, jvmOpts) ++ args
    log.debug(s"sproof: executing: ${cmd.mkString(" ")}")
    val logger = ProcessLogger(
      out => log.info(s"  $out"),
      err => log.warn(s"  $err"),
    )
    Process(cmd).!(logger)
  }

  // ---- Default settings ----

  override lazy val projectSettings: Seq[Setting[?]] = Seq(
    sproofVersion := "0.1.0",
    sproofSources := Seq(sourceDirectory.value / "main" / "sproof"),
    sproofOutput  := (Compile / sourceManaged).value / "sproof",
    sproofJar     := None,
    sproofJvmOpts := Seq("-Xss8m"),

    // ---- sproofCheck ----
    sproofCheck := {
      val log      = streams.value.log
      val jar      = sproofJar.value
      val jvmOpts  = sproofJvmOpts.value
      val files    = sproofSources.value.flatMap { dir =>
        if (dir.exists) (dir ** "*.sproof").get else Seq.empty
      }

      if (files.isEmpty) {
        log.info("sproof: No .sproof files found.")
      } else {
        log.info(s"sproof: Checking ${files.length} file(s)...")
        val failed = files.filterNot { f =>
          val code = runSproof(Seq("check", f.getAbsolutePath), jar, jvmOpts, log)
          code == 0
        }
        if (failed.nonEmpty) {
          sys.error(s"sproof: ${failed.length} file(s) failed type-checking:\n" +
            failed.map("  " + _.getAbsolutePath).mkString("\n"))
        } else {
          log.success(s"sproof: All ${files.length} file(s) passed.")
        }
      }
    },

    // ---- sproofExtract ----
    sproofExtract := {
      val log      = streams.value.log
      val jar      = sproofJar.value
      val jvmOpts  = sproofJvmOpts.value
      val outDir   = sproofOutput.value
      val files    = sproofSources.value.flatMap { dir =>
        if (dir.exists) (dir ** "*.sproof").get else Seq.empty
      }

      IO.createDirectory(outDir)

      val generated: Seq[File] = files.map { src =>
        val outFile = outDir / src.getName.replace(".sproof", ".scala")
        // Only re-extract if source is newer than the generated file.
        if (!outFile.exists || src.lastModified > outFile.lastModified) {
          log.info(s"sproof: Extracting ${src.getName} → ${outFile.getName}")
          val code = runSproof(
            Seq("extract", src.getAbsolutePath, "--output", outFile.getAbsolutePath),
            jar,
            jvmOpts,
            log,
          )
          if (code != 0)
            sys.error(s"sproof: Extraction failed for ${src.getAbsolutePath}")
        } else {
          log.debug(s"sproof: ${src.getName} is up to date, skipping extraction.")
        }
        outFile
      }

      generated
    },

    // Hook sproofExtract into the compile cycle so `sbt compile` runs it automatically.
    Compile / sourceGenerators += sproofExtract.taskValue,

    // ---- sproofRepl ----
    sproofRepl := {
      val log     = streams.value.log
      val jar     = sproofJar.value
      val jvmOpts = sproofJvmOpts.value
      log.info("Starting sproof REPL (press Ctrl-D or type :quit to exit)...")
      val cmd = sproofCmd(jar, jvmOpts) ++ Seq("repl")
      log.debug(s"sproof: executing: ${cmd.mkString(" ")}")
      Process(cmd).!
      ()
    },
  )
}
