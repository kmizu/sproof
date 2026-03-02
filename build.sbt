import scala.scalanative.build._

val scala3Version  = "3.3.6"
val parsleyVersion = "5.0.0-M19"  // JVM + Native 共通（M15+ が native 対応）
val catsVersion    = "2.12.0"
val munitVersion   = "1.0.2"

// ---- Shared settings ----

val commonSettings = Seq(
  scalaVersion := scala3Version,
  version := "0.2.0",
  organization := "io.sproof",
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % catsVersion,
    "org.scalameta" %% "munit"     % munitVersion % Test,
  ),
  testFrameworks += new TestFramework("munit.Framework"),
  scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked"),
)

// Scala Native settings for native sub-projects.
// Uses %%% so cats-core and munit resolve as native artifacts.
val nativeCommonSettings = Seq(
  scalaVersion := scala3Version,
  version := "0.2.0",
  organization := "io.sproof",
  libraryDependencies ++= Seq(
    "org.typelevel" %%% "cats-core" % catsVersion,
    "org.scalameta" %%% "munit"     % munitVersion % Test,
  ),
  testFrameworks += new TestFramework("munit.Framework"),
  scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked"),
)

// Optimised native binary config (applied only to cliNative)
val nativeLinkSettings = Seq(
  nativeConfig ~= {
    _.withLTO(LTO.thin)
     .withMode(Mode.releaseFast)   // releaseFull is slower to link; swap when releasing
     .withGC(GC.immix)
  },
)

// ---- Helper: share JVM project sources with a native sibling ----
// Usage: .settings(shareSourcesWith(someJvmProject))
def shareSourcesWith(jvmProject: Project): Seq[Setting[?]] = Seq(
  Compile / unmanagedSourceDirectories +=
    (jvmProject / Compile / sourceDirectory).value,
  Test / unmanagedSourceDirectories +=
    (jvmProject / Test / sourceDirectory).value,
)

// ---- Root aggregate (JVM only by default) ----
lazy val root = project.in(file("."))
  .aggregate(core, nbe, checker, tactic, syntax, extract, kernel, cli)
  .settings(
    name := "sproof",
    publish / skip := true,
  )

// Root aggregate for all Native projects — run explicitly:
//   sbt nativeRoot/compile  or  sbt nativeRoot/test
lazy val nativeRoot = project.in(file("native-root"))
  .aggregate(coreNative, nbeNative, checkerNative, tacticNative,
             syntaxNative, extractNative, kernelNative, cliNative)
  .settings(
    name := "sproof-native",
    publish / skip := true,
    // Exclude from the default `sbt test` run so LLVM is optional
    aggregate := false,
  )

// ============================================================
// JVM projects
// ============================================================

lazy val core = project.in(file("core"))
  .settings(commonSettings)
  .settings(name := "sproof-core")

lazy val nbe = project.in(file("eval"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(name := "sproof-eval")

lazy val checker = project.in(file("checker"))
  .dependsOn(nbe)
  .settings(commonSettings)
  .settings(name := "sproof-checker")

lazy val tactic = project.in(file("tactic"))
  .dependsOn(checker)
  .settings(commonSettings)
  .settings(name := "sproof-tactic")

lazy val syntax = project.in(file("syntax"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    name := "sproof-syntax",
    libraryDependencies += "com.github.j-mie6" %% "parsley" % parsleyVersion,
  )

lazy val extract = project.in(file("extract"))
  .dependsOn(checker, tactic)
  .settings(commonSettings)
  .settings(name := "sproof-extract")

lazy val kernel = project.in(file("kernel"))
  .dependsOn(checker, tactic)
  .settings(commonSettings)
  .settings(name := "sproof-kernel")

lazy val cli = project.in(file("cli"))
  .dependsOn(syntax, tactic, extract, kernel)
  .settings(commonSettings)
  .settings(
    name := "sproof-cli",
    Compile / mainClass := Some("sproof.Main"),
  )

// ============================================================
// Scala Native projects
//
// Each native project:
//   - lives in a stub directory (no source files of its own)
//   - shares sources from the JVM counterpart via unmanagedSourceDirectories
//   - uses %%% for cross-platform deps
//
// PREREQUISITES (Ubuntu/WSL2):
//   sudo apt-get install clang lld libunwind-dev
//
// Build native CLI binary:
//   sbt cliNative/nativeLink
//   ./cli-native/target/scala-3.3.6/sproof-cli-native-out
// ============================================================

lazy val coreNative = project.in(file("core-native"))
  .enablePlugins(ScalaNativePlugin)
  .settings(nativeCommonSettings)
  .settings(shareSourcesWith(core))
  .settings(name := "sproof-core-native")

lazy val nbeNative = project.in(file("eval-native"))
  .enablePlugins(ScalaNativePlugin)
  .dependsOn(coreNative)
  .settings(nativeCommonSettings)
  .settings(shareSourcesWith(nbe))
  .settings(name := "sproof-eval-native")

lazy val checkerNative = project.in(file("checker-native"))
  .enablePlugins(ScalaNativePlugin)
  .dependsOn(nbeNative)
  .settings(nativeCommonSettings)
  .settings(shareSourcesWith(checker))
  .settings(name := "sproof-checker-native")

lazy val tacticNative = project.in(file("tactic-native"))
  .enablePlugins(ScalaNativePlugin)
  .dependsOn(checkerNative)
  .settings(nativeCommonSettings)
  .settings(shareSourcesWith(tactic))
  .settings(name := "sproof-tactic-native")

lazy val syntaxNative = project.in(file("syntax-native"))
  .enablePlugins(ScalaNativePlugin)
  .dependsOn(coreNative)
  .settings(nativeCommonSettings)
  .settings(shareSourcesWith(syntax))
  .settings(
    name := "sproof-syntax-native",
    libraryDependencies += "com.github.j-mie6" %%% "parsley" % parsleyVersion,
  )

lazy val extractNative = project.in(file("extract-native"))
  .enablePlugins(ScalaNativePlugin)
  .dependsOn(checkerNative, tacticNative)
  .settings(nativeCommonSettings)
  .settings(shareSourcesWith(extract))
  .settings(name := "sproof-extract-native")

lazy val kernelNative = project.in(file("kernel-native"))
  .enablePlugins(ScalaNativePlugin)
  .dependsOn(checkerNative, tacticNative)
  .settings(nativeCommonSettings)
  .settings(shareSourcesWith(kernel))
  .settings(name := "sproof-kernel-native")

lazy val cliNative = project.in(file("cli-native"))
  .enablePlugins(ScalaNativePlugin)
  .dependsOn(syntaxNative, tacticNative, extractNative, kernelNative)
  .settings(nativeCommonSettings)
  .settings(shareSourcesWith(cli))
  .settings(nativeLinkSettings)
  .settings(
    name := "sproof-cli-native",
    Compile / mainClass := Some("sproof.Main"),
  )
