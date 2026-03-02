package sproof

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.*
import munit.FunSuite

class LemmaBundleSuite extends FunSuite:

  private val bundleDir = Paths.get("stdlib/bundles")
  private val stdlibDir = Paths.get("stdlib")

  private def read(path: java.nio.file.Path): String =
    Files.readString(path)

  private def listFiles(dir: java.nio.file.Path, suffix: String): List[java.nio.file.Path] =
    val stream = Files.list(dir)
    try
      stream.iterator().asScala.toList
        .filter(_.toString.endsWith(suffix))
        .sortBy(_.getFileName.toString)
    finally stream.close()

  private def bundleFiles: List[java.nio.file.Path] =
    listFiles(bundleDir, ".bundle")

  private def bundleEntries(path: java.nio.file.Path): List[String] =
    read(path).linesIterator
      .map(_.trim)
      .filter(line => line.nonEmpty && !line.startsWith("#"))
      .toList

  test("at least three practical bundles are available"):
    assert(Files.isDirectory(bundleDir), "stdlib/bundles must exist")
    assert(bundleFiles.length >= 3, s"expected >= 3 bundles, got ${bundleFiles.map(_.getFileName)}")
    bundleFiles.foreach { b =>
      val entries = bundleEntries(b)
      assert(entries.nonEmpty, s"bundle must contain at least one lemma: ${b.getFileName}")
    }

  test("bundle entries reference known stdlib lemma names"):
    val stdlibText =
      listFiles(stdlibDir, ".sproof")
        .map(read)
        .mkString("\n")

    bundleFiles.foreach { b =>
      bundleEntries(b).foreach { lemma =>
        assert(
          stdlibText.contains(s"defspec $lemma"),
          s"bundle lemma '$lemma' must exist in stdlib defspecs (${b.getFileName})",
        )
      }
    }

  test("available bundle names are documented"):
    val doc = read(Paths.get("docs/lemma-bundles.md"))
    bundleFiles.foreach { path =>
      val bundleName = path.getFileName.toString.stripSuffix(".bundle")
      assert(doc.contains(s"`$bundleName`"), s"bundle name must be documented: $bundleName")
    }

  test("bundle compatibility policy is documented"):
    val doc = read(Paths.get("docs/lemma-bundles.md"))
    assert(doc.contains("Compatibility policy"))
    assert(doc.contains("major stdlib revision"))
