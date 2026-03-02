package sproof

import java.nio.file.{Files, Paths}
import munit.FunSuite

class ProofCookbookSuite extends FunSuite:

  private val SnippetPattern = "(?s)```sproof\\n(.*?)\\n```".r

  test("proof cookbook snippets are executable against current checker"):
    val path = Paths.get("docs/proof-cookbook.md")
    assert(Files.exists(path), "docs/proof-cookbook.md must exist")
    val doc = Files.readString(path)
    val snippets = SnippetPattern.findAllMatchIn(doc).map(_.group(1)).toList
    assert(snippets.length >= 4, s"expected multiple cookbook snippets, got ${snippets.length}")

    snippets.zipWithIndex.foreach { case (src, i) =>
      val result = Main.processSource(src, s"cookbook-snippet-$i")
      assert(result.isRight, s"cookbook snippet $i failed: $result\n$src")
    }

