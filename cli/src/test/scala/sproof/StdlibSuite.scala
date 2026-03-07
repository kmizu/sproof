package sproof

import java.nio.file.{Files, Paths}
import munit.FunSuite

class StdlibSuite extends FunSuite:

  private def read(path: String): String =
    Files.readString(Paths.get(path))

  test("stdlib v1 files parse/elaborate/check successfully"):
    val files = List(
      "stdlib/Nat.sproof",
      "stdlib/List.sproof",
      "stdlib/Vec.sproof",
      "stdlib/Bool.sproof",
      "stdlib/Relation.sproof",
      "stdlib/Effect.sproof",
      "stdlib/Dictionary.sproof",
      "stdlib/Option.sproof",
      "stdlib/Either.sproof",
      "stdlib/Pair.sproof",
    )
    files.foreach { file =>
      val src = read(file)
      val result = Main.processSource(src, file)
      assert(result.isRight, s"stdlib file should pass checker: $file -> $result")
    }

  test("usage examples consume stdlib declarations directly (concatenated source)"):
    val pairs = List(
      ("stdlib/Nat.sproof", "examples/stdlib/nat_usage.sproof"),
      ("stdlib/List.sproof", "examples/stdlib/list_usage.sproof"),
      ("stdlib/Vec.sproof", "examples/stdlib/vec_usage.sproof"),
      ("stdlib/Bool.sproof", "examples/stdlib/bool_usage.sproof"),
      ("stdlib/Relation.sproof", "examples/stdlib/relation_usage.sproof"),
      ("stdlib/Effect.sproof", "examples/stdlib/effect_usage.sproof"),
      ("stdlib/Dictionary.sproof", "examples/stdlib/dictionary_usage.sproof"),
      ("stdlib/Option.sproof", "examples/stdlib/option_usage.sproof"),
      ("stdlib/Either.sproof", "examples/stdlib/either_usage.sproof"),
      ("stdlib/Pair.sproof", "examples/stdlib/pair_usage.sproof"),
    )
    pairs.foreach { (lib, usage) =>
      val src = read(lib) + "\n\n" + read(usage)
      val result = Main.processSource(src, s"$lib + $usage")
      assert(result.isRight, s"usage should type-check with stdlib declarations: $lib + $usage -> $result")
    }

  test("stdlib naming/layout conventions are documented"):
    val doc = read("docs/stdlib.md")
    assert(doc.contains("stdlib/Nat.sproof"))
    assert(doc.contains("stdlib/List.sproof"))
    assert(doc.contains("stdlib/Vec.sproof"))
    assert(doc.contains("stdlib/Bool.sproof"))
    assert(doc.contains("stdlib/Relation.sproof"))
    assert(doc.contains("stdlib/Effect.sproof"))
    assert(doc.contains("stdlib/Dictionary.sproof"))
    assert(doc.contains("stdlib/Option.sproof"))
    assert(doc.contains("stdlib/Either.sproof"))
    assert(doc.contains("stdlib/Pair.sproof"))
