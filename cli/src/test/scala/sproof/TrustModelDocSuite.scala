package sproof

import munit.FunSuite
import java.nio.file.{Files, Paths}

class TrustModelDocSuite extends FunSuite:

  private def read(path: String): String =
    val p = Paths.get(path)
    assert(Files.exists(p), s"expected file to exist: $path")
    Files.readString(p)

  test("README links to trust-model doc"):
    val readme = read("README.md")
    assert(
      readme.contains("(docs/trust-model.md)"),
      "README must link to docs/trust-model.md",
    )

  test("trust-model doc defines boundary and kernel contract"):
    val doc = read("docs/trust-model.md")
    assert(doc.contains("Trusted vs Untrusted Components"))
    assert(doc.contains("Kernel Responsibilities"))
    assert(doc.contains("Kernel API Contract"))
    assert(doc.contains("Failure Modes"))

