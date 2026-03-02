package sproof

import munit.FunSuite
import java.nio.file.{Files, Paths}

class CiSoundnessGateSuite extends FunSuite:

  test("CI workflow contains dedicated kernel soundness gate"):
    val path = Paths.get(".github/workflows/ci.yml")
    assert(Files.exists(path), "CI workflow file must exist")
    val ci = Files.readString(path)
    assert(ci.contains("kernel-soundness:"), "CI must define kernel-soundness job")
    assert(ci.contains("sbt kernel/test"), "kernel-soundness job must run kernel/test")

