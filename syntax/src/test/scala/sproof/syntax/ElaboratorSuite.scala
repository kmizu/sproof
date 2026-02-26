package sproof.syntax

import munit.FunSuite
import sproof.core.*

class ElaboratorSuite extends FunSuite:

  private def parseAndElab(input: String): Either[ElabError, ElabResult] =
    Parser.parseProgram(input).left.map(e => ElabError(e)).flatMap(Elaborator.elaborate)

  // ===== Inductive type elaboration =====

  test("elaborate Nat inductive") {
    val input = """inductive Nat { case zero: Nat case succ(n: Nat): Nat }"""
    val result = parseAndElab(input)
    assert(result.isRight, s"Elaboration failed: $result")
    val elab = result.toOption.get
    val natDef = elab.env.lookupInd("Nat")
    assert(natDef.isDefined, "Nat should be in global env")
    assertEquals(natDef.get.name, "Nat")
    assertEquals(natDef.get.ctors.length, 2)
    assertEquals(natDef.get.ctors(0).name, "zero")
    assertEquals(natDef.get.ctors(0).argTpes, Nil)
    assertEquals(natDef.get.ctors(1).name, "succ")
  }

  test("elaborate Nat succ has one argument") {
    val input = """inductive Nat { case zero: Nat case succ(n: Nat): Nat }"""
    val result = parseAndElab(input)
    val elab = result.toOption.get
    val succCtor = elab.env.lookupInd("Nat").get.ctors(1)
    assertEquals(succCtor.argTpes.length, 1)
  }

  test("elaborate empty inductive") {
    val input = """inductive Empty { }"""
    val result = parseAndElab(input)
    assert(result.isRight, s"Elaboration failed: $result")
    val elab = result.toOption.get
    val emptyDef = elab.env.lookupInd("Empty")
    assert(emptyDef.isDefined, "Empty should be in global env")
    assertEquals(emptyDef.get.ctors, Nil)
  }

  // ===== Function elaboration =====

  test("elaborate identity function") {
    val input =
      """inductive Nat { case zero: Nat case succ(n: Nat): Nat }
        |def id(x: Nat): Nat = x""".stripMargin
    val result = parseAndElab(input)
    assert(result.isRight, s"Elaboration failed: $result")
    val elab = result.toOption.get
    assert(elab.defs.contains("id"), "id should be in defs")
    // body should be Var(0) since x is the only binding
    assertEquals(elab.defs("id"), Term.Var(0))
  }

  test("elaborate function with two params") {
    val input =
      """inductive Nat { case zero: Nat case succ(n: Nat): Nat }
        |def const(x: Nat, y: Nat): Nat = x""".stripMargin
    val result = parseAndElab(input)
    assert(result.isRight, s"Elaboration failed: $result")
    val elab = result.toOption.get
    // x is at index 1 (pushed first), y is at index 0
    assertEquals(elab.defs("const"), Term.Var(1))
  }

  test("elaborate function with constructor call") {
    val input =
      """inductive Nat { case zero: Nat case succ(n: Nat): Nat }
        |def zero_const(x: Nat): Nat = Nat.zero""".stripMargin
    val result = parseAndElab(input)
    assert(result.isRight, s"Elaboration failed: $result")
    val elab = result.toOption.get
    assertEquals(elab.defs("zero_const"), Term.Con("zero", "Nat", Nil))
  }

  test("elaborate function with match") {
    val input =
      """inductive Nat { case zero: Nat case succ(n: Nat): Nat }
        |def isZero(n: Nat): Nat {
        |  match n {
        |    case Nat.zero => Nat.zero
        |    case Nat.succ(k) => Nat.zero
        |  }
        |}""".stripMargin
    val result = parseAndElab(input)
    assert(result.isRight, s"Elaboration failed: $result")
    val elab = result.toOption.get
    val body = elab.defs("isZero")
    assert(body.isInstanceOf[Term.Mat], s"Expected Mat, got $body")
  }

  test("elaborate match cases have correct bindings count") {
    val input =
      """inductive Nat { case zero: Nat case succ(n: Nat): Nat }
        |def isZero(n: Nat): Nat {
        |  match n {
        |    case Nat.zero => Nat.zero
        |    case Nat.succ(k) => Nat.zero
        |  }
        |}""".stripMargin
    val result = parseAndElab(input)
    val elab = result.toOption.get
    val Term.Mat(_, cases, _) = elab.defs("isZero"): @unchecked
    assertEquals(cases(0).bindings, 0)
    assertEquals(cases(1).bindings, 1)
  }

  // ===== Defspec elaboration =====

  test("elaborate defspec preserves proof") {
    val input =
      """inductive Nat { case zero: Nat case succ(n: Nat): Nat }
        |defspec refl(n: Nat): n = n program = { by trivial }""".stripMargin
    val result = parseAndElab(input)
    assert(result.isRight, s"Elaboration failed: $result")
    val elab = result.toOption.get
    assert(elab.defspecs.contains("refl"), "refl should be in defspecs")
    val (_, _, proof) = elab.defspecs("refl")
    val SProof.SBy(STactic.STrivial) = proof: @unchecked
  }

  test("elaborate defspec with induction preserves proof") {
    val input =
      """inductive Nat { case zero: Nat case succ(n: Nat): Nat }
        |def plus(n: Nat, m: Nat): Nat {
        |  match n {
        |    case Nat.zero => m
        |    case Nat.succ(k) => Nat.succ(plus(k, m))
        |  }
        |}
        |defspec plus_zero(n: Nat): plus(n, Nat.zero) = n program = {
        |  by induction n {
        |    case zero => trivial
        |    case succ k ih => simplify [plus, ih]
        |  }
        |}""".stripMargin
    val result = parseAndElab(input)
    assert(result.isRight, s"Elaboration failed: $result")
    val elab = result.toOption.get
    assert(elab.defspecs.contains("plus_zero"), "plus_zero should be in defspecs")
  }

  // ===== Error cases =====

  test("elaborate duplicate inductive name") {
    val input =
      """inductive Nat { case zero: Nat }
        |inductive Nat { case one: Nat }""".stripMargin
    val result = parseAndElab(input)
    assert(result.isLeft, "Should fail on duplicate inductive name")
  }

  test("elaborate duplicate def name") {
    val input =
      """inductive Nat { case zero: Nat }
        |def f(x: Nat): Nat = x
        |def f(y: Nat): Nat = y""".stripMargin
    val result = parseAndElab(input)
    assert(result.isLeft, "Should fail on duplicate def name")
  }

  test("elaborate unresolved type variable") {
    val input = """def f(x: Unknown): Unknown = x"""
    val result = parseAndElab(input)
    assert(result.isLeft, "Should fail on unresolved type")
  }

  test("elaborate unresolved constructor") {
    val input =
      """inductive Nat { case zero: Nat }
        |def f(x: Nat): Nat = Foo.bar""".stripMargin
    val result = parseAndElab(input)
    assert(result.isLeft, "Should fail on unresolved constructor")
  }

  test("elaborate function with recursive call") {
    val input =
      """inductive Nat { case zero: Nat case succ(n: Nat): Nat }
        |def plus(n: Nat, m: Nat): Nat {
        |  match n {
        |    case Nat.zero => m
        |    case Nat.succ(k) => Nat.succ(plus(k, m))
        |  }
        |}""".stripMargin
    val result = parseAndElab(input)
    assert(result.isRight, s"Elaboration failed: $result")
    val elab = result.toOption.get
    assert(elab.defs.contains("plus"), "plus should be in defs")
    val body = elab.defs("plus")
    assert(body.isInstanceOf[Term.Mat], s"Expected Mat at top level, got $body")
  }

  // ===== Global env state =====

  test("elaborate builds cumulative global env") {
    val input =
      """inductive Nat { case zero: Nat case succ(n: Nat): Nat }
        |inductive Bool { case true: Bool case false: Bool }""".stripMargin
    val result = parseAndElab(input)
    assert(result.isRight, s"Elaboration failed: $result")
    val elab = result.toOption.get
    assert(elab.env.lookupInd("Nat").isDefined, "Nat should be in env")
    assert(elab.env.lookupInd("Bool").isDefined, "Bool should be in env")
  }
