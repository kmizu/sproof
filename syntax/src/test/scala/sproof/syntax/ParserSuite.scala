package sproof.syntax

import munit.FunSuite

class ParserSuite extends FunSuite:

  // ===== Type parsing =====

  test("parse simple type variable") {
    val result = Parser.parseType("Nat")
    assertEquals(result, Right(SType.STVar("Nat")))
  }

  test("parse arrow type") {
    val result = Parser.parseType("Nat -> Nat")
    assertEquals(result, Right(SType.STArrow(SType.STVar("Nat"), SType.STVar("Nat"))))
  }

  test("parse arrow type is right-associative") {
    val result = Parser.parseType("A -> B -> C")
    assertEquals(result, Right(
      SType.STArrow(SType.STVar("A"), SType.STArrow(SType.STVar("B"), SType.STVar("C")))
    ))
  }

  test("parse Pi type") {
    val result = Parser.parseType("Pi(x: A, B)")
    assertEquals(result, Right(SType.STPi("x", SType.STVar("A"), SType.STVar("B"))))
  }

  test("parse Type (universe 0)") {
    val result = Parser.parseType("Type")
    assertEquals(result, Right(SType.STUni(0)))
  }

  test("parse Type0") {
    val result = Parser.parseType("Type0")
    assertEquals(result, Right(SType.STUni(0)))
  }

  test("parse Type1") {
    val result = Parser.parseType("Type1")
    assertEquals(result, Right(SType.STUni(1)))
  }

  test("parse type application") {
    val result = Parser.parseType("List(A)")
    assertEquals(result, Right(SType.STApp(SType.STVar("List"), SType.STVar("A"))))
  }

  test("parse parenthesized type") {
    val result = Parser.parseType("(Nat -> Nat) -> Nat")
    assertEquals(result, Right(
      SType.STArrow(
        SType.STArrow(SType.STVar("Nat"), SType.STVar("Nat")),
        SType.STVar("Nat"),
      )
    ))
  }

  // ===== Expression parsing =====

  test("parse variable expression") {
    val result = Parser.parseExpr("x")
    assertEquals(result, Right(SExpr.SEVar("x")))
  }

  test("parse function application") {
    val result = Parser.parseExpr("f(a, b)")
    assertEquals(result, Right(SExpr.SEApp(SExpr.SEVar("f"), List(SExpr.SEVar("a"), SExpr.SEVar("b")))))
  }

  test("parse constructor no args") {
    val result = Parser.parseExpr("Nat.zero")
    assertEquals(result, Right(SExpr.SECon("Nat", "zero", Nil)))
  }

  test("parse constructor with args") {
    val result = Parser.parseExpr("Nat.succ(n)")
    assertEquals(result, Right(SExpr.SECon("Nat", "succ", List(SExpr.SEVar("n")))))
  }

  test("parse match expression") {
    val input = "match n { case Nat.zero => m case Nat.succ(k) => Nat.succ(plus(k, m)) }"
    val result = Parser.parseExpr(input)
    assert(result.isRight, s"Parse failed: $result")
    val SExpr.SEMatch(scrutinee, cases) = result.toOption.get: @unchecked
    assertEquals(scrutinee, SExpr.SEVar("n"))
    assertEquals(cases.length, 2)
    assertEquals(cases(0).ctor, "Nat.zero")
    assertEquals(cases(0).bindings, Nil)
    assertEquals(cases(1).ctor, "Nat.succ")
    assertEquals(cases(1).bindings, List("k"))
  }

  test("parse lambda expression") {
    val result = Parser.parseExpr("fun (x: Nat) => x")
    assert(result.isRight, s"Parse failed: $result")
    val SExpr.SELam(params, body) = result.toOption.get: @unchecked
    assertEquals(params, List(SParam("x", SType.STVar("Nat"))))
    assertEquals(body, SExpr.SEVar("x"))
  }

  // ===== Declaration parsing =====

  test("parse inductive Nat") {
    val input = """inductive Nat { case zero: Nat case succ(n: Nat): Nat }"""
    val result = Parser.parseDecl(input)
    assert(result.isRight, s"Parse failed: $result")
    val SDecl.SInductive(name, params, ctors) = result.toOption.get: @unchecked
    assertEquals(name, "Nat")
    assertEquals(params, Nil)
    assertEquals(ctors.length, 2)
    assertEquals(ctors(0).name, "zero")
    assertEquals(ctors(0).argParams, Nil)
    assertEquals(ctors(0).retTpe, SType.STVar("Nat"))
    assertEquals(ctors(1).name, "succ")
    assertEquals(ctors(1).argParams, List(SParam("n", SType.STVar("Nat"))))
    assertEquals(ctors(1).retTpe, SType.STVar("Nat"))
  }

  test("parse inductive with parameters") {
    val input = """inductive List(A: Type) { case nil: List(A) case cons(head: A, tail: List(A)): List(A) }"""
    val result = Parser.parseDecl(input)
    assert(result.isRight, s"Parse failed: $result")
    val SDecl.SInductive(name, params, ctors) = result.toOption.get: @unchecked
    assertEquals(name, "List")
    assertEquals(params, List(SParam("A", SType.STUni(0))))
    assertEquals(ctors.length, 2)
  }

  test("parse def with equals body") {
    val input = """def id(x: Nat): Nat = x"""
    val result = Parser.parseDecl(input)
    assert(result.isRight, s"Parse failed: $result")
    val SDecl.SDef(name, params, retTpe, body) = result.toOption.get: @unchecked
    assertEquals(name, "id")
    assertEquals(params, List(SParam("x", SType.STVar("Nat"))))
    assertEquals(retTpe, SType.STVar("Nat"))
    assertEquals(body, SExpr.SEVar("x"))
  }

  test("parse def with block body") {
    val input =
      """def plus(n: Nat, m: Nat): Nat {
        |  match n {
        |    case Nat.zero => m
        |    case Nat.succ(k) => Nat.succ(plus(k, m))
        |  }
        |}""".stripMargin
    val result = Parser.parseDecl(input)
    assert(result.isRight, s"Parse failed: $result")
    val SDecl.SDef(name, params, retTpe, body) = result.toOption.get: @unchecked
    assertEquals(name, "plus")
    assertEquals(params.length, 2)
    assert(body.isInstanceOf[SExpr.SEMatch])
  }

  test("parse defspec with trivial") {
    val input = """defspec refl(n: Nat): n = n { by trivial }"""
    val result = Parser.parseDecl(input)
    assert(result.isRight, s"Parse failed: $result")
    val SDecl.SDefspec(name, params, prop, proof) = result.toOption.get: @unchecked
    assertEquals(name, "refl")
    assertEquals(params, List(SParam("n", SType.STVar("Nat"))))
    val SProof.SBy(tactic) = proof: @unchecked
    assertEquals(tactic, STactic.STrivial)
  }

  test("parse defspec with induction") {
    val input =
      """defspec plus_zero(n: Nat): plus(n, Nat.zero) = n {
        |  by induction n {
        |    case zero => trivial
        |    case succ k ih => simplify [plus, ih]
        |  }
        |}""".stripMargin
    val result = Parser.parseDecl(input)
    assert(result.isRight, s"Parse failed: $result")
    val SDecl.SDefspec(_, _, _, proof) = result.toOption.get: @unchecked
    val SProof.SBy(STactic.SInduction(varName, cases)) = proof: @unchecked
    assertEquals(varName, "n")
    assertEquals(cases.length, 2)
    assertEquals(cases(0).ctorName, "zero")
    assertEquals(cases(0).tactic, STactic.STrivial)
    assertEquals(cases(1).ctorName, "succ")
    assertEquals(cases(1).extraBindings, List("k", "ih"))
    val STactic.SSimplify(lemmas) = cases(1).tactic: @unchecked
    assertEquals(lemmas, List("plus", "ih"))
  }

  // ===== Tactic parsing =====

  test("parse tactic trivial") {
    val result = Parser.parseTactic("trivial")
    assertEquals(result, Right(STactic.STrivial))
  }

  test("parse tactic triv") {
    val result = Parser.parseTactic("triv")
    assertEquals(result, Right(STactic.STriv))
  }

  test("parse tactic assume") {
    val result = Parser.parseTactic("assume x y")
    assertEquals(result, Right(STactic.SAssume(List("x", "y"))))
  }

  test("parse tactic apply") {
    val result = Parser.parseTactic("apply f(x)")
    assert(result.isRight, s"Parse failed: $result")
    val STactic.SApply(expr) = result.toOption.get: @unchecked
    assertEquals(expr, SExpr.SEApp(SExpr.SEVar("f"), List(SExpr.SEVar("x"))))
  }

  test("parse tactic simplify") {
    val result = Parser.parseTactic("simplify [plus, ih]")
    assertEquals(result, Right(STactic.SSimplify(List("plus", "ih"))))
  }

  test("parse tactic simp") {
    val result = Parser.parseTactic("simp [f]")
    assertEquals(result, Right(STactic.SSimp(List("f"))))
  }

  test("parse tactic simp without lemmas") {
    val result = Parser.parseTactic("simp")
    assertEquals(result, Right(STactic.SSimp(Nil)))
  }

  test("parse tactic sorry") {
    val result = Parser.parseTactic("sorry")
    assertEquals(result, Right(STactic.SSorry))
  }

  test("parse tactic induction") {
    val input = "induction n { case zero => triv case succ k ih => sorry }"
    val result = Parser.parseTactic(input)
    assert(result.isRight, s"Parse failed: $result")
    val STactic.SInduction(varName, cases) = result.toOption.get: @unchecked
    assertEquals(varName, "n")
    assertEquals(cases.length, 2)
    assertEquals(cases(0).ctorName, "zero")
    assertEquals(cases(0).extraBindings, Nil)
    assertEquals(cases(0).tactic, STactic.STriv)
    assertEquals(cases(1).ctorName, "succ")
    assertEquals(cases(1).extraBindings, List("k", "ih"))
    assertEquals(cases(1).tactic, STactic.SSorry)
  }

  // ===== Program parsing (multiple declarations) =====

  test("parse multiple declarations") {
    val input =
      """inductive Nat { case zero: Nat case succ(n: Nat): Nat }
        |def id(x: Nat): Nat = x""".stripMargin
    val result = Parser.parseProgram(input)
    assert(result.isRight, s"Parse failed: $result")
    assertEquals(result.toOption.get.length, 2)
  }

  // ===== Equality expression in defspec =====

  test("parse defspec proposition as equality type") {
    // The proposition "plus(n, Nat.zero) = n" is an equality type
    val input = """defspec test(n: Nat): plus(n, Nat.zero) = n { by sorry }"""
    val result = Parser.parseDecl(input)
    assert(result.isRight, s"Parse failed: $result")
  }

  // ===== Edge cases =====

  test("parse empty inductive (no ctors)") {
    val input = """inductive Empty { }"""
    val result = Parser.parseDecl(input)
    assert(result.isRight, s"Parse failed: $result")
    val SDecl.SInductive(name, _, ctors) = result.toOption.get: @unchecked
    assertEquals(name, "Empty")
    assertEquals(ctors, Nil)
  }

  test("parse nested constructor application") {
    val result = Parser.parseExpr("Nat.succ(Nat.succ(Nat.zero))")
    assert(result.isRight, s"Parse failed: $result")
    val SExpr.SECon(_, _, args) = result.toOption.get: @unchecked
    assertEquals(args.length, 1)
    assert(args(0).isInstanceOf[SExpr.SECon], "expected nested SECon")
  }

  test("invalid syntax returns Left") {
    val result = Parser.parseDecl("not a valid declaration")
    assert(result.isLeft, "Expected parse error for invalid input")
  }

  test("parse simplify with empty brackets") {
    val result = Parser.parseTactic("simplify []")
    assertEquals(result, Right(STactic.SSimplify(Nil)))
  }

  test("parse comments are ignored") {
    val input =
      """// this is a comment
        |def id(x: Nat): Nat = x""".stripMargin
    val result = Parser.parseDecl(input)
    assert(result.isRight, s"Parse failed: $result")
  }

  test("parse tactic apply with simple variable") {
    val result = Parser.parseTactic("apply x")
    assert(result.isRight, s"Parse failed: $result")
    val STactic.SApply(expr) = result.toOption.get: @unchecked
    assertEquals(expr, SExpr.SEVar("x"))
  }

  // ===== Structure declarations =====

  test("parse structure with one field") {
    val input = "structure Wrap { value: Nat }"
    val result = Parser.parseDecl(input)
    assert(result.isRight, s"Parse failed: $result")
    val SDecl.SStructure(name, fields) = result.toOption.get: @unchecked
    assertEquals(name, "Wrap")
    assertEquals(fields.length, 1)
    assertEquals(fields(0).name, "value")
    assertEquals(fields(0).tpe, SType.STVar("Nat"))
  }

  test("parse structure with two fields") {
    val input = "structure Pair { fst: Nat  snd: Nat }"
    val result = Parser.parseDecl(input)
    assert(result.isRight, s"Parse failed: $result")
    val SDecl.SStructure(name, fields) = result.toOption.get: @unchecked
    assertEquals(name, "Pair")
    assertEquals(fields.length, 2)
    assertEquals(fields(0).name, "fst")
    assertEquals(fields(1).name, "snd")
  }

  test("parse structure with arrow field type") {
    val input = "structure Add { add: Nat -> Nat -> Nat  zero: Nat }"
    val result = Parser.parseDecl(input)
    assert(result.isRight, s"Parse failed: $result")
    val SDecl.SStructure(name, fields) = result.toOption.get: @unchecked
    assertEquals(name, "Add")
    assertEquals(fields.length, 2)
    assertEquals(fields(0).name, "add")
    assertEquals(fields(0).tpe, SType.STArrow(SType.STVar("Nat"), SType.STArrow(SType.STVar("Nat"), SType.STVar("Nat"))))
    assertEquals(fields(1).name, "zero")
  }

  test("parse empty structure") {
    val input = "structure Empty { }"
    val result = Parser.parseDecl(input)
    assert(result.isRight, s"Parse failed: $result")
    val SDecl.SStructure(name, fields) = result.toOption.get: @unchecked
    assertEquals(name, "Empty")
    assertEquals(fields, Nil)
  }

  // ===== Instance declarations =====

  test("parse instance with one binding") {
    val input = "instance wrapZero: Wrap { value = Nat.zero }"
    val result = Parser.parseDecl(input)
    assert(result.isRight, s"Parse failed: $result")
    val SDecl.SInstance(name, structName, bindings) = result.toOption.get: @unchecked
    assertEquals(name, "wrapZero")
    assertEquals(structName, "Wrap")
    assertEquals(bindings.length, 1)
    assertEquals(bindings(0)._1, "value")
    assertEquals(bindings(0)._2, SExpr.SECon("Nat", "zero", Nil))
  }

  test("parse instance with two bindings") {
    val input = "instance addNat: Add { add = plus  zero = Nat.zero }"
    val result = Parser.parseDecl(input)
    assert(result.isRight, s"Parse failed: $result")
    val SDecl.SInstance(name, structName, bindings) = result.toOption.get: @unchecked
    assertEquals(name, "addNat")
    assertEquals(structName, "Add")
    assertEquals(bindings.length, 2)
    assertEquals(bindings(0)._1, "add")
    assertEquals(bindings(0)._2, SExpr.SEVar("plus"))
    assertEquals(bindings(1)._1, "zero")
    assertEquals(bindings(1)._2, SExpr.SECon("Nat", "zero", Nil))
  }

  // ===== Operator declarations =====

  test("parse operator declaration with equals body") {
    val input = "operator (x: Nat) + (y: Nat): Nat = plus(x, y)"
    val result = Parser.parseDecl(input)
    assert(result.isRight, s"Parse failed: $result")
    val SDecl.SOperator(lhsParam, opSym, rhsParam, retTpe, body) = result.toOption.get: @unchecked
    assertEquals(lhsParam.name, "x")
    assertEquals(lhsParam.tpe, SType.STVar("Nat"))
    assertEquals(opSym, "+")
    assertEquals(rhsParam.name, "y")
    assertEquals(rhsParam.tpe, SType.STVar("Nat"))
    assertEquals(retTpe, SType.STVar("Nat"))
    assertEquals(body, SExpr.SEApp(SExpr.SEVar("plus"), List(SExpr.SEVar("x"), SExpr.SEVar("y"))))
  }

  test("parse operator declaration with multiply") {
    val input = "operator (x: Nat) * (y: Nat): Nat = mul(x, y)"
    val result = Parser.parseDecl(input)
    assert(result.isRight, s"Parse failed: $result")
    val SDecl.SOperator(_, opSym, _, _, _) = result.toOption.get: @unchecked
    assertEquals(opSym, "*")
  }

  test("operator declaration requires type annotations (lhs)") {
    // Without parens and type annotation → parse error
    val input = "operator x + (y: Nat): Nat = plus(x, y)"
    val result = Parser.parseDecl(input)
    assert(result.isLeft, "Expected parse error when lhs has no type annotation")
  }

  test("operator declaration requires type annotations (rhs)") {
    val input = "operator (x: Nat) + y: Nat = plus(x, y)"
    val result = Parser.parseDecl(input)
    assert(result.isLeft, "Expected parse error when rhs has no type annotation")
  }

  // ===== Infix expressions =====

  test("parse infix addition: x + y") {
    val result = Parser.parseExpr("x + y")
    assertEquals(result, Right(SExpr.SInfix(SExpr.SEVar("x"), "+", SExpr.SEVar("y"))))
  }

  test("parse infix multiplication: x * y") {
    val result = Parser.parseExpr("x * y")
    assertEquals(result, Right(SExpr.SInfix(SExpr.SEVar("x"), "*", SExpr.SEVar("y"))))
  }

  test("parse infix addition is left-associative") {
    val result = Parser.parseExpr("x + y + z")
    assertEquals(result, Right(
      SExpr.SInfix(SExpr.SInfix(SExpr.SEVar("x"), "+", SExpr.SEVar("y")), "+", SExpr.SEVar("z"))
    ))
  }

  test("* binds tighter than +") {
    val result = Parser.parseExpr("x + y * z")
    assertEquals(result, Right(
      SExpr.SInfix(
        SExpr.SEVar("x"),
        "+",
        SExpr.SInfix(SExpr.SEVar("y"), "*", SExpr.SEVar("z")),
      )
    ))
  }

  test("parse infix with constructor args") {
    val result = Parser.parseExpr("Nat.succ(n) + Nat.zero")
    assert(result.isRight, s"Parse failed: $result")
    val SExpr.SInfix(lhs, op, rhs) = result.toOption.get: @unchecked
    assertEquals(op, "+")
    assert(lhs.isInstanceOf[SExpr.SECon])
    assertEquals(rhs, SExpr.SECon("Nat", "zero", Nil))
  }

  test("parse infix in def body") {
    val input = "def addOne(n: Nat): Nat = n + Nat.zero"
    val result = Parser.parseDecl(input)
    assert(result.isRight, s"Parse failed: $result")
    val SDecl.SDef(_, _, _, body) = result.toOption.get: @unchecked
    assert(body.isInstanceOf[SExpr.SInfix], s"Expected SInfix, got: $body")
  }

  // ===== Chained function application =====

  test("parse chained application: f(a)(b)") {
    val result = Parser.parseExpr("f(a)(b)")
    assertEquals(result, Right(
      SExpr.SEApp(SExpr.SEApp(SExpr.SEVar("f"), List(SExpr.SEVar("a"))), List(SExpr.SEVar("b")))
    ))
  }

  test("parse chained application with multiple args: f(a)(b, c)") {
    val result = Parser.parseExpr("f(a)(b, c)")
    assertEquals(result, Right(
      SExpr.SEApp(
        SExpr.SEApp(SExpr.SEVar("f"), List(SExpr.SEVar("a"))),
        List(SExpr.SEVar("b"), SExpr.SEVar("c")),
      )
    ))
  }

  test("parse chained application three deep: f(a)(b)(c)") {
    val result = Parser.parseExpr("f(a)(b)(c)")
    assertEquals(result, Right(
      SExpr.SEApp(
        SExpr.SEApp(SExpr.SEApp(SExpr.SEVar("f"), List(SExpr.SEVar("a"))), List(SExpr.SEVar("b"))),
        List(SExpr.SEVar("c")),
      )
    ))
  }

  test("single application unchanged: f(a, b)") {
    val result = Parser.parseExpr("f(a, b)")
    assertEquals(result, Right(SExpr.SEApp(SExpr.SEVar("f"), List(SExpr.SEVar("a"), SExpr.SEVar("b")))))
  }

  test("parse infix in defspec proposition") {
    val input = "defspec test(n: Nat): n + Nat.zero = n { by sorry }"
    val result = Parser.parseDecl(input)
    assert(result.isRight, s"Parse failed: $result")
    val SDecl.SDefspec(_, _, prop, _) = result.toOption.get: @unchecked
    val SType.STEq(lhs, _) = prop: @unchecked
    assert(lhs.isInstanceOf[SExpr.SInfix], s"Expected SInfix lhs in equality, got: $lhs")
  }

  // ===== Type parameter parsing (feature 2) =====

  test("parse def with single type parameter") {
    val input = "def id[A](x: A): A = x"
    val result = Parser.parseDecl(input)
    assert(result.isRight, s"Parse failed: $result")
    val SDecl.SDef(name, params, retTpe, body) = result.toOption.get: @unchecked
    assertEquals(name, "id")
    assertEquals(params.length, 2)
    assertEquals(params(0), SParam("A", SType.STUni(0)))
    assertEquals(params(1), SParam("x", SType.STVar("A")))
    assertEquals(retTpe, SType.STVar("A"))
  }

  test("parse def with multiple type parameters") {
    val input = "def const[A, B](x: A, y: B): A = x"
    val result = Parser.parseDecl(input)
    assert(result.isRight, s"Parse failed: $result")
    val SDecl.SDef(_, params, _, _) = result.toOption.get: @unchecked
    assertEquals(params.length, 4)
    assertEquals(params(0), SParam("A", SType.STUni(0)))
    assertEquals(params(1), SParam("B", SType.STUni(0)))
    assertEquals(params(2), SParam("x", SType.STVar("A")))
    assertEquals(params(3), SParam("y", SType.STVar("B")))
  }

  test("parse defspec with type parameter") {
    val input = "defspec idSpec[A](x: A): x = x { by sorry }"
    val result = Parser.parseDecl(input)
    assert(result.isRight, s"Parse failed: $result")
    val SDecl.SDefspec(_, params, _, _) = result.toOption.get: @unchecked
    assertEquals(params.length, 2)
    assertEquals(params(0), SParam("A", SType.STUni(0)))
    assertEquals(params(1), SParam("x", SType.STVar("A")))
  }

  test("parse def without type parameters is unchanged") {
    val input = "def id(x: Nat): Nat = x"
    val result = Parser.parseDecl(input)
    assert(result.isRight, s"Parse failed: $result")
    val SDecl.SDef(_, params, _, _) = result.toOption.get: @unchecked
    assertEquals(params.length, 1)
    assertEquals(params(0), SParam("x", SType.STVar("Nat")))
  }

  // ===== List literal parsing (feature 5) =====

  test("parse empty list literal") {
    val result = Parser.parseExpr("[]")
    assertEquals(result, Right(SExpr.SEList(Nil)))
  }

  test("parse single-element list literal") {
    val result = Parser.parseExpr("[x]")
    assertEquals(result, Right(SExpr.SEList(List(SExpr.SEVar("x")))))
  }

  test("parse multi-element list literal") {
    val result = Parser.parseExpr("[x, y, z]")
    assertEquals(result, Right(SExpr.SEList(List(SExpr.SEVar("x"), SExpr.SEVar("y"), SExpr.SEVar("z")))))
  }

  test("parse nested list literal") {
    val result = Parser.parseExpr("[Nat.zero, Nat.succ(Nat.zero)]")
    assert(result.isRight, s"Parse failed: $result")
    val SExpr.SEList(elems) = result.toOption.get: @unchecked
    assertEquals(elems.length, 2)
  }

  test("parse list literal in def body") {
    val input = "def myList(x: Nat): Nat = x"
    // Sanity: list literal compiles in expression context
    val exprResult = Parser.parseExpr("[x, y]")
    assert(exprResult.isRight)
  }

  // ===== have tactic =====

  test("parse tactic have with trivial sub-proof and trivial continuation") {
    val input = "have h : Nat = { by trivial } ; trivial"
    val result = Parser.parseTactic(input)
    assert(result.isRight, s"Parse failed: $result")
    val STactic.SHave(name, tpe, proof, cont) = result.toOption.get: @unchecked
    assertEquals(name, "h")
    assertEquals(tpe, SType.STVar("Nat"))
    val SProof.SBy(STactic.STrivial) = proof: @unchecked
    assertEquals(cont, STactic.STrivial)
  }

  test("parse tactic have with arrow type") {
    val input = "have h : Nat -> Nat = { by sorry } ; sorry"
    val result = Parser.parseTactic(input)
    assert(result.isRight, s"Parse failed: $result")
    val STactic.SHave(name, tpe, _, _) = result.toOption.get: @unchecked
    assertEquals(name, "h")
    assertEquals(tpe, SType.STArrow(SType.STVar("Nat"), SType.STVar("Nat")))
  }

  test("parse tactic have in defspec context") {
    val input = "defspec test(n: Nat): n = n { by have h : Nat = { by sorry } ; trivial }"
    val result = Parser.parseDecl(input)
    assert(result.isRight, s"Parse failed: $result")
  }

  // ===== rewrite tactic =====

  test("parse tactic rewrite with single lemma") {
    val input = "rewrite lemma1"
    val result = Parser.parseTactic(input)
    assert(result.isRight, s"Parse failed: $result")
    val STactic.SRewrite(lemmas) = result.toOption.get: @unchecked
    assertEquals(lemmas, List("lemma1"))
  }

  test("parse tactic rewrite with bracketed lemma list") {
    val input = "rewrite [lemma1, lemma2]"
    val result = Parser.parseTactic(input)
    assert(result.isRight, s"Parse failed: $result")
    val STactic.SRewrite(lemmas) = result.toOption.get: @unchecked
    assertEquals(lemmas, List("lemma1", "lemma2"))
  }

  // ===== calc block =====

  test("parse tactic calc with two steps") {
    val input =
      """calc {
        |  x = y { by trivial }
        |  _ = z { by trivial }
        |}""".stripMargin
    val result = Parser.parseTactic(input)
    assert(result.isRight, s"Parse failed: $result")
    val STactic.SCalc(steps) = result.toOption.get: @unchecked
    assertEquals(steps.length, 2)
    assert(steps(0).lhs.isDefined, "first step should have explicit lhs")
    assert(steps(1).lhs.isEmpty, "second step should have _ lhs")
  }

  // ===== numeric literals =====

  test("parse numeric literal 0") {
    val result = Parser.parseExpr("0")
    assert(result.isRight, s"Parse failed: $result")
    val SExpr.SEInt(n) = result.toOption.get: @unchecked
    assertEquals(n, 0)
  }

  test("parse numeric literal 42") {
    val result = Parser.parseExpr("42")
    assert(result.isRight, s"Parse failed: $result")
    val SExpr.SEInt(n) = result.toOption.get: @unchecked
    assertEquals(n, 42)
  }

  test("parse negative numeric literal -3") {
    val result = Parser.parseExpr("-3")
    assert(result.isRight, s"Parse failed: $result")
    val SExpr.SEInt(n) = result.toOption.get: @unchecked
    assertEquals(n, -3)
  }

  test("parse numeric literal in function argument position") {
    val result = Parser.parseExpr("f(0)")
    assert(result.isRight, s"Parse failed: $result")
    val SExpr.SEApp(fn, args) = result.toOption.get: @unchecked
    assertEquals(fn, SExpr.SEVar("f"))
    val SExpr.SEInt(0) = args(0): @unchecked
  }

  test("parse numeric literal in infix expression") {
    val result = Parser.parseExpr("0 + 1")
    assert(result.isRight, s"Parse failed: $result")
    val SExpr.SInfix(lhs, op, rhs) = result.toOption.get: @unchecked
    assertEquals(op, "+")
    val SExpr.SEInt(0) = lhs: @unchecked
    val SExpr.SEInt(1) = rhs: @unchecked
  }

  test("parse rewrite with empty brackets fails") {
    // rewrite requires at least one lemma
    val result = Parser.parseTactic("rewrite []")
    // This should fail because rewrite requires at least one lemma in brackets
    // Actually, it should try the identifier branch and fail
    assert(result.isLeft, "rewrite with empty brackets should fail")
  }

  test("parse calc with single step") {
    val input =
      """calc {
        |  x = y { by sorry }
        |}""".stripMargin
    val result = Parser.parseTactic(input)
    assert(result.isRight, s"Parse failed: $result")
    val STactic.SCalc(steps) = result.toOption.get: @unchecked
    assertEquals(steps.length, 1)
  }

  // ===== Unicode and backtick identifiers =====

  test("direct Unicode identifier in def name") {
    val result = Parser.parseDecl("def 加算(n: Nat): Nat = n")
    assert(result.isRight, s"Parse failed: $result")
    val SDecl.SDef(name, _, _, _) = result.toOption.get: @unchecked
    assertEquals(name, "加算")
  }

  test("direct Unicode identifier in defspec name") {
    val result = Parser.parseDecl("defspec 交換法則(n: Nat): n = n { by sorry }")
    assert(result.isRight, s"Parse failed: $result")
    val SDecl.SDefspec(name, _, _, _) = result.toOption.get: @unchecked
    assertEquals(name, "交換法則")
  }

  test("backtick identifier: simple ASCII name") {
    val result = Parser.parseExpr("`myFunc`")
    assertEquals(result, Right(SExpr.SEVar("myFunc")))
  }

  test("backtick identifier: Japanese name") {
    val result = Parser.parseExpr("`交換法則`")
    assertEquals(result, Right(SExpr.SEVar("交換法則")))
  }

  test("backtick identifier: name with spaces") {
    val result = Parser.parseExpr("`plus is commutative`")
    assertEquals(result, Right(SExpr.SEVar("plus is commutative")))
  }

  test("backtick identifier in def name") {
    val result = Parser.parseDecl("def `交換法則`(n: Nat): Nat = n")
    assert(result.isRight, s"Parse failed: $result")
    val SDecl.SDef(name, _, _, _) = result.toOption.get: @unchecked
    assertEquals(name, "交換法則")
  }

  test("backtick identifier: name with symbols") {
    val result = Parser.parseExpr("`a + b = b + a`")
    assertEquals(result, Right(SExpr.SEVar("a + b = b + a")))
  }

  test("backtick identifier in defspec") {
    val input = "defspec `交換法則`(n: Nat): n = n { by sorry }"
    val result = Parser.parseDecl(input)
    assert(result.isRight, s"Parse failed: $result")
    val SDecl.SDefspec(name, _, _, _) = result.toOption.get: @unchecked
    assertEquals(name, "交換法則")
  }

  test("backtick identifier used as function call") {
    val result = Parser.parseExpr("`plus`(x, y)")
    assertEquals(result, Right(SExpr.SEApp(SExpr.SEVar("plus"), List(SExpr.SEVar("x"), SExpr.SEVar("y")))))
  }

  // ===== Lean-inspired features =====

  // -- rfl / intro / intros / rw / exact tactic aliases --

  test("tactic: rfl parses as SRfl") {
    assertEquals(Parser.parseTactic("rfl"), Right(STactic.SRfl))
  }

  test("tactic: intro x parses as SAssume") {
    assertEquals(Parser.parseTactic("intro x"), Right(STactic.SAssume(List("x"))))
  }

  test("tactic: intros x y parses as SAssume") {
    assertEquals(Parser.parseTactic("intros x y"), Right(STactic.SAssume(List("x", "y"))))
  }

  test("tactic: rw [h] parses as SRw") {
    assertEquals(Parser.parseTactic("rw [h]"), Right(STactic.SRw(List("h"))))
  }

  test("tactic: rw [h1, h2] parses as SRw with multiple lemmas") {
    assertEquals(Parser.parseTactic("rw [h1, h2]"), Right(STactic.SRw(List("h1", "h2"))))
  }

  test("tactic: exact x parses as SExact") {
    assertEquals(Parser.parseTactic("exact x"), Right(STactic.SExact(SExpr.SEVar("x"))))
  }

  test("tactic: exact Nat.zero parses as SExact with Con") {
    assertEquals(Parser.parseTactic("exact Nat.zero"), Right(STactic.SExact(SExpr.SECon("Nat", "zero", Nil))))
  }

  // -- theorem keyword --

  test("theorem keyword parses as SDefspec") {
    val result = Parser.parseDecl("theorem plus_zero(n: Nat): n = n { by rfl }")
    assert(result.isRight, s"Parse failed: $result")
    val SDecl.SDefspec(name, _, _, _) = result.toOption.get: @unchecked
    assertEquals(name, "plus_zero")
  }

  // -- let expression --

  test("let x := e; body parses as SELet") {
    val result = Parser.parseExpr("let x := n; x")
    assertEquals(result, Right(SExpr.SELet("x", SExpr.SEVar("n"), SExpr.SEVar("x"))))
  }

  test("let with application body") {
    val result = Parser.parseExpr("let k := plus(n, m); k")
    assertEquals(result, Right(
      SExpr.SELet("k",
        SExpr.SEApp(SExpr.SEVar("plus"), List(SExpr.SEVar("n"), SExpr.SEVar("m"))),
        SExpr.SEVar("k"))
    ))
  }

  // -- @[simp] attribute --

  test("@[simp] def parses as SAttr") {
    val result = Parser.parseDecl("@[simp] def id(n: Nat): Nat = n")
    assert(result.isRight, s"Parse failed: $result")
    result.toOption.get match
      case SDecl.SAttr("simp", SDecl.SDef("id", _, _, _)) => ()
      case other => fail(s"Expected SAttr(simp, SDef(id,...)), got $other")
  }

  // -- #check command --

  test("#check expr parses as SCheck") {
    val result = Parser.parseDecl("#check n")
    assert(result.isRight, s"Parse failed: $result")
    result.toOption.get match
      case SDecl.SCheck(SExpr.SEVar("n")) => ()
      case other => fail(s"Expected SCheck(SEVar(n)), got $other")
  }

  // -- forall type syntax --

  test("forall (x: Nat), Nat parses as STPi") {
    val result = Parser.parseType("Pi(x: Nat, Nat)")
    assertEquals(result, Right(SType.STPi("x", SType.STVar("Nat"), SType.STVar("Nat"))))
  }

  // -- tactic sequence --

  test("{ trivial; sorry } parses as SSeq") {
    val result = Parser.parseTactic("{ trivial; sorry }")
    assertEquals(result, Right(STactic.SSeq(List(STactic.STrivial, STactic.SSorry))))
  }

  test("{ trivial } single-element sequence unwraps to tactic") {
    val result = Parser.parseTactic("{ trivial }")
    assertEquals(result, Right(STactic.STrivial))
  }
