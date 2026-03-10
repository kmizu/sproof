package sroof.syntax

import parsley.Parsley
import parsley.Parsley.{pure, empty, atomic}
import parsley.combinator.{option, sepBy, sepBy1, choice}
import parsley.token.Lexer
import parsley.token.descriptions.*
import parsley.token.Basic
import parsley.expr.chain
import parsley.syntax.zipped.*
import parsley.character.{digit, char, stringOfMany, stringOfSome}

/** Parser for the sroof surface syntax, built with Parsley 5.0.0-M9.
  *
  * All recursive grammar nonterminals are defined as `lazy val` with forward
  * references wrapped in by-name positions or `LazyParsley` to avoid
  * Scala 3 lazy-val initialization deadlocks.
  */
object Parser:

  // ---- Lexer setup ----

  private val lexer = new Lexer(LexicalDesc.plain.copy(
    nameDesc = NameDesc.plain.copy(
      identifierStart = Basic(c => c.isLetter || c == '_'),
      identifierLetter = Basic(c => c.isLetterOrDigit || c == '_'),
    ),
    spaceDesc = SpaceDesc.plain.copy(
      lineCommentStart = "//",
      multiLineCommentStart = "/*",
      multiLineCommentEnd = "*/",
    ),
    symbolDesc = SymbolDesc.plain.copy(
      hardKeywords = Set(
        "inductive", "enum", "def", "defspec", "theorem", "case", "match", "by",
        "trivial", "triv", "rfl", "assume", "intro", "intros",
        "apply", "exact", "simplify", "simp",
        "induction", "sorry", "fun", "Pi", "Type",
        "structure", "trait", "instance", "given", "operator",
        "have", "rewrite", "rw", "calc", "let", "in",
        "try", "first", "repeat", "all_goals", "skip",
        "assumption", "contradiction", "cases",
        "if", "then", "else",
        "split", "constructor",
        "use", "exists", "by_contra", "tauto",
        "obtain", "specialize",
      ),
      hardOperators = Set("->", "==", "=", "=>", ":=", ":", ",", "{", "}", "(", ")", "[", "]", ".", "+", "*", ";", "-", "@", "|"),
    ),
  ))

  // ---- Token helpers ----

  private val regularIdentifier = lexer.lexeme.names.identifier

  /** Backtick-quoted identifier: `any content except backtick`.
   *  Allows spaces, symbols, Unicode — anything that can't appear in a regular identifier.
   *  Consumes trailing whitespace via `ws`.
   */
  private lazy val backtickIdentifier: Parsley[String] =
    atomic(
      char('`') *> stringOfMany(_ != '`') <* char('`')
    ) <* ws

  /** Any identifier: backtick-quoted (tried first) or regular. */
  private lazy val anyIdentifier: Parsley[String] =
    backtickIdentifier <|> regularIdentifier

  private val identifier = regularIdentifier
  private def keyword(kw: String) = lexer.lexeme.symbol(kw)
  private def op(o: String) = lexer.lexeme.symbol(o)
  private val ws = lexer.space.whiteSpace

  private def parens[A](p: => Parsley[A]): Parsley[A] =
    op("(") *> p <* op(")")

  private def braces[A](p: => Parsley[A]): Parsley[A] =
    op("{") *> p <* op("}")

  private def brackets[A](p: => Parsley[A]): Parsley[A] =
    op("[") *> p <* op("]")

  private def many[A](p: => Parsley[A]): Parsley[List[A]] = Parsley.many(p)
  private def some[A](p: => Parsley[A]): Parsley[List[A]] = Parsley.some(p)

  // Forward-reference wrapper: delays evaluation of `p` until parse time.
  // pure(()) succeeds immediately, then flatMap evaluates p lazily.
  private def fwd[A](p: => Parsley[A]): Parsley[A] =
    pure(()).flatMap(_ => p)

  // ---- Type parsing ----
  // Using fwd() to break circular lazy val initialization

  private lazy val typeExpr: Parsley[SType] =
    chain.right1[SType](atomicType)(op("->") #> ((l: SType, r: SType) => SType.STArrow(l, r)))

  private lazy val atomicType: Parsley[SType] = choice(
    piType,
    forallType,
    uniType,
    typeVarOrApp,
    parens(fwd(typeExpr)),
  )

  /** ∀ (x : A), B — Unicode forall as Pi type
   *  Also handles `∀ x : A, B` without parens.
   */
  private lazy val forallType: Parsley[SType] =
    atomic(parsley.character.char('∀') <* ws) *> choice(
      // ∀ (x : A), B
      parens(identifier.flatMap { name =>
        op(":") *> fwd(typeExpr).map { dom => (name, dom) }
      }).flatMap { case (name, dom) =>
        op(",") *> fwd(typeExpr).map { cod =>
          SType.STPi(name, dom, cod)
        }
      },
      // ∀ x : A, B
      identifier.flatMap { name =>
        op(":") *> fwd(typeExpr).flatMap { dom =>
          op(",") *> fwd(typeExpr).map { cod =>
            SType.STPi(name, dom, cod)
          }
        }
      },
    )

  /** Pi(x: A, B) */
  private lazy val piType: Parsley[SType] =
    keyword("Pi") *> op("(") *> identifier.flatMap { name =>
      op(":") *> fwd(typeExpr).flatMap { dom =>
        op(",") *> fwd(typeExpr).flatMap { cod =>
          op(")") #> SType.STPi(name, dom, cod)
        }
      }
    }

  /** Type, Type0, Type1, ...
    * Since "Type" is a hard keyword, "Type0" is parsed as identifier "Type0".
    * We handle both: keyword "Type" alone and identifiers matching Type\d.
    */
  private lazy val uniType: Parsley[SType] = choice(
    // Type0, Type1, ... as identifiers (since Type is hard keyword, Type0 is a valid identifier)
    atomic(identifier.collect {
      case s if s.startsWith("Type") && s.drop(4).forall(_.isDigit) && s.length > 4 =>
        SType.STUni(s.drop(4).toInt)
    }),
    // Bare "Type" keyword
    keyword("Type") #> SType.STUni(0),
  )

  /** Simple type variable or multi-argument type application: Nat, List(A), Vec(A, n)
   *
   *  Multi-argument applications are desugared to left-associative binary STApp:
   *  Vec(A, n) → STApp(STApp(STVar("Vec"), STVar("A")), STVar("n"))
   *
   *  Type arguments may be either types (A: Type) or value expressions used as
   *  indices (e.g. Regex.eps, List.nil in Accept(Regex.eps, List.nil)).
   *  We first try parsing as a typeExpr; if that fails, fall back to simpleExpr
   *  wrapped in STExpr.
   */
  private lazy val typeOrExprArg: Parsley[SType] =
    // Parse as simpleExpr (handles both plain names and dotted constructors like Regex.eps).
    // SEVar is treated as a type variable (STVar); other exprs are wrapped in STExpr.
    // Arrow types in arg position require explicit parens: write (A -> B) not A -> B.
    fwd(simpleExpr).map {
      case SExpr.SEVar(name) => SType.STVar(name)
      case other             => SType.STExpr(other)
    }

  private lazy val typeVarOrApp: Parsley[SType] =
    identifier.flatMap { name =>
      option(parens(sepBy1(typeOrExprArg, op(",")))).map {
        case Some(args) =>
          args.foldLeft(SType.STVar(name): SType)((fn, arg) => SType.STApp(fn, arg))
        case None =>
          SType.STVar(name)
      }
    }

  // ---- Expression parsing ----

  /** Integer literal: optional minus sign followed by digits.
    * Consumes trailing whitespace to behave like a proper token.
    */
  private lazy val intLiteral: Parsley[SExpr] =
    atomic(
      option(parsley.character.char('-')).flatMap { neg =>
        some(digit).map { digits =>
          val n = digits.mkString.toInt
          SExpr.SEInt(if neg.isDefined then -n else n)
        }
      }
    ) <* ws

  private lazy val expr: Parsley[SExpr] = choice(
    matchExpr,
    lamExpr,
    scalaLamExpr,
    letExpr,
    ifExpr,
    listLiteral,
    addExpr,
  )

  /** (x: T) => body  or  (x: T, y: T) => body — Scala-style lambda without `fun` */
  private lazy val scalaLamExpr: Parsley[SExpr] =
    atomic(paramList.flatMap { params =>
      op("=>") *> fwd(expr).map { body =>
        SExpr.SELam(params, body)
      }
    })

  /** if cond then e1 else e2 */
  private lazy val ifExpr: Parsley[SExpr] =
    keyword("if") *> fwd(expr).flatMap { cond =>
      keyword("then") *> fwd(expr).flatMap { thenBr =>
        keyword("else") *> fwd(expr).map { elseBr =>
          SExpr.SEIf(cond, thenBr, elseBr)
        }
      }
    }

  /** let x := value; body  — local let binding */
  private lazy val letExpr: Parsley[SExpr] =
    keyword("let") *> anyIdentifier.flatMap { name =>
      op(":=") *> fwd(expr).flatMap { value =>
        op(";") *> fwd(expr).map { body =>
          SExpr.SELet(name, value, body)
        }
      }
    }

  /** List literal: [] or [e1, e2, e3] */
  private lazy val listLiteral: Parsley[SExpr] =
    atomic(op("[") *> sepBy(fwd(expr), op(",")) <* op("]")).map(SExpr.SEList.apply)

  /** Additive level: left-associative + */
  private lazy val addExpr: Parsley[SExpr] =
    chain.left1(mulExpr)(
      op("+") #> ((l: SExpr, r: SExpr) => SExpr.SInfix(l, "+", r))
    )

  /** Multiplicative level: left-associative * (tighter than +).
   *  Uses appOrVarOrConOrMatch so `e match { ... }` binds tighter than operators.
   */
  private lazy val mulExpr: Parsley[SExpr] =
    chain.left1(appOrVarOrConOrMatch)(
      op("*") #> ((l: SExpr, r: SExpr) => SExpr.SInfix(l, "*", r))
    )

  /** match e { case ... => ... } */
  private lazy val matchExpr: Parsley[SExpr] =
    keyword("match") *> simpleExpr.flatMap { scrutinee =>
      braces(many(matchCase)).map { cases =>
        SExpr.SEMatch(scrutinee, cases)
      }
    }

  private lazy val matchCase: Parsley[SMatchCase] =
    keyword("case") *> matchPattern.flatMap { case (ctor, bindings) =>
      op("=>") *> fwd(expr).map { body =>
        SMatchCase(ctor, bindings, body)
      }
    }

  /** Match pattern: Nat.zero, Nat.succ(k)
   *  The type/ctor names may be user-defined identifiers including hardKeywords,
   *  so `rawIdentifier` is used.  Binding names stay as regular identifiers.
   */
  private lazy val matchPattern: Parsley[(String, List[String])] =
    rawIdentifier.flatMap { first =>
      option(op(".") *> rawIdentifier).flatMap {
        case Some(ctorName) =>
          val fullName = s"$first.$ctorName"
          option(parens(sepBy(identifier, op(",")))).map { bindings =>
            (fullName, bindings.getOrElse(Nil))
          }
        case None =>
          option(parens(sepBy(identifier, op(",")))).map { bindings =>
            (first, bindings.getOrElse(Nil))
          }
      }
    }

  /** fun (x: A) => body */
  private lazy val lamExpr: Parsley[SExpr] =
    keyword("fun") *> some(paramDecl).flatMap { params =>
      op("=>") *> fwd(expr).map { body =>
        SExpr.SELam(params, body)
      }
    }

  /** Primary expression: identifier, or identifier.identifier (constructor/field access).
   *  Supports backtick-quoted identifiers for names with spaces/symbols/Unicode.
   *  Constructors consume their first arg-list here; plain variables do not.
   */
  private lazy val primaryExpr: Parsley[SExpr] =
    anyIdentifier.flatMap { first =>
      option(op(".") *> rawIdentifier).flatMap {
        case Some(ctorName) =>
          // Constructor: parse first arg-list inline (Type.ctor(...) syntax)
          option(parens(sepBy(fwd(expr), op(",")))).map { argsOpt =>
            SExpr.SECon(first, ctorName, argsOpt.getOrElse(Nil))
          }
        case None =>
          // Plain variable: no args consumed here
          pure(SExpr.SEVar(first))
      }
    }

  /** Parenthesized expression or type ascription: (e) or (e : T) */
  private lazy val parenExprOrAscr: Parsley[SExpr] =
    atomic(op("(") *> fwd(expr).flatMap { inner =>
      choice(
        // (e : T) — type ascription
        op(":") *> fwd(typeExpr).map { tpe =>
          SExpr.SEAscr(inner, tpe)
        } <* op(")"),
        // (e) — just a parenthesized expression
        op(")") #> inner,
      )
    })

  /** Variable, constructor (Type.ctor), function application, or integer literal.
   *  Supports chained calls: f(a)(b, c) = App(App(f, [a]), [b, c]).
   *  Constructors (Nat.succ) consume their first arg-list in primaryExpr;
   *  further chains like Ctor(x)(y) are parsed as App(Ctor(x), [y]).
   */
  private lazy val appOrVarOrCon: Parsley[SExpr] =
    (intLiteral <|> parenExprOrAscr <|> primaryExpr).flatMap { head =>
      // Parse the first optional arg-list for non-constructor primaries,
      // then any number of additional chains.
      head match
        case SExpr.SEVar(name) =>
          // Variable: first (args) makes it an application; then chain more.
          option(parens(sepBy(fwd(expr), op(",")))).flatMap {
            case None       => many(parens(sepBy(fwd(expr), op(",")))).map { rest =>
              rest.foldLeft(SExpr.SEVar(name): SExpr) { (acc, args) => SExpr.SEApp(acc, args) }
            }
            case Some(args0) =>
              many(parens(sepBy(fwd(expr), op(",")))).map { rest =>
                rest.foldLeft(SExpr.SEApp(SExpr.SEVar(name), args0): SExpr) { (acc, args) =>
                  SExpr.SEApp(acc, args)
                }
              }
          }
        case _ =>
          // Constructor or other: chain any further (args) calls.
          many(parens(sepBy(fwd(expr), op(",")))).map { argLists =>
            argLists.foldLeft(head) { (acc, args) => SExpr.SEApp(acc, args) }
          }
    }

  /** Simple expression (no match, for scrutinee position — avoids infinite loop) */
  private lazy val simpleExpr: Parsley[SExpr] = appOrVarOrCon

  /** Expression with optional postfix match: `e match { cases }` (Scala 3 style).
   *  Used in the mul/add chain so that `f(x) match { ... }` works at any precedence.
   */
  private lazy val appOrVarOrConOrMatch: Parsley[SExpr] =
    appOrVarOrCon.flatMap { base =>
      option(keyword("match") *> braces(many(fwd(matchCase)))).map {
        case Some(cases) => SExpr.SEMatch(base, cases)
        case None        => base
      }
    }

  // ---- Parameter declarations ----

  private lazy val paramDecl: Parsley[SParam] =
    parens(rawIdentifier.flatMap { name =>
      // Use propType so equality propositions (p: f(x) = y) work as param types.
      op(":") *> fwd(propType).map { tpe =>
        SParam(name, tpe)
      }
    })

  private lazy val paramList: Parsley[List[SParam]] =
    parens(sepBy(
      rawIdentifier.flatMap { name =>
        // Use propType so equality propositions (p: f(x) = y) work as param types.
        op(":") *> fwd(propType).map { tpe =>
          SParam(name, tpe)
        }
      },
      op(","),
    ))

  // ---- Declaration parsing ----

  private lazy val decl: Parsley[SDecl] = choice(
    attrDecl,
    checkDecl,
    enumDecl,
    inductiveDecl,
    theoremDecl,
    defspecDecl,
    defDecl,
    traitDecl,
    structureDecl,
    givenDecl,
    instanceDecl,
    operatorDecl,
  )

  /** Raw identifier: letters/digits/underscore, NOT keyword-filtered.
   *  Used for attribute names like @[simp] where "simp" is a keyword. */
  private lazy val rawIdentifier: Parsley[String] =
    atomic(stringOfSome(c => c.isLetterOrDigit || c == '_')) <* ws

  /** @[attr] decl */
  private lazy val attrDecl: Parsley[SDecl] =
    op("@") *> brackets(rawIdentifier).flatMap { attr =>
      fwd(decl).map { inner =>
        SDecl.SAttr(attr, inner)
      }
    }

  /** #check expr */
  private lazy val checkDecl: Parsley[SDecl] =
    atomic(op("#") *> keyword("check") *> fwd(expr)).map(SDecl.SCheck.apply)

  /** theorem — alias for defspec */
  private lazy val theoremDecl: Parsley[SDecl] =
    keyword("theorem") *> anyIdentifier.flatMap { name =>
      option(typeParamList).map(_.getOrElse(Nil)).flatMap { tparams =>
        option(paramList).map(_.getOrElse(Nil)).flatMap { params =>
          (op(":") *> propType).flatMap { prop =>
            braces(proof).map { prf =>
              SDecl.SDefspec(name, tparams ++ params, prop, prf)
            }
          }
        }
      }
    }

  /** inductive Name(params)(indices) { case ctor1: T case ctor2(a: A): T }
   *
   *  The first `paramList` group captures uniform parameters (e.g. A: Type).
   *  The optional second `paramList` group captures index parameters (e.g. n: Nat)
   *  that vary per constructor (as in indexed inductive types / inductive families).
   */
  private lazy val inductiveDecl: Parsley[SDecl] =
    keyword("inductive") *> identifier.flatMap { name =>
      option(paramList).map(_.getOrElse(Nil)).flatMap { params =>
        option(paramList).map(_.getOrElse(Nil)).flatMap { indices =>
          braces(many(ctorDecl)).map { ctors =>
            SDecl.SInductive(name, params, ctors, indices)
          }
        }
      }
    }

  /** enum Name[A, B] { case ctor1: T ... } — Scala 3-style alias for inductive */
  private lazy val enumDecl: Parsley[SDecl] =
    keyword("enum") *> identifier.flatMap { name =>
      option(typeParamList).map(_.getOrElse(Nil)).flatMap { tparams =>
        braces(many(ctorDecl)).map { ctors =>
          SDecl.SInductive(name, tparams, ctors)
        }
      }
    }

  private lazy val ctorDecl: Parsley[SCtor] =
    keyword("case") *> rawIdentifier.flatMap { name =>
      option(parens(sepBy(
        rawIdentifier.flatMap { pn =>
          // Use propType so equality constraints like `valid: f(x) = Bool.true` work.
          op(":") *> fwd(propType).map { pt =>
            SParam(pn, pt)
          }
        },
        op(","),
      ))).map(_.getOrElse(Nil)).flatMap { argParams =>
        op(":") *> fwd(typeExpr).map { retTpe =>
          SCtor(name, argParams, retTpe)
        }
      }
    }

  /** Optional type parameter list: [A] or [A, B, C] */
  private lazy val typeParamList: Parsley[List[SParam]] =
    brackets(sepBy1(identifier, op(","))).map { names =>
      names.map(n => SParam(n, SType.STUni(0)))
    }

  /** def name[A, B](params): retTpe = body  OR  def name(params): retTpe { body } */
  private lazy val defDecl: Parsley[SDecl] =
    keyword("def") *> anyIdentifier.flatMap { name =>
      option(typeParamList).map(_.getOrElse(Nil)).flatMap { tparams =>
        paramList.flatMap { params =>
          (op(":") *> fwd(typeExpr)).flatMap { retTpe =>
            defBody.map { body =>
              SDecl.SDef(name, tparams ++ params, retTpe, body)
            }
          }
        }
      }
    }

  private lazy val defBody: Parsley[SExpr] = choice(
    op("=") *> fwd(expr),
    braces(fwd(expr)),
  )

  /** defspec name[A](params): prop { by tactic }
    * Params are optional: `defspec foo: 1 == 1 { by trivial }` is valid. */
  private lazy val defspecDecl: Parsley[SDecl] =
    keyword("defspec") *> anyIdentifier.flatMap { name =>
      option(typeParamList).map(_.getOrElse(Nil)).flatMap { tparams =>
        option(paramList).map(_.getOrElse(Nil)).flatMap { params =>
          (op(":") *> propType).flatMap { prop =>
            braces(proof).map { prf =>
              SDecl.SDefspec(name, tparams ++ params, prop, prf)
            }
          }
        }
      }
    }

  /** Proposition type: either an equality `expr = expr` (or `expr == expr`) or a regular type.
    * For equality, both sides are parsed as expressions (to allow `plus(n, Nat.zero) = n`).
    * `==` is accepted as a synonym for `=` in proposition position.
    */
  private lazy val propType: Parsley[SType] = choice(
    // Try equality with == (must come before = to avoid ambiguity)
    atomic(fwd(expr).flatMap { lhs =>
      op("==") *> fwd(expr).map { rhs =>
        SType.STEq(lhs, rhs)
      }
    }),
    // Try equality with =
    atomic(fwd(expr).flatMap { lhs =>
      op("=") *> fwd(expr).map { rhs =>
        SType.STEq(lhs, rhs)
      }
    }),
    // Fallback: regular type
    fwd(typeExpr),
  )

  /** Proof: by tactic | direct expr */
  private lazy val proof: Parsley[SProof] = choice(
    keyword("by") *> tactic.map(SProof.SBy.apply),
    fwd(expr).map(SProof.STerm.apply),
  )

  // ---- Tactic parsing ----

  private lazy val tactic: Parsley[STactic] = choice(
    keyword("trivial") #> STactic.STrivial,
    keyword("triv") #> STactic.STriv,
    keyword("rfl") #> STactic.SRfl,
    keyword("decide") #> STactic.SDecide,
    keyword("sorry") #> STactic.SSorry,
    keyword("skip") #> STactic.SSkip,
    keyword("assumption") #> STactic.SAssumption,
    keyword("contradiction") #> STactic.SContradiction,
    keyword("try") *> fwd(tactic).map(STactic.STry.apply),
    keyword("first") *> some(op("|") *> fwd(tactic)).map(STactic.SFirst.apply),
    keyword("repeat") *> fwd(tactic).map(STactic.SRepeat.apply),
    keyword("all_goals") *> fwd(tactic).map(STactic.SAllGoals.apply),
    keyword("assume") *> some(identifier).map(STactic.SAssume.apply),
    keyword("intro") *> some(identifier).map(STactic.SAssume.apply),
    keyword("intros") *> some(identifier).map(STactic.SAssume.apply),
    keyword("apply") *> fwd(expr).map(STactic.SApply.apply),
    keyword("exact") *> fwd(expr).map(STactic.SExact.apply),
    atomic(keyword("simplify") *> brackets(sepBy(identifier, op(",")))).map(STactic.SSimplify.apply),
    keyword("simplify") #> STactic.SSimplify(Nil),
    atomic(keyword("simp") *> brackets(sepBy(identifier, op(",")))).map(STactic.SSimp.apply),
    keyword("simp") #> STactic.SSimp(Nil),
    haveTactic,
    atomic(keyword("rw") *> brackets(sepBy1(identifier, op(",")))).map(STactic.SRw.apply),
    rewriteTactic,
    calcTactic,
    seqTactic,
    casesTactic,
    inductionTactic,
    keyword("split") #> STactic.SSplit,
    keyword("constructor") #> STactic.SConstructor,
    keyword("left") #> STactic.SLeft,
    keyword("right") #> STactic.SRight,
    keyword("tauto") #> STactic.STauto,
    keyword("use") *> fwd(expr).map(STactic.SUse.apply),
    keyword("exists") *> fwd(expr).map(STactic.SExists.apply),
    keyword("by_contra") *> identifier.map(STactic.SByContra.apply),
    obtainTactic,
    specializeTactic,
  )

  /** { t1; t2; t3 } — tactic sequence */
  private lazy val seqTactic: Parsley[STactic] =
    braces(sepBy1(fwd(tactic), op(";"))).map { tactics =>
      if tactics.length == 1 then tactics.head
      else STactic.SSeq(tactics)
    }

  /** have h : T = { proof } ; cont_tactic
    * T can be a regular type or an equality proposition.
    */
  private lazy val haveTactic: Parsley[STactic] =
    keyword("have") *> identifier.flatMap { name =>
      op(":") *> fwd(propType).flatMap { tpe =>
        op("=") *> braces(fwd(proof)).flatMap { prf =>
          op(";") *> fwd(tactic).map { cont =>
            STactic.SHave(name, tpe, prf, cont)
          }
        }
      }
    }

  /** rewrite [lemma1, lemma2] or rewrite lemma */
  private lazy val rewriteTactic: Parsley[STactic] =
    keyword("rewrite") *> choice(
      brackets(sepBy1(identifier, op(","))).map(STactic.SRewrite.apply),
      identifier.map(n => STactic.SRewrite(List(n))),
    )

  /** calc { step1 step2 ... } where step = (expr | _) = expr { proof } */
  private lazy val calcTactic: Parsley[STactic] =
    keyword("calc") *> braces(some(calcStep)).map(STactic.SCalc.apply)

  private lazy val calcStep: Parsley[SCalcStep] =
    choice(
      // _ = expr { proof }
      atomic(identifier.filter(_ == "_") *> op("=") *> fwd(expr).flatMap { rhs =>
        braces(fwd(proof)).map { prf =>
          SCalcStep(None, rhs, prf)
        }
      }),
      // expr = expr { proof }
      atomic(fwd(expr).flatMap { lhs =>
        op("=") *> fwd(expr).flatMap { rhs =>
          braces(fwd(proof)).map { prf =>
            SCalcStep(Some(lhs), rhs, prf)
          }
        }
      }),
    )

  /** cases x { case c1 => t1 ... } — case split without induction hypothesis */
  private lazy val casesTactic: Parsley[STactic] =
    keyword("cases") *> identifier.flatMap { varName =>
      braces(many(tacticCase)).map { cases =>
        STactic.SCases(varName, cases)
      }
    }

  private lazy val inductionTactic: Parsley[STactic] =
    keyword("induction") *> identifier.flatMap { varName =>
      braces(many(tacticCase)).map { cases =>
        STactic.SInduction(varName, cases)
      }
    }

  /** obtain [a, b] := h ; cont — destruct a single-constructor hypothesis */
  private lazy val obtainTactic: Parsley[STactic] =
    keyword("obtain") *> brackets(sepBy(identifier, op(","))).flatMap { bindings =>
      op(":=") *> identifier.flatMap { hypName =>
        op(";") *> fwd(tactic).map { cont =>
          STactic.SObtain(bindings, hypName, cont)
        }
      }
    }

  /** specialize h arg1 arg2 ; cont — apply a hypothesis to arguments */
  private lazy val specializeTactic: Parsley[STactic] =
    keyword("specialize") *> identifier.flatMap { hypName =>
      some(fwd(simpleExpr)).flatMap { args =>
        op(";") *> fwd(tactic).map { cont =>
          STactic.SSpecialize(hypName, args, cont)
        }
      }
    }

  private lazy val tacticCase: Parsley[STacticCase] =
    keyword("case") *> identifier.flatMap { ctorName =>
      many(atomic(identifier.filter(n => !isKeyword(n) && n != "=>"))).flatMap { bindings =>
        op("=>") *> tactic.map { tac =>
          STacticCase(ctorName, bindings, tac)
        }
      }
    }

  // ---- structure / instance / operator declarations ----

  /** structure Name { field: Type  field2: Type2 } */
  private lazy val structureDecl: Parsley[SDecl] =
    keyword("structure") *> identifier.flatMap { name =>
      braces(many(structureField)).map { fields =>
        SDecl.SStructure(name, fields)
      }
    }

  /** trait Name { field: Type ... } — Scala 3-style alias for structure */
  private lazy val traitDecl: Parsley[SDecl] =
    keyword("trait") *> identifier.flatMap { name =>
      braces(many(structureField)).map { fields =>
        SDecl.SStructure(name, fields)
      }
    }

  /** field: Type  (inside structure body) */
  private lazy val structureField: Parsley[SParam] =
    rawIdentifier.flatMap { name =>
      op(":") *> fwd(typeExpr).map { tpe =>
        SParam(name, tpe)
      }
    }

  /** instance instName: StructName { field = expr  field2 = expr2 } */
  private lazy val instanceDecl: Parsley[SDecl] =
    keyword("instance") *> identifier.flatMap { name =>
      op(":") *> identifier.flatMap { structName =>
        braces(many(instanceBinding)).map { bindings =>
          SDecl.SInstance(name, structName, bindings)
        }
      }
    }

  /** given instName: StructName { field = expr ... } — Scala 3-style alias for instance */
  private lazy val givenDecl: Parsley[SDecl] =
    keyword("given") *> identifier.flatMap { name =>
      op(":") *> identifier.flatMap { structName =>
        braces(many(instanceBinding)).map { bindings =>
          SDecl.SInstance(name, structName, bindings)
        }
      }
    }

  /** field = expr  (inside instance body) */
  private lazy val instanceBinding: Parsley[(String, SExpr)] =
    atomic(rawIdentifier).flatMap { name =>
      op("=") *> fwd(expr).map { body =>
        (name, body)
      }
    }

  /** operator (x: T1) + (y: T2): T3 = body
   *  Type annotations on both operands are mandatory. */
  private lazy val operatorDecl: Parsley[SDecl] =
    keyword("operator") *> paramDecl.flatMap { lhsParam =>
      infixOpSymbol.flatMap { opSym =>
        paramDecl.flatMap { rhsParam =>
          (op(":") *> fwd(typeExpr)).flatMap { retTpe =>
            defBody.map { body =>
              SDecl.SOperator(lhsParam, opSym, rhsParam, retTpe, body)
            }
          }
        }
      }
    }

  /** Supported infix operator symbols in operator declarations and expressions. */
  private lazy val infixOpSymbol: Parsley[String] = choice(
    op("+") #> "+",
    op("*") #> "*",
  )

  private val keywords = Set(
    "inductive", "enum", "def", "defspec", "theorem", "case", "match", "by",
    "trivial", "triv", "rfl", "decide", "assume", "intro", "intros",
    "apply", "exact", "simplify", "simp",
    "induction", "sorry", "fun", "Pi", "Type",
    "structure", "trait", "instance", "given", "operator",
    "have", "rewrite", "rw", "calc", "let", "in",
    "try", "first", "repeat", "all_goals", "skip",
    "assumption", "contradiction", "cases",
    "if", "then", "else",
    "split", "constructor", "left", "right",
    "use", "exists", "by_contra", "tauto",
    "obtain", "specialize",
  )

  private def isKeyword(s: String): Boolean = keywords.contains(s)

  // ---- Program (multiple decls) ----

  private lazy val program: Parsley[List[SDecl]] =
    ws *> many(decl) <* Parsley.eof

  // ---- Public API ----

  def parseType(input: String): Either[String, SType] =
    runParser(ws *> typeExpr <* Parsley.eof, input)

  def parseExpr(input: String): Either[String, SExpr] =
    runParser(ws *> expr <* Parsley.eof, input)

  def parseDecl(input: String): Either[String, SDecl] =
    runParser(ws *> decl <* Parsley.eof, input)

  def parseTactic(input: String): Either[String, STactic] =
    runParser(ws *> tactic <* Parsley.eof, input)

  def parseProgram(input: String): Either[String, List[SDecl]] =
    runParser(program, input)

  private def runParser[A](p: Parsley[A], input: String): Either[String, A] =
    p.parse(input) match
      case parsley.Success(value) => Right(value)
      case parsley.Failure(err)   => Left(err.toString)

end Parser
