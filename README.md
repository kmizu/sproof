# sproof

**A Proof Assistant for Programmers**

sproof is a dependently-typed theorem prover written in Scala 3. It aims to make formal verification accessible to programmers who already know Scala, Java, Rust, or C++.

[![CI](https://github.com/kmizu/sproof/actions/workflows/ci.yml/badge.svg)](https://github.com/kmizu/sproof/actions/workflows/ci.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

---

## Why sproof?

Traditional proof assistants (Coq, Lean, Agda) haven't reached mainstream programmers — not just because dependent types are hard, but because **the syntax acts as an unnecessary barrier**.

```coq
(* Coq — readable without prior knowledge? *)
Fixpoint plus (n m : nat) : nat :=
  match n with
  | O => m
  | S n' => S (plus n' m)
  end.

Theorem plus_O_n : forall n : nat, 0 + n = n.
Proof.
  intros n. simpl. reflexivity.
Qed.
```

```scala
// sproof — readable if you know Scala/Java/Rust
def plus(n: Nat, m: Nat): Nat {
  match n {
    case Nat.zero    => m
    case Nat.succ(k) => Nat.succ(plus(k, m))
  }
}

defspec plus_zero_right(n: Nat): plus(n, Nat.zero) = n {
  by induction n {
    case zero      => trivial
    case succ k ih => simplify [ih]
  }
}
```

sproof's design principle: **keep only the essential complexity**.

- **Learning cost = type theory concepts only** — syntax adds no extra burden
- **Uniform brace `{ }` syntax** — familiar to anyone who knows Java, Rust, or Scala
- **Full English tactic names** — `trivial`, `induction`, `simplify` (no cryptic abbreviations)
- **Short aliases available** — `triv`, `induct`, `simp` (only self-evident abbreviations)
- **Helpful error messages** — point to the next step, not internal jargon

---

## Comparison

|                    | Coq        | Lean 4       | sproof                  |
|--------------------|------------|--------------|-------------------------|
| Implementation     | OCaml      | C++          | **Scala 3**             |
| Type theory        | CIC        | CIC          | **Predicative CIC**     |
| Syntax             | Math-first | Improved DSL | **Scala-like, braces**  |
| Extraction target  | OCaml/Haskell | Lean itself | **Scala 3 (default)**  |
| Native binary      | —          | —            | **Scala Native**        |
| Reflexivity tactic | `rfl`      | `rfl`        | **`trivial`**           |
| Intro tactic       | `intros`   | `intro`      | **`assume`**            |

---

## Quick Start

```bash
# Clone and build
git clone https://github.com/kmizu/sproof
cd sproof
sbt cli/run

# Check a proof file
sbt "cli/run check examples/nat.sproof"
```

### Output

```
OK: examples/nat.sproof — 1 inductive(s), 1 definition(s), 4 defspec(s)
```

JSON output schema is documented in [docs/json-schema.md](docs/json-schema.md).

---

## Language Guide

### Inductive Types

```scala
inductive Nat {
  case zero: Nat
  case succ(n: Nat): Nat
}

inductive List(A: Type) {
  case nil: List(A)
  case cons(head: A, tail: List(A)): List(A)
}

inductive Bool {
  case true:  Bool
  case false: Bool
}
```

### Function Definitions

```scala
// Block body
def plus(n: Nat, m: Nat): Nat {
  match n {
    case Nat.zero    => m
    case Nat.succ(k) => Nat.succ(plus(k, m))
  }
}

// Expression body
def id(x: Nat): Nat = x
```

### Specification Definitions (`defspec`)

`defspec` expresses the Curry-Howard correspondence directly:
**proposition = type**, **proof = program**.

```
defspec name(params): proposition { proof }
```

Symmetry with `def`:

```scala
def     foo(n: Nat): Nat  =         { n }          // function: program for a type
defspec bar(n: Nat): P(n) { ... }        // spec: proof program for a proposition
```

If the proof program has the wrong type, it is rejected — just like a type error in regular code.

### Tactic Proofs

```scala
// Trivial: both sides reduce to the same term
defspec plus_zero_left(m: Nat): plus(Nat.zero, m) = m {
  by trivial
}

// Induction with induction hypothesis
defspec plus_zero_right(n: Nat): plus(n, Nat.zero) = n {
  by induction n {
    case zero      => trivial
    case succ k ih => simplify [ih]
  }
}
```

### Term Proofs (direct Curry-Howard terms)

```scala
defspec refl_intro(n: Nat): n = n {
  by induction n {
    case zero   => trivial
    case succ k => trivial
  }
}
```

---

## Tactic Reference

| Tactic                   | Alias      | Effect                                                        |
|--------------------------|------------|---------------------------------------------------------------|
| `trivial`                | `triv`     | Close goal when both sides are definitionally equal           |
| `simplify [f, g, ...]`   | `simp`     | Rewrite using listed lemmas, then check trivially             |
| `induction x { ... }`   | `induct x` | Split on constructors of `x`; adds IH for recursive cases    |
| `assumption x`           | `assume x` | Introduce `x : A` into context, shifting goal to `B`         |
| `apply f`                | —          | Unify goal with return type of `f`; generate subgoals        |
| `cases x { ... }`        | —          | Case-split without induction hypothesis                       |
| `have h : T = proof`     | —          | Introduce a local lemma                                       |
| `calc { ... }`           | —          | Chain equational reasoning steps                              |
| `ring`                   | —          | Discharge ring-equation goals automatically                   |
| `sorry`                  | —          | Placeholder for incomplete proofs (emits a warning)           |

**Tip for beginners**: Write full names first (`trivial`, `induction`, `simplify`). Switch to aliases only once you understand what they mean.

---

## Coq Syntax Comparison

| Concept              | Coq                     | sproof                             |
|----------------------|-------------------------|------------------------------------|
| Inductive type       | `Inductive Nat : Set :=` | `inductive Nat {`                 |
| Function definition  | `Fixpoint plus ...`      | `def plus ...`                    |
| Theorem              | `Theorem plus_zero ...`  | `defspec plus_zero ... {`|
| Begin proof          | `Proof.`                 | `{`                               |
| End proof            | `Qed.`                   | `}`                               |
| Reflexivity          | `reflexivity` / `rfl`    | `trivial`                         |
| Simplify             | `simpl` / `simp`         | `simplify` / `simp`               |
| Introduce hypothesis | `intros`                 | `assume`                          |
| Induction            | `induction n`            | `induction n {`                   |

---

## Scala 3 Extraction

```bash
sbt "cli/run extract examples/nat.sproof --output Nat.scala"
```

Proofs (propositions) are erased at runtime; only the computational content remains.

```scala
// sproof
def plus(n: Nat, m: Nat): Nat { ... }
defspec plus_zero_right(n: Nat): plus(n, Nat.zero) = n { ... }

// Generated Scala 3
def plus(n: Nat, m: Nat): Nat = ...
def plus_zero_right(n: Nat): Unit = ()   // proof erased
```

---

## Architecture

```
sproof/
├── core/       # Term ADT, De Bruijn substitution, typing context
├── eval/       # Normalization by Evaluation (NbE)
├── checker/    # Bidirectional type checking
├── tactic/     # TacticM monad, built-in tactics
├── syntax/     # Parsley-based parser, surface AST, pretty-printer
├── extract/    # Scala 3 code generation
├── kernel/     # Trusted kernel (<500 lines, auditable)
└── cli/        # REPL and file loader
```

## Trust Model

Soundness boundary and trusted computing base (TCB) are documented in [docs/trust-model.md](docs/trust-model.md).

Pipeline note:
- checker/tactic generates candidate proof terms
- final accept/reject decision is centralized in `kernel` validation

**Type theory**: Predicative CIC (Calculus of Inductive Constructions)
- Universe hierarchy: `Type`, `Type1`, `Type2`, ...
- Inductive types + fixpoints (recursive functions)
- Curry-Howard isomorphism (proof = program)

---

## Scala Native (native binary)

sproof compiles to a self-contained native binary via [Scala Native](https://scala-native.org/). No JVM required at runtime.

### Prerequisites

```bash
# Ubuntu / WSL2
sudo apt-get install clang lld libunwind-dev
```

### Build

```bash
# Compile all modules for native (requires clang)
sbt cliNative/nativeLink

# Run the native binary
./cli-native/target/scala-3.3.6/sproof-cli-native-out check examples/nat.sproof
```

### Performance

The native binary uses `releaseFast` + `LTO.thin` + `immix` GC by default. For maximum performance (slower to link):

```sbt
// in build.sbt, change releaseFast → releaseFull in nativeLinkSettings
```

### Checking native compilation (without linking)

```bash
# Compile all native modules (only requires Scala Native sbt plugin, no LLVM):
sbt cliNative/compile
```

---

## sbt Plugin

See [sbt-sproof](sbt-sproof/README.md) for integrating sproof into an sbt build.

```sbt
// project/plugins.sbt
addSbtPlugin("io.sproof" % "sbt-sproof" % "0.1.0")

// build.sbt
enablePlugins(SproofPlugin)
```

```bash
sbt sproofCheck    # Type-check all .sproof files
sbt sproofExtract  # Extract to Scala 3 source (runs before compile)
sbt sproofRepl     # Interactive REPL
```

---

## License

MIT
