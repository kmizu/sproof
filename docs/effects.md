# Effects and IO Boundary

This document describes the recommended effect model for `sproof`.

## Principle

- Keep the trusted kernel pure.
- Represent effectful programs as data (`IO` script AST).
- Execute effects only in untrusted runtime code (outside kernel verification).

This preserves soundness: kernel checking never depends on host-side side effects.

## Current model

`stdlib/Effect.sproof` provides a free-monad-like script type:

- `IO.pure(value)`      : pure result
- `IO.read_int`         : request runtime input
- `IO.print_int(value)` : request runtime output
- `IO.bind(action, k)`  : sequencing node

Helpers:

- `io_pure`, `io_read`, `io_print`, `io_bind`, `io_then`
- `echo_once` as a basic script example

## Why this fits theorem proving

- `IO` terms are ordinary inductive terms, so they can be type-checked and reasoned about.
- No impure reduction rule is added to definitional equality.
- Runtime interpretation is explicit and isolated at extraction/execution boundary.

## Runtime execution

After extracting to Scala, run scripts with an external interpreter in trusted host code.
If `IO` (with constructors `pure/read_int/print_int/bind`) is present, the extractor emits
`object IORuntime` automatically.

`IORuntime` provides:

- `run(script: IO): Int` (uses stdin/stdout)
- `runWith(script, readInt, printInt): Int` (dependency-injected handlers)
- `runWithTrace(script, inputs): Trace` (pure test-double style runner)

The interpreter can map:

- `read_int` to `scala.io.StdIn.readLine()`
- `print_int` to `println`
- `bind` to sequential composition

This split keeps proof checking pure while still enabling practical side effects in deployed programs.

## Quick check

You can verify runtime emission locally:

```bash
./scripts/check_effect_runtime.sh
```
