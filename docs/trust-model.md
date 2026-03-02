# Trust Model and Trusted Computing Base (TCB)

This document defines the soundness boundary of `sproof`.

## Safety Principle

`sproof` is sound only if the trusted kernel is sound.
Bugs outside the kernel may cause proof search failures, bad UX, or false negatives, but they must not be able to accept an invalid proof term as valid.

## Trusted vs Untrusted Components

Trusted (TCB):

- `kernel/` (`sproof.kernel.Kernel`)
- Core type-checking semantics used by kernel verification (`checker.Bidirectional`, term/eval semantics)

Untrusted (must be re-checked by kernel):

- `tactic/` (proof-term generation)
- `checker/` orchestration logic
- `syntax/` parser/elaborator
- `cli/` command-line and JSON formatting
- `vscode-sproof/` editor integration
- `cli/agent` automated proof search

## Kernel Responsibilities

The kernel is the final authority for accepting completed proofs.

- Verify a candidate proof term against its claimed proposition.
- Reject type-incorrect terms and ill-typed equality proofs.
- Return typed failures (no implicit success/fallback path).

Kernel entrypoints:

- `Kernel.check(ctx, proof, claimedType): Either[TypeError, Unit]`
- `Kernel.infer(ctx, term): Either[TypeError, Term]`

The final accept/reject decision for defspec proofs must pass through `Kernel.check`.

## Kernel API Contract

Callers must provide:

- A complete context for local assumptions/definitions.
- A candidate proof term.
- The claimed type/proposition for that term.

Expected behavior:

- `Right(())`: proof accepted by the trusted kernel.
- `Left(TypeError)`: proof rejected; caller must fail verification.

Non-goals of the kernel:

- Proof search, heuristics, simplification strategy, or UX diagnostics polish.

## Threat Model

Out of scope:

- Malicious runtime/JVM or compromised build infrastructure.
- Supply-chain attacks on dependencies.

In scope:

- Parser/elaborator bugs.
- Tactic engine bugs.
- CLI/agent bugs that generate wrong terms.
- Refactoring mistakes that bypass kernel verification.

## Failure Modes

- Parser/elaborator bug: may build wrong term, but kernel should reject invalid proofs.
- Tactic bug: may produce invalid candidate term, but kernel should reject.
- CLI bug: may print misleading output, but must not bypass kernel final check.
- Kernel bug: may accept invalid proof term (soundness break, highest severity).

## Soundness Review Checklist

When reviewing changes, verify:

1. Is every successful proof acceptance path still gated by `Kernel.check`?
2. Did any new shortcut bypass kernel verification?
3. Did kernel logic change for equality/type checking behavior?
4. Are new negative regression tests present for known-unsound patterns?
