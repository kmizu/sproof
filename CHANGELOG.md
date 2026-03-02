# Changelog

## [0.2.0] - 2026-03-02

### Added
- Richer tactic and proof authoring support in examples (`nat`, `int`, `list`, `vec`).
- Proof-state S-expression output on proof failures for tooling/LLM consumption.
- JSON output mode for `check --json`.
- VS Code extension with syntax highlighting, snippets, hover docs, and outline support.
- Scala Native build + CI smoke-test workflow.

### Changed
- Updated release documentation and v0.2 release checklist.
- Refined `examples/vec.sproof` to satisfy structural recursion checks.

### Known limitations
- Some advanced recursive patterns still require argument ordering that is friendly to the structural termination checker.
- `sorry` remains available as an explicit unsound placeholder and should not be used in production proofs.

