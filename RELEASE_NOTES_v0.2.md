# sproof v0.2 Release Notes

Release date: 2026-03-02

## Highlights

- Stabilized core proof workflow (`check`, REPL, extraction, tactics).
- Added/expanded end-to-end example coverage (`nat`, `int`, `list`, `vec`).
- Improved tooling story:
  - machine-readable `check --json`
  - VS Code extension packaging flow (`vscode-sproof`)
  - Scala Native binary CI smoke checks

## Migration Notes (from v0.1)

- Core CLI commands are unchanged.
- `check --json` is available for automation/tool integration.
- VS Code extension version bumped to `0.2.0`.
- `examples/vec.sproof` updated so `concat` uses argument ordering compatible with structural recursion checking.

## Known Limitations

- Structural termination checking is conservative: certain recursive definitions may require refactoring argument order.
- Tactic automation is intentionally minimal; complex proofs may require explicit intermediate lemmas (`have`) and rewrites.
- `sorry` is intentionally available but unsound.

## Release Artifacts

- Source release tag: `v0.2.0`
- JVM build via sbt
- Scala Native binary artifact from CI: `sproof-cli-linux-amd64`
- VS Code extension package built from `vscode-sproof/`

