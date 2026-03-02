# Release Checklist v0.2

Target release date: 2026-03-02

## Documentation and Versioning

- [x] Update root changelog (`CHANGELOG.md`).
- [x] Add release notes (`RELEASE_NOTES_v0.2.md`).
- [x] Add migration notes and release links in `README.md`.
- [x] Bump VS Code extension version to `0.2.0`.

## Regression and Smoke Checks

- [x] `sbt test`
- [x] `sbt "cli/run check examples/nat.sproof"`
- [x] `sbt "cli/run check examples/int.sproof"`
- [x] `sbt "cli/run check examples/list.sproof"`
- [x] `sbt "cli/run check examples/vec.sproof"`
- [x] `cd vscode-sproof && npm ci && npm run compile`

## CI and Artifacts

- [x] Verify CI workflows include JVM tests + native smoke checks.
- [ ] Ensure benchmark artifact/report job exists in CI (tracked separately in #20).
- [x] Confirm native binary artifact name/path (`sproof-cli-linux-amd64`).

## Known Limitations

- [x] Document structural recursion checker constraints.
- [x] Document `sorry` unsoundness caveat.
