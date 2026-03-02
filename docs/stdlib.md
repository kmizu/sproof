# stdlib v1 layout and naming

This document defines stdlib v1 layout for `Nat`, `List`, `Vec`, `Bool`, `Dictionary`, and `Effect`.

## Layout

- `stdlib/Nat.sproof`
- `stdlib/List.sproof`
- `stdlib/Vec.sproof`
- `stdlib/Bool.sproof`
- `stdlib/Dictionary.sproof`
- `stdlib/Effect.sproof`

Each file is self-contained and checker-runnable by itself.

## Naming conventions

- File names use PascalCase domain names (`Nat`, `List`, `Vec`, `Bool`, `Effect`).
- Definitions and lemmas use `snake_case`.
- Core operations keep short canonical names (`plus`, `append`, `length`, `concat`, `not`, `and`, `or`).
- Baseline lemmas use descriptive suffixes (`_nil`, `_left`, `_right`).

## Compatibility policy

- Existing lemma names in v1 are stable.
- New lemmas can be added in a backward-compatible way.
- Removing or renaming existing exported lemmas requires a major stdlib revision.
