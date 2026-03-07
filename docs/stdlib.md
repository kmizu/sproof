# stdlib v1 layout and naming

This document defines stdlib v1 layout for `Nat`, `List`, `Vec`, `Bool`, `Relation`, `Dictionary`, `Effect`, `Option`, `Either`, and `Pair`.

## Layout

- `stdlib/Nat.sproof`
- `stdlib/List.sproof`
- `stdlib/Vec.sproof`
- `stdlib/Bool.sproof`
- `stdlib/Relation.sproof`
- `stdlib/Effect.sproof`
- `stdlib/Dictionary.sproof`
- `stdlib/Option.sproof`
- `stdlib/Either.sproof`
- `stdlib/Pair.sproof`

Each file is self-contained and checker-runnable by itself.

## Naming conventions

- File names use PascalCase domain names (`Nat`, `List`, `Vec`, `Bool`, `Relation`, `Dictionary`, `Effect`, `Option`, `Either`, `Pair`).
- Definitions and lemmas use `snake_case`.
- Core operations keep short canonical names (`plus`, `append`, `length`, `concat`, `not`, `and`, `or`, `map_option`, `get_or_else`, `map_either`, `fst`, `snd`).
- Baseline lemmas use descriptive suffixes (`_nil`, `_left`, `_right`, `_none`, `_some`, `_mk`).

## Module Summary

| Module | Types | Key operations | Defspecs |
|--------|-------|---------------|---------|
| Nat | Nat | plus | plus_zero_left, plus_zero_right, plus_succ_left, plus_succ_right |
| List | Nat, List | length, append, reverse, map, filter, fold_left, fold_right | length_nil, append_nil_left/right, append_assoc, map_cons, length_append |
| Vec | Nat, Vec(A)(n) | concat | concat_nil |
| Bool | Bool | not, and, or | not_not, and_true, or_false |
| Relation | Nat, Bool, Relation | contains, insert, union, subset, remove, size | contains_empty, union_empty_left, … |
| Effect | Nat, IO | io_pure, io_bind, io_then, echo_once | io_then_desugars |
| Dictionary | Nat, Bool, MaybeNat, Dictionary | insert, lookup, member, remove, size | lookup_empty, lookup_insert_zero, … |
| Option | Nat, Bool, Option | map_option, get_or_else, is_some, is_none | map_option_none/some, get_or_else_none/some, is_some/is_none variants |
| Either | Nat, Bool, Either | map_either, is_left, is_right, get_left, get_right | map_either_left/right, is_left/right variants, get_left/right |
| Pair | Nat, Pair | fst, snd, swap | fst_mk, snd_mk, swap_mk |

## Compatibility policy

- Existing lemma names in v1 are stable.
- New lemmas can be added in a backward-compatible way.
- Removing or renaming existing exported lemmas requires a major stdlib revision.
