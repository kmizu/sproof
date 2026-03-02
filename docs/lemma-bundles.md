# Reusable Lemma Bundles

This document defines reusable lemma bundles for common proof maintenance workflows.

## Available bundles

- `nat-simplify`
- `list-core`
- `bool-normalize`
- `dictionary-core`
- `relation-core`

Bundle manifests live under `stdlib/bundles/*.bundle`.

## Bundle contents

### nat-simplify

- `plus_zero_left`
- `plus_succ_left`
- `plus_zero_right`
- `refl`

### list-core

- `length_nil`
- `append_nil_left`
- `map_nil`

### bool-normalize

- `not_not`
- `and_true`
- `or_false`

### dictionary-core

- `eq_nat_refl`
- `lookup_empty`
- `member_empty`
- `remove_empty`
- `size_empty`
- `size_insert`
- `lookup_insert_zero`
- `lookup_insert_one`

### relation-core

- `contains_empty`
- `insert_empty`
- `union_empty_left`
- `subset_empty_left`
- `remove_empty`
- `size_empty`
- `eq_nat_refl`
- `contains_insert_zero_zero_empty`

## Compatibility policy

- Bundle names are stable in v1.
- Existing lemma entries are append-only in minor updates.
- Removing or renaming an existing bundle entry requires a major stdlib revision.
