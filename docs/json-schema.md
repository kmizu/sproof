# `sproof check --json` Schema v2

This document defines the stable JSON contract for:

```bash
sproof check --json <file.sproof>
```

## Versioning

- `schemaVersion` is mandatory.
- Current stable version: `2`.
- Backward-compatible additions (new optional fields) are allowed within v2.
- Breaking changes require a new major schema version.

## Top-Level Shape (v2)

All responses include the same keys:

```json
{
  "schemaVersion": 2,
  "ok": true,
  "phase": "check",
  "file": "examples/nat.sproof",
  "result": { "inductives": 1, "defs": 1, "defspecs": 4 },
  "warnings": [],
  "diagnostics": [],
  "checks": [],
  "error": null
}
```

Fields:

- `schemaVersion`: number, currently `2`
- `ok`: boolean
- `phase`: `"parse" | "elab" | "proof" | "check"`
- `file`: input filename
- `result`: object on success, `null` on failure
- `warnings`: warning objects (for example sorry-related warnings)
- `diagnostics`: structured error diagnostics
- `checks`: per-`#check` results
- `error`: string on failure, `null` on success

## `warnings[]`

Each warning is an object:

```json
{
  "severity": "warning",
  "code": "sorry",
  "message": "warning: 'unfinished' uses sorry (1 occurrence(s)) — proof is unsound"
}
```

## `diagnostics[]`

Failure responses include at least one diagnostic:

```json
{
  "phase": "proof",
  "code": "proof_error",
  "message": "Proof error",
  "range": {"start": {"line": 3, "column": 1}, "end": {"line": 3, "column": 30}},
  "expected": null,
  "actual": null,
  "hint": "Inspect proof-state and apply a tactic that matches the current goal.",
  "goalTarget": "plus(n, zero) = n",
  "tacticSuggestions": ["induction", "simplify [ih]", "assumption", "sorry"]
}
```

Fields:

- `phase`: `"parse" | "elab" | "proof"` — where the error occurred
- `code`: `"parse_error" | "type_mismatch" | "unknown_variable" | "unknown_type" | "proof_error" | "error"`
- `message`: short human-readable summary
- `range`: source location (`null` if unavailable)
- `expected` / `actual`: type mismatch details (`null` if not applicable)
- `hint`: actionable advice (`null` if not applicable)
- `goalTarget`: human-readable rendering of the failing goal (e.g. `"plus(n, zero) = n"`); `null` for non-proof errors or when not extractable
- `tacticSuggestions`: ordered list of tactics most likely to close the goal; `null` for non-proof errors

## `checks[]`

When the source contains `#check` declarations, each entry has:

- `expr`: expression text (or elaborated term)
- `ok`: boolean
- `type`: string or `null`
- `error`: string or `null`

Example:

```json
{
  "expr": "Nat.zero",
  "ok": true,
  "type": "Nat",
  "error": null
}
```

## Compatibility Policy

- v2 consumers should ignore unknown fields.
- Producers must keep existing field names and meanings stable within v2.
- Field type changes, removals, or semantic reinterpretation require v3.
