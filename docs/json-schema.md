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
  "severity": "error",
  "phase": "proof",
  "message": "Proof of 'bad' failed: ..."
}
```

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
