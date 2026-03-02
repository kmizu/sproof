# Incremental Checking Strategy

`sproof` uses a file-scoped, stage-aware cache during repeated checks in the same JVM process.

## Stages and cache keys

1. `parse` stage
- key: `sourceHash` (SHA-256 of raw file text)
- value: parsed declarations + `declHash`

2. `elab` stage
- key: `declHash` (SHA-256 of parsed declaration sequence)
- value: elaboration result + `programHash`

3. `proof` stage
- key: `programHash` (SHA-256 of elaborated program units)
- value: proof-check outcome (success or failure) and warnings

## Invalidation behavior

- If `sourceHash` changes:
  - parse cache is re-evaluated.
- If resulting `declHash` is unchanged (e.g. comment/whitespace edits):
  - elaboration/proof caches are reused.
- If `declHash` changes:
  - elaboration and proof caches are invalidated.
- If resulting `programHash` changes:
  - proof cache is invalidated.

## Safety fallback

Whenever a key mismatch or missing cache entry is detected, sproof falls back to full re-check from that stage onward. This guarantees correctness even when cache reuse is uncertain.

