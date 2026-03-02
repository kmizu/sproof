#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT_DIR"

TMP_OUT="$(mktemp)"
trap 'rm -f "$TMP_OUT"' EXIT

# Extract Effect stdlib and verify runtime helpers are emitted.
sbt "cli/run extract stdlib/Effect.sproof" >"$TMP_OUT"

grep -q "object IORuntime" "$TMP_OUT"
grep -q "def run(script: IO): Int" "$TMP_OUT"
grep -q "def runWith(" "$TMP_OUT"
grep -q "def runWithTrace(script: IO, inputs: List\\[Int\\]): Trace" "$TMP_OUT"

echo "Effect runtime extraction check: OK"
