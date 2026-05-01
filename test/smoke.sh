#!/usr/bin/env bash
# Smoke test for protections — verifies each feature actually triggers.
# Run from repo root after `dune build`.
set -u
BIN=_build/default/bin/main.exe
TMP=/tmp/speedjs-smoke
rm -rf "$TMP" && mkdir -p "$TMP"

pass=0
fail=0

check() {
  local label="$1"
  local needle="$2"
  local file="$3"
  if grep -q -- "$needle" "$file"; then
    echo "  ✓ $label"
    pass=$((pass + 1))
  else
    echo "  ✗ $label  (missing: $needle in $file)"
    fail=$((fail + 1))
  fi
}

echo "[1/6] default: cache + active line + summary"
$BIN "What is 7*8?" > "$TMP/t1.out" 2> "$TMP/t1.err"
check "active line shown" "[active]" "$TMP/t1.err"
check "tool-truncation in default" "tool-truncation" "$TMP/t1.err"
check "prompt-cache in default" "prompt-cache" "$TMP/t1.err"
check "walltime in default" "walltime(1800s)" "$TMP/t1.err"
check "loop-guard in default" "loop-guard" "$TMP/t1.err"
check "cache stats reported" "cache: write=" "$TMP/t1.err"
check "cache read non-zero" "read=[1-9]" "$TMP/t1.err"

echo
echo "[2/6] --budget triggers Budget_exceeded"
$BIN --budget 0.0001 "Calculate 12*13, 17*19, 23*29, 31*37, 41*43 and sum" \
  > "$TMP/t2.out" 2> "$TMP/t2.err" || true
check "Budget_exceeded surfaced" "budget exceeded" "$TMP/t2.out"

echo
echo "[3/6] --walltime 5 + sleep 30 triggers Walltime_exceeded"
$BIN --walltime 5 "Use bash to sleep 30 seconds" \
  > "$TMP/t3.out" 2> "$TMP/t3.err" || true
check "Walltime_exceeded surfaced" "walltime exceeded" "$TMP/t3.out"

echo
echo "[4/6] --debug-request shows cache_control + context_management"
$BIN --debug-request "What is 1+1?" > "$TMP/t4.out" 2> "$TMP/t4.err"
check "cache_control in request" '"cache_control"' "$TMP/t4.err"
check "context_management in request" "context_management" "$TMP/t4.err"
check "ephemeral marker present" "ephemeral" "$TMP/t4.err"

echo
echo "[5/6] tape replay zero-cost"
rm -f "$TMP/tape.jsonl"
$BIN --tape "$TMP/tape.jsonl" "What is 5*6?" > "$TMP/t5a.out" 2> "$TMP/t5a.err"
$BIN --tape "$TMP/tape.jsonl" "What is 5*6?" > "$TMP/t5b.out" 2> "$TMP/t5b.err"
check "second run uses 0 LLM calls" "0 LLM calls" "$TMP/t5b.err"

echo
echo "[6/6] dune unit tests (mock-based protections)"
if dune test --display quiet 2>&1 | tail -10 | grep -q "All tests passed"; then
  echo "  ✓ all 8 unit tests pass (loop guard, truncation, etc.)"
  pass=$((pass + 1))
else
  echo "  ✗ unit tests failed"
  fail=$((fail + 1))
fi

echo
echo "------------------------------------------------------------"
echo "Result: $pass passed, $fail failed"
echo "Logs in: $TMP"
[ "$fail" -eq 0 ]
