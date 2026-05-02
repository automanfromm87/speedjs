#!/usr/bin/env bash
# demo_trace.sh — small plan-act run that exercises the trace machinery.
#
# Goal is intentionally tiny: ~$0.05 budget, 1-2 min wall, ~30 frames.
# Designed to produce one of every kind: phase / plan_step / iteration
# / llm_call / tool_call. Drag the resulting NDJSON into
# tools/trace_viewer.html to see the tree.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
[ -f "$SCRIPT_DIR/.env.local" ] && . "$SCRIPT_DIR/.env.local"

PROJECT_DIR="${SPEEDJS_PROJECT:-/tmp/trace-demo}"
TRACE_FILE="${SPEEDJS_TRACE:-/tmp/demo-trace.ndjson}"
LOG_FILE="${SPEEDJS_LOG:-/tmp/demo-trace.log}"

rm -rf "$PROJECT_DIR" /tmp/demo-trace-mem
: > "$TRACE_FILE"
: > "$LOG_FILE"

echo "→ project: $PROJECT_DIR"
echo "→ trace:   $TRACE_FILE"
echo "→ logs:    $LOG_FILE"
echo "→ goal:    3-task fizzbuzz: write, run, verify"
echo

dune exec speedjs -- \
  --plan \
  --budget 0.5 \
  --max-iters 15 \
  --working-dir "$PROJECT_DIR" \
  --memory-dir /tmp/demo-trace-mem \
  --trace-file "$TRACE_FILE" \
  --log-file "$LOG_FILE" \
  --no-loop-guard \
  "Three tasks. (1) Write a Python file ${PROJECT_DIR}/fizzbuzz.py that defines fizzbuzz(n) returning a list of strings for 1..n, prints fizzbuzz(15) when run as main. (2) Run it via bash and capture the output. (3) Verify the output starts with '1','2','Fizz','4','Buzz' and report a one-line summary."

echo
echo "──────────── TRACE STRUCTURE ────────────"
echo "frames: $(wc -l < "$TRACE_FILE")"

echo
echo "kinds (count + total ms):"
jq -s '
  group_by(.kind)
  | map({
      kind: .[0].kind,
      count: length,
      ms: ([.[]|.duration_ms]|add|floor)
    })
' "$TRACE_FILE"

echo
echo "root frames (should be 1: 'phase plan_act'):"
jq -c 'select(.parent_id == null) | {kind, name}' "$TRACE_FILE"

echo
echo "tree skeleton (depth 2, parent → child kinds):"
jq -s '
  . as $all
  | map(select(.parent_id == null)) as $roots
  | $roots
  | map({
      root: (.kind + " " + .name),
      children: ([$all[] | select(.parent_id == ($roots[0].id))] | map(.kind + " " + .name) | unique)
    })
' "$TRACE_FILE"

echo
echo "→ open viewer:    file://$SCRIPT_DIR/tools/trace_viewer.html"
echo "→ load file:      $TRACE_FILE"
echo "→ cost summary:   grep summary $LOG_FILE"
