#!/usr/bin/env bash
# trace.sh — smoke test for the structured trace capture (--trace-file).
#
# Runs a tiny task that exercises both LLM calls and tool dispatches,
# then prints summary stats from the resulting NDJSON. The point isn't
# the agent's answer — it's that the trace file ends up with sensible
# parent-child structure, durations, costs, and per-call tokens.
#
# Env vars (optional):
#   SPEEDJS_TRACE=path  trace destination (default /tmp/speedjs-trace.ndjson)
#   SPEEDJS_LOG=path    log destination   (default /tmp/speedjs-trace.log)

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
[ -f "$SCRIPT_DIR/.env.local" ] && . "$SCRIPT_DIR/.env.local"

TRACE_FILE="${SPEEDJS_TRACE:-/tmp/speedjs-trace.ndjson}"
LOG_FILE="${SPEEDJS_LOG:-/tmp/speedjs-trace.log}"

: > "$TRACE_FILE"
: > "$LOG_FILE"

echo "→ trace: $TRACE_FILE"
echo "→ logs:  $LOG_FILE"
echo "→ goal:  use bash to compute 12 * 47 and tell me"
echo

dune exec speedjs -- \
  --trace-file "$TRACE_FILE" \
  --log-file "$LOG_FILE" \
  --max-iters 8 \
  --no-loop-guard \
  "What is 12 * 47? Use the bash tool to compute it, then tell me the answer."

echo
echo "──────────────── TRACE ANALYSIS ────────────────"
echo "frames captured:"
wc -l < "$TRACE_FILE"

echo
echo "first 5 frames (compact):"
head -5 "$TRACE_FILE" | jq -c '{
  id: .id[0:6],
  parent: (.parent_id // "-" | .[0:6]),
  kind,
  name,
  ms: (.duration_ms | floor),
  ok,
  tokens_in: .tokens.input,
  tokens_out: .tokens.output,
  cost_usd: (.cost_delta_usd * 10000 | floor / 10000)
}'

echo
echo "summary by kind:"
jq -s '
  group_by(.kind)
  | map({
      kind: .[0].kind,
      count: length,
      total_ms: ([.[] | .duration_ms] | add | floor),
      total_cost_usd: ([.[] | .cost_delta_usd] | add * 10000 | floor / 10000),
      total_tokens_in: ([.[] | .tokens.input] | add),
      total_tokens_out: ([.[] | .tokens.output] | add),
      failures: ([.[] | select(.ok == false)] | length)
    })
' "$TRACE_FILE"

echo
echo "tool call breakdown:"
jq -c 'select(.kind == "tool_call") | {name, ms: (.duration_ms | floor), ok, in: (.input_summary | .[0:60])}' "$TRACE_FILE"

echo
echo "→ raw trace at $TRACE_FILE — try:"
echo "    jq -s 'group_by(.kind)' $TRACE_FILE"
echo "    jq 'select(.ok == false)' $TRACE_FILE"
echo
echo "→ open the HTML viewer:"
echo "    open $SCRIPT_DIR/tools/trace_viewer.html"
echo "    (drop $TRACE_FILE into the file picker)"
