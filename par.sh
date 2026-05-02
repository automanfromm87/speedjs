#!/usr/bin/env bash
# par.sh — smoke test for parallel_delegate (real Anthropic API).
#
# Asks the agent to fan out 3 independent calculator tasks via the
# parallel_delegate tool. Watch the log for:
#
#   [parallel_delegate] spawning 3 domain(s)
#   [sub:0] ... interleaving with [sub:1] ... and [sub:2] ...
#
# If sub-agents really run in parallel (OCaml 5 Domains), the wall
# clock should be ~max(sub:0, sub:1, sub:2), NOT their sum.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
[ -f "$SCRIPT_DIR/.env.local" ] && . "$SCRIPT_DIR/.env.local"

LOG_FILE="${SPEEDJS_LOG:-/tmp/speedjs-par.log}"
: > "$LOG_FILE"

echo "→ logs:  $LOG_FILE   (tail -f to follow)"
echo "→ goal:  three parallel calculator tasks via parallel_delegate"
echo

dune exec speedjs -- \
  --max-iters 12 \
  --max-retries 3 \
  --max-subagent-depth 2 \
  --log-file "$LOG_FILE" \
  "Use the parallel_delegate tool to run these THREE independent tasks at the same time:

  Task A: Use the calculator tool to compute (12 * 25) + 7. Return only the numeric answer.
  Task B: Use the calculator tool to compute 256 / 8 - 3. Return only the numeric answer.
  Task C: Use the calculator tool to compute (5 + 3) * (7 - 2). Return only the numeric answer.

  Each task should issue exactly one calculator call and submit its result. After parallel_delegate returns the three results, give a one-line final answer listing them as 'A=X, B=Y, C=Z'."
