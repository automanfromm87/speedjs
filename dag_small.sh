#!/usr/bin/env bash
# DAG + cascade demo. Two INDEPENDENT branches converge into a final
# verification task — exactly the shape where DAG mode beats sequential:
#
#   branch A          branch B
#   ┌────────┐        ┌────────┐
#   │compute │        │compute │
#   │ 7 * 6  │        │ 9 + 4  │
#   └────┬───┘        └────┬───┘
#        ▼                 ▼
#   ┌────────┐        ┌────────┐
#   │write   │        │write   │
#   │ a.txt  │        │ b.txt  │
#   └────┬───┘        └────┬───┘
#        └────────┬────────┘
#                 ▼
#           ┌──────────┐
#           │final     │  reads both files,
#           │report    │  cascade-fails if either branch fails
#           └──────────┘
#
# Chaos rate is set so one branch is likely to die — when that happens
# you should see in the log:
#   [event] task_failed N attempt=0 err=cascade: skipped because depends_on=K failed earlier
# meaning DRIVE_DAG_FLOW SHORT-CIRCUITED THE DEPENDENT WITHOUT CALLING LLM.
# That's the win.

set -e
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
[ -f "$SCRIPT_DIR/.env.local" ] && . "$SCRIPT_DIR/.env.local"

LOG_FILE="${SPEEDJS_LOG:-/tmp/speedjs-dag.log}"
PROJECT_DIR="${SPEEDJS_PROJECT:-/tmp/notes-dag}"
TRACE_FILE="${SPEEDJS_TRACE:-/tmp/speedjs-dag-trace.ndjson}"
CHAOS_LLM="${CHAOS_LLM:-0.20}"
CHAOS_TOOL="${CHAOS_TOOL:-0.10}"

OPEN_FLAG=()
[ -z "${SPEEDJS_NO_OPEN:-}" ] && OPEN_FLAG=(--trace-open)

rm -rf "$PROJECT_DIR" /tmp/speedjs-dag-memory
: > "$TRACE_FILE"

echo "→ project:    $PROJECT_DIR"
echo "→ logs:       $LOG_FILE   (tail -f to follow)"
echo "→ trace:      $TRACE_FILE  → ${TRACE_FILE%.*}.html"
echo "→ chaos:      llm=$CHAOS_LLM  tool=$CHAOS_TOOL"
echo "→ analyze:    grep -aE '^\\s+[0-9]+\\.|cascade|task_(started|failed|completed)' \$LOG_FILE"
echo

dune exec speedjs -- \
  --plan \
  --plan-dag \
  --budget 1.00 \
  --walltime 600 \
  --max-iters 30 \
  --working-dir "$PROJECT_DIR" \
  --log-file "$LOG_FILE" \
  --memory-dir "/tmp/speedjs-dag-memory" \
  --trace-file "$TRACE_FILE" \
  --chaos-llm "$CHAOS_LLM" \
  --chaos-tool "$CHAOS_TOOL" \
  "${OPEN_FLAG[@]}" \
  "Run TWO INDEPENDENT computation branches and a final report:

   Branch A: Use the calculator to compute 7 * 6. Then write the
   numeric result (just the digits) to $PROJECT_DIR/a.txt.

   Branch B: Use the calculator to compute 9 + 4. Then write the
   numeric result (just the digits) to $PROJECT_DIR/b.txt.

   Final report: Use bash to cat both files. Print one line
   containing both values and a confirmation that both branches
   completed.

   CRITICAL: Branches A and B are completely independent — neither
   reads the other's file or depends on the other's calculator
   result. Your plan MUST reflect this independence (no false
   dependencies between A-tasks and B-tasks). The final report
   depends on BOTH branches' file outputs.

   Use depends_on to express the structure honestly."
