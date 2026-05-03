#!/usr/bin/env bash
# Tiny plan-act run for quickly testing trace HTML auto-open and other
# end-to-end features. Cheap (~$0.02) and fast (~20-40s) — pick this
# over chaos.sh / dev.sh when iterating on the runtime / observability.
#
# Env vars (all optional):
#   SPEEDJS_LOG=path      log file (default /tmp/speedjs-small.log)
#   SPEEDJS_PROJECT=dir   project root (default /tmp/notes-small)
#   SPEEDJS_TRACE=path    trace ndjson (default /tmp/speedjs-small-trace.ndjson)
#   SPEEDJS_NO_OPEN=1     don't auto-open the html report

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
[ -f "$SCRIPT_DIR/.env.local" ] && . "$SCRIPT_DIR/.env.local"

LOG_FILE="${SPEEDJS_LOG:-/tmp/speedjs-small.log}"
PROJECT_DIR="${SPEEDJS_PROJECT:-/tmp/notes-small}"
TRACE_FILE="${SPEEDJS_TRACE:-/tmp/speedjs-small-trace.ndjson}"

OPEN_FLAG=()
[ -z "${SPEEDJS_NO_OPEN:-}" ] && OPEN_FLAG=(--trace-open)

rm -rf "$PROJECT_DIR" /tmp/speedjs-small-memory
: > "$TRACE_FILE"

echo "→ project: $PROJECT_DIR"
echo "→ logs:    $LOG_FILE   (tail -f to follow live)"
echo "→ trace:   $TRACE_FILE  (html report → ${TRACE_FILE%.*}.html)"
[ -z "${SPEEDJS_NO_OPEN:-}" ] && echo "→ on exit: HTML report auto-opens in your browser"
echo

dune exec speedjs -- \
  --plan \
  --budget 0.50 \
  --walltime 300 \
  --max-iters 30 \
  --working-dir "$PROJECT_DIR" \
  --log-file "$LOG_FILE" \
  --memory-dir "/tmp/speedjs-small-memory" \
  --trace-file "$TRACE_FILE" \
  "${OPEN_FLAG[@]}" \
  "Compute the result of 7 * 6 using the calculator tool. Write the
   numeric answer (just the digits, no other text) to $PROJECT_DIR/answer.txt.
   Then read the file back with bash 'cat' to verify, and produce a
   one-line confirmation message including the value."
