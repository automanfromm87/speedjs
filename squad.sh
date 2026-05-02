#!/usr/bin/env bash
# squad.sh — 4-agent engineering team workflow with REAL file I/O.
#
#   PM → Design → Loop_until(qa.pass) { Fullstack → QA }
#
# Fullstack writes code to $WORKING_DIR via write_file + runs tests via
# bash. QA reads code via view_file + re-runs tests via bash. QA's
# pass/fail is based on actual test exit codes, not LLM intuition.
#
# After completion, $WORKING_DIR contains a real project you can:
#   cd $WORKING_DIR && npm test         # (or whatever stack was chosen)
#
# File ops are sandboxed to $WORKING_DIR. bash is unrestricted — the
# agent prompt warns it not to wreck the host, but for paranoid runs
# use a container or VM.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
[ -f "$SCRIPT_DIR/.env.local" ] && . "$SCRIPT_DIR/.env.local"

LOG_FILE="${SPEEDJS_LOG:-/tmp/speedjs-squad.log}"
: > "$LOG_FILE"

# Default working dir is unique per run. Override via SPEEDJS_WORKING_DIR.
WORKING_DIR="${SPEEDJS_WORKING_DIR:-/tmp/speedjs-squad-$(date +%Y%m%d-%H%M%S)}"
mkdir -p "$WORKING_DIR"

GOAL="${1:-Build a small in-memory URL shortener service. POST /shorten {url} returns {code, expires_at}; GET /:code redirects with 302 (or 404 if expired/missing). Each entry has a configurable TTL (default 1 hour). Use Node.js + Express + TypeScript. Include a tiny test that exercises both endpoints (using node:test or vitest).}"
MAX_ITERS="${MAX_ITERS:-3}"

RESUME_FLAG=""
if [ -n "${RESUME:-}" ]; then
  RESUME_FLAG="--resume"
fi

echo "→ logs:        $LOG_FILE   (tail -f to follow)"
echo "→ working_dir: $WORKING_DIR  (real code lives here)"
echo "→ goal:        $GOAL"
echo "→ max_iters:   $MAX_ITERS  (fullstack/qa retry cap)"
if [ -n "$RESUME_FLAG" ]; then
  echo "→ pair:        (RESUME) reviewer → loop(engineer ↔ reviewer)"
else
  echo "→ pair:        engineer ↔ reviewer  (build/review loop)"
fi
echo

dune exec squad -- \
  --max-iters "$MAX_ITERS" \
  --working-dir "$WORKING_DIR" \
  --log-file "$LOG_FILE" \
  $RESUME_FLAG \
  "$GOAL"

echo
echo "→ project on disk: $WORKING_DIR"
echo "  cd $WORKING_DIR && ls -la"
