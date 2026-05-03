#!/usr/bin/env bash
# Chaos engineering runner — same Notes-app target as dev.sh, but with
# probabilistic failure injection on the LLM and tool chains. Verifies
# that the resilience layers (LLM retry, tool retry, plan-act recovery,
# workflow [recover]) actually fire on real failures.
#
# Env vars (all optional):
#   CHAOS=N             single rate applied to BOTH llm + tool (default 0.05)
#   CHAOS_LLM=N         override LLM-only rate (default = CHAOS)
#   CHAOS_TOOL=N        override tool-only rate (default = CHAOS)
#   CHAOS_SEED=N        deterministic replay (default 42)
#   RESUME=1            keep state, continue from last (post-chaos) crash
#   SPEEDJS_LOG=path    log file (default /tmp/speedjs-chaos.log)
#   SPEEDJS_TAPE=path   tape file (default /tmp/notes-chaos.tape)
#   SPEEDJS_PROJECT=dir project root (default /tmp/notes-chaos)
#   SPEEDJS_TRACE=path  trace NDJSON (default /tmp/speedjs-chaos-trace.ndjson)
#                       set to 'off' to disable
#   PLANNER_MODEL       override planner model (e.g. claude-opus-4-5)
#   EXECUTOR_MODEL      override per-task executor model (carries ~93% of tokens)
#   RECOVERY_MODEL      override recovery planner model
#   SUMMARIZER_MODEL    override final synthesizer model
#
# Tips for reading the run:
#   tail -f /tmp/speedjs-chaos.log | grep -E 'chaos|retry|Recovery'
#   jq 'select(.ok == false)' /tmp/speedjs-chaos-trace.ndjson

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
[ -f "$SCRIPT_DIR/.env.local" ] && . "$SCRIPT_DIR/.env.local"

CHAOS="${CHAOS:-0.05}"
CHAOS_LLM="${CHAOS_LLM:-$CHAOS}"
CHAOS_TOOL="${CHAOS_TOOL:-$CHAOS}"
CHAOS_SEED="${CHAOS_SEED:-42}"

LOG_FILE="${SPEEDJS_LOG:-/tmp/speedjs-chaos.log}"
PROJECT_DIR="${SPEEDJS_PROJECT:-/tmp/notes-chaos}"
SKILLS_DIR="$SCRIPT_DIR/skills"
TRACE_FILE="${SPEEDJS_TRACE:-/tmp/speedjs-chaos-trace.ndjson}"

# NOTE: --tape (Checkpoint replay) intentionally NOT used here.
# Tape records "replay LLM calls 1..N from start"; chaos.sh wants
# fresh non-determinism every run AND uses plan_state.json for
# RESUME (skip-done-tasks semantics, different from tape replay).
# The two don't compose: a chaos run that crashed and advanced
# plan_state.json has a DIFFERENT LLM call sequence on RESUME, so
# tape entries misalign and crash with "tape misalignment".

TRACE_FLAG=()
if [ "$TRACE_FILE" != "off" ]; then
  TRACE_FLAG=(--trace-file "$TRACE_FILE")
fi

MODEL_FLAGS=()
[ -n "$PLANNER_MODEL"    ] && MODEL_FLAGS+=(--planner-model    "$PLANNER_MODEL")
[ -n "$EXECUTOR_MODEL"   ] && MODEL_FLAGS+=(--executor-model   "$EXECUTOR_MODEL")
[ -n "$RECOVERY_MODEL"   ] && MODEL_FLAGS+=(--recovery-model   "$RECOVERY_MODEL")
[ -n "$SUMMARIZER_MODEL" ] && MODEL_FLAGS+=(--summarizer-model "$SUMMARIZER_MODEL")

if [ -z "${RESUME:-}" ]; then
  rm -rf "$PROJECT_DIR" /tmp/speedjs-chaos-memory
  [ "$TRACE_FILE" != "off" ] && : > "$TRACE_FILE"
fi

echo "→ chaos:   llm=$CHAOS_LLM  tool=$CHAOS_TOOL  seed=$CHAOS_SEED"
echo "→ project: $PROJECT_DIR"
echo "→ skills:  $SKILLS_DIR"
echo "→ logs:    $LOG_FILE   (tail -f to follow live)"
echo "→ resume:  /tmp/speedjs-chaos-memory  (RESUME=1 ./chaos.sh skips done tasks)"
[ ${#MODEL_FLAGS[@]} -gt 0 ] && echo "→ models:  ${MODEL_FLAGS[*]}"
[ "$TRACE_FILE" != "off" ] && \
  echo "→ trace:   $TRACE_FILE  (jq 'select(.ok==false)' to find chaos-injected failures)"
echo

# Higher caps than dev.sh — chaos triggers more retries / recovery
# cycles, and each retry is one more step (Llm_started tick). 5%
# injection ≈ 30-50% step inflation; bump max-steps proportionally.
# walltime + budget remain the real backstops.
dune exec speedjs -- \
  --plan \
  --budget 15.0 \
  --walltime 5400 \
  --max-iters 200 \
  --max-steps 500 \
  --max-retries 5 \
  --skills-dir "$SKILLS_DIR" \
  --working-dir "$PROJECT_DIR" \
  --log-file "$LOG_FILE" \
  "${TRACE_FLAG[@]}" \
  --memory-dir "/tmp/speedjs-chaos-memory" \
  --chaos-seed "$CHAOS_SEED" \
  --chaos-llm "$CHAOS_LLM" \
  --chaos-tool "$CHAOS_TOOL" \
  "${MODEL_FLAGS[@]}" \
  "Build a production-quality full-stack Notes app at $PROJECT_DIR.

Layout (TWO sibling projects under the root):
  $PROJECT_DIR/api/    — FastAPI backend, SQLite via SQLAlchemy
  $PROJECT_DIR/web/    — Vite + React + TypeScript frontend

You have TWO skills available (see <available_skills> in the system prompt):
  - 'backend'   — opinionated Python/FastAPI/Pydantic v2/SQLAlchemy stack
  - 'frontend'  — opinionated Vite/React/TS/Tailwind/Lucide stack
LOAD BOTH via the load_skill tool BEFORE scaffolding the corresponding side.
The skills are binding: stack, layout, conventions, gotchas — don't deviate.

NOTE: This run injects probabilistic failures on LLM and tool calls so the
resilience layers (LLM retry, tool retry, plan-act recovery, summit_task_result
success=false) actively engage. Errors prefixed '[chaos]' are simulated;
real upstream issues are not.

============================================================================
API CONTRACT (both sides MUST agree)
============================================================================

Resource: Note { id:int, title:str(1-200), body:str(0-10000), pinned:bool,
                 created_at:iso8601, updated_at:iso8601 }

Endpoints under /api/v1/notes:
  GET    /        list (pinned first, then updated_at desc)
  POST   /        create — body: {title, body?, pinned?}
  GET    /{id}    fetch — 404 if missing
  PATCH  /{id}    partial update
  DELETE /{id}    delete — 204; 404 if missing

Validation: title trimmed-non-empty, ≤200; body ≤10000; PATCH non-empty.
CORS: allow http://localhost:5173.

============================================================================
BACKEND ($PROJECT_DIR/api) — follow 'backend' skill
============================================================================

  - SQLite at api/notes.db (gitignored)
  - SQLAlchemy 2.0 async, Pydantic v2 schemas, app factory
  - Tests (pytest+httpx): roundtrip, ordering, validation 422 cases

Verify: cd $PROJECT_DIR/api && uv sync && uv run pytest -q

============================================================================
FRONTEND ($PROJECT_DIR/web) — follow 'frontend' skill
============================================================================

  - Vite + React 18 + TS + Tailwind v3 + Lucide
  - API client typed, base URL from VITE_API_BASE
  - List view (pinned first), inline edit, pin toggle, delete, empty state

Verify: cd $PROJECT_DIR/web && npm install && npm run build

CONSTRAINTS: never run 'npm run dev' / 'uv run uvicorn' (they hang). Tests
+ build only.

============================================================================
ROOT
============================================================================

  - $PROJECT_DIR/README.md and .gitignore

============================================================================
FINAL OUTPUT
============================================================================

After verification passes, print: tree (2 levels), pytest tail (20 lines),
npm build tail (10 lines), and a 4-sentence summary."

echo
[ "$TRACE_FILE" != "off" ] && echo "→ chaos failures captured in trace: $(jq -c 'select(.ok==false) | .name' "$TRACE_FILE" 2>/dev/null | wc -l)"
