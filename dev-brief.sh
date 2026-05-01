#!/usr/bin/env bash
# Brief → PRD → Build demo.
#
# Tests the new 'product' skill: feed a 1-line user brief, agent first
# loads the product skill to expand it into a complete PRD, THEN the
# planner decomposes the PRD into tasks. Compare with dev.sh which
# pre-supplies a hand-written PRD-equivalent.
#
# Usage:
#   ./dev-brief.sh
#   BRIEF="Build a habit tracker" ./dev-brief.sh
#
# Env vars (optional):
#   BRIEF=...           the 1-line user request (default: notes app)
#   RESUME=1            keep tape, continue from crash
#   SPEEDJS_LOG=path    log destination (default /tmp/speedjs-brief.log)
#   SPEEDJS_TAPE=path   tape file (default /tmp/brief-build.tape)
#   SPEEDJS_PROJECT=dir project root (default /tmp/brief-app)

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
[ -f "$SCRIPT_DIR/.env.local" ] && . "$SCRIPT_DIR/.env.local"

BRIEF="${BRIEF:-Build a personal notes app where I can keep short markdown notes, edit them, and pin important ones.}"
LOG_FILE="${SPEEDJS_LOG:-/tmp/speedjs-brief.log}"
TAPE_FILE="${SPEEDJS_TAPE:-/tmp/brief-build.tape}"
PROJECT_DIR="${SPEEDJS_PROJECT:-/tmp/brief-app}"
SKILLS_DIR="$SCRIPT_DIR/skills"

if [ -z "${RESUME:-}" ]; then
  rm -rf "$PROJECT_DIR" "$TAPE_FILE" /tmp/speedjs-brief-memory
fi

echo "→ brief:   $BRIEF"
echo "→ project: $PROJECT_DIR"
echo "→ skills:  $SKILLS_DIR"
echo "→ logs:    $LOG_FILE   (tail -f to follow live)"
echo "→ tape:    $TAPE_FILE  (RESUME=1 ./dev-brief.sh to continue)"
echo

dune exec speedjs -- \
  --plan \
  --budget 12.0 \
  --walltime 3600 \
  --max-iters 150 \
  --max-retries 8 \
  --skills-dir "$SKILLS_DIR" \
  --working-dir "$PROJECT_DIR" \
  --log-file "$LOG_FILE" \
  --tape "$TAPE_FILE" \
  --memory-dir "/tmp/speedjs-brief-memory" \
  "User brief: \"$BRIEF\"

You have skills available (see <available_skills> in the system prompt):
  - 'product'   — convert a brief into a complete PRD
  - 'backend'   — Python/FastAPI/Pydantic v2/SQLAlchemy stack
  - 'frontend'  — Vite/React/TS/Tailwind/Lucide stack

STEP 0 — REQUIREMENTS PHASE (do this BEFORE any task decomposition):
  1. Load the 'product' skill via load_skill.
  2. Apply it: convert the brief above into a complete PRD per the
     12-section template. Use the skill's default assumptions
     (single-user local, sqlite, Vite+React) since the brief doesn't
     specify otherwise. Document each assumption in §10.
  3. Once the PRD is emitted, treat IT as the binding goal — every
     functional requirement (§6) maps to one or more tasks; every
     entity (§5) becomes a model; every endpoint (§7) becomes a route.
     The PRD is the contract; the build implements it line-by-line.

STEP 1 — BUILD PHASE:
  Build the product into $PROJECT_DIR with:
    $PROJECT_DIR/api/    — backend (load 'backend' skill before scaffolding)
    $PROJECT_DIR/web/    — frontend (load 'frontend' skill before scaffolding)
    $PROJECT_DIR/README.md — project summary + run/verify commands

VERIFICATION (must pass cleanly):
  cd $PROJECT_DIR/api && uv sync && uv run pytest -q
  cd $PROJECT_DIR/web && npm install && npm run build

CONSTRAINTS (absolute):
  - NEVER run 'npm run dev' or 'uv run uvicorn' — they hang. Use
    'npm run build' / 'uv run pytest' for verification ONLY.
  - TypeScript strict; Tailwind utility classes only; Lucide icons only.
  - Python: Pydantic v2 + SQLAlchemy 2.0 typed Mapped[] style.

FINAL OUTPUT:
  After verification passes, print:
    1. The full PRD you emitted in STEP 0
    2. Tree of $PROJECT_DIR (2 levels, exclude node_modules/.venv/dist)
    3. Last 20 lines of pytest output, last 10 lines of npm build output
    4. Summary of how each FR in the PRD maps to delivered code"
