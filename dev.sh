#!/usr/bin/env bash
# speedjs dev runner — full-stack Notes app stress test.
#
# Why this prompt: it forces the agent to use BOTH skills (backend +
# frontend), plan across multiple components, agree on an API contract,
# run real verification (pytest + npm build), and likely hit at least one
# recovery cycle (cross-component contracts rarely land first try).
#
# Env vars (optional):
#   RESUME=1            keep existing project + tape, continue from last run
#   SPEEDJS_LOG=path    log destination     (default /tmp/speedjs-run.log)
#   SPEEDJS_TAPE=path   tape file           (default /tmp/notes-build.tape)
#   SPEEDJS_PROJECT=dir project root        (default /tmp/notes-app)
#   SPEEDJS_TRACE=path  trace NDJSON        (default /tmp/speedjs-trace.ndjson)
#   SPEEDJS_TRACE=off   disable structured tracing

set -e

# Load local env overrides (gateway URL, proxy, API key, etc).
# .env.local is gitignored; copy .env.local.example to start.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
[ -f "$SCRIPT_DIR/.env.local" ] && . "$SCRIPT_DIR/.env.local"

LOG_FILE="${SPEEDJS_LOG:-/tmp/speedjs-run.log}"
TAPE_FILE="${SPEEDJS_TAPE:-/tmp/notes-build.tape}"
PROJECT_DIR="${SPEEDJS_PROJECT:-/tmp/notes-app}"
SKILLS_DIR="$SCRIPT_DIR/skills"
TRACE_FILE="${SPEEDJS_TRACE:-/tmp/speedjs-trace.ndjson}"

TRACE_FLAG=()
if [ "$TRACE_FILE" != "off" ]; then
  TRACE_FLAG=(--trace-file "$TRACE_FILE")
fi

if [ -z "${RESUME:-}" ]; then
  rm -rf "$PROJECT_DIR" "$TAPE_FILE" /tmp/speedjs-memory
  [ "$TRACE_FILE" != "off" ] && : > "$TRACE_FILE"
fi

echo "→ project: $PROJECT_DIR"
echo "→ skills:  $SKILLS_DIR"
echo "→ logs:    $LOG_FILE   (tail -f to follow live)"
echo "→ tape:    $TAPE_FILE  (RESUME=1 ./dev.sh to continue from crash)"
[ "$TRACE_FILE" != "off" ] && \
  echo "→ trace:   $TRACE_FILE  (open tools/trace_viewer.html, drag the file in)"
echo

dune exec speedjs -- \
  --plan \
  --budget 10.0 \
  --walltime 3600 \
  --max-iters 150 \
  --max-retries 3 \
  --skills-dir "$SKILLS_DIR" \
  --working-dir "$PROJECT_DIR" \
  --log-file "$LOG_FILE" \
  --tape "$TAPE_FILE" \
  "${TRACE_FLAG[@]}" \
  --memory-dir "/tmp/speedjs-memory" \
  "Build a production-quality full-stack Notes app at $PROJECT_DIR.

Layout (TWO sibling projects under the root):
  $PROJECT_DIR/api/    — FastAPI backend, SQLite via SQLAlchemy
  $PROJECT_DIR/web/    — Vite + React + TypeScript frontend

You have TWO skills available (see <available_skills> in the system prompt):
  - 'backend'   — opinionated Python/FastAPI/Pydantic v2/SQLAlchemy stack
  - 'frontend'  — opinionated Vite/React/TS/Tailwind/Lucide stack
LOAD BOTH via the load_skill tool BEFORE scaffolding the corresponding side.
The skills are binding: stack, layout, conventions, gotchas — don't deviate.

============================================================================
API CONTRACT (both sides MUST agree on this exact shape)
============================================================================

Resource: Note
  - id:         integer, server-assigned
  - title:      string, 1–200 chars, required
  - body:       string, 0–10000 chars, default ''
  - pinned:     boolean, default false
  - created_at: ISO-8601 string (UTC), server-assigned
  - updated_at: ISO-8601 string (UTC), server-assigned

Endpoints (all under /api/v1/notes):
  GET    /                     → list notes, pinned first then by updated_at desc
  POST   /                     → create. body: {title, body?, pinned?}
  GET    /{id}                 → fetch one. 404 if missing.
  PATCH  /{id}                 → partial update. body: any subset of {title, body, pinned}
  DELETE /{id}                 → delete. 204 on success, 404 if missing.

Validation:
  - title trimmed; empty after trim → 422
  - title > 200 chars → 422
  - body > 10000 chars → 422
  - PATCH with no fields → 422

CORS: allow http://localhost:5173 (Vite default).

============================================================================
BACKEND ($PROJECT_DIR/api) — follow the 'backend' skill
============================================================================

  - SQLite file at api/notes.db (gitignored).
  - SQLAlchemy 2.0 declarative models, async session.
  - Pydantic v2 schemas: NoteCreate, NoteUpdate, NoteRead.
  - Routes in app/routes/notes.py. App factory in app/main.py.
  - Dependency-injected DB session.
  - On startup: create tables if missing (no Alembic — keep it simple).

Tests (pytest + httpx AsyncClient):
  - tests/test_notes.py covering: create+read roundtrip; list ordering
    (pinned-first); patch updates updated_at; delete then 404; validation
    422 cases (empty title, oversized body, empty patch).
  - Tests use a separate sqlite file (tmp_path fixture) — never the dev DB.

Verification:
  cd $PROJECT_DIR/api
  uv sync
  uv run pytest -q     # MUST pass with zero failures

============================================================================
FRONTEND ($PROJECT_DIR/web) — follow the 'frontend' skill
============================================================================

  - Vite + React 18 + TS + Tailwind v3 + Lucide.
  - API client in src/lib/api.ts — typed wrappers per endpoint, base URL
    from import.meta.env.VITE_API_BASE (default 'http://localhost:8000').
  - State via React's useState + useEffect (no Redux/Zustand for this scope).

UI features:
  - List view with pinned notes pinned at top (Pin icon from Lucide).
  - 'New note' form: title (required) + body (textarea) + pinned checkbox.
  - Edit-in-place on row click (title + body + pinned toggle, Save/Cancel).
  - Delete button with Trash2 icon (hover-revealed).
  - Pin/unpin button toggles via PATCH.
  - Empty state with NotebookPen icon when zero notes.
  - Loading + error states for the initial fetch.

Constraints:
  - NEVER run 'npm run dev' or 'uv run uvicorn' — they hang. Use 'npm run build'
    and 'uv run pytest' for verification ONLY.
  - TypeScript strict: no any; named props interfaces; type-only imports
    (verbatimModuleSyntax).
  - Tailwind utility classes only — no inline style={}, no extra .css beyond
    Tailwind's index.css.
  - Use Lucide icons exclusively.

Verification:
  cd $PROJECT_DIR/web
  npm install
  npm run build       # MUST succeed with zero TS errors

============================================================================
ROOT
============================================================================

  - $PROJECT_DIR/README.md: 1-paragraph summary + how to run both sides
    (start the API, then the web), plus the 'verify' commands above.
  - $PROJECT_DIR/.gitignore covering node_modules/, dist/, __pycache__/,
    .venv/, *.db, .pytest_cache/.

============================================================================
FINAL OUTPUT
============================================================================

After all verification commands pass, print:
  1. Tree of $PROJECT_DIR (two levels deep, excluding node_modules/.venv/dist).
  2. Last 20 lines of pytest output.
  3. Last 10 lines of npm build output.
  4. A 4-sentence summary describing the API contract and the UI."
