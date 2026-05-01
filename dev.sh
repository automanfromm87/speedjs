#!/usr/bin/env bash
# speedjs dev runner — full Todo SPA build with all the goodies on:
#   - plan-act mode
#   - skill loading (frontend skill auto-injected into system prompt)
#   - tape checkpoint (resume from crash)
#   - log file (stdout stays clean — only final answer)
#   - max_iters 100, retry, walltime, budget all configured
#
# Env vars (optional):
#   RESUME=1            keep existing /tmp/todo-app and tape, continue from last run
#   SPEEDJS_LOG=path    override log destination (default /tmp/speedjs-run.log)
#   SPEEDJS_TAPE=path   override tape file (default /tmp/todo-build.tape)
#   SPEEDJS_PROJECT=dir override project dir (default /tmp/todo-app)

set -e

LOG_FILE="${SPEEDJS_LOG:-/tmp/speedjs-run.log}"
TAPE_FILE="${SPEEDJS_TAPE:-/tmp/todo-build.tape}"
PROJECT_DIR="${SPEEDJS_PROJECT:-/tmp/todo-app}"
SKILLS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/skills"

# Clean previous run unless RESUME=1 is set. Also wipes /tmp/speedjs-memory
# so executor memory starts fresh — matters because continue.sh later loads
# from the same dir, and stale memory would confuse the survey.
if [ -z "${RESUME:-}" ]; then
  rm -rf "$PROJECT_DIR" "$TAPE_FILE" /tmp/speedjs-memory
fi

echo "→ project: $PROJECT_DIR"
echo "→ skills:  $SKILLS_DIR"
echo "→ logs:    $LOG_FILE   (tail -f to follow live)"
echo "→ tape:    $TAPE_FILE  (RESUME=1 ./dev.sh to continue from crash)"
echo

dune exec speedjs -- \
  --plan \
  --budget 5.0 \
  --walltime 1800 \
  --max-iters 100 \
  --max-retries 3 \
  --skills-dir "$SKILLS_DIR" \
  --log-file "$LOG_FILE" \
  --tape "$TAPE_FILE" \
  --memory-dir "/tmp/speedjs-memory" \
  "Build a production-quality Todo SPA at $PROJECT_DIR — pure frontend, no backend.

You have a 'frontend' skill available in your skill index (see <available_skills>
in the system prompt). LOAD IT FIRST via the load_skill tool before scaffolding.
The skill is opinionated and binding — it covers the stack, directory layout,
all build gotchas (verbatimModuleSyntax, type-only imports, content paths),
styling conventions, and Lucide icon usage. Don't deviate from what it says.

Goal:
  Stack: Vite + React 18 + TypeScript + Tailwind v3 + Lucide.
  Persist state in localStorage under key 'speedjs-todos'.
  All styling via Tailwind utility classes only.

Required features:
  - Add new todo (text input + Enter key)
  - Toggle complete (checkbox)
  - Delete todo (button with Trash2 icon, hover-revealed)
  - Edit todo on double-click
  - Filter tabs: All / Active / Completed
  - 'Clear completed' button (only when there are completed todos)
  - Counter: 'N items left'
  - Empty state with ClipboardList icon when no todos

Constraints (absolute):
  - NEVER run 'npm run dev' — interactive, will hang. Use 'npm run build' to verify.
  - TypeScript strictly: no any, all functions typed, named props interfaces.
  - Tailwind ONLY: no inline style={}, no separate .css files except index.css.
  - Use Lucide icons (Trash2, Plus, Check, X, ClipboardList for empty state).

Verification:
  After writing all files, run 'npm run build' from the project dir. It MUST succeed
  with zero TypeScript errors. If errors appear, fix them and rebuild until clean.

Final output:
  Show the src tree, the build output, and a 3-sentence description of what was built."
