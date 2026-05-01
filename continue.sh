#!/usr/bin/env bash
# speedjs continue runner — iterate on an existing /tmp/todo-app from a prior
# `./dev.sh` build. Pass your follow-up instruction as args.
#
# Two cross-run mechanisms (helix-style, both orthogonal to plan-act mode):
#
#   1. WorkspaceSurveyor (--working-dir): scans the project ONCE at startup,
#      compresses to a markdown brief via one LLM call, and injects it into
#      the planner's goal AND the executor's first task message. The planner
#      writes task descriptions with absolute paths because it knows the
#      file layout.
#
#   2. Persistent executor memory (--memory-dir): the executor's accumulated
#      message history is saved after each successful task. The next run
#      loads it, so the model sees its own prior tool calls and decisions
#      without needing to re-discover them.
#
# Usage:
#   ./continue.sh "add a dark mode toggle in the header"
#   ./continue.sh "make the trash button always visible, not hover-only"
#   ./continue.sh "extract the filter tabs into a reusable Tabs component"
#
# Env vars (optional):
#   RESUME=1            keep tape, continue a crashed continue-run
#   FRESH_MEMORY=1      wipe executor memory before this run (clean slate)
#   SPEEDJS_LOG=path    log file (default /tmp/speedjs-continue.log)
#   SPEEDJS_TAPE=path   tape file (default /tmp/todo-continue.tape)
#   SPEEDJS_MEMORY=p    memory dir (default /tmp/speedjs-memory, shared with dev.sh)
#   SPEEDJS_PROJECT=dir project dir (default /tmp/todo-app)

set -e

LOG_FILE="${SPEEDJS_LOG:-/tmp/speedjs-continue.log}"
TAPE_FILE="${SPEEDJS_TAPE:-/tmp/todo-continue.tape}"
MEMORY_DIR="${SPEEDJS_MEMORY:-/tmp/speedjs-memory}"
PROJECT_DIR="${SPEEDJS_PROJECT:-/tmp/todo-app}"
SKILLS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/skills"

# Pre-flight: the prior `./dev.sh` must have built the project.
if [ ! -d "$PROJECT_DIR" ]; then
  echo "✗ project dir $PROJECT_DIR not found — run ./dev.sh first" >&2
  exit 1
fi
if [ ! -f "$PROJECT_DIR/package.json" ]; then
  echo "✗ $PROJECT_DIR has no package.json — looks incomplete; rerun ./dev.sh" >&2
  exit 1
fi

# Take the user's follow-up instruction as positional args.
if [ $# -eq 0 ]; then
  echo "usage: $0 \"<your follow-up instruction>\"" >&2
  echo "  e.g. $0 \"add a dark mode toggle\"" >&2
  exit 2
fi
USER_INSTRUCTION="$*"

# Each continue-run is a NEW task — fresh tape by default. RESUME=1 keeps it.
if [ -z "${RESUME:-}" ]; then
  rm -f "$TAPE_FILE"
fi

# Memory accumulates ACROSS continue runs (and across dev → continue).
# FRESH_MEMORY=1 wipes it for a clean slate.
if [ -n "${FRESH_MEMORY:-}" ]; then
  rm -rf "$MEMORY_DIR"
  echo "→ memory wiped (FRESH_MEMORY=1)"
fi

PRIOR_MSGS="(none)"
if [ -f "$MEMORY_DIR/executor.json" ]; then
  PRIOR_MSGS=$(grep -c '"role"' "$MEMORY_DIR/executor.json" 2>/dev/null || echo "?")
fi

echo "→ project:   $PROJECT_DIR (existing — preserving)"
echo "→ skills:    $SKILLS_DIR"
echo "→ logs:      $LOG_FILE   (tail -f to follow live)"
echo "→ tape:      $TAPE_FILE  (RESUME=1 $0 ... to continue from crash)"
echo "→ memory:    $MEMORY_DIR  ($PRIOR_MSGS executor message(s); FRESH_MEMORY=1 to wipe)"
echo "→ task:      $USER_INSTRUCTION"
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
  --working-dir "$PROJECT_DIR" \
  --memory-dir "$MEMORY_DIR" \
  "Continue work on the existing Todo SPA at $PROJECT_DIR.

A <workspace_brief> block precedes this message — read it for the current
file layout and tech stack. The brief is generated fresh each run from the
files actually on disk; it's the source of truth for paths.

Stack reminder (also in the brief): Vite + React 18 + TypeScript (strict)
+ Tailwind v3 + Lucide icons. Todos persist in localStorage under key
'speedjs-todos'.

You have a 'frontend' skill in your skill index — LOAD IT FIRST via
load_skill. The skill binds stack conventions, type-only imports,
Tailwind-only styling, and Lucide icon usage.

USER INSTRUCTION:
$USER_INSTRUCTION

Process (binding):
  1. Use ABSOLUTE paths everywhere. The bash tool requires exec_dir
     (absolute path); file tools require absolute path arguments. Refer
     to files via their full path under $PROJECT_DIR/.
  2. Inspect files BEFORE editing — the workspace brief tells you which
     files exist; view_file each one you intend to touch.
  3. Make changes minimally. Match existing conventions: Tailwind only,
     'import type' for type-only imports, named props interfaces, no any.
  4. Verify with 'cd $PROJECT_DIR && npm run build'. Loop fix-rebuild
     until clean. NEVER run 'npm run dev' — interactive.

Final output:
  - Files changed/added (absolute paths).
  - Final build output (bundle sizes line).
  - 1-2 sentence summary."
