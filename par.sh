#!/usr/bin/env bash
# par.sh — parallel_delegate live demo with non-trivial tasks.
#
# Spawns 3 sub-agents that each:
#   1. view_file the assigned source module
#   2. reason in 1-3 ReAct iterations
#   3. return a structured analysis
#
# Then the parent agent synthesizes a unified architectural overview.
#
# Watch the log for [sub:0] / [sub:1] / [sub:2] interleaving — the
# proof that 3 LLM calls are fanning out concurrently across OCaml 5
# domains, each with its own effect-handler stack.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
[ -f "$SCRIPT_DIR/.env.local" ] && . "$SCRIPT_DIR/.env.local"

LOG_FILE="${SPEEDJS_LOG:-/tmp/speedjs-par.log}"
: > "$LOG_FILE"

REPO="$SCRIPT_DIR"

echo "→ logs:  $LOG_FILE   (tail -f to follow)"
echo "→ repo:  $REPO"
echo "→ goal:  3 sub-agents analyze 3 modules in parallel, parent synthesizes"
echo

dune exec speedjs -- \
  --max-iters 20 \
  --max-retries 3 \
  --max-subagent-depth 2 \
  --log-file "$LOG_FILE" \
  "Use the parallel_delegate tool to analyze THREE source modules of this agent framework concurrently. Each sub-agent should use the view_file tool to read its assigned file (path is given), then deliver a focused analysis. After parallel_delegate returns, write a unified architectural review.

  Task A: View $REPO/lib/handlers/file_handler.ml. Identify the TWO main composition patterns (the type-t-based middleware chain and the install function bridging chain to effect). Explain in ONE paragraph why this dual pattern matters for testability.

  Task B: View $REPO/lib/agents/parallel_subagent.ml. Identify the TWO key challenges OCaml 5 Domain-based parallelism creates (effect handlers don't propagate across domains; shared mutable state needs synchronization) and explain how this module addresses each. ONE paragraph.

  Task C: View $REPO/lib/runtime/runtime.ml. Explain the install order (Governor → Llm_handler → File/Time/Log → Tool_handler) and why moving any handler into Tool_handler's scope would break tools that perform effects. ONE paragraph.

  After parallel_delegate returns, write a 200-word architectural review tying the three modules together. Focus on the recurring theme: 'effect handler stacks as the unit of composition'."
