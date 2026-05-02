#!/usr/bin/env bash
# debate.sh — two-agent system-design debate.
#
# bash_expert vs pl_designer arguing about a topic. By default:
#
#   "Should we design a new DSL for shell scripting that replaces bash,
#    or is bash + posix tooling already the right answer?"
#
# Watch the log: each round shows one speaker's full ReAct trace
# (system prompt → LLM call → submit_argument), then the other.
# After max_rounds OR a speaker calls submit_argument with agree=true,
# the transcript prints to stdout.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
[ -f "$SCRIPT_DIR/.env.local" ] && . "$SCRIPT_DIR/.env.local"

LOG_FILE="${SPEEDJS_LOG:-/tmp/speedjs-debate.log}"
: > "$LOG_FILE"

TOPIC="${1:-Should we design a new DSL for shell scripting that replaces bash, or is bash + POSIX tooling already the right answer?}"
MAX_ROUNDS="${MAX_ROUNDS:-6}"

echo "→ logs:        $LOG_FILE   (tail -f to follow)"
echo "→ topic:       $TOPIC"
echo "→ max_rounds:  $MAX_ROUNDS"
echo "→ agents:      bash_expert vs pl_designer"
echo

dune exec debate -- \
  --max-rounds "$MAX_ROUNDS" \
  --log-file "$LOG_FILE" \
  "$TOPIC"
