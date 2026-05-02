#!/usr/bin/env bash
# squad.sh — 4-agent engineering team workflow.
#
#   PM → Design → Loop_until(qa.pass) { Fullstack → QA }
#
# Each agent has its own role prompt + structured terminal tool. The
# fullstack/qa pair iterates: QA reports issues, Fullstack revises,
# QA re-reviews, until pass=true OR max-iters reached.
#
# Watch the log: you'll see role-tagged ReAct loops (each role's
# Agent.run_loop has its own iter counter). The transcript at the end
# shows spec → design → final implementation → QA verdict + history.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
[ -f "$SCRIPT_DIR/.env.local" ] && . "$SCRIPT_DIR/.env.local"

LOG_FILE="${SPEEDJS_LOG:-/tmp/speedjs-squad.log}"
: > "$LOG_FILE"

GOAL="${1:-Build a small in-memory URL shortener service. POST /shorten {url} returns {code, expires_at}; GET /:code redirects with 302 (or 404 if expired/missing). Each entry has a configurable TTL (default 1 hour). Use Node.js + Express + TypeScript. Include a tiny test that exercises both endpoints.}"
MAX_ITERS="${MAX_ITERS:-3}"

echo "→ logs:       $LOG_FILE   (tail -f to follow)"
echo "→ goal:       $GOAL"
echo "→ max_iters:  $MAX_ITERS  (fullstack/qa retry cap)"
echo "→ team:       product → design → (fullstack ↔ qa)"
echo

dune exec squad -- \
  --max-iters "$MAX_ITERS" \
  --log-file "$LOG_FILE" \
  "$GOAL"
