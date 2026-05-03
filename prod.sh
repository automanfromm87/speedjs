#!/usr/bin/env bash
# Production-class test: full-stack photo-timeline (Instagram-ish MVP).
# Designed to stress every layer of the system on a real project:
#   - large DAG plan (auth / posts / users / follows / feed are
#     structurally independent subgraphs)
#   - real cross-end contract (auth + JWT + 5 entities + ~20 endpoints)
#   - multi-model (Opus planner + recovery, Sonnet executor + summarizer)
#   - per-task git checkpoint
#   - skill loading (backend + frontend)
#   - real LLM failure / recovery (no chaos forced — production behavior)
#
# Env vars (all optional):
#   SPEEDJS_LOG=path     log file (default /tmp/speedjs-prod.log)
#   SPEEDJS_PROJECT=dir  project root (default /tmp/photo-feed)
#   SPEEDJS_TRACE=path   trace ndjson (default /tmp/speedjs-prod-trace.ndjson)
#   SPEEDJS_NO_OPEN=1    don't auto-open the html report
#   RESUME=1             continue from saved plan_state.json
#
# Tail the run live:
#   tail -f /tmp/speedjs-prod.log | grep -E '\[event\]|\[recovery\]|cascade'

set -e
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
[ -f "$SCRIPT_DIR/.env.local" ] && . "$SCRIPT_DIR/.env.local"

LOG_FILE="${SPEEDJS_LOG:-/tmp/speedjs-prod.log}"
PROJECT_DIR="${SPEEDJS_PROJECT:-/tmp/photo-feed}"
SKILLS_DIR="$SCRIPT_DIR/skills"
TRACE_FILE="${SPEEDJS_TRACE:-/tmp/speedjs-prod-trace.ndjson}"

OPEN_FLAG=()
[ -z "${SPEEDJS_NO_OPEN:-}" ] && OPEN_FLAG=(--trace-open)

if [ -z "${RESUME:-}" ]; then
  rm -rf "$PROJECT_DIR" /tmp/speedjs-prod-memory
  : > "$TRACE_FILE"
fi

echo "→ project:  $PROJECT_DIR"
echo "→ skills:   $SKILLS_DIR"
echo "→ logs:     $LOG_FILE   (tail -f to follow)"
echo "→ trace:    $TRACE_FILE  → ${TRACE_FILE%.*}.html"
echo "→ models:   planner=Opus  recovery=Opus  executor=Sonnet  summarizer=Sonnet"
echo "→ resume:   /tmp/speedjs-prod-memory  (RESUME=1 ./prod.sh to continue)"
echo

dune exec speedjs -- \
  --plan \
  --plan-dag \
  --budget 15.0 \
  --walltime 5400 \
  --max-iters 200 \
  --max-steps 1500 \
  --max-retries 5 \
  --skills-dir "$SKILLS_DIR" \
  --working-dir "$PROJECT_DIR" \
  --log-file "$LOG_FILE" \
  --memory-dir "/tmp/speedjs-prod-memory" \
  --trace-file "$TRACE_FILE" \
  "${OPEN_FLAG[@]}" \
  --planner-model claude-opus-4-5 \
  --recovery-model claude-opus-4-5 \
  "Build a production-quality full-stack photo-timeline app at $PROJECT_DIR.

Inspired by Instagram's MVP — auth, posts (image URL + caption), feed of
followed users' posts, likes, comments, follow/followers, user profiles.
NO real image upload (paste URL or use https://picsum.photos placeholder).
NO stories / DM / video / search / notifications.

Layout (TWO sibling projects under the root):
  $PROJECT_DIR/api/  — FastAPI backend, SQLite via SQLAlchemy
  $PROJECT_DIR/web/  — Vite + React + TS frontend

You have TWO skills available (see <available_skills> in the system prompt):
  - 'backend'   — opinionated Python/FastAPI/Pydantic v2/SQLAlchemy stack
  - 'frontend'  — opinionated Vite/React/TS/Tailwind/Lucide stack
LOAD BOTH via load_skill BEFORE scaffolding the corresponding side. The
skills are binding: stack, layout, conventions — don't deviate.

============================================================================
API CONTRACT (both sides MUST agree exactly)
============================================================================

Auth (username-only, no email):
  POST /api/v1/auth/signup    body: {username, password, bio?}
                              → 201 {access_token, user}
  POST /api/v1/auth/login     body: {username, password}
                              → 200 {access_token, user}

  JWT in 'Authorization: Bearer <token>' header. Expire in 7 days. HS256.
  Passwords hashed with bcrypt (passlib[bcrypt]).

Users:
  User { id, username (unique, 3-30, [a-zA-Z0-9_]),
         bio (0-500, default ''), avatar_url (str|null),
         created_at }

  GET   /api/v1/users/me                              auth → current user
  PATCH /api/v1/users/me      body: {bio?, avatar_url?}     auth → updated
  GET   /api/v1/users/{username}                       → profile +
                                                         post_count, follower_count,
                                                         following_count, is_following
  GET   /api/v1/users/{username}/posts?limit=20&before_id=N
                                                       → cursor-paginated

Posts:
  Post { id, author:UserPublic, image_url (1-1000), caption (0-2000),
         created_at, like_count, comment_count, liked_by_me }

  POST   /api/v1/posts        body: {image_url, caption?}    auth → 201
  GET    /api/v1/posts/{id}                                  → 200 / 404
  DELETE /api/v1/posts/{id}   auth (author only)             → 204 / 403

Feed:
  GET /api/v1/feed?limit=20&before_id=N    auth
        → posts from {self} ∪ {users I follow}, most recent first,
          cursor-paginated by post.id descending.

Likes:
  POST   /api/v1/posts/{id}/like      auth (idempotent — 200 if already liked)
  DELETE /api/v1/posts/{id}/like      auth (idempotent — 200 if not liked)
  GET    /api/v1/posts/{id}/likes     → list of UserPublic

Comments:
  Comment { id, post_id, author:UserPublic, body (1-1000), created_at }

  POST   /api/v1/posts/{id}/comments   body: {body}    auth → 201
  GET    /api/v1/posts/{id}/comments?limit=50&after_id=N
                                                        → cursor-paginated, oldest first
  DELETE /api/v1/comments/{id}    auth (author OR post owner) → 204 / 403

Follow:
  POST   /api/v1/users/{username}/follow     auth (idempotent) → 200
  DELETE /api/v1/users/{username}/follow     auth (idempotent) → 200
  GET    /api/v1/users/{username}/followers  → list of UserPublic
  GET    /api/v1/users/{username}/following  → list of UserPublic

Validation rules:
  - username: trimmed, 3-30 chars, regex ^[a-zA-Z0-9_]+$ (server-enforced 422)
  - password: min 8 chars
  - cannot follow yourself (422)
  - 401 on missing/invalid token
  - 403 on author-mismatch delete
  - 404 on missing entities

CORS: allow origin http://localhost:5173, methods *, headers *.

============================================================================
BACKEND ($PROJECT_DIR/api) — follow 'backend' skill
============================================================================

  - SQLite at api/photo.db (gitignored)
  - SQLAlchemy 2.0 async, Pydantic v2 schemas, app factory
  - Indexes: User.username unique; (Follow.follower_id, Follow.followed_id)
    composite unique; (Like.post_id, Like.user_id) composite unique
  - bcrypt password hashing (passlib[bcrypt])
  - python-jose[cryptography] for JWT (HS256, 7-day expiry)
  - Cursor pagination uses post.id (not offset)

  Tests (pytest + pytest-asyncio + httpx ASGITransport, no real network):
    - auth roundtrip: signup → login → /users/me
    - signup duplicate username → 409
    - signup invalid username regex → 422
    - login wrong password → 401
    - missing/invalid token → 401
    - post CRUD + author-only delete (other user → 403)
    - feed contains self+followed posts in id-desc order
    - feed cursor pagination correctness (no overlap, last page empty)
    - like idempotence (POST twice → still 1 like)
    - comment author OR post owner can delete; third party 403
    - follow idempotence + cannot-follow-self 422

  Aim for 18-25 tests. Verify: cd $PROJECT_DIR/api && uv sync && uv run pytest -q

============================================================================
FRONTEND ($PROJECT_DIR/web) — follow 'frontend' skill
============================================================================

  - Vite + React 18 + TS + Tailwind v3 + Lucide + react-router-dom v6
  - Typed API client (one fn per endpoint), base URL from VITE_API_BASE
    (default http://localhost:8000)
  - JWT stored in localStorage; auth context provides {currentUser, login,
    logout, signup}; logout on any 401
  - Routes:
      /login          login form, link to /signup
      /signup         signup form
      /               Feed (posts list with infinite scroll OR 'load more')
      /post/:id       post detail with comments + add-comment form
      /u/:username    user profile (bio, avatar, post grid 3-col, follow btn)
      /u/:username/followers      followers list
      /u/:username/following      following list
      /new            new post: paste image URL + caption (live image preview)
      /settings       edit bio + avatar URL
  - Protected routes redirect to /login when no token
  - Each list view has empty / loading / error states
  - PostCard shows: avatar+username link, image, caption, like button
    (filled when liked_by_me), like_count, comment_count link to detail

  Verify: cd $PROJECT_DIR/web && npm install && npm run build

CONSTRAINTS: never run 'npm run dev' / 'uv run uvicorn' (they hang). Tests
+ build only. The frontend is verified by 'npm run build' (typecheck +
production bundle).

============================================================================
ROOT
============================================================================

  - $PROJECT_DIR/README.md   project overview, setup instructions for
                              both sides, API summary table, screenshots
                              section (placeholder)
  - $PROJECT_DIR/.gitignore   Python: __pycache__, *.pyc, .venv, *.db,
                              .pytest_cache; Node: node_modules, dist;
                              misc: .env, .DS_Store

============================================================================
PLANNING NOTES (DAG mode is enabled)
============================================================================

This plan should have multiple INDEPENDENT subgraphs that can run in
parallel structurally — declare depends_on honestly:
  - backend skeleton (config, db session, models) is one chain
  - each backend feature module (auth / users / posts / likes / comments /
    follows / feed) only depends on skeleton, NOT on each other
  - frontend skeleton (vite scaffold, tailwind, router, auth context, api
    client) is one chain
  - each frontend page only depends on frontend skeleton + api client
  - integration verifications depend on the modules they verify

Don't chain unrelated tasks (e.g. don't make 'create likes router' depend
on 'create comments router' if neither imports the other)."

echo
echo "→ run done; check $LOG_FILE for events / cost summary"
echo "→ HTML report: ${TRACE_FILE%.*}.html"
