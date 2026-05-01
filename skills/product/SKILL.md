---
name: product
description: |
  Convert a brief user description (1-3 sentences like "build a notes app"
  or "I need a URL shortener") into a complete PRD before kicking off
  engineering work. Load this BEFORE any planning task that starts from
  a vague brief. The skill's job is to fill in the gaps that would
  otherwise cause the planner to thrash and the executor to mid-build pivot.
---

# Product Skill — Brief → PRD in one pass

When a user gives you a 1-2 sentence brief like "build a notes app" or
"I need a URL shortener", DO NOT immediately start planning tasks. Vague
briefs cause:

- **Wrong-shape tasks**: planner picks one interpretation, user wanted another
- **Mid-build pivots**: recovery cycles burn budget on REPLAN
- **Missing non-functional requirements**: validation, error states, empty states get added late or not at all
- **Scope creep**: "small additions" snowball when the original boundary was fuzzy

Your job: emit a complete PRD **before the first planning call**. The
planner consumes the PRD as its goal text; clearer goal → cleaner task
decomposition → fewer recovery cycles.

---

## 1. Decide: ASSUME or ASK?

For ambiguous points, default to **ASSUME-AND-DOCUMENT**, not ASK. Each
question is a turn the user has to wait for. Only ASK when the
assumption space is genuinely bimodal and the wrong assumption would
require throwing away most of the build.

**ASK only if you can't infer:**

1. **The core noun** — "track tasks" but what counts as a task?
2. **Single-user vs multi-user** — changes auth, schema, sharing surface
3. **Persistence** — in-memory ok? must survive restart? sync across devices?

For everything else, **assume the default and write it as an explicit
"Assumption" in the PRD**. Let the user correct it after seeing the PRD,
not before.

### Default assumptions (pick when not specified)

- **Scale**: single-user, local-first (no auth, no sharing)
- **Backend**: Python + FastAPI + SQLAlchemy 2.0 + SQLite (matches `backend` skill)
- **Frontend**: Vite + React 18 + TypeScript + Tailwind v3 + Lucide (matches `frontend` skill)
- **API style**: REST/JSON, no GraphQL
- **Validation**: server-side via Pydantic v2; client-side via React state
- **Testing**: pytest for backend (`uv run pytest -q`), build-only for frontend (`npm run build`)
- **Deployment**: localhost-only, no Docker, no cloud

---

## 2. PRD Template

Emit these 12 sections in order. Each section is REQUIRED. If a section
genuinely doesn't apply, write `N/A — <one-line reason>`.

```markdown
# <Product Name> PRD

## 1. Problem
1-3 sentences. What pain does this solve? Who feels it?

## 2. Users
- **Who**: 1 user persona (unless specified otherwise)
- **When**: frequency / context of use
- **What now**: what they currently do without this product

## 3. Goals (priority order)
- **G1**: <verb-led outcome, testable>
- **G2**: ...
- **G3**: ...

3-5 goals. Each must be testable: "user can do X" not "user enjoys Y".

## 4. Non-Goals (equally important — cuts scope)
- **NG1**: <thing this product explicitly does NOT do>
- **NG2**: ...
- **NG3**: ...

3-5 non-goals. Common ones for personal tools:
- No multi-user / sharing
- No mobile app
- No realtime collaboration
- No analytics / metrics
- No undo / history beyond current session

## 5. Data Model

### Entity: <Name>

| Field | Type | Constraints | Notes |
|---|---|---|---|
| id | int | PK, server-generated | |
| <field> | <type> | <validation rule> | <comment> |
| created_at | datetime | server-generated UTC | ISO-8601 |
| updated_at | datetime | server-updated UTC | ISO-8601 |

- **Created when**: <event that creates a row>
- **Modified when**: <events that mutate>
- **Deleted when**: <event, or "never (soft-delete via flag)">

(Repeat for every entity.)

## 6. Functional Requirements

### FR-1: <Capability name>

**As a** <user>, **I want to** <action>, **so that** <outcome>.

**Acceptance criteria** (testable):
- AC: When <input/action>, system <observable response>
- AC: When <error condition>, system <rejection behavior>
- AC: ...

3+ ACs per FR. Each AC must be deterministically verifiable
(automated tests can decide pass/fail).

(Repeat for every capability.)

## 7. API Contract

(If no backend: `N/A — frontend-only`.)

### `<METHOD> <path>` — <one-line purpose>

- **Request body**: JSON shape or `none`
- **Response (success)**: `<status>` with JSON shape
- **Errors**:
  - `<status>`: <when, what message>
  - `<status>`: ...

(Repeat for every endpoint.)

## 8. UI Surface

(If no frontend: `N/A — backend-only`.)

### View: <name>

- **Shows**: <what's rendered>
- **Interactions**: <user actions and their effects>
- **Loading state**: <how loading is indicated>
- **Empty state**: <copy + icon when zero items>
- **Error state**: <how errors surface to user>

(Repeat for every view.)

## 9. Non-Functional Requirements

- **Performance**: expected latency / throughput
- **Validation**: where + what (server-side / client-side)
- **Error handling**: how errors surface (toasts? inline? logs?)
- **Persistence**: what survives a process restart?
- **Security**: auth / authz / secrets handling — for single-user local
  tools usually `N/A` but state it explicitly

## 10. Assumptions

Things you assumed without asking. CRITICAL — user must be able to
override these before build starts.

- **Assumption**: <statement>. **Rationale**: <why this default>.
- **Assumption**: ...

## 11. Verification Plan

- **Backend**: `<test command — e.g. cd api && uv run pytest -q>`
- **Frontend**: `<test command — e.g. cd web && npm run build>`
- **Integration** (if applicable): `<command or steps>`

Define what counts as PASS for each — exit code? specific output?

## 12. Out of Scope (future work)

- <item>: <one-line why it's deferred>
- <item>: ...
```

---

## 3. Common pitfalls

**DO:**
- Write **specific** ACs: "user can edit a note inline by clicking it" — not "good editing UX"
- State **validation rules** with numbers: "title 1-200 chars after trim" — not "title required"
- List **error states** explicitly: 404, 422, 500, network failure, validation rejection
- Define **empty states**: "show NotebookPen icon + 'No notes yet' when zero items"
- State **non-functional defaults**: "no auth (local-only tool)" — even when obvious

**DON'T:**
- Use untestable adjectives: "intuitive", "nice", "clean", "modern" — what does the test assert?
- Skip non-goals — they set up scope creep mid-build
- Defer "small" decisions ("we'll figure out edit UI later") — that's the planner's pain
- Mix layers — UI details belong in §8, API shape in §7, validation in §9

---

## 4. Sizing your PRD to the brief — KEEP IT TIGHT

A 1-line brief produces a focused PRD, not a 5000-word epic. Tight
PRDs help the planner: shorter goal → cleaner task decomposition.
Verbose PRDs waste output tokens and slow the next step.

**HARD CAPS by brief size:**

| Brief size | Entities | FRs | Endpoints | Views | Total PRD |
|---|---|---|---|---|---|
| 1-line, 1 noun ("notes app") | 1 | 4-5 | 4-5 | 1 | **≤120 lines** |
| 1-paragraph, 1-2 nouns ("project tracker with tasks") | 2-3 | 6-8 | 6-10 | 2 | **≤200 lines** |
| Multi-feature ("Slack clone") | — | — | — | — | STOP and ASK what subset to scope |

**Tighten by:**
- Each FR's ACs: 3 short bullets max, not paragraphs
- API contract: one endpoint per ~3-line block
- Skip sections that genuinely don't apply with `N/A — <one-line reason>`
- For `## 8 UI Surface` with one view, list it briefly — don't repeat
  every loading/empty/error variant if they're predictable from §6
- For `## 10 Assumptions`, only list NON-obvious ones (don't write
  "Assumption: backend uses Python" — that's the default stack)

If your PRD heads toward 300+ lines for a 1-line brief, **stop**: you're
inflating. Cut to MVP shape and dump the rest into §12 Out of Scope.

---

## 5. After emitting the PRD

End your message with ONE of:

1. **If you made any non-trivial assumption** (anything that changes
   data shape or core behavior):
   ```
   I made the following assumptions to keep moving (see §10): [bulleted recap].
   Reply "approve" to start the build, or correct any of them.
   ```
   This gives the user one chance to redirect before the planner spends tokens.

2. **If brief was crisp and assumptions are minimal**:
   ```
   Starting the build based on the PRD above.
   ```
   Then proceed directly to planning.

The planner that consumes this PRD will treat it as a binding spec —
every FR maps to one or more tasks, every AC becomes a test target,
every entity becomes a model. Treat the PRD as the contract between
"what user asked for" and "what gets built".

---

## 6. Output format

Emit the full PRD as ONE markdown document. **Start your message
DIRECTLY with `# <Product Name> PRD`** — no preamble like "Now I'll
generate..." or "Here is the PRD:". Every preamble token is wasted
output and (on flaky network paths) one more token that needs to
survive transit before the real content starts.

Do NOT wrap the PRD in JSON, code fences, or epilogue text. Just emit
the PRD itself, then stop.

If you're asking a clarifying question (per §1), do that INSTEAD of
emitting the PRD — never mix "questions" with "draft PRD with TODOs"
in one message. The planner will get confused about what's spec and
what's open.
