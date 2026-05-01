# Contributing to speedjs

## Project layout

```
bin/                              # CLI entry point + glue
├── args.ml                       # CLI flag parsing (data only)
├── log.ml                        # log sink (stderr / file)
├── protection_stack.ml           # wrap agent thunk in protection layers
├── setup.ml                      # MCP / skills / tools / runtime wiring
├── modes.ml                      # session vs one-shot dispatch
└── main.ml                       # entry point (orchestration only)

lib/                              # core library
├── core/                         # foundational types + utilities
│   ├── effects.ml                # effect declarations
│   ├── types.ml                  # message / tool_def / agent_error / etc.
│   ├── conversation.ml + .mli    # smart-constructor for valid message lists
│   ├── memory.ml                 # per-agent message-list with persistence
│   ├── session.ml                # chat session JSON I/O
│   └── layer.ml                  # generic thunk-wrapper composition
├── llm/                          # LLM transport
│   ├── http.ml                   # curl spawn + HTTP parsing
│   ├── sse.ml                    # SSE event stream parser
│   ├── anthropic_req.ml          # request body / response shape
│   ├── anthropic.ml              # complete / complete_stream wiring
│   ├── llm_error.ml              # typed error taxonomy
│   ├── retry.ml                  # retry handler
│   └── checkpoint.ml             # tape replay
├── handlers/                     # effect handler infrastructure
│   ├── handlers.ml               # production / mock / silent
│   ├── protection.ml             # walltime / budget / loop_guard
│   └── parallel.ml               # threaded map
├── agents/                       # ReAct loops + flows
│   ├── agent.ml + .mli           # core ReAct loop
│   ├── planner.ml                # planner agent (forced submit_plan)
│   ├── plan_act.ml + .mli        # plan-act flow + run_for_task
│   └── sub_agent.ml              # delegate tool
└── tools/                        # built-in tools + integrations
    ├── tools.ml + .mli           # bash / view_file / write_file / etc.
    ├── skill.ml + .mli           # skill loading + load_skill tool
    ├── mcp.ml                    # MCP client
    └── workspace_surveyor.ml     # pre-plan project scan
```

Module names stay flat in the namespace via `(include_subdirs unqualified)`;
the directory layout reflects concept grouping, not module paths.

## Error handling style

Two mechanisms, used for distinct failure classes. The line is honored
across the codebase — please don't mix.

| Mechanism | When to use | Examples |
|---|---|---|
| `('a, string) result` / `('a, error) result` | **Expected** failures: user input, IO, external protocol | `Conversation.of_messages`, `Mcp.connect_and_load_tools`, `Tools.require_absolute_path` |
| **Exceptions** | **Programming errors**, invariant violations, control-flow short-circuits | `Conversation.Invariant_violated`, `Agent.Wait_for_user`, `Agent.Task_terminal_called`, `Llm_error.Llm_api_error` |

### Decision rules

- **External boundary** (HTTP response, JSON parse, file read, user
  argument) → `result`. Caller can choose to handle or surface.
- **Internal boundary** (smart constructor receives malformed input from
  trusted caller) → exception. Programmer error; no recovery expected.
- **Loop short-circuit** (terminal tool, ask_user pause, loop_guard
  fires) → exception. Cleaner than threading sentinel returns through
  the recursive loop.
- **Protection guards** (Budget/Walltime/Loop_aborted) → exception.
  Captured at the outermost call site by
  `Protection.catch_protection_errors`.

### Anti-patterns

- ❌ Don't return `result` for things that can't fail in a controlled
  context (e.g. an internal helper called only from a place that just
  validated the input).
- ❌ Don't raise from a function that's a `result`-returning module's
  public API just because the implementation path is easier — wrap it in
  a `try`/`with` and convert to `Error`.
- ❌ Don't catch generic `exception _` — name what you're catching, or
  let it propagate.

## API surface (`.mli`) discipline

Modules with `.mli`:
- `core/conversation.mli` — seals state machine; only safe constructors leak
- `agents/agent.mli` — exposes ReAct loop + entry points; hides log helpers
- `agents/plan_act.mli` — exposes `config` record + `run` + `run_for_task`
- `tools/tools.mli` — built-in tool defs + path/dir helpers
- `tools/skill.mli` — skill registry + `load_skill` tool

Modules without `.mli` export everything (legacy + small modules). When
you find yourself `open`-ing a module just to use 1-2 things, consider
adding an `.mli` to make the public surface explicit.

## Adding a new effect handler

1. Define a function `(unit -> 'a) -> 'a` (matches `Layer.t` shape):
   ```ocaml
   let with_my_handler ?config f = try_with f () { effc = ... }
   ```
2. Compose with existing layers via `Layer.compose` or directly nest.
3. If user-facing, add a flag in `bin/args.ml` and wire into
   `bin/protection_stack.ml`.

## Adding a new tool

1. Define `tool_def` in `lib/tools/tools.ml` (or its own file if large).
2. Register in `Tools.all`.
3. Production handler dispatches automatically via the `Tool_calls` effect.

## Adding a new MCP server / skill

No code changes needed — pass via `--mcp` / `--skills-dir` CLI flags.
