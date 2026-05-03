(** See [specs.mli]. *)

open Types

(* ===== Synthetic terminal tool definitions =====
   The handlers are never invoked — the agent loop intercepts via
   [terminal_tools] and short-circuits before dispatch. They exist as
   tool_defs so the model sees the schema and knows when to call. *)

let submit_plan_name = "submit_plan"
let submit_recovery_name = "submit_recovery"
let submit_task_result_name = "submit_task_result"

(* Thin builders for the JSON-schema fragments used by terminal tools.
   The full Anthropic tool input_schema vocabulary is much larger;
   these cover the cases we actually emit. *)

let schema_string ?description () =
  let desc =
    match description with
    | Some d -> [ ("description", `String d) ]
    | None -> []
  in
  `Assoc (("type", `String "string") :: desc)

let schema_bool ?description () =
  let desc =
    match description with
    | Some d -> [ ("description", `String d) ]
    | None -> []
  in
  `Assoc (("type", `String "boolean") :: desc)

let schema_string_enum ~values =
  `Assoc
    [
      ("type", `String "string");
      ("enum", `List (List.map (fun v -> `String v) values));
    ]

let schema_object ?description ~required ~properties () : Yojson.Safe.t =
  let desc =
    match description with
    | Some d -> [ ("description", `String d) ]
    | None -> []
  in
  `Assoc
    (("type", `String "object")
    :: desc
    @ [
        ("properties", `Assoc properties);
        ("required", `List (List.map (fun n -> `String n) required));
      ])

let schema_array ?description ~items () : Yojson.Safe.t =
  let desc =
    match description with
    | Some d -> [ ("description", `String d) ]
    | None -> []
  in
  `Assoc (("type", `String "array") :: desc @ [ ("items", items) ])

(** Build a synthetic terminal tool. The [handler] is configurable
    because [submit_task_result] returns Error if ever invoked
    (handler-as-assertion), while the others return a benign Ok
    placeholder. The agent loop intercepts via [terminal_tools]
    before dispatch, so the handler runs only if a caller wires the
    tool outside the agent loop. *)
let make_terminal_tool ~name ~description ~allowed_modes ~input_schema
    ~handler : tool_def =
  {
    idempotent = true;
    timeout_sec = None;
    category = "meta";
    capabilities = [ Terminal ];
    allowed_modes;
    classify_error = default_classify_error;
    name;
    description;
    input_schema;
    handler;
  }

let task_item_schema =
  schema_object
    ~required:[ "description" ]
    ~properties:
      [
        ( "description",
          schema_string ~description:"Self-contained task description" () );
        ( "depends_on",
          schema_array
            ~description:
              "Optional. 1-indexed positions of earlier tasks this task \
               depends on. Empty / omitted = no dependencies (can run \
               from the start). Only consumed in DAG plan mode; \
               sequential mode ignores this field."
            ~items:(`Assoc [ ("type", `String "integer") ]) () );
      ]
    ()

let submit_plan_tool : tool_def =
  make_terminal_tool ~name:submit_plan_name
    ~description:
      "Submit the structured plan that decomposes the user's goal into \
       ordered actionable tasks."
    ~allowed_modes:[ Planner ]
    ~input_schema:
      (schema_object
         ~required:[ "title"; "tasks" ]
         ~properties:
           [
             ( "title",
               schema_string ~description:"Short title of the plan (max ~10 words)" () );
             ( "tasks",
               schema_array
                 ~description:
                   "Ordered tasks, each one actionable and self-contained"
                 ~items:task_item_schema () );
           ]
         ())
    ~handler:(fun _ -> Ok "(submit_plan handled by planner orchestrator)")

let submit_recovery_tool : tool_def =
  make_terminal_tool ~name:submit_recovery_name
    ~description:"Submit the recovery decision after seeing a task failure."
    ~allowed_modes:[ Recovery ]
    ~input_schema:
      (schema_object
         ~required:[ "decision" ]
         ~properties:
           [
             ( "decision",
               schema_string_enum
                 ~values:[ "replan"; "split"; "skip"; "abandon" ] );
             ( "tasks",
               schema_array
                 ~description:
                   "Replacement tasks if decision=replan; empty if abandon"
                 ~items:task_item_schema () );
           ]
         ())
    ~handler:(fun _ -> Ok "(submit_recovery handled by orchestrator)")

let submit_task_result_tool : tool_def =
  make_terminal_tool ~name:submit_task_result_name
    ~description:
      "Submit the FINAL outcome for the current task. Call this exactly \
       once when the task is fully complete (or has definitively failed \
       and cannot be salvaged). The agent loop will end immediately after \
       this call. Do NOT call any other tools in the same response."
    ~allowed_modes:[ Executor; Subagent ]
    ~input_schema:
      (schema_object
         ~required:[ "success"; "result" ]
         ~properties:
           [
             ( "success",
               schema_bool
                 ~description:
                   "true if the task was completed successfully; false if \
                    it failed and cannot be salvaged"
                 () );
             ( "result",
               schema_string
                 ~description:
                   "Concise summary of what was accomplished (or empty if \
                    success=false)"
                 () );
             ( "error",
               schema_string
                 ~description:
                   "Diagnostic explanation if success=false; empty string \
                    when success=true"
                 () );
           ]
         ())
    ~handler:(fun _ ->
      Error
        "submit_task_result is a terminal tool — its handler should never \
         be invoked")

(* ===== Default system prompts ===== *)

let dag_planner_prompt =
  {|You are a PLANNER. Your job: read the user's goal, gather any relevant \
context, then break it into actionable tasks AND DECLARE THEIR DEPENDENCIES.

CRITICAL FIRST STEP — context gathering:
- Look at the <available_skills> block in your system prompt.
- For ANY skill whose description matches the user's goal, call \
  load_skill(name=<skill>) to read its full body BEFORE planning.
- If multiple skills are relevant, load each one. Don't skip.
- You may also use read-only tools (view_file, bash for ls/cat) to inspect \
  existing project state if relevant.

PLANNING rules:
- Each task is one concrete unit the executor can complete in a few tool \
  calls. If a skill says "split into types.ts + hooks/ + components/", \
  produce ONE TASK PER FILE.
- Tasks should be self-contained — description alone tells the executor \
  what to build.
- 1-3 tasks for simple goals; 5-15 for complex multi-file builds.
- Don't write code or files yourself — you are the planner. Use \
  read-only tools only.

DEPENDENCY rules (DAG mode — what makes this prompt different):
- Each task carries a `depends_on` array of 1-indexed positions of earlier \
  tasks that MUST complete before this one runs. Empty `depends_on` = \
  ready from the start.
- Declare a dependency ONLY when task B truly cannot run without task A's \
  output (file, function, schema, package). Common true dependencies: \
  "implement endpoint" depends on "create db model"; "verify build" \
  depends on every code-creation task.
- DO NOT declare dependencies between tasks that just touch different \
  files or different sub-projects (backend vs frontend) — those are \
  independent and can run in parallel.
- Verification / test-running tasks at the end should depend on every \
  code-creation task they verify (so they only run once everything they \
  test is in place).
- Avoid spurious linear chains. If 5 tasks could run in parallel, they \
  should each have empty `depends_on`, not chain to each other.
- 1-indexed: the FIRST task is index 1.

OUTPUT:
- After loading relevant skills, call submit_plan with title + tasks. \
  Each task object: {description, depends_on}. The order in the tasks \
  array is the canonical 1-indexed position. Do NOT respond in plain text.|}

let default_planner_prompt =
  {|You are a PLANNER. Your job: read the user's goal, gather any relevant \
context, then break it into a small ordered list of actionable tasks.

CRITICAL FIRST STEP — context gathering:
- Look at the <available_skills> block in your system prompt.
- For ANY skill whose description matches the user's goal, call \
  load_skill(name=<skill>) to read its full body BEFORE planning. Skill \
  descriptions are TERSE — the body contains binding structural rules \
  (file layout, naming conventions, config gotchas) that you MUST encode \
  into your task descriptions.
- If multiple skills are relevant, load each one. Don't skip.
- You may also use read-only tools (view_file, bash for ls/cat) to inspect \
  existing project state if relevant.

PLANNING rules:
- Each task is one concrete unit the executor can complete in a few tool \
  calls. If a skill says "split into types.ts + hooks/ + components/", \
  produce ONE TASK PER FILE, not one big "implement everything" task.
- Tasks should be self-contained — description alone tells the executor \
  what to build. The executor has the same skills loaded, so referencing \
  "follow the frontend skill conventions" is fine.
- 1-3 tasks for simple goals; 5-10 for complex multi-file builds. \
  Don't add ceremonial tasks (planning, reviewing, finalizing) — those \
  happen implicitly. Don't add a "load skill" task unless the executor \
  genuinely needs to load a skill mid-execution.
- Don't write code or files yourself — you are the planner. Use \
  read-only tools only.

OUTPUT:
- After you've loaded the relevant skills (if any), call submit_plan with \
  the structured plan. Do NOT respond in plain text — your only valid \
  termination is calling submit_plan.|}

let default_recovery_prompt =
  {|You are a recovery planner. A task in the plan failed. You see: the \
original goal, completed tasks, the failed task + error, remaining tasks, \
prior recovery failures (if any), and the current cycle index. Decide ONE of:

1. REPLAN: a new ordered list replacing the failed task AND all remaining \
tasks. Use submit_recovery with decision="replan" and a tasks array. Pick \
this when the original strategy is broken and a fundamentally different \
approach is needed.

2. SPLIT: keep the remaining tasks as-is, but replace the failed task with \
SMALLER sub-tasks. Use submit_recovery with decision="split" and a tasks \
array of ONLY the replacement sub-tasks. Pick this when the task was too \
coarse — e.g. "set up backend + frontend + tests + deploy" is one task and \
should be 4. Don't pick this if the issue is the strategy itself.

3. SKIP: drop the failed task and continue with the remaining tasks \
unchanged. Use submit_recovery with decision="skip" and an empty tasks \
array. Pick this when the task isn't strictly required (optional \
verification, nice-to-have polish) and skipping doesn't break the rest.

4. ABANDON: the goal can't be achieved. Use submit_recovery with \
decision="abandon" and an empty tasks array.

Heuristics:

- If [cycle_index] > 0 and your replacement would look similar to a prior \
failed attempt, prefer ABANDON. Repeating the same shape typically fails \
the same way.
- If [cycle_index] >= [max_cycles - 1], strongly prefer ABANDON.
- If the failure means the goal is fundamentally blocked (missing tool, \
broken external dep, contradictory requirement), choose ABANDON immediately.
- Prefer SPLIT over REPLAN when the failed task was just too big — it's a \
smaller, more conservative move.
- Prefer SKIP when the task is genuinely optional and the rest of the plan \
can succeed without it.

Budget heuristics (use the budget section if present):
- If cost or walltime is at >85% of its cap, strongly prefer ABANDON or SKIP \
over REPLAN/SPLIT — there isn't time/money left to attempt new work.
- If at >70% but the failure is recoverable, prefer SKIP over REPLAN. \
REPLAN replaces N tasks; you may not finish them.
- Below 50%, decide based on the strategic merits without budget pressure.

If a task previously skipped is listed under "Previously skipped tasks" \
and the failure mentions an artifact that the skipped task was supposed \
to produce (file, function, dependency), this is CASCADE FAILURE: prefer \
SPLIT to insert the missing piece, not SKIP again.

Don't replan to a "similar but slightly different" task list — that's how \
death-spirals start.|}

(* Default planner cap — planner usually does 1-3 LLM calls, 10 is plenty. *)
let default_planner_max_iters = 10

(* ===== Role spec constructors =====
   Each constructor returns [Agent_spec.validated]. The role
   constructions are known-valid by construction (the synthetic
   terminal tool is added to [tools] alongside [terminal=Tool{name}]),
   so we treat any [validate] failure as a Specs bug, not a caller
   error. *)

let validate_or_bug ~role spec =
  match Agent_spec.validate spec with
  | Ok v -> v
  | Error msg ->
      failwith
        (Printf.sprintf "Specs.%s: produced invalid spec — %s (Specs bug)"
           role msg)

let planner ?(system_prompt = default_planner_prompt)
    ?(max_iters = default_planner_max_iters) ?model ~tools () :
    Agent_spec.validated =
  Agent_spec.make
    ~name:"planner"
    ~mode:Planner
    ~system_prompt
    ~max_iters
    ?model
    ~terminal:(Agent_spec.Tool { name = submit_plan_name })
    ~tools:(tools @ [ submit_plan_tool ])
    ()
  |> validate_or_bug ~role:"planner"

let recovery ?(name = "recovery")
    ?(system_prompt = default_recovery_prompt)
    ?(max_iters = default_planner_max_iters) ?model ~tools () :
    Agent_spec.validated =
  Agent_spec.make
    ~name
    ~mode:Recovery
    ~system_prompt
    ~max_iters
    ?model
    ~terminal:(Agent_spec.Tool { name = submit_recovery_name })
    ~tools:(tools @ [ submit_recovery_tool ])
    ()
  |> validate_or_bug ~role:"recovery"

let executor ?(system_prompt = Agent_spec.default_system_prompt)
    ?(system_blocks = []) ?(strategy = Context.Strategy.flat)
    ?(max_iters = Agent_spec.default_max_iters) ?model ?(env = [])
    ~tools () : Agent_spec.validated =
  Agent_spec.make
    ~name:"executor"
    ~mode:Executor
    ~system_prompt
    ~system_blocks
    ~strategy
    ~max_iters
    ?model
    ~env
    ~terminal:(Agent_spec.Tool { name = submit_task_result_name })
    ~tools:(tools @ [ submit_task_result_tool ])
    ()
  |> validate_or_bug ~role:"executor"

let chat ?(system_prompt = Agent_spec.default_system_prompt)
    ?(system_blocks = []) ?(max_iters = Agent_spec.default_max_iters)
    ?model ~tools () : Agent_spec.validated =
  Agent_spec.make
    ~name:"agent"
    ~mode:Executor
    ~system_prompt
    ~system_blocks
    ~max_iters
    ?model
    ~terminal:Agent_spec.Free_text
    ~tools
    ()
  |> validate_or_bug ~role:"chat"

let subagent ?(system_prompt = Agent_spec.default_system_prompt)
    ?(max_iters = Agent_spec.default_max_iters) ?model ~tools () :
    Agent_spec.validated =
  Agent_spec.make
    ~name:"subagent"
    ~mode:Subagent
    ~system_prompt
    ~max_iters
    ?model
    ~terminal:Agent_spec.Free_text
    ~tools
    ()
  |> validate_or_bug ~role:"subagent"
