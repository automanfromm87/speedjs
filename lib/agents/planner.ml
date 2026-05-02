(** Planner agent — ReAct-style.

    Given a high-level goal, asks the LLM to decompose it into a small ordered
    list of actionable tasks. The planner is NOT a single LLM call — it runs
    a small ReAct loop where it can [load_skill], [view_file], and other
    read-only tools to gather context BEFORE submitting the plan. This lets
    the planner produce informed task descriptions that incorporate
    skill-specific structural rules (e.g. "split into components/ + hooks/")
    rather than guessing from a one-line skill index.

    The planner terminates by calling [submit_plan]; this is the
    [terminal_tools] argument to the inner loop. *)

open Types

let default_system_prompt =
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

(** The synthetic terminal tool the planner is forced to call. The handler
    is a no-op — planning consumes the [Tool_use] block directly, the
    executor never invokes the registered handler. *)
let submit_plan_tool : tool_def =
  {
    idempotent = true;
    timeout_sec = None;
    category = "meta";
    capabilities = [ Terminal ];
    allowed_modes = [ Planner ];
    classify_error = default_classify_error;
    name = "submit_plan";
    description =
      "Submit the structured plan that decomposes the user's goal into \
       ordered actionable tasks.";
    input_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "title",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Short title of the plan (max ~10 words)" );
                    ] );
                ( "tasks",
                  `Assoc
                    [
                      ("type", `String "array");
                      ( "description",
                        `String
                          "Ordered tasks, each one actionable and \
                           self-contained" );
                      ( "items",
                        `Assoc
                          [
                            ("type", `String "object");
                            ( "properties",
                              `Assoc
                                [
                                  ( "description",
                                    `Assoc
                                      [
                                        ( "type",
                                          `String "string" );
                                        ( "description",
                                          `String
                                            "Self-contained task description"
                                        );
                                      ] );
                                ] );
                            ("required", `List [ `String "description" ]);
                          ] );
                    ] );
              ] );
          ("required", `List [ `String "title"; `String "tasks" ]);
        ];
    handler = (fun _ -> Ok "(submit_plan handled by planner orchestrator)");
  }

(** Extract the [submit_plan] tool_use input from an LLM response. *)
let extract_plan_input (response : llm_response) : Yojson.Safe.t option =
  List.find_map
    (function
      | Tool_use { name = "submit_plan"; input; _ } -> Some input | _ -> None)
    response.content

(** Parse the [submit_plan] input JSON into a [plan]. Errors propagate as
    [Plan_invalid] via the agent_result. *)
let parse_plan ~goal (input : Yojson.Safe.t) : (plan, agent_error) Result.t =
  match input with
  | `Assoc fields ->
      let title = Json_decode.get_string_field_or "title" ~default:"(untitled)" fields in
      (match List.assoc_opt "tasks" fields with
      | Some (`List items) ->
          let tasks =
            List.mapi
              (fun i j ->
                let description =
                  match j with
                  | `Assoc fs ->
                      Json_decode.get_string_field_or "description" ~default:"" fs
                  | _ -> ""
                in
                { index = i + 1; description })
              items
            |> List.filter (fun t -> t.description <> "")
          in
          if tasks = [] then
            Error (Plan_invalid "submit_plan returned empty tasks")
          else Ok { title; goal; tasks }
      | _ -> Error (Plan_invalid "submit_plan missing 'tasks' array"))
  | _ -> Error (Plan_invalid "submit_plan input is not an object")

(** Default cap on planner ReAct iterations. The planner usually does 1-3
    LLM calls (load skills + submit). 10 is plenty of headroom; if the
    planner is looping more than this it's stuck and should fail loudly. *)
let default_planner_max_iter = 10

(** Run the planner as a small ReAct loop.

    Unlike the executor, the planner has [submit_plan] as its terminal tool
    and is expected to also use [research_tools] (load_skill, view_file,
    bash for ls/cat) to gather context BEFORE submitting. This lets the
    planner produce informed task descriptions that respect skill-specified
    structural rules.

    [research_tools] should typically be the executor's tool list — same
    LLM, same context budget. The planner self-restricts via system prompt
    (don't write files, only read). *)
let plan ?(system_prompt = default_system_prompt)
    ?(max_iterations = default_planner_max_iter)
    ?(research_tools : tool_def list = []) ~goal () :
    (plan, agent_error) Result.t =
  let capture (r : (plan, agent_error) Result.t) : Trace.capture_result =
    match r with
    | Ok p ->
        Trace.ok_capture
          ~output:(Printf.sprintf "%d task(s)" (List.length p.tasks))
          ~tokens:Trace.zero_tokens ~cost_delta:0.0
    | Error e ->
        {
          output = "";
          tokens = Trace.zero_tokens;
          cost_delta = 0.0;
          ok = false;
          error = Some (agent_error_pp e);
        }
  in
  Trace.span_current ~kind:Trace.Phase ~name:"planner"
    ~input_summary:goal ~capture (fun () ->
  Effect.perform
    (Effects.Log
       (Printf.sprintf "[planner] decomposing goal: %s"
          (if String.length goal > 80 then String.sub goal 0 80 ^ "..."
           else goal)));
  (* Hard gate: planner sees only tools whose [allowed_modes] include
     [Planner]. Mutating / Exec tools are filtered out at the type
     level — even if the prompt fails to deter, the tool is literally
     not in the surface presented to the model. *)
  let research_tools = tools_for_mode Planner research_tools in
  let planner_tools = research_tools @ [ submit_plan_tool ] in
  let initial_messages =
    [
      {
        role = User;
        content =
          [
            Text
              (Printf.sprintf
                 "Goal:\n\n%s\n\nFirst, load any relevant skills from the \
                  available_skills index. Then call submit_plan with the \
                  structured task list."
                 goal);
          ];
      };
    ]
  in

  (* Planner-specific ReAct loop. Tool_choice = Tc_auto lets the model pick
     freely between research tools and submit_plan. We terminate by raising
     [Agent.Task_terminal_called] when submit_plan is called. *)
  let rec loop messages iter =
    if iter > max_iterations then
      Error
        (Plan_invalid
           (Printf.sprintf "planner exceeded %d iterations without calling submit_plan"
              max_iterations))
    else
      let args : llm_call_args =
        {
          messages;
          tools = planner_tools;
          system_override = Some system_prompt;
          tool_choice = Tc_auto;
        }
      in
      Effect.perform
        (Effects.Log (Printf.sprintf "[planner] iter %d" iter));
      let response = Effect.perform (Effects.Llm_complete args) in
      match response.stop_reason with
      | End_turn ->
          Error
            (Plan_invalid
               "planner ended turn without calling submit_plan — \
                model lost the protocol")
      | Tool_use_stop -> (
          (* Check submit_plan terminal first. *)
          match extract_plan_input response with
          | Some input -> parse_plan ~goal input
          | None ->
              (* Non-terminal tool calls — reuse the agent's dispatch. *)
              let assistant =
                { role = Assistant; content = response.content }
              in
              let tool_results = Step.dispatch_tool_uses response.content in
              let user_turn = { role = User; content = tool_results } in
              loop (messages @ [ assistant; user_turn ]) (iter + 1))
      | Max_tokens ->
          Error
            (Plan_invalid "planner: model hit max_tokens before submitting plan")
      | Stop_sequence | Other _ ->
          Error
            (Plan_invalid
               "planner: unexpected stop reason without submit_plan")
  in
  loop initial_messages 1)

(* ===== Plan recovery =====

   When a task fails (after retries), ask the planner what to do. The
   planner sees the original goal, completed tasks, the failed task +
   error, remaining pending tasks, prior recovery failures, and the
   cycle index. It returns one of four decisions: *)

type recovery_decision =
  | Replan of task list
      (** Replace the failed task AND all remaining tasks with a new
          ordered list. Use when the original strategy is broken and
          a different approach is needed. *)
  | Split of task list
      (** Replace ONLY the failed task with smaller sub-tasks. The
          remaining tasks stay. Use when the failed task was too
          coarse and decomposing it would unblock progress. *)
  | Skip
      (** Drop the failed task and continue with the remaining tasks
          unchanged. Use when the task isn't strictly required for the
          goal (e.g. an optional verification step). *)
  | Abandon
      (** Give up cleanly; the summarizer reports what was achieved. *)

let recovery_system_prompt =
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

Don't replan to a "similar but slightly different" task list — that's how \
death-spirals start.|}

let submit_recovery_tool : tool_def =
  {
    idempotent = true;
    timeout_sec = None;
    category = "meta";
    capabilities = [ Terminal ];
    allowed_modes = [ Recovery ];
    classify_error = default_classify_error;
    name = "submit_recovery";
    description =
      "Submit the recovery decision after seeing a task failure.";
    input_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "decision",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "enum",
                        `List
                          [
                            `String "replan";
                            `String "split";
                            `String "skip";
                            `String "abandon";
                          ] );
                    ] );
                ( "tasks",
                  `Assoc
                    [
                      ("type", `String "array");
                      ( "description",
                        `String
                          "Replacement tasks if decision=replan; empty if abandon"
                      );
                      ( "items",
                        `Assoc
                          [
                            ("type", `String "object");
                            ( "properties",
                              `Assoc
                                [
                                  ( "description",
                                    `Assoc [ ("type", `String "string") ] );
                                ] );
                            ("required", `List [ `String "description" ]);
                          ] );
                    ] );
              ] );
          ("required", `List [ `String "decision" ]);
        ];
    handler = (fun _ -> Ok "(submit_recovery handled by orchestrator)");
  }

let extract_recovery_input (response : llm_response) : Yojson.Safe.t option =
  List.find_map
    (function
      | Tool_use { name = "submit_recovery"; input; _ } -> Some input
      | _ -> None)
    response.content

let parse_tasks_field fs =
  Json_decode.get_list_field_or_empty "tasks" fs
  |> List.mapi (fun i j ->
         let description =
           match j with
           | `Assoc tfs ->
               Json_decode.get_string_field_or "description" ~default:"" tfs
           | _ -> ""
         in
         { index = i + 1; description })
  |> List.filter (fun t -> t.description <> "")

let parse_recovery_decision (input : Yojson.Safe.t) :
    (recovery_decision, agent_error) Result.t =
  let open Json_decode in
  let with_tasks ctor name fs =
    let new_tasks = parse_tasks_field fs in
    if new_tasks = [] then
      Error (Printf.sprintf "%s with empty tasks list" name)
    else Ok (ctor new_tasks)
  in
  let decode fs =
    let* decision = get_string_field "decision" fs in
    match decision with
    | "abandon" -> Ok Abandon
    | "skip" -> Ok Skip
    | "replan" -> with_tasks (fun ts -> Replan ts) "replan" fs
    | "split" -> with_tasks (fun ts -> Split ts) "split" fs
    | other -> Error (Printf.sprintf "unknown recovery decision %S" other)
  in
  Result.map_error (fun msg -> Plan_invalid msg)
    (with_object_input input decode)

(** ReAct-style recovery. Like [plan], allows the recovery planner to
    [load_skill] / [view_file] / [bash 'cat ...'] to inspect the actual
    failed code before deciding replan vs abandon.

    [prior_failures] are [(task_description, error)] pairs from earlier
    recovery cycles in this plan. [cycle_index] is which recovery cycle
    we're currently in (0-indexed); [max_cycles] is the orchestrator's
    hard cap. The planner uses these to bias toward abandon when prior
    replans look similar or when we're at the last cycle. *)
let recover ?(max_iterations = default_planner_max_iter)
    ?(research_tools : tool_def list = []) ?(prior_failures = [])
    ?(cycle_index = 0) ?(max_cycles = 2) ~goal ~completed ~failed_task
    ~failed_error ~remaining () :
    (recovery_decision, agent_error) Result.t =
  let capture (r : (recovery_decision, agent_error) Result.t)
      : Trace.capture_result =
    match r with
    | Ok decision ->
        let label =
          match decision with
          | Replan _ -> "REPLAN"
          | Split _ -> "SPLIT"
          | Skip -> "SKIP"
          | Abandon -> "ABANDON"
        in
        Trace.ok_capture ~output:label ~tokens:Trace.zero_tokens
          ~cost_delta:0.0
    | Error e ->
        {
          output = "";
          tokens = Trace.zero_tokens;
          cost_delta = 0.0;
          ok = false;
          error = Some (agent_error_pp e);
        }
  in
  Trace.span_current ~kind:Trace.Phase
    ~name:(Printf.sprintf "recovery#%d" cycle_index)
    ~input_summary:(Printf.sprintf "task %d: %s" failed_task.index failed_error)
    ~capture (fun () ->
  Effect.perform
    (Effects.Log
       (Printf.sprintf
          "[recovery] cycle %d/%d task %d failed (%s), consulting planner"
          cycle_index max_cycles failed_task.index failed_error));
  let prior_failures_section =
    match prior_failures with
    | [] -> "Prior recovery failures: (none — this is the first cycle)"
    | _ ->
        let lines =
          List.map
            (fun (desc, err) -> Printf.sprintf "  - %s — error: %s" desc err)
            prior_failures
        in
        Printf.sprintf "Prior recovery failures (%d):\n%s"
          (List.length prior_failures) (String.concat "\n" lines)
  in
  let body =
    Printf.sprintf
      "Goal: %s\n\n\
       Completed tasks (%d):\n\
       %s\n\n\
       FAILED task: %s\n\n\
       Error: %s\n\n\
       Remaining tasks (%d):\n\
       %s\n\n\
       %s\n\n\
       Recovery cycle: %d of max %d\n\n\
       First, investigate (load_skill, view_file, bash for cat/ls) — then \
       decide replan or abandon by calling submit_recovery."
      goal
      (List.length completed)
      (String.concat "\n" (List.map (fun t -> "  - " ^ t) completed))
      failed_task.description failed_error (List.length remaining)
      (String.concat "\n" (List.map (fun t -> "  - " ^ t) remaining))
      prior_failures_section cycle_index max_cycles
  in
  let research_tools = tools_for_mode Recovery research_tools in
  let recovery_tools = research_tools @ [ submit_recovery_tool ] in
  let initial_messages = [ { role = User; content = [ Text body ] } ] in

  let rec loop messages iter =
    if iter > max_iterations then
      Error
        (Plan_invalid
           (Printf.sprintf
              "recovery exceeded %d iterations without calling submit_recovery"
              max_iterations))
    else
      let args : llm_call_args =
        {
          messages;
          tools = recovery_tools;
          system_override = Some recovery_system_prompt;
          tool_choice = Tc_auto;
        }
      in
      Effect.perform
        (Effects.Log (Printf.sprintf "[recovery] iter %d" iter));
      let response = Effect.perform (Effects.Llm_complete args) in
      match response.stop_reason with
      | End_turn ->
          Error
            (Plan_invalid
               "recovery ended turn without submit_recovery — \
                lost protocol")
      | Tool_use_stop -> (
          match extract_recovery_input response with
          | Some input -> parse_recovery_decision input
          | None ->
              let assistant =
                { role = Assistant; content = response.content }
              in
              let tool_results = Step.dispatch_tool_uses response.content in
              let user_turn = { role = User; content = tool_results } in
              loop (messages @ [ assistant; user_turn ]) (iter + 1))
      | Max_tokens | Stop_sequence | Other _ ->
          Error
            (Plan_invalid
               "recovery: unexpected stop reason without submit_recovery")
  in
  loop initial_messages 1)
