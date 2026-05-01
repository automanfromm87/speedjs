(** Plan-Act flow with helix-style resilience and persistent executor memory.

    Phases:
      SURVEY     → optional WorkspaceSurveyor scan (when [~working_dir] is set)
      PLANNING   → planner produces ordered task list (sees workspace brief)
      EXECUTING  → loop tasks; each task is one [Agent.run_for_task] call
                   that EXTENDS the executor's accumulated message history
                   instead of starting fresh. Early tasks' tool_calls become
                   cache prefix for later ones; the model "remembers".
      RECOVERING → planner picks REPLAN or ABANDON when retries are exhausted
      SUMMARIZING → executor synthesizes the final answer

    Persistent memory ([~memory_dir]):
      - executor.json holds the executor's accumulated [message list]
      - loaded on startup (so a continue.sh-style follow-up run picks up
        where the last one left off)
      - saved after every successful task
      - on task failure: rolled back to the pre-task checkpoint length
        (the failed ReAct trail would poison the next attempt)

    Failure handling (4 layers, helix-inspired):
      1. [max_iterations] cap inside one task's ReAct loop
      2. submit_task_result with success=false → caller knows the task failed
      3. Per-task retry (default 1 retry on failure)
      4. Plan recovery (planner replans or abandons)

    Reuses the same effect handler stack — every LLM call (surveyor, planner,
    recovery, executor, summarizer) goes through [Effects.Llm_complete], so
    cost / streaming / checkpoint / loop_guard / budget all work transparently. *)

open Types

(* ===== submit_task_result: terminal tool for explicit task closure =====

   Synthetic tool that lets the executor signal task completion (or
   definitive failure) explicitly, instead of relying on
   stop_reason=End_turn. The handler is never executed — the agent loop
   intercepts via [terminal_tools] and raises [Task_terminal_called]
   before tool dispatch. *)

let submit_task_result_name = "submit_task_result"

let submit_task_result_tool : tool_def =
  {
    (* Synthetic terminal tool: handler never invoked. *)
    idempotent = true;
    timeout_sec = None;
    category = "meta";
    name = submit_task_result_name;
    description =
      "Submit the FINAL outcome for the current task. Call this exactly \
       once when the task is fully complete (or has definitively failed \
       and cannot be salvaged). The agent loop will end immediately after \
       this call. Do NOT call any other tools in the same response.";
    input_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "success",
                  `Assoc
                    [
                      ("type", `String "boolean");
                      ( "description",
                        `String
                          "true if the task was completed successfully; \
                           false if it failed and cannot be salvaged" );
                    ] );
                ( "result",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Concise summary of what was accomplished (or \
                           empty if success=false)" );
                    ] );
                ( "error",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Diagnostic explanation if success=false; empty \
                           string when success=true" );
                    ] );
              ] );
          ("required", `List [ `String "success"; `String "result" ]);
        ];
    handler =
      (fun _ ->
        Error
          "submit_task_result is a terminal tool — its handler should \
           never be invoked");
  }

let parse_task_submit (input : Yojson.Safe.t) : task_submit =
  match input with
  | `Assoc fs ->
      let bool_field name default =
        match List.assoc_opt name fs with
        | Some (`Bool b) -> b
        | _ -> default
      in
      let str_field name default =
        match List.assoc_opt name fs with
        | Some (`String s) -> s
        | _ -> default
      in
      {
        ts_success = bool_field "success" true;
        ts_result = str_field "result" "";
        ts_error = str_field "error" "";
      }
  | _ ->
      { ts_success = false; ts_result = ""; ts_error = "non-object input" }

(** Run a single task with structured outcome.

    Adds [submit_task_result] to the tool list, instructs the model to
    call it for explicit closure, returns a [task_run_outcome] that
    distinguishes success/failure/waiting/structural-error.

    [prior_messages]: seed message history. The new task is appended as
    a fresh user turn — or merged with synthetic Tool_results if the
    prior turn ended with an unanswered submit_task_result tool_use
    (preserves Anthropic's strict alternation). Pass [[]] for fresh
    per-task context; pass the executor's accumulated memory for
    helix-style continuity (early tasks' tool_calls become cache prefix
    for later ones, and the model "remembers" what was done). *)
let run_for_task ?(max_iterations = Agent.default_max_iterations)
    ?(strategy = Context.Strategy.flat)
    ?(prior_messages : message list = [])
    ?(env : (string * string) list = []) ~task_description ~tools () :
    task_run_outcome =
  let tools_with_submit = tools @ [ submit_task_result_tool ] in
  let user_query =
    Printf.sprintf
      "%s\n\nWhen this task is fully complete, call the \
       submit_task_result tool with success=true and a concise result \
       summary. If you cannot complete the task, call submit_task_result \
       with success=false and an error explanation."
      task_description
  in
  (* If [prior_messages] ends with a dangling submit_task_result tool_use
     (executor memory across tasks), close it by merging a synthetic
     Tool_result with the new task text into a single User turn. *)
  let conv =
    match Conversation.of_messages prior_messages with
    | Ok c -> c
    | Error msg ->
        failwith ("run_for_task: malformed prior_messages — " ^ msg)
  in
  let conv =
    if Conversation.is_dangling conv then
      Conversation.close_dangling_with_ack
        ~ack:"(submit_task_result acknowledged — proceeding to next task)"
        ~extra:[ Text user_query ] conv
    else Conversation.push_user_text conv user_query
  in
  let ctx =
    let base =
      Context.empty
      |> Context.with_tools tools_with_submit
      |> Context.with_conversation conv
    in
    List.fold_left
      (fun c (tag, body) -> Context.with_env ~tag ~body c)
      base env
  in
  let final_messages c =
    Conversation.to_messages (Context.conversation c)
  in
  try
    match
      Agent.run_loop ~max_iterations ~strategy
        ~terminal_tools:[ submit_task_result_name ] ~ctx ()
    with
    | Ok (answer, ctx) ->
        Task_done_implicit { answer; messages = final_messages ctx }
    | Error (reason, ctx) ->
        Task_run_failed { reason; messages = final_messages ctx }
  with
  | Agent.Task_terminal_called { input; ctx_so_far; _ } ->
      Task_done_explicit
        {
          submit = parse_task_submit input;
          messages = final_messages ctx_so_far;
        }
  | Agent.Wait_for_user { tool_use_id; question; ctx_so_far } ->
      Task_run_waiting
        { tool_use_id; question; messages = final_messages ctx_so_far }

let summarizer_system_prompt =
  {|You are a synthesizer. The user gave a goal. A plan was created and each \
task was executed. You will be given the goal, the plan, and each task's \
result. Write a single concise final answer that addresses the goal. Do not \
narrate the process — just deliver the answer. If a task produced concrete \
output (numbers, code, lists), include it directly.|}

let format_task_results plan results =
  if List.length plan.tasks <> List.length results then
    "(plan / results length mismatch)"
  else
    let lines =
      List.map2
        (fun task result ->
          match result with
          | Ok answer ->
              Printf.sprintf "Task %d: %s\n  → %s" task.index task.description
                answer
          | Error err ->
              Printf.sprintf "Task %d: %s\n  ✗ FAILED: %s" task.index
                task.description err)
        plan.tasks results
    in
    String.concat "\n\n" lines

(** Synthesizer LLM call to produce a final answer. *)
let summarize ~plan ~results () : agent_result =
  Effect.perform (Effects.Log "[summarizer] synthesizing final answer");
  let user_msg =
    Printf.sprintf
      "Goal:\n%s\n\nPlan: %s\n\nTask results:\n%s\n\nWrite the final answer."
      plan.goal plan.title (format_task_results plan results)
  in
  let args : llm_call_args =
    {
      messages = [ user_text_message user_msg ];
      tools = [];
      system_override = Some summarizer_system_prompt;
      tool_choice = Tc_auto;
    }
  in
  let response = Effect.perform (Effects.Llm_complete args) in
  Ok (Agent.extract_final_text response.content)

let executor_memory_name = "executor"

(* ===== Workspace brief ===== *)

(** Survey the workspace and return [(env_blocks, planner_goal_prefix)].
    The env block goes into the executor's [Context.env] (system-prompt
    layer of the cache, shared across all tasks); the prefix is added to
    the planner's goal so the planner sees the file layout while drafting
    tasks. *)
let survey_workspace ~working_dir : (string * string) list * string =
  match working_dir with
  | None -> ([], "")
  | Some dir ->
      let brief = Workspace_surveyor.survey ~working_dir:dir in
      if brief = "" then ([], "")
      else
        let env =
          [
            ( "workspace_brief",
              Printf.sprintf "Project root: %s\n\n%s" dir brief );
          ]
        in
        let planner_prefix =
          Workspace_surveyor.render_brief ~working_dir:dir brief
        in
        (env, planner_prefix)

(** Run one task against the executor's accumulated memory.

    [env] blocks (e.g. workspace_brief) ride at the system-prompt layer
    of the prompt cache, so they're shared across all tasks for free. *)
let exec_one_task ~tools ~max_iterations ~strategy ~env
    ~(prior_messages : message list) ~current_task :
    [ `Done of string * message list
    | `Failed of string * message list ] =
  match
    run_for_task ~max_iterations ~strategy ~prior_messages ~env
      ~task_description:current_task.description ~tools ()
  with
  | Task_done_explicit { submit; messages } ->
      if submit.ts_success then `Done (submit.ts_result, messages)
      else
        `Failed
          ( (if submit.ts_error <> "" then submit.ts_error
             else "task reported failure without details"),
            messages )
  | Task_done_implicit { answer; messages } -> `Done (answer, messages)
  | Task_run_failed { reason; messages } ->
      `Failed (agent_error_pp reason, messages)
  | Task_run_waiting _ ->
      `Failed
        ( "task called ask_user — pause-tool not supported in plan-act mode",
          prior_messages )

(* ===== Configuration ===== *)

type config = {
  skip_summarizer : bool;
  max_iterations_per_task : int;
  max_task_retries : int;
  max_recoveries : int;
  working_dir : string option;
  memory_dir : string option;
  model : string;
  planner_system_prompt : string option;
  executor_strategy : unit -> Context.Strategy.t;
      (** Factory, not a [Strategy.t] value: stateful strategies (e.g.
          [sliding_window_at]) carry a per-instance [cut_at] ref that
          must be fresh per run. The factory is called once per
          [run] invocation. *)
}

(** Default executor strategy factory: soft-trigger sliding window.
    The executor accumulates messages across tasks (helix-style memory)
    and within each task — unbounded growth blows past 100K tokens on
    long runs. Frozen cut anchor keeps the cached prompt prefix stable
    between trims (~one trim per ~30 calls). *)
let default_executor_strategy () =
  Context.Strategy.sliding_window_at ~trigger_at:60 ~keep_recent:30

let default_config =
  {
    skip_summarizer = false;
    max_iterations_per_task = Agent.default_max_iterations;
    max_task_retries = 1;
    max_recoveries = 2;
    working_dir = None;
    memory_dir = None;
    model = Anthropic.default_model;
    planner_system_prompt = None;
    executor_strategy = default_executor_strategy;
  }

(** Top-level plan-act run.

    [working_dir] enables [Workspace_surveyor] before planning. The
    brief is injected into the planner's goal AND prepended to the
    executor's first task message.

    [memory_dir] enables cross-run executor memory: messages are loaded
    on startup (continue.sh-style resume) and persisted after each
    successful task. *)
let run ?(config = default_config) ~goal ~tools () : agent_result =
  let { skip_summarizer; max_iterations_per_task = max_iter;
        max_task_retries; max_recoveries; working_dir; memory_dir;
        model; planner_system_prompt;
        executor_strategy = executor_strategy_factory } = config in
  (* Construct a fresh strategy ONCE per run. Sliding-window-style
     strategies are stateful (cut_at ref); the same closure must be
     reused across every executor LLM call within a run so the cache
     prefix stays stable, but a NEW closure must be made for the next
     run. *)
  let executor_strategy = executor_strategy_factory () in

  (* Step 1: optional workspace survey. One LLM call; the brief is
     reused across planner + executor so marginal value is high.
     Executor receives env blocks (system-prompt layer cache); planner
     gets the brief inlined in its goal text since its system prompt is
     already specialized. *)
  let exec_env, planner_prefix = survey_workspace ~working_dir in

  (* Step 2: plan. *)
  let goal_for_planner =
    if planner_prefix = "" then goal
    else planner_prefix ^ "\n\n--- USER GOAL ---\n" ^ goal
  in
  match
    Planner.plan ?system_prompt:planner_system_prompt
      ~research_tools:tools ~goal:goal_for_planner ()
  with
  | Error e -> Error e
  | Ok initial_plan ->
      Effect.perform
        (Effects.Log
           (Printf.sprintf "[plan] %s — %d task(s)" initial_plan.title
              (List.length initial_plan.tasks)));
      List.iter
        (fun (t : task) ->
          Effect.perform
            (Effects.Log (Printf.sprintf "  %d. %s" t.index t.description)))
        initial_plan.tasks;

      (* Step 3: executor memory (loads from disk if memory_dir set). *)
      let exec_mem =
        Memory.create ~model ?dir:memory_dir ~name:executor_memory_name ()
      in
      if Memory.length exec_mem > 0 then
        Effect.perform
          (Effects.Log
             (Printf.sprintf
                "[memory] loaded %d prior executor message(s)"
                (Memory.length exec_mem)));

      (* Step 4: drive tasks one-by-one with persistent memory +
         retry/recovery. [prior_failures] carries (description, error)
         pairs from earlier recovery cycles so the recovery planner can
         see whether a similar replan has already failed and prefer
         abandon over yet another similar-shape replan. *)
      let rec drive ~pending ~acc ~recoveries ~prior_failures =
        match pending with
        | [] ->
            let results = List.map snd (List.rev acc) in
            let any_failed = List.exists Result.is_error results in
            let summary_plan =
              { initial_plan with tasks = List.rev_map fst acc }
            in
            if any_failed && recoveries >= max_recoveries then
              if skip_summarizer then
                Ok
                  (Printf.sprintf "Plan: %s (with failures)\n\n%s"
                     initial_plan.title
                     (format_task_results summary_plan results))
              else summarize ~plan:summary_plan ~results ()
            else if skip_summarizer then
              Ok
                (Printf.sprintf "Plan: %s\n\n%s" initial_plan.title
                   (format_task_results summary_plan results))
            else summarize ~plan:summary_plan ~results ()
        | task :: rest ->
            Effect.perform
              (Effects.Log
                 (Printf.sprintf "[task %d/%d] %s" task.index
                    (List.length pending + List.length acc)
                    task.description));

            let checkpoint = Memory.checkpoint exec_mem in

            let rec attempt n =
              let prior = Memory.to_messages exec_mem in
              match
                exec_one_task ~tools ~max_iterations:max_iter
                  ~strategy:executor_strategy ~env:exec_env
                  ~prior_messages:prior ~current_task:task
              with
              | `Done (answer, messages) ->
                  Memory.set exec_mem messages;
                  Memory.persist exec_mem;
                  `Ok answer
              | `Failed (err, _) ->
                  Memory.restore exec_mem checkpoint;
                  if n < max_task_retries then begin
                    Effect.perform
                      (Effects.Log
                         (Printf.sprintf
                            "[task %d] failed (%s) — retry %d/%d"
                            task.index err (n + 1) max_task_retries));
                    attempt (n + 1)
                  end
                  else `Exhausted err
            in
            match attempt 0 with
            | `Ok answer ->
                drive ~pending:rest
                  ~acc:((task, Ok answer) :: acc)
                  ~recoveries ~prior_failures
            | `Exhausted err ->
                handle_recovery ~task ~err ~rest ~acc ~recoveries
                  ~prior_failures
      and handle_recovery ~task ~err ~rest ~acc ~recoveries ~prior_failures =
        Effect.perform
          (Effects.Log
             (Printf.sprintf
                "[task %d] exhausted retries — consulting planner for recovery"
                task.index));
        if recoveries >= max_recoveries then begin
          Effect.perform
            (Effects.Log
               (Printf.sprintf
                  "[recovery] budget exhausted (%d/%d), abandoning"
                  recoveries max_recoveries));
          drive ~pending:rest
            ~acc:((task, Error err) :: acc)
            ~recoveries ~prior_failures
        end
        else
          let completed_descs =
            List.rev_map (fun (t, _) -> t.description) acc
          in
          let remaining_descs =
            List.map (fun (t : task) -> t.description) rest
          in
          let decision =
            Planner.recover ~research_tools:tools
              ~prior_failures:(List.rev prior_failures)
              ~cycle_index:recoveries ~max_cycles:max_recoveries ~goal
              ~completed:completed_descs ~failed_task:task
              ~failed_error:err ~remaining:remaining_descs ()
          in
          (* On replan or abandon, the current failure becomes part of
             prior_failures for any FUTURE recovery cycle in this run. *)
          let next_prior_failures =
            (task.description, err) :: prior_failures
          in
          match decision with
          | Error _ ->
              drive ~pending:rest
                ~acc:((task, Error err) :: acc)
                ~recoveries:(recoveries + 1)
                ~prior_failures:next_prior_failures
          | Ok Abandon ->
              Effect.perform
                (Effects.Log
                   "[recovery] planner chose ABANDON — surfacing failure to summarizer");
              drive ~pending:[]
                ~acc:((task, Error err) :: acc)
                ~recoveries:(recoveries + 1)
                ~prior_failures:next_prior_failures
          | Ok (Replan new_tasks) ->
              Effect.perform
                (Effects.Log
                   (Printf.sprintf
                      "[recovery] planner chose REPLAN — %d new task(s)"
                      (List.length new_tasks)));
              let renumbered =
                List.mapi
                  (fun i (t : task) ->
                    { t with index = List.length acc + 1 + i })
                  new_tasks
              in
              drive ~pending:renumbered ~acc
                ~recoveries:(recoveries + 1)
                ~prior_failures:next_prior_failures
      in
      drive ~pending:initial_plan.tasks ~acc:[] ~recoveries:0
        ~prior_failures:[]
