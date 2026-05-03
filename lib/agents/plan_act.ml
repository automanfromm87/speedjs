(** Plan-Act flow with helix-style resilience and persistent executor memory.

    The orchestration is expressed as a recursive {!Workflow.t}:

    {ol
    {- {!plan_or_resume_flow} produces an [initial_plan] (planner LLM
       call, or resumed [plan_state.json]).}
    {- {!drive_flow} walks the [pending] task list. Each task runs
       through {!run_task_flow} (with retry + memory checkpoint /
       rollback). On success → bind result, recurse with [rest]. On
       failure → {!handle_failure_flow} consults {!Planner.recover}
       and pivots [pending] (replan / split / skip / abandon).}
    {- {!finalize_flow} produces the synthesized answer (summarizer
       LLM call, or skip-summarizer formatted dump).}}

    Persistence is side-effect at bind boundaries: {!Memory} +
    {!Plan_state} writes happen between binds via [Workflow.action].
    All LLM calls go through [Effects.Llm_complete], so cost / tape /
    cache / retry middleware applies transparently to every leaf. *)

open Types

(** Structured payload submitted via [submit_task_result]. *)
type task_submit = {
  ts_success : bool;
  ts_result : string;
  ts_error : string;
}

(** Outcome of a single per-task ReAct run. *)
type task_run_outcome =
  | Task_done_explicit of {
      submit : task_submit;
      messages : message list;
    }
  | Task_done_implicit of {
      answer : string;
      messages : message list;
    }
  | Task_run_failed of { reason : agent_error; messages : message list }
  | Task_run_waiting of {
      tool_use_id : Id.Tool_use_id.t;
      question : string;
      messages : message list;
    }

let submit_task_result_name = Specs.submit_task_result_name

let parse_task_submit (input : Yojson.Safe.t) : task_submit =
  match input with
  | `Assoc fs ->
      let open Json_decode in
      {
        ts_success = get_bool_field_or "success" ~default:true fs;
        ts_result = get_string_field_or "result" ~default:"" fs;
        ts_error = get_string_field_or "error" ~default:"" fs;
      }
  | _ ->
      { ts_success = false; ts_result = ""; ts_error = "non-object input" }

(** Translate one {!Agent.output} into a task-run outcome. The 4-way
    mapping exists because plan-act distinguishes explicit submit
    (with success/failure semantics) from implicit End_turn / from
    structural failure / from pause-tool. *)
let outcome_of_output (output : Agent.output) : task_run_outcome =
  match output with
  | Agent.Terminal_tool { name; payload; messages }
    when name = submit_task_result_name ->
      Task_done_explicit { submit = parse_task_submit payload; messages }
  | Agent.Terminal_tool { name; messages; _ } ->
      Task_run_failed
        {
          reason =
            Plan_invalid
              (Printf.sprintf
                 "executor terminated on unexpected tool %S" name);
          messages;
        }
  | Agent.Done { answer; messages } ->
      Task_done_implicit { answer; messages }
  | Agent.Failed { reason; messages } -> Task_run_failed { reason; messages }
  | Agent.Waiting { tool_use_id; question; messages } ->
      Task_run_waiting { tool_use_id; question; messages }

(** Build the resume conversation for an executor task: validate
    [prior_messages], close any dangling [submit_task_result]
    tool_use, append the new user query as a fresh User turn (or
    merged with the ack). Returns the resume message list, or a
    typed error if the prior history is malformed / its dangling
    tool_use isn't [submit_task_result]. *)
let build_resume_messages ~prior_messages ~user_query :
    (message list, agent_error) Result.t =
  match Conversation.of_messages prior_messages with
  | Error msg ->
      Error
        (Plan_invalid
           ("run_for_task: malformed prior_messages — " ^ msg))
  | Ok conv when not (Conversation.is_dangling conv) ->
      Ok (Conversation.to_messages
            (Conversation.push_user_text conv user_query))
  | Ok conv ->
      let dangling = Conversation.dangling_tool_use_names conv in
      if List.for_all (fun n -> n = submit_task_result_name) dangling then
        Ok
          (Conversation.to_messages
             (Conversation.close_dangling_with_ack
                ~ack:
                  (Printf.sprintf
                     "(%s acknowledged — proceeding to next task)"
                     submit_task_result_name)
                ~extra:[ Text user_query ] conv))
      else
        Error
          (Plan_invalid
             (Printf.sprintf
                "run_for_task: prior_messages has dangling tool_use(s) \
                 [%s] that don't match %s"
                (String.concat "," dangling)
                submit_task_result_name))

(** Workflow form of one task's ReAct run. Returns
    [task_run_outcome] always wrapped in [Ok] (errors are encoded as
    [Task_run_failed], not flow-level errors). *)
(* Workflow-shaped wrappers for the two effect emit paths used by
   the drive loop. They hide the [action (fun () -> Effect.perform ...)]
   plumbing so the surrounding flow reads as a sequence of named
   events. *)
let log_event ev = Workflow.action (fun () -> Effect.perform (Effects.Event_log ev))
let log_line s = Workflow.action (fun () -> Effect.perform (Effects.Log s))

let task_attempt_flow ~spec ~prior_messages ~task_description :
    task_run_outcome Workflow.t =
  let open Workflow in
  let user_query =
    Printf.sprintf
      "%s\n\nWhen this task is fully complete, call the \
       submit_task_result tool with success=true and a concise result \
       summary. If you cannot complete the task, call submit_task_result \
       with success=false and an error explanation."
      task_description
  in
  match build_resume_messages ~prior_messages ~user_query with
  | Error reason -> pure (Task_run_failed { reason; messages = prior_messages })
  | Ok resume_messages ->
      let+ output = leaf spec (Agent.Resume resume_messages) in
      outcome_of_output output

(** Public single-task driver. Preserved for back-compat with tests
    and any external caller. Internally delegates to the workflow form. *)
let run_for_task ?(max_iterations = Agent.default_max_iterations)
    ?(strategy = Context.Strategy.flat)
    ?(system_prompt = Agent.default_system_prompt)
    ?(system_blocks : (string * string) list = [])
    ?(prior_messages : message list = [])
    ?(env : (string * string) list = []) ~task_description ~tools () :
    task_run_outcome =
  let spec =
    Specs.executor ~system_prompt ~system_blocks ~strategy
      ~max_iters:max_iterations ~env ~tools ()
  in
  let capture (outcome : task_run_outcome) : Trace.capture_result =
    match outcome with
    | Task_done_explicit { submit; _ } ->
        let output =
          Printf.sprintf "explicit submit; success=%b; %s"
            submit.ts_success
            (if submit.ts_success then submit.ts_result
             else "error: " ^ submit.ts_error)
        in
        {
          output;
          tokens = Trace.zero_tokens;
          cost_delta = 0.0;
          ok = submit.ts_success;
          error =
            (if submit.ts_success then None else Some submit.ts_error);
        }
    | Task_done_implicit { answer; _ } ->
        Trace.ok_capture ~output:("implicit: " ^ answer)
          ~tokens:Trace.zero_tokens ~cost_delta:0.0
    | Task_run_failed { reason; _ } ->
        Trace.fail_capture ~error:(agent_error_pp reason)
    | Task_run_waiting { question; _ } ->
        Trace.ok_capture ~output:("waiting on user: " ^ question)
          ~tokens:Trace.zero_tokens ~cost_delta:0.0
  in
  Trace.span_current ~kind:Trace.Plan_step ~name:"task"
    ~input_summary:task_description ~capture (fun () ->
      match
        Workflow.run
          (task_attempt_flow ~spec ~prior_messages ~task_description)
      with
      | Ok outcome -> outcome
      | Error reason ->
          Task_run_failed { reason; messages = prior_messages })

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

(** Plain-text fallback when the synthesizer LLM call exhausts retries
    — produces a deterministic dump (plan + every task's result or
    error) so the run still has SOMETHING for the caller to display.
    Better than letting an end-of-run LLM hiccup throw away the work
    of every prior task. *)
let fallback_summary plan results err =
  Printf.sprintf
    "Goal: %s\n\nPlan: %s\n\nTask results:\n%s\n\n[note] summarizer LLM \
     call failed after retries (%s); raw task results shown above instead \
     of synthesized answer."
    plan.goal plan.title
    (format_task_results plan results)
    (agent_error_pp err)

(** Synthesizer LLM call: one final call, no tools, free-text answer.
    [model] picks the synthesizer model — None falls back to runtime
    default. Wrapped in [with_retry max=2] + [recover] so transient
    failures (chaos / 5xx) don't waste a long run; persistent failures
    fall back to the deterministic plan-results dump. *)
let summarize_flow ~(model : string option) ~plan ~results :
    string Workflow.t =
  let open Workflow in
  let user_msg =
    Printf.sprintf
      "Goal:\n%s\n\nPlan: %s\n\nTask results:\n%s\n\nWrite the final answer."
      plan.goal plan.title (format_task_results plan results)
  in
  let llm_call =
    of_thunk (fun () ->
        let args : llm_call_args =
          {
            messages = [ user_text_message user_msg ];
            tools = [];
            system_override = Some summarizer_system_prompt;
            tool_choice = Tc_auto;
            model;
          }
        in
        (* Catch Llm_api_error and convert to Result.Error so the
           outer with_retry / recover can see it. Without this catch,
           a chaos-injected (or real upstream) LLM error here flies
           past every workflow combinator straight to the protection
           layer and kills the whole run AFTER all tasks have
           succeeded — losing the whole plan's output for one
           summarizer hiccup. Mirrors the same fix in [Agent.execute]
           for ReAct-loop LLM calls. *)
        try
          let response = Effect.perform (Effects.Llm_complete args) in
          Ok (Step.extract_final_text response.content)
        with Llm_error.Llm_api_error e ->
          Error (Llm_call_failed (Llm_error.pp e)))
  in
  recover (with_retry ~max_attempts:2 llm_call) (fun err ->
      let* () =
        log_line
          (Printf.sprintf
             "[summarizer] LLM call failed after retries: %s — using \
              plain-text fallback"
             (agent_error_pp err))
      in
      pure (fallback_summary plan results err))

let summarize ?(model : string option) ~plan ~results () : agent_result =
  let capture (r : agent_result) : Trace.capture_result =
    match r with
    | Ok answer ->
        Trace.ok_capture
          ~output:(if String.length answer > 200 then String.sub answer 0 200 ^ "..." else answer)
          ~tokens:Trace.zero_tokens ~cost_delta:0.0
    | Error e -> Trace.fail_capture ~error:(agent_error_pp e)
  in
  Trace.span_current ~kind:Trace.Phase ~name:"summarizer"
    ~input_summary:plan.goal ~capture (fun () ->
      Effect.perform (Effects.Log "[summarizer] synthesizing final answer");
      Workflow.run (summarize_flow ~model ~plan ~results))

let executor_memory_name = "executor"

(* ===== Workspace brief ===== *)

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

(* ===== Configuration ===== *)

type plan_mode = [ `Sequential | `Dag ]

type config = {
  skip_summarizer : bool;
  max_iterations_per_task : int;
  max_task_retries : int;
  max_recoveries : int;
  working_dir : string option;
  memory_dir : string option;
  model : string;
  plan_mode : plan_mode;
      (** [`Sequential] (default): tasks run in list order, each one
          waits for the previous. [`Dag]: planner declares per-task
          [depends_on]; drive loop schedules ready tasks (all deps
          done). DAG mode lets the planner declare independent
          subgraphs explicitly so cascade failures stop dependents
          from launching, and (future) so independent tasks run in
          parallel. Switch via CLI [--plan-dag]. *)
  planner_model : string option;
      (** Override for [Planner.plan]. None inherits [model]. *)
  executor_model : string option;
      (** Override for per-task ReAct loop. None inherits [model].
          Most tokens go through this path — set to a cheaper model
          (e.g. Haiku) to cut costs, at the risk of lower task
          completion rate under chaos. *)
  recovery_model : string option;
      (** Override for [Planner.recover]. None inherits [model]. *)
  summarizer_model : string option;
      (** Override for the final synthesizer call. None inherits
          [model]. *)
  planner_system_prompt : string option;
  executor_system_prompt : string;
  executor_system_blocks : (string * string) list;
  executor_strategy : unit -> Context.Strategy.t;
  restart : bool;
}

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
    plan_mode = `Sequential;
    planner_model = None;
    executor_model = None;
    recovery_model = None;
    summarizer_model = None;
    planner_system_prompt = None;
    executor_system_prompt = Agent.default_system_prompt;
    executor_system_blocks = [];
    executor_strategy = default_executor_strategy;
    restart = false;
  }

(* ===== Drive state =====
   The drive loop's threaded state. [pending] is the remaining task
   list; [acc] is the completed (in reverse). [recoveries] /
   [prior_failures] track recovery cycles for the recovery planner's
   self-awareness. *)
type drive_state = {
  pending : task list;
  acc : (task * (string, string) result) list;
  recoveries : int;
  prior_failures : (string * string) list;
}

(* ===== Drive environment (immutable per run) ===== *)
type drive_env = {
  config : config;
  goal : string;
  tools : tool_def list;
  initial_plan : plan;
  exec_mem : Memory.t;
  exec_env : (string * string) list;
  executor_strategy : Context.Strategy.t;
}

let executor_spec env =
  Specs.executor
    ~system_prompt:env.config.executor_system_prompt
    ~system_blocks:env.config.executor_system_blocks
    ~strategy:env.executor_strategy
    ~max_iters:env.config.max_iterations_per_task
    ?model:env.config.executor_model
    ~env:env.exec_env ~tools:env.tools ()

(* Renumber a fresh task list so its [index] continues after the
   already-completed acc. Used by Replan / Split. *)
let renumber_after ~acc_count ~offset tasks =
  List.mapi
    (fun i (t : task) -> { t with index = acc_count + offset + 1 + i })
    tasks

(* Persist the unified durable state: control plane (live plan,
   pending, completed, recovery) + data plane (executor message
   history) in one atomic write. The [tasks] field reflects the LIVE
   plan (acc reversed + pending), not the original initial_plan. *)
let persist_state env state =
  match env.config.memory_dir with
  | None -> ()
  | Some dir ->
      let live_tasks = List.rev_map fst state.acc @ state.pending in
      let live_plan = { env.initial_plan with tasks = live_tasks } in
      Plan_state.save ~dir
        {
          goal = env.goal;
          plan = live_plan;
          pending = state.pending;
          completed_rev = state.acc;
          recoveries = state.recoveries;
          prior_failures = state.prior_failures;
          executor_messages = Memory.to_messages env.exec_mem;
        }

(* ===== Single-task workflow with checkpoint + retry =====
   Runs one task against the current executor memory:

   - Take a Memory checkpoint (so failure rolls back the poisoned trail).
   - Build the leaf attempt as a Workflow that re-reads memory on each
     retry (lazy via [of_thunk]).
   - On success: persist memory, log [Task_completed], return answer.
   - On failure (retries exhausted): restore checkpoint, propagate err. *)
let run_task_flow env task : string Workflow.t =
  let open Workflow in
  let spec = executor_spec env in
  let cp = Memory.checkpoint env.exec_mem in
  (* In-memory only: messages get persisted to disk in [persist_state]
     atomically alongside the plan_state, so a crash between them
     can't leave a split-brain "data advanced, control hasn't" state. *)
  let commit_success ~messages ~result_preview answer =
    let* () = action (fun () -> Memory.set env.exec_mem messages) in
    let* () =
      log_event
        (Task_completed { index = task.index; result_preview })
    in
    pure answer
  in
  let one_attempt =
    of_thunk (fun () ->
        let prior = Memory.to_messages env.exec_mem in
        Workflow.run
          (let* outcome =
             task_attempt_flow ~spec ~prior_messages:prior
               ~task_description:task.description
           in
           match outcome with
           | Task_done_explicit { submit = { ts_success = true; ts_result; _ }; messages } ->
               commit_success ~messages ~result_preview:ts_result ts_result
           | Task_done_explicit { submit = { ts_success = false; ts_error; _ }; _ } ->
               of_result
                 (Error
                    (Plan_invalid
                       (if ts_error <> "" then ts_error
                        else "task reported failure without details")))
           | Task_done_implicit { answer; messages } ->
               commit_success ~messages ~result_preview:answer answer
           | Task_run_failed { reason; _ } -> of_result (Error reason)
           | Task_run_waiting _ ->
               of_result
                 (Error
                    (Plan_invalid
                       "task called ask_user — pause-tool not supported in \
                        plan-act mode"))))
  in
  let with_rollback =
    recover one_attempt (fun err ->
        let* () = action (fun () -> Memory.restore env.exec_mem cp) in
        let* () =
          log_event
            (Task_failed
               { index = task.index; error = agent_error_pp err; attempt = 0 })
        in
        of_result (Error err))
  in
  (* Per-attempt git checkpoint: if [working_dir] is a git repo, each
     attempt captures HEAD on entry and either commits on success or
     hard-resets + cleans on failure. Retries then see the pre-attempt
     state instead of accumulated chaos-induced side effects (orphan
     files, partial edits, broken imports). [with_checkpoint] is the
     inner layer of [with_retry] so each retry creates a fresh ckpt. *)
  let body_with_ckpt =
    match env.config.working_dir with
    | Some cwd ->
        with_checkpoint ~cwd
          ~message:(fun () ->
            Printf.sprintf "task %d: %s" task.index task.description)
          with_rollback
    | None -> with_rollback
  in
  with_retry ~max_attempts:(env.config.max_task_retries + 1) body_with_ckpt

(* ===== Recovery flow =====
   Consult the recovery planner. Returns a {!Planner.recovery_decision}
   on success; the caller pattern-matches and pivots [drive_state]. *)
let recovery_flow env state task err :
    Planner.recovery_decision Workflow.t =
  (* completed = task descs from successful entries. skipped = task
     descs the drive loop accumulated as Error _ (recovery_decided
     SKIP / BUDGET_EXHAUSTED). The recovery planner needs to see these
     separately: a current failure complaining about a missing
     artifact may be cascading from a previously-skipped task. *)
  let completed_descs, skipped_descs =
    List.fold_left
      (fun (ok_acc, skip_acc) (t, r) ->
        match r with
        | Ok _ -> (t.description :: ok_acc, skip_acc)
        | Error _ -> (ok_acc, t.description :: skip_acc))
      ([], []) state.acc
  in
  let remaining_descs = List.map (fun (t : task) -> t.description) state.pending in
  Workflow.of_thunk (fun () ->
      let budget =
        try Some (Effect.perform Effects.Get_budget_progress)
        with Effect.Unhandled _ -> None
      in
      Planner.recover ?model:env.config.recovery_model
        ~research_tools:env.tools
        ~prior_failures:(List.rev state.prior_failures)
        ~skipped:(List.rev skipped_descs)
        ?budget_progress:budget
        ~cycle_index:state.recoveries
        ~max_cycles:env.config.max_recoveries
        ~goal:env.goal ~completed:(List.rev completed_descs) ~failed_task:task
        ~failed_error:err ~remaining:remaining_descs ())

(* ===== Drive loop =====
   Recursive workflow: each call either finalizes or runs one task and
   recurses with an updated state. The shape branches on
   [env.config.plan_mode] — Sequential walks the pending list head-on,
   DAG schedules tasks whose [depends_on] all completed (and short-
   circuits cascade failures without an LLM call). *)
let rec drive_flow env state : string Workflow.t =
  match env.config.plan_mode with
  | `Sequential -> drive_seq_flow env state
  | `Dag -> drive_dag_flow env state

and drive_seq_flow env state : string Workflow.t =
  let open Workflow in
  let* () = action (fun () -> persist_state env state) in
  match state.pending with
  | [] -> finalize_flow env state
  | task :: rest ->
      let* () =
        let total = List.length state.pending + List.length state.acc in
        log_event
          (Task_started
             { index = task.index; total; description = task.description })
      in
      let* outcome = attempt (run_task_flow env task) in
      (match outcome with
       | Ok answer ->
           drive_flow env
             {
               state with
               pending = rest;
               acc = (task, Ok answer) :: state.acc;
             }
       | Error err ->
           handle_failure_flow env state task err rest)

(* ===== DAG drive =====
   Pick the lowest-index pending task whose [depends_on] are all
   present as Ok in [acc]. Tasks whose deps include an Error in
   [acc] are CASCADE-FAILED — marked Error in acc without an LLM
   call (recovery is for task-level failures, cascade is structural).
   Tasks whose deps are still pending → wait for them.

   Termination: each iteration either drains a cascade-failed task
   without LLM, runs one ready task, or hits a stuck graph (no
   ready, no cascade, but pending non-empty → circular / unmet
   dependency, treated as failure of the entire remaining set). *)
and drive_dag_flow env state : string Workflow.t =
  let open Workflow in
  let* () = action (fun () -> persist_state env state) in
  match state.pending with
  | [] -> finalize_flow env state
  | _ ->
      let acc_status =
        (* index → `Ok | `Error from already-resolved tasks *)
        List.map
          (fun (t, r) ->
            (t.index, match r with Ok _ -> `Ok | Error _ -> `Error))
          state.acc
      in
      let dep_status (t : task) =
        let rec scan = function
          | [] -> `Ready
          | d :: rest ->
              (match List.assoc_opt d acc_status with
               | Some `Error -> `Cascade_failed d
               | Some `Ok -> scan rest
               | None -> `Waiting)
        in
        scan t.depends_on
      in
      let cascaded, others =
        List.partition
          (fun t ->
            match dep_status t with `Cascade_failed _ -> true | _ -> false)
          state.pending
      in
      (match cascaded with
       | t :: rest ->
           let dep_idx =
             match dep_status t with
             | `Cascade_failed d -> d
             | _ -> 0
           in
           let err_str =
             Printf.sprintf
               "cascade: skipped because depends_on=%d failed earlier"
               dep_idx
           in
           let* () =
             log_event
               (Task_failed
                  { index = t.index; error = err_str; attempt = 0 })
           in
           drive_dag_flow env
             {
               state with
               pending = rest @ others;
               acc = (t, Error err_str) :: state.acc;
             }
       | [] ->
           let ready, waiting =
             List.partition
               (fun t -> dep_status t = `Ready)
               others
           in
           (match
              List.sort
                (fun a b -> compare a.index b.index)
                ready
            with
            | [] ->
                (* Nothing is ready and nothing is cascade-failed but
                   tasks remain. The graph is unsatisfiable (circular
                   dep, or depends_on points to a non-existing task).
                   Mark every remaining as Failed and finalize. *)
                let acc' =
                  List.fold_left
                    (fun acc t ->
                      let err =
                        Printf.sprintf
                          "stuck: no satisfiable dependency order \
                           (depends_on=%s)"
                          (String.concat ","
                             (List.map string_of_int t.depends_on))
                      in
                      (t, Error err) :: acc)
                    state.acc waiting
                in
                drive_dag_flow env { state with pending = []; acc = acc' }
            | task :: _ ->
                let rest_ready =
                  List.filter
                    (fun t -> t.index <> task.index)
                    ready
                in
                let* () =
                  let total =
                    List.length state.pending + List.length state.acc
                  in
                  log_event
                    (Task_started
                       {
                         index = task.index;
                         total;
                         description = task.description;
                       })
                in
                let* outcome = attempt (run_task_flow env task) in
                let new_pending = rest_ready @ waiting in
                (match outcome with
                 | Ok answer ->
                     drive_dag_flow env
                       {
                         state with
                         pending = new_pending;
                         acc = (task, Ok answer) :: state.acc;
                       }
                 | Error err ->
                     handle_failure_flow env state task err new_pending)))

and handle_failure_flow env state task err rest :
    string Workflow.t =
  let open Workflow in
  let err_str = agent_error_pp err in
  let* () =
    log_event
      (Recovery_invoked
         {
           failed_index = task.index;
           failed_error = err_str;
           cycle = state.recoveries;
         })
  in
  let new_acc = (task, Error err_str) :: state.acc in
  let acc_count = List.length state.acc in
  (* Apply a recovery decision: log + recurse with the pivoted state.
     [pending] is the only thing that varies across decisions; the
     rest of the new state is shared. *)
  let pivot ~decision ~details ~pending =
    let* () =
      log_event (Recovery_decided { decision; details })
    in
    drive_flow env
      {
        pending;
        acc = new_acc;
        recoveries = state.recoveries + 1;
        prior_failures = (task.description, err_str) :: state.prior_failures;
      }
  in
  if state.recoveries >= env.config.max_recoveries then
    pivot ~decision:"BUDGET_EXHAUSTED"
      ~details:
        (Printf.sprintf "%d/%d cycles used" state.recoveries
           env.config.max_recoveries)
      ~pending:rest
  else
    (* Wrap recovery in [with_retry] so a transient chaos hit on the
       recovery LLM call doesn't waste a recovery cycle on no decision.
       The cycle counter advances on the OUTER planner outcome (post-
       retries); retries here are just to give the planner a chance to
       produce some decision. Llm_handler.with_retry already handles
       retryable API errors, but won't retry [auth] / [context_window]
       / [bad_request], which is exactly what chaos likes to inject. *)
    let* decision_result =
      attempt
        (with_retry ~max_attempts:2 (recovery_flow env state task err_str))
    in
    match decision_result with
    | Error _ ->
        (* Recovery planner failed even after retries → treat as Skip
           to make progress without an infinite loop. *)
        pivot ~decision:"SKIP" ~details:"recovery planner failed"
          ~pending:rest
    | Ok Planner.Abandon ->
        pivot ~decision:"ABANDON"
          ~details:"surfacing failure to summarizer" ~pending:[]
    | Ok (Planner.Replan new_tasks) ->
        pivot ~decision:"REPLAN"
          ~details:
            (Printf.sprintf "%d new task(s) replace failed + remaining"
               (List.length new_tasks))
          ~pending:(renumber_after ~acc_count ~offset:0 new_tasks)
    | Ok (Planner.Split new_tasks) ->
        let split_count = List.length new_tasks in
        let split_renumbered =
          renumber_after ~acc_count ~offset:0 new_tasks
        in
        let rest_renumbered =
          renumber_after ~acc_count ~offset:split_count rest
        in
        pivot ~decision:"SPLIT"
          ~details:
            (Printf.sprintf
               "failed task -> %d sub-task(s); %d remaining stay"
               split_count (List.length rest))
          ~pending:(split_renumbered @ rest_renumbered)
    | Ok Planner.Skip ->
        pivot ~decision:"SKIP"
          ~details:
            (Printf.sprintf "%d remaining task(s) continue"
               (List.length rest))
          ~pending:rest

and finalize_flow env state : string Workflow.t =
  let open Workflow in
  let results = List.map snd (List.rev state.acc) in
  let summary_plan =
    { env.initial_plan with tasks = List.rev_map fst state.acc }
  in
  if env.config.skip_summarizer then
    let header =
      if List.exists Result.is_error results then
        Printf.sprintf "Plan: %s (with failures)" env.initial_plan.title
      else Printf.sprintf "Plan: %s" env.initial_plan.title
    in
    pure
      (Printf.sprintf "%s\n\n%s" header
         (format_task_results summary_plan results))
  else summarize_flow ~model:env.config.summarizer_model ~plan:summary_plan ~results

(* ===== Plan / resume ===== *)

let plan_or_resume_flow ~config ~goal ~goal_for_planner ~tools
    ~resume_state : plan Workflow.t =
  let open Workflow in
  match resume_state with
  | Some s ->
      let* () =
        log_line
          (Printf.sprintf
             "[plan_act] resuming saved state: %d/%d tasks completed, %d \
              recoveries used"
             (List.length s.Plan_state.completed_rev)
             (List.length s.plan.tasks)
             s.recoveries)
      in
      pure s.plan
  | None ->
      (* User override wins; otherwise pick the prompt that matches
         plan_mode so DAG runs get the dependency-aware instructions. *)
      let effective_planner_prompt =
        match config.planner_system_prompt, config.plan_mode with
        | Some _, _ -> config.planner_system_prompt
        | None, `Dag -> Some Specs.dag_planner_prompt
        | None, `Sequential -> None
      in
      of_thunk (fun () ->
          Planner.plan ?system_prompt:effective_planner_prompt
            ?model:config.planner_model ~research_tools:tools
            ~goal:goal_for_planner ())
  |> fun plan_flow ->
  let* p = plan_flow in
  let* () =
    action (fun () ->
        Effect.perform
          (Effects.Event_log
             (Plan_decomposed
                {
                  goal_preview = p.title;
                  n_tasks = List.length p.tasks;
                }));
        List.iter
          (fun (t : task) ->
            let deps_str =
              match t.depends_on with
              | [] -> ""
              | ds ->
                  Printf.sprintf "  [deps: %s]"
                    (String.concat "," (List.map string_of_int ds))
            in
            Effect.perform
              (Effects.Log
                 (Printf.sprintf "  %d. %s%s" t.index t.description
                    deps_str)))
          p.tasks;
        ignore goal)
  in
  pure p

(** Top-level plan-act run, expressed as a single workflow value. *)
let run ?(config = default_config) ~goal ~tools () : agent_result =
  let capture (r : agent_result) : Trace.capture_result =
    match r with
    | Ok answer ->
        Trace.ok_capture
          ~output:(if String.length answer > 200 then String.sub answer 0 200 ^ "..." else answer)
          ~tokens:Trace.zero_tokens ~cost_delta:0.0
    | Error e -> Trace.fail_capture ~error:(agent_error_pp e)
  in
  Trace.span_current ~kind:Trace.Phase ~name:"plan_act"
    ~input_summary:goal ~capture (fun () ->
      let exec_env, planner_prefix = survey_workspace ~working_dir:config.working_dir in
      let resume_state =
        if config.restart then None
        else
          match config.memory_dir with
          | Some dir -> Plan_state.try_load ~dir ~goal
          | None -> None
      in
      let goal_for_planner =
        if planner_prefix = "" then goal
        else planner_prefix ^ "\n\n--- USER GOAL ---\n" ^ goal
      in
      let workflow : string Workflow.t =
        let open Workflow in
        let* initial_plan =
          plan_or_resume_flow ~config ~goal ~goal_for_planner ~tools
            ~resume_state
        in
        (* Memory is now an in-memory cache; persistence rides on
           [plan_state.json] via [persist_state]. Initial messages
           come from [resume_state.executor_messages] when resuming
           (already goal-checked); empty otherwise. *)
        let exec_mem =
          Memory.create ~model:config.model ~name:executor_memory_name ()
        in
        let* () =
          action (fun () ->
              match resume_state with
              | Some s when not config.restart ->
                  Memory.set exec_mem s.executor_messages;
                  if List.length s.executor_messages > 0 then
                    Effect.perform
                      (Effects.Log
                         (Printf.sprintf
                            "[memory] resumed %d prior executor message(s)"
                            (List.length s.executor_messages)))
              | _ -> ())
        in
        let env =
          {
            config;
            goal;
            tools;
            initial_plan;
            exec_mem;
            exec_env;
            executor_strategy = config.executor_strategy ();
          }
        in
        let init_state =
          match resume_state with
          | Some s ->
              {
                pending = s.pending;
                acc = s.completed_rev;
                recoveries = s.recoveries;
                prior_failures = s.prior_failures;
              }
          | None ->
              {
                pending = initial_plan.tasks;
                acc = [];
                recoveries = 0;
                prior_failures = [];
              }
        in
        drive_flow env init_state
      in
      let result = Workflow.run workflow in
      (match (result, config.memory_dir) with
      | Ok _, Some dir -> Plan_state.clear ~dir
      | _ -> ());
      result)
