(** Wire the effect-handler stack (Governor + Llm/Tool/Log chains +
    optional Checkpoint tape) and run a thunk inside it. *)

open Types

type config = {
  model : string;
  cost : cost_state;
  on_log : string -> unit;
  on_text_delta : string -> unit;
  governor_limits : Governor.Limits.t;
  llm_max_retries : int;
  tape_path : string option;
      (** When [Some path], every LLM response and tool batch is
          recorded to JSONL there for replay; if the file already exists
          its entries are replayed before live calls resume. *)
  crash_after : int option;
      (** Demo / testing only — abort after N live LLM calls so a
          subsequent resume can verify replay works. *)
  emit_governor_events_to_log : bool;
      (** When true, every Governor.Tick gets logged via [on_log] — the
          ["[gov] ..."] lines you see during dev runs. Off in tests. *)
  sandbox_root : string option;
      (** Restrict File_* ops to this prefix when set. *)
  tracer : Trace.tracer;
      (** Trace sink — [Trace.make_noop ()] silences. Use
          [Trace.make_file_writer path] to capture NDJSON frames for
          every LLM call and tool dispatch. *)
  on_event : Event.t -> unit;
      (** Observer for structured control-plane events
          ([Plan_decomposed] / [Task_started] / [Recovery_decided] /
          [Subagent_entered] / [Subagent_exited] / ...). Default
          no-op. Pass a function here to drive a UI, telemetry
          pipeline, or append-only journal; events still render to
          the [on_log] sink in parallel via [Event.to_log_line]. *)
  chaos : Chaos.config;
      (** Chaos middleware config (failure injection). [Chaos.default]
          for production runs (no-op). Set non-zero rates to exercise
          retry / recovery paths. *)
}

(* ===== chain builders ===== *)

let build_llm_chain ~config : Llm_handler.t =
  let on_retry n err delay =
    config.on_log
      (Printf.sprintf "[retry] attempt %d failed (%s); sleeping %.1fs"
         (n + 1)
         (Llm_error.kind err)
         delay)
  in
  let policy =
    if config.llm_max_retries > 0 then
      {
        Llm_handler.Retry_policy.default with
        max_attempts = config.llm_max_retries;
      }
    else Llm_handler.Retry_policy.none
  in
  Llm_handler.anthropic ~model:config.model
    ~on_text_delta:config.on_text_delta ()
  |> Llm_handler.with_validation
  |> Llm_handler.with_cost_tracking ~cost:config.cost
  |> Llm_handler.with_retry ~policy ~on_retry
  (* Chaos OUTSIDE retry: an injected failure bypasses per-call retry
     and propagates up to plan-act recovery / workflow [recover] —
     that's the layer chaos engineering wants to verify. The retry
     middleware already proves itself when REAL upstream failures hit
     (LLM API does throttle / blip). [Chaos.with_llm] is no-op when
     the rate is 0.0 so this is free in normal runs. *)
  |> Chaos.with_llm config.chaos
  (* Trace OUTSIDE retry+chaos so one span = one logical LLM call
     (covers all retry attempts AND captures chaos-injected failures
     in the trace), not one span per retry attempt. *)
  |> Llm_handler.with_tracing ~tracer:config.tracer ~model:config.model
  |> Llm_handler.with_governor_ticks
  |> Llm_handler.with_logging ~on_log:config.on_log

(** [Tool_handler.with_timeout] is intentionally NOT in the default
    chain. It dispatches the inner handler in a worker thread to
    enforce wall-clock; OCaml 5 effect handlers do NOT propagate to
    threads, so any tool that performs an Effect from its handler
    (view_file, write_file, current_time, delegate, parallel_delegate,
    skill loader's load tracking, ...) would crash with
    Effect.Unhandled inside the worker.

    Tools that DO need wall-clock caps for blocking subprocesses
    self-implement: [Tools.bash] uses [run_with_timeout]; [http_get]
    uses [curl --max-time]; [Mcp] uses [read_timeout_sec]. The
    middleware version remains exported for callers who want it
    around purely-synchronous tools without effects. *)
let build_tool_chain ~config : Tool_handler.t =
  Tool_handler.direct
  |> Tool_handler.with_validation
  |> Tool_handler.with_retry ~policy:Llm_handler.Retry_policy.default
  |> Tool_handler.with_circuit_breaker ~failure_threshold:5 ~cooldown:60.0
  (* Dedup OUTSIDE retry+breaker so it observes the post-retry final
     verdict. Annotates the error message when the model has called
     the SAME tool with the SAME input and gotten the SAME error code
     N times in a row — a strong signal it's stuck. The model reads
     the enriched message in the next ReAct iteration and pivots. *)
  |> Tool_handler.with_dedup_repeats ~threshold:3
  (* Chaos OUTSIDE retry+circuit-breaker: same rationale as the LLM
     chain — injected errors propagate to submit_task_result-failure
     / plan-act recovery / workflow [recover]. *)
  |> Chaos.with_tool config.chaos
  (* Trace OUTSIDE retry — one span per logical tool call. *)
  |> Tool_handler.with_tracing ~tracer:config.tracer
  |> Tool_handler.with_logging ~on_log:config.on_log

let build_log_chain ~config : Log_handler.t =
  Log_handler.to_function config.on_log

(* ===== install ===== *)

(** Build the child config for [Workflow.parallel] / sub-agent fan-out:
    fork the tracer (per-Domain push/pop stack writing to the parent's
    sink), drop tape recording (no recursive tape), drop streaming /
    crash demos. Cost is shared so [max_cost_usd] / [max_steps] account
    for child spend in real time. *)
let fork_child_config (parent : config) : config =
  let initial_parent_id = Trace.current_parent parent.tracer in
  let child_tracer =
    Trace.fork ~parent:parent.tracer ~initial_parent_id ()
  in
  {
    parent with
    on_text_delta = (fun _ -> ());
    tape_path = None;
    crash_after = None;
    tracer = child_tracer;
  }

let rec install ~(config : config) (thunk : unit -> 'a) : 'a =
  (* Self-install branch wrapper for [Workflow.parallel]: each branch
     runs against a freshly-installed child stack. Library callers
     using only [Runtime.install] now get a working [Workflow.parallel]
     without needing to remember [Workflow.set_branch_wrapper]
     manually. *)
  Workflow.set_branch_wrapper (fun branch_thunk ->
    install ~config:(fork_child_config config) branch_thunk);
  let llm_chain = build_llm_chain ~config in
  let tool_chain = build_tool_chain ~config in
  let log_chain = build_log_chain ~config in
  (* Publish the tracer to this Domain's DLS so library code (plan_act,
     sub_agent, ...) can emit spans via [Trace.span_current] without
     threading [tracer] through every signature. Restored on return. *)
  let thunk = fun () -> Trace.with_current ~tracer:config.tracer thunk in
  let governed inner =
    Governor.install ~limits:config.governor_limits ~cost:config.cost
      ~on_tick:(fun ev ->
        if config.emit_governor_events_to_log then
          config.on_log
            (Printf.sprintf "[gov] %s" (Governor.Event.to_string ev)))
      inner
  in
  let file_chain =
    match config.sandbox_root with
    | None -> File_handler.direct
    | Some root -> File_handler.direct |> File_handler.with_sandbox ~root
  in
  (* Time / File / Log handlers MUST install outside Tool_handler. When a
     tool handler is dispatched (inside Tool_handler's effc), its
     [Effect.perform Time_now / File_* / Log] bubbles UP from that effc
     — past Tool_handler — looking for the next matching try_with. The
     IO and Log handlers therefore need to be MORE OUTER than
     Tool_handler. *)
  let with_io_and_log inner =
    Time_handler.install Time_handler.direct (fun () ->
        File_handler.install file_chain (fun () ->
            Log_handler.install ~on_event:config.on_event log_chain inner))
  in
  match config.tape_path with
  | None ->
      governed (fun () ->
          Llm_handler.install llm_chain (fun () ->
              with_io_and_log (fun () ->
                  Tool_handler.install tool_chain thunk)))
  | Some path ->
      let session =
        Checkpoint.open_session ~path ~on_log:config.on_log
          ?crash_after_live_llm:config.crash_after ()
      in
      at_exit (fun () -> Checkpoint.close_session session);
      let llm_with_tape = llm_chain |> Checkpoint.with_tape_llm session in
      governed (fun () ->
          Llm_handler.install llm_with_tape (fun () ->
              with_io_and_log (fun () ->
                  Checkpoint.install_tools_with_tape session tool_chain
                    thunk)))
