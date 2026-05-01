(** Runtime composition — the speedjs library's "press play" entry point.

    Given a [config] and a list of tools, [install] wires together the
    full effect-handler stack (Governor + Llm_handler + Tool_handler +
    Log_handler + optional Checkpoint tape) and runs the supplied thunk
    inside it.

    This module exists so library users (e.g. someone building a vertical
    agent app on top of speedjs without wanting to write their own
    Setup.ml) can spin up the same runtime as the [speedjs] CLI with one
    function call. The CLI's [bin/setup.ml] is now a thin Args.t →
    Runtime.config mapping that delegates here. *)

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
  |> Llm_handler.with_governor_ticks
  |> Llm_handler.with_logging ~on_log:config.on_log

let build_tool_chain ~config : Tool_handler.t =
  Tool_handler.direct
  |> Tool_handler.with_timeout ~default_sec:60.0
  |> Tool_handler.with_validation
  |> Tool_handler.with_retry ~policy:Llm_handler.Retry_policy.default
  |> Tool_handler.with_circuit_breaker ~failure_threshold:5 ~cooldown:60.0
  |> Tool_handler.with_logging ~on_log:config.on_log

let build_log_chain ~config : Log_handler.t =
  Log_handler.to_function config.on_log

(* ===== install ===== *)

(** Compose the runtime as a stack of effect handlers and run [thunk]:

    OUTERMOST       Governor (global limits + death-loop detection)
                    Log_handler
                    Tool_handler / Llm_handler chains
                    + Checkpoint tape (when [tape_path] is set)
    INNERMOST       thunk

    The Governor sees [Tick] events emitted by the LLM chain
    [with_governor_ticks] middleware and by [Tool_handler.install]
    bookends, plus [Subagent_entered/Exited] from [Sub_agent.delegate]. *)
let install ~tools ~(config : config) (thunk : unit -> 'a) : 'a =
  let llm_chain = build_llm_chain ~config in
  let tool_chain = build_tool_chain ~config in
  let log_chain = build_log_chain ~config in
  let governed inner =
    Governor.install ~limits:config.governor_limits ~cost:config.cost
      ~on_tick:(fun ev ->
        if config.emit_governor_events_to_log then
          config.on_log
            (Printf.sprintf "[gov] %s" (Governor.Event.to_string ev)))
      inner
  in
  match config.tape_path with
  | None ->
      governed (fun () ->
          Llm_handler.install llm_chain (fun () ->
              Tool_handler.install ~tools tool_chain (fun () ->
                  Log_handler.install log_chain thunk)))
  | Some path ->
      let session =
        Checkpoint.open_session ~path ~on_log:config.on_log
          ?crash_after_live_llm:config.crash_after ()
      in
      at_exit (fun () -> Checkpoint.close_session session);
      let llm_with_tape = llm_chain |> Checkpoint.with_tape_llm session in
      governed (fun () ->
          Llm_handler.install llm_with_tape (fun () ->
              Checkpoint.install_tools_with_tape session ~tools tool_chain
                (fun () -> Log_handler.install log_chain thunk)))
