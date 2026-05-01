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
  let file_chain =
    match config.sandbox_root with
    | None -> File_handler.direct
    | Some root -> File_handler.direct |> File_handler.with_sandbox ~root
  in
  (* Time / File handlers MUST install outside Tool_handler. When a tool
     handler is dispatched (inside Tool_handler's effc), its
     [Effect.perform Time_now / File_*] bubbles UP from that effc — past
     Tool_handler — looking for the next matching try_with. So the IO
     handlers need to be more outer than Tool_handler. *)
  let with_io inner =
    Time_handler.install Time_handler.direct (fun () ->
        File_handler.install file_chain inner)
  in
  match config.tape_path with
  | None ->
      governed (fun () ->
          Llm_handler.install llm_chain (fun () ->
              with_io (fun () ->
                  Tool_handler.install ~tools tool_chain (fun () ->
                      Log_handler.install log_chain thunk))))
  | Some path ->
      let session =
        Checkpoint.open_session ~path ~on_log:config.on_log
          ?crash_after_live_llm:config.crash_after ()
      in
      at_exit (fun () -> Checkpoint.close_session session);
      let llm_with_tape = llm_chain |> Checkpoint.with_tape_llm session in
      governed (fun () ->
          Llm_handler.install llm_with_tape (fun () ->
              with_io (fun () ->
                  Checkpoint.install_tools_with_tape session ~tools tool_chain
                    (fun () -> Log_handler.install log_chain thunk))))
