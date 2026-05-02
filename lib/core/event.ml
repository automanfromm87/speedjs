(** Structured observability events.

    Replaces (or supplements) free-form [Effects.Log] strings for the
    high-value, machine-queryable moments: plan decomposition, task
    transitions, recovery decisions, run summary. Observers can filter
    by variant, route to OTel / Prometheus / a UI, or build assertions
    in tests without parsing log lines.

    NOT intended to overlap with [Governor.Event] — those are
    governance-related (counted toward limits, fed into death-loop
    detection). [Event.t] is pure observability. Some moments emit
    BOTH (e.g. an [Llm_call] is a Governor concern AND useful for a
    dashboard); the agent code emits one or the other depending on
    intent. *)

type t =
  | Plan_decomposed of { goal_preview : string; n_tasks : int }
      (** Planner returned a plan with N tasks. *)
  | Task_started of { index : int; total : int; description : string }
      (** Plan-act executor about to run task [index] of [total]. *)
  | Task_completed of { index : int; result_preview : string }
      (** Task succeeded; [result_preview] is a short summary. *)
  | Task_failed of { index : int; error : string; attempt : int }
      (** Task failed on attempt N (after per-task retries are
          exhausted, this triggers Recovery_invoked). *)
  | Recovery_invoked of {
      failed_index : int;
      failed_error : string;
      cycle : int;
    }
      (** Plan-act handing the failure to the recovery planner. *)
  | Recovery_decided of { decision : string; details : string }
      (** Recovery planner produced a decision (REPLAN N tasks /
          ABANDON / etc.). *)
  | Subagent_entered of { mode : string; n_children : int }
      (** A delegate / parallel_delegate boundary started; [mode] is
          ["delegate"] or ["parallel_delegate"]; [n_children] = 1 for
          serial, N for parallel fan-out. *)
  | Subagent_exited of { mode : string }
      (** Matching exit. Pair-counted with [Subagent_entered]. *)
  | Skills_loaded of { count : int; names : string list }
      (** Skill discovery completed. *)
  | Mcp_loaded of {
      server : string;
      version : string;
      n_tools : int;
      tool_names : string list;
    }
      (** MCP server connected, tools registered. *)
  | Run_summary of {
      llm_calls : int;
      input_tokens : int;
      output_tokens : int;
      cache_write : int;
      cache_read : int;
      cost_usd : float;
    }
      (** Periodic / final cost-and-usage summary. *)
  | Custom of { kind : string; payload : Yojson.Safe.t }
      (** Escape hatch for callers that want to emit something the
          framework doesn't predefine. Kind = a stable name; payload =
          arbitrary JSON. *)

(** Render an event as a single human-readable line — used as the
    fallback when a structured observer isn't installed; the line is
    forwarded through the same [Log] sink. *)
let to_log_line (e : t) : string =
  match e with
  | Plan_decomposed { goal_preview; n_tasks } ->
      let preview =
        if String.length goal_preview > 80 then
          String.sub goal_preview 0 80 ^ "..."
        else goal_preview
      in
      Printf.sprintf "[event] plan_decomposed n=%d goal=%s" n_tasks preview
  | Task_started { index; total; description } ->
      let preview =
        if String.length description > 100 then
          String.sub description 0 100 ^ "..."
        else description
      in
      Printf.sprintf "[event] task_started %d/%d %s" index total preview
  | Task_completed { index; result_preview } ->
      let preview =
        if String.length result_preview > 100 then
          String.sub result_preview 0 100 ^ "..."
        else result_preview
      in
      Printf.sprintf "[event] task_completed %d %s" index preview
  | Task_failed { index; error; attempt } ->
      Printf.sprintf "[event] task_failed %d attempt=%d err=%s" index
        attempt error
  | Recovery_invoked { failed_index; failed_error; cycle } ->
      Printf.sprintf
        "[event] recovery_invoked task=%d cycle=%d err=%s" failed_index
        cycle failed_error
  | Recovery_decided { decision; details } ->
      Printf.sprintf "[event] recovery_decided %s %s" decision details
  | Subagent_entered { mode; n_children } ->
      Printf.sprintf "[event] subagent_entered %s n=%d" mode n_children
  | Subagent_exited { mode } ->
      Printf.sprintf "[event] subagent_exited %s" mode
  | Skills_loaded { count; names } ->
      Printf.sprintf "[event] skills_loaded n=%d %s" count
        (String.concat "," names)
  | Mcp_loaded { server; version; n_tools; tool_names } ->
      Printf.sprintf "[event] mcp_loaded %s/%s n=%d %s" server version
        n_tools
        (String.concat "," tool_names)
  | Run_summary
      {
        llm_calls;
        input_tokens;
        output_tokens;
        cache_write;
        cache_read;
        cost_usd;
      } ->
      Printf.sprintf
        "[event] run_summary calls=%d in=%d out=%d cw=%d cr=%d $%.4f"
        llm_calls input_tokens output_tokens cache_write cache_read
        cost_usd
  | Custom { kind; payload } ->
      Printf.sprintf "[event] %s %s" kind (Yojson.Safe.to_string payload)
