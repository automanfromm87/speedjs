(** Plan-Act flow: planner → per-task executor (with persistent memory)
    → recovery → summarizer.

    All LLM calls go through [Effects.Llm_complete], so the same handler
    stack (production / mock / checkpoint / retry / protection / cost
    tracking) works transparently. *)

open Types

(** Synthetic terminal tool the executor calls to signal task completion
    (or definitive failure). The handler is never invoked — the agent
    loop intercepts via [terminal_tools] and short-circuits. *)
val submit_task_result_name : string

val submit_task_result_tool : tool_def

(** Run a single task with structured outcome. [prior_messages] is the
    executor's accumulated history; the new task is appended (with
    dangling-tool_use closure if needed) and run as a fresh ReAct loop.
    [env] blocks ride at the system-prompt layer (workspace_brief etc.). *)
val run_for_task :
  ?max_iterations:int ->
  ?strategy:Context.Strategy.t ->
  ?system_prompt:string ->
  ?system_blocks:(string * string) list ->
  ?prior_messages:message list ->
  ?env:(string * string) list ->
  task_description:string ->
  tools:tool_def list ->
  unit ->
  task_run_outcome

(** Configuration for [run]. Use [default_config] as a base and override
    fields with record update syntax:
    {[ run ~config:{ default_config with working_dir = Some "/tmp/proj" } ... ]} *)
type config = {
  skip_summarizer : bool;
  max_iterations_per_task : int;
  max_task_retries : int;
  max_recoveries : int;
  working_dir : string option;
      (** Enables [Workspace_surveyor] before planning. *)
  memory_dir : string option;
      (** Enables cross-run executor memory persistence. *)
  model : string;
  planner_system_prompt : string option;
  executor_system_prompt : string;
      (** Base system prompt for each per-task executor; defaults to
          [Agent.default_system_prompt]. *)
  executor_system_blocks : (string * string) list;
      (** Extension-contributed system-prompt fragments (e.g. skill
          index). Each rendered as [<name>body</name>] in registration
          order, after the base. *)
  executor_strategy : unit -> Context.Strategy.t;
      (** Factory for the per-task executor's context strategy. The
          factory is called ONCE per [run] invocation, so stateful
          strategies (like [sliding_window_at]) get fresh per-run state
          and don't leak across runs. *)
}

(** Default factory: builds a fresh [sliding_window_at(60→30)] each
    call. Use this in [config.executor_strategy] for the standard
    soft-trigger sliding window with frozen cut anchor. *)
val default_executor_strategy : unit -> Context.Strategy.t

val default_config : config

val run :
  ?config:config ->
  goal:string ->
  tools:tool_def list ->
  unit ->
  agent_result
