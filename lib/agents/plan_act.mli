(** Plan-Act flow: planner → per-task executor (with persistent memory)
    → recovery → summarizer.

    All LLM calls go through [Effects.Llm_complete], so the same handler
    stack (production / mock / checkpoint / retry / protection / cost
    tracking) works transparently. *)

open Types

(** Structured payload submitted via [submit_task_result]. Carries
    explicit success / failure semantics, distinct from an [End_turn]
    free-text response. *)
type task_submit = {
  ts_success : bool;
  ts_result : string;
  ts_error : string;
}

(** Outcome of {!run_for_task}. *)
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
      (** Default model — used by every leaf without a per-spec
          override. *)
  planner_model : string option;
      (** Override for [Planner.plan]. None inherits [model]. *)
  executor_model : string option;
      (** Override for the per-task ReAct loop (carries ~93% of total
          tokens in our chaos baseline). Set to a cheaper model to cut
          costs. *)
  recovery_model : string option;
      (** Override for [Planner.recover]. None inherits [model]. *)
  summarizer_model : string option;
      (** Override for the final synthesizer call. None inherits
          [model]. *)
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
  restart : bool;
      (** When true, ignore any persisted [plan_state.json] and run
          the planner from scratch. The new plan overwrites the old
          state file. *)
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
