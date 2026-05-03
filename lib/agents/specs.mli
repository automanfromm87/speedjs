(** Canonical [Agent_spec.t] constructors for the roles the system
    knows about: planner, recovery, executor (per-task), chat
    (multi-turn free-form), subagent (delegate target).

    A role spec is just a starting point — callers pipe through
    [Agent_spec] field-update helpers ([with_skill], [with_max_iters],
    ...) to specialize. The synthetic terminal tools the planner /
    recovery / executor rely on ([submit_plan], [submit_recovery],
    [submit_task_result]) live here too, since they exist purely to
    serve their spec. Parsers for those tools' payloads stay in
    [Planner] / [Plan_act]. *)

open Types

(** {1 Terminal-tool names}

    The synthetic terminal tools live inside their respective specs;
    callers only need the names to pattern-match on [Terminal_tool]
    outputs. The tool definitions themselves are spec-private. *)

val submit_plan_name : string
val submit_recovery_name : string
val submit_task_result_name : string

(** {1 Planner system prompts}

    Two flavors. The constructors in {!planner} default to the
    [Sequential] form; the [Dag] form is used by [Plan_act] when
    [config.plan_mode = `Dag]. *)

val default_planner_prompt : string
val dag_planner_prompt : string

(** {1 Role specs}

    Each constructor sets sensible defaults for the role (mode,
    system_prompt, terminal). Callers pass [tools] (the full
    research / executor surface) and override anything else via
    [Agent_spec] helpers. *)

(** Planner role: read-only research + [submit_plan]. The provided
    [tools] are filtered through [tools_for_mode Planner] when the
    spec is executed; mutating tools are dropped automatically.
    [model] routes this spec's LLM calls to a specific model
    ([None] = runtime default). *)
val planner :
  ?system_prompt:string ->
  ?max_iters:int ->
  ?model:string ->
  tools:tool_def list ->
  unit ->
  Agent_spec.validated

(** Recovery role: same surface as planner, terminates on
    [submit_recovery]. [name] defaults to ["recovery"] but callers
    typically override per-cycle (e.g. ["recovery#0"]). [model] like
    {!planner}. *)
val recovery :
  ?name:string ->
  ?system_prompt:string ->
  ?max_iters:int ->
  ?model:string ->
  tools:tool_def list ->
  unit ->
  Agent_spec.validated

(** Executor role: full tool surface + [submit_task_result] terminal.
    Used inside [Plan_act] for one-task ReAct runs. [model] like
    {!planner}; this is the highest-token-volume call site, so model
    choice here dominates the run's cost. *)
val executor :
  ?system_prompt:string ->
  ?system_blocks:(string * string) list ->
  ?strategy:Context.Strategy.t ->
  ?max_iters:int ->
  ?model:string ->
  ?env:(string * string) list ->
  tools:tool_def list ->
  unit ->
  Agent_spec.validated

(** Chat role: full tool surface, [Free_text] terminal, [Executor]
    mode. Used by the multi-turn CLI and one-shot pure-ReAct mode. *)
val chat :
  ?system_prompt:string ->
  ?system_blocks:(string * string) list ->
  ?max_iters:int ->
  ?model:string ->
  tools:tool_def list ->
  unit ->
  Agent_spec.validated

(** Subagent role: focused delegate run. [Subagent] mode +
    [Free_text] terminal. The mode filter drops tools that opt out of
    [Subagent] (notably [parallel_delegate], capped at the executor
    layer to prevent recursive fan-out). The serial [delegate] tool
    IS available — nested delegate is allowed; [Governor.max_subagent_depth]
    is the cap. *)
val subagent :
  ?system_prompt:string ->
  ?max_iters:int ->
  ?model:string ->
  tools:tool_def list ->
  unit ->
  Agent_spec.validated
