(** ReAct agent loop, written in pure effect-perform style. Knows
    nothing about HTTP, LLM APIs, or tool execution — it just performs
    [Effects.Llm_complete] / [Effects.Tool_calls] / [Effects.Log]
    effects. The handler installed at runtime decides everything.

    The loop's input is a [Context.t] (system + env + tools +
    conversation), with a [Strategy] controlling how the conversation
    materializes for each LLM call (flat / sliding window / compacted). *)

open Types

val default_max_iterations : int

val default_system_prompt : string

(** Concatenate all [Text] blocks (separated by newlines), drop tool_use
    and tool_result blocks. *)
val extract_final_text : content_block list -> string

(** Dispatch all [Tool_use] blocks in [content] via the [Tool_calls]
    effect, returning matching [Tool_result] blocks in original order.
    Reusable by other agents (e.g. [Planner]) that drive their own loop. *)
val execute_tool_calls : content_block list -> content_block list

(** Raised when the model calls [ask_user]. The agent loop halts
    mid-conversation and the caller (typically [run_session]) catches
    this to return [Outcome_waiting]. *)
exception Wait_for_user of {
  tool_use_id : string;
  question : string;
  ctx_so_far : Context.t;
}

(** Raised when the model calls a tool listed in the loop's
    [terminal_tools] — e.g. [submit_task_result]. The loop short-circuits
    and the caller (typically [Plan_act]) parses the structured input. *)
exception Task_terminal_called of {
  tool_name : string;
  input : Yojson.Safe.t;
  ctx_so_far : Context.t;
}

(** Core loop. Returns either [Ok (answer, final_ctx)] or
    [Error (reason, partial_ctx)]. May raise [Wait_for_user] or
    [Task_terminal_called].

    [name] tags [Governor.Iteration_started] events for telemetry —
    defaults to ["agent"]; flow agents pass ["planner"] / ["executor"]. *)
val run_loop :
  ?max_iterations:int ->
  ?terminal_tools:string list ->
  ?strategy:Context.Strategy.t ->
  ?name:string ->
  ctx:Context.t ->
  unit ->
  (string * Context.t, agent_error * Context.t) Result.t

(** One-shot entry point: returns just the final answer string.

    [system_prompt] defaults to [default_system_prompt]. [system_blocks]
    are extension-contributed fragments (skill index, memory summary,
    workspace brief, ...) — each rendered as [<name>body</name>] in the
    system prompt, in registration order, after the base. *)
val run :
  ?max_iterations:int ->
  ?system_prompt:string ->
  ?system_blocks:(string * string) list ->
  user_query:string ->
  tools:tool_def list ->
  unit ->
  agent_result

(** Multi-turn entry point: takes seed [messages] and returns an
    [agent_outcome] carrying the updated history (including any pending
    ask_user tool_use). [system_prompt] / [system_blocks] same as
    [run]. *)
val run_session :
  ?max_iterations:int ->
  ?system_prompt:string ->
  ?system_blocks:(string * string) list ->
  messages:message list ->
  tools:tool_def list ->
  unit ->
  agent_outcome
