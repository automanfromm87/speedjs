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

(** {1 Control-flow exception policy}

    The two exceptions below are protocol-level interrupts: they
    suspend the ReAct loop because the caller above the agent (CLI /
    plan-act orchestrator) needs to act on a synchronous handoff.
    They are NOT errors. Errors flow as [Result.Error agent_error]
    through normal returns.

    Rule of thumb: an [exception] in this codebase is reserved for
    cases where the caller must intercept and resume out-of-band —
    [Wait_for_user] (suspend until user replies) and
    [Task_terminal_called] (suspend so the orchestrator can parse
    structured submit input). Anything that fits "this run failed"
    should be a typed [agent_error] returned via [Result], not
    raised. Adding a new exception here requires the same
    justification.
*)

(** Raised when the model calls [ask_user]. The agent loop halts
    mid-conversation and the caller (typically [run_session]) catches
    this to return [Outcome_waiting]. *)
exception Wait_for_user of {
  tool_use_id : Id.Tool_use_id.t;
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

(** Run a ReAct loop until [terminal_tool_name] is called and return
    its parsed JSON input. Used by orchestrators (planner, recovery,
    plan_act executor) that need a structured submit payload, not a
    free-form text answer. The terminal tool's [handler] is never
    invoked — the loop intercepts via [terminal_tools] and raises
    [Task_terminal_called].

    Outcomes:
    - terminal tool called → [Ok input]
    - End_turn / Max_iterations / Failed without terminal call →
      [Error agent_error] (typically [Plan_invalid])
    - [Wait_for_user] propagates as the [Wait_for_user] exception.

    Replaces ad-hoc copies of this loop in [Planner.plan] /
    [Planner.recover] / [Plan_act.run_for_task] — they all do the
    same shape: build a one-shot conversation, run a ReAct loop,
    catch a specific tool's submission. *)
val run_until_terminal_tool :
  ?max_iterations:int ->
  ?system_prompt:string ->
  ?system_blocks:(string * string) list ->
  ?name:string ->
  terminal_tool_name:string ->
  user_query:string ->
  tools:tool_def list ->
  unit ->
  (Yojson.Safe.t, agent_error) Result.t

(** Multi-turn entry point: takes seed [messages] and returns an
    [session_result] carrying the updated history (including any pending
    ask_user tool_use). [system_prompt] / [system_blocks] same as
    [run]. *)
val run_session :
  ?max_iterations:int ->
  ?system_prompt:string ->
  ?system_blocks:(string * string) list ->
  messages:message list ->
  tools:tool_def list ->
  unit ->
  session_result
