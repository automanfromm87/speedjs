(** Single ReAct step — one LLM call + (optional) tool batch dispatch +
    Conversation update.

    [Agent.run_loop] is the standard driver, but [Step.once] can also be
    invoked directly to build custom loops:
    {ul
    {- step-through debugger}
    {- bounded experiments ("run 5 steps then dump ctx")}
    {- parallel agents sharing a Context}
    {- periodic reflection injection}
    {- stream-the-trace tooling}}

    [once] is pure modulo [Effects.Llm_complete] / [Effects.Tool_calls] /
    [Effects.Log] effects. It does NOT log iteration counters or perform
    [Governor.Iteration_started] — those are the driver's job. *)

open Types

(** What happened in one step. The Context is always returned at its
    new state (assistant response appended, tool_results appended if
    any), so callers can persist / inspect it. *)
type outcome =
  | Continue of Context.t
      (** Tool batch dispatched; ready for another step. *)
  | Terminal_text of { answer : string; ctx : Context.t }
      (** Model emitted text-only with [End_turn] / [Stop_sequence]. *)
  | Terminal_tool of {
      tool_name : string;
      input : Yojson.Safe.t;
      ctx : Context.t;
    }
      (** Model called a tool listed in [terminal_tools]. The tool was
          NOT dispatched — caller decides what to do with [input]
          (e.g. [submit_task_result] parsing in plan-act). *)
  | Wait_for_user of {
      tool_use_id : Id.Tool_use_id.t;
      question : string;
      ctx : Context.t;
    }
      (** Model called the [ask_user] pause-tool. Caller should suspend
          the run and persist [ctx]; resume by appending the user's
          answer as a [Tool_result] for [tool_use_id]. *)
  | Failed of { reason : agent_error; ctx : Context.t }
      (** [Llm_max_tokens], [Llm_refused], [Stop_reason_unexpected]. *)

(** Dispatch all [Tool_use] blocks in [content] via the [Tool_calls]
    effect, returning matched [Tool_result] blocks in original order.
    Reusable by other agents (e.g. [Planner]) that drive their own loop. *)
val dispatch_tool_uses : content_block list -> content_block list

(** Concatenate all [Text] blocks (newline-separated). Drop tool_use /
    tool_result blocks. *)
val extract_final_text : content_block list -> string

(** Run exactly ONE ReAct step on [ctx]:
    {ol
    {- materialize args via [strategy] + [tool_choice]}
    {- perform [Llm_complete]}
    {- append assistant content to ctx}
    {- inspect [stop_reason] / blocks → return [outcome]}}

    [terminal_tools]: names that short-circuit dispatch. *)
val once :
  ?strategy:Context.Strategy.t ->
  ?tool_choice:tool_choice ->
  ?terminal_tools:string list ->
  ctx:Context.t ->
  unit ->
  outcome
