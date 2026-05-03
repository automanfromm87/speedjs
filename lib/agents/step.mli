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

(** What [once] returns. The Context is always returned at its new
    state (assistant response + any tool_results appended), so
    callers can persist / inspect it. Pause / Terminal-tool DON'T
    appear here — they are surfaced via [Effects.Pause] /
    [Effects.Terminal] so upstream combinators can intercept (e.g.
    install an auto-answer handler for tests). The default handler
    in [Agent.execute] translates them into [Agent.Waiting] /
    [Agent.Terminal_tool]. *)
type result =
  | Continue of Context.t
      (** Tool batch dispatched; ready for another step. *)
  | Terminal_text of { answer : string; ctx : Context.t }
      (** Model emitted text-only with [End_turn] / [Stop_sequence]. *)
  | Failed of { reason : agent_error; ctx : Context.t }
      (** [Llm_max_tokens], [Llm_refused], [Stop_reason_unexpected]. *)

(** Dispatch all [Tool_use] blocks in [content] via the [Tool_calls]
    effect, returning matched [Tool_result] blocks in original order.
    [tools] is the registry the dispatcher will look names up in;
    typically [Context.tools ctx]. *)
val dispatch_tool_uses :
  tools:tool_def list -> content_block list -> content_block list

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
  ?model:string option ->
  ctx:Context.t ->
  unit ->
  result
