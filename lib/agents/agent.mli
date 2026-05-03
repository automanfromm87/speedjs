(** ReAct agent loop, written in pure effect-perform style. Knows
    nothing about HTTP, LLM APIs, or tool execution — it just performs
    [Effects.Llm_complete] / [Effects.Tool_calls] / [Effects.Log]
    effects. The handler installed at runtime decides everything.

    The public entry point is {!execute}; it takes a fully declared
    [Agent_spec.t] and an [input] and returns an [output]. The spec
    is the AUTHORITY for what a leaf agent is: prompt, tool surface,
    mode (which gates the surface), strategy, iteration cap, and
    terminal protocol. [Agent.execute] reads the spec — the runtime
    layer is registry-agnostic and trusts the spec's tool list (which
    flows through [Tool_calls] effect payloads). The input
    distinguishes Fresh / Resume; the output discriminates Done /
    Terminal_tool / Waiting / Failed. *)

open Types

val default_max_iterations : int
val default_system_prompt : string

(** {1 Unified entry point}

    Pause / Terminal-tool semantics flow through {!Effects.Pause} /
    {!Effects.Terminal} effects, NOT exceptions. The default handler
    installed by [execute] translates them to [Waiting] /
    [Terminal_tool] outputs; combinators above [execute] (workflow
    layer) can install their own handlers — e.g. an auto-answer
    handler that resumes [Pause] with a synthetic [Tool_result]
    instead of suspending the run. *)

(** Input to {!execute}.
    - [Fresh q]: brand-new conversation seeded with one user turn.
    - [Resume msgs]: pick up an existing conversation. [msgs] must be
      a valid [Conversation] (strict alternation, etc.). If the
      trailing turn is a dangling Assistant tool_use the run will
      reject — append a [Tool_result] for it (or use
      {!Conversation.close_dangling_with_ack}) before calling
      execute. Append a fresh user turn before calling for the
      "carry history forward + new instruction" case (chat session
      does this via [Session.append_input]; orchestrators that need
      it can use [Conversation.close_dangling_with_ack ~extra:[Text q]]
      to merge an ack and the new instruction in a single User turn). *)
type input =
  | Fresh of string
  | Resume of message list

(** Output of {!execute}. Always carries the final [messages] so the
    caller can persist / inspect the conversation regardless of how
    the run ended. *)
type output =
  | Done of { answer : string; messages : message list }
  | Terminal_tool of {
      name : string;
      payload : Yojson.Safe.t;
      messages : message list;
    }
  | Waiting of {
      tool_use_id : Id.Tool_use_id.t;
      question : string;
      messages : message list;
    }
  | Failed of { reason : agent_error; messages : message list }

(** Run [spec] against [input], performing {!Effects.Pause} /
    {!Effects.Terminal} effects without catching them — the CALLER
    must install a handler. Use this when a workflow combinator
    wants custom Pause / Terminal semantics (e.g. auto-answer
    Pause with a synthetic [Tool_result] message instead of
    suspending, or treat a specific Terminal_tool as a workflow
    return value).

    If neither effect fires (the loop ends naturally with Done /
    Failed), this returns the corresponding [output]. If an effect
    DOES fire, this expression doesn't return — control transfers
    to the outer effect handler.

    Most callers want {!execute}, which wraps this with the default
    handler (Pause → Waiting, Terminal → Terminal_tool). *)
val execute_raw : spec:Agent_spec.validated -> input:input -> output

(** Run [spec] against [input] with the default Pause/Terminal
    handler installed: Pause → [Waiting] output (no resume),
    Terminal → [Terminal_tool] output. Most leaf agent invocations
    use this. *)
val execute : spec:Agent_spec.validated -> input:input -> output

(** {1 Output helpers}

    Most call sites (planner / recovery / executor / sub-agent) want
    a typed result, not an [output] sum. These helpers consolidate
    the "expect this shape, anything else is a structural error"
    branching that otherwise gets copy-pasted everywhere. *)

(** Expect [Done]. Returns the answer + final messages. Any other
    output becomes [Plan_invalid] with a diagnostic that names the
    actual variant. [name] is included in the diagnostic so the
    caller's role (planner / executor / chat) is recoverable from
    logs. *)
val expect_done :
  name:string -> output -> (string * message list, agent_error) Result.t

(** {1 Lower-level loop}

    [run_loop] is exported for advanced uses ([Step]-by-[Step]
    debuggers, custom drivers, parallel agents sharing a [Context]).
    Most callers should use {!execute}. *)
val run_loop :
  ?max_iterations:int ->
  ?terminal_tools:string list ->
  ?force_terminal_in_last_n:int ->
  ?strategy:Context.Strategy.t ->
  ?name:string ->
  ctx:Context.t ->
  unit ->
  (string * Context.t, agent_error * Context.t) Result.t
