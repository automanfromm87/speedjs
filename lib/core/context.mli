(** Structured representation of one LLM call's input.

    The wire-level Anthropic API takes [system + tools + messages]. The
    agent-level conceptual model is richer: [system_prompt], [env]
    context blocks (workspace brief, current time, ...), [tools], and
    the actual conversation. [Context.t] models these separately so a
    [Strategy] can manage how the conversation materializes for the API
    — flat, sliding window, or compacted with a summary placeholder.

    See [context.ml] for the full design rationale. *)

open Types

type t

type env_block = { tag : string; body : string }

(* ===== Construction ===== *)

val empty : t

val with_system_prompt : string -> t -> t
val with_tools : tool_def list -> t -> t

(** Append a context block; tag is wrapped as [<tag>body</tag>] in the
    rendered system prompt. Use for ambient context like workspace
    briefs, current time, project state. *)
val with_env : tag:string -> body:string -> t -> t

val with_conversation : Conversation.t -> t -> t

(* ===== Inspection ===== *)

val conversation : t -> Conversation.t
val tools : t -> tool_def list
val system_prompt : t -> string

(** Length of the underlying conversation (number of messages). *)
val length : t -> int

(* ===== Append (raise [Conversation.Invariant_violated] on misuse) ===== *)

val push_assistant : content_block list -> t -> t

(** Append a User turn carrying tool_results that close pending dangling
    tool_uses. [blocks] must contain a Tool_result for every dangling
    tool_use_id. *)
val push_tool_results : content_block list -> t -> t

(** Append a fresh User turn with a single text block. *)
val push_user_text : string -> t -> t

(* ===== Materialization strategy ===== *)

module Strategy : sig
  type t = message list -> message list

  (** No-op: messages pass through unchanged. *)
  val flat : t

  (** Keep the LAST [keep_recent] messages, drop older ones. May break
      Conversation invariants — boundary [validate] is the safety net.
      Trims on EVERY call once exceeded — invalidates prompt cache
      every time. Use [sliding_window_at] when cache matters. *)
  val sliding_window : keep_recent:int -> t

  (** Soft-trigger sliding window: only trim once messages exceed
      [trigger_at], then drop down to [keep_recent]. The headroom
      preserves cache stability across the calls in between. *)
  val sliding_window_at : trigger_at:int -> keep_recent:int -> t

  (** Replace oldest messages with a single summarized User turn when
      total exceeds [compact_at]. [compactor] is invoked on the
      to-be-summarized prefix; typical impls call an LLM. *)
  val compacted :
    compact_at:int ->
    keep_recent:int ->
    compactor:(message list -> string) ->
    t
end

(* ===== Output ===== *)

(** Render the final system string: [system_prompt] then env blocks
    formatted as [<tag>...</tag>]. Returns "" when both empty. *)
val render_system : t -> string

(** Materialize for one [Llm_complete] call. *)
val to_llm_args :
  ?strategy:Strategy.t ->
  ?tool_choice:tool_choice ->
  t ->
  llm_call_args
