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
type system_block = { name : string; body : string }

(* ===== Construction ===== *)

val empty : t

(** Set the BASE system prompt — the most stable, prompt-cache-prefix
    part. Extension contributions go to [system_blocks] via
    [add_system_block] instead of being string-concatenated here. *)
val with_system_prompt : string -> t -> t

val with_tools : tool_def list -> t -> t

(** Append a single tool to the existing tool list (preserves order). *)
val add_tool : tool_def -> t -> t

(** Append several tools to the existing list (preserves order). *)
val add_tools : tool_def list -> t -> t

(** Append a context block; tag is wrapped as [<tag>body</tag>] in the
    rendered system prompt. Use for ambient context like workspace
    briefs, current time, project state. *)
val with_env : tag:string -> body:string -> t -> t

(** Add an extension-contributed system-prompt fragment, wrapped as
    [<name>body</name>]. Multiple contributors can call this in any
    order; they're rendered in registration order, AFTER the base
    [system_prompt] and BEFORE the [env] blocks. Stable ordering
    preserves prompt-cache stability when extensions don't change. *)
val add_system_block : name:string -> body:string -> t -> t

(** Set the base [system_prompt] and append [(name, body)] pairs as
    [system_blocks] (skipping empty bodies). *)
val apply_system :
  ?system_prompt:string ->
  ?system_blocks:(string * string) list ->
  t ->
  t

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
  (** A materialization strategy is pure data: each variant + its
      parameters fully determines behavior, so strategies can be
      shown, compared, and (for data-only variants) serialized.

      [Sliding_window_at]'s cut anchor is derived from the current
      message-list length via a closed-form formula. When the list
      shrinks (e.g. [Plan_act]'s [Memory.restore] after a failed
      task), the anchor recomputes from the smaller [n]. *)
  type t =
    | Flat
    | Sliding_window of { keep_recent : int }
    | Sliding_window_at of { trigger_at : int; keep_recent : int }
    | Compacted of {
        compact_at : int;
        keep_recent : int;
        compactor : message list -> string;
            (** The compactor function is the only opaque field —
                summarisation is intrinsically a function. Other
                variants are fully data. *)
      }

  (** Apply the strategy to a message list. Used by
      [Context.to_llm_args] internally; most callers don't call
      directly. *)
  val apply : t -> message list -> message list

  (** Human-readable description, suitable for [Agent_spec.show]. The
      data variants render their parameters; [Compacted]'s compactor
      shows as [<fun>]. *)
  val label : t -> string

  (** Convenience aliases for the variants. [flat] / [sliding_window
      ~keep_recent] / [sliding_window_at ~trigger_at ~keep_recent] /
      [compacted ~compact_at ~keep_recent ~compactor]. *)
  val flat : t
  val sliding_window : keep_recent:int -> t
  val sliding_window_at : trigger_at:int -> keep_recent:int -> t
  val compacted :
    compact_at:int ->
    keep_recent:int ->
    compactor:(message list -> string) ->
    t
end

(* ===== Output ===== *)

(** Render the final system string. Order:
    {ol
    {- base [system_prompt]}
    {- [system_blocks] in registration order}
    {- [env] blocks in registration order}}
    Returns "" when all three are empty. *)
val render_system : t -> string

(** Materialize for one [Llm_complete] call. *)
val to_llm_args :
  ?strategy:Strategy.t ->
  ?tool_choice:tool_choice ->
  t ->
  llm_call_args
