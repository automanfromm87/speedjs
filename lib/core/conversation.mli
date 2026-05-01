(** Smart-constructor wrapper around [Types.message list].

    Encodes Anthropic's API contract as an internal state machine. The
    type [t] is abstract; the only way to BUILD a conversation is via
    the constructors below — every constructor checks invariants.

    Invariants enforced:
    1. Strict alternation: User → Assistant → User → ...
    2. First message is User (or empty).
    3. [Tool_use] blocks live ONLY in Assistant turns.
    4. [Tool_result] blocks live ONLY in User turns.
    5. Every [Tool_use] is paired with a [Tool_result] (matched by
       [tool_use_id]) in the IMMEDIATELY following User turn.
    6. Trailing dangling allowed: if the last message is an Assistant
       turn with unanswered tool_uses, that's a legitimate "waiting"
       state — caller MUST close it before sending to Anthropic.

    Code paths that still use raw [list @ [...]] operations get caught
    by [validate] / [of_messages] at the Anthropic boundary. *)

open Types

type t

exception Invariant_violated of string

(* ===== Construction ===== *)

val empty : t

(** Build a conversation from a raw list, validating every invariant.
    Accepts trailing dangling tool_uses (legitimate persisted state). *)
val of_messages : message list -> (t, string) result

(** Validate without constructing. Convenience for the API boundary. *)
val validate : message list -> (unit, string) result

(* ===== Inspection (read-only) ===== *)

val to_messages : t -> message list
val length : t -> int
val is_empty : t -> bool
val is_dangling : t -> bool
val pending_tool_use_ids : t -> string list

(* ===== Append (raise [Invariant_violated] on misuse) ===== *)

(** Append an Assistant turn. Valid only when the conversation is
    expecting an Assistant (i.e. previous turn was User). Detects
    [Tool_use] blocks and updates state accordingly. *)
val push_assistant : t -> content_block list -> t

(** Append a User turn that closes pending dangling tool_uses. [blocks]
    must contain a [Tool_result] for every dangling [tool_use_id]; other
    block types ([Text]) may be mixed in alongside, producing a single
    User turn (the Anthropic-friendly way to submit "tool_results AND
    new user instruction" — preserves strict alternation). *)
val push_user_with_results : t -> content_block list -> t

(** Append a fresh User turn. Valid when the conversation is empty or
    the previous Assistant turn was text-only (no pending tool_uses). *)
val push_user : t -> content_block list -> t

(** Convenience: append a fresh User turn carrying a single text block. *)
val push_user_text : t -> string -> t

(** Close any pending dangling tool_uses with a synthetic [Tool_result]
    using [ack] as content; optionally append [extra] blocks (e.g. a
    [Text] for the next task) in the SAME User turn. Idempotent if no
    dangling. *)
val close_dangling_with_ack :
  ?ack:string -> ?extra:content_block list -> t -> t
