(** Phantom-typed IDs that carry their kind in the type system.

    Tool-use IDs are passed around as correlation keys (a Tool_use block's
    [id] must match a later Tool_result's [tool_use_id]). Mixing them up
    with other strings (e.g. tool names) was previously a silent string
    bug; wrapping as [private string] makes the mistake a compile error
    while keeping zero-cost projection back to [string] for printing /
    serialization (the [(:>)] cast).

    Tool names are NOT phantom-typed: dispatch routinely pattern-matches
    against literals (e.g. [Tool_use { name = "submit_plan"; _ }]), and
    forcing [Tool_name.of_string "submit_plan"] in every match arm is
    net-negative ergonomically. *)

(** ID Anthropic generates per tool call (e.g. ["toolu_01ABC..."]).
    Used to pair a Tool_use block with its matching Tool_result. *)
module Tool_use_id : sig
  type t = private string

  val of_string : string -> t
  val to_string : t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
end
