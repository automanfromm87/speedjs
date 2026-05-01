(** Phantom-typed IDs.

    See [id.mli]. The implementation is just [string] under the hood;
    the .mli exposes [private string] so external modules can read but
    not freely construct. *)

module Tool_use_id = struct
  type t = string

  let of_string s = s
  let to_string t = t
  let equal = String.equal
  let compare = String.compare
end
