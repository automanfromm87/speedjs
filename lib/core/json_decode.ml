(** JSON object decoder helpers used by tools, planner, and plan_act.

    All return [(_, string) result] so callers can short-circuit with
    [let*]. The error string identifies the offending field by name. *)

let ( let* ) = Result.bind

let with_object_input (input : Yojson.Safe.t) k =
  match input with
  | `Assoc fields -> k fields
  | _ -> Error "input must be a JSON object"

let get_string_field name fields =
  match List.assoc_opt name fields with
  | Some (`String s) -> Ok s
  | Some _ -> Error (Printf.sprintf "field '%s' must be a string" name)
  | None -> Error (Printf.sprintf "missing required field '%s'" name)

let get_int_field name fields =
  match List.assoc_opt name fields with
  | Some (`Int n) -> Ok n
  | Some _ -> Error (Printf.sprintf "field '%s' must be an integer" name)
  | None -> Error (Printf.sprintf "missing required field '%s'" name)

let get_bool_field name fields =
  match List.assoc_opt name fields with
  | Some (`Bool b) -> Ok b
  | Some _ -> Error (Printf.sprintf "field '%s' must be a boolean" name)
  | None -> Error (Printf.sprintf "missing required field '%s'" name)

(** Optional string with default: missing or wrong-typed → [default]. *)
let get_string_field_or name ~default fields =
  match List.assoc_opt name fields with
  | Some (`String s) -> s
  | _ -> default

(** Optional bool with default. *)
let get_bool_field_or name ~default fields =
  match List.assoc_opt name fields with
  | Some (`Bool b) -> b
  | _ -> default

(** Get a list field; missing or wrong-typed → empty list. *)
let get_list_field_or_empty name fields =
  match List.assoc_opt name fields with
  | Some (`List items) -> items
  | _ -> []
