(** Per-agent message-list memory with optional disk persistence.

    Unifies the "accumulating message history + checkpoint + restore"
    pattern that plan-act uses for the executor and that helix's
    BaseAgent uses for every agent. The on-disk layout matches
    [Session.t] (messages + model + pending_tool_use_id) so a memory
    file can also be loaded as a session.

    Persistence goes through [Effects.File_*] effects, so sandbox /
    audit / read-cache middleware applies automatically; tests can run
    Memory entirely on an in-memory FS via [File_handler.install_fs]. *)

open Types

(** Internally stored REVERSED so [push] is O(1); [to_messages] reverses
    back. With long sessions (100+ messages), the previous [@ [...]]
    append was quadratic — tests didn't catch it but profilers would. *)
type t = {
  name : string;
  dir : string option;
  model : string;
  mutable rev_messages : message list;
  mutable len : int;
}

let path_of ~dir ~name = Filename.concat dir (name ^ ".json")

(* ===== JSON shape (compatible with Session.t on-disk format) ===== *)

let to_json t : Yojson.Safe.t =
  `Assoc
    [
      ("model", `String t.model);
      ( "messages",
        `List (List.map Codec.message_to_json (List.rev t.rev_messages)) );
      ("pending_tool_use_id", `Null);
    ]

let message_of_json = function
  | `Assoc fs ->
      let role =
        match List.assoc_opt "role" fs with
        | Some (`String "assistant") -> Assistant
        | _ -> User
      in
      let content =
        match List.assoc_opt "content" fs with
        | Some (`List items) -> List.map Codec.content_block_of_json items
        | _ -> []
      in
      { role; content }
  | _ -> failwith "message must be a JSON object"

let parse_messages_from_json (j : Yojson.Safe.t) : message list =
  match j with
  | `Assoc fs -> (
      match List.assoc_opt "messages" fs with
      | Some (`List items) -> List.map message_of_json items
      | _ -> [])
  | _ -> []

(* ===== Construction ===== *)

(** Try to load prior messages from disk via [File_*] effects. Errors
    (missing file, parse failure) are swallowed — Memory is a best-effort
    cache, not a load-bearing data source. *)
let try_load_messages ~dir ~name : message list =
  let path = path_of ~dir ~name in
  match Effect.perform (Effects.File_stat path) with
  | `Missing | `Dir -> []
  | `File -> (
      match Effect.perform (Effects.File_read path) with
      | Error _ -> []
      | Ok body -> (
          try parse_messages_from_json (Yojson.Safe.from_string body)
          with _ -> []))

(** Construct an empty memory handle. If [dir] is given AND a prior file
    exists, loads its messages on creation. *)
let create ?(model = "claude-sonnet-4-5-20250929") ?dir ~name () : t =
  let messages =
    match dir with
    | None -> []
    | Some d -> try_load_messages ~dir:d ~name
  in
  {
    name;
    dir;
    model;
    rev_messages = List.rev messages;
    len = List.length messages;
  }

let length t = t.len
let to_messages t = List.rev t.rev_messages

let set t msgs =
  t.rev_messages <- List.rev msgs;
  t.len <- List.length msgs

(** Append one message. O(1). *)
let push t msg =
  t.rev_messages <- msg :: t.rev_messages;
  t.len <- t.len + 1

(** Snapshot the current length for later [restore]. *)
let checkpoint t : int = t.len

(** Truncate to the [n]-prefix. No-op if already at or below [n]. *)
let restore t n =
  if n < t.len then begin
    let drop = t.len - n in
    let rec skip k = function
      | rest when k <= 0 -> rest
      | [] -> []
      | _ :: rest -> skip (k - 1) rest
    in
    t.rev_messages <- skip drop t.rev_messages;
    t.len <- n
  end

(** Write the current messages via [File_write]. No-op when [dir = None].
    Errors are swallowed — persistence is best-effort. *)
let persist t =
  match t.dir with
  | None -> ()
  | Some dir ->
      let path = path_of ~dir ~name:t.name in
      let body = Yojson.Safe.pretty_to_string (to_json t) in
      let _ = Effect.perform (Effects.File_write { path; content = body }) in
      ()
