(** Per-agent message-list memory with optional disk persistence.

    Unifies the "accumulating message history + checkpoint + restore"
    pattern that plan-act uses for the executor and that helix's
    BaseAgent uses for every agent. Persistence reuses [Session.t] as
    the on-disk shape (so a memory file is also a valid loadable
    session) but exposes a memory-first API.

    Use as a mutable handle threaded through code paths:
    {[
      let mem = Memory.create ~name:"executor" ~dir:(Some "/tmp/speedjs-memory") () in
      Memory.push mem msg;
      let cp = Memory.checkpoint mem in
      ... run task ...
      if failed then Memory.restore mem cp;
      Memory.persist mem;
    ]}

    The on-disk path is [<dir>/<name>.json]. With [dir = None], persist
    is a no-op (in-memory only). *)

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

(** Construct an empty memory handle. If [dir] is given AND a prior file
    exists, loads its messages on creation. *)
let create ?(model = "claude-sonnet-4-5-20250929") ?dir ~name () : t =
  let messages =
    match dir with
    | None -> []
    | Some d -> (
        let p = path_of ~dir:d ~name in
        match Session.load ~path:p with
        | Some s -> s.messages
        | None -> [])
  in
  { name; dir; model; rev_messages = List.rev messages; len = List.length messages }

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

(** Idempotent [mkdir -p]. Inline (not via [Tools]) to keep the
    [core/] layer free of [tools/] dependencies. *)
let ensure_dir dir =
  if dir <> "" && dir <> "." && dir <> "/" && not (Sys.file_exists dir)
  then
    ignore
      (Sys.command (Printf.sprintf "mkdir -p %s" (Filename.quote dir)))

(** Write the current messages to disk. No-op when [dir = None]. *)
let persist t =
  match t.dir with
  | None -> ()
  | Some dir ->
      ensure_dir dir;
      Session.save ~path:(path_of ~dir ~name:t.name)
        {
          messages = to_messages t;
          pending_tool_use_id = None;
          model = t.model;
        }
