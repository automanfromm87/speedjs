(** Checkpointing: durable agent execution via JSONL "tape".

    On the first run, every LLM response and tool batch result is
    appended to the tape. If the process is killed mid-flight, running
    again with the same tape replays previously-recorded steps for free,
    then seamlessly switches to live mode at the crash point.

    Architecture: tape is a [session] (mutable pending list + out_channel)
    shared between two middleware:

    {[
      let session = Checkpoint.open_session ~path () in
      let llm = llm_chain |> Checkpoint.with_tape_llm session in
      Llm_handler.install llm (fun () ->
        Checkpoint.install_tools_with_tape session ~tools tool_chain
          thunk)
    ]}

    Both share [session.pending], so LLM and Tool entries replay in the
    correct INTERLEAVED order regardless of which middleware sees them
    first.

    Tape format (JSONL): each line is a [tape_entry]:
    {v
      {"kind":"llm","data":{...}}
      {"kind":"tool_batch","data":[{"id":...,"result":...}, ...]}
    v} *)

open Types

(** Tool result on tape: encodes Ok/Error so error semantics survive replay. *)
type tape_tool_result = Tt_ok of string | Tt_err of string

type tape_entry =
  | Tape_llm of llm_response
  | Tape_tool_batch of (Id.Tool_use_id.t * tape_tool_result) list
      (** [(use_id, result)] in input order. Single-tool calls use a
          1-element list. *)

let tape_tool_result_to_json = function
  | Tt_ok s -> `Assoc [ ("ok", `String s) ]
  | Tt_err s -> `Assoc [ ("err", `String s) ]

let tape_tool_result_of_json = function
  | `Assoc fields -> (
      match (List.assoc_opt "ok" fields, List.assoc_opt "err" fields) with
      | Some (`String s), _ -> Tt_ok s
      | _, Some (`String s) -> Tt_err s
      | _ -> failwith "tool tape entry needs ok or err field")
  | _ -> failwith "tool tape entry must be object"

let tape_entry_to_json = function
  | Tape_llm r ->
      `Assoc [ ("kind", `String "llm"); ("data", Codec.llm_response_to_json r) ]
  | Tape_tool_batch results ->
      `Assoc
        [
          ("kind", `String "tool_batch");
          ( "data",
            `List
              (List.map
                 (fun (id, tr) ->
                   `Assoc
                     [
                       ("id", `String (Id.Tool_use_id.to_string id));
                       ("result", tape_tool_result_to_json tr);
                     ])
                 results) );
        ]

let tape_entry_of_json = function
  | `Assoc fields -> (
      let kind = List.assoc_opt "kind" fields in
      let data = List.assoc_opt "data" fields in
      match (kind, data) with
      | Some (`String "llm"), Some j -> Tape_llm (Codec.llm_response_of_json j)
      | Some (`String "tool_batch"), Some (`List items) ->
          let parsed =
            List.map
              (function
                | `Assoc fs ->
                    let id =
                      match List.assoc_opt "id" fs with
                      | Some (`String s) -> Id.Tool_use_id.of_string s
                      | _ -> Id.Tool_use_id.of_string ""
                    in
                    let r =
                      match List.assoc_opt "result" fs with
                      | Some j -> tape_tool_result_of_json j
                      | None -> Tt_err "missing result"
                    in
                    (id, r)
                | _ -> failwith "tool_batch item must be object")
              items
          in
          Tape_tool_batch parsed
      | _ -> failwith "unrecognized tape entry")
  | _ -> failwith "tape entry must be JSON object"

let tape_tool_to_result = function Tt_ok s -> Ok s | Tt_err s -> Error s
let result_to_tape_tool = function Ok s -> Tt_ok s | Error s -> Tt_err s

(** Read all entries from tape file. Returns [] if file doesn't exist. *)
let load_tape (path : string) : tape_entry list =
  if not (Sys.file_exists path) then []
  else begin
    let ic = open_in path in
    let entries = ref [] in
    (try
       while true do
         let line = input_line ic in
         let line = String.trim line in
         if line <> "" then
           entries :=
             tape_entry_of_json (Yojson.Safe.from_string line) :: !entries
       done
     with End_of_file -> ());
    close_in ic;
    List.rev !entries
  end

(** Open tape file for append. Creates if missing. *)
let open_tape_for_append (path : string) : out_channel =
  open_out_gen [ Open_wronly; Open_append; Open_creat ] 0o644 path

(** Append one entry, flushing immediately so a crash preserves it. *)
let append_entry (oc : out_channel) (e : tape_entry) =
  output_string oc (Yojson.Safe.to_string (tape_entry_to_json e));
  output_char oc '\n';
  flush oc

(* ===== Session: shared state for the tape middlewares ===== *)

type session = {
  mutable pending : tape_entry list;
  oc : out_channel;
  on_log : string -> unit;
  mutable n_replayed : int;
  mutable n_live_llm : int;
  total_in_tape : int;
  crash_after_live_llm : int option;
  path : string;
}

let open_session ~path ?(on_log = ignore) ?crash_after_live_llm () : session
    =
  let pending = load_tape path in
  let oc = open_tape_for_append path in
  let total = List.length pending in
  if total > 0 then
    on_log
      (Printf.sprintf
         "[tape] loaded %d entries from %s — will replay before live"
         total path);
  {
    pending;
    oc;
    on_log;
    n_replayed = 0;
    n_live_llm = 0;
    total_in_tape = total;
    crash_after_live_llm;
    path;
  }

let close_session (s : session) = close_out s.oc

let bump_replayed s =
  s.n_replayed <- s.n_replayed + 1;
  s.on_log
    (Printf.sprintf "  [tape] replayed entry (%d/%d)" s.n_replayed
       s.total_in_tape)

let maybe_simulate_crash s =
  match s.crash_after_live_llm with
  | Some n when s.n_live_llm >= n ->
      s.on_log
        (Printf.sprintf
           "[demo] simulating crash after %d live LLM calls (tape preserved at %s)"
           s.n_live_llm s.path);
      close_session s;
      exit 137
  | _ -> ()

(* ===== LLM middleware: replay/record Tape_llm ===== *)

let with_tape_llm (s : session) (inner : Llm_handler.t) : Llm_handler.t =
 fun args ->
  match s.pending with
  | Tape_llm r :: rest ->
      s.pending <- rest;
      bump_replayed s;
      r
  | Tape_tool_batch _ :: _ ->
      failwith
        "tape misalignment: expected LLM entry, got tool_batch. Agent \
         path diverged from recording — delete tape and start over"
  | [] ->
      let r = inner args in
      append_entry s.oc (Tape_llm r);
      s.n_live_llm <- s.n_live_llm + 1;
      maybe_simulate_crash s;
      r

(* ===== Tool installer: replay/record Tape_tool_batch =====

   Tool tape lives at the install / batch level, not per-call: tape
   entries record one [Tool_calls] effect's full result. So this is a
   replacement for [Tool_handler.install] rather than a chain
   middleware. *)

let install_tools_with_tape (s : session) ~tools (chain : Tool_handler.t)
    f =
  let open Effect.Deep in
  let find_tool name =
    List.find_opt (fun (t : Types.tool_def) -> t.name = name) tools
  in
  let rec wrap : type r. (unit -> r) -> r =
   fun thunk ->
    try_with thunk ()
      {
        effc =
          (fun (type a) (eff : a Effect.t) ->
            match eff with
            | Effects.Tool_calls uses ->
                Some
                  (fun (k : (a, _) continuation) ->
                    let results =
                      match s.pending with
                      | Tape_tool_batch saved :: rest ->
                          s.pending <- rest;
                          bump_replayed s;
                          List.map
                            (fun (id, tr) -> (id, tape_tool_to_result tr))
                            saved
                      | Tape_llm _ :: _ ->
                          failwith
                            "tape misalignment: expected tool_batch, got LLM"
                      | [] ->
                          (* Live: emit Tool_started ticks on main fiber
                             (workers can't perform effects), then dispatch
                             through chain, then emit Tool_finished /
                             Tool_timeout from main fiber. *)
                          List.iter
                            (fun (id, name, input) ->
                              Tool_handler.perform_tool_started_tick ~name
                                ~use_id:id ~input)
                            uses;
                          let one_with_truncate use =
                            let _, name, _ = use in
                            let t0 = Unix.gettimeofday () in
                            let id, r =
                              Tool_handler.dispatch_one ~chain ~tools use
                            in
                            let duration = Unix.gettimeofday () -. t0 in
                            (id, name, Tool_handler.truncate_result name r,
                             duration)
                          in
                          let timed =
                            match uses with
                            | [ single ] ->
                                [ wrap (fun () -> one_with_truncate single) ]
                            | _ ->
                                Parallel.map_threaded one_with_truncate uses
                          in
                          List.iter
                            (fun (id, name, r, duration) ->
                              match find_tool name with
                              | Some tool ->
                                  Tool_handler.perform_tool_finished_ticks
                                    ~tool ~use_id:id ~ok:(Result.is_ok r)
                                    ~duration
                              | None -> ())
                            timed;
                          let truncated =
                            List.map (fun (id, _, r, _) -> (id, r)) timed
                          in
                          let saved =
                            List.map
                              (fun (id, r) -> (id, result_to_tape_tool r))
                              truncated
                          in
                          append_entry s.oc (Tape_tool_batch saved);
                          truncated
                    in
                    continue k results)
            | _ -> None);
      }
  in
  wrap f
