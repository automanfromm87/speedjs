(** Unified durable state for [Plan_act.run]: control plane (plan /
    pending / completed / recovery) AND data plane (executor message
    history) in a single file written atomically through
    [Effects.File_*]. Replaces the older two-file design (plan_state +
    executor) which had a split-brain crash window between the two
    writes. *)

open Types

let plan_state_name = "plan_state"

let path_of ~dir = Filename.concat dir (plan_state_name ^ ".json")

type t = {
  goal : string;
  plan : plan;
  pending : task list;
      (** Tasks still to run, in order. *)
  completed_rev : (task * (string, string) result) list;
      (** Completed tasks, REVERSED (matches Plan_act.drive's [acc]
          accumulator so save/restore is a direct field swap). *)
  recoveries : int;
  prior_failures : (string * string) list;
      (** [(task_description, error)] from earlier recovery cycles. *)
  executor_messages : message list;
      (** Executor's accumulating conversation history across tasks
          (helix-style cross-task memory). Persisted in this same
          file so that a crash between control and data plane writes
          can't leave them inconsistent. *)
}

(* ===== JSON ===== *)

let task_to_json (t : task) : Yojson.Safe.t =
  `Assoc
    [
      ("index", `Int t.index);
      ("description", `String t.description);
      ( "depends_on",
        `List (List.map (fun n -> `Int n) t.depends_on) );
    ]

let task_of_json = function
  | `Assoc fs ->
      let index =
        match List.assoc_opt "index" fs with Some (`Int n) -> n | _ -> 0
      in
      let description =
        Json_decode.get_string_field_or "description" ~default:"" fs
      in
      let depends_on =
        Json_decode.get_pos_int_list_field_or_empty "depends_on" fs
      in
      { index; description; depends_on }
  | _ -> failwith "task must be JSON object"

let result_to_json = function
  | Ok s -> `Assoc [ ("ok", `Bool true); ("value", `String s) ]
  | Error s -> `Assoc [ ("ok", `Bool false); ("value", `String s) ]

let result_of_json = function
  | `Assoc fs ->
      let ok = Json_decode.get_bool_field_or "ok" ~default:true fs in
      let value = Json_decode.get_string_field_or "value" ~default:"" fs in
      if ok then Ok value else Error value
  | _ -> Error "non-object result"

let to_json (s : t) : Yojson.Safe.t =
  `Assoc
    [
      ("version", `Int 2);
      ("goal", `String s.goal);
      ( "plan",
        `Assoc
          [
            ("title", `String s.plan.title);
            ("goal", `String s.plan.goal);
            ("tasks", `List (List.map task_to_json s.plan.tasks));
          ] );
      ("pending", `List (List.map task_to_json s.pending));
      ( "completed",
        `List
          (List.rev_map
             (fun (t, r) ->
               `Assoc
                 [ ("task", task_to_json t); ("result", result_to_json r) ])
             s.completed_rev) );
      ("recoveries", `Int s.recoveries);
      ( "prior_failures",
        `List
          (List.map
             (fun (d, e) ->
               `Assoc
                 [ ("description", `String d); ("error", `String e) ])
             s.prior_failures) );
      ( "executor_messages",
        `List (List.map Codec.message_to_json s.executor_messages) );
    ]

let plan_of_json = function
  | `Assoc fs ->
      let title = Json_decode.get_string_field_or "title" ~default:"" fs in
      let goal = Json_decode.get_string_field_or "goal" ~default:"" fs in
      let tasks =
        match List.assoc_opt "tasks" fs with
        | Some (`List items) -> List.map task_of_json items
        | _ -> []
      in
      { title; goal; tasks }
  | _ -> failwith "plan must be JSON object"

let of_json (j : Yojson.Safe.t) : t =
  match j with
  | `Assoc fs ->
      let goal = Json_decode.get_string_field_or "goal" ~default:"" fs in
      let plan =
        match List.assoc_opt "plan" fs with
        | Some j -> plan_of_json j
        | None -> { title = ""; goal; tasks = [] }
      in
      let pending =
        match List.assoc_opt "pending" fs with
        | Some (`List items) -> List.map task_of_json items
        | _ -> []
      in
      let completed_rev =
        match List.assoc_opt "completed" fs with
        | Some (`List items) ->
            List.rev_map
              (function
                | `Assoc cfs ->
                    let task =
                      match List.assoc_opt "task" cfs with
                      | Some j -> task_of_json j
                      | None -> { index = 0; description = ""; depends_on = [] }
                    in
                    let result =
                      match List.assoc_opt "result" cfs with
                      | Some j -> result_of_json j
                      | None -> Error "missing result"
                    in
                    (task, result)
                | _ -> failwith "completed entry must be object")
              items
        | _ -> []
      in
      let recoveries =
        match List.assoc_opt "recoveries" fs with
        | Some (`Int n) -> n
        | _ -> 0
      in
      let prior_failures =
        match List.assoc_opt "prior_failures" fs with
        | Some (`List items) ->
            List.map
              (function
                | `Assoc pfs ->
                    let d =
                      Json_decode.get_string_field_or "description" ~default:"" pfs
                    in
                    let e =
                      Json_decode.get_string_field_or "error" ~default:"" pfs
                    in
                    (d, e)
                | _ -> ("", ""))
              items
        | _ -> []
      in
      let executor_messages =
        match List.assoc_opt "executor_messages" fs with
        | Some (`List items) ->
            (try List.map Codec.message_of_json items with _ -> [])
        | _ -> []
      in
      {
        goal;
        plan;
        pending;
        completed_rev;
        recoveries;
        prior_failures;
        executor_messages;
      }
  | _ -> failwith "plan_state root must be JSON object"

(* ===== Persistence (via File effects) ===== *)

let save ~dir (s : t) : unit =
  let path = path_of ~dir in
  let body = Yojson.Safe.pretty_to_string (to_json s) in
  let _ = Effect.perform (Effects.File_write { path; content = body }) in
  ()

(** Load + verify the persisted goal matches. Returns [None] if the
    file is missing, malformed, or stores a different goal — caller
    should fall through to fresh planning. Parse failures and
    goal-mismatch are logged via [Effects.Log] so silent fallback to
    "fresh planner run" is visible. *)
let try_load ~dir ~goal : t option =
  let path = path_of ~dir in
  match Effect.perform (Effects.File_stat path) with
  | `Missing | `Dir -> None
  | `File -> (
      match Effect.perform (Effects.File_read path) with
      | Error msg ->
          Effect.perform
            (Effects.Log
               (Printf.sprintf
                  "[plan_state] %s: read failed (%s) — replanning" path msg));
          None
      | Ok body ->
          if String.trim body = "" then None
          else (
            try
              let s = of_json (Yojson.Safe.from_string body) in
              if s.goal = goal then Some s
              else (
                Effect.perform
                  (Effects.Log
                     (Printf.sprintf
                        "[plan_state] %s: goal mismatch (saved=%S, current=%S) — replanning"
                        path s.goal goal));
                None)
            with e ->
              Effect.perform
                (Effects.Log
                   (Printf.sprintf
                      "[plan_state] %s: parse failed (%s) — replanning"
                      path (Printexc.to_string e)));
              None))

(** Mark the run as complete by writing an empty body. The next [try_load]
    will skip this path. (No File_delete effect; empty body is the
    sentinel.) *)
let clear ~dir : unit =
  let path = path_of ~dir in
  let _ = Effect.perform (Effects.File_write { path; content = "" }) in
  ()
