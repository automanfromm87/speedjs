(** Persistent multi-turn session state.

    Stored as a JSON file with the message history and an optional pending
    [tool_use_id] (set when the last assistant turn called [ask_user] and
    is awaiting a user response). Each invocation of speedjs with the same
    session file:

    - Loads messages and pending state from disk.
    - If pending is set, treats the new user input as a [Tool_result]
      answering that tool_use_id.
    - Otherwise treats the input as a fresh user message.
    - Runs the agent, then writes the updated session back. *)

open Types

type t = {
  messages : message list;
  pending_tool_use_id : Id.Tool_use_id.t option;
  model : string;
}

let empty ?(model = "claude-sonnet-4-5-20250929") () : t =
  { messages = []; pending_tool_use_id = None; model }

let to_json (s : t) : Yojson.Safe.t =
  `Assoc
    [
      ("model", `String s.model);
      ("messages", `List (List.map Codec.message_to_json s.messages));
      ( "pending_tool_use_id",
        match s.pending_tool_use_id with
        | Some id -> `String (Id.Tool_use_id.to_string id)
        | None -> `Null );
    ]

let message_of_json = function
  | `Assoc fields ->
      let role =
        match List.assoc_opt "role" fields with
        | Some (`String "user") -> User
        | Some (`String "assistant") -> Assistant
        | _ -> User
      in
      let content =
        match List.assoc_opt "content" fields with
        | Some (`List items) ->
            List.map
              (fun j ->
                match j with
                | `Assoc fs -> (
                    match List.assoc_opt "type" fs with
                    | Some (`String "tool_result") ->
                        let tool_use_id =
                          match List.assoc_opt "tool_use_id" fs with
                          | Some (`String s) -> Id.Tool_use_id.of_string s
                          | _ -> Id.Tool_use_id.of_string ""
                        in
                        let content =
                          match List.assoc_opt "content" fs with
                          | Some (`String s) -> s
                          | _ -> ""
                        in
                        let is_error =
                          match List.assoc_opt "is_error" fs with
                          | Some (`Bool b) -> b
                          | _ -> false
                        in
                        Tool_result { tool_use_id; content; is_error }
                    | _ -> Codec.content_block_of_json j)
                | _ -> Codec.content_block_of_json j)
              items
        | _ -> []
      in
      { role; content }
  | _ -> failwith "session message must be JSON object"

let of_json (j : Yojson.Safe.t) : t =
  match j with
  | `Assoc fields ->
      let model =
        match List.assoc_opt "model" fields with
        | Some (`String s) -> s
        | _ -> "claude-sonnet-4-5-20250929"
      in
      let messages =
        match List.assoc_opt "messages" fields with
        | Some (`List items) -> List.map message_of_json items
        | _ -> []
      in
      let pending_tool_use_id =
        match List.assoc_opt "pending_tool_use_id" fields with
        | Some (`String s) -> Some (Id.Tool_use_id.of_string s)
        | _ -> None
      in
      { messages; pending_tool_use_id; model }
  | _ -> failwith "session root must be JSON object"

let save ~path (s : t) : unit =
  let oc = open_out path in
  output_string oc (Yojson.Safe.pretty_to_string (to_json s));
  close_out oc

let load ~path : t option =
  if not (Sys.file_exists path) then None
  else
    try
      let ic = open_in path in
      let n = in_channel_length ic in
      let body = really_input_string ic n in
      close_in ic;
      Some (of_json (Yojson.Safe.from_string body))
    with _ -> None

(** Append the user's new input to the session, taking pending state into
    account: if there's a pending ask_user tool_use, the input becomes its
    answer (Tool_result block); otherwise it's a fresh user text turn. *)
let append_input (s : t) (input : string) : t =
  let new_user_msg =
    match s.pending_tool_use_id with
    | Some id ->
        {
          role = User;
          content =
            [ Tool_result { tool_use_id = id; content = input; is_error = false } ];
        }
    | None -> { role = User; content = [ Text input ] }
  in
  {
    s with
    messages = s.messages @ [ new_user_msg ];
    pending_tool_use_id = None;
  }

(** Update session state after running the agent: stash the resulting
    message history and pending state. *)
let update_after_run (s : t) (outcome : session_result) : t =
  match outcome with
  | Outcome_done { final_messages; _ } ->
      { s with messages = final_messages; pending_tool_use_id = None }
  | Outcome_waiting { messages; tool_use_id; _ } ->
      {
        s with
        messages;
        pending_tool_use_id = Some tool_use_id;
      }
  | Outcome_failed { messages; _ } ->
      (* Keep messages so user can retry from the failure point. *)
      { s with messages; pending_tool_use_id = None }
