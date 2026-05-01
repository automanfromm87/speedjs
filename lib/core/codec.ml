(** JSON ↔ Types serialization, factored out of [Types] to keep the
    type-definition file a small navigable index. The functions here
    deal exclusively with the Anthropic Messages API wire shape.

    Errors during decoding raise [Failure] — callers (e.g.
    [Anthropic_req.parse_response]) wrap into typed errors at their
    own boundary. *)

open Types

(* ===== role / stop_reason ===== *)

let role_to_string = function User -> "user" | Assistant -> "assistant"

let stop_reason_of_string = function
  | "end_turn" -> End_turn
  | "tool_use" -> Tool_use_stop
  | "max_tokens" -> Max_tokens
  | "stop_sequence" -> Stop_sequence
  | s -> Other s

let stop_reason_to_string = function
  | End_turn -> "end_turn"
  | Tool_use_stop -> "tool_use"
  | Max_tokens -> "max_tokens"
  | Stop_sequence -> "stop_sequence"
  | Other s -> s

(* ===== tool_choice ===== *)

let tool_choice_to_json = function
  | Tc_auto -> `Assoc [ ("type", `String "auto") ]
  | Tc_any -> `Assoc [ ("type", `String "any") ]
  | Tc_tool name ->
      `Assoc [ ("type", `String "tool"); ("name", `String name) ]
  | Tc_none -> `Assoc [ ("type", `String "none") ]

(* ===== content_block ===== *)

let content_block_to_json = function
  | Text s -> `Assoc [ ("type", `String "text"); ("text", `String s) ]
  | Tool_use { id; name; input } ->
      `Assoc
        [
          ("type", `String "tool_use");
          ("id", `String (Id.Tool_use_id.to_string id));
          ("name", `String name);
          ("input", input);
        ]
  | Tool_result { tool_use_id; content; is_error } ->
      `Assoc
        [
          ("type", `String "tool_result");
          ("tool_use_id", `String (Id.Tool_use_id.to_string tool_use_id));
          ("content", `String content);
          ("is_error", `Bool is_error);
        ]

let content_block_of_json = function
  | `Assoc fields as j -> (
      match List.assoc_opt "type" fields with
      | Some (`String "text") -> (
          match List.assoc_opt "text" fields with
          | Some (`String s) -> Text s
          | _ -> failwith "text block missing text field")
      | Some (`String "tool_use") ->
          let id =
            match List.assoc_opt "id" fields with
            | Some (`String s) -> Id.Tool_use_id.of_string s
            | _ -> failwith "tool_use missing id"
          in
          let name =
            match List.assoc_opt "name" fields with
            | Some (`String s) -> s
            | _ -> failwith "tool_use missing name"
          in
          let input =
            match List.assoc_opt "input" fields with
            | Some v -> v
            | None -> `Assoc []
          in
          Tool_use { id; name; input }
      | _ ->
          failwith
            (Printf.sprintf "unknown content block: %s"
               (Yojson.Safe.to_string j)))
  | j ->
      failwith
        (Printf.sprintf "content block must be object, got: %s"
           (Yojson.Safe.to_string j))

(* ===== message ===== *)

let message_to_json (m : message) =
  `Assoc
    [
      ("role", `String (role_to_string m.role));
      ("content", `List (List.map content_block_to_json m.content));
    ]

(* ===== usage ===== *)

let usage_to_json (u : usage) =
  `Assoc
    [
      ("input_tokens", `Int u.input_tokens);
      ("output_tokens", `Int u.output_tokens);
      ("cache_creation_input_tokens", `Int u.cache_creation_input_tokens);
      ("cache_read_input_tokens", `Int u.cache_read_input_tokens);
    ]

let usage_of_json : Yojson.Safe.t -> usage = function
  | `Assoc fields ->
      let int_field name =
        match List.assoc_opt name fields with Some (`Int n) -> n | _ -> 0
      in
      {
        input_tokens = int_field "input_tokens";
        output_tokens = int_field "output_tokens";
        cache_creation_input_tokens = int_field "cache_creation_input_tokens";
        cache_read_input_tokens = int_field "cache_read_input_tokens";
      }
  | _ ->
      {
        input_tokens = 0;
        output_tokens = 0;
        cache_creation_input_tokens = 0;
        cache_read_input_tokens = 0;
      }

(* ===== llm_response ===== *)

let llm_response_to_json (r : llm_response) =
  `Assoc
    [
      ("content", `List (List.map content_block_to_json r.content));
      ("stop_reason", `String (stop_reason_to_string r.stop_reason));
      ("usage", usage_to_json r.usage);
    ]

let llm_response_of_json = function
  | `Assoc fields ->
      let content =
        match List.assoc_opt "content" fields with
        | Some (`List items) -> List.map content_block_of_json items
        | _ -> []
      in
      let stop_reason =
        match List.assoc_opt "stop_reason" fields with
        | Some (`String s) -> stop_reason_of_string s
        | _ -> Other "missing"
      in
      let usage =
        match List.assoc_opt "usage" fields with
        | Some j -> usage_of_json j
        | None ->
            {
              input_tokens = 0;
              output_tokens = 0;
              cache_creation_input_tokens = 0;
              cache_read_input_tokens = 0;
            }
      in
      { content; stop_reason; usage }
  | _ -> failwith "llm_response_of_json: not an object"

(* ===== tool_def ===== *)

let tool_def_to_json (t : tool_def) =
  `Assoc
    [
      ("name", `String t.name);
      ("description", `String t.description);
      ("input_schema", t.input_schema);
    ]
