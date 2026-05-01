(** Anthropic SSE event-stream parser.

    Anthropic's streaming protocol emits a sequence of JSON events
    ([message_start], [content_block_start], [content_block_delta], etc.)
    that incrementally describe content blocks (text + tool_use). This
    module accumulates partial blocks in a [Hashtbl] keyed by SSE block
    index, then [finalize_blocks] flattens them into [content_block list]. *)

open Types

(** Builder for one in-progress content block keyed by SSE block index. *)
type block_builder =
  | Build_text of Buffer.t
  | Build_tool of {
      id : string;
      name : string;
      input_buf : Buffer.t;
    }
  | Build_other  (** unknown block type, ignored *)

(** Process one SSE event JSON. Mutates [blocks], [stop_reason], [usage]. *)
let handle_event ~blocks ~stop_reason ~(usage : usage ref) ~on_text_delta
    json =
  match json with
  | `Assoc fields -> (
      let typ = List.assoc_opt "type" fields in
      match typ with
      | Some (`String "message_start") -> (
          match List.assoc_opt "message" fields with
          | Some (`Assoc m) -> (
              match List.assoc_opt "usage" m with
              | Some j ->
                  let u : usage = usage_of_json j in
                  usage :=
                    {
                      !usage with
                      input_tokens = u.input_tokens;
                      cache_creation_input_tokens =
                        u.cache_creation_input_tokens;
                      cache_read_input_tokens = u.cache_read_input_tokens;
                    }
              | _ -> ())
          | _ -> ())
      | Some (`String "content_block_start") -> (
          let index =
            match List.assoc_opt "index" fields with
            | Some (`Int n) -> n
            | _ -> -1
          in
          let block = List.assoc_opt "content_block" fields in
          match block with
          | Some (`Assoc b) -> (
              match List.assoc_opt "type" b with
              | Some (`String "text") ->
                  Hashtbl.replace blocks index
                    (Build_text (Buffer.create 64))
              | Some (`String "tool_use") ->
                  let id =
                    match List.assoc_opt "id" b with
                    | Some (`String s) -> s
                    | _ -> ""
                  in
                  let name =
                    match List.assoc_opt "name" b with
                    | Some (`String s) -> s
                    | _ -> ""
                  in
                  Hashtbl.replace blocks index
                    (Build_tool
                       { id; name; input_buf = Buffer.create 64 })
              | _ -> Hashtbl.replace blocks index Build_other)
          | _ -> ())
      | Some (`String "content_block_delta") -> (
          let index =
            match List.assoc_opt "index" fields with
            | Some (`Int n) -> n
            | _ -> -1
          in
          let delta = List.assoc_opt "delta" fields in
          match delta with
          | Some (`Assoc d) -> (
              match List.assoc_opt "type" d with
              | Some (`String "text_delta") -> (
                  let text =
                    match List.assoc_opt "text" d with
                    | Some (`String s) -> s
                    | _ -> ""
                  in
                  on_text_delta text;
                  match Hashtbl.find_opt blocks index with
                  | Some (Build_text buf) -> Buffer.add_string buf text
                  | _ -> ())
              | Some (`String "input_json_delta") -> (
                  let partial =
                    match List.assoc_opt "partial_json" d with
                    | Some (`String s) -> s
                    | _ -> ""
                  in
                  match Hashtbl.find_opt blocks index with
                  | Some (Build_tool b) ->
                      Buffer.add_string b.input_buf partial
                  | _ -> ())
              | _ -> ())
          | _ -> ())
      | Some (`String "content_block_stop") -> ()
      | Some (`String "message_delta") -> (
          (match List.assoc_opt "delta" fields with
          | Some (`Assoc d) -> (
              match List.assoc_opt "stop_reason" d with
              | Some (`String s) -> stop_reason := stop_reason_of_string s
              | _ -> ())
          | _ -> ());
          match List.assoc_opt "usage" fields with
          | Some (`Assoc u) -> (
              match List.assoc_opt "output_tokens" u with
              | Some (`Int n) -> usage := { !usage with output_tokens = n }
              | _ -> ())
          | _ -> ())
      | Some (`String "message_stop") -> ()
      | Some (`String "ping") -> ()
      | Some (`String "error") ->
          let msg = Yojson.Safe.to_string json in
          failwith (Printf.sprintf "stream error event: %s" msg)
      | _ -> ())
  | _ -> ()

(** Finalize block builders into a sorted [content_block list]. *)
let finalize_blocks blocks : content_block list =
  Hashtbl.fold (fun idx b acc -> (idx, b) :: acc) blocks []
  |> List.sort (fun (a, _) (b, _) -> compare a b)
  |> List.filter_map (fun (_, b) ->
         match b with
         | Build_text buf -> Some (Text (Buffer.contents buf))
         | Build_tool { id; name; input_buf } ->
             let raw = Buffer.contents input_buf in
             let input =
               match raw with
               | "" -> `Assoc []
               | s -> (
                   try Yojson.Safe.from_string s
                   with Yojson.Json_error _ -> `String s)
             in
             Some (Tool_use { id; name; input })
         | Build_other -> None)
