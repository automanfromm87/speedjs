(** Anthropic Messages API request/response shape.

    Builds the request body (with cache_control + context_management) and
    parses the response JSON. Knows the API contract; doesn't know HTTP. *)

open Types

(** Parse [ANTHROPIC_CUSTOM_HEADERS] env var, format
    [Key: Value\nKey: Value]. *)
let parse_custom_headers raw =
  String.split_on_char '\n' raw
  |> List.filter_map (fun line ->
         match String.index_opt line ':' with
         | Some idx when idx > 0 ->
             let k = String.trim (String.sub line 0 idx) in
             let v =
               String.trim
                 (String.sub line (idx + 1) (String.length line - idx - 1))
             in
             if k <> "" then Some (k, v) else None
         | _ -> None)

let custom_headers () =
  match Sys.getenv_opt "ANTHROPIC_CUSTOM_HEADERS" with
  | Some raw when raw <> "" -> parse_custom_headers raw
  | _ -> []

(** Mark a message JSON's last content block with
    [cache_control: ephemeral]. This is the third cache breakpoint (after
    system + tools), giving us a 3-tier sliding-window cache: turn N
    writes the cache up through the last block; turn N+1 hits it on
    history that hasn't moved and writes a new breakpoint on its own
    last block. *)
let mark_last_message_cacheable (messages : Yojson.Safe.t list) :
    Yojson.Safe.t list =
  match List.rev messages with
  | [] -> []
  | last :: rest_rev -> (
      let mark_block = function
        | `Assoc fields ->
            let already_marked = List.mem_assoc "cache_control" fields in
            if already_marked then `Assoc fields
            else
              `Assoc
                (fields
                @ [
                    ( "cache_control",
                      `Assoc [ ("type", `String "ephemeral") ] );
                  ])
        | other -> other
      in
      match last with
      | `Assoc fields -> (
          match List.assoc_opt "content" fields with
          | Some (`List blocks) when blocks <> [] ->
              let blocks_rev = List.rev blocks in
              let last_block = List.hd blocks_rev in
              let rest = List.rev (List.tl blocks_rev) in
              let new_blocks = rest @ [ mark_block last_block ] in
              let new_fields =
                List.map
                  (fun (k, v) ->
                    if k = "content" then ("content", `List new_blocks)
                    else (k, v))
                  fields
              in
              List.rev (`Assoc new_fields :: rest_rev)
          | _ -> messages)
      | _ -> messages)

(** System prompt as a single text block with [cache_control: ephemeral]. *)
let system_blocks system =
  match system with
  | Some s when s <> "" ->
      Some
        (`List
          [
            `Assoc
              [
                ("type", `String "text");
                ("text", `String s);
                ("cache_control", `Assoc [ ("type", `String "ephemeral") ]);
              ];
          ])
  | _ -> None

(** Mark the last entry in tools list with [cache_control: ephemeral]. *)
let cacheable_tools tools =
  match List.rev tools with
  | [] -> []
  | last :: rest_rev ->
      let with_cache =
        match Codec.tool_def_to_json last with
        | `Assoc fields ->
            `Assoc
              (fields
              @ [ ("cache_control", `Assoc [ ("type", `String "ephemeral") ]) ])
        | other -> other
      in
      List.rev_map Codec.tool_def_to_json rest_rev |> List.rev |> fun rest ->
      rest @ [ with_cache ]

(** Server-side context management: prune stale tool_uses when input
    grows. Triggers at >20k input tokens, keeps last 4 tool_uses, clears
    at least 3000 tokens worth. Conservative defaults that helix uses in
    production. *)
let context_management_block =
  `Assoc
    [
      ( "edits",
        `List
          [
            `Assoc
              [
                ("type", `String "clear_tool_uses_20250919");
                ( "trigger",
                  `Assoc
                    [
                      ("type", `String "input_tokens");
                      ("value", `Int 20_000);
                    ] );
                ( "keep",
                  `Assoc
                    [
                      ("type", `String "tool_uses");
                      ("value", `Int 4);
                    ] );
                ( "clear_at_least",
                  `Assoc
                    [
                      ("type", `String "input_tokens");
                      ("value", `Int 3_000);
                    ] );
              ];
          ] );
    ]

let build_request_body ~model ~system ~messages ~tools ~tool_choice
    ~max_tokens ~stream =
  let messages_json =
    List.map Codec.message_to_json messages |> mark_last_message_cacheable
  in
  let base =
    [
      ("model", `String model);
      ("max_tokens", `Int max_tokens);
      ("messages", `List messages_json);
    ]
  in
  let with_system =
    match system_blocks system with
    | Some blocks -> base @ [ ("system", blocks) ]
    | None -> base
  in
  let with_tools =
    match tools with
    | [] -> with_system
    | _ ->
        with_system
        @ [
            ("tools", `List (cacheable_tools tools));
            ("context_management", context_management_block);
          ]
  in
  let with_tool_choice =
    match tool_choice with
    | Tc_auto -> with_tools (* API default; omit to keep body small *)
    | tc -> with_tools @ [ ("tool_choice", Codec.tool_choice_to_json tc) ]
  in
  let with_stream =
    if stream then with_tool_choice @ [ ("stream", `Bool true) ]
    else with_tool_choice
  in
  `Assoc with_stream

let parse_response (json : Yojson.Safe.t) : llm_response =
  match json with
  | `Assoc fields ->
      let content =
        match List.assoc_opt "content" fields with
        | Some (`List items) -> List.map Codec.content_block_of_json items
        | _ -> failwith "response missing content"
      in
      let stop_reason =
        match List.assoc_opt "stop_reason" fields with
        | Some (`String s) -> Codec.stop_reason_of_string s
        | Some `Null -> Other "null"
        | _ -> Other "missing"
      in
      let usage =
        match List.assoc_opt "usage" fields with
        | Some j -> Codec.usage_of_json j
        | None ->
            {
              input_tokens = 0;
              output_tokens = 0;
              cache_creation_input_tokens = 0;
              cache_read_input_tokens = 0;
            }
      in
      { content; stop_reason; usage }
  | _ -> failwith "API response root must be object"
