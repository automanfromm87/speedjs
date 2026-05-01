(** Anthropic Messages API client.

    Defaults talk directly to api.anthropic.com using an API key from the
    [ANTHROPIC_API_KEY] environment variable. To route through a corporate
    proxy or alternate gateway, set [AGENT_LLM_BASE_URL] and/or
    [AGENT_LLM_PROXY] (see the env-var docs in [Args]).

    This module is the top-level wiring; the heavy lifting lives in:
    - [Http]          — curl spawn / read / status parsing
    - [Sse]           — streaming event parser
    - [Anthropic_req] — request body / response shape

    Two transport modes:
    - [complete]        : non-streaming, simpler
    - [complete_stream] : SSE streaming, fires [on_text_delta] as text arrives,
                          assembles the same final [llm_response] *)

open Types

let default_base_url = "https://api.anthropic.com"
let default_proxy = ""
let default_model = "claude-sonnet-4-5-20250929"
let api_version = "2023-06-01"

(** Boundary validation: any malformed message list (dangling tool_use,
    consecutive same-role turns, etc.) gets caught here and raised as
    [Bad_request] (non-retryable, fail-fast) BEFORE we waste a network
    round-trip and watch Anthropic return a misleading 500.

    Safety net for code paths that build messages via raw [list @ [...]]
    operations rather than [Conversation] smart constructors. *)
let validate_messages_or_fail messages =
  match Conversation.validate messages with
  | Ok () -> ()
  | Error msg ->
      raise
        (Llm_error.Llm_api_error
           (Llm_error.Bad_request
              (Printf.sprintf
                 "speedjs: malformed messages list (would be rejected by \
                  Anthropic): %s"
                 msg)))

let build_headers ~api_key ~extra =
  [
    ("content-type", "application/json");
    ("x-api-key", api_key);
    ("anthropic-version", api_version);
  ]
  @ extra
  @ Anthropic_req.custom_headers ()

let waitpid_exit pid =
  let _, wait_status = Unix.waitpid [] pid in
  match wait_status with
  | Unix.WEXITED n -> n
  | Unix.WSIGNALED n -> 128 + n
  | Unix.WSTOPPED n -> 128 + n

let raise_curl_failure ~prefix ~code ~stderr =
  raise
    (Llm_error.Llm_api_error
       (Llm_error.classify_transport
          ~message:(Printf.sprintf "%s exited %d. stderr: %s" prefix code stderr)))

let complete
    ?(base_url = Http.getenv_default "AGENT_LLM_BASE_URL" default_base_url)
    ?(proxy = Http.getenv_default "AGENT_LLM_PROXY" default_proxy)
    ?(api_key = Http.getenv_default "ANTHROPIC_API_KEY" "")
    ?(model = default_model) ?(max_tokens = 32768)
    ?(tool_choice = Tc_auto) ~system ~messages ~tools () : llm_response =
  validate_messages_or_fail messages;
  let body_json =
    Anthropic_req.build_request_body ~model ~system ~messages ~tools
      ~tool_choice ~max_tokens ~stream:false
  in
  if Sys.getenv_opt "SPEEDJS_DEBUG_REQUEST" <> None then
    Printf.eprintf "[debug-request] %s\n%!"
      (Yojson.Safe.pretty_to_string body_json);
  let body = Yojson.Safe.to_string body_json in
  let url = base_url ^ "/v1/messages" in
  let headers = build_headers ~api_key ~extra:[] in
  let pid, stdout_r, stderr_r =
    Http.spawn_curl ~proxy ~url ~headers ~body ~stream:false ()
  in
  let out = Http.read_all stdout_r in
  let err = Http.read_all stderr_r in
  Unix.close stdout_r;
  Unix.close stderr_r;
  let code = waitpid_exit pid in
  if code <> 0 then raise_curl_failure ~prefix:"curl" ~code ~stderr:err;
  let status, resp_headers, resp_body = Http.split_http_message out in
  Http.raise_for_status ~status ~headers:resp_headers ~body:resp_body;
  let json =
    try Yojson.Safe.from_string resp_body
    with Yojson.Json_error msg ->
      raise
        (Llm_error.Llm_api_error
           (Llm_error.classify_transport
              ~message:
                (Printf.sprintf "Failed to parse response JSON: %s\nbody: %s"
                   msg resp_body)))
  in
  Anthropic_req.parse_response json

let complete_stream
    ?(base_url = Http.getenv_default "AGENT_LLM_BASE_URL" default_base_url)
    ?(proxy = Http.getenv_default "AGENT_LLM_PROXY" default_proxy)
    ?(api_key = Http.getenv_default "ANTHROPIC_API_KEY" "")
    ?(model = default_model) ?(max_tokens = 32768)
    ?(on_text_delta = fun _ -> ()) ?(tool_choice = Tc_auto) ~system
    ~messages ~tools () : llm_response =
  validate_messages_or_fail messages;
  let body_json =
    Anthropic_req.build_request_body ~model ~system ~messages ~tools
      ~tool_choice ~max_tokens ~stream:true
  in
  if Sys.getenv_opt "SPEEDJS_DEBUG_REQUEST" <> None then
    Printf.eprintf "[debug-request] %s\n%!"
      (Yojson.Safe.pretty_to_string body_json);
  let body = Yojson.Safe.to_string body_json in
  let url = base_url ^ "/v1/messages" in
  let headers =
    build_headers ~api_key ~extra:[ ("accept", "text/event-stream") ]
  in
  let pid, stdout_r, stderr_r =
    Http.spawn_curl ~proxy ~url ~headers ~body ~stream:true ()
  in
  let blocks = Hashtbl.create 4 in
  let stop_reason = ref (Other "missing") in
  let usage : usage ref =
    ref
      {
        input_tokens = 0;
        output_tokens = 0;
        cache_creation_input_tokens = 0;
        cache_read_input_tokens = 0;
      }
  in
  (* Three-layer timeout: curl's [--max-time] caps the TOTAL transfer
     (set by [spawn_curl] above); [first_byte_sec] bounds the wait for
     the first byte; [idle_sec] bounds gaps between bytes after that.
     The first two complement [--max-time] by detecting CLOSE_WAIT-style
     stalls EARLIER than the total cap. *)
  let reader =
    Http.make_line_reader ~first_byte_sec:30.0 ~idle_sec:60.0 stdout_r
  in
  let stream_failed = ref None in
  (* curl with -i + a proxy may emit a CONNECT response first
     (e.g. "HTTP/1.1 200 Connection established"). We loop, taking the
     LAST status line as the real response. *)
  let status = ref None in
  let response_headers = ref [] in
  let response_body_buffer = Buffer.create 1024 in
  let in_body = ref false in
  let in_headers = ref true in
  let kill_curl_silently () =
    try Unix.kill pid Sys.sigkill with _ -> ()
  in
  (try
     while true do
       let line = Http.read_line_with_deadlines reader in
       if !in_headers then begin
         let trimmed = Http.normalize_header_line line in
         if trimmed = "" then begin
           in_headers := false;
           in_body := true
         end
         else if String.length trimmed >= 4 && String.sub trimmed 0 4 = "HTTP"
         then begin
           status := Http.parse_status_line trimmed;
           response_headers := []
         end
         else
           match Http.parse_header_line trimmed with
           | Some kv -> response_headers := kv :: !response_headers
           | None -> ()
       end
       else
         match !status with
         | Some s when s >= 200 && s < 300 ->
             if String.length line >= 6 && String.sub line 0 6 = "data: " then begin
               let payload = String.sub line 6 (String.length line - 6) in
               match
                 try Some (Yojson.Safe.from_string payload)
                 with Yojson.Json_error _ -> None
               with
               | Some j ->
                   Sse.handle_event ~blocks ~stop_reason ~usage
                     ~on_text_delta j
               | None -> ()
             end
             else if Http.normalize_header_line line = "" then ()
         | _ ->
             Buffer.add_string response_body_buffer line;
             Buffer.add_char response_body_buffer '\n'
     done
   with
  | End_of_file -> ()
  | Http.Stream_first_byte_timeout sec ->
      kill_curl_silently ();
      stream_failed :=
        Some (Printf.sprintf "no response from server within %.0fs" sec)
  | Http.Stream_idle_timeout sec ->
      kill_curl_silently ();
      stream_failed :=
        Some
          (Printf.sprintf
             "stream went silent for %.0fs (likely CLOSE_WAIT-style wedge)"
             sec)
  | e -> stream_failed := Some (Printexc.to_string e));
  Unix.close stdout_r;
  let err = Http.read_all stderr_r in
  Unix.close stderr_r;
  let code = waitpid_exit pid in
  if code <> 0 then
    raise_curl_failure ~prefix:"curl streaming" ~code ~stderr:err;
  Http.raise_for_status ~status:!status ~headers:!response_headers
    ~body:(Buffer.contents response_body_buffer);
  (match !stream_failed with
  | Some msg ->
      raise
        (Llm_error.Llm_api_error
           (Llm_error.classify_transport
              ~message:("stream parse failed: " ^ msg)))
  | None -> ());
  let content = Sse.finalize_blocks blocks in
  { content; stop_reason = !stop_reason; usage = !usage }
