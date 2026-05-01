(** HTTP transport via curl subprocess.

    Spawns curl as a subprocess, feeds the request body via stdin, returns
    file descriptors for stdout (response headers + body) and stderr. The
    caller is responsible for reading + cleaning up.

    Used by both non-streaming and streaming paths in [Anthropic]; this
    module knows nothing about Anthropic-specific shapes — only HTTP. *)

(** Read [name] from the environment; fall back to [default] if unset or
    empty. *)
let getenv_default name default =
  match Sys.getenv_opt name with
  | Some v when v <> "" -> v
  | _ -> default

(* ===== Line reader with first-byte + idle timeouts =====

   Used by SSE streaming: regular [input_line] blocks indefinitely on a
   wedged socket (CLOSE_WAIT in production). [Unix.select] gives us a
   wake-up budget so a stuck transport can't hang the agent. *)

exception Stream_first_byte_timeout of float
exception Stream_idle_timeout of float

type line_reader = {
  fd : Unix.file_descr;
  buf : Buffer.t;
  byte : Bytes.t;
  mutable last_byte_at : float;
  mutable got_first_byte : bool;
  start_time : float;
  first_byte_sec : float;
  idle_sec : float;
}

let make_line_reader ?(first_byte_sec = 30.0) ?(idle_sec = 60.0) fd =
  let now = Unix.gettimeofday () in
  {
    fd;
    buf = Buffer.create 256;
    byte = Bytes.create 1;
    last_byte_at = now;
    got_first_byte = false;
    start_time = now;
    first_byte_sec;
    idle_sec;
  }

(** Read one line (sans trailing [\n]). Raises [End_of_file] on EOF
    (mimicking [input_line]), [Stream_first_byte_timeout] if no byte
    has arrived within [first_byte_sec], or [Stream_idle_timeout] if
    bytes started arriving but went silent for [idle_sec]. *)
let read_line_with_deadlines (r : line_reader) : string =
  Buffer.clear r.buf;
  let rec loop () =
    let now = Unix.gettimeofday () in
    let deadline =
      if r.got_first_byte then r.last_byte_at +. r.idle_sec
      else r.start_time +. r.first_byte_sec
    in
    let remaining = deadline -. now in
    if remaining <= 0.0 then
      if r.got_first_byte then raise (Stream_idle_timeout r.idle_sec)
      else raise (Stream_first_byte_timeout r.first_byte_sec)
    else
      let ready, _, _ = Unix.select [ r.fd ] [] [] remaining in
      match ready with
      | [] ->
          if r.got_first_byte then raise (Stream_idle_timeout r.idle_sec)
          else raise (Stream_first_byte_timeout r.first_byte_sec)
      | _ -> (
          match Unix.read r.fd r.byte 0 1 with
          | 0 ->
              if Buffer.length r.buf = 0 then raise End_of_file
              else Buffer.contents r.buf
          | _ ->
              r.got_first_byte <- true;
              r.last_byte_at <- Unix.gettimeofday ();
              let c = Bytes.get r.byte 0 in
              if c = '\n' then Buffer.contents r.buf
              else begin
                Buffer.add_char r.buf c;
                loop ()
              end
          | exception Unix.Unix_error _ ->
              if Buffer.length r.buf = 0 then raise End_of_file
              else Buffer.contents r.buf)
  in
  loop ()

(** Read all bytes from a file descriptor until EOF or error. *)
let read_all fd =
  let buf = Buffer.create 8192 in
  let chunk = Bytes.create 4096 in
  let rec loop () =
    match Unix.read fd chunk 0 (Bytes.length chunk) with
    | 0 -> ()
    | n ->
        Buffer.add_subbytes buf chunk 0 n;
        loop ()
    | exception Unix.Unix_error _ -> ()
  in
  loop ();
  Buffer.contents buf

(** Strip CR and trim whitespace from a header line. *)
let normalize_header_line line =
  let l =
    if String.length line > 0 && line.[String.length line - 1] = '\r' then
      String.sub line 0 (String.length line - 1)
    else line
  in
  String.trim l

(** Parse status from an HTTP/1.x or HTTP/2 status line.
    e.g. "HTTP/1.1 429 Too Many Requests" → 429. *)
let parse_status_line line =
  let line = normalize_header_line line in
  match String.split_on_char ' ' line with
  | _ :: code :: _ -> ( try Some (int_of_string code) with _ -> None)
  | _ -> None

let parse_header_line line : (string * string) option =
  let line = normalize_header_line line in
  match String.index_opt line ':' with
  | Some idx when idx > 0 ->
      let k = String.lowercase_ascii (String.trim (String.sub line 0 idx)) in
      let v =
        String.trim
          (String.sub line (idx + 1) (String.length line - idx - 1))
      in
      Some (k, v)
  | _ -> None

(** Split an HTTP message [headers + blank line + body] from a string.
    Returns (status_line, headers, body). curl with [-i] may emit multiple
    HTTP responses (e.g. proxy CONNECT responses) — we keep only the LAST. *)
let split_http_message (raw : string) =
  let lines = String.split_on_char '\n' raw in
  let rec find_last_http_block before lines =
    match lines with
    | [] -> List.rev before
    | l :: rest when String.length l >= 4 && String.sub l 0 4 = "HTTP" ->
        find_last_http_block [ l ] rest
    | l :: rest -> find_last_http_block (l :: before) rest
  in
  let final_block_lines = find_last_http_block [] lines in
  match final_block_lines with
  | [] -> (None, [], raw)
  | status_line :: rest ->
      let rec split_at_blank acc = function
        | [] -> (List.rev acc, [])
        | l :: rest when normalize_header_line l = "" -> (List.rev acc, rest)
        | l :: rest -> split_at_blank (l :: acc) rest
      in
      let header_lines, body_lines = split_at_blank [] rest in
      let status = parse_status_line status_line in
      let headers = List.filter_map parse_header_line header_lines in
      let body = String.concat "\n" body_lines in
      (status, headers, body)

(** Parse Retry-After header to seconds. Supports integer seconds; we
    treat HTTP-date format as not-parseable for now. *)
let retry_after_seconds (headers : (string * string) list) : float option =
  match List.assoc_opt "retry-after" headers with
  | None -> None
  | Some v -> ( try Some (float_of_string v) with _ -> None)

(** Raise typed [Llm_error.Llm_api_error] for non-2xx responses. *)
let raise_for_status ~status ~headers ~body =
  match status with
  | Some s when s >= 200 && s < 300 -> ()
  | Some s ->
      let retry_after = retry_after_seconds headers in
      raise
        (Llm_error.Llm_api_error
           (Llm_error.classify_status ~status:s ~body ~retry_after))
  | None ->
      raise
        (Llm_error.Llm_api_error
           (Llm_error.classify_transport
              ~message:("could not parse HTTP status; body: " ^ body)))

(** Spawn curl with given args, body fed via stdin. Returns (pid,
    stdout_r, stderr_r). Caller must read stdout/stderr and waitpid.

    Uses [-i] to include response headers in stdout, so the caller can
    parse the HTTP status code and headers (esp. Retry-After) for typed
    error classification.

    [max_time_sec] caps the entire transfer (curl's [--max-time]). Without
    this a wedged mid-stream socket (CLOSE_WAIT) would hang indefinitely.
    Default 120s for non-stream, 600s for stream — tune via the parameter
    when calling sites need different windows. *)
let spawn_curl ?max_time_sec ~proxy ~url ~headers ~body ~stream () =
  let default_max = if stream then 600.0 else 120.0 in
  let max_t = Option.value max_time_sec ~default:default_max in
  let header_args =
    List.concat_map
      (fun (k, v) -> [ "-H"; Printf.sprintf "%s: %s" k v ])
      headers
  in
  let stream_args = if stream then [ "--no-buffer" ] else [] in
  let timeout_args = [ "--max-time"; Printf.sprintf "%.0f" max_t ] in
  let proxy_args = if proxy = "" then [] else [ "-x"; "http://" ^ proxy ] in
  let args =
    [ "curl"; "-sS"; "-i" ] @ stream_args @ timeout_args @ proxy_args
    @ [ "-X"; "POST"; url ]
    @ header_args
    @ [ "--data-binary"; "@-" ]
  in
  let stdin_r, stdin_w = Unix.pipe () in
  let stdout_r, stdout_w = Unix.pipe () in
  let stderr_r, stderr_w = Unix.pipe () in
  Unix.set_close_on_exec stdin_w;
  Unix.set_close_on_exec stdout_r;
  Unix.set_close_on_exec stderr_r;
  let pid =
    Unix.create_process "curl" (Array.of_list args) stdin_r stdout_w
      stderr_w
  in
  Unix.close stdin_r;
  Unix.close stdout_w;
  Unix.close stderr_w;
  let _ = Unix.write_substring stdin_w body 0 (String.length body) in
  Unix.close stdin_w;
  (pid, stdout_r, stderr_r)
