(** Minimal MCP (Model Context Protocol) client.

    Spawns an MCP server as a subprocess, communicates via JSON-RPC 2.0
    over stdin/stdout (newline-delimited JSON). Currently supports:

    - [initialize] handshake
    - [tools/list] — pull tool definitions
    - [tools/call] — invoke a tool, get text content back
    - [shutdown] — close cleanly

    Skips: notifications, prompts, resources, sampling, image / resource
    content blocks (only text is parsed from tool results). *)

type t = {
  pid : int;
  stdin : Unix.file_descr;
  stdout : Unix.file_descr;
  stderr : Unix.file_descr;
  mutable next_id : int;
  (* For diagnostic logging only — populated after [initialize]. *)
  mutable server_name : string;
  mutable server_version : string;
  (* Serialize concurrent [tools/call] requests through this connection.
     The MCP server is a single subprocess sharing one stdio pair —
     parallel reads/writes interleave and corrupt JSON-RPC framing. *)
  mutex : Mutex.t;
  (* Read timeout for stdout. A wedged MCP server (zombie subprocess /
     deadlock / forgot to flush) would otherwise hang [Unix.read]
     indefinitely and take down the whole agent run. Default 30s; set
     per-connection. *)
  read_timeout_sec : float;
}

let protocol_version = "2024-11-05"

(** Spawn the server. [cmd] is the executable name (looked up in PATH);
    [args] is the argv tail. [read_timeout_sec] caps any single
    stdout read; default 30s. *)
let start ?(read_timeout_sec = 30.0) ~cmd ~args () : t =
  let stdin_r, stdin_w = Unix.pipe () in
  let stdout_r, stdout_w = Unix.pipe () in
  let stderr_r, stderr_w = Unix.pipe () in
  Unix.set_close_on_exec stdin_w;
  Unix.set_close_on_exec stdout_r;
  Unix.set_close_on_exec stderr_r;
  let pid =
    Unix.create_process cmd
      (Array.of_list (cmd :: args))
      stdin_r stdout_w stderr_w
  in
  Unix.close stdin_r;
  Unix.close stdout_w;
  Unix.close stderr_w;
  {
    pid;
    stdin = stdin_w;
    stdout = stdout_r;
    stderr = stderr_r;
    next_id = 1;
    server_name = "(uninitialized)";
    server_version = "?";
    mutex = Mutex.create ();
    read_timeout_sec;
  }

(** Write a JSON value as one newline-terminated line to the server. *)
let send_json (conn : t) (json : Yojson.Safe.t) : unit =
  let line = Yojson.Safe.to_string json ^ "\n" in
  let written = Unix.write_substring conn.stdin line 0 (String.length line) in
  if written < String.length line then
    failwith "Mcp.send_json: short write"

(** Reading hit the per-line timeout. *)
exception Read_timeout of float

(** Read one newline-terminated line from the server with a wall-clock
    cap (per-connection [read_timeout_sec]). Returns [None] on EOF.
    Raises [Read_timeout] if the cap fires before either a newline or
    EOF is observed. Uses [Unix.select] so a wedged server can't block
    us indefinitely. *)
let read_line (conn : t) : string option =
  let buf = Buffer.create 256 in
  let byte = Bytes.create 1 in
  let deadline = Unix.gettimeofday () +. conn.read_timeout_sec in
  let rec loop () =
    let now = Unix.gettimeofday () in
    let remaining = deadline -. now in
    if remaining <= 0.0 then raise (Read_timeout conn.read_timeout_sec)
    else
      let r, _, _ = Unix.select [ conn.stdout ] [] [] remaining in
      match r with
      | [] -> raise (Read_timeout conn.read_timeout_sec)
      | _ -> (
          match Unix.read conn.stdout byte 0 1 with
          | 0 ->
              if Buffer.length buf = 0 then None
              else Some (Buffer.contents buf)
          | _ ->
              let c = Bytes.get byte 0 in
              if c = '\n' then Some (Buffer.contents buf)
              else begin
                Buffer.add_char buf c;
                loop ()
              end
          | exception Unix.Unix_error _ ->
              if Buffer.length buf = 0 then None
              else Some (Buffer.contents buf))
  in
  loop ()

(** Send a request, await the matching response (by id), ignoring any
    notifications that arrive in the meantime. Returns the [result] field
    on success, or an [Error msg] for JSON-RPC errors.

    Acquires [conn.mutex] for the entire send/recv exchange. This is
    important because MCP tools may be invoked from worker threads (via
    [Tool_calls_batch]) — without serialization, two threads could
    interleave their writes / reads on the shared stdio pipes and corrupt
    the JSON-RPC framing.

    Diagnostics go via [Printf.eprintf] (NOT [Effect.perform Log]),
    because we may be running in a worker thread where effect handlers
    aren't available. *)
let request (conn : t) ~method_ ~params : (Yojson.Safe.t, string) Result.t =
  Mutex.lock conn.mutex;
  let id = conn.next_id in
  conn.next_id <- conn.next_id + 1;
  let req =
    `Assoc
      [
        ("jsonrpc", `String "2.0");
        ("id", `Int id);
        ("method", `String method_);
        ("params", params);
      ]
  in
  let result =
    try
      send_json conn req;
      let rec await () =
        match read_line conn with
        | exception Read_timeout sec ->
            Error
              (Printf.sprintf
                 "Mcp.request: server '%s' did not respond within %.1fs (read timeout)"
                 conn.server_name sec)
        | None -> Error "Mcp.request: server closed connection unexpectedly"
        | Some line ->
            let json =
              try Some (Yojson.Safe.from_string line)
              with Yojson.Json_error msg ->
                Printf.eprintf "[mcp] bad JSON line: %s — %s\n%!" line msg;
                None
            in
            (match json with
            | None -> await ()
            | Some (`Assoc fields) -> (
                match List.assoc_opt "id" fields with
                | Some (`Int n) when n = id -> (
                    match List.assoc_opt "error" fields with
                    | Some err -> Error (Yojson.Safe.to_string err)
                    | None -> (
                        match List.assoc_opt "result" fields with
                        | Some r -> Ok r
                        | None -> Error "Mcp.request: no result or error field"))
                | _ -> await () (* different id or notification *))
            | _ -> await ())
      in
      await ()
    with e ->
      Mutex.unlock conn.mutex;
      raise e
  in
  Mutex.unlock conn.mutex;
  result

(** Send a notification (no id, no response expected). *)
let notify (conn : t) ~method_ ~params : unit =
  let n =
    `Assoc
      [
        ("jsonrpc", `String "2.0");
        ("method", `String method_);
        ("params", params);
      ]
  in
  send_json conn n

(** Initialize handshake — send [initialize] request + [initialized]
    notification. Returns server name/version on success. *)
let initialize (conn : t) : (unit, string) Result.t =
  let params =
    `Assoc
      [
        ("protocolVersion", `String protocol_version);
        ("capabilities", `Assoc []);
        ( "clientInfo",
          `Assoc
            [ ("name", `String "speedjs"); ("version", `String "0.1.0") ] );
      ]
  in
  match request conn ~method_:"initialize" ~params with
  | Error e -> Error e
  | Ok result ->
      (match result with
      | `Assoc fields -> (
          match List.assoc_opt "serverInfo" fields with
          | Some (`Assoc info) ->
              let n =
                match List.assoc_opt "name" info with
                | Some (`String s) -> s
                | _ -> "?"
              in
              let v =
                match List.assoc_opt "version" info with
                | Some (`String s) -> s
                | _ -> "?"
              in
              conn.server_name <- n;
              conn.server_version <- v
          | _ -> ())
      | _ -> ());
      notify conn ~method_:"notifications/initialized" ~params:(`Assoc []);
      Ok ()

(** [tools/list] — fetch the JSON list of tool descriptors from the server.
    Each item has [name], [description], [inputSchema]. *)
let list_tools (conn : t) : (Yojson.Safe.t list, string) Result.t =
  match request conn ~method_:"tools/list" ~params:(`Assoc []) with
  | Error e -> Error e
  | Ok (`Assoc fields) -> (
      match List.assoc_opt "tools" fields with
      | Some (`List items) -> Ok items
      | _ -> Error "tools/list: missing 'tools' array")
  | Ok _ -> Error "tools/list: response not object"

(** Extract text content from an MCP tool_result. The result has shape:
    {[ {"content": [{"type": "text", "text": "..."}, ...], "isError": bool} ]}
    Returns concatenated text or an error message. *)
let extract_text_content (json : Yojson.Safe.t) : (string, string) Result.t =
  match json with
  | `Assoc fields ->
      let is_error =
        match List.assoc_opt "isError" fields with
        | Some (`Bool b) -> b
        | _ -> false
      in
      let texts =
        match List.assoc_opt "content" fields with
        | Some (`List items) ->
            List.filter_map
              (function
                | `Assoc fs -> (
                    match List.assoc_opt "type" fs with
                    | Some (`String "text") -> (
                        match List.assoc_opt "text" fs with
                        | Some (`String s) -> Some s
                        | _ -> None)
                    | _ -> None)
                | _ -> None)
              items
        | _ -> []
      in
      let combined = String.concat "\n" texts in
      if is_error then
        Error (if combined = "" then "(MCP tool reported error)" else combined)
      else Ok combined
  | _ -> Error "tools/call: response not object"

(** [tools/call] — invoke a tool by name with JSON arguments. *)
let call_tool (conn : t) ~name ~(arguments : Yojson.Safe.t) :
    (string, string) Result.t =
  let params =
    `Assoc [ ("name", `String name); ("arguments", arguments) ]
  in
  match request conn ~method_:"tools/call" ~params with
  | Error e -> Error e
  | Ok r -> extract_text_content r

(** Convert one MCP tool descriptor (JSON) to a [Types.tool_def] whose
    handler dispatches to the live MCP server connection. *)
let to_tool_def (conn : t) (json : Yojson.Safe.t) : Types.tool_def =
  match json with
  | `Assoc fields ->
      let name =
        match List.assoc_opt "name" fields with
        | Some (`String s) -> s
        | _ -> failwith "Mcp.to_tool_def: missing tool name"
      in
      let description =
        match List.assoc_opt "description" fields with
        | Some (`String s) -> s
        | _ -> Printf.sprintf "MCP tool '%s' (%s)" name conn.server_name
      in
      let input_schema =
        match List.assoc_opt "inputSchema" fields with
        | Some j -> j
        | None ->
            `Assoc
              [
                ("type", `String "object");
                ("properties", `Assoc []);
              ]
      in
      let handler input = call_tool conn ~name ~arguments:input in
      (* MCP tools come from external servers — we can't know their
         semantics, so default to conservative metadata. *)
      Types.
        {
          name;
          description;
          input_schema;
          handler;
          idempotent = false;
          timeout_sec = Some 30.0;
          category = "mcp";
          (* MCP servers can do anything in principle. Conservative
             classification: assume Mutating + Network so they're
             gated to Executor / Subagent. Operators can override
             at the connector layer if they know the server is
             read-only. *)
          capabilities = [ Mutating; Network ];
          allowed_modes = [ Executor; Subagent ];
          classify_error = Types.default_classify_error;
        }
  | _ -> failwith "Mcp.to_tool_def: tool must be JSON object"

(** Spawn server, initialize, fetch tools, return ready-to-use tool_defs.
    Caller is responsible for [shutdown] when done. *)
let connect_and_load_tools ?(read_timeout_sec = 30.0) ~cmd ~args () :
    (t * Types.tool_def list, string) Result.t =
  let conn = start ~read_timeout_sec ~cmd ~args () in
  let ( let* ) = Result.bind in
  let* () = initialize conn in
  let* tools_json = list_tools conn in
  let defs = List.map (to_tool_def conn) tools_json in
  Ok (conn, defs)

let shutdown (conn : t) : unit =
  (try Unix.close conn.stdin with _ -> ());
  (try Unix.close conn.stdout with _ -> ());
  (try Unix.close conn.stderr with _ -> ());
  let _ =
    try Unix.waitpid [ Unix.WNOHANG ] conn.pid
    with _ -> (0, Unix.WEXITED 0)
  in
  ()
