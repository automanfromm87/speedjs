(** Built-in tools that the agent can call.

    Each handler returns a [Types.tool_handler_result]: [Ok text] for
    successful observations, [Error msg] for failures. The agent surfaces
    errors back to the LLM with [is_error: true] so the model can recover. *)

open Types

(* ===== helpers ===== *)

let read_all_from_proc cmd =
  try
    let ic = Unix.open_process_in cmd in
    let buf = Buffer.create 4096 in
    (try
       while true do
         Buffer.add_channel buf ic 4096
       done
     with End_of_file -> ());
    let _ = Unix.close_process_in ic in
    Ok (Buffer.contents buf)
  with e -> Error (Printexc.to_string e)

let truncate_string ~max s =
  if String.length s > max then
    String.sub s 0 max
    ^ Printf.sprintf "\n[... truncated %d chars ...]" (String.length s - max)
  else s

let get_string_field name fields =
  match List.assoc_opt name fields with
  | Some (`String s) -> Ok s
  | Some _ -> Error (Printf.sprintf "field '%s' must be a string" name)
  | None -> Error (Printf.sprintf "missing required field '%s'" name)

let get_int_field name fields =
  match List.assoc_opt name fields with
  | Some (`Int n) -> Ok n
  | Some _ -> Error (Printf.sprintf "field '%s' must be an integer" name)
  | None -> Error (Printf.sprintf "missing required field '%s'" name)

let with_object_input input k =
  match input with
  | `Assoc fields -> k fields
  | _ -> Error "input must be a JSON object"

(** All file-touching tools require absolute paths. A relative path
    resolves against the OCaml process CWD which is almost never what the
    agent intends; silently writing to the wrong place is the worst
    failure mode. *)
let require_absolute_path ~field path =
  if path = "" then
    Error (Printf.sprintf "field '%s' must not be empty" field)
  else if path.[0] <> '/' then
    Error
      (Printf.sprintf
         "field '%s' must be an absolute path (starts with /), got: %S"
         field path)
  else Ok path

(** Idempotent [mkdir -p]. Used by tools that auto-create parent
    directories on write. *)
let ensure_dir dir =
  if dir <> "" && dir <> "." && dir <> "/" && not (Sys.file_exists dir) then
    ignore
      (Sys.command (Printf.sprintf "mkdir -p %s" (Filename.quote dir)))

(** Process management with timeout. Kills child after [timeout_sec]. *)
let run_with_timeout ~timeout_sec ~cmd =
  let stdin_r, _stdin_w = Unix.pipe () in
  let stdout_r, stdout_w = Unix.pipe () in
  let stderr_r, stderr_w = Unix.pipe () in
  Unix.set_close_on_exec stdout_r;
  Unix.set_close_on_exec stderr_r;
  let pid =
    Unix.create_process "/bin/sh"
      [| "/bin/sh"; "-c"; cmd |]
      stdin_r stdout_w stderr_w
  in
  Unix.close stdin_r;
  Unix.close stdout_w;
  Unix.close stderr_w;
  (* Watchdog *)
  let timed_out = ref false in
  let start = Unix.gettimeofday () in
  let read_some fd =
    let buf = Buffer.create 4096 in
    let chunk = Bytes.create 4096 in
    let rec loop () =
      Unix.set_nonblock fd;
      match Unix.read fd chunk 0 (Bytes.length chunk) with
      | 0 -> ()
      | n ->
          Buffer.add_subbytes buf chunk 0 n;
          loop ()
      | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) ->
          if Unix.gettimeofday () -. start > float_of_int timeout_sec then begin
            timed_out := true;
            (try Unix.kill pid Sys.sigkill with _ -> ())
          end
          else begin
            Unix.sleepf 0.05;
            loop ()
          end
    in
    (try loop () with _ -> ());
    Buffer.contents buf
  in
  let out = read_some stdout_r in
  let err = read_some stderr_r in
  Unix.close stdout_r;
  Unix.close stderr_r;
  let _, status = Unix.waitpid [] pid in
  if !timed_out then Error (Printf.sprintf "command timed out after %ds" timeout_sec)
  else
    let combined =
      match (out, err) with
      | "", "" -> "(no output)"
      | o, "" -> o
      | "", e -> e
      | o, e -> o ^ "\n[stderr]\n" ^ e
    in
    let combined = truncate_string ~max:8000 combined in
    match status with
    | Unix.WEXITED 0 -> Ok combined
    | Unix.WEXITED n ->
        Error (Printf.sprintf "exit %d:\n%s" n combined)
    | Unix.WSIGNALED n ->
        Error (Printf.sprintf "killed by signal %d:\n%s" n combined)
    | Unix.WSTOPPED n ->
        Error (Printf.sprintf "stopped by signal %d:\n%s" n combined)

(* ===== calculator: bc ===== *)

let calculator : tool_def =
  make_typed_tool ~name:"calculator"
    ~description:
      "Evaluate a simple arithmetic expression. Supports +, -, *, /, parens, \
       powers (^). Example: '(15 * 7 + 3) / 2'"
    ~idempotent:true ~timeout_sec:(Some 5.0) ~category:"compute"
    ~input_schema:
      (`Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "expression",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String "Arithmetic expression to evaluate" );
                    ] );
              ] );
          ("required", `List [ `String "expression" ]);
        ])
    ~input_decoder:(fun json ->
      with_object_input json (fun fs ->
          get_string_field "expression" fs))
    ~handler:(fun expression ->
      let safe =
        String.for_all
          (fun c ->
            match c with
            | '0' .. '9' | '+' | '-' | '*' | '/' | '(' | ')' | '.' | ' '
            | '^' | 's' | 'q' | 'r' | 't' ->
                true
            | _ -> false)
          expression
      in
      if not safe then Error "expression contains disallowed characters"
      else
        let cmd =
          Printf.sprintf "echo 'scale=6; %s' | bc -l 2>&1" expression
        in
        Result.map String.trim (read_all_from_proc cmd))
    ()

let http_get : tool_def =
  make_typed_tool ~name:"http_get"
    ~description:
      "Fetch a URL via HTTP GET and return the response body as text. \
       Truncated to ~8000 chars. Useful for reading web pages or API endpoints."
    ~idempotent:true ~timeout_sec:(Some 15.0) ~category:"network"
    ~input_schema:
      (`Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "url",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String "Absolute URL to fetch (https://...)" );
                    ] );
              ] );
          ("required", `List [ `String "url" ]);
        ])
    ~input_decoder:(fun json ->
      with_object_input json (fun fs -> get_string_field "url" fs))
    ~handler:(fun url ->
      (* --compressed: ask for gzip/deflate/zstd. Without this many sites
         (python.org, cloudflare-fronted, etc.) return binary garbage
         that breaks the next LLM call (invalid UTF-8 in tool_result). *)
      let cmd =
        Printf.sprintf
          "curl -sL --compressed --max-time 15 -A 'speedjs/0.1' %s 2>&1"
          (Filename.quote url)
      in
      Result.map (truncate_string ~max:8000) (read_all_from_proc cmd))
    ()

(* ===== current_time ===== *)

let current_time : tool_def =
  make_typed_tool ~name:"current_time"
    ~description:
      "Get the current date and time in ISO 8601 format (local time)."
    ~idempotent:true ~timeout_sec:(Some 1.0) ~category:"compute"
    ~input_schema:
      (`Assoc [ ("type", `String "object"); ("properties", `Assoc []) ])
    ~input_decoder:(fun _ -> Ok ())
    ~handler:(fun () ->
      let t = Unix.gettimeofday () in
      let tm = Unix.localtime t in
      Ok
        (Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d"
           (tm.Unix.tm_year + 1900)
           (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour
           tm.Unix.tm_min tm.Unix.tm_sec))
    ()

(* ===== bash: shell command execution ===== *)

(** [exec_dir] is REQUIRED on bash to prevent the agent from accidentally
    operating on the speedjs source tree (or whatever the OCaml process's
    CWD happens to be). The handler [cd]s into [exec_dir] before running
    the command, so [find .] / relative paths in [command] are anchored
    there — not in whichever directory the parent process started in.
    Modeled on helix's [shell_exec] which has the same required field. *)
type bash_input = { command : string; exec_dir : string }

let bash : tool_def =
  make_typed_tool ~name:"bash"
    ~description:
      "Execute a shell command and return its combined stdout+stderr. \
       30s timeout. Output truncated to ~8000 chars. \
       REQUIRES exec_dir (absolute path) — the command runs in that \
       directory. Use exec_dir=/tmp or similar for one-off commands; for \
       project work pass the project's absolute path so relative paths \
       in your command resolve there."
    (* Not idempotent: shell commands often have side effects (mkdir,
       npm install, file writes). Conservative default. *)
    ~idempotent:false ~timeout_sec:(Some 30.0) ~category:"exec"
    ~input_schema:
      (`Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "command",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String "Shell command to execute (passed to sh -c)" );
                    ] );
                ( "exec_dir",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Working directory (absolute path). The command \
                           runs in this directory." );
                    ] );
              ] );
          ("required", `List [ `String "command"; `String "exec_dir" ]);
        ])
    ~input_decoder:(fun json ->
      with_object_input json (fun fs ->
          let ( let* ) = Result.bind in
          let* command = get_string_field "command" fs in
          let* exec_dir = get_string_field "exec_dir" fs in
          let* exec_dir = require_absolute_path ~field:"exec_dir" exec_dir in
          Ok { command; exec_dir }))
    ~handler:(fun { command; exec_dir } ->
      (* [cd "$dir" && (...)] rather than [chdir] so subshells invoked by
         the user command inherit the dir too. The shell surfaces a clear
         error if exec_dir is missing or not a directory — no need to
         pre-check (TOCTOU). *)
      let wrapped =
        Printf.sprintf "cd %s && (%s)" (Filename.quote exec_dir) command
      in
      run_with_timeout ~timeout_sec:30 ~cmd:wrapped)
    ()

(* ===== view_file ===== *)

type view_file_input = { path : string; view_range : (int * int) option }

let view_file : tool_def =
  make_typed_tool ~name:"view_file"
    ~description:
      "Read a file and return its contents (with line numbers). Optional \
       view_range [start, end] (1-indexed, inclusive). For directories, \
       lists entries instead. PATH MUST BE ABSOLUTE (starts with /)."
    ~idempotent:true ~timeout_sec:(Some 5.0) ~category:"file_io"
    ~input_schema:
      (`Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "path",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String "Absolute file or directory path" );
                    ] );
                ( "view_range",
                  `Assoc
                    [
                      ("type", `String "array");
                      ( "description",
                        `String
                          "Optional [start, end] line numbers, 1-indexed"
                      );
                      ("items", `Assoc [ ("type", `String "integer") ]);
                    ] );
              ] );
          ("required", `List [ `String "path" ]);
        ])
    ~input_decoder:(fun json ->
      with_object_input json (fun fs ->
          let ( let* ) = Result.bind in
          let* path = get_string_field "path" fs in
          let* path = require_absolute_path ~field:"path" path in
          let view_range =
            match List.assoc_opt "view_range" fs with
            | Some (`List [ `Int s; `Int e ]) -> Some (s, e)
            | _ -> None
          in
          Ok { path; view_range }))
    ~handler:(fun { path; view_range } ->
      (* Operate directly; let exceptions surface ENOENT / EISDIR /
         ENOTDIR rather than pre-stat'ing (TOCTOU + extra syscall). *)
      let read_dir () =
        let entries = Sys.readdir path |> Array.to_list |> List.sort compare in
        Printf.sprintf "Directory: %s\n%s" path
          (String.concat "\n" (List.map (fun e -> "  " ^ e) entries))
      in
      let read_file () =
        let ic = open_in path in
        Fun.protect
          ~finally:(fun () -> close_in_noerr ic)
          (fun () ->
            let lines = ref [] in
            (try
               while true do
                 lines := input_line ic :: !lines
               done
             with End_of_file -> ());
            let lines = List.rev !lines in
            let total = List.length lines in
            let s, e =
              match view_range with
              | Some r -> r
              | None -> (1, total)
            in
            let s = max 1 s and e = min total e in
            lines
            |> List.mapi (fun i ln ->
                   let n = i + 1 in
                   if n >= s && n <= e then
                     Some (Printf.sprintf "%5d\t%s" n ln)
                   else None)
            |> List.filter_map Fun.id
            |> String.concat "\n"
            |> truncate_string ~max:8000)
      in
      try
        if Sys.is_directory path then Ok (read_dir ())
        else Ok (read_file ())
      with
      | Sys_error msg -> Error msg
      | e -> Error (Printexc.to_string e))
    ()

(* ===== write_file ===== *)

type write_file_input = { path : string; content : string }

let write_file : tool_def =
  make_typed_tool ~name:"write_file"
    ~description:
      "Create or overwrite a file with the given content. Returns success \
       message with byte count. PATH MUST BE ABSOLUTE (starts with /)."
    (* Not idempotent: overwrites file content. *)
    ~idempotent:false ~timeout_sec:(Some 10.0) ~category:"file_io"
    ~input_schema:
      (`Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "path",
                  `Assoc
                    [
                      ("type", `String "string");
                      ("description", `String "Absolute path to file");
                    ] );
                ( "content",
                  `Assoc
                    [
                      ("type", `String "string");
                      ("description", `String "Full file content");
                    ] );
              ] );
          ("required", `List [ `String "path"; `String "content" ]);
        ])
    ~input_decoder:(fun json ->
      with_object_input json (fun fs ->
          let ( let* ) = Result.bind in
          let* path = get_string_field "path" fs in
          let* path = require_absolute_path ~field:"path" path in
          let* content = get_string_field "content" fs in
          Ok { path; content }))
    ~handler:(fun { path; content } ->
      try
        (* Auto-create the parent dir; saves the LLM 2 iterations every
           time it writes to a fresh subdir. *)
        ensure_dir (Filename.dirname path);
        let oc = open_out path in
        output_string oc content;
        close_out oc;
        Ok
          (Printf.sprintf "wrote %d bytes to %s" (String.length content) path)
      with e ->
        Error (Printf.sprintf "write failed: %s" (Printexc.to_string e)))
    ()

(* ===== str_replace: edit a file by exact string substitution ===== *)

type str_replace_input = {
  path : string;
  old_str : string;
  new_str : string;
}

let str_replace : tool_def =
  make_typed_tool ~name:"str_replace"
    ~description:
      "Edit a file by replacing an exact string with a new one. The old_str \
       must match exactly once in the file (whitespace-sensitive). \
       PATH MUST BE ABSOLUTE (starts with /)."
    (* Not idempotent: mutates file. *)
    ~idempotent:false ~timeout_sec:(Some 10.0) ~category:"file_io"
    ~input_schema:
      (`Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "path",
                  `Assoc
                    [
                      ("type", `String "string");
                      ("description", `String "Absolute path to file");
                    ] );
                ( "old_str",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String "Exact string to replace (must occur once)"
                      );
                    ] );
                ( "new_str",
                  `Assoc
                    [
                      ("type", `String "string");
                      ("description", `String "Replacement string");
                    ] );
              ] );
          ( "required",
            `List [ `String "path"; `String "old_str"; `String "new_str" ]
          );
        ])
    ~input_decoder:(fun json ->
      with_object_input json (fun fs ->
          let ( let* ) = Result.bind in
          let* path = get_string_field "path" fs in
          let* path = require_absolute_path ~field:"path" path in
          let* old_str = get_string_field "old_str" fs in
          let* new_str = get_string_field "new_str" fs in
          Ok { path; old_str; new_str }))
    ~handler:(fun { path; old_str; new_str } ->
      try
        let ic = open_in path in
        let n = in_channel_length ic in
        let content = really_input_string ic n in
        close_in ic;
        (* Find all occurrences using naive substring search. *)
        let find_all needle hay =
          let n = String.length needle in
          let h = String.length hay in
          if n = 0 then []
          else
            let rec loop i acc =
              if i + n > h then List.rev acc
              else if String.sub hay i n = needle then
                loop (i + n) (i :: acc)
              else loop (i + 1) acc
            in
            loop 0 []
        in
        let occurrences = find_all old_str content in
        match occurrences with
        | [] -> Error (Printf.sprintf "old_str not found in %s" path)
        | _ :: _ :: _ ->
            Error
              (Printf.sprintf
                 "old_str matches %d times in %s — must be unique"
                 (List.length occurrences) path)
        | [ idx ] ->
            let before = String.sub content 0 idx in
            let after_start = idx + String.length old_str in
            let after =
              String.sub content after_start
                (String.length content - after_start)
            in
            let new_content = before ^ new_str ^ after in
            let oc = open_out path in
            output_string oc new_content;
            close_out oc;
            Ok
              (Printf.sprintf
                 "replaced 1 occurrence in %s (%d → %d bytes)" path n
                 (String.length new_content))
      with e ->
        Error
          (Printf.sprintf "str_replace failed: %s"
             (Printexc.to_string e)))
    ()

(* ===== ask_user: pause-tool =====

   The LLM uses this when it needs clarification mid-task. The agent loop
   special-cases the tool: instead of running the handler, it suspends with
   [Wait_for_user], saves the session, and exits. The user's next message
   becomes the answer. *)

let ask_user_name = "ask_user"

let ask_user : tool_def =
  {
    (* Pause-tool: handler is never invoked (the agent loop intercepts).
       Metadata is mostly cosmetic for this one. *)
    idempotent = false;
    timeout_sec = None;
    category = "meta";
    name = ask_user_name;
    description =
      "Pause the agent and ask the user a clarifying question. The agent \
       run will halt; the user's next message will be treated as the \
       answer. Use this when you genuinely need the user's input — don't \
       guess.";
    input_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "question",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String "The clarifying question to ask the user" );
                    ] );
              ] );
          ("required", `List [ `String "question" ]);
        ];
    handler =
      (fun _ ->
        Error
          "ask_user is a pause-tool — its handler should never be invoked \
           (the agent loop intercepts it before dispatch)");
  }

(* ===== registry ===== *)

let all =
  [
    calculator;
    http_get;
    current_time;
    bash;
    view_file;
    write_file;
    str_replace;
    ask_user;
  ]

let find name = List.find_opt (fun t -> t.name = name) all
