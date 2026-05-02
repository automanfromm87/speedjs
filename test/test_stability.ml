(** Stability gap probes — confirm absence of guards we discussed.
    Cover curl --max-time, stream first-byte / idle timeouts,
    MCP read timeout, and Tool_handler.with_timeout. *)

open Speedjs
open Types

(** Regression: spawn_curl with [max_time_sec] caps the whole transfer. *)
let test_curl_max_time_caps_transfer () =
  let pid, stdout_r, stderr_r =
    Speedjs.Http.spawn_curl ~max_time_sec:2.0 ~proxy:"127.0.0.1:1"
      ~url:"http://example.invalid" ~headers:[] ~body:"x" ~stream:false
      ()
  in
  let start = Unix.gettimeofday () in
  let _ = Speedjs.Http.read_all stdout_r in
  let _ = Speedjs.Http.read_all stderr_r in
  let _ = Unix.waitpid [] pid in
  let elapsed = Unix.gettimeofday () -. start in
  Unix.close stdout_r;
  Unix.close stderr_r;
  assert (elapsed < 3.0);
  print_endline
    (Printf.sprintf
       "✓ curl --max-time caps transfer at %.0fs cap (actual %.3fs)" 2.0
       elapsed)

(** Regression: [read_line_with_deadlines] raises
    [Stream_first_byte_timeout] when nothing arrives. *)
let test_stream_first_byte_timeout () =
  let stdin_r, stdin_w = Unix.pipe () in
  let stdout_r, stdout_w = Unix.pipe () in
  let pid =
    Unix.create_process "sleep"
      [| "sleep"; "5" |]
      stdin_r stdout_w Unix.stderr
  in
  Unix.close stdin_r;
  Unix.close stdout_w;
  Unix.close stdin_w;
  let reader =
    Speedjs.Http.make_line_reader ~first_byte_sec:0.2 ~idle_sec:5.0
      stdout_r
  in
  let start = Unix.gettimeofday () in
  (try
     let _ = Speedjs.Http.read_line_with_deadlines reader in
     failwith "expected first-byte timeout"
   with Speedjs.Http.Stream_first_byte_timeout sec -> assert (sec = 0.2));
  let elapsed = Unix.gettimeofday () -. start in
  assert (elapsed < 1.0);
  (try Unix.kill pid Sys.sigkill with _ -> ());
  let _ = Unix.waitpid [] pid in
  Unix.close stdout_r;
  print_endline
    (Printf.sprintf
       "✓ Http.read_line_with_deadlines fires Stream_first_byte_timeout (%.3fs)"
       elapsed)

(** Regression: idle timeout fires after first byte then silence. *)
let test_stream_idle_timeout () =
  let stdin_r, stdin_w = Unix.pipe () in
  let stdout_r, stdout_w = Unix.pipe () in
  let pid =
    Unix.create_process "sh"
      [| "sh"; "-c"; "printf 'hi\\n'; sleep 5" |]
      stdin_r stdout_w Unix.stderr
  in
  Unix.close stdin_r;
  Unix.close stdout_w;
  Unix.close stdin_w;
  let reader =
    Speedjs.Http.make_line_reader ~first_byte_sec:5.0 ~idle_sec:0.2
      stdout_r
  in
  let line = Speedjs.Http.read_line_with_deadlines reader in
  assert (line = "hi");
  let start = Unix.gettimeofday () in
  (try
     let _ = Speedjs.Http.read_line_with_deadlines reader in
     failwith "expected idle timeout"
   with Speedjs.Http.Stream_idle_timeout sec -> assert (sec = 0.2));
  let elapsed = Unix.gettimeofday () -. start in
  assert (elapsed < 1.0);
  (try Unix.kill pid Sys.sigkill with _ -> ());
  let _ = Unix.waitpid [] pid in
  Unix.close stdout_r;
  print_endline
    (Printf.sprintf
       "✓ Http.read_line_with_deadlines fires Stream_idle_timeout after \
        first byte (%.3fs)"
       elapsed)

(** Regression: MCP read_line raises [Read_timeout] when stdout has no
    data within the cap. *)
let test_mcp_read_timeout () =
  let conn =
    Speedjs.Mcp.start ~read_timeout_sec:0.3 ~cmd:"sleep" ~args:[ "10" ]
      ()
  in
  let start = Unix.gettimeofday () in
  (match Speedjs.Mcp.read_line conn with
  | exception Speedjs.Mcp.Read_timeout sec -> assert (sec = 0.3)
  | _ -> failwith "expected Read_timeout from a wedged MCP server");
  let elapsed = Unix.gettimeofday () -. start in
  assert (elapsed < 1.0);
  Speedjs.Mcp.shutdown conn;
  print_endline
    (Printf.sprintf
       "✓ MCP read_line raises Read_timeout on wedged server (%.3fs)"
       elapsed)

(** Regression: [Tool_handler.with_timeout] enforces tool.timeout_sec. *)
let test_tool_handler_with_timeout_aborts_slow_tool () =
  let slow_tool : tool_def =
    {
      name = "slow";
      description = "sleeps";
      input_schema = `Assoc [];
      handler =
        (fun _ ->
          Unix.sleepf 1.0;
          Ok "done");
      idempotent = true;
      timeout_sec = Some 0.1;
      category = "test";
      capabilities = [ Read_only ];
      allowed_modes = [ Planner; Recovery; Executor; Subagent ];
      classify_error = default_classify_error;
    }
  in
  let chain = Tool_handler.direct |> Tool_handler.with_timeout in
  let start = Unix.gettimeofday () in
  let result =
    chain { tool = slow_tool; input = `Assoc []; use_id = Id.Tool_use_id.of_string "u1" }
  in
  let elapsed = Unix.gettimeofday () -. start in
  (match result with
  | Error err when err.code = "timeout" -> ()
  | _ -> failwith "expected timeout error");
  assert (elapsed < 0.5);
  print_endline
    (Printf.sprintf
       "✓ Tool_handler.with_timeout aborts at tool.timeout_sec=0.1s \
        (actual %.3fs)"
       elapsed)

(* Regression: stderr-heavy commands must not deadlock the
   stdout-then-stderr drain order. Pipe buffer is typically 16-64
   KiB; we write 200 KiB to stderr, expect the command to complete
   normally (NOT time out), and the stderr content to come through. *)
let test_run_with_timeout_drains_stderr_concurrently () =
  let big = 200 * 1024 in
  let cmd =
    Printf.sprintf
      "yes 'X' | head -c %d 1>&2; echo done-on-stdout"
      big
  in
  let start = Unix.gettimeofday () in
  let result = Tools.run_with_timeout ~timeout_sec:5 ~cmd in
  let elapsed = Unix.gettimeofday () -. start in
  let contains s sub =
    let ls = String.length s and lsub = String.length sub in
    let rec scan i =
      if i + lsub > ls then false
      else if String.sub s i lsub = sub then true
      else scan (i + 1)
    in
    scan 0
  in
  (match result with
  | Ok body -> assert (contains body "done-on-stdout")
  | Error msg ->
      failwith
        (Printf.sprintf "expected Ok with stderr drained, got: %s" msg));
  assert (elapsed < 5.0);
  print_endline
    (Printf.sprintf
       "✓ run_with_timeout drains stdout+stderr concurrently (%.3fs, no \
        deadlock on 200KB stderr)"
       elapsed)

(* Regression: a command that reads stdin (cat, sort, ...) must see
   EOF immediately and complete, not block forever waiting for input
   we never send. The old impl created the stdin pipe but never
   closed the parent's WRITE end, so cat hung until run_with_timeout
   killed it on timeout. *)
let test_run_with_timeout_closes_stdin_eof () =
  let start = Unix.gettimeofday () in
  let result = Tools.run_with_timeout ~timeout_sec:5 ~cmd:"cat" in
  let elapsed = Unix.gettimeofday () -. start in
  (match result with
  | Ok body -> assert (body = "(no output)" || body = "")
  | Error msg ->
      failwith
        (Printf.sprintf "cat with no stdin should exit cleanly, got: %s"
           msg));
  assert (elapsed < 1.0);
  print_endline
    (Printf.sprintf
       "✓ run_with_timeout: child sees stdin EOF immediately (cat \
        completed in %.3fs, not 5s timeout)"
       elapsed)

let run () =
  test_curl_max_time_caps_transfer ();
  test_stream_first_byte_timeout ();
  test_stream_idle_timeout ();
  test_mcp_read_timeout ();
  test_tool_handler_with_timeout_aborts_slow_tool ();
  test_run_with_timeout_drains_stderr_concurrently ();
  test_run_with_timeout_closes_stdin_eof ()
