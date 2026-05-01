(** Tests for [Time_now] / [File_*] effects + their handlers.

    Verifies the effect-level abstraction works:
    - production handler hits real disk / clock
    - mock handler returns canned values for hermetic tests
    - tools that perform these effects pick up whichever is installed *)

open Speedjs
open Types

let test_time_now_default_handler_returns_real_time () =
  let t1 =
    Time_handler.install Time_handler.direct (fun () ->
        Effect.perform Effects.Time_now)
  in
  (* Sanity: must be in the recent-past / near-future range *)
  assert (t1 > 1.7e9 && t1 < 3.0e9);
  print_endline "✓ Time_handler.install delegates Time_now to Unix.gettimeofday"

let test_time_now_fixed_handler_returns_canned () =
  let canned = 1700000000.0 in
  let observed = ref None in
  Time_handler.install_fixed ~now:canned (fun () ->
      observed := Some (Effect.perform Effects.Time_now));
  assert (!observed = Some canned);
  print_endline "✓ Time_handler.install_fixed returns canned value"

let test_file_handler_in_memory_round_trip () =
  let files = Hashtbl.create 4 in
  File_handler.install_fs ~files (fun () ->
      let written =
        Effect.perform
          (Effects.File_write
             { path = "/v/hello.txt"; content = "hi there" })
      in
      assert (written = Ok 8);
      let read = Effect.perform (Effects.File_read "/v/hello.txt") in
      assert (read = Ok "hi there"));
  assert (Hashtbl.find files "/v/hello.txt" = "hi there");
  print_endline "✓ File_handler.install_fs round-trips writes through reads"

let test_file_handler_stat_classifies_paths () =
  let files = Hashtbl.create 4 in
  Hashtbl.add files "/proj/a.ml" "let x = 1";
  Hashtbl.add files "/proj/b.ml" "let y = 2";
  File_handler.install_fs ~files (fun () ->
      assert (Effect.perform (Effects.File_stat "/proj/a.ml") = `File);
      assert (Effect.perform (Effects.File_stat "/proj") = `Dir);
      assert (Effect.perform (Effects.File_stat "/missing") = `Missing));
  print_endline "✓ File_handler.install_fs stats files / dirs / missing paths"

let test_file_handler_list_dir_returns_basenames () =
  let files = Hashtbl.create 4 in
  Hashtbl.add files "/proj/a.ml" "";
  Hashtbl.add files "/proj/b.ml" "";
  Hashtbl.add files "/proj/sub/c.ml" "";
  let listed = ref [] in
  File_handler.install_fs ~files (fun () ->
      match Effect.perform (Effects.File_list_dir "/proj") with
      | Ok entries -> listed := entries
      | Error e -> failwith e);
  assert (!listed = [ "a.ml"; "b.ml"; "sub" ]);
  print_endline "✓ File_handler.install_fs list_dir returns sorted basenames"

let test_file_handler_read_missing_returns_error () =
  let files = Hashtbl.create 4 in
  let r = ref (Ok "") in
  File_handler.install_fs ~files (fun () ->
      r := Effect.perform (Effects.File_read "/no/such/file"));
  (match !r with
  | Error msg ->
      assert (Test_helpers.contains msg "/no/such/file")
  | Ok _ -> failwith "expected Error");
  print_endline "✓ File_handler.install_fs read missing path returns Error"

let test_view_file_tool_uses_file_effects () =
  (* End-to-end: view_file tool's handler performs File_stat + File_read.
     Install the in-memory FS, dispatch via the tool handler, verify
     the formatted output matches what the FS contains. *)
  let files = Hashtbl.create 4 in
  Hashtbl.add files "/p/x.txt" "line 1\nline 2\nline 3";
  let result = ref (Ok "") in
  File_handler.install_fs ~files (fun () ->
      result :=
        Tools.view_file.handler
          (`Assoc [ ("path", `String "/p/x.txt") ]));
  (match !result with
  | Ok s ->
      assert (Test_helpers.contains s "line 1");
      assert (Test_helpers.contains s "line 2");
      assert (Test_helpers.contains s "line 3")
  | Error e -> failwith ("view_file failed: " ^ e));
  print_endline
    "✓ Tools.view_file.handler reads through File_handler.install_fs"

let test_current_time_tool_uses_time_effect () =
  let result = ref (Ok "") in
  Time_handler.install_fixed ~now:0.0 (fun () ->
      result := Tools.current_time.handler (`Assoc []));
  (match !result with
  | Ok s ->
      (* Unix.localtime 0.0 → 1970-01-01 in UTC; in a non-UTC TZ it shifts.
         We only assert the year prefix to stay TZ-portable. *)
      assert (Test_helpers.contains s "1969" || Test_helpers.contains s "1970")
  | Error e -> failwith ("current_time failed: " ^ e));
  print_endline
    "✓ Tools.current_time.handler reads through Time_handler.install_fixed"

(* ===== File_handler middleware ===== *)

(* ========================================================================
   Sandbox attack-vector library — pin down what's defended vs known-open.
   ======================================================================== *)

let attempt_read root path =
  let files = Hashtbl.create 4 in
  Hashtbl.add files "/proj/safe.txt" "ok";
  Hashtbl.add files "/etc/passwd" "secret";
  let chain =
    File_handler.in_memory ~files |> File_handler.with_sandbox ~root
  in
  let result = ref (Ok "") in
  File_handler.install chain (fun () ->
      result := Effect.perform (Effects.File_read path));
  !result

let test_sandbox_vectors_pinned () =
  (* Pin current behavior for known attack shapes. Some are defended,
     some explicitly NOT (and noted) — this test locks both classes
     so future changes are visible diffs. *)
  let root = "/proj" in
  let allowed =
    match attempt_read root "/proj/safe.txt" with
    | Ok "ok" -> true
    | _ -> false
  in
  assert allowed;

  let outside =
    match attempt_read root "/etc/passwd" with Error _ -> true | _ -> false
  in
  assert outside;

  let prefix_confused =
    match attempt_read root "/proj-evil/x" with
    | Error _ -> true
    | _ -> false
  in
  assert prefix_confused;

  let relative =
    match attempt_read root "etc/passwd" with Error _ -> true | _ -> false
  in
  assert relative;

  let empty_path =
    match attempt_read root "" with Error _ -> true | _ -> false
  in
  assert empty_path;

  (* Path traversal: normalize_path resolves [..] before the prefix
     check, so /proj/../etc/passwd → /etc/passwd → rejected. *)
  let dotdot_defended =
    match attempt_read root "/proj/../etc/passwd" with
    | Error msg -> Test_helpers.contains msg "escapes sandbox"
    | Ok _ -> false
  in
  assert dotdot_defended;

  (* Sibling traversal that resolves to under-root must still pass:
     /proj/sub/../safe.txt → /proj/safe.txt → allowed. *)
  let lateral_kept_inside =
    let files = Hashtbl.create 4 in
    Hashtbl.add files "/proj/safe.txt" "ok";
    let chain =
      File_handler.in_memory ~files |> File_handler.with_sandbox ~root
    in
    let r = ref (Ok "") in
    File_handler.install chain (fun () ->
        r := Effect.perform (Effects.File_read "/proj/sub/../safe.txt"));
    match !r with Ok "ok" -> true | _ -> false
  in
  assert lateral_kept_inside;

  print_endline
    "✓ Sandbox vectors: blocks /etc, /proj-evil, relative, empty, \
     /proj/../etc/passwd; allows /proj/sub/../safe.txt (resolves under \
     root)"

let test_with_sandbox_rejects_outside_root () =
  let files = Hashtbl.create 4 in
  Hashtbl.add files "/proj/a.ml" "ok";
  Hashtbl.add files "/etc/passwd" "secret";
  let chain =
    File_handler.in_memory ~files
    |> File_handler.with_sandbox ~root:"/proj"
  in
  File_handler.install chain (fun () ->
      assert (Effect.perform (Effects.File_read "/proj/a.ml") = Ok "ok");
      (match Effect.perform (Effects.File_read "/etc/passwd") with
      | Error msg -> assert (Test_helpers.contains msg "escapes sandbox")
      | Ok _ -> failwith "sandbox let /etc/passwd through");
      (* prefix-confusion guard: /proj-evil should NOT match /proj *)
      (match Effect.perform (Effects.File_read "/proj-evil/x") with
      | Error _ -> ()
      | Ok _ -> failwith "sandbox confused /proj-evil with /proj"));
  print_endline
    "✓ File_handler.with_sandbox rejects paths outside root (no prefix \
     confusion)"

let test_with_audit_observes_every_op () =
  let files = Hashtbl.create 4 in
  Hashtbl.add files "/p/a.ml" "x";
  let observed = ref [] in
  let on_op ~op ~path ~ok =
    observed := (op, path, ok) :: !observed
  in
  let chain =
    File_handler.in_memory ~files |> File_handler.with_audit ~on_op
  in
  File_handler.install chain (fun () ->
      let _ = Effect.perform (Effects.File_read "/p/a.ml") in
      let _ = Effect.perform (Effects.File_read "/missing") in
      let _ =
        Effect.perform
          (Effects.File_write { path = "/p/b.ml"; content = "y" })
      in
      let _ = Effect.perform (Effects.File_stat "/p") in
      ());
  let log = List.rev !observed in
  (match log with
  | [
      (`Read, "/p/a.ml", true);
      (`Read, "/missing", false);
      (`Write, "/p/b.ml", true);
      (`Stat, "/p", true);
    ] -> ()
  | _ -> failwith "audit log doesn't match expected sequence");
  print_endline "✓ File_handler.with_audit observes every op with success flag"

let test_memory_persist_through_file_effects () =
  (* Memory.persist should write to whatever File_handler is installed.
     Verify by installing in-memory FS and checking the Hashtbl after. *)
  let files = Hashtbl.create 4 in
  File_handler.install (File_handler.in_memory ~files) (fun () ->
      let mem = Memory.create ~dir:"/var/mem" ~name:"executor" () in
      Memory.push mem (user_text_message "hi");
      Memory.push mem (assistant_text_message "hello");
      Memory.persist mem);
  let body = Hashtbl.find files "/var/mem/executor.json" in
  assert (Test_helpers.contains body "\"messages\"");
  assert (Test_helpers.contains body "hi");
  assert (Test_helpers.contains body "hello");
  print_endline "✓ Memory.persist writes through File_handler.install"

let test_memory_create_loads_prior_through_file_effects () =
  (* Pre-seed the virtual FS with a memory file, then Memory.create
     should pick it up via File_read. *)
  let files = Hashtbl.create 4 in
  let body =
    {|{"model": "test", "messages": [
       {"role": "user", "content": [{"type": "text", "text": "earlier"}]}
     ], "pending_tool_use_id": null}|}
  in
  Hashtbl.add files "/var/mem/executor.json" body;
  File_handler.install (File_handler.in_memory ~files) (fun () ->
      let mem = Memory.create ~dir:"/var/mem" ~name:"executor" () in
      assert (Memory.length mem = 1);
      let msgs = Memory.to_messages mem in
      match msgs with
      | [ { role = User; content = [ Text "earlier" ] } ] -> ()
      | _ -> failwith "memory didn't reload prior message");
  print_endline
    "✓ Memory.create reloads prior messages through File_handler.install"

let test_with_read_cache_avoids_reread () =
  let files = Hashtbl.create 4 in
  Hashtbl.add files "/p/a.ml" "v1";
  let read_count = ref 0 in
  let inner =
    let base = File_handler.in_memory ~files in
    {
      base with
      read =
        (fun path ->
          incr read_count;
          base.read path);
    }
  in
  let chain = File_handler.with_read_cache inner in
  File_handler.install chain (fun () ->
      assert (Effect.perform (Effects.File_read "/p/a.ml") = Ok "v1");
      assert (Effect.perform (Effects.File_read "/p/a.ml") = Ok "v1");
      assert (Effect.perform (Effects.File_read "/p/a.ml") = Ok "v1");
      assert (!read_count = 1);
      (* Write must evict the cached entry. *)
      let _ =
        Effect.perform
          (Effects.File_write { path = "/p/a.ml"; content = "v2" })
      in
      assert (Effect.perform (Effects.File_read "/p/a.ml") = Ok "v2");
      assert (!read_count = 2));
  print_endline
    "✓ File_handler.with_read_cache memoizes reads; writes evict the path"

let run () =
  test_time_now_default_handler_returns_real_time ();
  test_time_now_fixed_handler_returns_canned ();
  test_file_handler_in_memory_round_trip ();
  test_file_handler_stat_classifies_paths ();
  test_file_handler_list_dir_returns_basenames ();
  test_file_handler_read_missing_returns_error ();
  test_view_file_tool_uses_file_effects ();
  test_current_time_tool_uses_time_effect ();
  test_with_sandbox_rejects_outside_root ();
  test_sandbox_vectors_pinned ();
  test_with_audit_observes_every_op ();
  test_with_read_cache_avoids_reread ();
  test_memory_persist_through_file_effects ();
  test_memory_create_loads_prior_through_file_effects ()
