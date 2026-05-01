(** Tests for [Time_now] / [File_*] effects + their handlers.

    Verifies the effect-level abstraction works:
    - production handler hits real disk / clock
    - mock handler returns canned values for hermetic tests
    - tools that perform these effects pick up whichever is installed *)

open Speedjs

let test_time_now_default_handler_returns_real_time () =
  let t1 =
    Time_handler.install (fun () -> Effect.perform Effects.Time_now)
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

let run () =
  test_time_now_default_handler_returns_real_time ();
  test_time_now_fixed_handler_returns_canned ();
  test_file_handler_in_memory_round_trip ();
  test_file_handler_stat_classifies_paths ();
  test_file_handler_list_dir_returns_basenames ();
  test_file_handler_read_missing_returns_error ();
  test_view_file_tool_uses_file_effects ();
  test_current_time_tool_uses_time_effect ()
