(** Tests for [Git_checkpoint] and [Workflow.with_checkpoint]: rollback
    removes orphan files (the chaos arphan-side-effect case), reset
    reverts edited tracked content, commit advances HEAD, and the
    combinator preserves Workflow's Result error path. *)

open Speedjs

(* ===== test fixtures ===== *)

(* Make a brand-new git repo in a temp dir with one initial commit so
   [Git_checkpoint.create] has a HEAD to capture. Returns the cwd. *)
let make_repo () =
  let dir = Filename.temp_file "speedjs_gck_" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  let run cmd =
    match Sys.command cmd with
    | 0 -> ()
    | n -> failwith (Printf.sprintf "%s exited %d" cmd n)
  in
  run (Printf.sprintf "git -C %s init --quiet" (Filename.quote dir));
  (* user.name/email aren't always set in CI/sandbox; pin them. *)
  run
    (Printf.sprintf "git -C %s config user.email test@speedjs.local"
       (Filename.quote dir));
  run
    (Printf.sprintf "git -C %s config user.name 'speedjs test'"
       (Filename.quote dir));
  run
    (Printf.sprintf
       "git -C %s commit --allow-empty --quiet -m 'initial'"
       (Filename.quote dir));
  dir

let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let buf = Bytes.create n in
  really_input ic buf 0 n;
  close_in ic;
  Bytes.to_string buf

let file_exists = Sys.file_exists

let with_silent_logs f =
  Log_handler.install Log_handler.null f

(* ===== Git_checkpoint module tests ===== *)

let test_validate_rejects_dirty_tree () =
  let dir = make_repo () in
  write_file (Filename.concat dir "uncommitted.txt") "stuff";
  match Git_checkpoint.validate_clean_repo ~cwd:dir with
  | Ok () -> failwith "expected validate to reject dirty tree"
  | Error msg ->
      assert (Test_helpers.contains msg "uncommitted");
      print_endline
        "✓ validate_clean_repo rejects dirty tree with helpful message"

let test_rollback_removes_orphan_files () =
  let dir = make_repo () in
  let ckpt =
    match Git_checkpoint.create ~cwd:dir with
    | Ok c -> c
    | Error e -> failwith ("create failed: " ^ e)
  in
  (* Simulate chaos creating an orphan + editing nothing tracked. *)
  Unix.mkdir (Filename.concat dir "orphan_dir") 0o755;
  write_file (Filename.concat dir "orphan_dir/notes.py") "junk";
  write_file (Filename.concat dir "orphan.txt") "more junk";
  with_silent_logs (fun () -> Git_checkpoint.rollback ckpt);
  assert (not (file_exists (Filename.concat dir "orphan_dir")));
  assert (not (file_exists (Filename.concat dir "orphan.txt")));
  print_endline
    "✓ rollback removes orphan files + dirs (the chaos arphan side-effect)"

let test_rollback_reverts_tracked_content () =
  let dir = make_repo () in
  let f = Filename.concat dir "tracked.txt" in
  write_file f "original";
  ignore
    (Sys.command
       (Printf.sprintf "git -C %s add tracked.txt && git -C %s commit -q -m 'add'"
          (Filename.quote dir) (Filename.quote dir)));
  let ckpt = Result.get_ok (Git_checkpoint.create ~cwd:dir) in
  write_file f "polluted by chaos";
  with_silent_logs (fun () -> Git_checkpoint.rollback ckpt);
  assert (read_file f = "original");
  print_endline
    "✓ rollback reverts tracked-file edits to checkpoint state"

let test_rollback_preserves_gitignored () =
  let dir = make_repo () in
  write_file (Filename.concat dir ".gitignore") "node_modules/\n.venv/\n";
  ignore
    (Sys.command
       (Printf.sprintf
          "git -C %s add .gitignore && git -C %s commit -q -m 'gitignore'"
          (Filename.quote dir) (Filename.quote dir)));
  let ckpt = Result.get_ok (Git_checkpoint.create ~cwd:dir) in
  Unix.mkdir (Filename.concat dir "node_modules") 0o755;
  write_file (Filename.concat dir "node_modules/foo.js") "// dep";
  write_file (Filename.concat dir "orphan.txt") "junk";
  with_silent_logs (fun () -> Git_checkpoint.rollback ckpt);
  (* gitignored: kept *)
  assert (file_exists (Filename.concat dir "node_modules/foo.js"));
  (* untracked non-ignored: removed *)
  assert (not (file_exists (Filename.concat dir "orphan.txt")));
  print_endline
    "✓ rollback preserves gitignored deps (node_modules / .venv survive)"

let test_commit_advances_head () =
  let dir = make_repo () in
  let ckpt = Result.get_ok (Git_checkpoint.create ~cwd:dir) in
  write_file (Filename.concat dir "new.txt") "hello";
  let new_head =
    match Git_checkpoint.commit ckpt ~message:"task 1: add new.txt" with
    | Ok h -> h
    | Error e -> failwith ("commit failed: " ^ e)
  in
  assert (new_head <> "");
  print_endline
    "✓ commit stages + commits all changes, returns new HEAD sha"

let test_commit_noop_when_clean () =
  let dir = make_repo () in
  let ckpt = Result.get_ok (Git_checkpoint.create ~cwd:dir) in
  match Git_checkpoint.commit ckpt ~message:"empty" with
  | Ok h ->
      assert (h <> "");
      print_endline "✓ commit is no-op when nothing changed (returns prev HEAD)"
  | Error e -> failwith ("commit no-op failed: " ^ e)

(* ===== Workflow.with_checkpoint integration ===== *)

let test_workflow_rollback_on_error () =
  let dir = make_repo () in
  let f = Filename.concat dir "leaked.txt" in
  let body =
    Workflow.of_thunk (fun () ->
        write_file f "leaked by failed task";
        Error
          (Types.Plan_invalid "simulated chaos failure"))
  in
  let wrapped =
    Workflow.with_checkpoint ~cwd:dir
      ~message:(fun () -> "should not reach commit")
      body
  in
  (match with_silent_logs (fun () -> Workflow.run wrapped) with
  | Error _ -> ()
  | Ok _ -> failwith "expected Error to propagate");
  assert (not (file_exists f));
  print_endline
    "✓ with_checkpoint rolls back on Workflow Error (leaked file removed)"

let test_workflow_commit_on_success () =
  let dir = make_repo () in
  let f = Filename.concat dir "kept.txt" in
  let body =
    Workflow.of_thunk (fun () ->
        write_file f "task output";
        Ok ())
  in
  let wrapped =
    Workflow.with_checkpoint ~cwd:dir
      ~message:(fun () -> "task ok")
      body
  in
  (match with_silent_logs (fun () -> Workflow.run wrapped) with
  | Ok () -> ()
  | Error _ -> failwith "expected Ok");
  assert (file_exists f);
  (* and is now committed: working tree clean *)
  (match Git_checkpoint.validate_clean_repo ~cwd:dir with
  | Ok () -> ()
  | Error msg -> failwith ("expected clean post-commit, got: " ^ msg));
  print_endline
    "✓ with_checkpoint commits on success, working tree clean afterwards"

let test_workflow_each_retry_sees_clean_slate () =
  (* Crucial integration: when with_checkpoint sits inside with_retry,
     each retry attempt observes the pre-task state, NOT the prior
     attempt's leaked files. This is the chaos resilience property. *)
  let dir = make_repo () in
  let leaked_seen_on_retry = ref false in
  let attempt_count = ref 0 in
  let body =
    Workflow.of_thunk (fun () ->
        incr attempt_count;
        let leaked = Filename.concat dir "from_prior_attempt.txt" in
        if !attempt_count = 2 && file_exists leaked then
          leaked_seen_on_retry := true;
        if !attempt_count = 1 then (
          write_file leaked "polluted";
          Error (Types.Plan_invalid "first attempt fails"))
        else Ok "second attempt wins")
  in
  let wrapped =
    Workflow.with_retry ~max_attempts:2
      (Workflow.with_checkpoint ~cwd:dir
         ~message:(fun () ->
           Printf.sprintf "task attempt %d" !attempt_count)
         body)
  in
  (match with_silent_logs (fun () -> Workflow.run wrapped) with
  | Ok "second attempt wins" -> ()
  | Ok other -> failwith ("unexpected Ok: " ^ other)
  | Error _ -> failwith "expected retry to succeed");
  assert (not !leaked_seen_on_retry);
  assert (!attempt_count = 2);
  print_endline
    "✓ with_retry + with_checkpoint: retry sees clean slate, not prior \
     attempt's leak"

let test_workflow_noop_for_non_repo () =
  let dir = Filename.temp_file "speedjs_gck_nonrepo_" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  let ran = ref false in
  let body =
    Workflow.of_thunk (fun () ->
        ran := true;
        Ok ())
  in
  let wrapped =
    Workflow.with_checkpoint ~cwd:dir
      ~message:(fun () -> "noop")
      body
  in
  (match with_silent_logs (fun () -> Workflow.run wrapped) with
  | Ok () -> ()
  | Error _ -> failwith "non-repo wrapper should fall through to inner");
  assert !ran;
  print_endline
    "✓ with_checkpoint is a transparent no-op when cwd isn't a git repo"

let run () =
  print_endline "\n=== Git_checkpoint ===";
  test_validate_rejects_dirty_tree ();
  test_rollback_removes_orphan_files ();
  test_rollback_reverts_tracked_content ();
  test_rollback_preserves_gitignored ();
  test_commit_advances_head ();
  test_commit_noop_when_clean ();
  test_workflow_rollback_on_error ();
  test_workflow_commit_on_success ();
  test_workflow_each_retry_sees_clean_slate ();
  test_workflow_noop_for_non_repo ()
