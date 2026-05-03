(** See [git_checkpoint.mli]. *)

type t = {
  cwd : string;
  head : string;
}

(* Run git, capture stdout. argv-based (no shell), so paths can't be
   command-injected. Drains stdout + stderr concurrently with select —
   sequential drain deadlocks when stderr fills its pipe buffer (~64KiB)
   before stdout closes (e.g. git status with thousands of files). *)
let run_git ~cwd (args : string list) : (string, string) result =
  let argv = Array.of_list ("git" :: "-C" :: cwd :: args) in
  let stdout_r, stdout_w = Unix.pipe ~cloexec:true () in
  let stderr_r, stderr_w = Unix.pipe ~cloexec:true () in
  let pid =
    Unix.create_process "git" argv Unix.stdin stdout_w stderr_w
  in
  Unix.close stdout_w;
  Unix.close stderr_w;
  let out_buf = Buffer.create 256 in
  let err_buf = Buffer.create 256 in
  let chunk = Bytes.create 4096 in
  let read_into fd buf =
    match Unix.read fd chunk 0 4096 with
    | 0 -> false
    | n -> Buffer.add_subbytes buf chunk 0 n; true
    | exception Unix.Unix_error _ -> false
  in
  let open_fds = ref [ stdout_r; stderr_r ] in
  while !open_fds <> [] do
    let ready, _, _ = Unix.select !open_fds [] [] (-1.0) in
    List.iter
      (fun fd ->
        let buf = if fd = stdout_r then out_buf else err_buf in
        if not (read_into fd buf) then begin
          Unix.close fd;
          open_fds := List.filter (fun f -> f <> fd) !open_fds
        end)
      ready
  done;
  let out = Buffer.contents out_buf in
  let err = Buffer.contents err_buf in
  let _, status = Unix.waitpid [] pid in
  match status with
  | Unix.WEXITED 0 -> Ok (String.trim out)
  | Unix.WEXITED n ->
      Error
        (Printf.sprintf "git %s exited %d: %s"
           (String.concat " " args) n
           (String.trim err))
  | Unix.WSIGNALED s ->
      Error (Printf.sprintf "git killed by signal %d" s)
  | Unix.WSTOPPED _ -> Error "git stopped"

let is_git_repo ~cwd =
  Sys.file_exists (Filename.concat cwd ".git")

let head_sha ~cwd : (string, string) result =
  run_git ~cwd [ "rev-parse"; "HEAD" ]

let porcelain_status ~cwd : (string, string) result =
  run_git ~cwd [ "status"; "--porcelain" ]

let validate_clean_repo ~cwd =
  if not (Sys.file_exists cwd) then
    Error (Printf.sprintf "working dir %s does not exist" cwd)
  else if not (is_git_repo ~cwd) then
    Error
      (Printf.sprintf
         "working dir %s is not a git repo. Initialize with:\n\
         \  cd %s && git init && git commit --allow-empty -m 'initial'"
         cwd cwd)
  else
    match head_sha ~cwd with
    | Error _ ->
        Error
          (Printf.sprintf
             "git repo at %s has no commits. Create one with:\n\
             \  cd %s && git commit --allow-empty -m 'initial'"
             cwd cwd)
    | Ok _ ->
        (match porcelain_status ~cwd with
         | Error e -> Error e
         | Ok "" -> Ok ()
         | Ok status ->
             let lines =
               String.split_on_char '\n' status
               |> List.filter (fun s -> s <> "")
             in
             let preview =
               if List.length lines <= 5 then status
               else
                 String.concat "\n"
                   (List.filteri (fun i _ -> i < 5) lines)
                 ^ Printf.sprintf "\n  ... (%d more)" (List.length lines - 5)
             in
             Error
               (Printf.sprintf
                  "working dir %s has uncommitted changes:\n\
                   %s\n\
                   speedjs uses per-task git checkpoints to roll back failed \
                   attempts. Commit or stash your changes first, or run with \
                   --no-checkpoint (data-loss risk)."
                  cwd preview))

let ensure_initial_commit ~cwd =
  if not (is_git_repo ~cwd) then
    Error (Printf.sprintf "%s is not a git repo" cwd)
  else
    match head_sha ~cwd with
    | Ok _ -> Ok ()
    | Error _ ->
        (match
           run_git ~cwd
             [ "commit"; "--allow-empty"; "-m"; "speedjs initial checkpoint" ]
         with
         | Ok _ -> Ok ()
         | Error e -> Error e)

let create ~cwd =
  match head_sha ~cwd with
  | Error e -> Error e
  | Ok head -> Ok { cwd; head }

(* Best-effort cleanup: log on partial failure but don't propagate. The
   alternative — raising — would corrupt the calling Workflow's error
   value (with_checkpoint already has an Error to propagate to the
   caller). *)
let log_warn fmt =
  Printf.ksprintf
    (fun s ->
      try Effect.perform (Effects.Log ("[checkpoint] " ^ s))
      with Effect.Unhandled _ -> ())
    fmt

let rollback (t : t) =
  (match run_git ~cwd:t.cwd [ "reset"; "--hard"; t.head ] with
   | Ok _ -> ()
   | Error e -> log_warn "reset --hard %s failed: %s" t.head e);
  (* -f: force, -d: include untracked dirs. Crucially WITHOUT -x: keep
     gitignored paths (node_modules, .venv) so reinstall isn't needed. *)
  match run_git ~cwd:t.cwd [ "clean"; "-fd" ] with
  | Ok _ -> ()
  | Error e -> log_warn "clean -fd failed: %s" e

let nothing_to_commit ~cwd =
  match porcelain_status ~cwd with
  | Ok "" -> true
  | _ -> false

let commit (t : t) ~message =
  if nothing_to_commit ~cwd:t.cwd then Ok t.head
  else
    match run_git ~cwd:t.cwd [ "add"; "-A" ] with
    | Error e -> Error e
    | Ok _ ->
        (match run_git ~cwd:t.cwd [ "commit"; "-m"; message ] with
         | Error e -> Error e
         | Ok _ -> head_sha ~cwd:t.cwd)
