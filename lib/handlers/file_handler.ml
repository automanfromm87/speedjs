(** Production handler for [Effects.File_*].

    Real-disk implementations using [In_channel] / [Out_channel] / [Sys].
    Mockable: tests / Checkpoint replay can install [install_fs] backed
    by an in-memory map for deterministic, hermetic runs. *)

(* ===== production: real disk ===== *)

let read_all_in path =
  try
    let ic = open_in path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        let n = in_channel_length ic in
        Ok (really_input_string ic n))
  with
  | Sys_error msg -> Error msg
  | e -> Error (Printexc.to_string e)

let ensure_dir dir =
  if dir <> "" && dir <> "." && dir <> "/" && not (Sys.file_exists dir) then
    ignore (Sys.command (Printf.sprintf "mkdir -p %s" (Filename.quote dir)))

let write_all path content =
  try
    ensure_dir (Filename.dirname path);
    let oc = open_out path in
    Fun.protect
      ~finally:(fun () -> close_out_noerr oc)
      (fun () ->
        output_string oc content;
        Ok (String.length content))
  with
  | Sys_error msg -> Error msg
  | e -> Error (Printexc.to_string e)

let list_dir path =
  try Ok (Sys.readdir path |> Array.to_list |> List.sort compare)
  with
  | Sys_error msg -> Error msg
  | e -> Error (Printexc.to_string e)

let stat_path path : Effects.path_kind =
  if not (Sys.file_exists path) then `Missing
  else if Sys.is_directory path then `Dir
  else `File

let install (thunk : unit -> 'a) : 'a =
  Effect.Deep.try_with thunk ()
    {
      effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Effects.File_read path ->
              Some
                (fun (k : (a, _) Effect.Deep.continuation) ->
                  Effect.Deep.continue k (read_all_in path))
          | Effects.File_write { path; content } ->
              Some
                (fun (k : (a, _) Effect.Deep.continuation) ->
                  Effect.Deep.continue k (write_all path content))
          | Effects.File_list_dir path ->
              Some
                (fun (k : (a, _) Effect.Deep.continuation) ->
                  Effect.Deep.continue k (list_dir path))
          | Effects.File_stat path ->
              Some
                (fun (k : (a, _) Effect.Deep.continuation) ->
                  Effect.Deep.continue k (stat_path path))
          | _ -> None);
    }

(* ===== test: in-memory FS ===== *)

(** In-memory virtual filesystem for tests. [files] is a mutable
    [(path, content) Hashtbl] — writes update it, reads consult it.
    [list_dir] computes children by prefix-match on the path. *)
let install_fs ~files (thunk : unit -> 'a) : 'a =
  let read path =
    match Hashtbl.find_opt files path with
    | Some s -> Ok s
    | None -> Error (Printf.sprintf "No such file: %s" path)
  in
  let write path content =
    Hashtbl.replace files path content;
    Ok (String.length content)
  in
  let list path =
    let prefix = if String.length path > 0 && path.[String.length path - 1] = '/' then path else path ^ "/" in
    let plen = String.length prefix in
    Hashtbl.fold
      (fun k _ acc ->
        if String.length k > plen && String.sub k 0 plen = prefix then
          let rest = String.sub k plen (String.length k - plen) in
          let entry =
            try String.sub rest 0 (String.index rest '/')
            with Not_found -> rest
          in
          if List.mem entry acc then acc else entry :: acc
        else acc)
      files []
    |> List.sort compare
    |> Result.ok
  in
  let stat path : Effects.path_kind =
    if Hashtbl.mem files path then `File
    else
      let prefix = if String.length path > 0 && path.[String.length path - 1] = '/' then path else path ^ "/" in
      let plen = String.length prefix in
      let any_under =
        Hashtbl.fold
          (fun k _ acc ->
            acc
            || (String.length k > plen && String.sub k 0 plen = prefix))
          files false
      in
      if any_under then `Dir else `Missing
  in
  Effect.Deep.try_with thunk ()
    {
      effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Effects.File_read path ->
              Some
                (fun (k : (a, _) Effect.Deep.continuation) ->
                  Effect.Deep.continue k (read path))
          | Effects.File_write { path; content } ->
              Some
                (fun (k : (a, _) Effect.Deep.continuation) ->
                  Effect.Deep.continue k (write path content))
          | Effects.File_list_dir path ->
              Some
                (fun (k : (a, _) Effect.Deep.continuation) ->
                  Effect.Deep.continue k (list path))
          | Effects.File_stat path ->
              Some
                (fun (k : (a, _) Effect.Deep.continuation) ->
                  Effect.Deep.continue k (stat path))
          | _ -> None);
    }
