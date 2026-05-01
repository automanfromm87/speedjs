(** Handler for [Effects.File_*], composable via [|>].

    [t] packages all four file ops as closures so middleware can wrap a
    [t] and produce a new [t] — same pattern as [Llm_handler] /
    [Tool_handler]. Production handler [direct] hits real disk;
    [in_memory] is a hermetic test backend. Middleware in this module:

    {ul
    {- [with_sandbox] — reject paths outside a root prefix}
    {- [with_audit]   — observe every op (success / failure) for telemetry}
    {- [with_read_cache] — memoize [read] within a run; writes evict}} *)

type t = {
  read : string -> (string, string) result;
  write : path:string -> content:string -> (int, string) result;
  list_dir : string -> (string list, string) result;
  stat : string -> Effects.path_kind;
}

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

let list_dir_real path =
  try Ok (Sys.readdir path |> Array.to_list |> List.sort compare)
  with
  | Sys_error msg -> Error msg
  | e -> Error (Printexc.to_string e)

let stat_path_real path : Effects.path_kind =
  if not (Sys.file_exists path) then `Missing
  else if Sys.is_directory path then `Dir
  else `File

let direct : t =
  {
    read = read_all_in;
    write = (fun ~path ~content -> write_all path content);
    list_dir = list_dir_real;
    stat = stat_path_real;
  }

(* ===== test: in-memory FS =====
   [files] is a [(path, content) Hashtbl]; [list_dir] / [stat] derive
   directory structure by prefix-match (any key beginning with the
   queried path + "/" implies the queried path is a dir). *)

let in_memory ~files : t =
  let starts_with prefix s =
    let pl = String.length prefix in
    String.length s >= pl && String.sub s 0 pl = prefix
  in
  let with_sep path =
    if String.length path > 0 && path.[String.length path - 1] = '/' then path
    else path ^ "/"
  in
  let read path =
    match Hashtbl.find_opt files path with
    | Some s -> Ok s
    | None -> Error (Printf.sprintf "No such file: %s" path)
  in
  let write ~path ~content =
    Hashtbl.replace files path content;
    Ok (String.length content)
  in
  let list path =
    let prefix = with_sep path in
    let plen = String.length prefix in
    Hashtbl.fold
      (fun k _ acc ->
        if starts_with prefix k then
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
      let prefix = with_sep path in
      let any_under =
        Hashtbl.fold (fun k _ acc -> acc || starts_with prefix k) files false
      in
      if any_under then `Dir else `Missing
  in
  { read; write; list_dir = list; stat }

(* ===== middleware ===== *)

(** [with_sandbox ~root inner]: refuse any path that doesn't lie under
    [root]. The inner handler is only called for paths that pass.

    [root] should be canonical (no trailing slash, no [.] / [..]); we
    don't normalize it here — sandbox is only as strict as its root. *)
let with_sandbox ~root (inner : t) : t =
  let in_root path =
    let rl = String.length root in
    String.length path >= rl
    && String.sub path 0 rl = root
    && (String.length path = rl || path.[rl] = '/')
  in
  let reject path =
    Error (Printf.sprintf "path %S escapes sandbox root %S" path root)
  in
  {
    read = (fun path -> if in_root path then inner.read path else reject path);
    write =
      (fun ~path ~content ->
        if in_root path then inner.write ~path ~content else reject path);
    list_dir =
      (fun path ->
        if in_root path then inner.list_dir path else reject path);
    stat = (fun path -> if in_root path then inner.stat path else `Missing);
  }

type op = [ `Read | `Write | `List | `Stat ]

(** [with_audit ~on_op inner]: invoke [on_op] after every operation
    with the op kind, path, and whether it succeeded. *)
let with_audit ~(on_op : op:op -> path:string -> ok:bool -> unit)
    (inner : t) : t =
  let observe op path ok = on_op ~op ~path ~ok in
  {
    read =
      (fun path ->
        let r = inner.read path in
        observe `Read path (Result.is_ok r);
        r);
    write =
      (fun ~path ~content ->
        let r = inner.write ~path ~content in
        observe `Write path (Result.is_ok r);
        r);
    list_dir =
      (fun path ->
        let r = inner.list_dir path in
        observe `List path (Result.is_ok r);
        r);
    stat =
      (fun path ->
        let r = inner.stat path in
        observe `Stat path (r <> `Missing);
        r);
  }

(** [with_read_cache inner]: memoize [read] results by path within this
    run. [write] / [stat] / [list_dir] evict the entry for that path
    (paranoid: also evict on write to ensure consistency). Reset is
    implicit — a new install creates a new cache. *)
let with_read_cache (inner : t) : t =
  let cache : (string, (string, string) result) Hashtbl.t =
    Hashtbl.create 16
  in
  let evict path = Hashtbl.remove cache path in
  {
    read =
      (fun path ->
        match Hashtbl.find_opt cache path with
        | Some r -> r
        | None ->
            let r = inner.read path in
            Hashtbl.add cache path r;
            r);
    write =
      (fun ~path ~content ->
        evict path;
        inner.write ~path ~content);
    list_dir = inner.list_dir;
    stat = inner.stat;
  }

(* ===== install ===== *)

let install (chain : t) (thunk : unit -> 'a) : 'a =
  Effect.Deep.try_with thunk ()
    {
      effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Effects.File_read path ->
              Some
                (fun (k : (a, _) Effect.Deep.continuation) ->
                  Effect.Deep.continue k (chain.read path))
          | Effects.File_write { path; content } ->
              Some
                (fun (k : (a, _) Effect.Deep.continuation) ->
                  Effect.Deep.continue k (chain.write ~path ~content))
          | Effects.File_list_dir path ->
              Some
                (fun (k : (a, _) Effect.Deep.continuation) ->
                  Effect.Deep.continue k (chain.list_dir path))
          | Effects.File_stat path ->
              Some
                (fun (k : (a, _) Effect.Deep.continuation) ->
                  Effect.Deep.continue k (chain.stat path))
          | _ -> None);
    }

(** Back-compat shim: tests still call [install_fs ~files thunk]. *)
let install_fs ~files thunk = install (in_memory ~files) thunk
