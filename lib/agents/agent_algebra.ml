(** Agent algebra — see [.mli]. *)

open Types

(* ===== ADT ===== *)

(* Pure record; combinators rebuild. Mutability would defeat
   composability. *)
type leaf_config = {
  tools : tool_def list;
  system_prompt : string option;
  system_blocks : (string * string) list;
  max_iters : int option;
}

type t =
  | Leaf of leaf_config
  | With_retry of { max_attempts : int; inner : t }
  | With_max_iters of { max : int; inner : t }
  | With_skill of { skill_name : string; inner : t }
  | Replicate of { n : int; inner : t }
  | Pipe of t * t

(* ===== Constructors ===== *)

let base ?(tools = []) ?system_prompt ?(system_blocks = []) ?max_iters () =
  Leaf { tools; system_prompt; system_blocks; max_iters }

let with_retry ?(max_attempts = 3) inner =
  With_retry { max_attempts; inner }

let with_max_iters max inner = With_max_iters { max; inner }

let with_skill skill_name inner = With_skill { skill_name; inner }

let replicate n inner =
  if n < 1 then invalid_arg "replicate: n must be >= 1";
  Replicate { n; inner }

let pipe a b = Pipe (a, b)

(* ===== Skill lookup =====
   The runtime config doesn't currently expose loaded skills. For v0
   we read them from a DLS slot the caller is expected to populate
   before calling [execute]. Default is the empty list (skill lookup
   then fails with a clear error). *)

let skills_key : Skill.t list Domain.DLS.key =
  Domain.DLS.new_key (fun () -> [])

let with_skills skills thunk =
  let prev = Domain.DLS.get skills_key in
  Domain.DLS.set skills_key skills;
  Fun.protect ~finally:(fun () -> Domain.DLS.set skills_key prev) thunk

let lookup_skill name =
  match List.find_opt (fun (s : Skill.t) -> s.name = name)
          (Domain.DLS.get skills_key) with
  | Some s -> s
  | None ->
      failwith
        (Printf.sprintf
           "Agent_algebra.with_skill: skill %S not found. Did you wrap \
            execute in [with_skills loaded_skills]?"
           name)

(* ===== Pretty printer ===== *)

let rec show ?(indent = 0) (a : t) : string =
  let pad = String.make indent ' ' in
  match a with
  | Leaf c ->
      let n_tools = List.length c.tools in
      let iters =
        match c.max_iters with
        | Some n -> Printf.sprintf "max_iters=%d " n
        | None -> ""
      in
      Printf.sprintf "%sleaf(%stools=%d, blocks=%d)" pad iters n_tools
        (List.length c.system_blocks)
  | With_retry { max_attempts; inner } ->
      Printf.sprintf "%swith_retry(max=%d):\n%s" pad max_attempts
        (show ~indent:(indent + 2) inner)
  | With_max_iters { max; inner } ->
      Printf.sprintf "%swith_max_iters(%d):\n%s" pad max
        (show ~indent:(indent + 2) inner)
  | With_skill { skill_name; inner } ->
      Printf.sprintf "%swith_skill(%S):\n%s" pad skill_name
        (show ~indent:(indent + 2) inner)
  | Replicate { n; inner } ->
      Printf.sprintf "%sreplicate(n=%d):\n%s" pad n
        (show ~indent:(indent + 2) inner)
  | Pipe (a, b) ->
      Printf.sprintf "%spipe:\n%s\n%s" pad
        (show ~indent:(indent + 2) a)
        (show ~indent:(indent + 2) b)

let show t = show ~indent:0 t

(* ===== Interpreter ===== *)

(* Push a new system_block at the front of a leaf's existing blocks.
   Other nodes recurse. *)
let rec inject_block ~name ~body : t -> t = function
  | Leaf c ->
      Leaf { c with system_blocks = (name, body) :: c.system_blocks }
  | With_retry r -> With_retry { r with inner = inject_block ~name ~body r.inner }
  | With_max_iters r ->
      With_max_iters { r with inner = inject_block ~name ~body r.inner }
  | With_skill r ->
      With_skill { r with inner = inject_block ~name ~body r.inner }
  | Replicate r -> Replicate { r with inner = inject_block ~name ~body r.inner }
  | Pipe (a, b) -> Pipe (inject_block ~name ~body a, inject_block ~name ~body b)

let rec override_max_iters n : t -> t = function
  | Leaf c -> Leaf { c with max_iters = Some n }
  | With_retry r ->
      With_retry { r with inner = override_max_iters n r.inner }
  | With_max_iters r when r.max <= n -> With_max_iters r  (* outer is tighter *)
  | With_max_iters r ->
      With_max_iters { r with inner = override_max_iters n r.inner }
  | With_skill r -> With_skill { r with inner = override_max_iters n r.inner }
  | Replicate r -> Replicate { r with inner = override_max_iters n r.inner }
  | Pipe (a, b) -> Pipe (override_max_iters n a, override_max_iters n b)

let rec execute (a : t) (input : string) : agent_result =
  match a with
  | Leaf c ->
      Agent.run
        ?max_iterations:c.max_iters
        ?system_prompt:c.system_prompt
        ~system_blocks:c.system_blocks
        ~user_query:input
        ~tools:c.tools
        ()
  | With_retry { max_attempts; inner } ->
      let rec attempt n =
        match execute inner input with
        | Ok x -> Ok x
        | Error _ when n < max_attempts -> attempt (n + 1)
        | Error e -> Error e
      in
      attempt 1
  | With_max_iters { max; inner } ->
      execute (override_max_iters max inner) input
  | With_skill { skill_name; inner } ->
      let s = lookup_skill skill_name in
      execute (inject_block ~name:s.name ~body:s.body inner) input
  | Replicate { n; inner } ->
      (* v0: sequential. v1 will route through an effect that the
         runtime handles via [Setup.build_child_stack] for true
         per-Domain parallelism with shared cost_state + parent
         governor inheritance — same wiring as parallel_delegate. *)
      let rec run i acc =
        if i >= n then Ok (String.concat "\n\n---\n\n" (List.rev acc))
        else
          match execute inner input with
          | Ok s -> run (i + 1) (Printf.sprintf "=== run:%d ===\n%s" i s :: acc)
          | Error e -> Error e
      in
      run 0 []
  | Pipe (a, b) -> (
      match execute a input with
      | Ok mid -> execute b mid
      | Error _ as e -> e)
