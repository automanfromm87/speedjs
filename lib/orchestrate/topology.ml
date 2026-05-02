(** Pluggable agent topology executor.

    See [topology.mli] for design rationale. The internal trick:
    [direct] performs [Topo_run] for sub-steps (not direct calls), so
    middleware installed via [install] sees every level. To make this
    work, [install] re-installs itself recursively in each effc — each
    sub-step gets its own fresh try_with that catches its own
    sub-sub-step performs. *)

type 'state node = 'state -> 'state

type 'state shape =
  | Node of { name : string; run : 'state node }
  | Sequence of 'state shape list
  | Parallel of {
      merge : 'state -> 'state -> 'state;
      branches : 'state shape list;
    }
  | Loop_until of {
      cond : 'state -> bool;
      body : 'state shape;
      max_iters : int option;
    }
  | Branch of ('state -> 'state shape)

type _ Effect.t += Topo_run : 'state * 'state shape -> 'state Effect.t

type runner = { run : 'a. 'a -> 'a shape -> 'a }

let shape_name : type s. s shape -> string = function
  | Node { name; _ } -> Printf.sprintf "Node(%s)" name
  | Sequence steps -> Printf.sprintf "Sequence[%d]" (List.length steps)
  | Parallel { branches; _ } ->
      Printf.sprintf "Parallel[%d]" (List.length branches)
  | Loop_until _ -> "Loop_until"
  | Branch _ -> "Branch"

(* ===== direct runner =====
   Each sub-step goes through [Effect.perform (Topo_run ...)] so the
   handler chain sees every level. Direct recursion would bypass
   middleware. *)
let direct : runner =
  let run : type s. s -> s shape -> s =
   fun state shape ->
    match shape with
    | Node { run; _ } -> run state
    | Sequence steps ->
        List.fold_left
          (fun s step -> Effect.perform (Topo_run (s, step)))
          state steps
    | Parallel { merge; branches } ->
        (* Synchronous fold; Domain-based version is a future drop-in. *)
        let initial = state in
        let results =
          List.map (fun b -> Effect.perform (Topo_run (initial, b))) branches
        in
        List.fold_left merge initial results
    | Loop_until { cond; body; max_iters } ->
        let rec loop st n =
          if cond st then st
          else
            match max_iters with
            | Some m when n >= m -> st
            | _ -> loop (Effect.perform (Topo_run (st, body))) (n + 1)
        in
        loop state 0
    | Branch chooser ->
        Effect.perform (Topo_run (state, chooser state))
  in
  { run }

(* ===== Middleware ===== *)

let with_logging ~(on_event : string -> unit) (inner : runner) : runner =
  let run : type s. s -> s shape -> s =
   fun state shape ->
    let name = shape_name shape in
    on_event (Printf.sprintf "→ %s" name);
    let result =
      try inner.run state shape
      with e ->
        on_event (Printf.sprintf "✗ %s — %s" name (Printexc.to_string e));
        raise e
    in
    on_event (Printf.sprintf "← %s" name);
    result
  in
  { run }

let with_retry ?(max_attempts = 3) ?(base_delay = 0.1) ?(cap = 5.0)
    (inner : runner) : runner =
  let run : type s. s -> s shape -> s =
   fun state shape ->
    let rec attempt n =
      try inner.run state shape
      with e when n + 1 < max_attempts ->
        let delay = min cap (base_delay *. (2.0 ** float_of_int n)) in
        Unix.sleepf delay;
        ignore e;
        attempt (n + 1)
    in
    attempt 0
  in
  { run }

(* ===== install =====
   Recursive: every Topo_run effect is handled by re-running install
   on chain.run. This way each sub-step gets its own try_with that
   catches its own sub-sub-steps. Without this, only the top-level
   perform would see the chain — sub-steps would escape past install
   and become Unhandled. *)

let rec install : type r. runner -> (unit -> r) -> r =
 fun chain thunk ->
  Effect.Deep.try_with thunk ()
    {
      effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Topo_run (state, shape) ->
              Some
                (fun (k : (a, _) Effect.Deep.continuation) ->
                  let result : a =
                    install chain (fun () -> chain.run state shape)
                  in
                  Effect.Deep.continue k result)
          | _ -> None);
    }

(* ===== User entry ===== *)

let run_with chain shape state =
  install chain (fun () -> Effect.perform (Topo_run (state, shape)))

let run shape state = run_with direct shape state
