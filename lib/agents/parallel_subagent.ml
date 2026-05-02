(** Run sub-agents in parallel using OCaml 5 [Domain]s.

    Each thunk runs in a FRESH domain with its OWN effect-handler
    scope. The parent's installed handlers are NOT visible inside the
    child — each child must install its own stack (mock LLM in tests,
    real Anthropic chain in production, etc.). This mirrors how
    [Tool_handler.install] / [Llm_handler.install] work but at a
    coarser granularity.

    Sharing policy is the caller's choice:
    - State the caller wants shared (cost_state, log sink): pass a
      [Mutex.t] alongside the data and synchronize manually.
    - State the caller wants isolated (Memory, Conversation, file
      handlers with per-domain backing): build it fresh inside each
      thunk.

    The runtime cost of [Domain.spawn] is non-trivial (~milliseconds);
    use this for tasks where the LLM call dominates, not for cheap
    fan-out. *)

(** Spawn each [thunk] in its own domain, [Domain.join] all of them,
    and return results in input order. Exceptions raised by a child
    thunk propagate when its result is joined. *)
let run (thunks : (unit -> 'a) list) : 'a list =
  match thunks with
  | [] -> []
  | [ single ] -> [ single () ]
  | _ ->
      let domains = List.map (fun thunk -> Domain.spawn thunk) thunks in
      List.map Domain.join domains

(** A Mutex-guarded log sink. Pass the [sink] into each child's
    [Log_handler.to_function]; call [drain ()] from the parent after
    [run] returns to read the captured lines in chronological order
    (best-effort; threads can interleave by line). *)
type shared_sink = {
  sink : string -> unit;
  drain : unit -> string list;
}

let make_shared_sink () : shared_sink =
  let mu = Mutex.create () in
  let buf = ref [] in
  let sink line =
    Mutex.lock mu;
    buf := line :: !buf;
    Mutex.unlock mu
  in
  let drain () =
    Mutex.lock mu;
    let r = List.rev !buf in
    Mutex.unlock mu;
    r
  in
  { sink; drain }

(** Wrap a sink so every line gets a [\[prefix\]] tag. Useful for
    distinguishing per-domain logs in a shared sink: each child wraps
    [shared.sink] with its own prefix. *)
let with_prefix prefix (sink : string -> unit) : string -> unit =
 fun line -> sink (Printf.sprintf "[%s] %s" prefix line)
