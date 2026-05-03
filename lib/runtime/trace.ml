(** Structured trace capture for observability.

    Every LLM call and tool dispatch becomes a [frame] with start/end
    timestamps, parent-child relationship (from the runtime call stack),
    and cost/token/duration metrics. Frames serialize to NDJSON, one
    line per frame on completion. Default off; enable with
    [--trace-file PATH] in the CLI. *)

type kind =
  | Llm_call             (** One logical LLM call (covers retries). *)
  | Tool_call            (** One logical tool dispatch (covers retries). *)
  | Agent_spawn          (** A delegated sub-agent run. *)
  | Plan_step            (** One [Plan_act.run_for_task] (one task). *)
  | Phase                (** Orchestration boundary: plan_act / planner / recovery / summarizer. *)
  | Iteration            (** One iteration of [Agent.run_loop] — gives clear iter boundaries in the trace. *)
  | Log

let kind_to_string = function
  | Llm_call -> "llm_call"
  | Tool_call -> "tool_call"
  | Agent_spawn -> "agent_spawn"
  | Plan_step -> "plan_step"
  | Phase -> "phase"
  | Iteration -> "iteration"
  | Log -> "log"

type tokens = {
  input : int;
  output : int;
  cache_read : int;
  cache_write : int;
}

let zero_tokens =
  { input = 0; output = 0; cache_read = 0; cache_write = 0 }

type frame = {
  id : string;
  parent_id : string option;
  kind : kind;
  name : string;
  started_at : float;     (* unix seconds *)
  ended_at : float;
  duration_ms : float;
  cost_delta_usd : float;
  tokens : tokens;
  input_summary : string;
  output_summary : string;
  ok : bool;
  error : string option;
}

let truncate s n =
  if String.length s <= n then s
  else String.sub s 0 n ^ "..."

let frame_to_yojson f : Yojson.Safe.t =
  let opt_string = function None -> `Null | Some s -> `String s in
  `Assoc
    [
      "id", `String f.id;
      "parent_id", opt_string f.parent_id;
      "kind", `String (kind_to_string f.kind);
      "name", `String f.name;
      "started_at", `Float f.started_at;
      "ended_at", `Float f.ended_at;
      "duration_ms", `Float f.duration_ms;
      "cost_delta_usd", `Float f.cost_delta_usd;
      ( "tokens",
        `Assoc
          [
            "input", `Int f.tokens.input;
            "output", `Int f.tokens.output;
            "cache_read", `Int f.tokens.cache_read;
            "cache_write", `Int f.tokens.cache_write;
          ] );
      "input_summary", `String f.input_summary;
      "output_summary", `String f.output_summary;
      "ok", `Bool f.ok;
      "error", opt_string f.error;
    ]

(* Frame ids are short hex strings — random enough for one-run uniqueness
   without dragging in a uuid lib. [Random.int]'s bound is < 2^30, so we
   compose two halves to get 56 bits of randomness in 14 hex chars. *)
let gen_id () =
  Printf.sprintf "%07x%07x"
    (Random.int 0x10000000) (Random.int 0x10000000)

(** Tracer state. The stack tracks the current call chain so emitted
    frames can stamp parent_id. [emit] is the persistence sink — for
    [noop] it discards; for [file_writer] it writes one NDJSON line.

    Single-threaded by construction: parallel sub-agents spawn fresh
    runtimes in their own Domain, each with its own tracer. We don't
    propagate parent_id across Domain boundaries in v0.1. *)
type tracer = {
  mutable stack : string list;
  emit : frame -> unit;
}

let make_noop () = { stack = []; emit = (fun _ -> ()) }

(** Domain-local "current tracer" so library code (plan_act, sub_agent)
    can emit spans without threading [tracer] through every signature.
    Each Domain has its own slot; parallel sub-agents need [fork] to
    get a per-Domain tracer that shares the emit sink but has its own
    stack (so concurrent push/pop doesn't race). *)
let current_key : tracer Domain.DLS.key =
  Domain.DLS.new_key (fun () -> { stack = []; emit = (fun _ -> ()) })

let current () = Domain.DLS.get current_key

let set_current t = Domain.DLS.set current_key t

let with_current ~tracer thunk =
  let prev = current () in
  set_current tracer;
  Fun.protect ~finally:(fun () -> set_current prev) thunk

(** Create a sibling tracer that shares [parent]'s emit sink but has
    its own (independent) stack. Optionally seed the stack with one
    parent_id so the first frame emitted in this tracer threads a
    parent reference back to the originating frame in the parent
    Domain. Use this when spawning a new Domain that should appear
    under the call site that spawned it. *)
let fork ?(initial_parent_id = None) ~parent () : tracer =
  let stack = match initial_parent_id with Some id -> [ id ] | None -> [] in
  { stack; emit = parent.emit }

let make_file_writer (path : string) : tracer =
  let oc =
    open_out_gen [ Open_creat; Open_append; Open_wronly ] 0o644 path
  in
  let mutex = Mutex.create () in
  let emit (f : frame) =
    Mutex.lock mutex;
    Fun.protect
      ~finally:(fun () -> Mutex.unlock mutex)
      (fun () ->
        output_string oc (Yojson.Safe.to_string (frame_to_yojson f));
        output_char oc '\n';
        flush oc)
  in
  at_exit (fun () -> try close_out oc with _ -> ());
  { stack = []; emit }

let current_parent t =
  match t.stack with x :: _ -> Some x | [] -> None

let push t id = t.stack <- id :: t.stack

let pop t =
  match t.stack with _ :: rest -> t.stack <- rest | [] -> ()

type capture_result = {
  output : string;
  tokens : tokens;
  cost_delta : float;
  ok : bool;            (** non-exceptional failures (e.g. tool [Error]) *)
  error : string option;
}

let ok_capture ~output ~tokens ~cost_delta =
  { output; tokens; cost_delta; ok = true; error = None }

let fail_capture ~error =
  {
    output = "";
    tokens = zero_tokens;
    cost_delta = 0.0;
    ok = false;
    error = Some error;
  }

(** Run [f ()] inside a span. [capture] inspects [f]'s return value AFTER
    the inner call to extract output / tokens / cost / ok / error. On
    exception, emits a failed frame and re-raises. *)
let with_span (t : tracer) ~(kind : kind) ~(name : string)
    ~(input_summary : string) ~(capture : 'a -> capture_result)
    (f : unit -> 'a) : 'a =
  let id = gen_id () in
  let parent_id = current_parent t in
  let started_at = Unix.gettimeofday () in
  push t id;
  let emit_frame ~ended_at ~ok ~error ~output_summary ~tokens
      ~cost_delta_usd =
    let frame =
      {
        id;
        parent_id;
        kind;
        name;
        started_at;
        ended_at;
        duration_ms = (ended_at -. started_at) *. 1000.0;
        cost_delta_usd;
        tokens;
        input_summary = truncate input_summary 500;
        output_summary = truncate output_summary 500;
        ok;
        error;
      }
    in
    t.emit frame
  in
  match f () with
  | result ->
      pop t;
      let c = capture result in
      emit_frame ~ended_at:(Unix.gettimeofday ()) ~ok:c.ok ~error:c.error
        ~output_summary:c.output ~tokens:c.tokens
        ~cost_delta_usd:c.cost_delta;
      result
  | exception e ->
      pop t;
      emit_frame ~ended_at:(Unix.gettimeofday ()) ~ok:false
        ~error:(Some (Printexc.to_string e)) ~output_summary:""
        ~tokens:zero_tokens ~cost_delta_usd:0.0;
      raise e

(** [with_span] using the current Domain's tracer. *)
let span_current ~kind ~name ~input_summary ~capture f =
  with_span (current ()) ~kind ~name ~input_summary ~capture f
