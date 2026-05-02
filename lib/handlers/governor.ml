(** Cross-cutting governor: global-level fault tolerance.

    Single-call middleware ([Llm_handler.with_retry], [Tool_handler.with_retry],
    [with_circuit_breaker]) handles per-call concerns. The governor
    handles GLOBAL concerns across the whole agent run:

    - **Step budget**: total LLM calls across the run
    - **Wall time**: real seconds since start
    - **Cost**: cumulative USD
    - **Tool budget**: total tool invocations
    - **Sub-agent depth**: max nesting of [delegate] calls
    - **Death loops**: same tool with identical args repeated N times

    Implementation: every component (LLM chain, tool chain, sub-agent)
    performs [Tick event]; this module's effect handler observes,
    increments counters, and raises [Governor_aborted] when a limit is
    crossed. The top-level catch ([Protection.catch_protection_errors])
    converts to a typed [agent_error]. *)

open Types

(* ===== Events the runtime reports ===== *)

module Event = struct
  type t =
    | Llm_started of { messages : int; tools : int }
    | Llm_finished of { input_tokens : int; output_tokens : int }
    | Iteration_started of { agent : string; iter : int }
        (** Each ReAct loop iteration. Currently observational (no
            governor-enforced cap; per-agent [max_iterations] already
            caps the inner loop). Useful for telemetry / on_tick logs. *)
    | Tool_started of {
        name : string;
        use_id : string;
        input_digest : string;
            (** MD5 hex of input JSON; used for death-loop detection
                without holding large payloads. *)
      }
    | Tool_finished of {
        name : string;
        use_id : string;
        ok : bool;
        duration : float;
      }
    | Tool_timeout of {
        name : string;
        duration : float;
        budget : float;
      }
        (** Fired when a tool's measured wall-clock duration exceeded
            its declared [tool.timeout_sec]. The tool may still have
            returned a value — this is an observation event, not an
            enforcement signal. *)
    | Stuck_waiting of { tool : string; pending_for : float }
        (** Reserved: [Tool_started] without a matching [Tool_finished]
            for [pending_for] seconds. Detection requires a watchdog
            thread; not currently emitted, declared here for forward
            compat. *)
    | Subagent_entered
    | Subagent_exited

  let to_string = function
    | Llm_started { messages; tools } ->
        Printf.sprintf "Llm_started(msgs=%d,tools=%d)" messages tools
    | Llm_finished { input_tokens; output_tokens } ->
        Printf.sprintf "Llm_finished(in=%d,out=%d)" input_tokens
          output_tokens
    | Iteration_started { agent; iter } ->
        Printf.sprintf "Iteration_started(%s,%d)" agent iter
    | Tool_started { name; _ } -> Printf.sprintf "Tool_started(%s)" name
    | Tool_finished { name; ok; duration; _ } ->
        Printf.sprintf "Tool_finished(%s,ok=%b,%.2fs)" name ok duration
    | Tool_timeout { name; duration; budget } ->
        Printf.sprintf "Tool_timeout(%s,%.2fs > %.2fs)" name duration
          budget
    | Stuck_waiting { tool; pending_for } ->
        Printf.sprintf "Stuck_waiting(%s,%.1fs)" tool pending_for
    | Subagent_entered -> "Subagent_entered"
    | Subagent_exited -> "Subagent_exited"
end

(* ===== Configurable limits ===== *)

module Limits = struct
  type t = {
    max_steps : int option;
        (** Total LLM calls across the run. *)
    max_wall_time_sec : float option;
        (** Real seconds since the [install] call. *)
    max_cost_usd : float option;
    max_tool_calls : int option;
        (** Total tool invocations across the run. *)
    max_subagent_depth : int option;
        (** Max nesting of [delegate]-spawned sub-agents. *)
    max_repeated_tool_calls : int option;
        (** Same [(tool_name, input_digest)] this many times in a row =
            death loop. *)
  }

  let none =
    {
      max_steps = None;
      max_wall_time_sec = None;
      max_cost_usd = None;
      max_tool_calls = None;
      max_subagent_depth = None;
      max_repeated_tool_calls = None;
    }

  let default =
    {
      max_steps = Some 200;
      max_wall_time_sec = Some 1800.0;
      max_cost_usd = None;
      max_tool_calls = Some 500;
      max_subagent_depth = Some 3;
      max_repeated_tool_calls = Some 5;
    }
end

(* ===== Effect + abort exception ===== *)

type _ Effect.t += Tick : Event.t -> unit Effect.t

exception Governor_aborted of {
  limit : string;
  reason : string;
}

(* ===== Internal state ===== *)

type state = {
  clock : unit -> float;
  cost : cost_state;
      (** All cumulative counters — steps / tool_calls / tokens /
          subagent_depth / start_time — live HERE, shared across all
          Domains via [cost_state]'s mutex. [max_steps] /
          [max_tool_calls] / [max_cost_usd] / [max_wall_time_sec] /
          [max_subagent_depth] are therefore GLOBAL caps. *)
  recent_tool_sigs : string Queue.t;
      (** Death-loop detection is per-Domain: each child's repetition
          is detected on its own. Cross-Domain death-loops are a
          different signal we don't model yet. *)
}

let make_state ~cost ~clock =
  (* Anchor the wall-clock origin lazily and ONLY at the outermost
     Governor — the first one whose [cost_state.start_time] is still
     the sentinel 0.0. Children inherit [cost] including the now-
     anchored start_time, so their walltime check measures age-since-
     RUN-start rather than age-since-Domain-spawn. *)
  if cost.start_time = 0.0 then begin
    Mutex.lock cost.mu;
    if cost.start_time = 0.0 then cost.start_time <- clock ();
    Mutex.unlock cost.mu
  end;
  {
    clock;
    cost;
    recent_tool_sigs = Queue.create ();
  }

let abort ~limit ~reason =
  raise (Governor_aborted { limit; reason })

(* ===== Limit checks ===== *)

(* All read paths take a single locked snapshot per check so the
   numbers reported are consistent — multi-field arithmetic (cost_usd
   over 4 token counters) won't see a torn write under concurrent
   Domain mutators. *)
let check_steps state limits =
  match limits.Limits.max_steps with
  | None -> ()
  | Some m ->
      let s = Types.cost_state_snapshot state.cost in
      if s.s_steps > m then
        abort ~limit:"max_steps"
          ~reason:(Printf.sprintf "%d steps exceeded cap %d" s.s_steps m)

let check_wall_time state limits =
  match limits.Limits.max_wall_time_sec with
  | None -> ()
  | Some m ->
      let s = Types.cost_state_snapshot state.cost in
      let elapsed = state.clock () -. s.s_start_time in
      if elapsed > m then
        abort ~limit:"max_wall_time"
          ~reason:
            (Printf.sprintf "%.1fs elapsed exceeded cap %.0fs" elapsed m)

let check_cost state limits =
  match limits.Limits.max_cost_usd with
  | None -> ()
  | Some m ->
      let s = Types.cost_state_snapshot state.cost in
      let used =
        (float_of_int s.s_input_tokens *. Types.price_input_per_m
        +. float_of_int s.s_output_tokens *. Types.price_output_per_m
        +. float_of_int s.s_cache_creation *. Types.price_cache_write_per_m
        +. float_of_int s.s_cache_read *. Types.price_cache_read_per_m)
        /. 1_000_000.0
      in
      if used > m then
        abort ~limit:"max_cost"
          ~reason:
            (Printf.sprintf "$%.4f used exceeded cap $%.4f" used m)

let check_tool_calls state limits =
  match limits.Limits.max_tool_calls with
  | None -> ()
  | Some m ->
      let s = Types.cost_state_snapshot state.cost in
      if s.s_tool_calls > m then
        abort ~limit:"max_tool_calls"
          ~reason:
            (Printf.sprintf "%d tool calls exceeded cap %d"
               s.s_tool_calls m)

let check_depth state limits =
  match limits.Limits.max_subagent_depth with
  | None -> ()
  | Some m ->
      let s = Types.cost_state_snapshot state.cost in
      if s.s_subagent_depth > m then
        abort ~limit:"max_subagent_depth"
          ~reason:
            (Printf.sprintf "sub-agent depth %d exceeded cap %d"
               s.s_subagent_depth m)

(** Death-loop detection: track the most recent N tool signatures; if
    the queue is full AND every entry is identical, abort. *)
let check_repeated state limits sig_ =
  match limits.Limits.max_repeated_tool_calls with
  | None -> ()
  | Some m ->
      Queue.push sig_ state.recent_tool_sigs;
      while Queue.length state.recent_tool_sigs > m do
        ignore (Queue.pop state.recent_tool_sigs)
      done;
      if Queue.length state.recent_tool_sigs >= m then
        let all_same =
          let first = Queue.peek state.recent_tool_sigs in
          Queue.fold (fun acc s -> acc && s = first) true
            state.recent_tool_sigs
        in
        if all_same then
          abort ~limit:"max_repeated_tool_calls"
            ~reason:
              (Printf.sprintf
                 "tool signature '%s' repeated %d times with identical args"
                 sig_ m)

(* ===== Observer: dispatch on event type ===== *)

let observe state limits event =
  match event with
  | Event.Llm_started _ ->
      cost_state_inc_step state.cost;
      check_steps state limits;
      check_wall_time state limits;
      check_cost state limits
  | Event.Llm_finished _ ->
      (* [Llm_handler.with_cost_tracking] updates cost strictly before
         this handler sees the Tick, so re-check the cost cap after a
         call too. *)
      check_cost state limits
  | Event.Iteration_started _ ->
      (* Pure observation — per-agent [max_iterations] already caps
         the inner loop. Wall-time check piggy-backs cheaply. *)
      check_wall_time state limits
  | Event.Tool_started { name; input_digest; _ } ->
      cost_state_inc_tool_call state.cost;
      check_tool_calls state limits;
      check_repeated state limits (name ^ ":" ^ input_digest)
  | Event.Tool_finished _ -> ()
  | Event.Tool_timeout _ ->
      (* Observation only. The tool itself decided to bail (or measured
         out of its declared timeout). Useful via [on_tick] for warnings. *)
      ()
  | Event.Stuck_waiting _ ->
      (* Reserved; nothing to enforce yet. Caller's [on_tick] can
         decide on action. *)
      ()
  | Event.Subagent_entered ->
      Types.cost_state_inc_depth state.cost;
      check_depth state limits
  | Event.Subagent_exited ->
      Types.cost_state_dec_depth state.cost

(** Install the Governor effect handler. Catches [Tick] events from the
    inner handlers / agents, observes them against [limits], and raises
    [Governor_aborted] when a cap is crossed.

    [on_tick] is an optional spy hook (e.g. for telemetry / tests). *)
let install ?(limits = Limits.default) ?(clock = Unix.gettimeofday) ~cost
    ?(on_tick = ignore) f =
  let state = make_state ~cost ~clock in
  let open Effect.Deep in
  try_with f ()
    {
      effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Tick event ->
              Some
                (fun (k : (a, _) continuation) ->
                  on_tick event;
                  observe state limits event;
                  continue k ())
          | _ -> None);
    }
