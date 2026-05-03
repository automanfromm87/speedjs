(** Cross-cutting governor: global-level fault tolerance.

    Single-call middleware (per-LLM retry, per-tool circuit breaker)
    handles per-call concerns. The Governor handles GLOBAL concerns
    across the whole agent run via observer events.

    The runtime ([Llm_handler] / [Tool_handler] / [Sub_agent]) performs
    [Tick event] effects; this handler observes, increments counters,
    and raises [Governor_aborted] when a configured limit is crossed. *)

open Types

module Event : sig
  type t =
    | Llm_started of { messages : int; tools : int }
    | Llm_finished of { input_tokens : int; output_tokens : int }
    | Iteration_started of { agent : string; iter : int }
        (** Per-agent ReAct loop iteration. Observational only. *)
    | Tool_started of {
        name : string;
        use_id : string;
        input_digest : string;
            (** MD5 hex of input JSON; for death-loop detection
                without holding large payloads. *)
      }
    | Tool_finished of {
        name : string;
        use_id : string;
        ok : bool;
        duration : float;
      }
    | Tool_timeout of { name : string; duration : float; budget : float }
        (** Wall-clock duration exceeded the tool's declared
            [timeout_sec]. Observational. *)
    | Stuck_waiting of { tool : string; pending_for : float }
        (** Reserved for a future watchdog implementation. Not
            currently emitted. *)
    | Subagent_entered
    | Subagent_exited

  val to_string : t -> string
end

module Limits : sig
  type t = {
    max_steps : int option;
    max_wall_time_sec : float option;
    max_cost_usd : float option;
    max_tool_calls : int option;
    max_subagent_depth : int option;
    max_repeated_tool_calls : int option;
  }

  (** All caps disabled. *)
  val none : t

  (** Sensible production defaults: 200 steps, 30 min walltime, 500
      tool calls, 3 levels of nesting, 5 identical-arg repeats. Cost
      cap is opt-in. *)
  val default : t
end

(** The effect runtime components perform to report a lifecycle event. *)
type _ Effect.t += Tick : Event.t -> unit Effect.t

(** Best-effort [Tick] emit: silently drops if no Governor handler is
    installed (typical in unit tests / dev runs without the production
    runtime). Use this from sub-agent entry/exit and similar
    bookkeeping where the sender doesn't want to crash if nobody is
    listening. The production runtime always installs a Governor at
    the outer layer. *)
val safe_tick : Event.t -> unit

(** Raised when a configured limit is crossed. Caught at the top-level
    by [Protection.catch_protection_errors]. *)
exception Governor_aborted of {
  limit : string;
  reason : string;
}

(** Install the Governor effect handler around [thunk]. [clock]
    defaults to [Unix.gettimeofday]; tests inject a mutable-ref clock
    to drive walltime checks deterministically. *)
val install :
  ?limits:Limits.t ->
  ?clock:(unit -> float) ->
  cost:cost_state ->
  ?on_tick:(Event.t -> unit) ->
  (unit -> 'a) ->
  'a
