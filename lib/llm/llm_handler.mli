(** Composable LLM handler chain.

    A handler is a plain function [llm_call_args -> llm_response]. Build
    a chain bottom-up via [|>]:

    {[
      let llm =
        Llm_handler.anthropic ()
        |> Llm_handler.with_validation
        |> Llm_handler.with_cost_tracking ~cost
        |> Llm_handler.with_retry ~policy:Retry_policy.default
        |> Llm_handler.with_logging ~on_log
    ]}

    [install] converts the chain into an effect handler that intercepts
    [Effects.Llm_complete] from any [Agent.run_loop] running underneath. *)

open Types

type t = llm_call_args -> llm_response

module Retry_policy : sig
  type t = {
    max_attempts : int;
    base_delay : float;
    cap : float;
  }

  (** 3 attempts, 1.0s base, 30s cap. *)
  val default : t

  (** 5 attempts, 0.5s base, 60s cap. *)
  val aggressive : t

  (** 0 attempts — disables retry entirely. *)
  val none : t
end

(* ===== Bottom: actual API call ===== *)

(** The leaf handler — calls Anthropic. *)
val anthropic :
  ?base_url:string ->
  ?proxy:string ->
  ?api_key:string ->
  ?model:string ->
  ?max_tokens:int ->
  ?on_text_delta:(string -> unit) ->
  unit ->
  t

(* ===== Middleware ===== *)

(** Validate messages against [Conversation] invariants before sending.
    Raises [Bad_request] if invalid. *)
val with_validation : t -> t

(** Update [cost_state] after each successful call. *)
val with_cost_tracking : cost:cost_state -> t -> t

(** Wrap each LLM call in a [Trace.Llm_call] span. Captures usage
    tokens + per-call cost. Pass [Trace.make_noop ()] to silence. *)
val with_tracing : tracer:Trace.tracer -> model:string -> t -> t

(** Log a one-line summary before each call and after each completion
    (or failure). *)
val with_logging : ?on_log:(string -> unit) -> t -> t

(** Retry transient API errors with capped exponential backoff + jitter,
    honoring [Retry-After]. Non-retryable errors are re-raised
    immediately. [on_retry] is invoked with [(attempt, err, delay)]
    before each sleep. *)
val with_retry :
  ?policy:Retry_policy.t ->
  ?on_retry:(int -> Llm_error.t -> float -> unit) ->
  t ->
  t

(** Emit [Governor.Tick] events around each LLM call (Llm_started /
    Llm_finished). Pair with [Governor.install] to enforce global
    limits (max_steps, max_wall_time, max_cost). *)
val with_governor_ticks : t -> t

(** Catch [Context_window] errors and retry ONCE with a compaction
    strategy applied to the messages. [compactor] summarizes the older
    portion; [keep_recent] preserves the N most-recent messages
    verbatim.

    This is the only structured recovery for [Context_window], which is
    otherwise non-retryable (re-sending the same too-long prompt won't
    help). *)
val with_compaction_on_overflow :
  keep_recent:int ->
  compactor:(message list -> string) ->
  t ->
  t

(** Print the JSON request body to stderr before each call. *)
val with_debug_request : t -> t

(* ===== Install ===== *)

(** Install the chain as an effect handler around [thunk]. Captures
    [Effects.Llm_complete] effects performed inside; other effects pass
    through. *)
val install : t -> (unit -> 'a) -> 'a
