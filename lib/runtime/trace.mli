(** Structured trace capture. See [trace.ml] for full semantics. *)

type kind =
  | Llm_call
  | Tool_call
  | Agent_spawn         (** Delegated sub-agent run. *)
  | Plan_step           (** [Plan_act.run_for_task] for one plan task. *)
  | Phase               (** Orchestration: plan_act_run / planner / recovery / summarizer. *)
  | Iteration           (** One [Agent.run_loop] iteration. *)
  | Log

type tokens = {
  input : int;
  output : int;
  cache_read : int;
  cache_write : int;
}

val zero_tokens : tokens

type frame = {
  id : string;
  parent_id : string option;
  kind : kind;
  name : string;
  started_at : float;
  ended_at : float;
  duration_ms : float;
  cost_delta_usd : float;
  tokens : tokens;
  input_summary : string;
  output_summary : string;
  ok : bool;
  error : string option;
}

(** Opaque tracer handle. Holds the current frame stack and the persistence
    sink. Pass through [Runtime.config]. *)
type tracer

val make_noop : unit -> tracer
(** No-op tracer — accepts every event but discards. Default. *)

val make_file_writer : string -> tracer
(** Append-mode NDJSON writer. One line per emitted frame. Closed at
    [at_exit]. Mutex-protected so Domains and threads can share the
    same emit sink safely. *)

val current : unit -> tracer
(** The tracer currently installed in this Domain (via [with_current]).
    Defaults to [make_noop ()] when none has been installed. *)

val with_current : tracer:tracer -> (unit -> 'a) -> 'a
(** Install [tracer] as the current tracer for this Domain for the
    duration of the thunk. Restored on return / exception. *)

val current_parent : tracer -> string option
(** Top of [tracer]'s stack, or [None] if empty. Useful when spawning
    a new Domain to seed its tracer with the parent frame's id via
    [fork ~initial_parent_id]. *)

val fork :
  ?initial_parent_id:string option ->
  parent:tracer ->
  unit ->
  tracer
(** Create a sibling tracer sharing [parent]'s emit sink but with its
    own stack. Optionally seed the stack with [initial_parent_id] so
    the first frame in the fork has [parent_id] pointing back to the
    originating frame in the parent Domain. *)

type capture_result = {
  output : string;
  tokens : tokens;
  cost_delta : float;
  ok : bool;            (** non-exceptional failures (e.g. tool [Error]) *)
  error : string option;
}

val ok_capture :
  output:string ->
  tokens:tokens ->
  cost_delta:float ->
  capture_result
(** Convenience builder for the success path. *)

val fail_capture : error:string -> capture_result
(** Convenience builder for the failure path. [output = ""],
    [tokens = zero_tokens], [cost_delta = 0.0], [ok = false],
    [error = Some error]. *)

val with_span :
  tracer ->
  kind:kind ->
  name:string ->
  input_summary:string ->
  capture:('a -> capture_result) ->
  (unit -> 'a) ->
  'a
(** Runs [f ()] inside a span, stamping [parent_id] from the current
    call stack. [capture] runs AFTER [f] returns to extract the frame's
    metrics from the result. On exception, emits a failed frame and
    re-raises. *)

val span_current :
  kind:kind ->
  name:string ->
  input_summary:string ->
  capture:('a -> capture_result) ->
  (unit -> 'a) ->
  'a
(** [with_span] applied to the Domain's [current ()] tracer. *)
