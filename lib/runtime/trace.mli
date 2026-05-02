(** Structured trace capture. See [trace.ml] for full semantics. *)

type kind =
  | Llm_call
  | Tool_call
  | Agent_spawn          (** reserved *)
  | Plan_step            (** reserved *)
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
    [at_exit]. Mutex-protected for safe use across threads (Domains
    install their own tracer; this guard is for incidental Thread use). *)

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
