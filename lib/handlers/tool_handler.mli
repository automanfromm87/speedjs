(** Composable tool dispatch chain.

    Mirrors [Llm_handler]: each middleware wraps an inner handler with
    extra behavior. Build a chain via [|>]:

    {[
      let tool =
        Tool_handler.direct
        |> Tool_handler.with_validation
        |> Tool_handler.with_retry ~policy
        |> Tool_handler.with_circuit_breaker
        |> Tool_handler.with_audit ~on_call ~on_result
        |> Tool_handler.with_logging ~on_log
    ]}

    [install] converts the chain into an effect handler intercepting
    [Effects.Tool_calls]. Batches of 2+ tools run in parallel through
    the chain. *)

open Types

(** Per-call payload. [tool] carries metadata (idempotent, timeout,
    category) used by middleware decisions. *)
type call_args = {
  tool : tool_def;
  input : Yojson.Safe.t;
  use_id : string;
}

type chain_result = (string, Error.t) result

type t = call_args -> chain_result

(* ===== Bottom: actually invoke the tool ===== *)

(** Calls [args.tool.handler args.input] and wraps the string error in
    [Error.t] (Permanent by default — middleware can reclassify). *)
val direct : t

(* ===== Middleware ===== *)

(** Reject inputs that aren't JSON objects; raises [Validation] error. *)
val with_validation : t -> t

(** Retry [Transient] failures, but ONLY when [tool.idempotent = true].
    Honors [retry_after] hints, capped exp backoff with jitter. *)
val with_retry :
  ?policy:Llm_handler.Retry_policy.t ->
  ?on_retry:(int -> Error.t -> float -> unit) ->
  t ->
  t

(** Per-tool wall-clock timeout from [tool.timeout_sec] metadata
    (fallback [default_sec]). On timeout returns [Transient] error;
    the worker thread is abandoned (OCaml has no safe thread cancel).
    Last-resort cap — prefer cooperative timeouts inside tools when
    possible. *)
val with_timeout :
  ?default_sec:float -> ?poll_interval:float -> t -> t

(** Per-tool circuit breaker. After [failure_threshold] consecutive
    failures, the breaker opens for [cooldown] seconds (subsequent
    calls fail fast with a [Transient] [circuit_open] error). One
    success closes the breaker. *)
val with_circuit_breaker :
  ?failure_threshold:int -> ?cooldown:float -> t -> t

(** Telemetry / security-review hooks. *)
val with_audit :
  ?on_call:(call_args -> unit) ->
  ?on_result:(call_args -> chain_result -> unit) ->
  t ->
  t

(** Compact one-line in/out log per call, including category. *)
val with_logging : ?on_log:(string -> unit) -> t -> t

(* ===== Install ===== *)

(** Install the chain as an effect handler around [thunk]. Catches
    [Effects.Tool_calls]; single-tool calls re-install the handler
    around invocation so [delegate]-style tools can perform their own
    effects through the outer stack.

    Emits [Governor.Tick] events ([Tool_started] / [Tool_finished] /
    [Tool_timeout]) on the main fiber before/after dispatch — worker
    threads can't perform effects, so tick emission can't live inside a
    chain middleware. *)
val install : tools:tool_def list -> t -> (unit -> 'a) -> 'a

(* ===== Building blocks (also reused by [Checkpoint]) ===== *)

(** Apply [chain] to a single use, returning the legacy
    [(use_id, tool_handler_result)] pair the [Tool_calls] effect expects.
    [Error.t] is converted to its pretty-printed string at this boundary. *)
val dispatch_one :
  chain:t ->
  tools:tool_def list ->
  string * string * Yojson.Safe.t ->
  string * tool_handler_result

(** Apply tool-result truncation. [load_skill] is exempt — a partial
    skill body is worse than no skill at all. *)
val truncate_result : string -> tool_handler_result -> tool_handler_result

(** Perform [Governor.Tick (Tool_started ...)] on the current fiber. Must
    be called from a fiber that has [Governor.install] in scope (i.e. the
    main fiber) — used by tool installers to emit ticks before/after the
    parallel-thread dispatch boundary. *)
val perform_tool_started_tick :
  name:string -> use_id:string -> input:Yojson.Safe.t -> unit

(** Perform [Governor.Tick (Tool_finished ...)] and, if [duration]
    exceeds [tool.timeout_sec], also emit [Tool_timeout]. Must run on
    the main fiber for the same reason as [perform_tool_started_tick]. *)
val perform_tool_finished_ticks :
  tool:tool_def -> use_id:string -> ok:bool -> duration:float -> unit
