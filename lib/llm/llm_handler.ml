(** Composable LLM handler chain.

    Each middleware wraps an inner handler with extra behavior; the
    chain is built bottom-up via [|>]:

    {[
      let llm =
        Llm_handler.anthropic ~api_key ()
        |> Llm_handler.with_validation
        |> Llm_handler.with_cost_tracking ~cost
        |> Llm_handler.with_retry ~policy:Retry_policy.default
        |> Llm_handler.with_logging ~on_log
    ]}

    [install] converts the chain into an effect handler that intercepts
    [Effects.Llm_complete] and delegates to the chain.

    Each layer is a plain function transformation, so adding new
    behavior (caching, fallback model, telemetry, ...) is one new
    middleware function. The single [install] at the end is the only
    effect-handler boilerplate. *)

open Types

type t = llm_call_args -> llm_response

(* ===== Retry policy ===== *)

module Retry_policy = struct
  type t = {
    max_attempts : int;
    base_delay : float;
    cap : float;
  }

  let default = { max_attempts = 3; base_delay = 1.0; cap = 30.0 }
  let aggressive = { max_attempts = 5; base_delay = 0.5; cap = 60.0 }
  let none = { max_attempts = 0; base_delay = 0.0; cap = 0.0 }
end

(* ===== Bottom: actual API call ===== *)

(** The "leaf" handler — calls Anthropic via [complete_stream]. Apply
    middleware on top via [|>]. Defaults match the proxy/model routing
    speedjs uses; override per call site as needed. *)
let anthropic ?base_url ?proxy ?api_key ?model ?max_tokens ?on_text_delta
    () : t =
 fun args ->
  Anthropic.complete_stream ?base_url ?proxy ?api_key ?model ?max_tokens
    ?on_text_delta ~system:args.system_override ~messages:args.messages
    ~tools:args.tools ~tool_choice:args.tool_choice ()

(* ===== Middleware ===== *)

(** Validate messages list against [Conversation] invariants before
    sending. Catches malformed message lists (dangling tool_use,
    alternation violations) at the boundary, fail-fast as
    [Bad_request]. *)
let with_validation (inner : t) : t =
 fun args ->
  match Conversation.validate args.messages with
  | Ok () -> inner args
  | Error msg ->
      raise
        (Llm_error.Llm_api_error
           (Llm_error.Bad_request
              (Printf.sprintf
                 "speedjs: malformed messages list (would be rejected by \
                  Anthropic): %s"
                 msg)))

(** Update [cost] with this call's usage on success. Goes through
    [cost_state_add] so parallel sub-agents (which share the parent
    cost_state) don't race when they finish LLM calls concurrently. *)
let with_cost_tracking ~(cost : cost_state) (inner : t) : t =
 fun args ->
  let response = inner args in
  cost_state_add cost ~input_tokens:response.usage.input_tokens
    ~output_tokens:response.usage.output_tokens
    ~cache_creation:response.usage.cache_creation_input_tokens
    ~cache_read:response.usage.cache_read_input_tokens;
  response

(** Wrap each call in a [Trace] span. Captures usage tokens + per-call
    cost into the frame; fails closed if tracer is no-op (writes
    nothing). Must sit OUTSIDE [with_cost_tracking] so it observes the
    same usage that gets folded into [cost_state]. *)
let with_tracing ~(tracer : Trace.tracer) ~(model : string) (inner : t) : t =
 fun args ->
  let input_summary =
    Printf.sprintf "msgs=%d tools=%d" (List.length args.messages)
      (List.length args.tools)
  in
  let capture (response : llm_response) =
    let u = response.usage in
    let tokens : Trace.tokens =
      {
        input = u.input_tokens;
        output = u.output_tokens;
        cache_read = u.cache_read_input_tokens;
        cache_write = u.cache_creation_input_tokens;
      }
    in
    let cost_delta =
      (float_of_int u.input_tokens *. price_input_per_m
      +. float_of_int u.output_tokens *. price_output_per_m
      +. float_of_int u.cache_creation_input_tokens *. price_cache_write_per_m
      +. float_of_int u.cache_read_input_tokens *. price_cache_read_per_m)
      /. 1_000_000.0
    in
    let output =
      Printf.sprintf "stop=%s in=%d out=%d cache_r=%d cache_w=%d"
        (Codec.stop_reason_to_string response.stop_reason)
        u.input_tokens u.output_tokens u.cache_read_input_tokens
        u.cache_creation_input_tokens
    in
    Trace.ok_capture ~output ~tokens ~cost_delta
  in
  Trace.with_span tracer ~kind:Trace.Llm_call ~name:model
    ~input_summary ~capture (fun () -> inner args)

(** Log before/after each call. *)
let with_logging ?(on_log = prerr_endline) (inner : t) : t =
 fun args ->
  on_log
    (Printf.sprintf "[llm] → %d msg(s), %d tool(s)"
       (List.length args.messages) (List.length args.tools));
  match inner args with
  | response ->
      on_log
        (Printf.sprintf "[llm] ← stop=%s, %d in / %d out tokens"
           (Codec.stop_reason_to_string response.stop_reason)
           response.usage.input_tokens response.usage.output_tokens);
      response
  | exception (Llm_error.Llm_api_error err as e) ->
      on_log (Printf.sprintf "[llm] ✗ %s" (Llm_error.pp err));
      raise e

(** Retry transient errors with exponential backoff + jitter, honoring
    [Retry-After] hints from the API.

    Non-retryable errors ([Bad_request], [Auth], [Not_found],
    [Context_window]) are re-raised immediately — retrying won't help
    and burns budget. *)
let with_retry ?(policy = Retry_policy.default)
    ?(on_retry = fun _ _ _ -> ()) (inner : t) : t =
 fun args ->
  let exp_backoff attempt =
    let raw = policy.base_delay *. (2.0 ** float_of_int attempt) in
    if raw > policy.cap then policy.cap else raw
  in
  let compute_delay err attempt =
    let suggested =
      Option.value (Llm_error.retry_after_hint err) ~default:0.0
    in
    let backoff = exp_backoff attempt in
    let max_delay = if suggested > backoff then suggested else backoff in
    Random.float max_delay +. (max_delay /. 2.0)
  in
  let rec attempt n =
    try inner args
    with Llm_error.Llm_api_error err ->
      if (not (Llm_error.is_retryable err)) || n >= policy.max_attempts
      then raise (Llm_error.Llm_api_error err)
      else
        let delay = compute_delay err n in
        on_retry n err delay;
        Unix.sleepf delay;
        attempt (n + 1)
  in
  attempt 0

(** Emit [Governor.Tick] events around each LLM call. Pair with
    [Governor.install] for global limits (max_steps, max_wall_time,
    max_cost) and step-budget enforcement. The middleware is cheap when
    no Governor handler is installed — the perform is unhandled and
    raises (so [Governor.install] should be at the runtime root). *)
let with_governor_ticks (inner : t) : t =
 fun args ->
  Effect.perform
    (Governor.Tick
       (Llm_started
          {
            messages = List.length args.messages;
            tools = List.length args.tools;
          }));
  let response = inner args in
  Effect.perform
    (Governor.Tick
       (Llm_finished
          {
            input_tokens = response.usage.input_tokens;
            output_tokens = response.usage.output_tokens;
          }));
  response

(** Catch [Context_window] errors and retry once with a compaction
    applied to the messages. [compactor] turns the old prefix into a
    summary string; [keep_recent] preserves the N most-recent messages
    verbatim.

    Retries ONCE: if the second call also fails, the error is
    propagated. This is the structured recovery for the one error class
    that can't otherwise be retried (re-sending the same too-long
    prompt won't help). *)
let with_compaction_on_overflow ~keep_recent
    ~(compactor : Types.message list -> string) (inner : t) : t =
 fun args ->
  match inner args with
  | response -> response
  | exception (Llm_error.Llm_api_error (Llm_error.Context_window _)) ->
      (* Reuse [Context.Strategy.compacted] so the compaction shape
         (synthetic [<compacted_history>] User turn) stays single-source.
         Threshold = current length so it's guaranteed to trigger here. *)
      let strategy =
        Context.Strategy.compacted
          ~compact_at:(List.length args.messages - 1)
          ~keep_recent ~compactor
      in
      let compacted = Context.Strategy.apply strategy args.messages in
      inner { args with messages = compacted }

(** Print the request body (pretty JSON) before each call. Wired to
    [SPEEDJS_DEBUG_REQUEST] env var historically — middleware form
    lets each call site decide. *)
let with_debug_request (inner : t) : t =
 fun args ->
  let body =
    Anthropic_req.build_request_body ~model:Anthropic.default_model
      ~system:args.system_override ~messages:args.messages
      ~tools:args.tools ~tool_choice:args.tool_choice ~max_tokens:4096
      ~stream:false
  in
  Printf.eprintf "[debug-request] %s\n%!"
    (Yojson.Safe.pretty_to_string body);
  inner args

(* ===== Install: convert chain to effect handler ===== *)

(** Install the chain as an effect handler. Captures
    [Effects.Llm_complete] in [thunk] and delegates to [chain]. *)
let install (chain : t) thunk =
  let open Effect.Deep in
  try_with thunk ()
    {
      effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Effects.Llm_complete args ->
              Some
                (fun (k : (a, _) continuation) ->
                  match chain args with
                  | response -> continue k response
                  | exception (Llm_error.Llm_api_error _ as e) ->
                      discontinue k e)
          | _ -> None);
    }
