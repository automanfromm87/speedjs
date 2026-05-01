(** Composable tool dispatch chain.

    Mirrors the [Llm_handler] design: each middleware wraps an inner
    handler with extra behavior (validation, retry, circuit breaker,
    audit). Build a chain bottom-up via [|>]:

    {[
      let tool =
        Tool_handler.direct
        |> Tool_handler.with_validation
        |> Tool_handler.with_retry ~policy:default_policy
        |> Tool_handler.with_circuit_breaker ~failure_threshold:3
        |> Tool_handler.with_audit ~on_call ~on_result
    ]}

    [install] converts the chain into an effect handler that intercepts
    [Effects.Tool_calls]. The chain operates on a single tool call
    ([call_args]); batched calls are dispatched in parallel through it.

    Errors flow through the chain as the unified [Error.t] type, so
    middleware can branch on [Transient] vs [Permanent] semantics. *)

open Types

(** Per-call payload. Carries the tool definition (with metadata) plus
    the LLM-supplied input and the [tool_use_id] for correlation. *)
type call_args = {
  tool : tool_def;
  input : Yojson.Safe.t;
  use_id : Id.Tool_use_id.t;
}

(** Chain result. Inside the chain, errors are typed [Error.t] for
    semantic branching; [install] converts to plain string errors
    at the effect-handler boundary so the wire signature stays
    backward-compatible. *)
type chain_result = (string, Error.t) result

type t = call_args -> chain_result

(* ===== Bottom: invoke the tool's own handler, classify errors ===== *)

let direct : t =
 fun args ->
  match args.tool.handler args.input with
  | Ok s -> Ok s
  | Error msg ->
      (* Default classification: tool errors are Permanent unless a
         middleware reclassifies (e.g. [http_get] could promote
         "connection refused" to Transient). *)
      Error
        (Error.permanent ~domain:(Tool args.tool.name)
           ~code:"tool_error" msg)

(* ===== Middleware: validation ===== *)

(** Validate that input is a JSON object (Anthropic always sends
    objects when input_schema declares object type). Tools rely on
    [with_object_input] internally; this catches malformed inputs at
    the boundary with a clearer error. Schema-aware validation could
    extend this to check required fields. *)
let with_validation (inner : t) : t =
 fun args ->
  match args.input with
  | `Assoc _ -> inner args
  | _ ->
      Error
        (Error.make ~kind:Permanent ~domain:Validation
           ~code:"invalid_input"
           (Printf.sprintf
              "tool '%s' input must be a JSON object, got: %s"
              args.tool.name (Yojson.Safe.to_string args.input)))

(* ===== Middleware: retry ===== *)

(** Retry transient failures, but ONLY for idempotent tools (per
    [tool.idempotent] metadata). Non-idempotent tools are NEVER
    retried — re-running [bash 'rm -rf'] or [write_file] would
    duplicate side effects. *)
let with_retry ?(policy = Llm_handler.Retry_policy.default)
    ?(on_retry = fun _ _ _ -> ()) (inner : t) : t =
 fun args ->
  if not args.tool.idempotent then inner args
  else
    let exp_backoff attempt =
      let raw = policy.base_delay *. (2.0 ** float_of_int attempt) in
      if raw > policy.cap then policy.cap else raw
    in
    let compute_delay (err : Error.t) attempt =
      let suggested = Option.value err.retry_after ~default:0.0 in
      let backoff = exp_backoff attempt in
      let max_delay =
        if suggested > backoff then suggested else backoff
      in
      Random.float max_delay +. (max_delay /. 2.0)
    in
    let rec attempt n =
      match inner args with
      | Ok _ as r -> r
      | Error err when not (Error.is_retryable err) ->
          Error err  (* permanent — fail fast *)
      | Error err when n >= policy.max_attempts -> Error err
      | Error err ->
          let delay = compute_delay err n in
          on_retry n err delay;
          Unix.sleepf delay;
          attempt (n + 1)
    in
    attempt 0

(* ===== Middleware: circuit breaker ===== *)

type breaker_state = {
  mutable failures : int;
  mutable open_until : float;  (** [0.0] = closed *)
}

(** Per-tool wall-clock timeout. Reads [args.tool.timeout_sec] (falls
    back to [default_sec] when unset). Runs [inner args] in a worker
    thread; the calling thread polls a result-ref with a deadline. On
    timeout returns a [Transient] error.

    OCaml caveat: there's no safe way to interrupt a running thread, so
    on timeout the worker is ABANDONED — it continues until natural
    completion (or process exit). Memory grows during a hung-tool run
    but the agent advances. For tools that hold OS resources (file
    handles, sockets), prefer adding cooperative timeouts in the tool
    itself ([Tools.bash] does this via [run_with_timeout]; [Mcp] does
    via per-connection [read_timeout_sec]); the middleware is a
    last-resort cap. *)
let with_timeout ?(default_sec = 60.0) ?(poll_interval = 0.05)
    (inner : t) : t =
 fun args ->
  let timeout = Option.value args.tool.timeout_sec ~default:default_sec in
  let result_ref : chain_result option ref = ref None in
  let mu = Mutex.create () in
  let _ : Thread.t =
    Thread.create
      (fun () ->
        let r = try inner args with e ->
          Error
            (Error.permanent ~domain:(Tool args.tool.name)
               ~code:"thread_exn" (Printexc.to_string e))
        in
        Mutex.lock mu;
        result_ref := Some r;
        Mutex.unlock mu)
      ()
  in
  let deadline = Unix.gettimeofday () +. timeout in
  let rec wait () =
    Mutex.lock mu;
    match !result_ref with
    | Some r ->
        Mutex.unlock mu;
        r
    | None ->
        Mutex.unlock mu;
        let now = Unix.gettimeofday () in
        if now >= deadline then
          Error
            (Error.transient ~domain:(Tool args.tool.name) ~code:"timeout"
               (Printf.sprintf
                  "tool '%s' exceeded %.1fs wall-clock budget; worker thread abandoned"
                  args.tool.name timeout))
        else begin
          Unix.sleepf (min poll_interval (deadline -. now));
          wait ()
        end
  in
  wait ()

(** Per-tool circuit breaker: after [failure_threshold] consecutive
    failures, the breaker opens and subsequent calls fail fast for
    [cooldown] seconds. One success closes the breaker.

    Thread-safe: the breaker map is accessed under a mutex because
    [Tool_handler.install] dispatches batches through [Parallel.map_threaded]
    — concurrent threads would otherwise race on [Hashtbl] internals
    (which are not domain-safe in OCaml 5). *)
let with_circuit_breaker ?(failure_threshold = 5) ?(cooldown = 60.0)
    (inner : t) : t =
  let breakers : (string, breaker_state) Hashtbl.t =
    Hashtbl.create 16
  in
  let mu = Mutex.create () in
  let with_lock f =
    Mutex.lock mu;
    Fun.protect ~finally:(fun () -> Mutex.unlock mu) f
  in
  let get_breaker name =
    with_lock (fun () ->
        match Hashtbl.find_opt breakers name with
        | Some b -> b
        | None ->
            let b = { failures = 0; open_until = 0.0 } in
            Hashtbl.add breakers name b;
            b)
  in
  fun args ->
    let breaker = get_breaker args.tool.name in
    let now = Unix.gettimeofday () in
    let open_until = with_lock (fun () -> breaker.open_until) in
    if open_until > now then
      Error
        (Error.transient ~domain:(Tool args.tool.name)
           ~code:"circuit_open"
           (Printf.sprintf
              "circuit breaker open for '%s' (cooldown %.1fs)"
              args.tool.name (open_until -. now)))
    else
      match inner args with
      | Ok _ as r ->
          with_lock (fun () ->
              breaker.failures <- 0;
              breaker.open_until <- 0.0);
          r
      | Error _ as r ->
          with_lock (fun () ->
              breaker.failures <- breaker.failures + 1;
              if breaker.failures >= failure_threshold then begin
                breaker.open_until <- now +. cooldown;
                breaker.failures <- 0
              end);
          r

(* ===== Middleware: audit ===== *)

(** Hooks for telemetry / security review. Both callbacks default to
    no-op so passing this middleware with no args still gives you the
    chain shape without side effects. *)
let with_audit ?(on_call = fun _ -> ())
    ?(on_result = fun _ _ -> ()) (inner : t) : t =
 fun args ->
  on_call args;
  let result = inner args in
  on_result args result;
  result

(* ===== Middleware: log ===== *)

(** Tick emission for batches. Lives outside the chain because parallel
    batches dispatch via [Parallel.map_threaded] — OCaml 5 effect handlers
    don't propagate to threads, so [Effect.perform (Governor.Tick _)] from
    a worker thread would crash with [Effect.Unhandled]. The installer
    emits ticks on the main fiber before/after the parallel map. *)

(* Silently drop ticks when no Governor is installed (e.g. unit tests
   that exercise Tool_handler standalone). The chain remains usable
   without Governor; Governor just observes nothing. *)
let safe_tick ev =
  try Effect.perform (Governor.Tick ev)
  with Effect.Unhandled _ -> ()

let perform_tool_started_tick ~name ~use_id ~input =
  let digest =
    Digest.to_hex (Digest.string (Yojson.Safe.to_string input))
  in
  safe_tick
    (Tool_started
       {
         name;
         use_id = Id.Tool_use_id.to_string use_id;
         input_digest = digest;
       })

let perform_tool_finished_ticks ~tool ~use_id ~ok ~duration =
  safe_tick
    (Tool_finished
       {
         name = tool.name;
         use_id = Id.Tool_use_id.to_string use_id;
         ok;
         duration;
       });
  match tool.timeout_sec with
  | Some budget when duration > budget ->
      safe_tick (Tool_timeout { name = tool.name; duration; budget })
  | _ -> ()

(** Compact one-line log before/after each tool call, including the
    tool's category metadata. *)
let with_logging ?(on_log = prerr_endline) (inner : t) : t =
 fun args ->
  on_log
    (Printf.sprintf "  → tool: %s [%s] (%s)" args.tool.name
       args.tool.category
       (let s = Yojson.Safe.to_string args.input in
        if String.length s > 100 then String.sub s 0 100 ^ "..." else s));
  let result = inner args in
  (match result with
  | Ok content ->
      let preview =
        if String.length content > 200 then
          String.sub content 0 200 ^ "..."
        else content
      in
      on_log (Printf.sprintf "  ← %s ok: %s" args.tool.name preview)
  | Error err ->
      on_log (Printf.sprintf "  ✗ %s: %s" args.tool.name (Error.pp err)));
  result

(* ===== Install: convert chain to effect handler ===== *)

(** Apply [chain] to a single use, converting [Error.t] back to a plain
    string at the boundary (for the legacy effect signature). Returns
    the [tool_handler_result] tuple expected by [Effects.Tool_calls]. *)
let dispatch_one ~chain ~tools
    ((id, name, input) : Id.Tool_use_id.t * string * Yojson.Safe.t)
    : Id.Tool_use_id.t * tool_handler_result =
  let result =
    match List.find_opt (fun (t : tool_def) -> t.name = name) tools with
    | None ->
        Error (Printf.sprintf "unknown tool: %s" name)
    | Some tool -> (
        let args = { tool; input; use_id = id } in
        match chain args with
        | Ok s -> Ok s
        | Error err -> Error (Error.pp err))
  in
  (id, result)

(** Apply truncation to tool results. [load_skill] is exempt — a partial
    skill body is worse than no skill at all. *)
let truncate_result name r =
  if name = Skill.load_skill_tool_name then r
  else
    Result.map Protection.truncate_tool_content r
    |> Result.map_error Protection.truncate_tool_content

(** Install the chain as an effect handler. Catches [Effects.Tool_calls]
    and dispatches each tool through the chain; for batches of 2+,
    runs in parallel via threads.

    Governor ticks ([Tool_started]/[Tool_finished]/[Tool_timeout]) are
    emitted on the main fiber, around the dispatch. Worker threads only
    run the chain + measure duration — they never perform effects, since
    effect handlers don't propagate to threads.

    The recursive [wrap] lets tools that perform their own effects
    (notably [delegate], spawning sub-agents) propagate their effects
    through the SAME handler stack. *)
let install ~tools (chain : t) f =
  let open Effect.Deep in
  let find_tool name = List.find_opt (fun (t : tool_def) -> t.name = name) tools in
  let rec wrap : type r. (unit -> r) -> r =
   fun thunk ->
    try_with thunk ()
      {
        effc =
          (fun (type a) (eff : a Effect.t) ->
            match eff with
            | Effects.Tool_calls uses ->
                Some
                  (fun (k : (a, _) continuation) ->
                    List.iter
                      (fun (id, name, input) ->
                        perform_tool_started_tick ~name ~use_id:id ~input)
                      uses;
                    let one_call use =
                      let _, name, _ = use in
                      let t0 = Unix.gettimeofday () in
                      let id, raw = dispatch_one ~chain ~tools use in
                      let duration = Unix.gettimeofday () -. t0 in
                      (id, name, truncate_result name raw, duration)
                    in
                    let timed =
                      match uses with
                      | [ single ] ->
                          [ wrap (fun () -> one_call single) ]
                      | _ -> Parallel.map_threaded one_call uses
                    in
                    List.iter
                      (fun (id, name, r, duration) ->
                        match find_tool name with
                        | Some tool ->
                            perform_tool_finished_ticks ~tool ~use_id:id
                              ~ok:(Result.is_ok r) ~duration
                        | None -> ())
                      timed;
                    let results =
                      List.map (fun (id, _, r, _) -> (id, r)) timed
                    in
                    continue k results)
            | _ -> None);
      }
  in
  wrap f
