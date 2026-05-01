(** Composable Log handler chain.

    Same pattern as [Llm_handler] / [Tool_handler]: each middleware
    wraps the next, [|>] for assembly, [install] for the effect-handler
    boundary.

    {[
      let log =
        Log_handler.to_stderr
        |> Log_handler.with_prefix "[speedjs] "
        |> Log_handler.with_filter ~min_level:Info

      Log_handler.install log thunk
    ]}

    Log lines are usually short and frequent; this chain keeps the
    structure consistent with the LLM/Tool handlers without adding
    much value beyond enabling per-handler filters / prefixes / fanout
    when needed. *)

(** A log handler is a function that takes a line and produces side
    effects (write to stderr, file, network, ...). *)
type t = string -> unit

(* ===== Sinks ===== *)

let to_stderr : t =
 fun s ->
  output_string stderr s;
  output_char stderr '\n';
  flush stderr

let to_function (f : string -> unit) : t = f

let to_channel (oc : out_channel) : t =
 fun s ->
  output_string oc s;
  output_char oc '\n';
  flush oc

let null : t = fun _ -> ()

(* ===== Middleware ===== *)

(** Prepend [prefix] to every line. *)
let with_prefix prefix (inner : t) : t = fun s -> inner (prefix ^ s)

(** Drop lines that don't pass the predicate. *)
let with_filter ~accept (inner : t) : t =
 fun s -> if accept s then inner s

(** Fan out to multiple sinks. *)
let tee (handlers : t list) : t =
 fun s -> List.iter (fun h -> h s) handlers

(* ===== Install ===== *)

(** Install the chain as an effect handler around [thunk]. Catches
    [Effects.Log] (free-form strings) AND [Effects.Event_log]
    (structured events). When [?on_event] is provided, structured
    events are routed there in addition to being rendered via
    [Event.to_log_line] and pushed through the string [chain] —
    observers can keep using grep on logs while machines tap into the
    structured stream. *)
let install ?(on_event : (Event.t -> unit) option) (chain : t) thunk =
  let open Effect.Deep in
  let event_observer =
    match on_event with Some f -> f | None -> fun _ -> ()
  in
  try_with thunk ()
    {
      effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Effects.Log msg ->
              Some
                (fun (k : (a, _) continuation) ->
                  chain msg;
                  continue k ())
          | Effects.Event_log ev ->
              Some
                (fun (k : (a, _) continuation) ->
                  event_observer ev;
                  chain (Event.to_log_line ev);
                  continue k ())
          | _ -> None);
    }
