(** Agent algebra demo. Builds a small composed agent and runs it.

    Usage:
      dune exec algebra_demo -- "<your input>"

    The agent built is intentionally simple — a [base] wrapped with
    [with_retry 2] and [pipe] to a follow-up [base]. The point is to
    show:
      1. [show] renders the tree before running ($ value).
      2. [execute] runs the same value through the standard runtime
         (cost / trace / sandbox / governor all inherited).
      3. Combinators are pure — building doesn't burn API credits. *)

open Speedjs

let () =
  let input =
    if Array.length Sys.argv < 2 then "What is 7 * 6?"
    else Sys.argv.(1)
  in

  let model =
    match Sys.getenv_opt "SPEEDJS_MODEL" with
    | Some m when m <> "" -> m
    | _ -> Anthropic.default_model
  in
  let cost = Handlers.new_cost_state () in

  (* Build a small composed agent. *)
  let agent =
    let summarize = Agent_algebra.base ~tools:[] () in
    let analyze =
      Agent_algebra.with_retry ~max_attempts:2
        (Agent_algebra.base
           ~tools:[ Tools.bash; Tools.calculator ]
           ~max_iters:5 ())
    in
    Agent_algebra.pipe analyze summarize
  in

  Printf.printf "──── agent tree ────\n%s\n──── input ────\n%s\n\n%!"
    (Agent_algebra.show agent) input;

  (* Standard runtime — same wiring main.ml uses, minus skills/MCP. *)
  let runtime_config : Runtime.config =
    {
      model;
      cost;
      on_log = prerr_endline;
      on_text_delta = (fun s -> output_string stderr s; flush stderr);
      governor_limits = Governor.Limits.default;
      llm_max_retries = 3;
      tape_path = None;
      crash_after = None;
      emit_governor_events_to_log = false;
      sandbox_root = None;
      tracer = Trace.make_noop ();
      on_event = (fun _ -> ());
    }
  in

  let result =
    Runtime.install ~tools:[ Tools.bash; Tools.calculator ]
      ~config:runtime_config (fun () ->
        Agent_algebra.execute agent input)
  in

  print_endline "\n──── result ────";
  (match result with
  | Ok s -> print_endline s
  | Error e ->
      Printf.eprintf "ERROR: %s\n" (Speedjs.Types.agent_error_pp e);
      exit 1);
  Printf.printf "\n──── cost ────\n$%.4f (in=%d out=%d cache_r=%d)\n"
    (Handlers.cost_usd cost) cost.input_tokens cost.output_tokens
    cost.cache_read_tokens
