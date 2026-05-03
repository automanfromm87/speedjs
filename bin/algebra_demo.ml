(** Workflow demo. Builds a small composed agent flow and runs it.

    Usage:
      dune exec algebra_demo -- "<your input>"

    The flow is intentionally simple — analyze (with retry) → pipe its
    answer into a summarize leaf. Demonstrates:
      1. Workflow combinators ([with_retry], [bind] via [let*], [map]).
      2. Same runtime as [main.ml] (cost / trace / sandbox / governor
         inherited).
      3. Building the flow is pure; only [Workflow.run] burns API
         credits. *)

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

  (* analyze leaf, retried up to 2 times on failure; its answer becomes
     the input to summarize. *)
  let flow : (string * Speedjs.Types.message list) Workflow.t =
    let open Workflow in
    let analyze_spec =
      Specs.chat ~tools:[ Tools.bash; Tools.calculator ] ~max_iters:5 ()
    in
    let summarize_spec = Specs.chat ~tools:[] () in
    let* (answer, _) =
      leaf analyze_spec (Agent.Fresh input)
      |> expect_done ~name:"analyze"
      |> with_retry ~max_attempts:2
    in
    leaf summarize_spec (Agent.Fresh answer)
    |> expect_done ~name:"summarize"
  in

  Printf.printf "──── input ────\n%s\n\n%!" input;

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
      chaos = Chaos.default;
    }
  in

  let result =
    Runtime.install ~config:runtime_config (fun () -> Workflow.run flow)
  in

  print_endline "\n──── result ────";
  (match result with
  | Ok (answer, _) -> print_endline answer
  | Error reason ->
      Printf.eprintf "ERROR: %s\n" (Speedjs.Types.agent_error_pp reason);
      exit 1);
  Printf.printf "\n──── cost ────\n$%.4f (in=%d out=%d cache_r=%d)\n"
    (Handlers.cost_usd cost) cost.input_tokens cost.output_tokens
    cost.cache_read_tokens
