open Speedjs

let default_query =
  "What is 23 * 19, and what time is it right now? Use the tools instead \
   of guessing."

let query_from_argv () =
  if Array.length Sys.argv <= 1 then default_query
  else
    Sys.argv |> Array.to_list |> List.tl |> String.concat " "

let print_output (out : Agent.output) =
  match out with
  | Agent.Done { answer; _ } ->
      print_endline "\n──── output (Done) ────";
      print_endline answer
  | Agent.Waiting { question; _ } ->
      print_endline "\n──── output (Waiting) ────";
      print_endline question
  | Agent.Terminal_tool { name; payload; _ } ->
      Printf.printf "\n──── output (Terminal_tool: %s) ────\n%s\n" name
        (Yojson.Safe.pretty_to_string payload)
  | Agent.Failed { reason; _ } ->
      Printf.eprintf "\n──── output (Failed) ────\n%s\n"
        (Types.agent_error_pp reason);
      exit 1

let () =
  let query = query_from_argv () in
  let model =
    match Sys.getenv_opt "SPEEDJS_MODEL" with
    | Some m when m <> "" -> m
    | _ -> Anthropic.default_model
  in
  let cost = Handlers.new_cost_state () in

  let raw_spec =
    Agent_spec.make
      ~name:"demo_researcher"
      ~mode:Types.Executor
      ~system_prompt:
        "You are a careful demo agent. When a tool can answer directly, \
         use the tool instead of guessing. Keep the final answer short."
      ~tools:[ Tools.calculator; Tools.current_time ]
      ()
    |> Agent_spec.with_max_iters 6
    |> Agent_spec.add_env ~tag:"demo" ~body:"agent_creation_demo"
  in
  let validated =
    match Agent_spec.validate raw_spec with
    | Ok v -> v
    | Error msg ->
        Printf.eprintf "invalid spec: %s\n" msg;
        exit 2
  in

  let visible_tool_names =
    validated.visible_tools
    |> List.map (fun (tool : Types.tool_def) -> tool.name)
    |> String.concat ", "
  in
  Printf.printf "──── agent creation demo ────\n";
  Printf.printf "query: %s\n" query;
  Printf.printf "spec: %s\n" (Agent_spec.show raw_spec);
  Printf.printf "visible tools: %s\n" visible_tool_names;

  let runtime_config : Runtime.config =
    {
      model;
      cost;
      on_log = (fun _ -> ());
      on_text_delta = (fun _ -> ());
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
  let out =
    Runtime.install ~config:runtime_config (fun () ->
        Agent.execute ~spec:validated ~input:(Agent.Fresh query))
  in
  print_output out;
  Printf.printf
    "\n──── cost ────\n$%.4f (in=%d out=%d cache_r=%d)\n"
    (Handlers.cost_usd cost) cost.input_tokens cost.output_tokens
    cost.cache_read_tokens
