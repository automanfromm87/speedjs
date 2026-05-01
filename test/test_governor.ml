(** Tests for [Governor] caps + [Planner.recover] / [parse_recovery_decision]. *)

open Speedjs
open Types

let test_governor_passes_tick_to_observer () =
  let cost = new_cost_state () in
  let captured = ref [] in
  Governor.install ~limits:Governor.Limits.none ~cost
    ~on_tick:(fun ev -> captured := ev :: !captured)
    (fun () ->
      Effect.perform
        (Governor.Tick
           (Llm_started { messages = 1; tools = 0 }));
      Effect.perform
        (Governor.Tick
           (Tool_started
              { name = "x"; use_id = "u"; input_digest = "d" })));
  assert (List.length !captured = 2);
  print_endline "✓ Governor.install routes Tick events through on_tick"

let test_governor_aborts_on_max_steps () =
  let cost = new_cost_state () in
  let limits = { Governor.Limits.none with max_steps = Some 3 } in
  let attempts = ref 0 in
  (try
     Governor.install ~limits ~cost (fun () ->
         for _ = 1 to 10 do
           incr attempts;
           Effect.perform
             (Governor.Tick (Llm_started { messages = 0; tools = 0 }))
         done);
     failwith "expected Governor_aborted"
   with Governor.Governor_aborted { limit; _ } ->
     assert (limit = "max_steps"));
  assert (!attempts = 4);
  print_endline "✓ Governor aborts on max_steps cap"

let test_governor_detects_death_loop () =
  let cost = new_cost_state () in
  let limits =
    { Governor.Limits.none with max_repeated_tool_calls = Some 4 }
  in
  let same_sig =
    Governor.Event.Tool_started
      { name = "stuck"; use_id = "u"; input_digest = "deadbeef" }
  in
  (try
     Governor.install ~limits ~cost (fun () ->
         for _ = 1 to 10 do
           Effect.perform (Governor.Tick same_sig)
         done);
     failwith "expected Governor_aborted"
   with
  | Governor.Governor_aborted { limit; _ } ->
      assert (limit = "max_repeated_tool_calls"));
  print_endline "✓ Governor detects identical-tool-call death loop"

let test_governor_subagent_depth () =
  let cost = new_cost_state () in
  let limits = { Governor.Limits.none with max_subagent_depth = Some 2 } in
  (try
     Governor.install ~limits ~cost (fun () ->
         Effect.perform (Governor.Tick Subagent_entered);
         Effect.perform (Governor.Tick Subagent_entered);
         Effect.perform (Governor.Tick Subagent_entered));
     failwith "expected Governor_aborted"
   with Governor.Governor_aborted { limit; _ } ->
     assert (limit = "max_subagent_depth"));
  print_endline "✓ Governor enforces sub-agent recursion depth"

(** Recovery prompt includes prior_failures + cycle_index so the
    planner can prefer abandon over similar-shape replans. *)
let test_recovery_prompt_includes_prior_failures_and_cycle () =
  let captured = ref None in
  let mock_response =
    {
      content =
        [
          Tool_use
            {
              id = Id.Tool_use_id.of_string "u1";
              name = "submit_recovery";
              input =
                `Assoc
                  [ ("decision", `String "abandon"); ("tasks", `List []) ];
            };
        ];
      stop_reason = Tool_use_stop;
      usage = usage_of_basic ~input_tokens:1 ~output_tokens:1;
    }
  in
  let mock_handler : Llm_handler.t =
   fun args ->
    captured := Some args;
    mock_response
  in
  let chain = Llm_handler.install mock_handler in
  let _ =
    chain (fun () ->
        Handlers.silent (fun () ->
            Planner.recover
              ~prior_failures:
                [
                  ("install widget", "npm fail");
                  ("set up CDN", "DNS error");
                ]
              ~cycle_index:1 ~max_cycles:2 ~goal:"build it"
              ~completed:[ "scaffold" ]
              ~failed_task:{ index = 2; description = "create binary" }
              ~failed_error:"compile error"
              ~remaining:[ "verify"; "ship" ] ()))
  in
  match !captured with
  | None -> failwith "mock LLM was never invoked"
  | Some args ->
      let body =
        match (List.hd args.messages).content with
        | [ Text s ] -> s
        | _ -> failwith "expected single text block"
      in
      assert (Test_helpers.contains body "Prior recovery failures");
      assert (Test_helpers.contains body "install widget");
      assert (Test_helpers.contains body "DNS error");
      assert (Test_helpers.contains body "Recovery cycle: 1 of max 2");
      print_endline
        "✓ Planner.recover injects prior_failures + cycle_index into prompt"

let test_recovery_parser_handles_all_four_decisions () =
  let mk_input ?(tasks = []) decision =
    let task_items =
      `List
        (List.map
           (fun d -> `Assoc [ ("description", `String d) ])
           tasks)
    in
    `Assoc [ ("decision", `String decision); ("tasks", task_items) ]
  in
  (match Planner.parse_recovery_decision (mk_input "abandon") with
  | Ok Planner.Abandon -> ()
  | _ -> failwith "abandon failed to parse");
  (match Planner.parse_recovery_decision (mk_input "skip") with
  | Ok Planner.Skip -> ()
  | _ -> failwith "skip failed to parse");
  (match
     Planner.parse_recovery_decision
       (mk_input ~tasks:[ "step1"; "step2" ] "replan")
   with
  | Ok (Planner.Replan ts) -> assert (List.length ts = 2)
  | _ -> failwith "replan failed to parse");
  (match
     Planner.parse_recovery_decision
       (mk_input ~tasks:[ "sub1"; "sub2"; "sub3" ] "split")
   with
  | Ok (Planner.Split ts) -> assert (List.length ts = 3)
  | _ -> failwith "split failed to parse");
  (match Planner.parse_recovery_decision (mk_input "unknown") with
  | Error _ -> ()
  | _ -> failwith "unknown should reject");
  (match Planner.parse_recovery_decision (mk_input "replan") with
  | Error _ -> ()
  | _ -> failwith "empty replan should reject");
  print_endline
    "✓ Planner.parse_recovery_decision handles replan / split / skip / \
     abandon"

let run () =
  test_governor_passes_tick_to_observer ();
  test_governor_aborts_on_max_steps ();
  test_governor_detects_death_loop ();
  test_governor_subagent_depth ();
  test_recovery_prompt_includes_prior_failures_and_cycle ();
  test_recovery_parser_handles_all_four_decisions ()
