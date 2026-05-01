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

(* ========================================================================
   Governor end-to-end through the full handler stack.

   Verifies the limits actually fire when Governor is wired up via
   Llm_handler.with_governor_ticks + Tool_handler tick emissions —
   not just at the unit level (which the four tests above cover).
   ======================================================================== *)

let canned_llm responses : Llm_handler.t =
  let pending = ref responses in
  fun _args ->
    match !pending with
    | [] -> failwith "[mock-llm] ran out of canned responses"
    | r :: rest ->
        pending := rest;
        r

let test_governor_aborts_on_cost_via_full_chain () =
  let cost = new_cost_state () in
  let limits = { Governor.Limits.none with max_cost_usd = Some 0.001 } in
  (* Each LLM response charges ~$0.0009 input + $0.00015 output ≈ $0.001+
     so the second call trips the cap. *)
  let expensive_resp =
    {
      content =
        [
          Tool_use
            {
              id = Id.Tool_use_id.of_string "u";
              name = "noop";
              input = `Assoc [];
            };
        ];
      stop_reason = Tool_use_stop;
      usage =
        {
          input_tokens = 300;
          output_tokens = 10;
          cache_creation_input_tokens = 0;
          cache_read_input_tokens = 0;
        };
    }
  in
  let llm_chain =
    canned_llm [ expensive_resp; expensive_resp; expensive_resp ]
    |> Llm_handler.with_cost_tracking ~cost
    |> Llm_handler.with_governor_ticks
  in
  let aborted = ref false in
  (try
     Governor.install ~limits ~cost (fun () ->
         Llm_handler.install llm_chain (fun () ->
             Log_handler.install Log_handler.null (fun () ->
                 Handlers.silent (fun () ->
                     ignore
                       (Agent.run ~max_iterations:10
                          ~user_query:"loop forever" ~tools:[] ())))))
   with Governor.Governor_aborted { limit; _ } ->
     assert (limit = "max_cost");
     aborted := true);
  assert !aborted;
  print_endline
    "✓ Governor aborts mid-run when cost crosses max_cost_usd (full chain)"

let test_governor_aborts_on_death_loop_via_full_chain () =
  (* LLM keeps emitting the SAME tool_use with identical input. The
     Governor's input-digest counter trips after [max_repeated_tool_calls]
     identical calls. *)
  let cost = new_cost_state () in
  let limits =
    { Governor.Limits.none with max_repeated_tool_calls = Some 3 }
  in
  let same_call =
    {
      content =
        [
          Tool_use
            {
              id = Id.Tool_use_id.of_string "u";
              name = "calculator";
              input = `Assoc [ ("expression", `String "1+1") ];
            };
        ];
      stop_reason = Tool_use_stop;
      usage = usage_of_basic ~input_tokens:1 ~output_tokens:1;
    }
  in
  let llm_chain =
    canned_llm (List.init 20 (fun _ -> same_call))
  in
  let aborted = ref false in
  (try
     Governor.install ~limits ~cost (fun () ->
         Llm_handler.install llm_chain (fun () ->
             File_handler.install File_handler.direct (fun () ->
                 Time_handler.install Time_handler.direct (fun () ->
                     Tool_handler.install ~tools:Tools.all
                       Tool_handler.direct (fun () ->
                         Log_handler.install Log_handler.null (fun () ->
                             ignore
                               (Agent.run ~max_iterations:50
                                  ~user_query:"loop"
                                  ~tools:Tools.all ())))))))
   with Governor.Governor_aborted { limit; _ } ->
     assert (limit = "max_repeated_tool_calls");
     aborted := true);
  assert !aborted;
  print_endline
    "✓ Governor aborts mid-run on identical-input tool repetition (full chain)"

let run () =
  test_governor_passes_tick_to_observer ();
  test_governor_aborts_on_max_steps ();
  test_governor_detects_death_loop ();
  test_governor_subagent_depth ();
  test_recovery_prompt_includes_prior_failures_and_cycle ();
  test_recovery_parser_handles_all_four_decisions ();
  test_governor_aborts_on_cost_via_full_chain ();
  test_governor_aborts_on_death_loop_via_full_chain ()
