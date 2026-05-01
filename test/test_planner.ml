(** Tests for [Planner] (plan parsing) and [Plan_act.run_for_task]. *)

open Speedjs
open Types

let test_planner_parses_submit_plan () =
  let planner_response =
    {
      content =
        [
          Tool_use
            {
              id = Id.Tool_use_id.of_string "toolu_p";
              name = "submit_plan";
              input =
                `Assoc
                  [
                    ("title", `String "Compute 7*8");
                    ( "tasks",
                      `List
                        [
                          `Assoc
                            [ ("description", `String "Multiply 7 by 8") ];
                          `Assoc
                            [
                              ( "description",
                                `String "State the answer to the user" );
                            ];
                        ] );
                  ];
            };
        ];
      stop_reason = Tool_use_stop;
      usage = usage_of_basic ~input_tokens:50 ~output_tokens:30;
    }
  in
  let result =
    Handlers.mock ~llm_responses:[ planner_response ] (fun () ->
        Planner.plan ~goal:"What is 7*8?" ())
  in
  (match result with
  | Ok plan ->
      assert (plan.title = "Compute 7*8");
      assert (List.length plan.tasks = 2);
      assert ((List.hd plan.tasks).description = "Multiply 7 by 8");
      assert ((List.hd plan.tasks).index = 1)
  | Error e -> failwith ("planner failed: " ^ agent_error_pp e));
  print_endline "✓ planner parses submit_plan tool_use into typed plan"

let test_planner_invalid_no_submit () =
  let bad_response =
    {
      content = [ Text "I refuse to plan." ];
      stop_reason = End_turn;
      usage = usage_of_basic ~input_tokens:5 ~output_tokens:5;
    }
  in
  let result =
    Handlers.mock ~llm_responses:[ bad_response ] (fun () ->
        Planner.plan ~goal:"do thing" ())
  in
  (match result with
  | Error (Plan_invalid _) -> ()
  | _ -> failwith "expected Plan_invalid");
  print_endline "✓ planner returns Plan_invalid when submit_plan missing"

let test_run_for_task_explicit_submit () =
  let response =
    {
      content =
        [
          Tool_use
            {
              id = Id.Tool_use_id.of_string "u_submit";
              name = "submit_task_result";
              input =
                `Assoc
                  [
                    ("success", `Bool true);
                    ("result", `String "computed 42");
                    ("error", `String "");
                  ];
            };
        ];
      stop_reason = Tool_use_stop;
      usage = usage_of_basic ~input_tokens:5 ~output_tokens:10;
    }
  in
  let outcome =
    Handlers.mock ~llm_responses:[ response ] (fun () ->
        Plan_act.run_for_task ~task_description:"compute the answer"
          ~tools:Tools.all ())
  in
  (match outcome with
  | Task_done_explicit { submit; _ } ->
      assert submit.ts_success;
      assert (submit.ts_result = "computed 42")
  | _ -> failwith "expected Task_done_explicit");
  print_endline "✓ run_for_task captures submit_task_result as Task_done_explicit"

let test_run_for_task_explicit_failure () =
  let response =
    {
      content =
        [
          Tool_use
            {
              id = Id.Tool_use_id.of_string "u_submit";
              name = "submit_task_result";
              input =
                `Assoc
                  [
                    ("success", `Bool false);
                    ("result", `String "");
                    ("error", `String "input file missing");
                  ];
            };
        ];
      stop_reason = Tool_use_stop;
      usage = usage_of_basic ~input_tokens:5 ~output_tokens:10;
    }
  in
  let outcome =
    Handlers.mock ~llm_responses:[ response ] (fun () ->
        Plan_act.run_for_task ~task_description:"do thing" ~tools:Tools.all ())
  in
  (match outcome with
  | Task_done_explicit { submit; _ } ->
      assert (not submit.ts_success);
      assert (submit.ts_error = "input file missing")
  | _ -> failwith "expected Task_done_explicit failure");
  print_endline "✓ run_for_task captures submit_task_result with success=false"

let test_run_for_task_implicit_end_turn () =
  let response =
    {
      content = [ Text "the answer is 42" ];
      stop_reason = End_turn;
      usage = usage_of_basic ~input_tokens:5 ~output_tokens:5;
    }
  in
  let outcome =
    Handlers.mock ~llm_responses:[ response ] (fun () ->
        Plan_act.run_for_task ~task_description:"compute" ~tools:[] ())
  in
  (match outcome with
  | Task_done_implicit { answer; _ } -> assert (answer = "the answer is 42")
  | _ -> failwith "expected Task_done_implicit");
  print_endline
    "✓ run_for_task falls back to End_turn answer when submit not called"

let run () =
  test_planner_parses_submit_plan ();
  test_planner_invalid_no_submit ();
  test_run_for_task_explicit_submit ();
  test_run_for_task_explicit_failure ();
  test_run_for_task_implicit_end_turn ()
