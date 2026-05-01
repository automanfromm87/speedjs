(** Tests for [Step.once] — single ReAct step + Event_log routing. *)

open Speedjs
open Types

let test_event_log_routes_through_observer_and_string_chain () =
  let captured_events = ref [] in
  let captured_lines = ref [] in
  let on_event ev = captured_events := ev :: !captured_events in
  let chain s = captured_lines := s :: !captured_lines in
  Log_handler.install ~on_event chain (fun () ->
      Effect.perform
        (Effects.Event_log (Plan_decomposed { goal_preview = "build X"; n_tasks = 7 }));
      Effect.perform
        (Effects.Event_log
           (Task_started { index = 1; total = 7; description = "init" }));
      Effect.perform (Effects.Log "free-form line"));
  (match List.rev !captured_events with
  | [ Event.Plan_decomposed { n_tasks = 7; _ };
      Event.Task_started { index = 1; total = 7; _ } ] ->
      ()
  | _ -> failwith "observer didn't receive expected events in order");
  assert (List.length !captured_lines = 3);
  print_endline
    "✓ Log_handler.install routes Event_log to ~on_event AND renders to \
     string chain"

(* Each test pins ONE step's outcome by feeding a single canned LLM
   response and asserting the variant Step.once returns. *)

let mk_resp ?(usage = usage_of_basic ~input_tokens:1 ~output_tokens:1)
    ~stop_reason content =
  { content; stop_reason; usage }

let mk_ctx () = Context.empty |> Context.push_user_text "Q"

let test_step_returns_terminal_text_on_end_turn () =
  let response =
    mk_resp ~stop_reason:End_turn [ Text "the answer" ]
  in
  let outcome =
    Handlers.mock ~llm_responses:[ response ] (fun () ->
        Step.once ~ctx:(mk_ctx ()) ())
  in
  (match outcome with
  | Step.Terminal_text { answer; _ } -> assert (answer = "the answer")
  | _ -> failwith "expected Terminal_text");
  print_endline "✓ Step.once returns Terminal_text on End_turn"

let test_step_returns_continue_on_normal_tool_use () =
  let response =
    mk_resp ~stop_reason:Tool_use_stop
      [
        Tool_use
          { id = Id.Tool_use_id.of_string "u1"; name = "read_file"; input = `Assoc [ ("p", `String "/x") ] };
      ]
  in
  let outcome =
    Handlers.mock ~llm_responses:[ response ]
      ~tool_results:[ ("read_file", Ok "file contents") ]
      (fun () -> Step.once ~ctx:(mk_ctx ()) ())
  in
  (match outcome with
  | Step.Continue ctx ->
      let msgs = Conversation.to_messages (Context.conversation ctx) in
      assert (List.length msgs = 3)
  | _ -> failwith "expected Continue");
  print_endline "✓ Step.once returns Continue + appends tool_result on Tool_use_stop"

let test_step_returns_terminal_tool_when_terminal_called () =
  let response =
    mk_resp ~stop_reason:Tool_use_stop
      [
        Tool_use
          {
            id = Id.Tool_use_id.of_string "u1";
            name = "submit_task_result";
            input = `Assoc [ ("ok", `Bool true) ];
          };
      ]
  in
  let outcome =
    Handlers.mock ~llm_responses:[ response ] (fun () ->
        Step.once ~terminal_tools:[ "submit_task_result" ]
          ~ctx:(mk_ctx ()) ())
  in
  (match outcome with
  | Step.Terminal_tool { tool_name; _ } ->
      assert (tool_name = "submit_task_result")
  | _ -> failwith "expected Terminal_tool");
  print_endline
    "✓ Step.once returns Terminal_tool when terminal tool is called"

let test_step_returns_wait_for_user_on_ask_user () =
  let response =
    mk_resp ~stop_reason:Tool_use_stop
      [
        Tool_use
          {
            id = Id.Tool_use_id.of_string "u1";
            name = Tools.ask_user_name;
            input = `Assoc [ ("question", `String "what color?") ];
          };
      ]
  in
  let outcome =
    Handlers.mock ~llm_responses:[ response ] (fun () ->
        Step.once ~ctx:(mk_ctx ()) ())
  in
  (match outcome with
  | Step.Wait_for_user { question; _ } -> assert (question = "what color?")
  | _ -> failwith "expected Wait_for_user");
  print_endline "✓ Step.once returns Wait_for_user on ask_user tool"

let test_step_returns_failed_on_max_tokens () =
  let response =
    mk_resp ~stop_reason:Max_tokens [ Text "partial..." ]
  in
  let outcome =
    Handlers.mock ~llm_responses:[ response ] (fun () ->
        Step.once ~ctx:(mk_ctx ()) ())
  in
  (match outcome with
  | Step.Failed { reason = Llm_max_tokens; _ } -> ()
  | _ -> failwith "expected Failed Llm_max_tokens");
  print_endline "✓ Step.once returns Failed on Max_tokens stop_reason"

let run () =
  test_event_log_routes_through_observer_and_string_chain ();
  test_step_returns_terminal_text_on_end_turn ();
  test_step_returns_continue_on_normal_tool_use ();
  test_step_returns_terminal_tool_when_terminal_called ();
  test_step_returns_wait_for_user_on_ask_user ();
  test_step_returns_failed_on_max_tokens ()
