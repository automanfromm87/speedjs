(** Tests for [Agent.run] / [Agent.run_session] / sub-agent / MCP. *)

open Speedjs
open Types

let test_simple_text_response () =
  let response =
    {
      content = [ Text "Hello, world!" ];
      stop_reason = End_turn;
      usage = usage_of_basic ~input_tokens:10 ~output_tokens:5;
    }
  in
  let result =
    Handlers.mock ~llm_responses:[ response ] (fun () ->
        Agent.run ~user_query:"hi" ~tools:[] ())
  in
  (match result with
  | Ok s -> assert (s = "Hello, world!")
  | Error e -> failwith ("expected Ok, got Error: " ^ agent_error_pp e));
  print_endline "✓ simple text response"

let test_single_tool_call () =
  let response1 =
    {
      content =
        [
          Text "Let me check.";
          Tool_use
            {
              id = Id.Tool_use_id.of_string "toolu_1";
              name = "calculator";
              input = `Assoc [ ("expression", `String "2+2") ];
            };
        ];
      stop_reason = Tool_use_stop;
      usage = usage_of_basic ~input_tokens:50 ~output_tokens:30;
    }
  in
  let response2 =
    {
      content = [ Text "The answer is 4." ];
      stop_reason = End_turn;
      usage = usage_of_basic ~input_tokens:80 ~output_tokens:8;
    }
  in
  let result =
    Handlers.mock
      ~llm_responses:[ response1; response2 ]
      ~tool_results:[ ("calculator", Ok "4") ]
      (fun () -> Agent.run ~user_query:"what is 2+2?" ~tools:Tools.all ())
  in
  (match result with
  | Ok s -> assert (s = "The answer is 4.")
  | Error e -> failwith ("expected Ok, got Error: " ^ agent_error_pp e));
  print_endline "✓ single tool call"

let test_tool_error_surfaced () =
  let response1 =
    {
      content =
        [
          Tool_use
            {
              id = Id.Tool_use_id.of_string "toolu_2";
              name = "calculator";
              input = `Assoc [ ("expression", `String "bad") ];
            };
        ];
      stop_reason = Tool_use_stop;
      usage = usage_of_basic ~input_tokens:10 ~output_tokens:10;
    }
  in
  let response2 =
    {
      content = [ Text "Sorry, I couldn't compute that." ];
      stop_reason = End_turn;
      usage = usage_of_basic ~input_tokens:20 ~output_tokens:8;
    }
  in
  let result =
    Handlers.mock
      ~llm_responses:[ response1; response2 ]
      ~tool_results:[ ("calculator", Error "bad expression") ]
      (fun () -> Agent.run ~user_query:"compute bad" ~tools:Tools.all ())
  in
  (match result with
  | Ok s -> assert (s = "Sorry, I couldn't compute that.")
  | Error e -> failwith ("expected Ok, got Error: " ^ agent_error_pp e));
  print_endline "✓ tool error surfaced as is_error to LLM"

let test_max_iterations () =
  let bad_response =
    {
      content =
        [
          Tool_use
            {
              id = Id.Tool_use_id.of_string "toolu_x";
              name = "calculator";
              input = `Assoc [ ("expression", `String "1+1") ];
            };
        ];
      stop_reason = Tool_use_stop;
      usage = usage_of_basic ~input_tokens:10 ~output_tokens:10;
    }
  in
  let result =
    Handlers.mock
      ~llm_responses:(List.init 20 (fun _ -> bad_response))
      ~tool_results:[ ("calculator", Ok "2") ]
      (fun () ->
        Agent.run ~max_iterations:5 ~user_query:"loop forever"
          ~tools:Tools.all ())
  in
  (match result with
  | Error (Max_iterations_reached n) -> assert (n = 5)
  | Ok _ -> failwith "expected Max_iterations_reached, got Ok"
  | Error e ->
      failwith ("expected Max_iterations_reached, got: " ^ agent_error_pp e));
  print_endline "✓ max iterations gives Error Max_iterations_reached"

let test_truncate_tool_content () =
  let huge = String.make 30_000 'x' in
  let truncated = Protection.truncate_tool_content huge in
  assert (String.length truncated < 14_000);
  assert (String.length truncated > 12_000);
  assert (String.sub truncated 0 5 = "xxxxx");
  assert (String.sub truncated (String.length truncated - 5) 5 = "xxxxx");
  assert (Test_helpers.contains truncated "[truncated");
  print_endline "✓ truncate_tool_content keeps head + tail"

let test_silent_handler_composition () =
  let logs = ref [] in
  let response =
    {
      content = [ Text "ok" ];
      stop_reason = End_turn;
      usage = usage_of_basic ~input_tokens:1 ~output_tokens:1;
    }
  in
  let result =
    Handlers.mock ~llm_responses:[ response ]
      ~on_log:(fun s -> logs := s :: !logs)
      (fun () ->
        Handlers.silent (fun () -> Agent.run ~user_query:"hi" ~tools:[] ()))
  in
  (match result with
  | Ok s -> assert (s = "ok")
  | Error e -> failwith ("expected Ok, got Error: " ^ agent_error_pp e));
  assert (!logs = []);
  print_endline "✓ silent handler composes (suppresses logs)"

let test_parallel_tool_batch_dispatched () =
  let many_uses_response =
    {
      content =
        [
          Tool_use
            {
              id = Id.Tool_use_id.of_string "u1";
              name = "calculator";
              input = `Assoc [ ("expression", `String "1+1") ];
            };
          Tool_use
            {
              id = Id.Tool_use_id.of_string "u2";
              name = "current_time";
              input = `Assoc [];
            };
          Tool_use
            {
              id = Id.Tool_use_id.of_string "u3";
              name = "calculator";
              input = `Assoc [ ("expression", `String "2+2") ];
            };
        ];
      stop_reason = Tool_use_stop;
      usage = usage_of_basic ~input_tokens:10 ~output_tokens:30;
    }
  in
  let final =
    {
      content = [ Text "all done" ];
      stop_reason = End_turn;
      usage = usage_of_basic ~input_tokens:5 ~output_tokens:5;
    }
  in
  let result =
    Handlers.mock
      ~llm_responses:[ many_uses_response; final ]
      ~tool_results:
        [
          ("calculator", Ok "result-from-calc");
          ("current_time", Ok "result-from-time");
        ]
      (fun () -> Agent.run ~user_query:"do many things" ~tools:Tools.all ())
  in
  (match result with
  | Ok s -> assert (s = "all done")
  | Error e -> failwith ("expected Ok, got Error: " ^ agent_error_pp e));
  print_endline "✓ parallel tool batch dispatched and assembled in order"

let test_run_session_outcome_done () =
  let response =
    {
      content = [ Text "the answer is 42" ];
      stop_reason = End_turn;
      usage = usage_of_basic ~input_tokens:5 ~output_tokens:5;
    }
  in
  let outcome =
    Handlers.mock ~llm_responses:[ response ] (fun () ->
        Agent.run_session
          ~messages:[ { role = User; content = [ Text "hi" ] } ]
          ~tools:[] ())
  in
  (match outcome with
  | Outcome_done { answer; final_messages } ->
      assert (answer = "the answer is 42");
      assert (List.length final_messages = 2);
      assert ((List.hd final_messages).role = User);
      assert ((List.nth final_messages 1).role = Assistant)
  | _ -> failwith "expected Outcome_done");
  print_endline "✓ run_session returns Done with full message history"

let test_run_session_outcome_waiting () =
  let response =
    {
      content =
        [
          Text "I need clarification.";
          Tool_use
            {
              id = Id.Tool_use_id.of_string "toolu_ask";
              name = "ask_user";
              input =
                `Assoc [ ("question", `String "Which file?") ];
            };
        ];
      stop_reason = Tool_use_stop;
      usage = usage_of_basic ~input_tokens:5 ~output_tokens:10;
    }
  in
  let outcome =
    Handlers.mock ~llm_responses:[ response ] (fun () ->
        Agent.run_session
          ~messages:[ { role = User; content = [ Text "help me" ] } ]
          ~tools:Tools.all ())
  in
  (match outcome with
  | Outcome_waiting { tool_use_id; question; messages } ->
      assert (Id.Tool_use_id.to_string tool_use_id = "toolu_ask");
      assert (question = "Which file?");
      assert (List.length messages = 2)
  | _ -> failwith "expected Outcome_waiting");
  print_endline "✓ run_session returns Waiting on ask_user pause-tool"

let test_session_resume_with_tool_result () =
  let prior_messages =
    [
      { role = User; content = [ Text "do thing" ] };
      {
        role = Assistant;
        content =
          [
            Text "ok let me ask";
            Tool_use
              {
                id = Id.Tool_use_id.of_string "toolu_q";
                name = "ask_user";
                input = `Assoc [ ("question", `String "?") ];
              };
          ];
      };
    ]
  in
  let prior_session =
    Session.{
      messages = prior_messages;
      pending_tool_use_id = Some (Id.Tool_use_id.of_string "toolu_q");
      model = "test";
    }
  in
  let session = Session.append_input prior_session "/tmp/x.py" in
  let last_msg = List.nth session.messages (List.length session.messages - 1) in
  (match last_msg.content with
  | [ Tool_result { tool_use_id; content; _ } ] ->
      assert (Id.Tool_use_id.to_string tool_use_id = "toolu_q");
      assert (content = "/tmp/x.py")
  | _ -> failwith "expected Tool_result block");
  print_endline "✓ Session.append_input answers pending ask_user"

let test_subagent_isolated_history () =
  let captured_first_message_text = ref None in
  let response =
    {
      content = [ Text "from sub-agent" ];
      stop_reason = End_turn;
      usage = usage_of_basic ~input_tokens:3 ~output_tokens:5;
    }
  in
  let recording_handler f =
    Effect.Deep.try_with f ()
      {
        effc =
          (fun (type a) (eff : a Effect.t) ->
            match eff with
            | Effects.Llm_complete args ->
                Some
                  (fun (k : (a, _) Effect.Deep.continuation) ->
                    (if !captured_first_message_text = None then
                       match args.messages with
                       | { content = Text s :: _; _ } :: _ ->
                           captured_first_message_text := Some s
                       | _ -> ());
                    Effect.Deep.continue k response)
            | Effects.Log _ ->
                Some (fun k -> Effect.Deep.continue k ())
            | _ -> None);
      }
  in
  let delegate = Sub_agent.make_delegate_tool ~tools_for_subagent:Tools.all in
  let result =
    recording_handler (fun () ->
        delegate.handler (`Assoc [ ("task", `String "ISOLATED TASK GOAL") ]))
  in
  (match result with
  | Ok s ->
      assert (s = "from sub-agent");
      assert (!captured_first_message_text = Some "ISOLATED TASK GOAL")
  | Error e -> failwith ("delegate failed: " ^ e));
  print_endline
    "✓ delegate.handler spawns Agent.run with task as fresh first message"

let test_mcp_extract_text_content () =
  let success_payload =
    `Assoc
      [
        ( "content",
          `List
            [
              `Assoc
                [
                  ("type", `String "text"); ("text", `String "hello");
                ];
              `Assoc
                [
                  ("type", `String "text"); ("text", `String "world");
                ];
            ] );
        ("isError", `Bool false);
      ]
  in
  (match Mcp.extract_text_content success_payload with
  | Ok s -> assert (s = "hello\nworld")
  | Error e -> failwith ("expected Ok, got Error: " ^ e));

  let error_payload =
    `Assoc
      [
        ( "content",
          `List
            [
              `Assoc
                [
                  ("type", `String "text"); ("text", `String "bad input");
                ];
            ] );
        ("isError", `Bool true);
      ]
  in
  (match Mcp.extract_text_content error_payload with
  | Error msg -> assert (msg = "bad input")
  | Ok _ -> failwith "expected Error");
  print_endline "✓ Mcp.extract_text_content parses success + isError payloads"

let run () =
  test_simple_text_response ();
  test_single_tool_call ();
  test_tool_error_surfaced ();
  test_max_iterations ();
  test_truncate_tool_content ();
  test_silent_handler_composition ();
  test_parallel_tool_batch_dispatched ();
  test_run_session_outcome_done ();
  test_run_session_outcome_waiting ();
  test_session_resume_with_tool_result ();
  test_subagent_isolated_history ();
  test_mcp_extract_text_content ()
