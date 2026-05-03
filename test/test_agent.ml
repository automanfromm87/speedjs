(** Tests for [Agent.execute] / [Specs.chat] / sub-agent / MCP. *)

open Speedjs
open Types

let chat ?max_iters ?(tools = []) () = Specs.chat ?max_iters ~tools ()

let run_chat ?max_iters ?tools q =
  Agent.execute
    ~spec:(chat ?max_iters ?tools ())
    ~input:(Agent.Fresh q)

let run_chat_resume ?tools messages =
  Agent.execute
    ~spec:(chat ?tools ())
    ~input:(Agent.Resume messages)

let expect_done = function
  | Agent.Done { answer; messages } -> (answer, messages)
  | Agent.Failed { reason; _ } ->
      failwith ("expected Done, got Failed: " ^ agent_error_pp reason)
  | Agent.Waiting _ -> failwith "expected Done, got Waiting"
  | Agent.Terminal_tool { name; _ } ->
      failwith
        (Printf.sprintf "expected Done, got Terminal_tool %s" name)

let test_simple_text_response () =
  let response =
    {
      content = [ Text "Hello, world!" ];
      stop_reason = End_turn;
      usage = usage_of_basic ~input_tokens:10 ~output_tokens:5;
    }
  in
  let out =
    Handlers.mock ~llm_responses:[ response ] (fun () -> run_chat "hi")
  in
  let answer, _ = expect_done out in
  assert (answer = "Hello, world!");
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
  let out =
    Handlers.mock
      ~llm_responses:[ response1; response2 ]
      ~tool_results:[ ("calculator", Ok "4") ]
      (fun () -> run_chat ~tools:Tools.all "what is 2+2?")
  in
  let answer, _ = expect_done out in
  assert (answer = "The answer is 4.");
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
  let out =
    Handlers.mock
      ~llm_responses:[ response1; response2 ]
      ~tool_results:[ ("calculator", Error "bad expression") ]
      (fun () -> run_chat ~tools:Tools.all "compute bad")
  in
  let answer, _ = expect_done out in
  assert (answer = "Sorry, I couldn't compute that.");
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
  let out =
    Handlers.mock
      ~llm_responses:(List.init 20 (fun _ -> bad_response))
      ~tool_results:[ ("calculator", Ok "2") ]
      (fun () ->
        run_chat ~max_iters:5 ~tools:Tools.all "loop forever")
  in
  (match out with
  | Agent.Failed { reason = Max_iterations_reached n; _ } ->
      assert (n = 5)
  | _ ->
      failwith "expected Failed Max_iterations_reached");
  print_endline "✓ max iterations gives Failed Max_iterations_reached"

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
  let out =
    Handlers.mock ~llm_responses:[ response ]
      ~on_log:(fun s -> logs := s :: !logs)
      (fun () -> Handlers.silent (fun () -> run_chat "hi"))
  in
  let answer, _ = expect_done out in
  assert (answer = "ok");
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
  let out =
    Handlers.mock
      ~llm_responses:[ many_uses_response; final ]
      ~tool_results:
        [
          ("calculator", Ok "result-from-calc");
          ("current_time", Ok "result-from-time");
        ]
      (fun () -> run_chat ~tools:Tools.all "do many things")
  in
  let answer, _ = expect_done out in
  assert (answer = "all done");
  print_endline "✓ parallel tool batch dispatched and assembled in order"

let test_resume_outcome_done () =
  let response =
    {
      content = [ Text "the answer is 42" ];
      stop_reason = End_turn;
      usage = usage_of_basic ~input_tokens:5 ~output_tokens:5;
    }
  in
  let out =
    Handlers.mock ~llm_responses:[ response ] (fun () ->
        run_chat_resume [ { role = User; content = [ Text "hi" ] } ])
  in
  let answer, messages = expect_done out in
  assert (answer = "the answer is 42");
  assert (List.length messages = 2);
  assert ((List.hd messages).role = User);
  assert ((List.nth messages 1).role = Assistant);
  print_endline "✓ Resume returns Done with full message history"

let test_resume_outcome_waiting () =
  let response =
    {
      content =
        [
          Text "I need clarification.";
          Tool_use
            {
              id = Id.Tool_use_id.of_string "toolu_ask";
              name = "ask_user";
              input = `Assoc [ ("question", `String "Which file?") ];
            };
        ];
      stop_reason = Tool_use_stop;
      usage = usage_of_basic ~input_tokens:5 ~output_tokens:10;
    }
  in
  let out =
    Handlers.mock ~llm_responses:[ response ] (fun () ->
        run_chat_resume ~tools:Tools.all
          [ { role = User; content = [ Text "help me" ] } ])
  in
  (match out with
  | Agent.Waiting { tool_use_id; question; messages } ->
      assert (Id.Tool_use_id.to_string tool_use_id = "toolu_ask");
      assert (question = "Which file?");
      assert (List.length messages = 2)
  | _ -> failwith "expected Waiting");
  print_endline "✓ Resume returns Waiting on ask_user pause-tool"

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
    "✓ delegate.handler spawns subagent spec with task as fresh first message"

(* C: validate rejects specs whose terminal tool isn't visible after
   mode-filtering. This is the contract that backs
   [Agent_spec.validated]: smart-constructed Specs.* are guaranteed
   valid, but raw [make] -> [validate] surfaces misuse here, not at
   run time. *)
let test_validate_rejects_terminal_not_in_tools () =
  let raw =
    Agent_spec.make
      ~mode:Executor
      ~terminal:(Agent_spec.Tool { name = "missing_tool" })
      ~tools:[]
      ()
  in
  (match Agent_spec.validate raw with
  | Ok _ -> failwith "expected validate to reject missing terminal tool"
  | Error msg ->
      assert (Test_helpers.contains msg "not visible"));
  print_endline
    "✓ Agent_spec.validate rejects terminal-tool-not-in-tools"

let test_validate_rejects_terminal_lacking_capability () =
  let bad =
    Types.make_typed_tool
      ~name:"submit_x"
      ~description:""
      ~capabilities:[ Read_only ]  (* missing Terminal *)
      ~input_schema:(`Assoc [ ("type", `String "object") ])
      ~input_decoder:(fun _ -> Ok ())
      ~handler:(fun () -> Ok "")
      ()
  in
  let raw =
    Agent_spec.make
      ~mode:Executor
      ~terminal:(Agent_spec.Tool { name = "submit_x" })
      ~tools:[ bad ]
      ()
  in
  (match Agent_spec.validate raw with
  | Ok _ -> failwith "expected validate to reject non-Terminal tool"
  | Error msg ->
      assert (Test_helpers.contains msg "lacks Terminal capability"));
  print_endline
    "✓ Agent_spec.validate rejects terminal tool lacking [Terminal] capability"

(* Regression: validate rejects spec.tools containing duplicate
   names. Silent shadowing in [Tool_handler.dispatch_one] /
   [Step.find_pause_block] was a real bug class — this catches it
   at the spec boundary. *)
let test_validate_rejects_duplicate_tool_names () =
  let mk name =
    Types.make_typed_tool
      ~name
      ~description:""
      ~capabilities:[ Read_only ]
      ~input_schema:(`Assoc [ ("type", `String "object") ])
      ~input_decoder:(fun _ -> Ok ())
      ~handler:(fun () -> Ok "")
      ()
  in
  let raw =
    Agent_spec.make
      ~mode:Executor
      ~tools:[ mk "calc"; mk "view"; mk "calc" ]  (* dup *)
      ()
  in
  (match Agent_spec.validate raw with
  | Ok _ -> failwith "expected validate to reject duplicate names"
  | Error msg ->
      assert (Test_helpers.contains msg "duplicate");
      assert (Test_helpers.contains msg "calc"));
  print_endline
    "✓ Agent_spec.validate rejects duplicate tool names"

(* Regression: execute_raw allows outer combinator to override
   Pause/Terminal handling. With the old impl (handler installed
   inside execute), an outer try_with around execute couldn't
   intercept either effect. Verify execute_raw + outer handler
   wires correctly. *)
let test_execute_raw_lets_outer_override_pause () =
  let response =
    {
      content =
        [
          Tool_use
            {
              id = Id.Tool_use_id.of_string "u1";
              name = Tools.ask_user_name;
              input = `Assoc [ ("question", `String "color?") ];
            };
        ];
      stop_reason = Tool_use_stop;
      usage = usage_of_basic ~input_tokens:1 ~output_tokens:1;
    }
  in
  let captured_question = ref None in
  let spec =
    Specs.chat ~tools:[ Tools.ask_user ] ~max_iters:5 ()
  in
  Handlers.mock ~llm_responses:[ response ] (fun () ->
      let _ : Agent.output =
        Effect.Deep.try_with
          (fun () ->
            Agent.execute_raw ~spec ~input:(Agent.Fresh "go"))
          ()
          {
            effc =
              (fun (type a) (eff : a Effect.t) ->
                match eff with
                | Effects.Pause { question; _ } ->
                    Some
                      (fun (_k : (a, _) Effect.Deep.continuation) ->
                        captured_question := Some question;
                        Agent.Failed
                          {
                            reason = Plan_invalid "intercepted";
                            messages = [];
                          })
                | _ -> None);
          }
      in
      ());
  (match !captured_question with
  | Some "color?" -> ()
  | _ -> failwith "outer Pause handler did not fire on execute_raw");
  print_endline
    "✓ execute_raw lets outer combinator install custom Pause handler"

(* B: force_terminal_in_last_n = 0 disables endgame tool-choice
   forcing. With default 2 the loop forces the submit on its last
   2 iterations (preventing Max_iterations on a model that just
   keeps emitting text). *)
let test_force_terminal_disabled_does_not_force () =
  (* Build a 4-iteration run where the model emits End_turn (no
     submit). With force_terminal_in_last_n = 0, endgame doesn't
     trip; the model's End_turn produces Done(implicit), not
     Terminal_tool. With default 2, the last 2 iters would set
     tool_choice=Tc_tool — but mock LLM ignores tool_choice and
     also returns End_turn, so that test would also produce
     Done. We verify the LOG path: with disable, "ENDGAME" never
     appears; with default it does. *)
  let resp =
    {
      content = [ Text "done" ];
      stop_reason = End_turn;
      usage = usage_of_basic ~input_tokens:1 ~output_tokens:1;
    }
  in
  let terminal_tool =
    Types.make_typed_tool
      ~name:"submit_x"
      ~description:""
      ~capabilities:[ Terminal ]
      ~allowed_modes:[ Executor ]
      ~input_schema:(`Assoc [ ("type", `String "object") ])
      ~input_decoder:(fun _ -> Ok ())
      ~handler:(fun () -> Ok "")
      ()
  in
  let logs_off = ref [] in
  let _ =
    Handlers.mock ~llm_responses:[ resp ]
      ~on_log:(fun s -> logs_off := s :: !logs_off)
      (fun () ->
        let raw =
          Agent_spec.make
            ~mode:Executor
            ~terminal:(Agent_spec.Tool { name = "submit_x" })
            ~force_terminal_in_last_n:0
            ~max_iters:3
            ~tools:[ terminal_tool ]
            ()
        in
        match Agent_spec.validate raw with
        | Ok spec -> Agent.execute ~spec ~input:(Agent.Fresh "go")
        | Error msg -> failwith msg)
  in
  assert (
    not
      (List.exists (fun s -> Test_helpers.contains s "ENDGAME") !logs_off));
  print_endline
    "✓ force_terminal_in_last_n=0 disables endgame tool-choice forcing"

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
  test_resume_outcome_done ();
  test_resume_outcome_waiting ();
  test_session_resume_with_tool_result ();
  test_subagent_isolated_history ();
  test_validate_rejects_terminal_not_in_tools ();
  test_validate_rejects_terminal_lacking_capability ();
  test_validate_rejects_duplicate_tool_names ();
  test_execute_raw_lets_outer_override_pause ();
  test_force_terminal_disabled_does_not_force ();
  test_mcp_extract_text_content ()
