(** Tests for the agent loop using the mock handler.

    Demonstrates the effect-system superpower: we can fully exercise the
    agent's control flow without ever calling the real Anthropic API or
    running real tools. The same agent code from [Speedjs.Agent.run] runs
    here and in production — only the handler differs. *)

open Speedjs
open Types

(* ===== Event_log routing test ===== *)

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
  (* Two events captured by observer, in order. *)
  (match List.rev !captured_events with
  | [ Event.Plan_decomposed { n_tasks = 7; _ };
      Event.Task_started { index = 1; total = 7; _ } ] ->
      ()
  | _ -> failwith "observer didn't receive expected events in order");
  (* String chain saw rendered events + the free-form line — 3 lines. *)
  assert (List.length !captured_lines = 3);
  print_endline
    "✓ Log_handler.install routes Event_log to ~on_event AND renders to \
     string chain"

(* ===== Step.once unit tests =====
   Each test pins ONE step's outcome by feeding a single canned LLM
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
      (* original User Q, assistant tool_use, user tool_result = 3 turns *)
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

let test_simple_text_response () =
  (* LLM gives one response, just text, no tool calls. End_turn. *)
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
  (* Tool returns Error; agent should still complete the loop with LLM
     receiving an is_error tool_result. *)
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
  (* Use a low [max_iterations] so the test doesn't have to enumerate 100+
     responses. Verifies the parameter is honored. *)
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

let contains haystack needle =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  let rec scan i =
    if i + nlen > hlen then false
    else if String.sub haystack i nlen = needle then true
    else scan (i + 1)
  in
  scan 0

let test_truncate_tool_content () =
  let huge = String.make 30_000 'x' in
  let truncated = Protection.truncate_tool_content huge in
  assert (String.length truncated < 14_000);
  assert (String.length truncated > 12_000);
  assert (String.sub truncated 0 5 = "xxxxx");
  assert (String.sub truncated (String.length truncated - 5) 5 = "xxxxx");
  assert (contains truncated "[truncated");
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

let test_parallel_tool_batch_dispatched () =
  (* LLM emits 3 tool_use blocks → agent should send Tool_calls_batch effect.
     Mock returns the configured per-tool result for each. Verify all three
     come back (preserving order). *)
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

let test_classify_status_codes () =
  let module E = Llm_error in
  assert (
    match E.classify_status ~status:429 ~body:"slow down" ~retry_after:(Some 5.) with
    | E.Rate_limit { retry_after = Some 5.; _ } -> true
    | _ -> false);
  assert (
    match E.classify_status ~status:529 ~body:"overloaded" ~retry_after:None with
    | E.Overloaded _ -> true
    | _ -> false);
  assert (
    match E.classify_status ~status:500 ~body:"oops" ~retry_after:None with
    | E.Server_error { status = 500; _ } -> true
    | _ -> false);
  assert (
    match E.classify_status ~status:401 ~body:"no" ~retry_after:None with
    | E.Auth _ -> true
    | _ -> false);
  assert (
    match E.classify_status ~status:404 ~body:"gone" ~retry_after:None with
    | E.Not_found _ -> true
    | _ -> false);
  assert (
    match
      E.classify_status ~status:400
        ~body:"prompt is too long for the model's context window"
        ~retry_after:None
    with
    | E.Context_window _ -> true
    | _ -> false);
  assert (E.is_retryable (E.Rate_limit { retry_after = None; message = "" }));
  assert (E.is_retryable (E.Server_error { status = 503; message = "" }));
  assert (not (E.is_retryable (E.Auth "")));
  assert (not (E.is_retryable (E.Bad_request "")));
  assert (not (E.is_retryable (E.Context_window "")));
  print_endline "✓ Llm_error.classify_status buckets correctly"
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
  (* LLM calls ask_user → run_session should produce Outcome_waiting. *)
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
  (* Build a session with a pending ask_user, then resume with user input.
     The agent should receive a Tool_result block (not a Text block) and
     produce a final answer. *)
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
  (* Last message in session should be a Tool_result, not a fresh user text. *)
  let last_msg = List.nth session.messages (List.length session.messages - 1) in
  (match last_msg.content with
  | [ Tool_result { tool_use_id; content; _ } ] ->
      assert (Id.Tool_use_id.to_string tool_use_id = "toolu_q");
      assert (content = "/tmp/x.py")
  | _ -> failwith "expected Tool_result block");
  print_endline "✓ Session.append_input answers pending ask_user"

let test_subagent_isolated_history () =
  (* Verify delegate.handler runs Agent.run with the task as the FIRST user
     message (not polluted by parent context). We use a custom handler
     that records what messages reach Llm_complete, then runs delegate
     directly. *)
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

let test_run_for_task_explicit_submit () =
  (* LLM calls submit_task_result. run_for_task returns Task_done_explicit
     with the parsed payload — without executing submit_task_result.handler. *)
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
  (* Model just answers without calling submit. run_for_task returns
     Task_done_implicit with the natural answer text. *)
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

let test_skill_parse_basic () =
  let text =
    "---\n\
     name: react-testing\n\
     description: |\n\
    \  React Testing Library best practices.\n\
    \  Use when writing component tests.\n\
     ---\n\
     # body\n\n\
     content here"
  in
  let s = Skill.parse_text text in
  assert (s.name = "react-testing");
  assert (
    String.length s.description > 0
    && String.starts_with ~prefix:"React Testing" s.description);
  assert (String.starts_with ~prefix:"# body" s.body);
  print_endline "✓ Skill.parse_text reads YAML frontmatter + body"

let test_skill_parse_missing_name () =
  let text = "---\ndescription: foo\n---\nbody" in
  match
    try Ok (Skill.parse_text text)
    with Skill.Parse_error msg -> Error msg
  with
  | Error _ ->
      print_endline "✓ Skill.parse_text rejects file with no `name`"
  | Ok _ -> failwith "expected Parse_error for missing name"

let test_skill_render_index_empty () =
  assert (Skill.render_index [] = "");
  print_endline "✓ Skill.render_index returns empty string when no skills"

let test_skill_render_index_with_skills () =
  let skills =
    [
      Skill.{
        name = "alpha";
        description = "First skill.\nLong description.";
        body = "...";
        source_path = "";
      };
      Skill.{
        name = "beta";
        description = "Second skill.";
        body = "...";
        source_path = "";
      };
    ]
  in
  let idx = Skill.render_index skills in
  assert (String.length idx > 0);
  (* render_index returns RAW body — the caller wraps via add_system_block.
     So <available_skills> tag should NOT be in the body. *)
  assert (not (contains idx "<available_skills>"));
  assert (contains idx "**alpha**");
  assert (contains idx "**beta**");
  assert (contains idx "First skill.");
  print_endline "✓ Skill.render_index produces well-formed index body"

let test_load_skill_tool_dispatch () =
  let skills =
    [
      Skill.{
        name = "demo";
        description = "demo skill";
        body = "BODY CONTENT";
        source_path = "";
      };
    ]
  in
  let tool = Skill.make_load_skill_tool skills in
  assert (tool.name = "load_skill");
  (match tool.handler (`Assoc [ ("name", `String "demo") ]) with
  | Ok body -> assert (body = "BODY CONTENT")
  | Error e -> failwith ("unexpected error: " ^ e));
  (match tool.handler (`Assoc [ ("name", `String "missing") ]) with
  | Error msg ->
      assert (
        let needle = "Unknown skill" in
        let nlen = String.length needle in
        let mlen = String.length msg in
        let rec loop i =
          if i + nlen > mlen then false
          else if String.sub msg i nlen = needle then true
          else loop (i + 1)
        in
        loop 0)
  | Ok _ -> failwith "expected Unknown skill error");
  print_endline
    "✓ load_skill tool returns body on hit, lists available on miss"

let test_load_skill_tool_memoizes_within_run () =
  let skills =
    [
      Skill.{
        name = "demo";
        description = "demo skill";
        body = "BIG BODY CONTENT";
        source_path = "";
      };
      Skill.{
        name = "other";
        description = "other skill";
        body = "OTHER BODY";
        source_path = "";
      };
    ]
  in
  let tool = Skill.make_load_skill_tool skills in
  (* First call returns full body. *)
  (match tool.handler (`Assoc [ ("name", `String "demo") ]) with
  | Ok body -> assert (body = "BIG BODY CONTENT")
  | Error e -> failwith ("first call: " ^ e));
  (* Second call to SAME skill returns short stub, not body. *)
  (match tool.handler (`Assoc [ ("name", `String "demo") ]) with
  | Ok msg ->
      assert (msg <> "BIG BODY CONTENT");
      assert (
        let needle = "already loaded" in
        let nlen = String.length needle in
        let mlen = String.length msg in
        let rec loop i =
          if i + nlen > mlen then false
          else if String.sub msg i nlen = needle then true
          else loop (i + 1)
        in
        loop 0)
  | Error e -> failwith ("second call: " ^ e));
  (* DIFFERENT skill on the same tool still returns full body. *)
  (match tool.handler (`Assoc [ ("name", `String "other") ]) with
  | Ok body -> assert (body = "OTHER BODY")
  | Error e -> failwith ("other call: " ^ e));
  (* And a fresh tool instance has independent memo. *)
  let tool2 = Skill.make_load_skill_tool skills in
  (match tool2.handler (`Assoc [ ("name", `String "demo") ]) with
  | Ok body -> assert (body = "BIG BODY CONTENT")
  | Error e -> failwith ("fresh tool: " ^ e));
  print_endline
    "✓ load_skill memoizes per-tool: 2nd call to same skill returns stub"

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

(* ===== Conversation (smart constructor + validate) tests =====

   These cover the dangling-tool_use bug class that bit us today —
   accumulating message history across plan-act tasks where the previous
   task's submit_task_result tool_use was never paired with a tool_result. *)

let test_conversation_empty_state () =
  let c = Speedjs.Conversation.empty in
  assert (Speedjs.Conversation.length c = 0);
  assert (Speedjs.Conversation.is_empty c);
  assert (not (Speedjs.Conversation.is_dangling c));
  print_endline "✓ Conversation.empty starts empty"

let test_conversation_push_user_then_assistant_text () =
  let c = Speedjs.Conversation.empty in
  let c = Speedjs.Conversation.push_user_text c "hi" in
  assert (not (Speedjs.Conversation.is_empty c));
  assert (Speedjs.Conversation.length c = 1);
  let c = Speedjs.Conversation.push_assistant c [ Text "hello" ] in
  assert (Speedjs.Conversation.length c = 2);
  assert (not (Speedjs.Conversation.is_dangling c));
  print_endline "✓ Conversation: User → Assistant text-only transitions cleanly"

let test_conversation_push_assistant_with_tool_use () =
  let c = Speedjs.Conversation.empty in
  let c = Speedjs.Conversation.push_user_text c "go" in
  let c =
    Speedjs.Conversation.push_assistant c
      [ Tool_use { id = Id.Tool_use_id.of_string "u1"; name = "tool"; input = `Assoc [] } ]
  in
  assert (Speedjs.Conversation.is_dangling c);
  assert (
    List.map Id.Tool_use_id.to_string
      (Speedjs.Conversation.pending_tool_use_ids c)
    = [ "u1" ]);
  print_endline "✓ Conversation: Assistant with Tool_use → S_has_dangling"

let test_conversation_close_dangling_with_ack () =
  let c = Speedjs.Conversation.empty in
  let c = Speedjs.Conversation.push_user_text c "go" in
  let c =
    Speedjs.Conversation.push_assistant c
      [
        Text "submitting";
        Tool_use { id = Id.Tool_use_id.of_string "u1"; name = "submit"; input = `Assoc [] };
      ]
  in
  assert (Speedjs.Conversation.is_dangling c);
  let c =
    Speedjs.Conversation.close_dangling_with_ack ~ack:"ok"
      ~extra:[ Text "next task" ] c
  in
  assert (not (Speedjs.Conversation.is_dangling c));
  let msgs = Speedjs.Conversation.to_messages c in
  (* Last message should be a single User turn merging tool_result + text. *)
  let last = List.nth msgs (List.length msgs - 1) in
  assert (last.role = User);
  (match last.content with
  | [ Tool_result { tool_use_id; content = "ok"; is_error = false };
      Text "next task" ]
    when Id.Tool_use_id.to_string tool_use_id = "u1" ->
      ()
  | _ -> failwith "close_dangling_with_ack didn't produce single merged User turn");
  print_endline
    "✓ Conversation.close_dangling_with_ack merges tool_result + text into one User turn"

let test_conversation_push_user_rejects_dangling () =
  let c = Speedjs.Conversation.empty in
  let c = Speedjs.Conversation.push_user_text c "go" in
  let c =
    Speedjs.Conversation.push_assistant c
      [ Tool_use { id = Id.Tool_use_id.of_string "u1"; name = "tool"; input = `Assoc [] } ]
  in
  (try
     let _ = Speedjs.Conversation.push_user_text c "next task" in
     failwith "push_user should have raised on dangling"
   with Speedjs.Conversation.Invariant_violated _ -> ());
  print_endline "✓ Conversation.push_user raises when dangling tool_use exists"

let test_conversation_validate_catches_dangling_pattern () =
  (* The bad message list that produces 500 server_error mid-stream: an
     Assistant turn with a tool_use, followed by a fresh User text turn
     (no matching tool_result). of_messages must reject it. *)
  let bad : message list =
    [
      user_text_message "task 1";
      {
        role = Assistant;
        content =
          [
            Text "done";
            Tool_use
              { id = Id.Tool_use_id.of_string "u1"; name = "submit_task_result"; input = `Assoc [] };
          ];
      };
      user_text_message "task 2";
    ]
  in
  (match Speedjs.Conversation.validate bad with
  | Ok () -> failwith "validate should have rejected dangling-then-user-text"
  | Error msg ->
      assert (
        String.length msg > 0
        &&
        let lower = String.lowercase_ascii msg in
        contains lower "dangling" || contains lower "answered"));
  print_endline "✓ Conversation.validate rejects dangling-tool_use pattern"

let test_conversation_alternation_violation () =
  let bad : message list =
    [
      user_text_message "first";
      user_text_message "second";  (* two consecutive User turns *)
    ]
  in
  (match Speedjs.Conversation.validate bad with
  | Ok () -> failwith "validate should have rejected consecutive User turns"
  | Error _ -> ());
  print_endline "✓ Conversation.validate rejects consecutive same-role turns"

let test_conversation_first_must_be_user () =
  let bad : message list = [ assistant_text_message "hi from nowhere" ] in
  (match Speedjs.Conversation.validate bad with
  | Ok () -> failwith "validate should have rejected Assistant-first"
  | Error _ -> ());
  print_endline "✓ Conversation.validate rejects Assistant as first turn"

let test_conversation_tool_use_in_user_rejected () =
  let bad : message list =
    [
      {
        role = User;
        content =
          [ Tool_use { id = Id.Tool_use_id.of_string "u1"; name = "x"; input = `Assoc [] } ];
      };
    ]
  in
  (match Speedjs.Conversation.validate bad with
  | Ok () -> failwith "validate should have rejected Tool_use in User turn"
  | Error _ -> ());
  print_endline "✓ Conversation.validate rejects Tool_use in User turn"

let test_conversation_round_trip_legitimate_dangling () =
  (* Persisted executor memory after Task_terminal_called legitimately
     ends with dangling tool_use. of_messages must ACCEPT this. *)
  let legit : message list =
    [
      user_text_message "task 1";
      {
        role = Assistant;
        content =
          [
            Text "done!";
            Tool_use
              { id = Id.Tool_use_id.of_string "u_submit"; name = "submit_task_result"; input = `Assoc [] };
          ];
      };
    ]
  in
  match Speedjs.Conversation.of_messages legit with
  | Error msg ->
      failwith
        ("of_messages should accept trailing dangling, got error: " ^ msg)
  | Ok c ->
      assert (Speedjs.Conversation.is_dangling c);
      assert (
        List.map Id.Tool_use_id.to_string
          (Speedjs.Conversation.pending_tool_use_ids c)
        = [ "u_submit" ]);
      print_endline
        "✓ Conversation.of_messages accepts trailing dangling (legitimate persisted state)"

(* ===== Context (structured input + strategy) tests ===== *)

let test_context_renders_system_with_env () =
  let ctx =
    Speedjs.Context.empty
    |> Speedjs.Context.with_system_prompt "You are a helpful agent."
    |> Speedjs.Context.with_env ~tag:"workspace_brief"
         ~body:"Project root: /tmp/x"
    |> Speedjs.Context.with_env ~tag:"current_time" ~body:"2026-05-01"
  in
  let s = Speedjs.Context.render_system ctx in
  assert (contains s "You are a helpful agent.");
  assert (contains s "<workspace_brief>");
  assert (contains s "<current_time>");
  print_endline "✓ Context.render_system composes system + env blocks"

let test_context_system_blocks_ordering () =
  (* Ordering invariant: render = base + system_blocks (registration order)
     + env_blocks (registration order). Critical for prompt-cache stability:
     extensions added later don't invalidate earlier blocks. *)
  let ctx =
    Speedjs.Context.empty
    |> Speedjs.Context.with_system_prompt "BASE"
    |> Speedjs.Context.add_system_block ~name:"first_block" ~body:"FIRST"
    |> Speedjs.Context.add_system_block ~name:"second_block" ~body:"SECOND"
    |> Speedjs.Context.with_env ~tag:"env_block" ~body:"ENV"
  in
  let s = Speedjs.Context.render_system ctx in
  let pos sub =
    let n = String.length sub and m = String.length s in
    let rec loop i =
      if i + n > m then -1
      else if String.sub s i n = sub then i
      else loop (i + 1)
    in
    loop 0
  in
  let p_base = pos "BASE" in
  let p_first = pos "FIRST" in
  let p_second = pos "SECOND" in
  let p_env = pos "ENV" in
  assert (p_base >= 0);
  assert (p_first > p_base);
  assert (p_second > p_first);
  assert (p_env > p_second);
  print_endline
    "✓ Context.render_system: base < system_blocks < env (registration \
     order)"

let test_context_add_tool_appends () =
  let mk_tool name : Speedjs.Types.tool_def =
    {
      idempotent = true;
      timeout_sec = None;
      category = "test";
      name;
      description = "";
      input_schema = `Assoc [ ("type", `String "object") ];
      handler = (fun _ -> Ok "");
    }
  in
  let ctx =
    Speedjs.Context.empty
    |> Speedjs.Context.add_tool (mk_tool "a")
    |> Speedjs.Context.add_tools [ mk_tool "b"; mk_tool "c" ]
    |> Speedjs.Context.add_tool (mk_tool "d")
  in
  let names =
    List.map
      (fun (t : Speedjs.Types.tool_def) -> t.name)
      (Speedjs.Context.tools ctx)
  in
  assert (names = [ "a"; "b"; "c"; "d" ]);
  print_endline "✓ Context.add_tool / add_tools preserve insertion order"

let test_context_to_llm_args_carries_strategy () =
  let conv =
    Speedjs.Conversation.empty
    |> (fun c -> Speedjs.Conversation.push_user_text c "first")
    |> (fun c -> Speedjs.Conversation.push_assistant c [ Text "ack 1" ])
    |> (fun c -> Speedjs.Conversation.push_user_text c "second")
    |> (fun c -> Speedjs.Conversation.push_assistant c [ Text "ack 2" ])
    |> fun c -> Speedjs.Conversation.push_user_text c "third"
  in
  let ctx =
    Speedjs.Context.empty |> Speedjs.Context.with_conversation conv
  in
  let flat = Speedjs.Context.to_llm_args ctx in
  assert (List.length flat.messages = 5);
  let windowed =
    Speedjs.Context.to_llm_args
      ~strategy:(Speedjs.Context.Strategy.sliding_window ~keep_recent:2)
      ctx
  in
  (* Expect 3: synthetic User marker + last 2 kept (Assistant, User). *)
  assert (List.length windowed.messages = 3);
  assert ((List.hd windowed.messages).role = User);
  print_endline "✓ Context.to_llm_args applies sliding_window strategy"

(* Helpers for sliding_window tests: build realistic ReAct-style turn
   sequences. After [user_text q], pairs alternate Assistant tool_use /
   User tool_result. *)
let mk_user_text s : Speedjs.Types.message =
  { role = User; content = [ Text s ] }

let mk_asst_tool_use (id : string) : Speedjs.Types.message =
  {
    role = Assistant;
    content =
      [
        Tool_use
          {
            id = Id.Tool_use_id.of_string id;
            name = "noop";
            input = `Assoc [ ("k", `String id) ];
          };
      ];
  }

let mk_user_tool_result (id : string) : Speedjs.Types.message =
  {
    role = User;
    content =
      [
        Tool_result
          {
            tool_use_id = Id.Tool_use_id.of_string id;
            content = "ok " ^ id;
            is_error = false;
          };
      ];
  }

(** Build a list of [n] messages: User-text query, then alternating
    Assistant tool_use / User tool_result pairs. Total length [n]
    starting with one User text turn. *)
let build_react_msgs ~n =
  if n <= 0 then []
  else
    let head = mk_user_text "Q" in
    let rec pairs k acc =
      if k <= 0 then List.rev acc
      else
        let id = Printf.sprintf "u%d" k in
        pairs (k - 1) (mk_user_tool_result id :: mk_asst_tool_use id :: acc)
    in
    head :: pairs ((n - 1) / 2) []
    |> fun l -> if List.length l > n then List.filteri (fun i _ -> i < n) l else l

let truncated_marker_prefix = "[earlier conversation truncated"

let is_truncated_marker (m : Speedjs.Types.message) =
  m.role = User
  && (match m.content with
      | [ Text s ] ->
          String.length s >= String.length truncated_marker_prefix
          && String.sub s 0 (String.length truncated_marker_prefix)
             = truncated_marker_prefix
      | _ -> false)

let test_sliding_window_at_below_trigger_passes_through () =
  let strat =
    Speedjs.Context.Strategy.sliding_window_at ~trigger_at:10 ~keep_recent:4
  in
  let msgs = build_react_msgs ~n:6 in
  let out = strat msgs in
  assert (out = msgs);
  print_endline
    "✓ sliding_window_at: below trigger returns messages unchanged"

let test_sliding_window_at_first_trim_invariants () =
  let strat =
    Speedjs.Context.Strategy.sliding_window_at ~trigger_at:6 ~keep_recent:3
  in
  let msgs = build_react_msgs ~n:11 in
  let out = strat msgs in
  (* Invariant 1: shorter than input. *)
  assert (List.length out < List.length msgs);
  (* Invariant 2: first message is the truncated marker (User text). *)
  assert (is_truncated_marker (List.hd out));
  (* Invariant 3: second message is Assistant (no orphan
     User-tool_result at front after marker). *)
  assert ((List.nth out 1).role = Assistant);
  (* Invariant 4: no Tool_result blocks in out[1..] map to dropped
     Tool_use ids. Because we kept a contiguous suffix and
     drop_leading_user discarded any orphans. *)
  let kept_use_ids = ref [] in
  List.iter
    (fun (m : Speedjs.Types.message) ->
      List.iter
        (function
          | Speedjs.Types.Tool_use { id; _ } ->
              kept_use_ids := id :: !kept_use_ids
          | _ -> ())
        m.content)
    out;
  List.iter
    (fun (m : Speedjs.Types.message) ->
      List.iter
        (function
          | Speedjs.Types.Tool_result { tool_use_id; _ } ->
              assert (List.mem tool_use_id !kept_use_ids)
          | _ -> ())
        m.content)
    out;
  print_endline
    "✓ sliding_window_at: first trim adds marker, drops orphans, keeps \
     User-first invariant"

let test_sliding_window_at_freezes_cut_anchor () =
  (* After the first trim, repeated calls with the SAME message list
     must return the SAME result. AND when 2 more turns are appended,
     the kept suffix must START at the SAME original index — meaning
     the message right after the marker is the SAME object as before,
     proving the cache prefix is stable. *)
  let strat =
    Speedjs.Context.Strategy.sliding_window_at ~trigger_at:6 ~keep_recent:3
  in
  let msgs1 = build_react_msgs ~n:11 in
  let out1 = strat msgs1 in
  let out1_again = strat msgs1 in
  assert (List.length out1 = List.length out1_again);
  let first_kept_a = List.nth out1 1 in
  let first_kept_b = List.nth out1_again 1 in
  assert (first_kept_a == first_kept_b);
  (* Extend by 2 messages — should NOT re-trim because effective is
     still under trigger. The first kept message must STILL be the
     same physical message (cache prefix preserved). *)
  let extra =
    [ mk_asst_tool_use "ext1"; mk_user_tool_result "ext1" ]
  in
  let msgs2 = msgs1 @ extra in
  let out2 = strat msgs2 in
  assert (is_truncated_marker (List.hd out2));
  let first_kept_c = List.nth out2 1 in
  assert (first_kept_c == first_kept_a);
  print_endline
    "✓ sliding_window_at: cut anchor frozen across calls (cache prefix \
     stable)"

let test_sliding_window_at_retrims_when_effective_exceeds_again () =
  (* Once the un-cut suffix grows past trigger again, cut_at advances:
     a NEW first-kept message replaces the old one. This is the only
     time the cache prefix changes. *)
  let strat =
    Speedjs.Context.Strategy.sliding_window_at ~trigger_at:6 ~keep_recent:3
  in
  let msgs1 = build_react_msgs ~n:11 in
  let out1 = strat msgs1 in
  let first_after_marker_1 = List.nth out1 1 in
  (* Add 8 more turns — effective definitely exceeds trigger again. *)
  let rec append_pairs k acc =
    if k <= 0 then acc
    else
      let id = Printf.sprintf "x%d" k in
      append_pairs (k - 1)
        (acc @ [ mk_asst_tool_use id; mk_user_tool_result id ])
  in
  let msgs2 = append_pairs 4 msgs1 in
  let out2 = strat msgs2 in
  let first_after_marker_2 = List.nth out2 1 in
  assert (not (first_after_marker_1 == first_after_marker_2));
  assert (is_truncated_marker (List.hd out2));
  print_endline
    "✓ sliding_window_at: re-trims (advances cut_at) when effective \
     length crosses trigger again"

let test_sliding_window_at_factory_independent_state () =
  (* Two factory invocations must produce strategies with independent
     [cut_at] state — calling one must not affect the other. *)
  let make () =
    Speedjs.Context.Strategy.sliding_window_at ~trigger_at:6 ~keep_recent:3
  in
  let s1 = make () in
  let s2 = make () in
  let trigger_msgs = build_react_msgs ~n:11 in
  let small_msgs = build_react_msgs ~n:4 in
  let _ = s1 trigger_msgs in
  (* s2 should still be in fresh state — small input passes through. *)
  let out_s2 = s2 small_msgs in
  assert (out_s2 = small_msgs);
  print_endline
    "✓ sliding_window_at: factory yields independent per-instance state"

(* ===== Llm_handler chain (composable middleware) tests ===== *)

let test_llm_handler_chain_validates_messages () =
  (* with_validation should reject malformed messages BEFORE the inner
     handler is invoked, raising Bad_request. *)
  let inner_called = ref false in
  let inner : Llm_handler.t =
   fun _args ->
    inner_called := true;
    {
      content = [ Text "ok" ];
      stop_reason = End_turn;
      usage = usage_of_basic ~input_tokens:1 ~output_tokens:1;
    }
  in
  let chain = Llm_handler.with_validation inner in
  let bad_args : llm_call_args =
    {
      messages = [ assistant_text_message "first must be User, not Assistant" ];
      tools = [];
      system_override = None;
      tool_choice = Tc_auto;
    }
  in
  (try
     let _ = chain bad_args in
     failwith "expected Bad_request"
   with Llm_error.Llm_api_error (Llm_error.Bad_request _) -> ());
  assert (not !inner_called);
  print_endline "✓ Llm_handler.with_validation rejects malformed messages"

let test_llm_handler_chain_cost_tracking () =
  let inner : Llm_handler.t =
   fun _args ->
    {
      content = [ Text "hi" ];
      stop_reason = End_turn;
      usage =
        {
          input_tokens = 100;
          output_tokens = 20;
          cache_creation_input_tokens = 50;
          cache_read_input_tokens = 80;
        };
    }
  in
  let cost = new_cost_state () in
  let chain = inner |> Llm_handler.with_cost_tracking ~cost in
  let _ =
    chain
      {
        messages = [ user_text_message "hi" ];
        tools = [];
        system_override = None;
        tool_choice = Tc_auto;
      }
  in
  let _ =
    chain
      {
        messages = [ user_text_message "again" ];
        tools = [];
        system_override = None;
        tool_choice = Tc_auto;
      }
  in
  assert (cost.calls = 2);
  assert (cost.input_tokens = 200);
  assert (cost.output_tokens = 40);
  assert (cost.cache_creation_tokens = 100);
  assert (cost.cache_read_tokens = 160);
  print_endline "✓ Llm_handler.with_cost_tracking accumulates usage"

let test_llm_handler_chain_retry_recovers () =
  let attempts = ref 0 in
  let inner : Llm_handler.t =
   fun _args ->
    incr attempts;
    if !attempts < 3 then
      raise
        (Llm_error.Llm_api_error
           (Llm_error.Rate_limit { retry_after = None; message = "slow down" }))
    else
      {
        content = [ Text "ok at last" ];
        stop_reason = End_turn;
        usage = usage_of_basic ~input_tokens:1 ~output_tokens:1;
      }
  in
  let chain =
    inner
    |> Llm_handler.with_retry
         ~policy:
           {
             max_attempts = 5;
             base_delay = 0.01;
             cap = 0.05;
           }
  in
  let response =
    chain
      {
        messages = [ user_text_message "hi" ];
        tools = [];
        system_override = None;
        tool_choice = Tc_auto;
      }
  in
  assert (!attempts = 3);
  (match response.content with
  | [ Text "ok at last" ] -> ()
  | _ -> failwith "expected text content");
  print_endline "✓ Llm_handler.with_retry retries Rate_limit and recovers"

let test_llm_handler_chain_retry_fails_fast_on_auth () =
  let attempts = ref 0 in
  let inner : Llm_handler.t =
   fun _args ->
    incr attempts;
    raise (Llm_error.Llm_api_error (Llm_error.Auth "bad token"))
  in
  let chain = inner |> Llm_handler.with_retry in
  (try
     let _ =
       chain
         {
           messages = [ user_text_message "hi" ];
           tools = [];
           system_override = None;
           tool_choice = Tc_auto;
         }
     in
     failwith "expected Auth to be re-raised"
   with Llm_error.Llm_api_error (Llm_error.Auth _) -> ());
  assert (!attempts = 1);
  print_endline "✓ Llm_handler.with_retry fails fast on non-retryable Auth"

let test_llm_handler_install_intercepts_effect () =
  (* End-to-end: install a chain, run an Agent.run, verify the chain
     handler was invoked with the messages the agent built. *)
  let captured = ref None in
  let inner : Llm_handler.t =
   fun args ->
    captured := Some args;
    {
      content = [ Text "from chain" ];
      stop_reason = End_turn;
      usage = usage_of_basic ~input_tokens:1 ~output_tokens:1;
    }
  in
  let result =
    Llm_handler.install inner (fun () ->
        Handlers.silent (fun () -> Agent.run ~user_query:"test" ~tools:[] ()))
  in
  (match result with
  | Ok "from chain" -> ()
  | Ok s -> failwith ("unexpected answer: " ^ s)
  | Error e -> failwith ("unexpected error: " ^ agent_error_pp e));
  (match !captured with
  | Some args ->
      assert (List.length args.messages = 1);
      assert (args.tools = [])
  | None -> failwith "chain handler was never invoked");
  print_endline "✓ Llm_handler.install intercepts Llm_complete effect"

let test_llm_handler_compaction_on_overflow () =
  let calls = ref 0 in
  let inner : Llm_handler.t =
   fun args ->
    incr calls;
    if !calls = 1 then
      raise
        (Llm_error.Llm_api_error
           (Llm_error.Context_window "input is too long"))
    else
      {
        content =
          [
            Text
              (Printf.sprintf "ok with %d msgs" (List.length args.messages));
          ];
        stop_reason = End_turn;
        usage = usage_of_basic ~input_tokens:1 ~output_tokens:1;
      }
  in
  let chain =
    inner
    |> Llm_handler.with_compaction_on_overflow ~keep_recent:1
         ~compactor:(fun msgs ->
           Printf.sprintf "summary of %d msgs" (List.length msgs))
  in
  let messages =
    List.init 5 (fun i -> user_text_message (Printf.sprintf "msg %d" i))
  in
  let response =
    chain
      {
        messages;
        tools = [];
        system_override = None;
        tool_choice = Tc_auto;
      }
  in
  assert (!calls = 2);
  (* compacted = 1 summary + 1 recent = 2 messages *)
  (match response.content with
  | [ Text s ] -> assert (contains s "2 msgs")
  | _ -> failwith "expected response from second attempt");
  print_endline
    "✓ Llm_handler.with_compaction_on_overflow retries with compacted messages"

(* ===== Log_handler tests ===== *)

let test_log_handler_chain () =
  let captured = ref [] in
  let chain =
    Log_handler.to_function (fun s -> captured := s :: !captured)
    |> Log_handler.with_prefix "[test] "
    |> Log_handler.with_filter ~accept:(fun s ->
           not (String.length s > 4 && String.sub s 0 5 = "skip:"))
  in
  Log_handler.install chain (fun () ->
      Effect.perform (Effects.Log "hello");
      Effect.perform (Effects.Log "skip: noise");
      Effect.perform (Effects.Log "world"));
  assert (List.rev !captured = [ "[test] hello"; "[test] world" ]);
  print_endline "✓ Log_handler chain (prefix + filter + install) works"

(* ===== Stability gap probes — confirm absence of guards we discussed ===== *)

(** Regression: spawn_curl with [max_time_sec] caps the whole transfer.
    With unreachable proxy curl exits within the cap. *)
let test_curl_max_time_caps_transfer () =
  let pid, stdout_r, stderr_r =
    Speedjs.Http.spawn_curl ~max_time_sec:2.0 ~proxy:"127.0.0.1:1"
      ~url:"http://example.invalid" ~headers:[] ~body:"x" ~stream:false
      ()
  in
  let start = Unix.gettimeofday () in
  let _ = Speedjs.Http.read_all stdout_r in
  let _ = Speedjs.Http.read_all stderr_r in
  let _ = Unix.waitpid [] pid in
  let elapsed = Unix.gettimeofday () -. start in
  Unix.close stdout_r;
  Unix.close stderr_r;
  (* Either connects fail fast OR --max-time kicks in. Both bound
     under the cap. *)
  assert (elapsed < 3.0);
  print_endline
    (Printf.sprintf
       "✓ curl --max-time caps transfer at %.0fs cap (actual %.3fs)" 2.0
       elapsed)

(** Regression: [read_line_with_deadlines] raises
    [Stream_first_byte_timeout] when nothing arrives. We pipe a
    short-lived subprocess that writes nothing then sleeps. *)
let test_stream_first_byte_timeout () =
  let stdin_r, stdin_w = Unix.pipe () in
  let stdout_r, stdout_w = Unix.pipe () in
  let pid =
    Unix.create_process "sleep"
      [| "sleep"; "5" |]
      stdin_r stdout_w Unix.stderr
  in
  Unix.close stdin_r;
  Unix.close stdout_w;
  Unix.close stdin_w;
  let reader =
    Speedjs.Http.make_line_reader ~first_byte_sec:0.2 ~idle_sec:5.0
      stdout_r
  in
  let start = Unix.gettimeofday () in
  (try
     let _ = Speedjs.Http.read_line_with_deadlines reader in
     failwith "expected first-byte timeout"
   with Speedjs.Http.Stream_first_byte_timeout sec -> assert (sec = 0.2));
  let elapsed = Unix.gettimeofday () -. start in
  assert (elapsed < 1.0);
  (try Unix.kill pid Sys.sigkill with _ -> ());
  let _ = Unix.waitpid [] pid in
  Unix.close stdout_r;
  print_endline
    (Printf.sprintf
       "✓ Http.read_line_with_deadlines fires Stream_first_byte_timeout (%.3fs)"
       elapsed)

(** Regression: idle timeout fires after first byte then silence. *)
let test_stream_idle_timeout () =
  (* `printf 'a\n'; sleep 5` — sends one line then goes silent. *)
  let stdin_r, stdin_w = Unix.pipe () in
  let stdout_r, stdout_w = Unix.pipe () in
  let pid =
    Unix.create_process "sh"
      [| "sh"; "-c"; "printf 'hi\\n'; sleep 5" |]
      stdin_r stdout_w Unix.stderr
  in
  Unix.close stdin_r;
  Unix.close stdout_w;
  Unix.close stdin_w;
  let reader =
    Speedjs.Http.make_line_reader ~first_byte_sec:5.0 ~idle_sec:0.2
      stdout_r
  in
  (* First read returns "hi" *)
  let line = Speedjs.Http.read_line_with_deadlines reader in
  assert (line = "hi");
  (* Second read should timeout on idle *)
  let start = Unix.gettimeofday () in
  (try
     let _ = Speedjs.Http.read_line_with_deadlines reader in
     failwith "expected idle timeout"
   with Speedjs.Http.Stream_idle_timeout sec -> assert (sec = 0.2));
  let elapsed = Unix.gettimeofday () -. start in
  assert (elapsed < 1.0);
  (try Unix.kill pid Sys.sigkill with _ -> ());
  let _ = Unix.waitpid [] pid in
  Unix.close stdout_r;
  print_endline
    (Printf.sprintf
       "✓ Http.read_line_with_deadlines fires Stream_idle_timeout after \
        first byte (%.3fs)"
       elapsed)

(** Regression: MCP read_line raises [Read_timeout] when stdout has no
    data within the cap. Spawns [sleep] (UNIX coreutil) — outputs
    nothing for the duration. *)
let test_mcp_read_timeout () =
  let conn =
    Speedjs.Mcp.start ~read_timeout_sec:0.3 ~cmd:"sleep" ~args:[ "10" ]
      ()
  in
  let start = Unix.gettimeofday () in
  (match Speedjs.Mcp.read_line conn with
  | exception Speedjs.Mcp.Read_timeout sec -> assert (sec = 0.3)
  | _ -> failwith "expected Read_timeout from a wedged MCP server");
  let elapsed = Unix.gettimeofday () -. start in
  assert (elapsed < 1.0);
  Speedjs.Mcp.shutdown conn;
  print_endline
    (Printf.sprintf
       "✓ MCP read_line raises Read_timeout on wedged server (%.3fs)"
       elapsed)

(** Regression: [Tool_handler.with_timeout] enforces tool.timeout_sec.
    A tool declaring timeout_sec=0.1s and sleeping 1s gets aborted at
    ~0.1s. *)
let test_tool_handler_with_timeout_aborts_slow_tool () =
  let slow_tool : tool_def =
    {
      name = "slow";
      description = "sleeps";
      input_schema = `Assoc [];
      handler =
        (fun _ ->
          Unix.sleepf 1.0;
          Ok "done");
      idempotent = true;
      timeout_sec = Some 0.1;
      category = "test";
    }
  in
  let chain = Tool_handler.direct |> Tool_handler.with_timeout in
  let start = Unix.gettimeofday () in
  let result =
    chain { tool = slow_tool; input = `Assoc []; use_id = Id.Tool_use_id.of_string "u1" }
  in
  let elapsed = Unix.gettimeofday () -. start in
  (match result with
  | Error err when err.code = "timeout" -> ()
  | _ -> failwith "expected timeout error");
  assert (elapsed < 0.5);
  print_endline
    (Printf.sprintf
       "✓ Tool_handler.with_timeout aborts at tool.timeout_sec=0.1s \
        (actual %.3fs)"
       elapsed)

(* ===== Governor tests ===== *)

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
  (* Step 1, 2, 3 pass; step 4 trips the cap. *)
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
         (* depth = 2; next push exceeds cap *)
         Effect.perform (Governor.Tick Subagent_entered));
     failwith "expected Governor_aborted"
   with Governor.Governor_aborted { limit; _ } ->
     assert (limit = "max_subagent_depth"));
  print_endline "✓ Governor enforces sub-agent recursion depth"

(** Recovery prompt includes prior_failures + cycle_index so the
    planner can prefer abandon over similar-shape replans. We exercise
    [Planner.recover] with a mock that returns Abandon and inspect the
    LLM call's user message for the new fields. *)
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
      assert (contains body "Prior recovery failures");
      assert (contains body "install widget");
      assert (contains body "DNS error");
      assert (contains body "Recovery cycle: 1 of max 2");
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
  (* abandon — no tasks *)
  (match Planner.parse_recovery_decision (mk_input "abandon") with
  | Ok Planner.Abandon -> ()
  | _ -> failwith "abandon failed to parse");
  (* skip — tasks ignored *)
  (match Planner.parse_recovery_decision (mk_input "skip") with
  | Ok Planner.Skip -> ()
  | _ -> failwith "skip failed to parse");
  (* replan — needs tasks *)
  (match
     Planner.parse_recovery_decision
       (mk_input ~tasks:[ "step1"; "step2" ] "replan")
   with
  | Ok (Planner.Replan ts) -> assert (List.length ts = 2)
  | _ -> failwith "replan failed to parse");
  (* split — needs tasks *)
  (match
     Planner.parse_recovery_decision
       (mk_input ~tasks:[ "sub1"; "sub2"; "sub3" ] "split")
   with
  | Ok (Planner.Split ts) -> assert (List.length ts = 3)
  | _ -> failwith "split failed to parse");
  (* unknown decision *)
  (match Planner.parse_recovery_decision (mk_input "unknown") with
  | Error _ -> ()
  | _ -> failwith "unknown should reject");
  (* replan with empty tasks should reject *)
  (match Planner.parse_recovery_decision (mk_input "replan") with
  | Error _ -> ()
  | _ -> failwith "empty replan should reject");
  print_endline
    "✓ Planner.parse_recovery_decision handles replan / split / skip / \
     abandon"

(* ===== Tool_handler chain tests ===== *)

let mk_test_tool ?(idempotent = false) ?(timeout_sec = None)
    ?(category = "test") name handler : tool_def =
  {
    name;
    description = "test tool";
    input_schema = `Assoc [];
    handler;
    idempotent;
    timeout_sec;
    category;
  }

let test_tool_handler_direct_classifies_errors () =
  let calls = ref 0 in
  let tool =
    mk_test_tool "fail_tool" (fun _ ->
        incr calls;
        Error "kaboom")
  in
  let args =
    { Tool_handler.tool; input = `Assoc []; use_id = Id.Tool_use_id.of_string "u1" }
  in
  (match Tool_handler.direct args with
  | Error err ->
      assert (err.kind = Permanent);
      assert (
        match err.domain with Tool n -> n = "fail_tool" | _ -> false);
      assert (err.message = "kaboom")
  | Ok _ -> failwith "expected error");
  assert (!calls = 1);
  print_endline "✓ Tool_handler.direct wraps tool error as Permanent Error.t"

let test_tool_handler_validation_rejects_non_object () =
  let inner_called = ref false in
  let tool =
    mk_test_tool "x" (fun _ ->
        inner_called := true;
        Ok "should not reach")
  in
  let chain = Tool_handler.with_validation Tool_handler.direct in
  let args = { Tool_handler.tool; input = `String "not an object"; use_id = Id.Tool_use_id.of_string "u" } in
  (match chain args with
  | Error err ->
      assert (err.kind = Permanent);
      assert (err.domain = Validation);
      assert (err.code = "invalid_input")
  | Ok _ -> failwith "expected validation error");
  assert (not !inner_called);
  print_endline "✓ Tool_handler.with_validation rejects non-object input"

let test_tool_handler_retry_only_for_idempotent () =
  (* Non-idempotent tool: should NOT retry even on Transient. *)
  let calls = ref 0 in
  let bad_tool =
    mk_test_tool ~idempotent:false "writer" (fun _ ->
        incr calls;
        Error "transient blip")
  in
  (* Manually classify as Transient via a custom inner that wraps direct. *)
  let inner : Tool_handler.t =
   fun args ->
    incr calls;
    Error
      (Error.transient ~domain:(Tool args.tool.name)
         ~code:"blip" "transient blip")
  in
  let chain = Tool_handler.with_retry ~policy:{ max_attempts = 5; base_delay = 0.01; cap = 0.05 } inner in
  calls := 0;
  let _ =
    chain
      { tool = bad_tool; input = `Assoc []; use_id = Id.Tool_use_id.of_string "u" }
  in
  assert (!calls = 1);  (* no retry for non-idempotent *)

  (* Idempotent tool: should retry until success or attempts exhausted. *)
  let attempts = ref 0 in
  let inner2 : Tool_handler.t =
   fun args ->
    incr attempts;
    if !attempts < 3 then
      Error
        (Error.transient ~domain:(Tool args.tool.name)
           ~code:"blip" "transient")
    else Ok "got it"
  in
  let chain2 =
    Tool_handler.with_retry
      ~policy:{ max_attempts = 5; base_delay = 0.01; cap = 0.05 }
      inner2
  in
  let good_tool = mk_test_tool ~idempotent:true "reader" (fun _ -> Ok "") in
  attempts := 0;
  let r =
    chain2
      { tool = good_tool; input = `Assoc []; use_id = Id.Tool_use_id.of_string "u" }
  in
  assert (!attempts = 3);
  (match r with Ok "got it" -> () | _ -> failwith "expected success");
  print_endline
    "✓ Tool_handler.with_retry only retries idempotent tools"

let test_tool_handler_circuit_breaker_opens_after_threshold () =
  let inner : Tool_handler.t =
   fun args ->
    Error
      (Error.transient ~domain:(Tool args.tool.name) ~code:"blip" "fail")
  in
  let chain =
    Tool_handler.with_circuit_breaker ~failure_threshold:3 ~cooldown:60.0
      inner
  in
  let tool = mk_test_tool "broken" (fun _ -> Error "x") in
  let args = { Tool_handler.tool; input = `Assoc []; use_id = Id.Tool_use_id.of_string "u" } in
  for _ = 1 to 3 do
    let _ = chain args in
    ()
  done;
  (* 4th call should hit the open breaker, not the inner. *)
  (match chain args with
  | Error err when err.code = "circuit_open" -> ()
  | _ ->
      failwith "expected circuit_open error after threshold breaches");
  print_endline
    "✓ Tool_handler.with_circuit_breaker opens after threshold"

let test_tool_handler_audit_observes_calls () =
  let calls = ref [] in
  let results = ref [] in
  let tool = mk_test_tool "x" (fun _ -> Ok "ok") in
  let chain =
    Tool_handler.with_audit
      ~on_call:(fun args -> calls := args.tool.name :: !calls)
      ~on_result:(fun args r ->
        results := (args.tool.name, Result.is_ok r) :: !results)
      Tool_handler.direct
  in
  let _ =
    chain { tool; input = `Assoc []; use_id = Id.Tool_use_id.of_string "u" }
  in
  assert (!calls = [ "x" ]);
  assert (!results = [ ("x", true) ]);
  print_endline "✓ Tool_handler.with_audit fires on_call + on_result"

(* ===== Typed tool builder ===== *)

let test_make_typed_tool_decodes_input_and_runs_handler () =
  (* A typed tool: input is a record { x; y }, handler adds them, output
     is a string. Demonstrates that the input_decoder runs first and
     errors propagate cleanly without the handler ever seeing bad JSON. *)
  let calls = ref 0 in
  let tool =
    make_typed_tool ~name:"add"
      ~description:"add two ints"
      ~input_schema:
        (`Assoc
          [
            ("type", `String "object");
            ( "properties",
              `Assoc
                [
                  ("x", `Assoc [ ("type", `String "integer") ]);
                  ("y", `Assoc [ ("type", `String "integer") ]);
                ] );
          ])
      ~input_decoder:(fun json ->
        match json with
        | `Assoc fs -> (
            match (List.assoc_opt "x" fs, List.assoc_opt "y" fs) with
            | Some (`Int a), Some (`Int b) -> Ok (a, b)
            | _ -> Error "x and y must be ints")
        | _ -> Error "input must be object")
      ~handler:(fun (a, b) ->
        incr calls;
        Ok (string_of_int (a + b)))
      ()
  in
  (* Happy path: typed handler receives unpacked tuple. *)
  (match tool.handler (`Assoc [ ("x", `Int 5); ("y", `Int 7) ]) with
  | Ok "12" -> ()
  | Ok s -> failwith ("unexpected output: " ^ s)
  | Error e -> failwith ("expected Ok, got Error: " ^ e));
  assert (!calls = 1);
  (* Decoder error: handler never invoked, error wrapped. *)
  (match tool.handler (`String "not an object") with
  | Error msg ->
      assert (
        let needle = "invalid input" in
        let nl = String.length needle and ml = String.length msg in
        let rec loop i =
          if i + nl > ml then false
          else if String.sub msg i nl = needle then true
          else loop (i + 1)
        in
        loop 0)
  | Ok _ -> failwith "expected decoder error to propagate");
  assert (!calls = 1);
  print_endline
    "✓ make_typed_tool: decoder errors short-circuit, handler sees \
     typed input"

let test_tool_handler_install_dispatches_via_chain () =
  let dispatched = ref [] in
  let tool =
    mk_test_tool "echo" (fun input ->
        dispatched := Yojson.Safe.to_string input :: !dispatched;
        Ok "echoed")
  in
  let chain = Tool_handler.with_logging ~on_log:(fun _ -> ()) Tool_handler.direct in
  let result =
    Tool_handler.install ~tools:[ tool ] chain (fun () ->
        Effect.perform
          (Effects.Tool_calls
             [
               ( Id.Tool_use_id.of_string "u1",
                 "echo",
                 `Assoc [ ("v", `Int 42) ] );
             ]))
  in
  assert (List.length result = 1);
  let id, r = List.hd result in
  assert (Id.Tool_use_id.to_string id = "u1");
  (match r with Ok "echoed" -> () | _ -> failwith "expected echoed");
  assert (List.length !dispatched = 1);
  print_endline
    "✓ Tool_handler.install routes Tool_calls effect through chain"

let test_context_compacted_strategy () =
  (* Build a conversation with 6 turns (3 user / 3 assistant). *)
  let conv = ref Speedjs.Conversation.empty in
  for i = 0 to 2 do
    conv :=
      Speedjs.Conversation.push_user_text !conv
        (Printf.sprintf "msg %d" i);
    conv :=
      Speedjs.Conversation.push_assistant !conv
        [ Text (Printf.sprintf "ack %d" i) ]
  done;
  let ctx =
    Speedjs.Context.empty |> Speedjs.Context.with_conversation !conv
  in
  let strategy =
    Speedjs.Context.Strategy.compacted ~compact_at:5 ~keep_recent:2
      ~compactor:(fun msgs ->
        Printf.sprintf "summary of %d msgs" (List.length msgs))
  in
  let args = Speedjs.Context.to_llm_args ~strategy ctx in
  assert (List.length args.messages = 3);
  let first = List.hd args.messages in
  (match first.content with
  | [ Text s ]
    when contains s "compacted_history"
         && contains s "summary of" ->
      ()
  | _ ->
      failwith "expected synthetic compacted summary as first message");
  print_endline "✓ Context.Strategy.compacted folds prefix into summary turn"

let () =
  test_event_log_routes_through_observer_and_string_chain ();
  test_step_returns_terminal_text_on_end_turn ();
  test_step_returns_continue_on_normal_tool_use ();
  test_step_returns_terminal_tool_when_terminal_called ();
  test_step_returns_wait_for_user_on_ask_user ();
  test_step_returns_failed_on_max_tokens ();
  test_simple_text_response ();
  test_single_tool_call ();
  test_tool_error_surfaced ();
  test_max_iterations ();
  test_silent_handler_composition ();
  test_truncate_tool_content ();
  test_parallel_tool_batch_dispatched ();
  test_classify_status_codes ();
  test_run_session_outcome_done ();
  test_run_session_outcome_waiting ();
  test_session_resume_with_tool_result ();
  test_subagent_isolated_history ();
  test_mcp_extract_text_content ();
  test_run_for_task_explicit_submit ();
  test_run_for_task_explicit_failure ();
  test_run_for_task_implicit_end_turn ();
  test_skill_parse_basic ();
  test_skill_parse_missing_name ();
  test_skill_render_index_empty ();
  test_skill_render_index_with_skills ();
  test_load_skill_tool_dispatch ();
  test_load_skill_tool_memoizes_within_run ();
  test_planner_parses_submit_plan ();
  test_planner_invalid_no_submit ();
  test_conversation_empty_state ();
  test_conversation_push_user_then_assistant_text ();
  test_conversation_push_assistant_with_tool_use ();
  test_conversation_close_dangling_with_ack ();
  test_conversation_push_user_rejects_dangling ();
  test_conversation_validate_catches_dangling_pattern ();
  test_conversation_alternation_violation ();
  test_conversation_first_must_be_user ();
  test_conversation_tool_use_in_user_rejected ();
  test_conversation_round_trip_legitimate_dangling ();
  test_context_renders_system_with_env ();
  test_context_system_blocks_ordering ();
  test_context_add_tool_appends ();
  test_context_to_llm_args_carries_strategy ();
  test_sliding_window_at_below_trigger_passes_through ();
  test_sliding_window_at_first_trim_invariants ();
  test_sliding_window_at_freezes_cut_anchor ();
  test_sliding_window_at_retrims_when_effective_exceeds_again ();
  test_sliding_window_at_factory_independent_state ();
  test_context_compacted_strategy ();
  test_llm_handler_chain_validates_messages ();
  test_llm_handler_chain_cost_tracking ();
  test_llm_handler_chain_retry_recovers ();
  test_llm_handler_chain_retry_fails_fast_on_auth ();
  test_llm_handler_install_intercepts_effect ();
  test_llm_handler_compaction_on_overflow ();
  test_log_handler_chain ();
  test_curl_max_time_caps_transfer ();
  test_stream_first_byte_timeout ();
  test_stream_idle_timeout ();
  test_mcp_read_timeout ();
  test_tool_handler_with_timeout_aborts_slow_tool ();
  test_governor_passes_tick_to_observer ();
  test_governor_aborts_on_max_steps ();
  test_governor_detects_death_loop ();
  test_governor_subagent_depth ();
  test_recovery_prompt_includes_prior_failures_and_cycle ();
  test_recovery_parser_handles_all_four_decisions ();
  test_tool_handler_direct_classifies_errors ();
  test_tool_handler_validation_rejects_non_object ();
  test_tool_handler_retry_only_for_idempotent ();
  test_tool_handler_circuit_breaker_opens_after_threshold ();
  test_tool_handler_audit_observes_calls ();
  test_make_typed_tool_decodes_input_and_runs_handler ();
  test_tool_handler_install_dispatches_via_chain ();
  print_endline "\nAll tests passed.\n"
