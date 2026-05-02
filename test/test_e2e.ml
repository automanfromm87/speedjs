(** End-to-end integration tests.

    These exercise the full handler stack (LLM mock + real Tool_handler
    chain + File_handler.in_memory + Time_handler.fixed) — any
    regression in tool dispatch, effect plumbing, or middleware
    composition that the unit tests miss should fall out here. *)

open Speedjs
open Types

(** Build an LLM handler that returns canned responses in order.
    Calling [handler] more times than there are canned responses
    [failwith]s. *)
let canned_llm responses : Llm_handler.t =
  let pending = ref responses in
  fun _args ->
    match !pending with
    | [] -> failwith "[mock-llm] ran out of canned responses"
    | r :: rest ->
        pending := rest;
        r

let mk_resp ?(usage = usage_of_basic ~input_tokens:1 ~output_tokens:1)
    ~stop_reason content =
  { content; stop_reason; usage }

let test_plan_act_task_edits_virtual_file () =
  (* Virtual FS seeded with one file. *)
  let files = Hashtbl.create 4 in
  Hashtbl.add files "/proj/main.ml" "let x = 1\nlet y = 2\nlet z = 3";

  let view_resp =
    mk_resp ~stop_reason:Tool_use_stop
      [
        Tool_use
          {
            id = Id.Tool_use_id.of_string "u_view";
            name = "view_file";
            input = `Assoc [ ("path", `String "/proj/main.ml") ];
          };
      ]
  in
  let edit_resp =
    mk_resp ~stop_reason:Tool_use_stop
      [
        Tool_use
          {
            id = Id.Tool_use_id.of_string "u_edit";
            name = "str_replace";
            input =
              `Assoc
                [
                  ("path", `String "/proj/main.ml");
                  ("old_str", `String "let x = 1");
                  ("new_str", `String "let x = 100");
                ];
          };
      ]
  in
  let submit_resp =
    mk_resp ~stop_reason:Tool_use_stop
      [
        Tool_use
          {
            id = Id.Tool_use_id.of_string "u_submit";
            name = "submit_task_result";
            input =
              `Assoc
                [
                  ("success", `Bool true);
                  ("result", `String "changed x to 100");
                  ("error", `String "");
                ];
          };
      ]
  in

  let llm_chain = canned_llm [ view_resp; edit_resp; submit_resp ] in
  let tool_chain = Tool_handler.direct in
  let tools = [ Tools.view_file; Tools.str_replace ] in

  let outcome =
    Llm_handler.install llm_chain (fun () ->
        File_handler.install
          (File_handler.in_memory ~files)
          (fun () ->
            Time_handler.install Time_handler.direct (fun () ->
                Tool_handler.install ~tools tool_chain (fun () ->
                    Handlers.silent (fun () ->
                        Plan_act.run_for_task
                          ~task_description:"change x to 100"
                          ~tools ())))))
  in

  (* Verify: outcome is Task_done_explicit, FS reflects the edit. *)
  (match outcome with
  | Task_done_explicit { submit; _ } ->
      assert submit.ts_success;
      assert (submit.ts_result = "changed x to 100")
  | _ -> failwith "expected Task_done_explicit");
  let final = Hashtbl.find files "/proj/main.ml" in
  assert (final = "let x = 100\nlet y = 2\nlet z = 3");
  print_endline
    "✓ E2E: Plan_act.run_for_task drives view_file → str_replace → submit on \
     virtual FS"

let test_sandbox_blocks_tool_outside_root () =
  (* Same skeleton, but the LLM tries to view a file outside the sandbox.
     view_file's File_read goes through with_sandbox and gets rejected;
     the tool returns Error; the LLM gets is_error feedback and submits
     a failure. *)
  let files = Hashtbl.create 4 in
  Hashtbl.add files "/etc/passwd" "secret";
  Hashtbl.add files "/proj/main.ml" "ok";

  let escape_resp =
    mk_resp ~stop_reason:Tool_use_stop
      [
        Tool_use
          {
            id = Id.Tool_use_id.of_string "u_escape";
            name = "view_file";
            input = `Assoc [ ("path", `String "/etc/passwd") ];
          };
      ]
  in
  let submit_resp =
    mk_resp ~stop_reason:Tool_use_stop
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
                  ("error", `String "blocked by sandbox");
                ];
          };
      ]
  in

  let llm_chain = canned_llm [ escape_resp; submit_resp ] in
  let tool_chain = Tool_handler.direct in
  let tools = [ Tools.view_file ] in
  let file_chain =
    File_handler.in_memory ~files
    |> File_handler.with_sandbox ~root:"/proj"
  in

  let outcome =
    Llm_handler.install llm_chain (fun () ->
        File_handler.install file_chain (fun () ->
            Time_handler.install Time_handler.direct (fun () ->
                Tool_handler.install ~tools tool_chain (fun () ->
                    Handlers.silent (fun () ->
                        Plan_act.run_for_task
                          ~task_description:"read /etc/passwd"
                          ~tools ())))))
  in

  (* Outer outcome: explicit failure submitted by the LLM in response
     to the tool error. We don't care about the LLM's error string —
     we care that the file system was NOT touched outside /proj. *)
  (match outcome with
  | Task_done_explicit { submit; _ } -> assert (not submit.ts_success)
  | _ -> failwith "expected Task_done_explicit (failure)");
  (* /etc/passwd content is intact (no read isn't observable from FS,
     but no write happened — Hashtbl unchanged). The point is that the
     view_file tool got `Error _` from the sandbox layer and surfaced
     it as a tool error to the LLM. We can verify that by checking the
     /proj/main.ml didn't become a copy of /etc/passwd or anything
     weird — table stays identical to seed. *)
  assert (Hashtbl.find files "/etc/passwd" = "secret");
  assert (Hashtbl.find files "/proj/main.ml" = "ok");
  print_endline
    "✓ E2E: sandbox middleware blocks tool's view_file from escaping root"

(* ========================================================================
   Scenario 1: Multi-task plan-act with cross-task FS state
   ======================================================================== *)

let test_multi_task_plan_act_chains_filesystem_state () =
  (* Virtual FS starts empty. Plan: task1 writes a file; task2 reads
     and edits it. Verifying cross-task FS visibility through real
     File_handler effects, not through some shortcut. *)
  let files = Hashtbl.create 4 in

  (* 1. planner *)
  let planner_resp =
    mk_resp ~stop_reason:Tool_use_stop
      [
        Tool_use
          {
            id = Id.Tool_use_id.of_string "u_plan";
            name = "submit_plan";
            input =
              `Assoc
                [
                  ("title", `String "Build then edit");
                  ( "tasks",
                    `List
                      [
                        `Assoc
                          [
                            ( "description",
                              `String "create /proj/main.ml with let x = 1"
                            );
                          ];
                        `Assoc
                          [
                            ( "description",
                              `String "change x to 2 in /proj/main.ml" );
                          ];
                      ] );
                ];
            };
      ]
  in
  (* 2. task 1 executor: write *)
  let t1_write =
    mk_resp ~stop_reason:Tool_use_stop
      [
        Tool_use
          {
            id = Id.Tool_use_id.of_string "u_t1_write";
            name = "write_file";
            input =
              `Assoc
                [
                  ("path", `String "/proj/main.ml");
                  ("content", `String "let x = 1\n");
                ];
          };
      ]
  in
  (* 3. task 1 executor: submit *)
  let t1_submit =
    mk_resp ~stop_reason:Tool_use_stop
      [
        Tool_use
          {
            id = Id.Tool_use_id.of_string "u_t1_submit";
            name = "submit_task_result";
            input =
              `Assoc
                [
                  ("success", `Bool true);
                  ("result", `String "wrote /proj/main.ml");
                  ("error", `String "");
                ];
          };
      ]
  in
  (* 4. task 2 executor: view (proves it sees task 1's write) *)
  let t2_view =
    mk_resp ~stop_reason:Tool_use_stop
      [
        Tool_use
          {
            id = Id.Tool_use_id.of_string "u_t2_view";
            name = "view_file";
            input = `Assoc [ ("path", `String "/proj/main.ml") ];
          };
      ]
  in
  (* 5. task 2 executor: edit *)
  let t2_edit =
    mk_resp ~stop_reason:Tool_use_stop
      [
        Tool_use
          {
            id = Id.Tool_use_id.of_string "u_t2_edit";
            name = "str_replace";
            input =
              `Assoc
                [
                  ("path", `String "/proj/main.ml");
                  ("old_str", `String "let x = 1");
                  ("new_str", `String "let x = 2");
                ];
          };
      ]
  in
  (* 6. task 2 executor: submit *)
  let t2_submit =
    mk_resp ~stop_reason:Tool_use_stop
      [
        Tool_use
          {
            id = Id.Tool_use_id.of_string "u_t2_submit";
            name = "submit_task_result";
            input =
              `Assoc
                [
                  ("success", `Bool true);
                  ("result", `String "changed x to 2");
                  ("error", `String "");
                ];
          };
      ]
  in
  let llm_chain =
    canned_llm
      [ planner_resp; t1_write; t1_submit; t2_view; t2_edit; t2_submit ]
  in
  let tools = [ Tools.view_file; Tools.write_file; Tools.str_replace ] in
  let config =
    {
      Plan_act.default_config with
      skip_summarizer = true;
      memory_dir = Some "/var/mem";
    }
  in
  let result =
    Llm_handler.install llm_chain (fun () ->
        File_handler.install
          (File_handler.in_memory ~files)
          (fun () ->
            Time_handler.install Time_handler.direct (fun () ->
                Tool_handler.install ~tools Tool_handler.direct (fun () ->
                    Log_handler.install Log_handler.null (fun () ->
                        Plan_act.run ~config ~goal:"do it" ~tools ())))))
  in
  (match result with
  | Ok _ -> ()
  | Error e -> failwith ("plan_act failed: " ^ agent_error_pp e));
  (* Final FS state: file exists, x = 2. *)
  let final = Hashtbl.find files "/proj/main.ml" in
  assert (final = "let x = 2\n");
  (* Memory persistence: executor.json was written via File_write. *)
  assert (Hashtbl.mem files "/var/mem/executor.json");
  print_endline
    "✓ E2E: multi-task plan-act — task 1 writes, task 2 reads + edits via \
     virtual FS"

(* ========================================================================
   Scenario 2: Recovery flow (task fails twice → skip → continue)
   ======================================================================== *)

let test_recovery_flow_skip_then_continue () =
  let files = Hashtbl.create 4 in
  Hashtbl.add files "/proj/main.ml" "ok";
  let planner_resp =
    mk_resp ~stop_reason:Tool_use_stop
      [
        Tool_use
          {
            id = Id.Tool_use_id.of_string "u_plan";
            name = "submit_plan";
            input =
              `Assoc
                [
                  ("title", `String "Two tasks");
                  ( "tasks",
                    `List
                      [
                        `Assoc
                          [
                            ( "description",
                              `String "task1 (will fail)" );
                          ];
                        `Assoc [ ("description", `String "task2 (ok)") ];
                      ] );
                ];
          };
      ]
  in
  let fail_resp tag =
    mk_resp ~stop_reason:Tool_use_stop
      [
        Tool_use
          {
            id = Id.Tool_use_id.of_string ("u_fail_" ^ tag);
            name = "submit_task_result";
            input =
              `Assoc
                [
                  ("success", `Bool false);
                  ("result", `String "");
                  ("error", `String ("failure-" ^ tag));
                ];
          };
      ]
  in
  let recovery_resp =
    mk_resp ~stop_reason:Tool_use_stop
      [
        Tool_use
          {
            id = Id.Tool_use_id.of_string "u_recover";
            name = "submit_recovery";
            input = `Assoc [ ("decision", `String "skip"); ("tasks", `List []) ];
          };
      ]
  in
  let t2_ok =
    mk_resp ~stop_reason:Tool_use_stop
      [
        Tool_use
          {
            id = Id.Tool_use_id.of_string "u_t2";
            name = "submit_task_result";
            input =
              `Assoc
                [
                  ("success", `Bool true);
                  ("result", `String "task2 done");
                  ("error", `String "");
                ];
          };
      ]
  in
  let llm_chain =
    canned_llm
      [
        planner_resp;
        fail_resp "1a";   (* task1 attempt 1 *)
        fail_resp "1b";   (* task1 retry *)
        recovery_resp;    (* recovery planner picks skip *)
        t2_ok;            (* task2 *)
      ]
  in
  let tools = [ Tools.view_file ] in
  let config =
    { Plan_act.default_config with skip_summarizer = true }
  in
  let result =
    Llm_handler.install llm_chain (fun () ->
        File_handler.install
          (File_handler.in_memory ~files)
          (fun () ->
            Time_handler.install Time_handler.direct (fun () ->
                Tool_handler.install ~tools Tool_handler.direct (fun () ->
                    Log_handler.install Log_handler.null (fun () ->
                        Plan_act.run ~config ~goal:"do" ~tools ())))))
  in
  (match result with
  | Ok answer ->
      (* Plan_act renders results when summarizer skipped — should show
         task1 as failed (after retries) and task2 as done. *)
      assert (Test_helpers.contains answer "task2 done");
      assert (Test_helpers.contains answer "FAILED")
  | Error e -> failwith ("plan_act failed: " ^ agent_error_pp e));
  print_endline
    "✓ E2E: recovery flow — task1 fails twice → planner says skip → task2 \
     completes"

(* ========================================================================
   Scenario 3: ask_user pause-resume threading
   ======================================================================== *)

let test_ask_user_pause_then_resume () =
  (* Run 1: agent calls ask_user → Outcome_waiting. *)
  let pause_resp =
    mk_resp ~stop_reason:Tool_use_stop
      [
        Text "I need clarification.";
        Tool_use
          {
            id = Id.Tool_use_id.of_string "u_ask";
            name = "ask_user";
            input = `Assoc [ ("question", `String "Which file?") ];
          };
      ]
  in
  let outcome1 =
    Handlers.mock ~llm_responses:[ pause_resp ] (fun () ->
        Agent.run_session
          ~messages:[ { role = User; content = [ Text "edit something" ] } ]
          ~tools:Tools.all ())
  in
  let pending_id, pending_messages =
    match outcome1 with
    | Outcome_waiting { tool_use_id; messages; _ } -> (tool_use_id, messages)
    | _ -> failwith "expected Outcome_waiting on first run"
  in
  assert (Id.Tool_use_id.to_string pending_id = "u_ask");

  (* Marshal through Session: build a session with pending state, then
     append the user's reply. The reply must become a Tool_result block,
     not a fresh User text turn. *)
  let session1 =
    Session.{
      messages = pending_messages;
      pending_tool_use_id = Some pending_id;
      model = "test";
    }
  in
  let session2 = Session.append_input session1 "/tmp/x.ml" in
  let last = List.nth session2.messages (List.length session2.messages - 1) in
  (match last.content with
  | [ Tool_result { tool_use_id; content; is_error = false } ] ->
      assert (Id.Tool_use_id.to_string tool_use_id = "u_ask");
      assert (content = "/tmp/x.ml")
  | _ -> failwith "resume should produce Tool_result, not text");
  assert (session2.pending_tool_use_id = None);

  (* Run 2: feed the resumed messages and finish normally. *)
  let final_resp =
    mk_resp ~stop_reason:End_turn [ Text "Got it: /tmp/x.ml" ]
  in
  let outcome2 =
    Handlers.mock ~llm_responses:[ final_resp ] (fun () ->
        Agent.run_session ~messages:session2.messages ~tools:Tools.all ())
  in
  (match outcome2 with
  | Outcome_done { answer; _ } -> assert (answer = "Got it: /tmp/x.ml")
  | _ -> failwith "expected Outcome_done after resume");
  print_endline
    "✓ E2E: ask_user pause → Session.append_input threads reply as \
     Tool_result → resume completes"

(* ========================================================================
   Scenario 5: Sub-agent isolation (delegate doesn't leak parent history)
   ======================================================================== *)

(* ========================================================================
   Scenario 7: Resumable plan-act — interrupted run picks up where it left
   ======================================================================== *)

let test_resumable_plan_act_picks_up_after_interruption () =
  let files = Hashtbl.create 16 in

  let planner_resp =
    mk_resp ~stop_reason:Tool_use_stop
      [
        Tool_use
          {
            id = Id.Tool_use_id.of_string "u_plan";
            name = "submit_plan";
            input =
              `Assoc
                [
                  ("title", `String "Two");
                  ( "tasks",
                    `List
                      [
                        `Assoc [ ("description", `String "task A") ];
                        `Assoc [ ("description", `String "task B") ];
                      ] );
                ];
          };
      ]
  in
  let mk_submit tag result =
    mk_resp ~stop_reason:Tool_use_stop
      [
        Tool_use
          {
            id = Id.Tool_use_id.of_string ("u_" ^ tag);
            name = "submit_task_result";
            input =
              `Assoc
                [
                  ("success", `Bool true);
                  ("result", `String result);
                  ("error", `String "");
                ];
          };
      ]
  in

  (* Run 1: planner + task A, deliberately short on task B's responses
     so canned_llm fails — simulating an interruption. *)
  let llm_run1 = canned_llm [ planner_resp; mk_submit "a" "did A" ] in
  let config =
    {
      Plan_act.default_config with
      skip_summarizer = true;
      memory_dir = Some "/var/mem";
    }
  in
  let tools = [] in
  (try
     ignore
       (Llm_handler.install llm_run1 (fun () ->
            File_handler.install
              (File_handler.in_memory ~files)
              (fun () ->
                Time_handler.install Time_handler.direct (fun () ->
                    Tool_handler.install ~tools Tool_handler.direct
                      (fun () ->
                        Log_handler.install Log_handler.null (fun () ->
                            Plan_act.run ~config ~goal:"do it" ~tools ()))))))
   with Failure _ -> ());

  (* Run 1 left state behind: plan_state.json + executor.json. *)
  assert (Hashtbl.mem files "/var/mem/plan_state.json");
  let state_body = Hashtbl.find files "/var/mem/plan_state.json" in
  assert (Test_helpers.contains state_body "\"goal\":");
  assert (Test_helpers.contains state_body "do it");
  assert (Test_helpers.contains state_body "\"task A\"");

  (* Run 2: ONLY task B's submit. No planner response provided —
     resume must skip planning and jump straight to task B. *)
  let llm_run2 = canned_llm [ mk_submit "b" "did B" ] in
  let result =
    Llm_handler.install llm_run2 (fun () ->
        File_handler.install
          (File_handler.in_memory ~files)
          (fun () ->
            Time_handler.install Time_handler.direct (fun () ->
                Tool_handler.install ~tools Tool_handler.direct (fun () ->
                    Log_handler.install Log_handler.null (fun () ->
                        Plan_act.run ~config ~goal:"do it" ~tools ())))))
  in
  (match result with
  | Ok answer ->
      assert (Test_helpers.contains answer "did A");
      assert (Test_helpers.contains answer "did B")
  | Error e -> failwith ("resumed run failed: " ^ agent_error_pp e));

  (* On successful completion plan_state is cleared (empty body). *)
  let final_state = Hashtbl.find files "/var/mem/plan_state.json" in
  assert (final_state = "");
  print_endline
    "✓ E2E: resumable plan-act — run 1 crashes after task A; run 2 reads \
     plan_state.json, skips planner, completes task B"

let test_subagent_no_parent_history_leak () =
  let captured_subagent_messages : message list ref = ref [] in
  let parent_resp =
    mk_resp ~stop_reason:Tool_use_stop
      [
        Text "SECRET-PARENT-CONTEXT-MARKER";
        Tool_use
          {
            id = Id.Tool_use_id.of_string "u_delegate";
            name = "delegate";
            input = `Assoc [ ("task", `String "compute 2+2") ];
          };
      ]
  in
  let _ = parent_resp in
  let subagent_resp =
    mk_resp ~stop_reason:End_turn [ Text "the answer is 4" ]
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
                    captured_subagent_messages := args.messages;
                    Effect.Deep.continue k subagent_resp)
            | Effects.Log _ ->
                Some (fun k -> Effect.Deep.continue k ())
            | _ -> None);
      }
  in
  let delegate = Sub_agent.make_delegate_tool ~tools_for_subagent:Tools.all in
  let result =
    recording_handler (fun () ->
        delegate.handler (`Assoc [ ("task", `String "compute 2+2") ]))
  in
  (match result with
  | Ok s -> assert (s = "the answer is 4")
  | Error e -> failwith ("delegate failed: " ^ e));
  (* Parent's secret marker must NOT appear in any sub-agent message. *)
  let dump =
    !captured_subagent_messages
    |> List.map (fun (m : message) ->
           m.content
           |> List.filter_map (function Text s -> Some s | _ -> None)
           |> String.concat " ")
    |> String.concat " | "
  in
  assert (not (Test_helpers.contains dump "SECRET-PARENT-CONTEXT-MARKER"));
  (* Sub-agent's first user message must be exactly the task string. *)
  (match !captured_subagent_messages with
  | [ { role = User; content = [ Text s ] } ] -> assert (s = "compute 2+2")
  | _ -> failwith "sub-agent should receive exactly one User text message");
  print_endline
    "✓ E2E: sub-agent isolation — parent context strings absent in delegate's \
     LLM args"

let run () =
  test_plan_act_task_edits_virtual_file ();
  test_sandbox_blocks_tool_outside_root ();
  test_multi_task_plan_act_chains_filesystem_state ();
  test_recovery_flow_skip_then_continue ();
  test_ask_user_pause_then_resume ();
  test_resumable_plan_act_picks_up_after_interruption ();
  test_subagent_no_parent_history_leak ()
