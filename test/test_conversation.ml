(** Conversation smart-constructor + validate tests.

    These cover the dangling-tool_use bug class — accumulating message
    history across plan-act tasks where the previous task's
    submit_task_result tool_use was never paired with a tool_result. *)

open Speedjs
open Types

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
        Test_helpers.contains lower "dangling"
        || Test_helpers.contains lower "answered"));
  print_endline "✓ Conversation.validate rejects dangling-tool_use pattern"

let test_conversation_alternation_violation () =
  let bad : message list =
    [
      user_text_message "first";
      user_text_message "second";
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

let run () =
  test_conversation_empty_state ();
  test_conversation_push_user_then_assistant_text ();
  test_conversation_push_assistant_with_tool_use ();
  test_conversation_close_dangling_with_ack ();
  test_conversation_push_user_rejects_dangling ();
  test_conversation_validate_catches_dangling_pattern ();
  test_conversation_alternation_violation ();
  test_conversation_first_must_be_user ();
  test_conversation_tool_use_in_user_rejected ();
  test_conversation_round_trip_legitimate_dangling ()
