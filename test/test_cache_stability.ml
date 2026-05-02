(** Cache-stability properties.

    Anthropic's prompt cache is keyed off the request body — every
    byte of the system prompt + tool list + messages prefix UP TO the
    first [cache_control] marker contributes to the cache key. If a
    code change perturbs that prefix between successive LLM calls
    within the same agent run, cache hits silently turn into misses
    and cost spikes 10-30x.

    These tests pin the invariants we rely on for cache locality:

    1. [Conversation.push_*] is purely additive (appends one entry).
    2. [Codec] serialization is deterministic.
    3. [Strategy.sliding_window_at] preserves its kept-suffix prefix
       across calls until the soft trigger fires.

    Failure of any of these is a regression that would silently
    multiply cost in production. *)

open Speedjs
open Types

let ok_or_fail = function
  | Ok c -> c
  | Error msg -> failwith ("Conversation.of_messages: " ^ msg)

(* ===== 1. Conversation push is additive ===== *)

let test_push_user_text_appends_one () =
  let c0 = Conversation.empty in
  let c1 = Conversation.push_user_text c0 "first" in
  let m0 = Conversation.to_messages c0 in
  let m1 = Conversation.to_messages c1 in
  assert (List.length m1 = List.length m0 + 1);
  (* The new turn is appended at the END (oldest first ordering). *)
  let last = List.nth m1 (List.length m1 - 1) in
  assert (last.role = User);
  print_endline
    "✓ push_user_text appends exactly one User turn at the tail"

let test_push_assistant_appends_one () =
  let c0 =
    Conversation.empty |> fun c -> Conversation.push_user_text c "hi"
  in
  let c1 =
    Conversation.push_assistant c0 [ Text "ack" ]
  in
  let m0 = Conversation.to_messages c0 in
  let m1 = Conversation.to_messages c1 in
  assert (List.length m1 = List.length m0 + 1);
  let last = List.nth m1 (List.length m1 - 1) in
  assert (last.role = Assistant);
  print_endline "✓ push_assistant appends exactly one Assistant turn"

let test_push_does_not_perturb_prior () =
  (* The PRIOR messages must be byte-for-byte identical after a push.
     If push_* internally re-renders or normalizes earlier turns,
     cache prefix shifts and Anthropic recomputes the whole key. *)
  let c0 =
    Conversation.empty
    |> (fun c -> Conversation.push_user_text c "system check")
    |> (fun c -> Conversation.push_assistant c [ Text "all green" ])
  in
  let m_before = Conversation.to_messages c0 in
  let c1 = Conversation.push_user_text c0 "next question" in
  let m_after = Conversation.to_messages c1 in
  let prefix = List.filteri (fun i _ -> i < List.length m_before) m_after in
  let ser = List.map (fun m -> Codec.message_to_json m |> Yojson.Safe.to_string) in
  assert (ser m_before = ser prefix);
  print_endline
    "✓ push_user_text leaves prior messages byte-identical (cache prefix preserved)"

(* ===== 2. Codec is deterministic ===== *)

let test_codec_message_deterministic () =
  let m =
    {
      role = User;
      content =
        [
          Text "hello";
          Tool_use
            {
              id = Id.Tool_use_id.of_string "tu_42";
              name = "calculator";
              input = `Assoc [ ("expression", `String "1+1") ];
            };
        ];
    }
  in
  let s1 = Yojson.Safe.to_string (Codec.message_to_json m) in
  let s2 = Yojson.Safe.to_string (Codec.message_to_json m) in
  let s3 = Yojson.Safe.to_string (Codec.message_to_json m) in
  assert (s1 = s2 && s2 = s3);
  print_endline "✓ Codec.message_to_json deterministic across calls"

let test_codec_full_message_list_deterministic () =
  let msgs =
    [
      { role = User; content = [ Text "hi" ] };
      { role = Assistant; content = [ Text "hi back" ] };
      { role = User; content = [ Text "now what?" ] };
    ]
  in
  let render () =
    msgs
    |> List.map (fun m -> Codec.message_to_json m)
    |> List.map Yojson.Safe.to_string
    |> String.concat ""
  in
  let r1 = render () in
  let r2 = render () in
  assert (r1 = r2);
  print_endline "✓ Codec serializes a message list deterministically"

(* ===== 3. sliding_window_at prefix stability ===== *)

let mk_user n =
  { role = User; content = [ Text (Printf.sprintf "user %d" n) ] }

let mk_assistant n =
  { role = Assistant; content = [ Text (Printf.sprintf "asst %d" n) ] }

let alternating_history ~turns =
  let rec build i acc =
    if i >= turns then List.rev acc
    else
      let acc = mk_user i :: acc in
      let acc = if i + 1 < turns then mk_assistant i :: acc else acc in
      build (i + 2) acc
  in
  build 0 []

let test_sliding_window_at_freezes_cut () =
  (* trigger=20, keep=10. Build a 25-message history; first call
     freezes cut. Subsequent calls with the same closure but more
     messages should keep dropping the SAME prefix until the
     effective length re-crosses trigger. *)
  let strategy =
    Context.Strategy.sliding_window_at ~trigger_at:20 ~keep_recent:10
  in
  let h25 = alternating_history ~turns:25 in
  let h26 = h25 @ [ mk_user 99 ] in
  let h27 = h26 @ [ mk_assistant 99 ] in
  let kept_25 = strategy h25 in
  let kept_26 = strategy h26 in
  let kept_27 = strategy h27 in
  (* All three should drop the SAME prefix (the cut anchor froze on
     call 1). The kept suffix grows by exactly 1 each subsequent
     call, so kept_25 should be a prefix of kept_26, which should be
     a prefix of kept_27. *)
  let serialize ms =
    List.map (fun m -> Codec.message_to_json m |> Yojson.Safe.to_string) ms
  in
  let s25 = serialize kept_25 in
  let s26 = serialize kept_26 in
  let s27 = serialize kept_27 in
  let prefix_eq long short =
    List.length long >= List.length short
    && List.filteri (fun i _ -> i < List.length short) long = short
  in
  assert (prefix_eq s26 s25);
  assert (prefix_eq s27 s26);
  print_endline
    "✓ sliding_window_at: frozen cut anchor → kept-suffix prefix stable across calls"

let test_two_independent_runs_use_independent_anchors () =
  (* Cache stability requires CALLERS to reuse the same closure within
     a run (frozen anchor) and create a NEW closure per run. Verify
     that two fresh closures don't share state — each gets its own
     anchor. *)
  let strategy_a =
    Context.Strategy.sliding_window_at ~trigger_at:20 ~keep_recent:10
  in
  let strategy_b =
    Context.Strategy.sliding_window_at ~trigger_at:20 ~keep_recent:10
  in
  let h25 = alternating_history ~turns:25 in
  let kept_a = strategy_a h25 in
  let kept_b = strategy_b h25 in
  (* Both should produce identical output for the same input *)
  assert (kept_a = kept_b);
  print_endline
    "✓ sliding_window_at: each fresh closure is independent (no shared state)"

let run () =
  print_endline "\n=== Cache stability ===";
  test_push_user_text_appends_one ();
  test_push_assistant_appends_one ();
  test_push_does_not_perturb_prior ();
  test_codec_message_deterministic ();
  test_codec_full_message_list_deterministic ();
  test_sliding_window_at_freezes_cut ();
  test_two_independent_runs_use_independent_anchors ()
