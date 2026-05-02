(** Tests for [Context] (structured input + render_system) and the
    sliding-window / compacted strategies. *)

open Speedjs
open Types

let test_context_renders_system_with_env () =
  let ctx =
    Speedjs.Context.empty
    |> Speedjs.Context.with_system_prompt "You are a helpful agent."
    |> Speedjs.Context.with_env ~tag:"workspace_brief"
         ~body:"Project root: /tmp/x"
    |> Speedjs.Context.with_env ~tag:"current_time" ~body:"2026-05-01"
  in
  let s = Speedjs.Context.render_system ctx in
  assert (Test_helpers.contains s "You are a helpful agent.");
  assert (Test_helpers.contains s "<workspace_brief>");
  assert (Test_helpers.contains s "<current_time>");
  print_endline "✓ Context.render_system composes system + env blocks"

let test_context_system_blocks_ordering () =
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
      capabilities = [ Speedjs.Types.Read_only ];
      allowed_modes = [ Planner; Recovery; Executor; Subagent ];
      classify_error = Speedjs.Types.default_classify_error;
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
  assert (List.length out < List.length msgs);
  assert (is_truncated_marker (List.hd out));
  assert ((List.nth out 1).role = Assistant);
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
  let strat =
    Speedjs.Context.Strategy.sliding_window_at ~trigger_at:6 ~keep_recent:3
  in
  let msgs1 = build_react_msgs ~n:11 in
  let out1 = strat msgs1 in
  let first_after_marker_1 = List.nth out1 1 in
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
  let make () =
    Speedjs.Context.Strategy.sliding_window_at ~trigger_at:6 ~keep_recent:3
  in
  let s1 = make () in
  let s2 = make () in
  let trigger_msgs = build_react_msgs ~n:11 in
  let small_msgs = build_react_msgs ~n:4 in
  let _ = s1 trigger_msgs in
  let out_s2 = s2 small_msgs in
  assert (out_s2 = small_msgs);
  print_endline
    "✓ sliding_window_at: factory yields independent per-instance state"

let test_context_compacted_strategy () =
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
    when Test_helpers.contains s "compacted_history"
         && Test_helpers.contains s "summary of" ->
      ()
  | _ ->
      failwith "expected synthetic compacted summary as first message");
  print_endline "✓ Context.Strategy.compacted folds prefix into summary turn"

let run () =
  test_context_renders_system_with_env ();
  test_context_system_blocks_ordering ();
  test_context_add_tool_appends ();
  test_context_to_llm_args_carries_strategy ();
  test_sliding_window_at_below_trigger_passes_through ();
  test_sliding_window_at_first_trim_invariants ();
  test_sliding_window_at_freezes_cut_anchor ();
  test_sliding_window_at_retrims_when_effective_exceeds_again ();
  test_sliding_window_at_factory_independent_state ();
  test_context_compacted_strategy ()
