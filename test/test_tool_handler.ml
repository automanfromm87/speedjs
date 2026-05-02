(** Tests for [Tool_handler] middleware chain + [make_typed_tool] builder. *)

open Speedjs
open Types

let mk_test_tool ?(idempotent = false) ?(timeout_sec = None)
    ?(category = "test")
    ?(capabilities = [ Read_only ])
    ?(allowed_modes = [ Planner; Recovery; Executor; Subagent ])
    ?(classify_error = default_classify_error)
    name handler : tool_def =
  {
    name;
    description = "test tool";
    input_schema = `Assoc [];
    handler;
    idempotent;
    timeout_sec;
    category;
    capabilities;
    allowed_modes;
    classify_error;
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
  let calls = ref 0 in
  let bad_tool =
    mk_test_tool ~idempotent:false "writer" (fun _ ->
        incr calls;
        Error "transient blip")
  in
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
  assert (!calls = 1);

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

let test_make_typed_tool_decodes_input_and_runs_handler () =
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
  (match tool.handler (`Assoc [ ("x", `Int 5); ("y", `Int 7) ]) with
  | Ok "12" -> ()
  | Ok s -> failwith ("unexpected output: " ^ s)
  | Error e -> failwith ("expected Ok, got Error: " ^ e));
  assert (!calls = 1);
  (match tool.handler (`String "not an object") with
  | Error msg -> assert (Test_helpers.contains msg "invalid input")
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

let run () =
  test_tool_handler_direct_classifies_errors ();
  test_tool_handler_validation_rejects_non_object ();
  test_tool_handler_retry_only_for_idempotent ();
  test_tool_handler_circuit_breaker_opens_after_threshold ();
  test_tool_handler_audit_observes_calls ();
  test_make_typed_tool_decodes_input_and_runs_handler ();
  test_tool_handler_install_dispatches_via_chain ()
