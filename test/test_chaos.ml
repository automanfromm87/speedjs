(** Tests for [Chaos] middleware: failure injection rate, determinism,
    no-op default. *)

open Speedjs
open Types

let canned_response =
  {
    content = [ Text "ok" ];
    stop_reason = End_turn;
    usage = usage_of_basic ~input_tokens:1 ~output_tokens:1;
  }

let test_chaos_default_is_noop () =
  let calls_in = ref 0 in
  let chained =
    Chaos.with_llm Chaos.default (fun args ->
        incr calls_in;
        let _ = args in
        canned_response)
  in
  for _ = 1 to 50 do
    let _ : llm_response = chained (basic_call ~messages:[] ~tools:[]) in
    ()
  done;
  assert (!calls_in = 50);
  print_endline
    "✓ Chaos.default leaves the inner handler called 100% (no injection \
     when rate=0.0)"

let test_chaos_llm_injects_at_rate () =
  let total = 1000 in
  let injected = ref 0 in
  let captured = ref [] in
  let config =
    {
      Chaos.default with
      seed = 42;
      llm_failure_rate = 0.10;
      on_inject =
        (fun ~kind:_ ~detail -> captured := detail :: !captured);
    }
  in
  let chained =
    Chaos.with_llm config (fun _ -> canned_response)
  in
  for _ = 1 to total do
    try
      let _ : llm_response =
        chained (basic_call ~messages:[] ~tools:[])
      in
      ()
    with Llm_error.Llm_api_error _ -> incr injected
  done;
  let rate = float_of_int !injected /. float_of_int total in
  assert (rate > 0.07 && rate < 0.13);
  assert (List.length !captured = !injected);
  print_endline
    (Printf.sprintf
       "✓ Chaos.with_llm injects ~10%%: %d / %d = %.3f (within ±3%%)"
       !injected total rate)

let test_chaos_seed_reproducibility () =
  (* Same seed + same call sequence must produce identical hit/miss
     pattern. *)
  let run_once seed =
    let hits = ref [] in
    let config =
      {
        Chaos.default with
        seed;
        llm_failure_rate = 0.30;
        on_inject = (fun ~kind:_ ~detail:_ -> ());
      }
    in
    let chained =
      Chaos.with_llm config (fun _ -> canned_response)
    in
    for i = 1 to 100 do
      try
        let _ : llm_response =
          chained (basic_call ~messages:[] ~tools:[])
        in
        ()
      with Llm_error.Llm_api_error _ -> hits := i :: !hits
    done;
    List.rev !hits
  in
  let a = run_once 42 in
  let b = run_once 42 in
  let c = run_once 99 in
  assert (a = b);
  assert (a <> c);
  print_endline
    (Printf.sprintf
       "✓ Chaos seed determinism: same seed → same %d-element hit \
        sequence; different seed differs"
       (List.length a))

let test_chaos_tool_returns_typed_error () =
  let config =
    {
      Chaos.default with
      seed = 7;
      tool_failure_rate = 1.0;  (* every call → guaranteed inject *)
    }
  in
  let dummy_tool : tool_def =
    Types.make_typed_tool
      ~name:"calc"
      ~description:""
      ~capabilities:[ Read_only ]
      ~input_schema:(`Assoc [ ("type", `String "object") ])
      ~input_decoder:(fun _ -> Ok ())
      ~handler:(fun () -> Ok "should not run")
      ()
  in
  let inner : Tool_handler.t = Tool_handler.direct in
  let chained = Chaos.with_tool config inner in
  let args : Tool_handler.call_args =
    { tool = dummy_tool; input = `Assoc []; use_id = Id.Tool_use_id.of_string "u" }
  in
  let result = chained args in
  (match result with
  | Ok _ -> failwith "expected chaos-injected error at rate=1.0"
  | Error e ->
      assert (Test_helpers.contains (Error.pp e) "chaos");
      assert (e.Error.code = "chaos"));
  print_endline
    "✓ Chaos.with_tool returns typed Error.t with code=chaos at rate=1.0"

(* Regression: an Llm_api_error raised from inside Agent.execute must
   surface as Agent.Failed, not propagate as an exception. Without
   this, chaos-injected LLM errors fly past every workflow combinator
   ([with_retry] / [recover] catch Result.Error only) and kill the
   whole run instead of triggering plan-act recovery. *)
let test_chaos_llm_exception_becomes_failed () =
  let chaos_handler args =
    let _ = args in
    raise
      (Llm_error.Llm_api_error
         (Overloaded
            { retry_after = Some 0.0; message = "chaos: simulated overload" }))
  in
  let spec = Specs.chat ~tools:[] () in
  let out =
    Llm_handler.install chaos_handler (fun () ->
        File_handler.install File_handler.direct (fun () ->
            Time_handler.install Time_handler.direct (fun () ->
                Tool_handler.install Tool_handler.direct (fun () ->
                    Log_handler.install Log_handler.null (fun () ->
                        Agent.execute ~spec ~input:(Agent.Fresh "go"))))))
  in
  (match out with
  | Agent.Failed { reason = Llm_call_failed msg; _ } ->
      assert (Test_helpers.contains msg "overloaded");
      assert (Test_helpers.contains msg "chaos")
  | Agent.Failed { reason; _ } ->
      failwith
        ("expected Llm_call_failed, got " ^ agent_error_pp reason)
  | _ ->
      failwith "expected Failed output (Llm_api_error must NOT propagate)");
  print_endline
    "✓ Agent.execute converts Llm_api_error → Failed (workflow combinators \
     can react)"

let test_chaos_show () =
  assert (Chaos.show Chaos.default = "");
  let config =
    {
      Chaos.default with
      llm_failure_rate = 0.05;
      tool_failure_rate = 0.10;
    }
  in
  let s = Chaos.show config in
  assert (Test_helpers.contains s "chaos");
  assert (Test_helpers.contains s "llm=0.05");
  assert (Test_helpers.contains s "tool=0.10");
  print_endline
    "✓ Chaos.show is empty when inactive, structured when injecting"

let run () =
  print_endline "\n=== Chaos ===";
  test_chaos_default_is_noop ();
  test_chaos_llm_injects_at_rate ();
  test_chaos_seed_reproducibility ();
  test_chaos_tool_returns_typed_error ();
  test_chaos_llm_exception_becomes_failed ();
  test_chaos_show ()
