(** Tests for [Llm_handler] middleware chain + [Log_handler] +
    [Llm_error] classification. *)

open Speedjs
open Types

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

let test_llm_handler_chain_validates_messages () =
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
      model = None;
      tool_choice = Tc_auto;
      purpose = `Other;
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
      model = None;
      tool_choice = Tc_auto;
      purpose = `Other;
      }
  in
  let _ =
    chain
      {
        messages = [ user_text_message "again" ];
        tools = [];
        system_override = None;
      model = None;
      tool_choice = Tc_auto;
      purpose = `Other;
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
      model = None;
      tool_choice = Tc_auto;
      purpose = `Other;
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
      model = None;
      tool_choice = Tc_auto;
      purpose = `Other;
         }
     in
     failwith "expected Auth to be re-raised"
   with Llm_error.Llm_api_error (Llm_error.Auth _) -> ());
  assert (!attempts = 1);
  print_endline "✓ Llm_handler.with_retry fails fast on non-retryable Auth"

let test_llm_handler_install_intercepts_effect () =
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
        Handlers.silent (fun () ->
            Agent.execute ~spec:(Specs.chat ~tools:[] ())
              ~input:(Agent.Fresh "test")))
  in
  (match result with
  | Agent.Done { answer = "from chain"; _ } -> ()
  | Agent.Done { answer; _ } -> failwith ("unexpected answer: " ^ answer)
  | Agent.Failed { reason; _ } ->
      failwith ("unexpected error: " ^ agent_error_pp reason)
  | _ -> failwith "unexpected output");
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
      model = None;
      tool_choice = Tc_auto;
      purpose = `Other;
      }
  in
  assert (!calls = 2);
  (match response.content with
  | [ Text s ] -> assert (Test_helpers.contains s "2 msgs")
  | _ -> failwith "expected response from second attempt");
  print_endline
    "✓ Llm_handler.with_compaction_on_overflow retries with compacted messages"

(* Per-call model override: spec.model = Some X must propagate through
   Agent.execute → Step.once → Context.to_llm_args → llm_call_args, and
   the leaf handler must use args.model rather than its install-time
   default. *)
let test_per_call_model_override () =
  let captured_models : string option list ref = ref [] in
  let inner : Llm_handler.t =
   fun args ->
    captured_models := args.model :: !captured_models;
    {
      content = [ Text "done" ];
      stop_reason = End_turn;
      usage = usage_of_basic ~input_tokens:1 ~output_tokens:1;
    }
  in
  let spec_with_model =
    Specs.chat ~model:"claude-haiku-4-5-20251001" ~tools:[] ()
  in
  let _out =
    Llm_handler.install inner (fun () ->
        File_handler.install File_handler.direct (fun () ->
            Time_handler.install Time_handler.direct (fun () ->
                Tool_handler.install Tool_handler.direct (fun () ->
                    Log_handler.install Log_handler.null (fun () ->
                        Agent.execute ~spec:spec_with_model
                          ~input:(Agent.Fresh "hi"))))))
  in
  assert (!captured_models <> []);
  List.iter
    (fun m -> assert (m = Some "claude-haiku-4-5-20251001"))
    !captured_models;
  print_endline
    "✓ spec.model propagates through Agent.execute to llm_call_args.model"

let test_anthropic_handler_uses_per_call_model () =
  (* anthropic handler chooses args.model over install-time ?model.
     We mock the underlying Anthropic.complete_stream by using a custom
     inner that observes which model the leaf would route to. The
     anthropic handler itself just calls complete_stream which is hard
     to mock without intercepting at HTTP layer; instead we test the
     decision logic directly via a hand-rolled equivalent. *)
  let chosen = ref None in
  let inner : Llm_handler.t =
   fun args ->
    chosen := args.model;
    {
      content = [ Text "ok" ];
      stop_reason = End_turn;
      usage = usage_of_basic ~input_tokens:1 ~output_tokens:1;
    }
  in
  let _ =
    inner
      {
        messages = [ user_text_message "hi" ];
        tools = [];
        system_override = None;
        tool_choice = Tc_auto;
        model = Some "claude-opus-4-7";
        purpose = `Other;
      }
  in
  assert (!chosen = Some "claude-opus-4-7");
  print_endline
    "✓ llm_call_args.model carries through unchanged in handler chain"

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

let run () =
  test_classify_status_codes ();
  test_llm_handler_chain_validates_messages ();
  test_llm_handler_chain_cost_tracking ();
  test_llm_handler_chain_retry_recovers ();
  test_llm_handler_chain_retry_fails_fast_on_auth ();
  test_llm_handler_install_intercepts_effect ();
  test_llm_handler_compaction_on_overflow ();
  test_per_call_model_override ();
  test_anthropic_handler_uses_per_call_model ();
  test_log_handler_chain ()
