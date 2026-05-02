(** Tests for [Parallel_subagent] — running sub-agents in OCaml 5
    domains with isolated effect-handler stacks. *)

open Speedjs
open Types

let mk_resp ?(usage = usage_of_basic ~input_tokens:1 ~output_tokens:1)
    ~stop_reason content =
  { content; stop_reason; usage }

(* ========================================================================
   Two parallel sub-agents, fully isolated effect stacks.
   ======================================================================== *)

let test_two_parallel_subagents_isolated () =
  let resp1 = mk_resp ~stop_reason:End_turn [ Text "answer for q1" ] in
  let resp2 = mk_resp ~stop_reason:End_turn [ Text "answer for q2" ] in
  let child resp task : unit -> string =
   fun () ->
    Handlers.mock ~llm_responses:[ resp ] (fun () ->
        match Agent.run ~user_query:task ~tools:[] () with
        | Ok s -> s
        | Error e -> "ERROR: " ^ agent_error_pp e)
  in
  let answers =
    Parallel_subagent.run [ child resp1 "q1"; child resp2 "q2" ]
  in
  (match answers with
  | [ a1; a2 ] ->
      assert (a1 = "answer for q1");
      assert (a2 = "answer for q2")
  | _ -> failwith "expected 2 answers");
  print_endline
    "✓ Parallel_subagent.run: 2 domains, each with own mock LLM stack, \
     both complete"

(* ========================================================================
   Shared log sink with per-domain prefix — Mutex-synchronized.
   ======================================================================== *)

let test_shared_log_sink_with_prefix () =
  let shared = Parallel_subagent.make_shared_sink () in
  let child id msg : unit -> unit =
   fun () ->
    let sink =
      Parallel_subagent.with_prefix
        (Printf.sprintf "sub:%d" id)
        shared.sink
    in
    Log_handler.install (Log_handler.to_function sink) (fun () ->
        Effect.perform (Effects.Log msg))
  in
  let _ = Parallel_subagent.run [ child 0 "from-zero"; child 1 "from-one" ] in
  let lines = shared.drain () in
  assert (
    List.exists
      (fun l ->
        Test_helpers.contains l "[sub:0]"
        && Test_helpers.contains l "from-zero")
      lines);
  assert (
    List.exists
      (fun l ->
        Test_helpers.contains l "[sub:1]"
        && Test_helpers.contains l "from-one")
      lines);
  print_endline
    "✓ Parallel_subagent.make_shared_sink: per-domain prefix tags, \
     Mutex-synchronized drain"

(* ========================================================================
   Failure in one domain doesn't sabotage the other.
   ======================================================================== *)

let test_failure_in_one_doesnt_affect_other () =
  let resp_ok = mk_resp ~stop_reason:End_turn [ Text "ok" ] in
  let child_ok : unit -> string =
   fun () ->
    Handlers.mock ~llm_responses:[ resp_ok ] (fun () ->
        match Agent.run ~user_query:"q" ~tools:[] () with
        | Ok s -> s
        | Error e -> "ERROR: " ^ agent_error_pp e)
  in
  let child_fail : unit -> string =
   fun () -> failwith "deliberate child failure"
  in
  (* Spawn both, then join them defensively so a raise in one doesn't
     prevent observing the other's completion. *)
  let d_ok = Domain.spawn child_ok in
  let d_fail = Domain.spawn child_fail in
  let r_ok = Domain.join d_ok in
  let r_fail =
    try Ok (Domain.join d_fail)
    with e -> Error (Printexc.to_string e)
  in
  assert (r_ok = "ok");
  (match r_fail with
  | Error msg -> assert (Test_helpers.contains msg "deliberate")
  | Ok _ -> failwith "expected child_fail to raise");
  print_endline
    "✓ Parallel_subagent: failure in one domain is observable as \
     join-time exception, the other completes independently"

(* ========================================================================
   Domains really run concurrently — wall-clock check.
   ======================================================================== *)

let test_domains_run_concurrently () =
  (* Each child sleeps 200ms. If serialized, total ≥ 600ms. If
     parallel, total ≈ 200ms. We assert under 500ms — comfortable
     middle so CI noise doesn't flake. *)
  let child : unit -> int =
   fun () ->
    Unix.sleepf 0.2;
    1
  in
  let start = Unix.gettimeofday () in
  let results = Parallel_subagent.run [ child; child; child ] in
  let elapsed = Unix.gettimeofday () -. start in
  assert (List.length results = 3);
  assert (List.for_all (fun n -> n = 1) results);
  assert (elapsed < 0.5);
  print_endline
    (Printf.sprintf
       "✓ Parallel_subagent.run: 3×200ms work parallelizes (actual %.0fms; \
        serial would be ≥600ms)"
       (elapsed *. 1000.0))

let run () =
  test_two_parallel_subagents_isolated ();
  test_shared_log_sink_with_prefix ();
  test_failure_in_one_doesnt_affect_other ();
  test_domains_run_concurrently ()
