(** Tests for [Workflow]: leaf / bind / retry / recover / foreach. *)

open Speedjs
open Types

let say text =
  {
    content = [ Text text ];
    stop_reason = End_turn;
    usage = usage_of_basic ~input_tokens:5 ~output_tokens:5;
  }

let chat_spec ?max_iters () =
  Specs.chat ?max_iters ~tools:[] ()

let test_leaf_done () =
  let result =
    Handlers.mock ~llm_responses:[ say "hello" ] (fun () ->
        Workflow.run
          (Workflow.leaf (chat_spec ()) (Agent.Fresh "go")
           |> Workflow.expect_done ~name:"chat"))
  in
  (match result with
  | Ok (answer, _) -> assert (answer = "hello")
  | Error e -> failwith ("expected Ok, got " ^ agent_error_pp e));
  print_endline "✓ leaf + expect_done returns Ok answer"

let test_bind_threads_value () =
  (* Two leaves: first answer becomes second leaf's user query. *)
  let result =
    Handlers.mock
      ~llm_responses:[ say "first"; say "second" ]
      (fun () ->
        Workflow.run
          (let open Workflow in
           let* (answer1, _) =
             leaf (chat_spec ()) (Agent.Fresh "start")
             |> expect_done ~name:"a"
           in
           leaf (chat_spec ()) (Agent.Fresh answer1)
           |> expect_done ~name:"b"))
  in
  (match result with
  | Ok (answer, _) -> assert (answer = "second")
  | Error e -> failwith ("expected Ok, got " ^ agent_error_pp e));
  print_endline "✓ let* threads first leaf's value into second"

let test_with_retry_first_attempt_success () =
  let result =
    Handlers.mock ~llm_responses:[ say "first ok" ] (fun () ->
        Workflow.run
          (Workflow.leaf (chat_spec ()) (Agent.Fresh "go")
           |> Workflow.expect_done ~name:"chat"
           |> Workflow.with_retry ~max_attempts:3))
  in
  (match result with
  | Ok (answer, _) -> assert (answer = "first ok")
  | Error e -> failwith (agent_error_pp e));
  print_endline "✓ with_retry succeeds on first attempt without retrying"

let test_recover_substitutes_on_error () =
  (* First leaf: empty mock → fails. recover catches and returns a
     constant value via Workflow.pure. *)
  let result =
    Handlers.mock ~llm_responses:[ say "fallback" ] (fun () ->
        Workflow.run
          (let open Workflow in
           recover
             (* Failing flow: return Failed via direct construction *)
             (of_result
                (Error (Plan_invalid "synthetic failure")))
             (fun _err ->
               leaf (chat_spec ()) (Agent.Fresh "go")
               |> expect_done ~name:"recovery")))
  in
  (match result with
  | Ok (answer, _) -> assert (answer = "fallback")
  | Error e -> failwith (agent_error_pp e));
  print_endline "✓ recover substitutes a replacement flow on error"

let test_foreach_collects_results () =
  let result =
    Handlers.mock
      ~llm_responses:[ say "A"; say "B"; say "C" ]
      (fun () ->
        Workflow.run
          (Workflow.foreach [ "x"; "y"; "z" ] (fun input ->
               Workflow.leaf (chat_spec ()) (Agent.Fresh input)
               |> Workflow.expect_done ~name:"each")))
  in
  (match result with
  | Ok results ->
      let answers = List.map fst results in
      assert (answers = [ "A"; "B"; "C" ])
  | Error e -> failwith (agent_error_pp e));
  print_endline "✓ foreach runs body on each item, collects in order"

let test_foreach_short_circuits_on_error () =
  (* Inject an error flow at the second iteration; verify foreach
     stops there and returns Error. *)
  let result =
    Workflow.run
      (Workflow.foreach [ 1; 2; 3 ] (fun n ->
           if n = 2 then
             Workflow.of_result (Error (Plan_invalid "stop at 2"))
           else Workflow.pure n))
  in
  (match result with
  | Ok _ -> failwith "expected error from item 2"
  | Error (Plan_invalid msg) -> assert (msg = "stop at 2")
  | Error e ->
      failwith ("expected Plan_invalid \"stop at 2\", got " ^ agent_error_pp e));
  print_endline "✓ foreach short-circuits on first error"

let run () =
  print_endline "\n=== Workflow ===";
  test_leaf_done ();
  test_bind_threads_value ();
  test_with_retry_first_attempt_success ();
  test_recover_substitutes_on_error ();
  test_foreach_collects_results ();
  test_foreach_short_circuits_on_error ()
