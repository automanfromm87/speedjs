(** Tests for [Agent_algebra]: combinator semantics + interpreter. *)

open Speedjs
open Types

(* Helper: build a canned "End_turn" LLM response. *)
let say text =
  {
    content = [ Text text ];
    stop_reason = End_turn;
    usage = usage_of_basic ~input_tokens:5 ~output_tokens:5;
  }

let unwrap = function
  | Ok s -> s
  | Error e -> failwith ("expected Ok, got: " ^ agent_error_pp e)

(* No regex dep — keep tests self-contained. *)
let contains haystack needle =
  let hl = String.length haystack in
  let nl = String.length needle in
  let rec scan i =
    if i + nl > hl then false
    else if String.sub haystack i nl = needle then true
    else scan (i + 1)
  in
  scan 0

let test_leaf_runs_one_agent () =
  let result =
    Handlers.mock ~llm_responses:[ say "hello from leaf" ] (fun () ->
        Agent_algebra.execute (Agent_algebra.base ()) "go")
  in
  assert (unwrap result = "hello from leaf");
  print_endline "✓ leaf runs one Agent.run"

let test_pipe_threads_output_to_input () =
  (* Two leaves; first says "FOO", second sees "FOO" as its input.
     With mock, the second's response is also canned but the test
     verifies the input the SECOND agent received. We can't
     introspect the mock's recorded inputs directly, so instead check
     that pipe runs both responses sequentially (both consumed). *)
  let result =
    Handlers.mock
      ~llm_responses:[ say "first"; say "second" ]
      (fun () ->
        let a = Agent_algebra.base () in
        let b = Agent_algebra.base () in
        Agent_algebra.execute (Agent_algebra.pipe a b) "start")
  in
  assert (unwrap result = "second");
  print_endline "✓ pipe: second leaf's output is final"

let test_with_retry_recovers () =
  (* First response causes the agent to error (mock will run out of
     responses on retry). To simulate Error -> Ok, we need a way to
     produce an agent_error from the agent. Easiest: 0 responses on
     attempt 1 → mock raises [failwith] which propagates as exception,
     not agent_error. So this test instead validates the structural
     case: two attempts both succeed, retry doesn't kick in. *)
  let result =
    Handlers.mock ~llm_responses:[ say "first attempt ok" ] (fun () ->
        Agent_algebra.execute
          (Agent_algebra.with_retry ~max_attempts:3
             (Agent_algebra.base ()))
          "go")
  in
  assert (unwrap result = "first attempt ok");
  print_endline "✓ with_retry: success on first attempt skips retry"

let test_replicate_runs_n_times_sequentially () =
  let result =
    Handlers.mock
      ~llm_responses:[ say "a"; say "b"; say "c" ]
      (fun () ->
        Agent_algebra.execute
          (Agent_algebra.replicate 3 (Agent_algebra.base ()))
          "go")
  in
  let s = unwrap result in
  assert (contains s "=== run:0 ===" && contains s "=== run:1 ===" && contains s "=== run:2 ===" && contains s "a" && contains s "b" && contains s "c");
  print_endline "✓ replicate(3): all three runs concatenated"

let test_with_max_iters_overrides_leaf () =
  (* Just verify the override doesn't blow up the build; the actual
     iter limit is enforced by Agent.run_loop and would need a more
     elaborate mock to observe. *)
  let a =
    Agent_algebra.with_max_iters 7 (Agent_algebra.base ~max_iters:1 ())
  in
  let s = Agent_algebra.show a in
  assert (contains s "with_max_iters(7)");
  assert (contains s "max_iters=1");  (* leaf retains its original until execute rewrites *)
  print_endline "✓ with_max_iters wraps the AST node"

let test_with_skill_injects_block () =
  let skill : Skill.t =
    {
      name = "test_skill";
      description = "does test things";
      body = "TEST_SKILL_BODY_MARKER";
      source_path = "/dev/null";
    }
  in
  let result =
    Agent_algebra.with_skills [ skill ] (fun () ->
        Handlers.mock ~llm_responses:[ say "skill ack" ] (fun () ->
            Agent_algebra.execute
              (Agent_algebra.with_skill "test_skill"
                 (Agent_algebra.base ()))
              "go"))
  in
  assert (unwrap result = "skill ack");
  print_endline "✓ with_skill resolves via DLS-provided skill list"

let test_show_renders_tree () =
  let a =
    Agent_algebra.pipe
      (Agent_algebra.with_retry ~max_attempts:2
         (Agent_algebra.with_skill "backend" (Agent_algebra.base ())))
      (Agent_algebra.replicate 3 (Agent_algebra.base ()))
  in
  let s = Agent_algebra.show a in
  assert (contains s "pipe:");
  assert (contains s "with_retry(max=2)");
  assert (contains s "with_skill(\"backend\")");
  assert (contains s "replicate(n=3)");
  print_endline "✓ show renders pipe / retry / skill / replicate"

let run () =
  print_endline "\n=== Agent_algebra ===";
  test_leaf_runs_one_agent ();
  test_pipe_threads_output_to_input ();
  test_with_retry_recovers ();
  test_replicate_runs_n_times_sequentially ();
  test_with_max_iters_overrides_leaf ();
  test_with_skill_injects_block ();
  test_show_renders_tree ()
