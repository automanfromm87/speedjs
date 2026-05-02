(** Tests for the orchestrate.Topology layer.

    Covers each constructor (Node / Sequence / Parallel / Loop_until /
    Branch) and the four canonical patterns expressible via them:
    pipeline, debate-shaped loop, scatter-gather, manager-worker. *)

open Speedjs

let mk_node name f = Topology.Node { name; run = f }

(* ========================================================================
   Pipeline: Sequence threads state through 3 nodes.
   ======================================================================== *)

type pipe_state = { topic : string; draft : string; final : string }

let test_pipeline_sequence () =
  let researcher =
    mk_node "researcher" (fun s ->
        { s with draft = Printf.sprintf "facts about %s" s.topic })
  in
  let writer =
    mk_node "writer" (fun s -> { s with draft = "draft: " ^ s.draft })
  in
  let editor =
    mk_node "editor" (fun s -> { s with final = "edited: " ^ s.draft })
  in
  let pipeline = Topology.Sequence [ researcher; writer; editor ] in
  let result =
    Topology.run pipeline { topic = "OCaml"; draft = ""; final = "" }
  in
  assert (result.final = "edited: draft: facts about OCaml");
  print_endline
    "✓ Pipeline (Sequence) threads state through 3 nodes left-to-right"

(* ========================================================================
   Debate-shaped: Loop_until + Sequence + agree-flag.
   ======================================================================== *)

type debate_state = {
  rounds : int;
  pro_said : string list;
  skeptic_said : string list;
  consensus : bool;
}

let test_debate_loop () =
  let pro =
    mk_node "pro" (fun s ->
        let arg = Printf.sprintf "pro-r%d" s.rounds in
        { s with pro_said = arg :: s.pro_said })
  in
  let skeptic =
    mk_node "skeptic" (fun s ->
        let arg = Printf.sprintf "skeptic-r%d" s.rounds in
        let agree = s.rounds >= 2 in
        {
          s with
          skeptic_said = arg :: s.skeptic_said;
          rounds = s.rounds + 1;
          consensus = agree;
        })
  in
  let debate =
    Topology.Loop_until
      {
        cond = (fun s -> s.consensus);
        body = Topology.Sequence [ pro; skeptic ];
        max_iters = Some 5;
      }
  in
  let initial =
    { rounds = 0; pro_said = []; skeptic_said = []; consensus = false }
  in
  let result = Topology.run debate initial in
  assert result.consensus;
  assert (result.rounds = 3);
  assert (List.length result.pro_said = 3);
  assert (List.length result.skeptic_said = 3);
  print_endline
    "✓ Debate-shaped (Loop_until + Sequence) — converges via consensus flag"

let test_loop_until_max_iters_floor () =
  let inc =
    mk_node "inc" (fun n -> n + 1)
  in
  let loop =
    Topology.Loop_until
      {
        cond = (fun _ -> false);
        (* never satisfied *)
        body = inc;
        max_iters = Some 5;
      }
  in
  assert (Topology.run loop 0 = 5);
  print_endline "✓ Loop_until: max_iters caps when cond never true"

(* ========================================================================
   Scatter-gather: Parallel + merge.
   ======================================================================== *)

type scatter_state = { tasks : string list; results : string list }

let test_parallel_with_merge () =
  let analyze label =
    mk_node ("analyze:" ^ label) (fun s ->
        { s with results = (label ^ "-done") :: s.results })
  in
  let merge a b =
    (* Concatenate results from both branches, dedupe is caller's
       problem in this test we just take the union. *)
    {
      a with
      results =
        List.sort_uniq compare (a.results @ b.results);
    }
  in
  let scatter =
    Topology.Parallel
      {
        merge;
        branches = [ analyze "A"; analyze "B"; analyze "C" ];
      }
  in
  let initial = { tasks = []; results = [] } in
  let result = Topology.run scatter initial in
  let sorted = List.sort compare result.results in
  assert (sorted = [ "A-done"; "B-done"; "C-done" ]);
  print_endline
    "✓ Parallel + merge: 3 branches over same start, results union-merged"

(* ========================================================================
   Branch: dynamic routing.
   ======================================================================== *)

let test_branch_dynamic_routing () =
  let to_a = mk_node "A" (fun n -> n + 100) in
  let to_b = mk_node "B" (fun n -> n + 1000) in
  let router =
    Topology.Branch
      (fun n -> if n mod 2 = 0 then to_a else to_b)
  in
  assert (Topology.run router 4 = 104);
  assert (Topology.run router 5 = 1005);
  print_endline "✓ Branch: chooses node from current state"

(* ========================================================================
   Manager-worker: Loop_until + Sequence + Branch (recovery shape).
   ======================================================================== *)

type mw_state = {
  pending : string list;
  completed : string list;
  next : string option;
  done_ : bool;
}

let test_manager_worker_loop () =
  let manager =
    mk_node "manager" (fun s ->
        match s.pending with
        | [] -> { s with next = None; done_ = true }
        | t :: rest -> { s with pending = rest; next = Some t })
  in
  let worker =
    mk_node "worker" (fun s ->
        match s.next with
        | Some t -> { s with completed = (t ^ "-ok") :: s.completed; next = None }
        | None -> s)
  in
  let workflow =
    Topology.Loop_until
      {
        cond = (fun s -> s.done_);
        body = Topology.Sequence [ manager; worker ];
        max_iters = Some 20;
      }
  in
  let initial =
    { pending = [ "t1"; "t2"; "t3" ]; completed = []; next = None; done_ = false }
  in
  let result = Topology.run workflow initial in
  assert result.done_;
  assert (List.sort compare result.completed = [ "t1-ok"; "t2-ok"; "t3-ok" ]);
  print_endline
    "✓ Manager-worker (Loop_until + Sequence) — manager dispatches, worker \
     consumes, terminates on empty pending"

(* ========================================================================
   Middleware: with_logging fires for every sub-step.
   ======================================================================== *)

let test_with_logging_observes_every_step () =
  let observed = ref [] in
  let on_event s = observed := s :: !observed in
  let chain = Topology.with_logging ~on_event Topology.direct in
  let inc = mk_node "inc" (fun n -> n + 1) in
  let pipeline = Topology.Sequence [ inc; inc; inc ] in
  let _ = Topology.run_with chain pipeline 0 in
  let log = List.rev !observed in
  (* Expect entry+exit for the top-level Sequence + each inner Node. *)
  let entries = List.filter (fun s -> String.starts_with ~prefix:"→" s) log in
  let exits = List.filter (fun s -> String.starts_with ~prefix:"←" s) log in
  assert (List.length entries = 4);
  assert (List.length exits = 4);
  assert (List.exists (fun s -> Test_helpers.contains s "Sequence[3]") entries);
  assert (
    List.length
      (List.filter (fun s -> Test_helpers.contains s "Node(inc)") entries)
    = 3);
  print_endline
    "✓ with_logging: emits entry/exit for every shape including each inner \
     sub-step"

(* ========================================================================
   Middleware: with_retry retries on exception.
   ======================================================================== *)

let test_with_retry_recovers_on_failure () =
  let attempts = ref 0 in
  let flaky =
    mk_node "flaky" (fun n ->
        incr attempts;
        if !attempts < 3 then failwith "flaky"
        else n + 100)
  in
  let chain =
    Topology.direct
    |> Topology.with_retry ~max_attempts:5 ~base_delay:0.001 ~cap:0.01
  in
  let result = Topology.run_with chain flaky 0 in
  assert (result = 100);
  assert (!attempts = 3);
  print_endline
    "✓ with_retry: retries failing nodes up to max_attempts then passes the \
     succeeding result"

let test_with_retry_gives_up_after_max () =
  let attempts = ref 0 in
  let always_fails =
    mk_node "always_fails" (fun _ ->
        incr attempts;
        failwith "boom")
  in
  let chain = Topology.direct |> Topology.with_retry ~max_attempts:3 ~base_delay:0.001 in
  (try
     let _ = Topology.run_with chain always_fails 0 in
     failwith "expected failure to propagate"
   with Failure msg -> assert (msg = "boom"));
  assert (!attempts = 3);
  print_endline
    "✓ with_retry: re-raises after max_attempts exhausted (no swallowing)"

let run () =
  test_pipeline_sequence ();
  test_debate_loop ();
  test_loop_until_max_iters_floor ();
  test_parallel_with_merge ();
  test_branch_dynamic_routing ();
  test_manager_worker_loop ();
  test_with_logging_observes_every_step ();
  test_with_retry_recovers_on_failure ();
  test_with_retry_gives_up_after_max ()
