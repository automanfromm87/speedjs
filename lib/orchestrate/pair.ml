(** Two-agent build/review workflow built on [Topology].

    The minimal-multi-agent design: engineer + hostile reviewer in a
    revision loop. PM and Design phases are folded INTO the engineer
    (it must produce architecture_summary + acceptance_criteria as
    part of its [submit_implementation] payload). The reviewer is
    deliberately strict — defaults to REJECT, must independently
    verify every claim by running tests / inspecting code / trying
    to break the system.

    Topology:
      Loop_until {
        cond = qa.pass || iteration >= max,
        body = Sequence [engineer; reviewer]
      }

    First iteration: engineer builds initial; reviewer reviews.
    Subsequent: engineer revises against reviewer.issues; reviewer re-reviews.

    Compared to [Team] (4-agent), this is ~50% the cost for ~90% of
    the quality, because:
    - PM/Design phases were producing structure that engineer can
      generate internally (forced via prompt + schema).
    - The cross-agent context-rebuild cost (~30%) is gone for those
      two stages.
    - The genuinely-valuable separation (adversarial review) stays. *)

open Types

(* ===== Types ===== *)

type artifact = {
  author : string;
  content : string;
  iteration : int;
}

type qa_issue = {
  severity : string;        (* "blocker" / "major" / "minor" *)
  description : string;
  location : string;        (* file:line or function name; "" if global *)
}

type qa_report = {
  pass : bool;
      (** True ONLY if (a) build_ok && (b) tests_fail_count = 0 &&
          (c) no blocker/major issues. Reviewer default is false. *)
  build_ok : bool;            (** Reviewer ran the build itself; it succeeded. *)
  tests_pass_count : int;     (** Reviewer ran the tests; this many passed. *)
  tests_fail_count : int;     (** And this many failed. *)
  issues : qa_issue list;
  recommendations : string list;
  verdict_summary : string;
      (** One paragraph; reviewer's overall assessment, citing facts. *)
}

type t = {
  goal : string;
  working_dir : string;
  implementation : artifact option;
      (** Engineer's submitted manifest + meta-info. The actual code
          is on disk. *)
  latest_qa : qa_report option;
  iteration : int;            (** engineer/reviewer loop counter *)
  max_iterations : int;
  history : (artifact * qa_report option) list;
}

let initial ~goal ~working_dir ~max_iterations =
  {
    goal;
    working_dir;
    implementation = None;
    latest_qa = None;
    iteration = 0;
    max_iterations;
    history = [];
  }

(* ===== Terminal tools ===== *)

let make_terminal_tool ~name ~description ~input_schema : tool_def =
  {
    idempotent = true;
    timeout_sec = None;
    category = "meta";
    name;
    description;
    input_schema;
    handler =
      (fun _ ->
        Error
          (Printf.sprintf
             "%s is a terminal tool — its handler should never be invoked"
             name));
  }

let submit_implementation_name = "submit_implementation"

let submit_implementation_tool : tool_def =
  make_terminal_tool ~name:submit_implementation_name
    ~description:
      "Submit the completed implementation. Call this ONLY when the \
       build succeeds AND your tests pass AND you've smoke-tested the \
       happy path. Provide structured meta-information so the reviewer \
       can verify your claims independently."
    ~input_schema:
      (`Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "architecture_summary",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "1 paragraph (3-5 sentences) on the design \
                           you chose: stack, key components, main \
                           tradeoff." );
                    ] );
                ( "acceptance_criteria",
                  `Assoc
                    [
                      ("type", `String "array");
                      ("items", `Assoc [ ("type", `String "string") ]);
                      ( "description",
                        `String
                          "5-7 testable acceptance criteria you \
                           targeted (e.g. 'POST /shorten returns 200 \
                           with valid URL'). Each must be objectively \
                           verifiable by the reviewer." );
                    ] );
                ( "code_manifest",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "List of files you wrote, one per line, with \
                           a 1-line description each. Source is on \
                           disk." );
                    ] );
                ( "deployment_notes",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Exact bash commands to install + build + \
                           run + test, copy-pasteable. The reviewer \
                           will run these verbatim." );
                    ] );
                ( "verification_evidence",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "What you tested + the test output. e.g. \
                           'npm test: 8/8 passed (294ms); curl POST \
                           returned {code, expires_at}; curl GET \
                           returned 302'. Be SPECIFIC." );
                    ] );
                ( "addresses_issues",
                  `Assoc
                    [
                      ("type", `String "array");
                      ("items", `Assoc [ ("type", `String "string") ]);
                      ( "description",
                        `String
                          "Empty on initial submission. On revisions, \
                           list which reviewer issues this iteration \
                           addresses + how (one line each)." );
                    ] );
              ] );
          ( "required",
            `List
              [
                `String "architecture_summary";
                `String "acceptance_criteria";
                `String "code_manifest";
                `String "deployment_notes";
                `String "verification_evidence";
              ] );
        ])

let submit_qa_report_name = "submit_qa_report"

let submit_qa_report_tool : tool_def =
  make_terminal_tool ~name:submit_qa_report_name
    ~description:
      "Submit your review verdict. You MUST have independently run \
       the engineer's deployment_notes commands AND inspected the \
       code AND attempted to break the system. Report FACTS \
       (build_ok, test counts) separately from OPINIONS (issues, \
       recommendations). pass=true is only earned when build_ok && \
       tests_fail_count=0 && no blocker/major issues remain."
    ~input_schema:
      (`Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "pass",
                  `Assoc
                    [
                      ("type", `String "boolean");
                      ( "description",
                        `String
                          "true ONLY if (a) build_ok && (b) \
                           tests_fail_count=0 && (c) NO blocker or \
                           major issues. Default to false; you must \
                           EARN pass=true." );
                    ] );
                ( "build_ok",
                  `Assoc
                    [
                      ("type", `String "boolean");
                      ( "description",
                        `String
                          "FACT: did the build command (typescript \
                           compile / cargo build / etc) succeed when \
                           YOU ran it?" );
                    ] );
                ( "tests_pass_count",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ( "description",
                        `String
                          "FACT: how many tests passed when YOU ran \
                           them? Read from the test runner's output." );
                    ] );
                ( "tests_fail_count",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ( "description",
                        `String "FACT: how many tests failed?" );
                    ] );
                ( "issues",
                  `Assoc
                    [
                      ("type", `String "array");
                      ( "items",
                        `Assoc
                          [
                            ("type", `String "object");
                            ( "properties",
                              `Assoc
                                [
                                  ( "severity",
                                    `Assoc
                                      [
                                        ("type", `String "string");
                                        ( "enum",
                                          `List
                                            [
                                              `String "blocker";
                                              `String "major";
                                              `String "minor";
                                            ] );
                                      ] );
                                  ( "description",
                                    `Assoc
                                      [ ("type", `String "string") ] );
                                  ( "location",
                                    `Assoc
                                      [
                                        ("type", `String "string");
                                        ( "description",
                                          `String
                                            "file:line or function \
                                             name; \"\" if no specific \
                                             location" );
                                      ] );
                                ] );
                            ( "required",
                              `List
                                [
                                  `String "severity";
                                  `String "description";
                                  `String "location";
                                ] );
                          ] );
                    ] );
                ( "recommendations",
                  `Assoc
                    [
                      ("type", `String "array");
                      ("items", `Assoc [ ("type", `String "string") ]);
                      ( "description",
                        `String
                          "Concrete fix suggestions — code-level when \
                           you can." );
                    ] );
                ( "verdict_summary",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "ONE paragraph. Cite the facts (build, \
                           tests) and the most important issues. No \
                           hand-waving — be specific." );
                    ] );
              ] );
          ( "required",
            `List
              [
                `String "pass";
                `String "build_ok";
                `String "tests_pass_count";
                `String "tests_fail_count";
                `String "issues";
                `String "recommendations";
                `String "verdict_summary";
              ] );
        ])

(* ===== Parsers ===== *)

let parse_implementation (input : Yojson.Safe.t) : string =
  let open Json_decode in
  match input with
  | `Assoc fs ->
      let arch = get_string_field_or "architecture_summary" ~default:"" fs in
      let manifest = get_string_field_or "code_manifest" ~default:"" fs in
      let notes = get_string_field_or "deployment_notes" ~default:"" fs in
      let evidence =
        get_string_field_or "verification_evidence" ~default:"" fs
      in
      let acs =
        match List.assoc_opt "acceptance_criteria" fs with
        | Some (`List items) ->
            List.filter_map
              (function `String s -> Some s | _ -> None)
              items
        | _ -> []
      in
      let addresses =
        match List.assoc_opt "addresses_issues" fs with
        | Some (`List items) ->
            List.filter_map
              (function `String s -> Some s | _ -> None)
              items
        | _ -> []
      in
      let acs_block =
        if acs = [] then ""
        else
          "\n\nAcceptance criteria targeted:\n"
          ^ String.concat "\n" (List.map (fun s -> "  ✓ " ^ s) acs)
      in
      let addr_block =
        if addresses = [] then ""
        else
          "\n\nAddresses issues:\n"
          ^ String.concat "\n"
              (List.map (fun s -> "  ↳ " ^ s) addresses)
      in
      Printf.sprintf
        "Architecture: %s\n\nManifest:\n%s\n\nDeployment notes: %s\n\n\
         Verification: %s%s%s"
        arch manifest notes evidence acs_block addr_block
  | _ -> "(invalid implementation input)"

let parse_qa_report (input : Yojson.Safe.t) : qa_report =
  let open Json_decode in
  match input with
  | `Assoc fs ->
      let pass = get_bool_field_or "pass" ~default:false fs in
      let build_ok = get_bool_field_or "build_ok" ~default:false fs in
      let int_or name =
        match List.assoc_opt name fs with Some (`Int n) -> n | _ -> 0
      in
      let tests_pass_count = int_or "tests_pass_count" in
      let tests_fail_count = int_or "tests_fail_count" in
      let issues =
        match List.assoc_opt "issues" fs with
        | Some (`List items) ->
            List.filter_map
              (function
                | `Assoc ifs ->
                    let severity =
                      get_string_field_or "severity" ~default:"minor" ifs
                    in
                    let description =
                      get_string_field_or "description" ~default:"" ifs
                    in
                    let location =
                      get_string_field_or "location" ~default:"" ifs
                    in
                    if description = "" then None
                    else Some { severity; description; location }
                | _ -> None)
              items
        | _ -> []
      in
      let recommendations =
        match List.assoc_opt "recommendations" fs with
        | Some (`List items) ->
            List.filter_map
              (function `String s -> Some s | _ -> None)
              items
        | _ -> []
      in
      let verdict_summary =
        get_string_field_or "verdict_summary" ~default:"" fs
      in
      {
        pass;
        build_ok;
        tests_pass_count;
        tests_fail_count;
        issues;
        recommendations;
        verdict_summary;
      }
  | _ ->
      {
        pass = false;
        build_ok = false;
        tests_pass_count = 0;
        tests_fail_count = 0;
        issues = [];
        recommendations = [];
        verdict_summary = "(invalid input)";
      }

(* ===== Generic role runner (same pattern as Team) ===== *)

let run_role ~name ~role_prompt ~user_input ~terminal_tool
    ~(dev_tools : tool_def list) ~max_iterations () =
  let conv =
    match
      Conversation.of_messages
        [ { role = User; content = [ Text user_input ] } ]
    with
    | Ok c -> c
    | Error msg -> failwith ("pair: malformed input — " ^ msg)
  in
  let all_tools = terminal_tool :: dev_tools in
  let ctx =
    Context.empty
    |> Context.with_tools all_tools
    |> Context.apply_system ~system_prompt:role_prompt
    |> Context.with_conversation conv
  in
  Agent.run_loop ~max_iterations
    ~terminal_tools:[ terminal_tool.name ]
    ~name ~ctx ()

let run_and_capture ~name ~role_prompt ~user_input ~terminal_tool
    ?(dev_tools = []) ?(max_iterations = 200) () : Yojson.Safe.t =
  try
    match
      run_role ~name ~role_prompt ~user_input ~terminal_tool ~dev_tools
        ~max_iterations ()
    with
    | Ok (text, _) ->
        `Assoc
          [
            ("_error",
             `String "agent ended turn without calling terminal tool");
            ("_text", `String text);
          ]
    | Error (e, _) ->
        `Assoc [ ("_error", `String (agent_error_pp e)) ]
  with Agent.Task_terminal_called { input; _ } -> input

(* ===== Engineer node ===== *)

let make_engineer_node ?(extra_tools = []) ~role_prompt () : t -> t =
 fun state ->
  let iter = state.iteration + 1 in
  let user_input =
    match (state.implementation, state.latest_qa) with
    | None, _ ->
        Printf.sprintf
          "GOAL: %s\n\nWORKING DIRECTORY: %s\n\n\
           This is iteration %d of max %d. Build the project from scratch.\n\n\
           Required steps:\n\
          \  1. Pick a stack appropriate for the goal.\n\
          \  2. Mentally list 5-7 testable acceptance criteria you'll \
           target — these go into submit_implementation later.\n\
          \  3. Sketch architecture (3-5 sentences mentally).\n\
          \  4. Use write_file to create source files under %s.\n\
          \  5. Use bash (exec_dir=%s) to install dependencies, build, \
           run tests. Verify they pass.\n\
          \  6. Use bash to smoke-test the happy path (curl an HTTP \
           endpoint, run the CLI, etc).\n\
          \  7. ONLY THEN call submit_implementation with all required \
           fields filled — including verification_evidence describing \
           what you actually ran and saw.\n\n\
           Pick realistic tools: real validation, real error handling, \
           at least one passing test. The reviewer is hostile and will \
           independently re-run everything you claim."
          state.goal state.working_dir iter state.max_iterations
          state.working_dir state.working_dir
    | Some _, Some qa ->
        let issues_text =
          if qa.issues = [] then "(no specific issues)"
          else
            qa.issues
            |> List.map (fun i ->
                   let loc =
                     if i.location = "" then "" else " (" ^ i.location ^ ")"
                   in
                   Printf.sprintf "  [%s] %s%s" i.severity i.description loc)
            |> String.concat "\n"
        in
        let recs_text =
          if qa.recommendations = [] then ""
          else
            "\n\nReviewer suggestions:\n"
            ^ String.concat "\n"
                (List.map (fun s -> "  • " ^ s) qa.recommendations)
        in
        Printf.sprintf
          "GOAL: %s\n\nWORKING DIRECTORY: %s\n\n\
           REVIEWER VERDICT (iteration %d of max %d):\n\
          \  build_ok: %b\n\
          \  tests: %d passed / %d failed\n\
          \  pass: %b\n\
           Verdict: %s\n\n\
           ISSUES:\n%s%s\n\n\
           Your job: revise the implementation. Steps:\n\
          \  1. view_file the files referenced by issues.\n\
          \  2. write_file fixes — minimal targeted changes preferred.\n\
          \  3. bash to re-run tests until they pass.\n\
          \  4. submit_implementation with addresses_issues populated.\n\n\
           Fix EVERY blocker and major issue. The reviewer will \
           re-verify independently — don't claim something is fixed if \
           it's not."
          state.goal state.working_dir iter state.max_iterations
          qa.build_ok qa.tests_pass_count qa.tests_fail_count qa.pass
          qa.verdict_summary issues_text recs_text
    | Some _, None ->
        Printf.sprintf
          "GOAL: %s\n\nWORKING DIRECTORY: %s\n\n\
           Continue iterating on the implementation (no reviewer \
           verdict yet — treat as initial build). Submit via \
           submit_implementation."
          state.goal state.working_dir
  in
  let input =
    run_and_capture ~name:"engineer" ~role_prompt ~user_input
      ~terminal_tool:submit_implementation_tool ~dev_tools:extra_tools ()
  in
  let new_impl =
    {
      author = "engineer";
      content = parse_implementation input;
      iteration = iter;
    }
  in
  { state with implementation = Some new_impl; iteration = iter }

(* ===== Reviewer node ===== *)

let make_reviewer_node ?(extra_tools = []) ~role_prompt () : t -> t =
 fun state ->
  match state.implementation with
  | None -> state
  | Some impl ->
      let user_input =
        Printf.sprintf
          "GOAL: %s\n\nWORKING DIRECTORY: %s\n\n\
           ENGINEER SUBMITTED (iteration %d of max %d):\n\
           %s\n\n\
           Your job: VERIFY the engineer's claims independently. The \
           engineer may be sloppy or self-deceiving. Default verdict \
           is REJECT until proven otherwise.\n\n\
           Required verification flow (do them ALL):\n\
          \  1. bash `ls -la %s` — discover the layout.\n\
          \  2. view_file the key source files (especially anything \
           referenced in the manifest).\n\
          \  3. bash to run the engineer's deployment_notes commands \
           VERBATIM (or close). Capture build output. Read it.\n\
          \  4. bash to run the test suite. Count pass/fail. Read \
           failing test output if any.\n\
          \  5. bash to smoke-test the happy path independently — if \
           it's an HTTP service, start it and curl it; if CLI, invoke \
           it.\n\
          \  6. ATTEMPT TO BREAK IT — at least 3 of: malformed input, \
           empty input, oversized input, concurrent input, security \
           payloads (XSS / injection / path traversal / wrong \
           protocols), edge cases from the goal text.\n\
          \  7. Read the source carefully for issues you can only \
           catch by reading: missing validation, swallowed errors, \
           resource leaks, race conditions.\n\n\
           Severity scale (be unforgiving):\n\
          \  • blocker  — could fail in production. Build broken, \
           security hole, missing core functionality, definitely-buggy \
           logic.\n\
          \  • major    — violates an acceptance criterion or has \
           user-visible bug. Missing edge case, wrong error code, slow \
           on realistic input.\n\
          \  • minor    — style, naming, suggestions for improvement.\n\n\
           Submit submit_qa_report with:\n\
          \  • pass: true ONLY IF (build_ok && tests_fail_count=0 && \
           NO blocker or major issues). Default false.\n\
          \  • build_ok / tests_pass_count / tests_fail_count: FACTS \
           from running the commands. Don't trust the engineer's \
           verification_evidence.\n\
          \  • issues: with file:line locations and quoted error \
           output where possible.\n\
          \  • recommendations: code-level when you can.\n\
          \  • verdict_summary: ONE paragraph. Cite the facts. No \
           hand-waving."
          state.goal state.working_dir state.iteration
          state.max_iterations impl.content state.working_dir
      in
      let input =
        run_and_capture ~name:"reviewer" ~role_prompt ~user_input
          ~terminal_tool:submit_qa_report_tool ~dev_tools:extra_tools ()
      in
      let report = parse_qa_report input in
      let new_history = (impl, Some report) :: state.history in
      { state with latest_qa = Some report; history = new_history }

(* ===== Topology builder ===== *)

let make_workflow ~engineer_node ~reviewer_node : t Topology.shape =
  let loop_cond s =
    (match s.latest_qa with Some r when r.pass -> true | _ -> false)
    || s.iteration >= s.max_iterations
  in
  Topology.Loop_until
    {
      cond = loop_cond;
      body =
        Topology.Sequence
          [
            Topology.Node { name = "engineer"; run = engineer_node };
            Topology.Node { name = "reviewer"; run = reviewer_node };
          ];
      max_iters = None;
    }

(** Resume topology: skip the initial engineer build, start with a
    fresh reviewer pass on whatever's already in working_dir. If
    reviewer passes, done. Otherwise enter the normal loop. *)
let make_resume_workflow ~engineer_node ~reviewer_node : t Topology.shape =
  Topology.Sequence
    [
      Topology.Node { name = "reviewer"; run = reviewer_node };
      Topology.Loop_until
        {
          cond =
            (fun s ->
              (match s.latest_qa with
              | Some r when r.pass -> true
              | _ -> false)
              || s.iteration >= s.max_iterations);
          body =
            Topology.Sequence
              [
                Topology.Node { name = "engineer"; run = engineer_node };
                Topology.Node { name = "reviewer"; run = reviewer_node };
              ];
          max_iters = None;
        };
    ]

(* ===== Pretty printer ===== *)

let print_summary state =
  print_endline "";
  print_endline "============== PAIR RUN SUMMARY ==============";
  Printf.printf "Goal: %s\n" state.goal;
  Printf.printf "Working dir: %s\n" state.working_dir;
  Printf.printf "Iterations used: %d / %d\n" state.iteration state.max_iterations;
  (match state.latest_qa with
  | Some r when r.pass ->
      Printf.printf
        "Verdict: ✅ PASSED on iteration %d (build_ok=%b, %d/%d tests)\n"
        state.iteration r.build_ok r.tests_pass_count
        (r.tests_pass_count + r.tests_fail_count)
  | Some r ->
      Printf.printf
        "Verdict: ❌ FAILED after %d iteration(s) — build_ok=%b, %d/%d \
         tests, %d issue(s)\n"
        state.iteration r.build_ok r.tests_pass_count
        (r.tests_pass_count + r.tests_fail_count)
        (List.length r.issues)
  | None -> Printf.printf "Verdict: (no review yet)\n");
  print_endline ""

let print_qa_report = function
  | None -> ()
  | Some r ->
      print_endline "──── REVIEWER VERDICT ────";
      Printf.printf "pass: %b | build_ok: %b | tests: %d passed / %d failed\n"
        r.pass r.build_ok r.tests_pass_count r.tests_fail_count;
      Printf.printf "\n%s\n" r.verdict_summary;
      if r.issues <> [] then begin
        print_endline "\nIssues:";
        List.iter
          (fun i ->
            let loc = if i.location = "" then "" else " (" ^ i.location ^ ")" in
            Printf.printf "  [%s] %s%s\n" i.severity i.description loc)
          r.issues
      end;
      if r.recommendations <> [] then begin
        print_endline "\nRecommendations:";
        List.iter (fun s -> Printf.printf "  • %s\n" s) r.recommendations
      end;
      print_endline ""

let print_full state =
  print_summary state;
  (match state.implementation with
  | None -> Printf.printf "── implementation: (none)\n\n"
  | Some a -> Printf.printf "──── IMPLEMENTATION SUMMARY ────\n%s\n\n" a.content);
  print_qa_report state.latest_qa;
  if List.length state.history > 1 then begin
    Printf.printf "──── HISTORY (%d iteration(s)) ────\n"
      (List.length state.history);
    let n = List.length state.history in
    List.iteri
      (fun i (_impl, qa) ->
        let iter_no = n - i in
        match qa with
        | Some r ->
            Printf.printf
              "  iter %d: pass=%b, build_ok=%b, tests %d/%d, %d issue(s)\n"
              iter_no r.pass r.build_ok r.tests_pass_count
              (r.tests_pass_count + r.tests_fail_count)
              (List.length r.issues)
        | None -> ())
      state.history
  end;
  print_endline "==============================================="
