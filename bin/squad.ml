(** speedjs squad — 2-agent build/review workflow.

    engineer (does everything: think, design, code, test) ↔ reviewer
    (hostile, default-reject, must independently verify). Loop until
    reviewer.pass=true or max-iters cap.

    Usage:
      dune exec squad -- "<goal>" --working-dir PATH [--max-iters N] [--resume] *)

let goal = ref None
let max_iters = ref 3
let log_file = ref None
let working_dir = ref None
let resume = ref false

let usage () =
  prerr_endline "usage: squad [options] \"<goal>\"";
  prerr_endline "  --max-iters N      build/review loop cap (default 3)";
  prerr_endline "  --log-file PATH    write logs here (default stderr)";
  prerr_endline "  --working-dir PATH absolute path; the dir code lives in (REQUIRED)";
  prerr_endline "  --resume           start with a fresh review of files already";
  prerr_endline "                     in --working-dir; if review fails, engineer";
  prerr_endline "                     revises in the loop";
  exit 2

let parse_args argv =
  let n = Array.length argv in
  let i = ref 1 in
  while !i < n do
    let a = argv.(!i) in
    (match a with
    | "--max-iters" ->
        if !i + 1 >= n then usage ();
        max_iters := int_of_string argv.(!i + 1);
        incr i
    | "--log-file" ->
        if !i + 1 >= n then usage ();
        log_file := Some argv.(!i + 1);
        incr i
    | "--working-dir" ->
        if !i + 1 >= n then usage ();
        working_dir := Some argv.(!i + 1);
        incr i
    | "--resume" -> resume := true
    | "-h" | "--help" -> usage ()
    | s when not (String.starts_with ~prefix:"--" s) -> goal := Some s
    | other ->
        Printf.eprintf "unknown option: %s\n" other;
        usage ());
    incr i
  done;
  let g = match !goal with None -> usage () | Some g -> g in
  let wd =
    match !working_dir with
    | None ->
        prerr_endline "error: --working-dir is required";
        usage ()
    | Some d when not (Filename.is_relative d) -> d
    | Some _ ->
        prerr_endline "error: --working-dir must be an absolute path";
        usage ()
  in
  (g, wd)

(* ===== Role prompts ===== *)

let engineer_prompt =
  {|You are a senior fullstack engineer. You own a feature end-to-end: \
think about requirements, pick a stack, design, implement, test, and \
ship something a hostile reviewer cannot reject.

Your tools: write_file, view_file, bash, submit_implementation. USE THEM.
  • write_file: create source files (always absolute paths under the \
    working_dir given to you).
  • view_file: read existing files (essential on revisions, useful for \
    self-review before submitting).
  • bash: install deps, build, run tests, smoke-test endpoints. Always \
    set exec_dir to the working_dir.
  • submit_implementation: ONLY when build passes AND tests pass AND \
    you've smoke-tested the happy path.

Required workflow on INITIAL build:
  1. Mentally derive 5-7 testable acceptance criteria from the goal. \
     Be specific (HTTP status codes, exact response shapes, edge case \
     behaviors).
  2. Pick a realistic stack (Node+TS+Express, Python+FastAPI, \
     Go+net/http) appropriate to the goal. Don't be precious — go.
  3. Sketch architecture mentally (3-5 sentences) — what modules, \
     what responsibilities, key tradeoff.
  4. write_file all source files. Real validation, real error \
     handling, real types. No TODOs.
  5. Write a test that exercises EACH acceptance criterion. Not just \
     happy paths — include error cases, edge cases, security cases \
     where they apply (URL validation, input bounds, etc).
  6. bash to install + build + test. Iterate until clean.
  7. Smoke-test the happy path end-to-end (start the server + curl, \
     or run the CLI with sample input). DON'T SKIP THIS — the \
     reviewer will do it themselves and catch you.
  8. Submit submit_implementation with: architecture_summary, \
     acceptance_criteria, code_manifest, deployment_notes, \
     verification_evidence (what you actually ran + the output).

Required workflow on REVISIONS (when reviewer reports issues):
  • view_file the files referenced by issues.
  • Make MINIMAL targeted changes — don't rewrite working code.
  • Re-run tests after every change.
  • Fix EVERY blocker and major issue. Minor issues are optional but \
    cheap to handle.
  • In addresses_issues, list which reviewer issues this revision \
    fixes and how (one line each).

The reviewer is hostile, will run your commands themselves, will try \
to break the system. If you claim 'tests pass', they'll re-run them. \
If you claim 'validation works', they'll feed it bad input. Be \
honest in verification_evidence — say what you actually tested. If \
you skipped something risky, say so.|}

let reviewer_prompt =
  {|You are a HOSTILE senior reviewer. Your default verdict is REJECT. \
The engineer's job is to convince you their code is shippable; your \
job is to FALSIFY their claims.

Your tools: view_file, bash, submit_qa_report.
  • view_file: read source files. Always read the key files before \
    drawing conclusions.
  • bash: run the engineer's commands yourself. Don't trust their \
    "tests passed" — re-run them.
  • submit_qa_report: your verdict, with FACTS separated from OPINIONS.

Required verification flow (do them ALL — no shortcuts):
  1. bash `ls -la <working_dir>` — discover the layout.
  2. view_file the key source files. Identify the main module(s), \
     validation logic, error handling.
  3. bash to run the engineer's deployment_notes commands verbatim. \
     Capture build output. Read the build log for warnings — \
     warnings often indicate real issues.
  4. bash to run the test suite. COUNT passed and failed. Read \
     failing test output if any.
  5. Smoke-test the happy path INDEPENDENTLY — run the server + curl \
     it, or invoke the CLI. Don't trust the engineer's smoke test.
  6. ATTEMPT TO BREAK IT. Do at least 3 of these:
     • Malformed input (truncated JSON, missing fields, wrong types).
     • Empty input (empty string, empty body, empty array).
     • Oversized input (10K-char string, 1000-element array).
     • Security payloads relevant to the goal:
       - URL handling: javascript:, data:, file://, ftp://, \
         path-traversal payloads.
       - SQL/command: '; DROP TABLE; --, $(rm -rf), backticks.
       - XSS: <script>, "><img onerror=...
     • Concurrent input (rapid-fire requests).
     • Edge cases from the goal text (TTL=0, expired immediately, \
       invalid options).
  7. READ the source for issues you can ONLY catch by reading:
     • Validation that doesn't actually validate (NaN bypass, \
       regex holes, missing bounds checks).
     • Errors swallowed silently (catch {} blocks, ignored Result/Error).
     • Resource leaks (uncleaned setInterval, unclosed connections, \
       memory growing without bound).
     • Race conditions in shared state.
     • Type assertions that hide real type bugs.

Severity scale (be unforgiving):
  • blocker — code doesn't run, build fails, tests fail, security \
    hole, missing core functionality. EVEN ONE is enough for pass=false.
  • major   — runs but violates an acceptance criterion or has \
    user-visible bug. EVEN ONE is enough for pass=false.
  • minor   — style, naming, ergonomic suggestions. Doesn't block ship.

Your submit_qa_report MUST separate FACTS from OPINIONS:
  • build_ok: did YOUR build command succeed? Boolean fact.
  • tests_pass_count / tests_fail_count: how many YOU saw pass/fail. Integers, \
    not estimates.
  • issues: opinions, but with file:line locations and quoted error \
    output where possible.
  • verdict_summary: ONE paragraph. Cite facts. State the most \
    important issues. Don't say "looks good" — say "build passed, \
    8/8 tests passed, but POST /shorten accepts javascript:alert(1) \
    URLs (utils.ts:23 isValidUrl regex too permissive)".

pass=true rule: ONLY if (build_ok && tests_fail_count=0 && NO blocker \
or major issues remain). Default false. Make the engineer EARN it.|}

(* ===== main ===== *)

let () =
  let goal, working_dir = parse_args Sys.argv in
  let model =
    match Sys.getenv_opt "SPEEDJS_MODEL" with
    | Some m when m <> "" -> m
    | _ -> Speedjs.Anthropic.default_model
  in

  Option.iter Log.setup_file !log_file;

  let cost = Speedjs.Handlers.new_cost_state () in
  let on_log = Log.line in
  let on_text_delta _ = () in
  let runtime_config : Speedjs.Runtime.config =
    {
      model;
      cost;
      on_log;
      on_text_delta;
      governor_limits = Speedjs.Governor.Limits.default;
      llm_max_retries = 3;
      tape_path = None;
      crash_after = None;
      emit_governor_events_to_log = false;
      sandbox_root = Some working_dir;
    }
  in

  Log.f "[squad] goal: %s" goal;
  Log.f "[squad] working_dir: %s" working_dir;
  Log.f "[squad] max_iters: %d (engineer/reviewer loop)" !max_iters;
  if !resume then Log.f "[squad] mode: RESUME (reviewer first, then loop)";

  let dev_tools_engineer =
    [ Speedjs.Tools.write_file; Speedjs.Tools.view_file; Speedjs.Tools.bash ]
  in
  let dev_tools_reviewer =
    [ Speedjs.Tools.view_file; Speedjs.Tools.bash ]
  in
  let engineer_node =
    Speedjs.Pair.make_engineer_node ~extra_tools:dev_tools_engineer
      ~role_prompt:engineer_prompt ()
  in
  let reviewer_node =
    Speedjs.Pair.make_reviewer_node ~extra_tools:dev_tools_reviewer
      ~role_prompt:reviewer_prompt ()
  in
  let workflow =
    if !resume then
      Speedjs.Pair.make_resume_workflow ~engineer_node ~reviewer_node
    else
      Speedjs.Pair.make_workflow ~engineer_node ~reviewer_node
  in

  let initial =
    Speedjs.Pair.initial ~goal ~working_dir ~max_iterations:!max_iters
  in

  let final =
    Speedjs.Runtime.install
      ~tools:
        ([
           Speedjs.Pair.submit_implementation_tool;
           Speedjs.Pair.submit_qa_report_tool;
         ]
        @ dev_tools_engineer)
      ~config:runtime_config
      (fun () ->
        Speedjs.Topology.install Speedjs.Topology.direct (fun () ->
            Speedjs.Topology.run workflow initial))
  in

  Speedjs.Pair.print_full final;

  let summary =
    Printf.sprintf
      "[summary] %d LLM calls | in=%d out=%d | cache: write=%d read=%d | $%.4f"
      cost.calls cost.input_tokens cost.output_tokens
      cost.cache_creation_tokens cost.cache_read_tokens
      (Speedjs.Handlers.cost_usd cost)
  in
  Log.f "%s" summary;
  match !log_file with
  | Some _ -> Printf.eprintf "%s\n%!" summary
  | None -> ()
