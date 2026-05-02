(** speedjs squad — 4-agent engineering team workflow.

    Pipeline shape:
      PM → Design → Loop_until(qa.pass) { Fullstack → QA }

    Usage:
      dune exec squad -- "<goal>" [--max-iters N] [--log-file PATH] *)

let goal = ref None
let max_iters = ref 3
let log_file = ref None
let working_dir = ref None
let resume = ref false

let usage () =
  prerr_endline "usage: squad [options] \"<goal>\"";
  prerr_endline "  --max-iters N      fullstack/qa loop cap (default 3)";
  prerr_endline "  --log-file PATH    write logs here (default stderr)";
  prerr_endline "  --working-dir PATH absolute path; the dir code lives in (REQUIRED)";
  prerr_endline "  --resume           skip PM+design, start with a fresh QA pass on";
  prerr_endline "                     existing files in --working-dir; if QA fails,";
  prerr_endline "                     fullstack revises in the loop";
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

(* ===== role prompts ===== *)

let product_prompt =
  {|You are a senior product manager. Your job is to translate user goals \
into clear, testable specifications.

What you do:
  • Identify the core user problem and the value of solving it.
  • Write user stories ('As X I want Y so that Z').
  • Write CONCRETE, TESTABLE acceptance criteria — each one must be \
    verifiable by a test or observation.
  • Call out non-goals explicitly to prevent scope creep.

What you DON'T do:
  • You do NOT design the architecture. You do NOT pick technologies. \
    You do NOT write code.

Be specific and avoid generalities. If the goal is ambiguous, make a \
defensible interpretation and state it. Submit your output via \
submit_spec.|}

let design_prompt =
  {|You are a senior technical designer / architect. You translate product \
specs into technical designs the engineering team can implement.

What you produce:
  • A multi-paragraph design rationale: architecture, key decisions, \
    tradeoffs, why this shape and not another.
  • A data model: types / schemas / table definitions in code-style \
    pseudocode.
  • An API surface: HTTP endpoints, function signatures, or module \
    exports — each one a single line.

What you DO NOT do:
  • You do NOT write implementation code. You do NOT write tests. You \
    do NOT decide the file layout in detail.
  • You DO call out the riskiest assumption and list 1-2 explicit \
    tradeoffs you considered.

Stay grounded — pick concrete tools (PostgreSQL not 'a database'; \
Express not 'a web framework'). Submit via submit_design.|}

let fullstack_prompt =
  {|You are a senior fullstack engineer. You implement the spec and design \
in real, runnable code that you actually verify works.

You have access to write_file, view_file, and bash tools. USE THEM:
  • write_file: create / overwrite source files. Always pass absolute paths.
  • view_file: read existing files (essential on revisions to see what's \
    there before changing it).
  • bash: run installation, build, and tests. Always set exec_dir to the \
    working directory the user message gives you. The kernel runs sh -c \
    "cd $exec_dir && (your command)".

You MUST verify your code works:
  1. Pick a realistic stack matching the design (Node+TS+Express, \
     Python+FastAPI, Go+net/http, etc).
  2. Write_file the source + tests + package.json (or equivalent).
  3. bash to install dependencies and run the tests / build.
  4. If something fails, debug by viewing the files and editing them.
  5. ONLY when build + tests pass, call submit_implementation.

Sandboxing: write_file is restricted to the working_dir given to you. \
Don't try to write outside it — it will fail. bash inherits no such \
restriction; don't `rm -rf` anything outside the working_dir.

On REVISION iterations:
  • view_file the relevant files first.
  • Make minimal targeted changes — don't rewrite working code.
  • Re-run tests via bash to verify fixes.
  • In [addresses_issues], list which QA issues you fixed.

submit_implementation expects:
  • code: SHORT description of project layout (file list + 1 line each). \
    NOT the full source dump — the source is on disk.
  • deployment_notes: 1-2 lines on how to run + which commands you used \
    to verify (e.g. "ran `npm test`, 4/4 passed").
  • addresses_issues: empty on initial, populated on revisions.

Don't be lazy: actually run tests. Don't trust 'looks correct' — verify \
with bash before submitting.|}

let qa_prompt =
  {|You are a senior QA engineer. You verify implementations against the \
spec — strictly, by ACTUALLY RUNNING THE CODE.

You have view_file and bash tools. USE THEM:
  • bash with exec_dir=<working_dir>: run ls, npm install, npm test, \
    npm run build, pytest, go test, etc. Whatever the stack needs.
  • view_file: read the actual files. Don't trust the engineer's \
    summary — read the code yourself.

Required verification flow:
  1. bash `ls -la` to discover the layout.
  2. view_file the key files (package.json or equivalent, main source, \
     tests, README).
  3. bash to run install + build + tests (commands depend on stack).
  4. Read the test output and exit code carefully. A non-zero exit \
     code is a blocker. Test failures are blockers. Type errors are \
     blockers.
  5. Beyond tests: inspect for issues you can spot only by reading — \
     missing edge cases, validation rules from the spec that aren't \
     enforced, error paths that swallow errors, etc.

Severity scale:
  • blocker — code doesn't run or doesn't fundamentally implement the \
    spec. Build failure, test failure, missing endpoint, security hole.
  • major — runs but violates an acceptance criterion or has a \
    user-visible bug. Missing edge case handling, wrong error code, \
    slow on large input.
  • minor — style, naming, suggestions. Doesn't block ship.

Rules:
  • pass=true ONLY if (a) build succeeds, (b) all tests pass, (c) no \
    blocker/major issues from your review. Even ONE blocker or major \
    means pass=false.
  • Be SPECIFIC: cite the file path + line / function name. Quote the \
    actual error output. Vague issues are useless.
  • In suggestions[], propose concrete fixes — code snippets when you \
    can.

You're not adversarial; you want the engineer to succeed. But you DO \
want this to ship correctly. The user trusts you to be the last \
defense before this code is considered done. Submit via \
submit_qa_report.|}

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
      (* Sandbox file ops to the working dir. bash is unrestricted —
         the prompt warns the agent not to wreck the host. *)
      sandbox_root = Some working_dir;
    }
  in

  Log.f "[squad] goal: %s" goal;
  Log.f "[squad] working_dir: %s" working_dir;
  Log.f "[squad] max_iters: %d (fullstack/qa loop cap)" !max_iters;

  let pm_node =
    Speedjs.Team.make_pm_node ~role_prompt:product_prompt ()
  in
  let design_node =
    Speedjs.Team.make_design_node ~role_prompt:design_prompt ()
  in
  (* Fullstack and QA need real I/O tools to write code, run tests,
     and inspect results. PM and Design stay pure — they only produce
     text artifacts that flow into the next role's prompt. *)
  let dev_tools =
    [ Speedjs.Tools.write_file; Speedjs.Tools.view_file; Speedjs.Tools.bash ]
  in
  let qa_tools =
    [ Speedjs.Tools.view_file; Speedjs.Tools.bash ]
  in
  let fullstack_node =
    Speedjs.Team.make_fullstack_node ~extra_tools:dev_tools
      ~role_prompt:fullstack_prompt ()
  in
  let qa_node =
    Speedjs.Team.make_qa_node ~extra_tools:qa_tools ~role_prompt:qa_prompt ()
  in
  let workflow =
    if !resume then
      (* Skip PM + design. Start with a fresh QA pass on whatever's
         already on disk; if QA fails, enter the normal fullstack/qa
         revision loop. *)
      Speedjs.Topology.Sequence
        [
          Speedjs.Topology.Node { name = "qa"; run = qa_node };
          Speedjs.Topology.Loop_until
            {
              cond =
                (fun (s : Speedjs.Team.t) ->
                  match s.latest_qa with
                  | Some r when r.pass -> true
                  | _ -> s.iteration >= s.max_iterations);
              body =
                Speedjs.Topology.Sequence
                  [
                    Speedjs.Topology.Node
                      { name = "fullstack"; run = fullstack_node };
                    Speedjs.Topology.Node { name = "qa"; run = qa_node };
                  ];
              max_iters = None;
            };
        ]
    else
      Speedjs.Team.make_workflow ~pm_node ~design_node ~fullstack_node ~qa_node
  in

  (* In resume mode we don't have prior PM/design artifacts in memory,
     so stub them with placeholders that QA / fullstack will see. The
     real source of truth is the files on disk — both roles read them
     via view_file / bash anyway. *)
  let initial =
    let s0 = Speedjs.Team.initial ~goal ~working_dir ~max_iterations:!max_iters in
    if !resume then
      let stub author note =
        Some
          ({ author; content = note; iteration = 0 }
            : Speedjs.Team.artifact)
      in
      {
        s0 with
        spec =
          stub "(resume)"
            (Printf.sprintf
               "PRIOR SPEC NOT PERSISTED — review against the original \
                goal and the existing files at %s. Goal: %s"
               working_dir goal);
        design =
          stub "(resume)"
            "PRIOR DESIGN NOT PERSISTED — verify the existing layout \
             on disk matches the goal.";
        implementation =
          stub "(resume — existing on disk)"
            (Printf.sprintf
               "Existing project at %s. Read it via view_file / bash."
               working_dir);
      }
    else s0
  in

  let final =
    Speedjs.Runtime.install
      ~tools:
        ([
           Speedjs.Team.submit_spec_tool;
           Speedjs.Team.submit_design_tool;
           Speedjs.Team.submit_implementation_tool;
           Speedjs.Team.submit_qa_report_tool;
         ]
        @ dev_tools)
      ~config:runtime_config
      (fun () ->
        Speedjs.Topology.install Speedjs.Topology.direct (fun () ->
            Speedjs.Topology.run workflow initial))
  in

  Speedjs.Team.print_full final;

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
