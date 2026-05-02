(** speedjs squad — 4-agent engineering team workflow.

    Pipeline shape:
      PM → Design → Loop_until(qa.pass) { Fullstack → QA }

    Usage:
      dune exec squad -- "<goal>" [--max-iters N] [--log-file PATH] *)

let goal = ref None
let max_iters = ref 3
let log_file = ref None

let usage () =
  prerr_endline "usage: squad [options] \"<goal>\"";
  prerr_endline "  --max-iters N    fullstack/qa loop cap (default 3)";
  prerr_endline "  --log-file PATH  write logs here (default stderr)";
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
    | "-h" | "--help" -> usage ()
    | s when not (String.starts_with ~prefix:"--" s) -> goal := Some s
    | other ->
        Printf.eprintf "unknown option: %s\n" other;
        usage ());
    incr i
  done;
  match !goal with None -> usage () | Some g -> g

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
in real, runnable code.

On INITIAL iteration:
  • Implement the design completely. Real code, not pseudocode.
  • Multiple files separated by '// ===== filename =====' headers (or \
    '# ===== filename =====' for Python/etc).
  • Include all necessary imports, types, error handling.

On REVISION iterations (when QA reports issues):
  • Fix EVERY blocker and major issue. Don't skip any.
  • You can fix minors at your discretion if cheap.
  • In [addresses_issues], list (briefly) which QA issues this revision \
    fixes, mapping to the specific code change.
  • You CAN restructure if needed — don't be afraid to refactor.

Pick a realistic stack (TypeScript/Node, Python, Go, etc — match the \
design's API choices). Don't be lazy: actually wire up routes, real \
storage, real validation. Submit via submit_implementation.|}

let qa_prompt =
  {|You are a senior QA engineer. You review implementations against the \
spec — strictly.

What you check:
  • Does the code actually implement EVERY acceptance criterion?
  • Are edge cases handled (empty input, large input, concurrent \
    access, malformed data)?
  • Are errors propagated correctly and meaningfully?
  • Is the code likely to RUN (no obvious type errors, missing \
    imports, undefined variables)?
  • Are validation rules from the spec actually enforced?

Severity scale:
  • blocker — would prevent the feature from working at all (missing \
    endpoint, security hole, definitely-buggy logic).
  • major — would cause user-visible issues or violate an acceptance \
    criterion (missing edge case, bad error message, slow on large input).
  • minor — code style, naming, missing comments, suggestions for \
    improvement.

Rules:
  • pass=true ONLY if NO blocker or major issues remain. Even one \
    blocker or major means pass=false.
  • Be SPECIFIC in issue descriptions: cite the relevant function / \
    line / behavior. Vague issues are useless to the engineer.
  • In suggestions[], propose concrete fixes — sometimes literal code \
    snippets.

You're not adversarial; you want the engineer to succeed. But you DO \
want this to ship correctly. Submit via submit_qa_report.|}

(* ===== main ===== *)

let () =
  let goal = parse_args Sys.argv in
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
      sandbox_root = None;
    }
  in

  Log.f "[squad] goal: %s" goal;
  Log.f "[squad] max_iters: %d (fullstack/qa loop cap)" !max_iters;

  let pm_node =
    Speedjs.Team.make_pm_node ~role_prompt:product_prompt ()
  in
  let design_node =
    Speedjs.Team.make_design_node ~role_prompt:design_prompt ()
  in
  let fullstack_node =
    Speedjs.Team.make_fullstack_node ~role_prompt:fullstack_prompt ()
  in
  let qa_node =
    Speedjs.Team.make_qa_node ~role_prompt:qa_prompt ()
  in
  let workflow =
    Speedjs.Team.make_workflow ~pm_node ~design_node ~fullstack_node ~qa_node
  in

  let initial =
    Speedjs.Team.initial ~goal ~max_iterations:!max_iters
  in

  let final =
    Speedjs.Runtime.install
      ~tools:
        [
          Speedjs.Team.submit_spec_tool;
          Speedjs.Team.submit_design_tool;
          Speedjs.Team.submit_implementation_tool;
          Speedjs.Team.submit_qa_report_tool;
        ]
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
