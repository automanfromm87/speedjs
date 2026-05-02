(** speedjs debate — two-agent system-design debate.

    Usage:
      dune exec debate -- "<topic>" [--max-rounds N] [--log-file PATH]

    Two pre-configured agents argue:
      - "bash_expert"  — argues from Unix / shell heritage perspective
      - "pl_designer"  — argues from formal language / type-system perspective *)

let topic = ref None
let max_rounds = ref 6
let log_file = ref None

let usage () =
  prerr_endline "usage: debate [options] \"<topic>\"";
  prerr_endline "  --max-rounds N    cap on debate length (default 6)";
  prerr_endline "  --log-file PATH   write logs here (default stderr)";
  exit 2

let parse_args argv =
  let n = Array.length argv in
  let i = ref 1 in
  while !i < n do
    let a = argv.(!i) in
    (match a with
    | "--max-rounds" ->
        if !i + 1 >= n then usage ();
        max_rounds := int_of_string argv.(!i + 1);
        incr i
    | "--log-file" ->
        if !i + 1 >= n then usage ();
        log_file := Some argv.(!i + 1);
        incr i
    | "-h" | "--help" -> usage ()
    | s when not (String.starts_with ~prefix:"--" s) -> topic := Some s
    | other ->
        Printf.eprintf "unknown option: %s\n" other;
        usage ());
    incr i
  done;
  match !topic with None -> usage () | Some t -> t

(* ===== role prompts ===== *)

let bash_expert_prompt =
  {|You are a senior Unix / shell tooling expert. You've spent 20+ years \
writing bash, zsh, awk, sed, make, and the tooling glue that holds Unix \
together.

You argue from these convictions:
  • Pipelines and stdin/stdout are the most powerful composition primitive ever invented.
  • Plain text streams trump structured data — `grep | awk | sort | uniq` beats any DSL.
  • Tools should do one thing well. Composability via process boundaries.
  • POSIX compatibility matters; new shells that break it (fish, ion) are toys.
  • Errors as exit codes + stderr; structured exceptions are over-engineering.
  • Globbing, expansion, quoting are features, not bugs — they're a programming model.
  • Most "bash pain points" are skill issues, not tool issues.

When you debate:
  • Cite concrete shell idioms and patterns (xargs -P, command substitution, here-docs).
  • Push back on abstractions that hide what the kernel actually does.
  • Argue that DSLs sacrifice the universal Unix interface (pipes, fds, signals).
  • Be willing to concede SPECIFIC weaknesses (quoting hell, error handling) but defend the model.|}

let pl_designer_prompt =
  {|You are a senior programming-language designer. Your background is type \
theory, language semantics, and the design of clean, composable languages \
(ML family, Rust, Elm, modern systems langs).

You argue from these convictions:
  • Bash is a hostile environment: dynamic scoping, string-based interfaces, \
    silent error swallowing, fragile quoting. It's an accident of history, not a design.
  • A modern shell DSL should have: real types (at least string vs list vs file), \
    structured error propagation (Result-like), explicit data flow, lexical scope, \
    first-class functions with closures.
  • Composition via TYPED pipelines (PowerShell, Nushell-style) is strictly better \
    than untyped text streams — fewer parsing errors, fewer awk one-liners.
  • A DSL doesn't have to abandon Unix interop — it can compile DOWN to subprocess \
    calls while presenting a sane surface.
  • POSIX compat is a poor goalpost; the goal should be ergonomic correctness, \
    with FFI/escape hatches for legacy needs.

When you debate:
  • Cite concrete language-design patterns (algebraic data types, effects, monadic IO).
  • Point to existing successes: Nushell, Oil, fish — and to failures of bash at scale.
  • Acknowledge bash's genuine strengths (ubiquity, REPL discoverability) but argue they're \
    orthogonal to the language design question.
  • Be willing to concede that a DSL needs ESCAPE HATCHES for raw shell, but maintain that \
    typed structure is the right default.|}

(* ===== main ===== *)

let () =
  let topic = parse_args Sys.argv in
  let model =
    match Sys.getenv_opt "SPEEDJS_MODEL" with
    | Some m when m <> "" -> m
    | _ -> Speedjs.Anthropic.default_model
  in

  Option.iter Log.setup_file !log_file;

  let cost = Speedjs.Handlers.new_cost_state () in
  let on_log = Log.line in
  let on_text_delta _ = () in (* mute streaming during debate *)
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

  Log.f "[debate] topic: %s" topic;
  Log.f "[debate] max_rounds: %d" !max_rounds;

  let bash_node =
    Speedjs.Debate.make_debater_node ~name:"bash_expert"
      ~role_prompt:bash_expert_prompt ()
  in
  let pl_node =
    Speedjs.Debate.make_debater_node ~name:"pl_designer"
      ~role_prompt:pl_designer_prompt ()
  in
  let architect_prompt =
    {|You are a senior systems architect. You've sat in on a debate \
between a Unix-shell expert and a programming-language designer. Both \
made valid points. Your job: NOT to declare a winner, but to produce a \
concrete forward-looking design that takes the strongest valid points \
from BOTH sides and makes deliberate tradeoffs where they disagreed.

Be specific. Cite actual mechanisms (types, fds, escape hatches) rather \
than abstract principles. When you make a choice, say briefly WHY (in \
one sentence). Acknowledge what you deliberately left open. Length: \
3-6 paragraphs in the proposal field, plus 3-6 key_decisions and \
2-4 open_questions.|}
  in
  let synth_node =
    Speedjs.Debate.make_synthesizer_node ~name:"architect"
      ~architect_prompt ()
  in

  let topology =
    Speedjs.Debate.make_workflow ~name_a:"bash_expert" ~node_a:bash_node
      ~name_b:"pl_designer" ~node_b:pl_node
      ~synthesizer_name:"architect" ~synthesizer_node:synth_node
  in
  (* CLI exposes "max_rounds" as conversational rounds (one round = both
     speakers). Internally Debate counts turns. *)
  let initial =
    Speedjs.Debate.initial ~topic ~max_turns:(!max_rounds * 2)
  in

  let final =
    Speedjs.Runtime.install
      ~tools:
        [
          Speedjs.Debate.submit_argument_tool;
          Speedjs.Debate.submit_design_tool;
        ]
      ~config:runtime_config
      (fun () ->
        Speedjs.Topology.install Speedjs.Topology.direct (fun () ->
            Speedjs.Topology.run topology initial))
  in

  Speedjs.Debate.print_transcript final;
  Speedjs.Debate.print_design final;

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
