(** CLI argument parsing for the [speedjs] executable.

    Pure data — no side effects beyond [usage ()] which prints to stderr
    and exits. Logging, protection wiring, and orchestration live in
    [main.ml].

    Defaults (sensible safety net):
    - tool output truncation @ 12K chars  : ON (always)
    - 3-tier prompt cache + sliding window: ON (always)
    - walltime budget 1800s (30 min)      : ON (override with --walltime)
    - loop guard (5x same call / 3x error): ON (disable with --no-loop-guard)
    - USD budget                          : OFF (opt-in via --budget)
    - streaming text to stderr            : ON (disable with SPEEDJS_NO_STREAM)
    - tape checkpointing                  : OFF (opt-in via --tape)

    Options:
    - --plan               Plan-Act mode: planner → per-task executor → summary.
    - --no-summarizer      With --plan, skip the synthesis LLM call.
    - --tape <path>        Enable JSONL tape checkpointing.
    - --crash-after <N>    Simulate crash after N live LLM calls (demo).
    - --budget <usd>       USD cost cap (Governor max_cost_usd).
    - --walltime <sec>     Wall-clock budget. 0 disables. Default 1800.
    - --no-loop-guard      Disable LoopGuard.
    - --max-iters <N>      Per-agent iteration cap. Default 100.
    - --skills-dir <path>  Load skills from <path>/<name>/SKILL.md files.
    - --log-file <path>    Send all logs to <path> instead of stderr.
    - --max-retries <N>    LLM call retry attempts. Default 3.
    - --session <path>     Multi-turn chat session with ask_user pause-tool.
    - --mcp <command>      Spawn an MCP server (repeatable).
    - --working-dir <path> Project directory for WorkspaceSurveyor + tools.
    - --memory-dir <path>  Persistent agent memory directory.
    - --debug-request      Dump JSON request body before each LLM call.

    Env vars:
    - ANTHROPIC_API_KEY           required (unless going through a gateway)
    - AGENT_LLM_BASE_URL          default: https://api.anthropic.com
    - AGENT_LLM_PROXY             default: unset (no proxy)
    - ANTHROPIC_CUSTOM_HEADERS    "Key: Val\nKey: Val"
    - SPEEDJS_MODEL               default: claude-sonnet-4-5-20250929
    - SPEEDJS_QUIET               silence iter logs
    - SPEEDJS_NO_STREAM           disable streaming text output *)

type t = {
  query : string;
  tape : string option;
  crash_after : int option;
  budget : float option;
  walltime : float;
  max_steps : int option;
  max_tool_calls : int option;
  max_subagent_depth : int option;
  max_repeated_tool_calls : int option;
  loop_guard : bool;
  debug_request : bool;
  plan : bool;
  skip_summarizer : bool;
  max_retries : int;
  max_iters : int;
  session : string option;
  mcp_servers : string list;
  skills_dir : string option;
  log_file : string option;
  working_dir : string option;
  memory_dir : string option;
  sandbox_root : string option;
      (** When set, all File_* effects are restricted to this prefix. *)
  restart : bool;
      (** Force [Plan_act.run] to ignore plan_state.json and replan. *)
  trace_file : string option;
      (** When [Some path], every LLM call and tool dispatch emits one
          NDJSON frame to [path] for offline analysis. Off by default. *)
  chaos_seed : int;
      (** RNG seed for [Chaos] middleware (failure injection). *)
  chaos_llm : float;
      (** [0.0–1.0] probability of injecting an LLM API failure per
          call. 0.0 = chaos middleware is no-op. *)
  chaos_tool : float;
      (** [0.0–1.0] probability of injecting a tool failure per
          dispatch. *)
  planner_model : string option;
      (** Override model for [Planner.plan]. None = inherit
          [SPEEDJS_MODEL] / default. *)
  executor_model : string option;
      (** Override model for per-task ReAct loop (~93% of tokens).
          Cheaper model = lower cost, higher failure-rate risk. *)
  recovery_model : string option;
      (** Override model for [Planner.recover]. *)
  summarizer_model : string option;
      (** Override model for the final synthesizer call. *)
}

let default_walltime = 1800.0

let usage () =
  prerr_endline "usage: speedjs [options] \"<your question>\"";
  prerr_endline
    "  --plan  --no-summarizer  --max-iters N  --max-retries N";
  prerr_endline
    "  --tape PATH  --crash-after N  --budget USD";
  prerr_endline
    "  --walltime SEC (0=off, default 1800)  --no-loop-guard";
  prerr_endline
    "  --skills-dir PATH  --working-dir PATH  --memory-dir PATH";
  prerr_endline
    "  --sandbox PATH (UX guardrail — textual prefix check only;";
  prerr_endline
    "                  does NOT follow symlinks, NOT a security boundary)";
  prerr_endline
    "  --restart (ignore saved plan_state.json and replan from scratch)";
  prerr_endline
    "  --trace-file PATH (NDJSON frame log for every LLM call + tool dispatch)";
  prerr_endline
    "  --chaos-llm RATE  --chaos-tool RATE  --chaos-seed N";
  prerr_endline
    "    (probabilistic failure injection — exercises retry / recovery";
  prerr_endline
    "     paths; 0.0 = off. Same seed reproduces the same failure sequence.)";
  prerr_endline
    "  --planner-model NAME  --executor-model NAME";
  prerr_endline
    "  --recovery-model NAME  --summarizer-model NAME";
  prerr_endline
    "    (per-role model override; falls back to SPEEDJS_MODEL.";
  prerr_endline
    "     Use to mix Opus planner + Sonnet executor on one stack.)";
  prerr_endline
    "  --log-file PATH  --session PATH  --mcp \"cmd args\" (repeatable)";
  prerr_endline "  --debug-request";
  exit 2

(** [need_arg flag i argv] returns [argv.(!i + 1)] and increments [i],
    or calls [usage] if the flag is at the end. Centralizes the
    "missing arg after flag" error. *)
let need_arg ~flag ~i ~argv =
  let n = Array.length argv in
  if !i + 1 >= n then begin
    Printf.eprintf "error: %s requires an argument\n" flag;
    usage ()
  end;
  let v = argv.(!i + 1) in
  incr i;
  v

let parse argv : t =
  let query = ref None in
  let tape = ref None in
  let crash_after = ref None in
  let budget = ref None in
  let walltime = ref default_walltime in
  let loop_guard = ref true in
  let debug_request = ref false in
  let plan = ref false in
  let skip_summarizer = ref false in
  let max_retries = ref 3 in
  let max_iters = ref Speedjs.Agent.default_max_iterations in
  let session = ref None in
  let mcp_servers = ref [] in
  let skills_dir = ref None in
  let log_file = ref None in
  let working_dir = ref None in
  let memory_dir = ref None in
  let sandbox_root = ref None in
  let restart = ref false in
  let trace_file = ref None in
  let max_steps = ref None in
  let max_tool_calls = ref None in
  let max_subagent_depth = ref None in
  let max_repeated_tool_calls = ref None in
  let chaos_seed = ref 42 in
  let chaos_llm = ref 0.0 in
  let chaos_tool = ref 0.0 in
  let planner_model = ref None in
  let executor_model = ref None in
  let recovery_model = ref None in
  let summarizer_model = ref None in
  let n = Array.length argv in
  let i = ref 1 in
  while !i < n do
    let a = argv.(!i) in
    let arg flag = need_arg ~flag ~i ~argv in
    (match a with
    | "--tape" -> tape := Some (arg "--tape")
    | "--crash-after" ->
        crash_after := Some (int_of_string (arg "--crash-after"))
    | "--budget" -> budget := Some (float_of_string (arg "--budget"))
    | "--walltime" -> walltime := float_of_string (arg "--walltime")
    | "--no-loop-guard" -> loop_guard := false
    | "--debug-request" -> debug_request := true
    | "--plan" -> plan := true
    | "--no-summarizer" -> skip_summarizer := true
    | "--max-retries" -> max_retries := int_of_string (arg "--max-retries")
    | "--max-iters" -> max_iters := int_of_string (arg "--max-iters")
    | "--session" -> session := Some (arg "--session")
    | "--mcp" -> mcp_servers := arg "--mcp" :: !mcp_servers
    | "--skills-dir" -> skills_dir := Some (arg "--skills-dir")
    | "--log-file" -> log_file := Some (arg "--log-file")
    | "--working-dir" -> working_dir := Some (arg "--working-dir")
    | "--memory-dir" -> memory_dir := Some (arg "--memory-dir")
    | "--sandbox" -> sandbox_root := Some (arg "--sandbox")
    | "--restart" -> restart := true
    | "--trace-file" -> trace_file := Some (arg "--trace-file")
    | "--max-steps" -> max_steps := Some (int_of_string (arg "--max-steps"))
    | "--max-tool-calls" ->
        max_tool_calls := Some (int_of_string (arg "--max-tool-calls"))
    | "--max-subagent-depth" ->
        max_subagent_depth :=
          Some (int_of_string (arg "--max-subagent-depth"))
    | "--max-repeated-tool-calls" ->
        max_repeated_tool_calls :=
          Some (int_of_string (arg "--max-repeated-tool-calls"))
    | "--chaos-seed" -> chaos_seed := int_of_string (arg "--chaos-seed")
    | "--chaos-llm" -> chaos_llm := float_of_string (arg "--chaos-llm")
    | "--chaos-tool" -> chaos_tool := float_of_string (arg "--chaos-tool")
    | "--planner-model" -> planner_model := Some (arg "--planner-model")
    | "--executor-model" -> executor_model := Some (arg "--executor-model")
    | "--recovery-model" -> recovery_model := Some (arg "--recovery-model")
    | "--summarizer-model" ->
        summarizer_model := Some (arg "--summarizer-model")
    | "-h" | "--help" -> usage ()
    | s when not (String.starts_with ~prefix:"--" s) -> query := Some s
    | other ->
        Printf.eprintf "unknown option: %s\n" other;
        usage ());
    incr i
  done;
  match !query with
  | None -> usage ()
  | Some q ->
      {
        query = q;
        tape = !tape;
        crash_after = !crash_after;
        budget = !budget;
        walltime = !walltime;
        loop_guard = !loop_guard;
        debug_request = !debug_request;
        plan = !plan;
        skip_summarizer = !skip_summarizer;
        max_retries = !max_retries;
        max_iters = !max_iters;
        session = !session;
        mcp_servers = List.rev !mcp_servers;
        skills_dir = !skills_dir;
        log_file = !log_file;
        working_dir = !working_dir;
        memory_dir = !memory_dir;
        sandbox_root = !sandbox_root;
        restart = !restart;
        trace_file = !trace_file;
        max_steps = !max_steps;
        max_tool_calls = !max_tool_calls;
        max_subagent_depth = !max_subagent_depth;
        max_repeated_tool_calls = !max_repeated_tool_calls;
        chaos_seed = !chaos_seed;
        chaos_llm = !chaos_llm;
        chaos_tool = !chaos_tool;
        planner_model = !planner_model;
        executor_model = !executor_model;
        recovery_model = !recovery_model;
        summarizer_model = !summarizer_model;
      }

(** Build the "[active] ..." status line as a list of flag descriptions
    currently in effect. Caller does the actual logging. *)
let active_flags (args : t) : string list =
  [
    Some (if args.plan then "mode=plan-act" else "mode=react");
    Some "tool-truncation(12K)";
    Some "prompt-cache(3-tier)";
    (if args.plan then Some "executor-sliding-window(60→30)" else None);
    (if args.walltime > 0.0 then
       Some (Printf.sprintf "walltime(%.0fs)" args.walltime)
     else None);
    (match args.budget with
    | Some b -> Some (Printf.sprintf "budget($%.4f)" b)
    | None -> None);
    (if args.loop_guard then Some "loop-guard(5/3)" else None);
    (if args.tape <> None then Some "checkpoint" else None);
    (match args.skills_dir with Some _ -> Some "skills" | None -> None);
    (match args.working_dir with
    | Some _ -> Some "workspace-surveyor"
    | None -> None);
    (match args.memory_dir with
    | Some _ -> Some "memory-persist"
    | None -> None);
    (match args.sandbox_root with
    | Some r -> Some (Printf.sprintf "sandbox(%s)" r)
    | None -> None);
    (if args.restart then Some "restart" else None);
    (match args.trace_file with
    | Some _ -> Some "trace"
    | None -> None);
    (if args.chaos_llm > 0.0 || args.chaos_tool > 0.0 then
       Some
         (Printf.sprintf "chaos(seed=%d, llm=%.2f, tool=%.2f)"
            args.chaos_seed args.chaos_llm args.chaos_tool)
     else None);
    (let parts =
       List.filter_map
         (fun (label, m) ->
           match m with
           | Some name -> Some (Printf.sprintf "%s=%s" label name)
           | None -> None)
         [
           ("plan", args.planner_model);
           ("exec", args.executor_model);
           ("rec", args.recovery_model);
           ("sum", args.summarizer_model);
         ]
     in
     if parts = [] then None
     else Some (Printf.sprintf "model-override(%s)" (String.concat "," parts)));
  ]
  |> List.filter_map Fun.id
