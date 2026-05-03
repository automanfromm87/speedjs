(** speedjs CLI entry point.

    Talks to api.anthropic.com using [ANTHROPIC_API_KEY] by default; set
    [AGENT_LLM_BASE_URL] / [AGENT_LLM_PROXY] to route through a gateway.
    See [args.ml] for the full flag reference. *)

let () =
  let args = Args.parse Sys.argv in
  let model =
    match Sys.getenv_opt "SPEEDJS_MODEL" with
    | Some m when m <> "" -> m
    | _ -> Speedjs.Anthropic.default_model
  in
  let quiet = Sys.getenv_opt "SPEEDJS_QUIET" <> None in
  let stream = Sys.getenv_opt "SPEEDJS_NO_STREAM" = None in

  Option.iter Log.setup_file args.log_file;

  (* Pre-run wiring: spawn MCP servers FIRST so their tools are visible
     to sub-agents (delegate captures the sub-tool list at construction
     time, not at call time). *)
  let mcp_tools = Setup.load_mcp_tools args in
  let skill_index, skill_tools = Setup.load_skills args in
  let cost = Speedjs.Handlers.new_cost_state () in

  if not quiet then
    Log.f "[active] %s" (String.concat " | " (Args.active_flags args));
  if args.debug_request then Unix.putenv "SPEEDJS_DEBUG_REQUEST" "1";

  let on_log = if quiet then fun _ -> () else Log.line in
  let on_text_delta = if stream then Log.str else fun _ -> () in
  (* Build runtime_config FIRST so parallel_delegate (constructed in
     build_tools) can close over it to spawn child stacks. *)
  let runtime_config =
    Setup.runtime_config_of_args args ~model ~cost ~on_log ~on_text_delta
  in
  let _subagent_tools, tools =
    Setup.build_tools ~mcp_tools ~skill_tools ~runtime_config
  in
  let system_blocks = Setup.build_system_blocks ~skill_index in
  let run_with_runtime thunk =
    Speedjs.Runtime.install ~config:runtime_config thunk
  in

  let exit_code =
    match args.session with
    | Some path ->
        Modes.session ~args ~path ~tools ~system_blocks ~run_with_runtime
          ~quiet ~model ()
    | None ->
        Modes.oneshot ~args ~tools ~system_blocks ~run_with_runtime ()
  in

  let summary_line =
    Printf.sprintf
      "[summary] %d LLM calls | in=%d out=%d | cache: write=%d read=%d | $%.4f"
      cost.calls cost.input_tokens cost.output_tokens
      cost.cache_creation_tokens cost.cache_read_tokens
      (Speedjs.Handlers.cost_usd cost)
  in
  Log.f "%s" summary_line;
  (* Echo summary to stderr too when --log-file is used; useful breadcrumb
     so the user sees cost without tailing the log. *)
  (match args.log_file with
  | Some _ -> Printf.eprintf "%s\n%!" summary_line
  | None -> ());
  exit exit_code
