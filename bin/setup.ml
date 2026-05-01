(** Pre-run wiring: skills, MCP servers, tools, system prompt, runtime.

    Pure assembly — given parsed [Args.t], produce the values the agent
    loop needs. Logging side-effects via [Log.f] are the only impurity. *)

(** Spawn each MCP server and collect their tools. Per-server failures
    are logged and skipped — one bad server doesn't take down the run.
    Registered [at_exit] shuts down spawned subprocesses cleanly. *)
let load_mcp_tools (args : Args.t) : Speedjs.Types.tool_def list =
  let mcp_conns = ref [] in
  let tools =
    List.concat_map
      (fun spec ->
        let parts =
          String.split_on_char ' ' spec |> List.filter (fun s -> s <> "")
        in
        match parts with
        | [] -> []
        | cmd :: cmd_args -> (
            Log.f "[mcp] starting: %s" spec;
            match
              Speedjs.Mcp.connect_and_load_tools ~cmd ~args:cmd_args ()
            with
            | Ok (conn, defs) ->
                mcp_conns := conn :: !mcp_conns;
                (* Pre-runtime: Log_handler isn't installed yet, so
                   emit a plain log line. Structured [Event_log] is
                   for code running INSIDE the agent runtime. *)
                Log.f "[mcp] %s/%s loaded %d tools: %s" conn.server_name
                  conn.server_version (List.length defs)
                  (String.concat ", "
                     (List.map
                        (fun (t : Speedjs.Types.tool_def) -> t.name)
                        defs));
                defs
            | Error e ->
                Log.f "[mcp] failed: %s — %s" spec e;
                []))
      args.mcp_servers
  in
  at_exit (fun () -> List.iter Speedjs.Mcp.shutdown !mcp_conns);
  tools

(** Load skills + build the [load_skill] tool. Returns
    [(skill_index_text, skill_tool_list)]. The index is a markdown
    block to splice into the system prompt; the tool list is appended
    to the agent's tool surface. *)
let load_skills (args : Args.t) :
    string * Speedjs.Types.tool_def list =
  match args.skills_dir with
  | None -> ("", [])
  | Some dir ->
      let on_error msg = Log.f "[skills] %s" msg in
      let loaded = Speedjs.Skill.load_dir ~on_error dir in
      if loaded <> [] then
        Log.f "[skills] loaded %d from %s: %s" (List.length loaded) dir
          (String.concat ", "
             (List.map (fun (s : Speedjs.Skill.t) -> s.name) loaded));
      let tool =
        if loaded = [] then []
        else [ Speedjs.Skill.make_load_skill_tool loaded ]
      in
      (Speedjs.Skill.render_index loaded, tool)

(** Build the (subagent_tools, parent_tools) pair.
    Sub-agent tools = built-ins + MCP + skill tool (NO delegate, prevents
    infinite recursion). Parent tools = sub-agent tools + delegate. *)
let build_tools ~mcp_tools ~skill_tools :
    Speedjs.Types.tool_def list * Speedjs.Types.tool_def list =
  let subagent_tools = Speedjs.Tools.all @ mcp_tools @ skill_tools in
  let delegate =
    Speedjs.Sub_agent.make_delegate_tool ~tools_for_subagent:subagent_tools
  in
  (subagent_tools, subagent_tools @ [ delegate ])

(** Build the list of system-prompt blocks contributed by enabled
    extensions. Currently just the skill index (when skills are
    loaded). Each entry is [(name, body)] and renders as
    [<name>body</name>] in the executor / agent's system prompt. *)
let build_system_blocks ~skill_index =
  if skill_index = "" then []
  else [ ("available_skills", skill_index) ]

(** Build the LLM handler chain from CLI args.

    Reads top-to-bottom as bottom-to-top of the call stack — the chain
    is built via [|>] so the leftmost (anthropic) is the leaf and each
    subsequent middleware wraps it. The on-the-wire order at runtime is
    the REVERSE: logging → retry → cost-tracking → validation → API
    call. (i.e. logging sees the OUTSIDE; anthropic does the actual call.) *)
let build_llm_chain ~(args : Args.t) ~model ~cost ~on_log ~on_text_delta
    : Speedjs.Llm_handler.t =
  let on_retry n err delay =
    Log.f "[retry] attempt %d failed (%s); sleeping %.1fs" (n + 1)
      (Speedjs.Llm_error.kind err) delay
  in
  let policy =
    if args.max_retries > 0 then
      {
        Speedjs.Llm_handler.Retry_policy.default with
        max_attempts = args.max_retries;
      }
    else Speedjs.Llm_handler.Retry_policy.none
  in
  Speedjs.Llm_handler.anthropic ~model ~on_text_delta ()
  |> Speedjs.Llm_handler.with_validation
  |> Speedjs.Llm_handler.with_cost_tracking ~cost
  |> Speedjs.Llm_handler.with_retry ~policy ~on_retry
  |> Speedjs.Llm_handler.with_governor_ticks
  |> Speedjs.Llm_handler.with_logging ~on_log

(** Build the Tool dispatch chain.

    Same shape as the LLM chain: each middleware wraps the next. Note
    that [with_retry] only fires on tools whose [idempotent = true]
    metadata is set; non-idempotent tools (bash, write_file) are never
    retried even on transient failures.

    Tick emission lives in [Tool_handler.install] (not in this chain) so
    that parallel batches dispatched via threads don't try to perform
    effects on workers — handlers don't propagate across threads. *)
let build_tool_chain ~on_log : Speedjs.Tool_handler.t =
  Speedjs.Tool_handler.direct
  |> Speedjs.Tool_handler.with_timeout ~default_sec:60.0
  |> Speedjs.Tool_handler.with_validation
  |> Speedjs.Tool_handler.with_retry
       ~policy:Speedjs.Llm_handler.Retry_policy.default
  |> Speedjs.Tool_handler.with_circuit_breaker ~failure_threshold:5
       ~cooldown:60.0
  |> Speedjs.Tool_handler.with_logging ~on_log

(** Build the Log handler chain. *)
let build_log_chain ~on_log : Speedjs.Log_handler.t =
  Speedjs.Log_handler.to_function on_log

(** Build [Governor.Limits] from CLI args.

    Existing CLI flags map onto governor caps:
    - [--walltime] → [max_wall_time_sec]
    - [--budget]   → [max_cost_usd]
    Other caps use governor defaults; we may add CLI flags later. *)
let build_governor_limits ~(args : Args.t) : Speedjs.Governor.Limits.t =
  let base = Speedjs.Governor.Limits.default in
  let pick_some override default = match override with Some _ -> override | None -> default in
  {
    Speedjs.Governor.Limits.max_wall_time_sec =
      (if args.walltime > 0.0 then Some args.walltime
       else base.max_wall_time_sec);
    max_cost_usd = pick_some args.budget base.max_cost_usd;
    max_steps = pick_some args.max_steps base.max_steps;
    max_tool_calls = pick_some args.max_tool_calls base.max_tool_calls;
    max_subagent_depth =
      pick_some args.max_subagent_depth base.max_subagent_depth;
    max_repeated_tool_calls =
      pick_some args.max_repeated_tool_calls base.max_repeated_tool_calls;
  }

(** Compose the runtime as a stack of effect handlers:

    OUTERMOST       Governor (global limits + death-loop detection)
                    Log_handler
                    Tool_handler / Llm_handler chain installs
                    + Checkpoint tape (when --tape)
    INNERMOST       agent thunk

    The Governor sees [Tick] events emitted by the LLM and Tool chains'
    [with_governor_ticks] middleware, plus [Subagent_entered/Exited]
    from [Sub_agent.delegate]. *)
let make_runtime (args : Args.t) ~tools ~model ~cost ~on_log ~on_text_delta
    : (unit -> 'a) -> 'a =
 fun thunk ->
  let llm_chain =
    build_llm_chain ~args ~model ~cost ~on_log ~on_text_delta
  in
  let tool_chain = build_tool_chain ~on_log in
  let limits = build_governor_limits ~args in
  let governed thunk =
    Speedjs.Governor.install ~limits ~cost
      ~on_tick:(fun ev ->
        if not (Sys.getenv_opt "SPEEDJS_QUIET" <> None) then
          on_log
            (Printf.sprintf "[gov] %s"
               (Speedjs.Governor.Event.to_string ev)))
      thunk
  in
  match args.tape with
  | None ->
      governed (fun () ->
          Speedjs.Llm_handler.install llm_chain (fun () ->
              Speedjs.Tool_handler.install ~tools tool_chain (fun () ->
                  Speedjs.Log_handler.install (build_log_chain ~on_log)
                    thunk)))
  | Some path ->
      let session =
        Speedjs.Checkpoint.open_session ~path ~on_log
          ?crash_after_live_llm:args.crash_after ()
      in
      at_exit (fun () -> Speedjs.Checkpoint.close_session session);
      let llm_with_tape =
        llm_chain |> Speedjs.Checkpoint.with_tape_llm session
      in
      governed (fun () ->
          Speedjs.Llm_handler.install llm_with_tape (fun () ->
              Speedjs.Checkpoint.install_tools_with_tape session ~tools
                tool_chain (fun () ->
                  Speedjs.Log_handler.install (build_log_chain ~on_log)
                    thunk)))
