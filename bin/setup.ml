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
    Sub-agent tools = built-ins + MCP + skill tool + serial delegate
    (NO parallel_delegate, prevents unbounded fan-out).
    Parent tools = sub-agent tools + parallel_delegate. *)
let build_tools ~mcp_tools ~skill_tools
    ~(runtime_config : Speedjs.Runtime.config) :
    Speedjs.Types.tool_def list * Speedjs.Types.tool_def list =
  let base = Speedjs.Tools.all @ mcp_tools @ skill_tools in
  let serial_delegate =
    Speedjs.Sub_agent.make_delegate_tool ~tools_for_subagent:base
  in
  let subagent_tools = base @ [ serial_delegate ] in
  let build_child_stack ~prefix ~child_cost thunk =
    let child_config : Speedjs.Runtime.config =
      {
        runtime_config with
        cost = child_cost;
        on_log =
          (fun line ->
            runtime_config.on_log (Printf.sprintf "[%s] %s" prefix line));
        on_text_delta = (fun _ -> ());
        tape_path = None;
        crash_after = None;
      }
    in
    Speedjs.Runtime.install ~tools:subagent_tools ~config:child_config thunk
  in
  let parallel_delegate =
    Speedjs.Parallel_subagent.make_delegate_tool
      ~tools_for_subagent:subagent_tools ~build_child_stack
      ~parent_cost:runtime_config.cost
  in
  (subagent_tools, subagent_tools @ [ parallel_delegate ])

(** Build the list of system-prompt blocks contributed by enabled
    extensions. Currently just the skill index (when skills are
    loaded). Each entry is [(name, body)] and renders as
    [<name>body</name>] in the executor / agent's system prompt. *)
let build_system_blocks ~skill_index =
  if skill_index = "" then []
  else [ ("available_skills", skill_index) ]

(** Map CLI [Args.t] to [Runtime.config] — the only CLI-specific bridge
    left in setup.ml. All actual chain composition lives in
    [Speedjs.Runtime] so library callers reuse the same wiring. *)
let runtime_config_of_args (args : Args.t) ~model ~cost ~on_log
    ~on_text_delta : Speedjs.Runtime.config =
  let base = Speedjs.Governor.Limits.default in
  let pick_some override default =
    match override with Some _ -> override | None -> default
  in
  let governor_limits : Speedjs.Governor.Limits.t =
    {
      max_wall_time_sec =
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
  in
  {
    model;
    cost;
    on_log;
    on_text_delta;
    governor_limits;
    llm_max_retries = args.max_retries;
    tape_path = args.tape;
    crash_after = args.crash_after;
    emit_governor_events_to_log = Sys.getenv_opt "SPEEDJS_QUIET" = None;
    sandbox_root = args.sandbox_root;
    tracer =
      (match args.trace_file with
      | None -> Speedjs.Trace.make_noop ()
      | Some path -> Speedjs.Trace.make_file_writer path);
  }

(** Compose runtime + run thunk. Thin wrapper around
    [Speedjs.Runtime.install]. *)
let make_runtime (args : Args.t) ~tools ~model ~cost ~on_log ~on_text_delta
    : (unit -> 'a) -> 'a =
 fun thunk ->
  let config =
    runtime_config_of_args args ~model ~cost ~on_log ~on_text_delta
  in
  Speedjs.Runtime.install ~tools ~config thunk
