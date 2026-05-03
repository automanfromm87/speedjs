(** Two execution modes: chat session (multi-turn with ask_user pause)
    vs. one-shot (plan-act or pure ReAct).

    Both take a [run_with_runtime] thunk-runner constructed by
    [Setup.make_runtime] and return the process exit code. *)

(** Multi-turn chat session: persists conversation across invocations,
    handles the [ask_user] pause-tool by suspending the run cleanly. *)
let session ~(args : Args.t) ~path ~tools
    ?(system_blocks : (string * string) list = [])
    ~run_with_runtime ~quiet ~model () : int =
  let prior =
    match Speedjs.Session.load ~on_corrupt:Log.line ~path () with
    | Some s ->
        if not quiet then
          Log.f "[session] loaded %d prior messages from %s"
            (List.length s.messages) path;
        s
    | None ->
        if not quiet then Log.f "[session] starting new session at %s" path;
        Speedjs.Session.empty ~model ()
  in
  let session_with_input = Speedjs.Session.append_input prior args.query in
  let agent_thunk () =
    let spec =
      Speedjs.Specs.chat ~system_blocks ~max_iters:args.max_iters ~tools ()
    in
    Speedjs.Agent.execute ~spec
      ~input:(Speedjs.Agent.Resume session_with_input.messages)
  in
  (* Global limits are enforced by [Governor] (installed in
     [Setup.make_runtime]); no per-call protection wrapping needed here. *)
  let outcome =
    Speedjs.Protection.catch_protection_errors_output (fun () ->
        run_with_runtime agent_thunk)
  in
  let updated =
    Speedjs.Session.update_after_output session_with_input outcome
  in
  Speedjs.Session.save ~path updated;
  print_endline "";
  match outcome with
  | Speedjs.Agent.Done { answer; _ } ->
      print_endline "================ Answer ======================";
      print_endline answer;
      print_endline "================================================";
      Log.f "[session] saved %d messages (completed)"
        (List.length updated.messages);
      0
  | Speedjs.Agent.Waiting { question; _ } ->
      print_endline "================ Waiting on you ==============";
      print_endline question;
      print_endline "================================================";
      Log.f
        "[session] saved %d messages (waiting on user). Run speedjs \
         --session %s \"<your answer>\" to continue."
        (List.length updated.messages) path;
      0
  | Speedjs.Agent.Failed { reason; _ } ->
      print_endline "================ Agent Error =================";
      print_endline (Speedjs.Types.agent_error_pp reason);
      print_endline "================================================";
      1
  | Speedjs.Agent.Terminal_tool { name; _ } ->
      print_endline "================ Agent Error =================";
      Printf.printf
        "chat spec unexpectedly terminated on tool %S — Free_text \
         spec should not have a terminal tool\n" name;
      print_endline "================================================";
      1

(** One-shot mode: plan-act flow when [--plan] is set, otherwise pure
    ReAct. Returns the process exit code. *)
let oneshot ~(args : Args.t) ~tools
    ?(system_blocks : (string * string) list = [])
    ~run_with_runtime () : int =
  let agent_thunk () =
    if args.plan then
      let config =
        {
          Speedjs.Plan_act.default_config with
          skip_summarizer = args.skip_summarizer;
          max_iterations_per_task = args.max_iters;
          working_dir = args.working_dir;
          memory_dir = args.memory_dir;
          executor_system_blocks = system_blocks;
          restart = args.restart;
        }
      in
      Speedjs.Plan_act.run ~config ~goal:args.query ~tools ()
    else
      let spec =
        Speedjs.Specs.chat ~system_blocks ~max_iters:args.max_iters ~tools
          ()
      in
      let out =
        Speedjs.Agent.execute ~spec
          ~input:(Speedjs.Agent.Fresh args.query)
      in
      Result.map fst (Speedjs.Agent.expect_done ~name:"chat" out)
  in
  (* Global limits are enforced by [Governor] (installed in
     [Setup.make_runtime]); no per-call protection wrapping needed here. *)
  let result =
    Speedjs.Protection.catch_protection_errors (fun () ->
        run_with_runtime agent_thunk)
  in
  print_endline "";
  match result with
  | Ok answer ->
      print_endline "================ Final Answer ================";
      print_endline answer;
      print_endline "================================================";
      0
  | Error err ->
      print_endline "================ Agent Error =================";
      print_endline (Speedjs.Types.agent_error_pp err);
      print_endline "================================================";
      1
