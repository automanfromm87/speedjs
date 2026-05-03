(** Planner agent — runs a [Specs.planner] spec and parses the
    resulting [submit_plan] payload.

    The spec construction (system prompt, tool surface, terminal
    protocol, mode) lives in [Specs]; this module owns the parsers
    and the orchestration around them. *)

open Types

(* ===== Plan parsing ===== *)

(** Parse the [submit_plan] input JSON into a [plan]. *)
let parse_plan ~goal (input : Yojson.Safe.t) : (plan, agent_error) Result.t =
  match input with
  | `Assoc fields ->
      let title =
        Json_decode.get_string_field_or "title" ~default:"(untitled)" fields
      in
      (match List.assoc_opt "tasks" fields with
      | Some (`List items) ->
          let parse_dep_list = function
            | `List xs ->
                List.filter_map
                  (function `Int n when n >= 1 -> Some n | _ -> None)
                  xs
            | _ -> []
          in
          let tasks =
            List.mapi
              (fun i j ->
                let description, depends_on =
                  match j with
                  | `Assoc fs ->
                      let desc =
                        Json_decode.get_string_field_or "description"
                          ~default:"" fs
                      in
                      let deps =
                        match List.assoc_opt "depends_on" fs with
                        | Some v -> parse_dep_list v
                        | None -> []
                      in
                      (desc, deps)
                  | _ -> ("", [])
                in
                { index = i + 1; description; depends_on })
              items
            |> List.filter (fun t -> t.description <> "")
          in
          if tasks = [] then
            Error (Plan_invalid "submit_plan returned empty tasks")
          else Ok { title; goal; tasks }
      | _ -> Error (Plan_invalid "submit_plan missing 'tasks' array"))
  | _ -> Error (Plan_invalid "submit_plan input is not an object")

(** Workflow form: planner spec + leaf + terminal-tool projection +
    payload parse. Replaces ~30 lines of try/catch + match ladder with
    three combinators. *)
let plan_flow ?system_prompt ?max_iterations ?model
    ?(research_tools : tool_def list = []) ~goal () : plan Workflow.t =
  let open Workflow in
  let spec =
    Specs.planner ?system_prompt ?max_iters:max_iterations ?model
      ~tools:research_tools ()
  in
  let user_query =
    Printf.sprintf
      "Goal:\n\n%s\n\nFirst, load any relevant skills from the \
       available_skills index. Then call submit_plan with the \
       structured task list."
      goal
  in
  let* (payload, _msgs) =
    leaf spec (Agent.Fresh user_query)
    |> expect_terminal_tool ~name:Specs.submit_plan_name ~label:"planner"
  in
  of_result (parse_plan ~goal payload)

let plan ?system_prompt ?max_iterations ?model
    ?(research_tools : tool_def list = []) ~goal () :
    (plan, agent_error) Result.t =
  let capture (r : (plan, agent_error) Result.t) : Trace.capture_result =
    match r with
    | Ok p ->
        Trace.ok_capture
          ~output:(Printf.sprintf "%d task(s)" (List.length p.tasks))
          ~tokens:Trace.zero_tokens ~cost_delta:0.0
    | Error e -> Trace.fail_capture ~error:(agent_error_pp e)
  in
  Trace.span_current ~kind:Trace.Phase ~name:"planner"
    ~input_summary:goal ~capture (fun () ->
      Effect.perform
        (Effects.Log
           (Printf.sprintf "[planner] decomposing goal: %s"
              (if String.length goal > 80 then String.sub goal 0 80 ^ "..."
               else goal)));
      Workflow.run
        (plan_flow ?system_prompt ?max_iterations ?model ~research_tools
           ~goal ()))

(* ===== Plan recovery =====

   When a task fails (after retries), ask the planner what to do. The
   recovery planner sees the original goal, completed tasks, the
   failed task + error, remaining pending tasks, prior recovery
   failures, and the cycle index. It returns one of four decisions: *)

type recovery_decision =
  | Replan of task list
  | Split of task list
  | Skip
  | Abandon

let parse_tasks_field fs =
  Json_decode.get_list_field_or_empty "tasks" fs
  |> List.mapi (fun i j ->
         let description, depends_on =
           match j with
           | `Assoc tfs ->
               let desc =
                 Json_decode.get_string_field_or "description" ~default:"" tfs
               in
               let deps =
                 match List.assoc_opt "depends_on" tfs with
                 | Some (`List xs) ->
                     List.filter_map
                       (function `Int n when n >= 1 -> Some n | _ -> None)
                       xs
                 | _ -> []
               in
               (desc, deps)
           | _ -> ("", [])
         in
         { index = i + 1; description; depends_on })
  |> List.filter (fun t -> t.description <> "")

let parse_recovery_decision (input : Yojson.Safe.t) :
    (recovery_decision, agent_error) Result.t =
  let open Json_decode in
  let with_tasks ctor name fs =
    let new_tasks = parse_tasks_field fs in
    if new_tasks = [] then
      Error (Printf.sprintf "%s with empty tasks list" name)
    else Ok (ctor new_tasks)
  in
  let decode fs =
    let* decision = get_string_field "decision" fs in
    match decision with
    | "abandon" -> Ok Abandon
    | "skip" -> Ok Skip
    | "replan" -> with_tasks (fun ts -> Replan ts) "replan" fs
    | "split" -> with_tasks (fun ts -> Split ts) "split" fs
    | other -> Error (Printf.sprintf "unknown recovery decision %S" other)
  in
  Result.map_error (fun msg -> Plan_invalid msg)
    (with_object_input input decode)

(** Render a budget snapshot section for the recovery prompt. Empty
    string when no progress data — keeps the prompt clean for callers
    that don't have a Governor installed. *)
let render_budget_section (p : budget_progress) : string =
  let pct cap used =
    match cap with
    | None -> "(no cap)"
    | Some c ->
        if c <= 0.0 then "(cap=0)"
        else Printf.sprintf "%.0f%% used" (used /. c *. 100.0)
  in
  let cap_or_dash = function
    | None -> "—"
    | Some c -> Printf.sprintf "%.4f" c
  in
  let cap_sec_or_dash = function
    | None -> "—"
    | Some c -> Printf.sprintf "%.0fs" c
  in
  Printf.sprintf
    "Budget progress:\n\
    \  cost:     $%.4f / %s  (%s)\n\
    \  walltime: %.0fs / %s  (%s)"
    p.cost_used_usd
    (cap_or_dash p.cost_max_usd)
    (pct p.cost_max_usd p.cost_used_usd)
    p.walltime_used_sec
    (cap_sec_or_dash p.walltime_max_sec)
    (pct p.walltime_max_sec p.walltime_used_sec)

(** Recovery agent. Like [plan] but builds [Specs.recovery] and parses
    [submit_recovery]. *)
let recover ?max_iterations ?model
    ?(research_tools : tool_def list = []) ?(prior_failures = [])
    ?(skipped = []) ?budget_progress
    ?(cycle_index = 0) ?(max_cycles = 2) ~goal ~completed ~failed_task
    ~failed_error ~remaining () :
    (recovery_decision, agent_error) Result.t =
  let capture (r : (recovery_decision, agent_error) Result.t) :
      Trace.capture_result =
    match r with
    | Ok decision ->
        let label =
          match decision with
          | Replan _ -> "REPLAN"
          | Split _ -> "SPLIT"
          | Skip -> "SKIP"
          | Abandon -> "ABANDON"
        in
        Trace.ok_capture ~output:label ~tokens:Trace.zero_tokens
          ~cost_delta:0.0
    | Error e -> Trace.fail_capture ~error:(agent_error_pp e)
  in
  Trace.span_current ~kind:Trace.Phase
    ~name:(Printf.sprintf "recovery#%d" cycle_index)
    ~input_summary:
      (Printf.sprintf "task %d: %s" failed_task.index failed_error)
    ~capture (fun () ->
      Effect.perform
        (Effects.Log
           (Printf.sprintf
              "[recovery] cycle %d/%d task %d failed (%s), consulting planner"
              cycle_index max_cycles failed_task.index failed_error));
      let prior_failures_section =
        match prior_failures with
        | [] -> "Prior recovery failures: (none — this is the first cycle)"
        | _ ->
            let lines =
              List.map
                (fun (desc, err) ->
                  Printf.sprintf "  - %s — error: %s" desc err)
                prior_failures
            in
            Printf.sprintf "Prior recovery failures (%d):\n%s"
              (List.length prior_failures) (String.concat "\n" lines)
      in
      let skipped_section =
        match skipped with
        | [] -> ""
        | _ ->
            Printf.sprintf
              "\n\nPreviously skipped tasks (%d) — their artifacts are \
               MISSING from the working dir; the current failure may be \
               cascading from one of these:\n%s"
              (List.length skipped)
              (String.concat "\n"
                 (List.map (fun t -> "  - " ^ t) skipped))
      in
      let budget_section =
        match budget_progress with
        | None -> ""
        | Some p -> "\n\n" ^ render_budget_section p
      in
      let body =
        Printf.sprintf
          "Goal: %s\n\n\
           Completed tasks (%d):\n\
           %s\n\n\
           FAILED task: %s\n\n\
           Error: %s\n\n\
           Remaining tasks (%d):\n\
           %s\n\n\
           %s%s%s\n\n\
           Recovery cycle: %d of max %d\n\n\
           First, investigate (load_skill, view_file, bash for cat/ls) — then \
           decide replan or abandon by calling submit_recovery."
          goal
          (List.length completed)
          (String.concat "\n" (List.map (fun t -> "  - " ^ t) completed))
          failed_task.description failed_error (List.length remaining)
          (String.concat "\n" (List.map (fun t -> "  - " ^ t) remaining))
          prior_failures_section skipped_section budget_section
          cycle_index max_cycles
      in
      let recovery_label =
        Printf.sprintf "recovery#%d" cycle_index
      in
      let spec =
        Specs.recovery
          ~name:recovery_label ?max_iters:max_iterations ?model
          ~tools:research_tools ()
      in
      let flow : recovery_decision Workflow.t =
        let open Workflow in
        let* (payload, _msgs) =
          leaf spec (Agent.Fresh body)
          |> expect_terminal_tool
               ~name:Specs.submit_recovery_name ~label:recovery_label
        in
        of_result (parse_recovery_decision payload)
      in
      Workflow.run flow)
