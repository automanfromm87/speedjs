(** Multi-agent engineering team workflow built on [Topology].

    Pipeline shape:

      PM → Design → Loop_until(qa.pass) { Fullstack → QA }

    Each role is its own ReAct-loop sub-agent with a role-specific
    system prompt and a structured terminal tool. State threads the
    artifacts (spec, design, implementation, qa_report) so each
    downstream role sees what came before.

    The fullstack role behaves differently across loop iterations:
    iter 0 builds from spec+design; iter 1+ revises based on
    [latest_qa.issues]. The QA role is strict — pass=true only if
    no blocker/major issues remain. *)

open Types

(* ===== Types ===== *)

type artifact = {
  author : string;
  content : string;
  iteration : int;
}

type qa_issue = {
  severity : string;     (* "blocker" / "major" / "minor" *)
  description : string;
}

type qa_report = {
  pass : bool;
  issues : qa_issue list;
  suggestions : string list;
}

type t = {
  goal : string;
  spec : artifact option;
  design : artifact option;
  implementation : artifact option;
  latest_qa : qa_report option;
  iteration : int;
      (** fullstack/qa loop iteration counter, starts at 0; bumped at
          end of each fullstack run. *)
  max_iterations : int;
  history : (artifact * qa_report option) list;
      (** Past (implementation, qa_report) pairs in chronological order.
          Useful for transcript / debugging. *)
}

let initial ~goal ~max_iterations =
  {
    goal;
    spec = None;
    design = None;
    implementation = None;
    latest_qa = None;
    iteration = 0;
    max_iterations;
    history = [];
  }

(* ===== Terminal tools (one per role) ===== *)

(* Common helper to declare a terminal tool — handler errors out, the
   agent loop short-circuits via Task_terminal_called. *)
let make_terminal_tool ~name ~description ~input_schema : tool_def =
  {
    idempotent = true;
    timeout_sec = None;
    category = "meta";
    name;
    description;
    input_schema;
    handler =
      (fun _ ->
        Error
          (Printf.sprintf
             "%s is a terminal tool — its handler should never be invoked"
             name));
  }

let submit_spec_name = "submit_spec"

let submit_spec_tool : tool_def =
  make_terminal_tool ~name:submit_spec_name
    ~description:
      "Submit the product spec. Translate the user's goal into a \
       concrete specification with user stories and acceptance \
       criteria. Don't design or implement — just describe WHAT and \
       WHY."
    ~input_schema:
      (`Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "spec",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Multi-paragraph product specification: \
                           problem, target users, scope, non-goals." );
                    ] );
                ( "user_stories",
                  `Assoc
                    [
                      ("type", `String "array");
                      ("items", `Assoc [ ("type", `String "string") ]);
                      ( "description",
                        `String
                          "3-7 user stories in 'As X I want Y so that Z' \
                           form." );
                    ] );
                ( "acceptance_criteria",
                  `Assoc
                    [
                      ("type", `String "array");
                      ("items", `Assoc [ ("type", `String "string") ]);
                      ( "description",
                        `String
                          "5-10 testable acceptance criteria — one line \
                           each, concrete and verifiable." );
                    ] );
              ] );
          ( "required",
            `List
              [
                `String "spec";
                `String "user_stories";
                `String "acceptance_criteria";
              ] );
        ])

let submit_design_name = "submit_design"

let submit_design_tool : tool_def =
  make_terminal_tool ~name:submit_design_name
    ~description:
      "Submit the technical design. Translate the spec into an \
       architecture: components, data model, API surface, key \
       tradeoffs. Don't write code — show signatures and shape."
    ~input_schema:
      (`Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "design",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Multi-paragraph technical design: \
                           architecture, key decisions, tradeoffs." );
                    ] );
                ( "data_model",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Data structures / schemas / types. Code-style \
                           pseudocode is fine." );
                    ] );
                ( "api_surface",
                  `Assoc
                    [
                      ("type", `String "array");
                      ("items", `Assoc [ ("type", `String "string") ]);
                      ( "description",
                        `String
                          "List of HTTP endpoints / function signatures \
                           / module exports — one per line." );
                    ] );
              ] );
          ( "required",
            `List
              [
                `String "design";
                `String "data_model";
                `String "api_surface";
              ] );
        ])

let submit_implementation_name = "submit_implementation"

let submit_implementation_tool : tool_def =
  make_terminal_tool ~name:submit_implementation_name
    ~description:
      "Submit the implementation. Provide complete, runnable code \
       fulfilling the spec & design. If revising after a QA report, \
       address EVERY blocker and major issue and explicitly list which \
       issues you fixed in [addresses_issues]."
    ~input_schema:
      (`Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "code",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Complete code — multiple files separated by \
                           '// ===== filename =====' headers. Runnable \
                           or close to it." );
                    ] );
                ( "deployment_notes",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "How to run / deploy / test. 1-2 sentences." );
                    ] );
                ( "addresses_issues",
                  `Assoc
                    [
                      ("type", `String "array");
                      ("items", `Assoc [ ("type", `String "string") ]);
                      ( "description",
                        `String
                          "On revisions: which QA issues this iteration \
                           addresses. Empty array on initial impl." );
                    ] );
              ] );
          ( "required",
            `List [ `String "code"; `String "deployment_notes" ] );
        ])

let submit_qa_report_name = "submit_qa_report"

let submit_qa_report_tool : tool_def =
  make_terminal_tool ~name:submit_qa_report_name
    ~description:
      "Submit a QA report on the latest implementation. Be strict: \
       pass=true ONLY if NO blocker or major issues remain. Categorize \
       each issue by severity and propose specific fixes. The fullstack \
       engineer will see your issues verbatim and revise."
    ~input_schema:
      (`Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "pass",
                  `Assoc
                    [
                      ("type", `String "boolean");
                      ( "description",
                        `String
                          "true if implementation fully satisfies spec \
                           with NO blocker/major issues; false \
                           otherwise." );
                    ] );
                ( "issues",
                  `Assoc
                    [
                      ("type", `String "array");
                      ( "items",
                        `Assoc
                          [
                            ("type", `String "object");
                            ( "properties",
                              `Assoc
                                [
                                  ( "severity",
                                    `Assoc
                                      [
                                        ("type", `String "string");
                                        ( "enum",
                                          `List
                                            [
                                              `String "blocker";
                                              `String "major";
                                              `String "minor";
                                            ] );
                                      ] );
                                  ( "description",
                                    `Assoc
                                      [ ("type", `String "string") ] );
                                ] );
                            ( "required",
                              `List
                                [
                                  `String "severity";
                                  `String "description";
                                ] );
                          ] );
                      ( "description",
                        `String
                          "Issues found, with severity. Empty array if \
                           pass=true." );
                    ] );
                ( "suggestions",
                  `Assoc
                    [
                      ("type", `String "array");
                      ("items", `Assoc [ ("type", `String "string") ]);
                      ( "description",
                        `String
                          "Specific fix suggestions for the fullstack \
                           engineer. Concrete code-level when \
                           possible." );
                    ] );
              ] );
          ( "required",
            `List
              [
                `String "pass";
                `String "issues";
                `String "suggestions";
              ] );
        ])

(* ===== Parsers ===== *)

let parse_spec (input : Yojson.Safe.t) : string =
  let open Json_decode in
  match input with
  | `Assoc fs ->
      let spec = get_string_field_or "spec" ~default:"" fs in
      let stories =
        match List.assoc_opt "user_stories" fs with
        | Some (`List items) ->
            List.filter_map
              (function `String s -> Some s | _ -> None)
              items
        | _ -> []
      in
      let criteria =
        match List.assoc_opt "acceptance_criteria" fs with
        | Some (`List items) ->
            List.filter_map
              (function `String s -> Some s | _ -> None)
              items
        | _ -> []
      in
      let stories_block =
        if stories = [] then ""
        else
          "\n\nUser stories:\n"
          ^ String.concat "\n"
              (List.map (fun s -> "  • " ^ s) stories)
      in
      let criteria_block =
        if criteria = [] then ""
        else
          "\n\nAcceptance criteria:\n"
          ^ String.concat "\n"
              (List.map (fun s -> "  ✓ " ^ s) criteria)
      in
      spec ^ stories_block ^ criteria_block
  | _ -> "(invalid spec input)"

let parse_design (input : Yojson.Safe.t) : string =
  let open Json_decode in
  match input with
  | `Assoc fs ->
      let design = get_string_field_or "design" ~default:"" fs in
      let data_model = get_string_field_or "data_model" ~default:"" fs in
      let api =
        match List.assoc_opt "api_surface" fs with
        | Some (`List items) ->
            List.filter_map
              (function `String s -> Some s | _ -> None)
              items
        | _ -> []
      in
      let dm_block =
        if data_model = "" then ""
        else "\n\nData model:\n" ^ data_model
      in
      let api_block =
        if api = [] then ""
        else
          "\n\nAPI surface:\n"
          ^ String.concat "\n" (List.map (fun s -> "  → " ^ s) api)
      in
      design ^ dm_block ^ api_block
  | _ -> "(invalid design input)"

let parse_implementation (input : Yojson.Safe.t) : string =
  let open Json_decode in
  match input with
  | `Assoc fs ->
      let code = get_string_field_or "code" ~default:"" fs in
      let notes =
        get_string_field_or "deployment_notes" ~default:"" fs
      in
      let addresses =
        match List.assoc_opt "addresses_issues" fs with
        | Some (`List items) ->
            List.filter_map
              (function `String s -> Some s | _ -> None)
              items
        | _ -> []
      in
      let notes_block =
        if notes = "" then "" else "\n\nDeployment notes: " ^ notes
      in
      let addr_block =
        if addresses = [] then ""
        else
          "\n\nThis revision addresses:\n"
          ^ String.concat "\n"
              (List.map (fun s -> "  ↳ " ^ s) addresses)
      in
      code ^ notes_block ^ addr_block
  | _ -> "(invalid implementation input)"

let parse_qa_report (input : Yojson.Safe.t) : qa_report =
  let open Json_decode in
  match input with
  | `Assoc fs ->
      let pass = get_bool_field_or "pass" ~default:false fs in
      let issues =
        match List.assoc_opt "issues" fs with
        | Some (`List items) ->
            List.filter_map
              (function
                | `Assoc ifs ->
                    let severity =
                      get_string_field_or "severity" ~default:"minor" ifs
                    in
                    let description =
                      get_string_field_or "description" ~default:"" ifs
                    in
                    if description = "" then None
                    else Some { severity; description }
                | _ -> None)
              items
        | _ -> []
      in
      let suggestions =
        match List.assoc_opt "suggestions" fs with
        | Some (`List items) ->
            List.filter_map
              (function `String s -> Some s | _ -> None)
              items
        | _ -> []
      in
      { pass; issues; suggestions }
  | _ -> { pass = false; issues = []; suggestions = [] }

(* ===== Generic role-node runner ===== *)

let run_role ~name ~role_prompt ~user_input ~tools () =
  let conv =
    match
      Conversation.of_messages
        [ { role = User; content = [ Text user_input ] } ]
    with
    | Ok c -> c
    | Error msg ->
        failwith ("team: malformed input — " ^ msg)
  in
  let ctx =
    Context.empty
    |> Context.with_tools tools
    |> Context.apply_system ~system_prompt:role_prompt
    |> Context.with_conversation conv
  in
  Agent.run_loop ~max_iterations:8
    ~terminal_tools:(List.map (fun (t : tool_def) -> t.name) tools)
    ~name ~ctx ()

(* Run a role and return its terminal_tool input as raw JSON. Errors / no
   submit_X ⇒ a synthetic JSON object with an "_error" field so the
   downstream parser can degrade gracefully. *)
let run_and_capture ~name ~role_prompt ~user_input ~tools () : Yojson.Safe.t =
  try
    match run_role ~name ~role_prompt ~user_input ~tools () with
    | Ok (text, _) ->
        `Assoc
          [
            ("_error", `String "agent ended turn without calling terminal tool");
            ("_text", `String text);
          ]
    | Error (e, _) ->
        `Assoc [ ("_error", `String (agent_error_pp e)) ]
  with Agent.Task_terminal_called { input; _ } -> input

(* ===== Role node factories ===== *)

let make_pm_node ?(extra_tools = []) ~role_prompt () : t -> t =
 fun state ->
  let user_input =
    Printf.sprintf
      "USER GOAL: %s\n\n\
       Translate this goal into a product spec. Submit via submit_spec \
       with: a multi-paragraph spec, 3-7 user stories, 5-10 testable \
       acceptance criteria. Don't design or implement."
      state.goal
  in
  let input =
    run_and_capture ~name:"product" ~role_prompt ~user_input
      ~tools:(submit_spec_tool :: extra_tools)
      ()
  in
  {
    state with
    spec =
      Some
        {
          author = "product";
          content = parse_spec input;
          iteration = 0;
        };
  }

let make_design_node ?(extra_tools = []) ~role_prompt () : t -> t =
 fun state ->
  let spec_text =
    match state.spec with
    | Some a -> a.content
    | None -> "(spec not yet produced — design from goal alone)"
  in
  let user_input =
    Printf.sprintf
      "ORIGINAL GOAL: %s\n\nPRODUCT SPEC:\n%s\n\n\
       Produce the technical design. Submit via submit_design with: \
       multi-paragraph design rationale, data model, and API surface \
       list. Don't write implementation code."
      state.goal spec_text
  in
  let input =
    run_and_capture ~name:"design" ~role_prompt ~user_input
      ~tools:(submit_design_tool :: extra_tools)
      ()
  in
  {
    state with
    design =
      Some
        {
          author = "design";
          content = parse_design input;
          iteration = 0;
        };
  }

let make_fullstack_node ?(extra_tools = []) ~role_prompt () : t -> t =
 fun state ->
  let spec_text =
    Option.map (fun a -> a.content) state.spec
    |> Option.value ~default:"(no spec)"
  in
  let design_text =
    Option.map (fun a -> a.content) state.design
    |> Option.value ~default:"(no design)"
  in
  let iter = state.iteration + 1 in
  let user_input =
    match (state.implementation, state.latest_qa) with
    | None, _ ->
        Printf.sprintf
          "GOAL: %s\n\nSPEC:\n%s\n\nDESIGN:\n%s\n\n\
           Build the INITIAL implementation (iteration %d of max %d). \
           Output complete, runnable code via submit_implementation. \
           [addresses_issues] should be empty on the initial pass."
          state.goal spec_text design_text iter state.max_iterations
    | Some prev, Some qa ->
        let issues_text =
          if qa.issues = [] then "(no specific issues, but pass=false)"
          else
            qa.issues
            |> List.map (fun i ->
                   Printf.sprintf "  [%s] %s" i.severity i.description)
            |> String.concat "\n"
        in
        let suggestions_text =
          if qa.suggestions = [] then ""
          else
            "\n\nQA suggestions:\n"
            ^ String.concat "\n"
                (List.map (fun s -> "  • " ^ s) qa.suggestions)
        in
        Printf.sprintf
          "GOAL: %s\n\nSPEC:\n%s\n\nDESIGN:\n%s\n\n\
           PREVIOUS IMPLEMENTATION (iteration %d):\n%s\n\n\
           QA REPORTED:\n%s%s\n\n\
           Revise the implementation (iteration %d of max %d). FIX every \
           blocker and major issue. List which issues you addressed in \
           [addresses_issues]. Output complete revised code via \
           submit_implementation."
          state.goal spec_text design_text state.iteration prev.content
          issues_text suggestions_text iter state.max_iterations
    | Some _, None ->
        (* shouldn't happen but degrade gracefully *)
        Printf.sprintf
          "GOAL: %s\n\nSPEC:\n%s\n\nDESIGN:\n%s\n\n\
           Iterate on the implementation (no QA report yet, treat as \
           initial). Submit via submit_implementation."
          state.goal spec_text design_text
  in
  let input =
    run_and_capture ~name:"fullstack" ~role_prompt ~user_input
      ~tools:(submit_implementation_tool :: extra_tools)
      ()
  in
  let new_impl =
    { author = "fullstack"; content = parse_implementation input; iteration = iter }
  in
  {
    state with
    implementation = Some new_impl;
    iteration = iter;
  }

let make_qa_node ?(extra_tools = []) ~role_prompt () : t -> t =
 fun state ->
  match state.implementation with
  | None ->
      (* Shouldn't happen — fullstack always runs first in the loop. *)
      state
  | Some impl ->
      let spec_text =
        Option.map (fun a -> a.content) state.spec
        |> Option.value ~default:"(no spec)"
      in
      let user_input =
        Printf.sprintf
          "Review this implementation against the spec.\n\nSPEC:\n%s\n\n\
           IMPLEMENTATION (iteration %d of max %d):\n%s\n\n\
           Be strict. Iteration %d is the LAST allowed attempt — flag \
           ALL issues now. Submit findings via submit_qa_report. \
           pass=true ONLY if NO blocker or major issues remain."
          spec_text state.iteration state.max_iterations impl.content
          state.max_iterations
      in
      let input =
        run_and_capture ~name:"qa" ~role_prompt ~user_input
          ~tools:(submit_qa_report_tool :: extra_tools)
          ()
      in
      let report = parse_qa_report input in
      let new_history = (impl, Some report) :: state.history in
      { state with latest_qa = Some report; history = new_history }

(* ===== Workflow ===== *)

let make_workflow ~pm_node ~design_node ~fullstack_node ~qa_node :
    t Topology.shape =
  let loop_cond s =
    match s.latest_qa with
    | Some r when r.pass -> true
    | _ -> s.iteration >= s.max_iterations
  in
  Topology.Sequence
    [
      Topology.Node { name = "product"; run = pm_node };
      Topology.Node { name = "design"; run = design_node };
      Topology.Loop_until
        {
          cond = loop_cond;
          body =
            Topology.Sequence
              [
                Topology.Node { name = "fullstack"; run = fullstack_node };
                Topology.Node { name = "qa"; run = qa_node };
              ];
          max_iters = None (* state.max_iterations is the cap *);
        };
    ]

(* ===== Pretty printer ===== *)

let print_summary state =
  print_endline "";
  print_endline "================ TEAM RUN SUMMARY ================";
  Printf.printf "Goal: %s\n" state.goal;
  Printf.printf "Iterations used: %d / %d\n" state.iteration state.max_iterations;
  (match state.latest_qa with
  | Some r when r.pass ->
      Printf.printf "QA verdict: ✅ PASSED on iteration %d\n" state.iteration
  | Some r ->
      Printf.printf
        "QA verdict: ❌ FAILED after %d iteration(s) — %d issue(s) remain\n"
        state.iteration (List.length r.issues)
  | None -> Printf.printf "QA verdict: (no QA report)\n");
  print_endline ""

let print_artifact label = function
  | None -> Printf.printf "── %s: (none)\n\n" label
  | Some a ->
      Printf.printf "──── %s ────\n%s\n\n" label a.content

let print_qa_report = function
  | None -> ()
  | Some r ->
      Printf.printf "──── QA REPORT ────\n";
      Printf.printf "pass: %b\n" r.pass;
      if r.issues <> [] then begin
        print_endline "Issues:";
        List.iter
          (fun i ->
            Printf.printf "  [%s] %s\n" i.severity i.description)
          r.issues
      end;
      if r.suggestions <> [] then begin
        print_endline "Suggestions:";
        List.iter (fun s -> Printf.printf "  • %s\n" s) r.suggestions
      end;
      print_endline ""

let print_full state =
  print_summary state;
  print_artifact "PRODUCT SPEC" state.spec;
  print_artifact "TECHNICAL DESIGN" state.design;
  print_artifact "FINAL IMPLEMENTATION" state.implementation;
  print_qa_report state.latest_qa;
  if List.length state.history > 1 then begin
    Printf.printf "──── ITERATION HISTORY (%d) ────\n"
      (List.length state.history);
    List.iteri
      (fun i (impl, qa) ->
        let n = List.length state.history - i in
        Printf.printf "\n  iteration %d:\n    impl: %d chars\n" n
          (String.length impl.content);
        match qa with
        | Some r ->
            Printf.printf "    qa.pass=%b, %d issue(s)\n" r.pass
              (List.length r.issues)
        | None -> ())
      state.history
  end;
  print_endline "==================================================="
