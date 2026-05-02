(** Two-agent structured debate built on [Topology].

    Each round, the active speaker reads the opponent's last argument
    plus the topic, formulates a position, and submits a structured
    response via the [submit_argument] terminal tool. The debate ends
    when either side sets [agree=true] or [max_rounds] is reached.

    Implementation notes:
    - State is [t]; each speaker is a node ([state -> state]).
    - The topology is a [Loop_until { cond; body = Sequence [a; b] }].
    - [submit_argument] follows the same "terminal tool" pattern as
      [Plan_act.submit_task_result_tool] — handler errors out, the
      agent loop short-circuits via [Agent.Task_terminal_called]. *)

open Types

type round = {
  speaker : string;
  argument : string;
  key_points : string list;
  agree : bool;
}

type design = {
  proposal : string;
      (** The actual design / plan. Free-form prose, ideally a few paragraphs
          + concrete decisions. *)
  key_decisions : string list;
      (** Headline choices the architect made when reconciling the debate. *)
  open_questions : string list;
      (** Things deliberately left for human follow-up. *)
}

type t = {
  topic : string;
  turns : round list;
      (** Most recent first. [List.rev] for chronological order. One turn
          = one speaker. A "round" of debate is 2 turns. *)
  consensus : bool;
  max_turns : int;
      (** Cap on total speaker turns. Two speakers per round, so 6 turns
          = 3 conversational rounds. *)
  final_design : design option;
      (** Set by the synthesizer node after the debate concludes. *)
}

let initial ~topic ~max_turns =
  {
    topic;
    turns = [];
    consensus = false;
    max_turns;
    final_design = None;
  }

(* ===== submit_argument: terminal tool ===== *)

let submit_argument_name = "submit_argument"

let submit_argument_tool : tool_def =
  {
    idempotent = true;
    timeout_sec = None;
    category = "meta";
    name = submit_argument_name;
    description =
      "Submit your argument for THIS round of the debate. Call this \
       exactly once when you've made your case. Provide your full \
       argument text, 2-4 short key points summarizing your stance, \
       and an [agree] flag — set agree=true ONLY if your opponent's \
       last argument fully convinced you and you concede the point. \
       Otherwise agree=false and provide a counter-argument.";
    input_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "argument",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Full argument for this round (1-3 paragraphs). \
                           Be substantive — cite specific tradeoffs, not \
                           generalities." );
                    ] );
                ( "key_points",
                  `Assoc
                    [
                      ("type", `String "array");
                      ("items", `Assoc [ ("type", `String "string") ]);
                      ( "description",
                        `String
                          "2-4 short bullet points summarizing your \
                           central claims this round." );
                    ] );
                ( "agree",
                  `Assoc
                    [
                      ("type", `String "boolean");
                      ( "description",
                        `String
                          "True iff opponent's argument fully convinces \
                           you to concede. Default false." );
                    ] );
              ] );
          ( "required",
            `List
              [ `String "argument"; `String "key_points"; `String "agree" ] );
        ];
    handler =
      (fun _ ->
        Error
          "submit_argument is a terminal tool — its handler should never \
           be invoked");
  }

let parse_argument ~speaker (input : Yojson.Safe.t) : round =
  let open Json_decode in
  match input with
  | `Assoc fs ->
      let argument = get_string_field_or "argument" ~default:"(no argument)" fs in
      let agree = get_bool_field_or "agree" ~default:false fs in
      let key_points =
        match List.assoc_opt "key_points" fs with
        | Some (`List items) ->
            List.filter_map (function `String s -> Some s | _ -> None) items
        | _ -> []
      in
      { speaker; argument; key_points; agree }
  | _ ->
      { speaker; argument = "(invalid input)"; key_points = []; agree = false }

(* ===== Build the user message for the next speaker ===== *)

let build_input ~speaker ~max_turns state =
  let turn_no = List.length state.turns + 1 in
  match state.turns with
  | [] ->
      Printf.sprintf
        "DEBATE TOPIC: %s\n\n\
         You are speaking FIRST (turn 1 of max %d). Open with your \
         strongest substantive position. Be specific — cite concrete \
         tradeoffs, mechanisms, or examples from your domain. End your \
         turn by calling submit_argument."
        state.topic max_turns
  | last :: _ ->
      Printf.sprintf
        "DEBATE TOPIC: %s\n\n\
         Turn %d of max %d. You are %s.\n\n\
         Your opponent (%s) just argued:\n\
         ---\n\
         %s\n\
         ---\n\n\
         Their key points:\n\
         %s\n\n\
         Now: rebut, refine, or — if their argument actually convinces \
         you — call submit_argument with agree=true to concede. \
         Otherwise present your counter-argument grounded in concrete \
         specifics from YOUR domain. Stay substantive; don't restate \
         their position. End your turn with submit_argument."
        state.topic turn_no max_turns speaker last.speaker last.argument
        (List.map (fun p -> "  • " ^ p) last.key_points |> String.concat "\n")

(* ===== Debater node factory ===== *)

let make_debater_node ?(extra_tools = []) ~name ~role_prompt () :
    t -> t =
 fun state ->
  if state.consensus then state
  else
    let user_input = build_input ~speaker:name ~max_turns:state.max_turns state in
    let conv =
      match
        Conversation.of_messages
          [ { role = User; content = [ Text user_input ] } ]
      with
      | Ok c -> c
      | Error msg ->
          failwith ("debate: malformed initial message — " ^ msg)
    in
    let ctx =
      Context.empty
      |> Context.with_tools (submit_argument_tool :: extra_tools)
      |> Context.apply_system ~system_prompt:role_prompt
      |> Context.with_conversation conv
    in
    let new_turn =
      try
        match
          Agent.run_loop ~max_iterations:8
            ~terminal_tools:[ submit_argument_name ]
            ~name ~ctx ()
        with
        | Ok (answer, _) ->
            (* No submit_argument called — treat the End_turn text as
               the argument. agree=false; debate continues. *)
            {
              speaker = name;
              argument =
                (if answer = "" then "(no argument; agent ended turn empty)"
                 else answer);
              key_points = [];
              agree = false;
            }
        | Error (e, _) ->
            {
              speaker = name;
              argument =
                Printf.sprintf "[ERROR] %s" (agent_error_pp e);
              key_points = [];
              agree = false;
            }
      with Agent.Task_terminal_called { input; _ } ->
        parse_argument ~speaker:name input
    in
    {
      state with
      turns = new_turn :: state.turns;
      consensus = new_turn.agree;
    }

(* ===== submit_design: synthesizer's terminal tool ===== *)

let submit_design_name = "submit_design"

let submit_design_tool : tool_def =
  {
    idempotent = true;
    timeout_sec = None;
    category = "meta";
    name = submit_design_name;
    description =
      "Submit the FINAL design / proposal that synthesizes the debate \
       transcript. Call this exactly once with: a substantive [proposal] \
       (multiple paragraphs covering the design + rationale), \
       [key_decisions] (the major choices you made when reconciling \
       disagreements), and [open_questions] (deliberate gaps left for \
       human follow-up). The proposal should NOT recap the debate; it \
       should be a concrete forward-looking design.";
    input_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "proposal",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "The actual design / plan, multi-paragraph. \
                           Concrete decisions, not a summary of \
                           positions." );
                    ] );
                ( "key_decisions",
                  `Assoc
                    [
                      ("type", `String "array");
                      ("items", `Assoc [ ("type", `String "string") ]);
                      ( "description",
                        `String
                          "3-6 short bullets naming the key choices and \
                           why you made them." );
                    ] );
                ( "open_questions",
                  `Assoc
                    [
                      ("type", `String "array");
                      ("items", `Assoc [ ("type", `String "string") ]);
                      ( "description",
                        `String
                          "Things you intentionally left for human \
                           follow-up, with brief justification." );
                    ] );
              ] );
          ( "required",
            `List
              [
                `String "proposal";
                `String "key_decisions";
                `String "open_questions";
              ] );
        ];
    handler =
      (fun _ ->
        Error
          "submit_design is a terminal tool — its handler should never be \
           invoked");
  }

let parse_design (input : Yojson.Safe.t) : design =
  let open Json_decode in
  match input with
  | `Assoc fs ->
      let proposal =
        get_string_field_or "proposal" ~default:"(no proposal)" fs
      in
      let pull name =
        match List.assoc_opt name fs with
        | Some (`List items) ->
            List.filter_map
              (function `String s -> Some s | _ -> None)
              items
        | _ -> []
      in
      {
        proposal;
        key_decisions = pull "key_decisions";
        open_questions = pull "open_questions";
      }
  | _ ->
      {
        proposal = "(invalid input)";
        key_decisions = [];
        open_questions = [];
      }

(* ===== Synthesizer node ===== *)

let render_transcript_for_architect state =
  let chronological = List.rev state.turns in
  let buf = Buffer.create 4096 in
  Buffer.add_string buf
    (Printf.sprintf "TOPIC: %s\n\n" state.topic);
  Buffer.add_string buf
    (Printf.sprintf "DEBATE TRANSCRIPT (%d turn(s) over %s):\n\n"
       (List.length state.turns)
       (if state.consensus then "consensus reached"
        else "no consensus"));
  List.iteri
    (fun i r ->
      Buffer.add_string buf
        (Printf.sprintf "--- Turn %d: %s ---\n" (i + 1) r.speaker);
      Buffer.add_string buf r.argument;
      Buffer.add_char buf '\n';
      if r.key_points <> [] then begin
        Buffer.add_string buf "Key points:\n";
        List.iter
          (fun p -> Buffer.add_string buf (Printf.sprintf "  • %s\n" p))
          r.key_points
      end;
      Buffer.add_char buf '\n')
    chronological;
  Buffer.contents buf

let make_synthesizer_node ?(extra_tools = []) ~name ~architect_prompt () :
    t -> t =
 fun state ->
  let transcript = render_transcript_for_architect state in
  let user_input =
    Printf.sprintf
      "%s\n\n\
       Your job: produce the FINAL design / proposal.\n\n\
       The two debaters surfaced tradeoffs and concrete patterns from \
       their domains. Your job is NOT to summarize who won, but to \
       synthesize a forward-looking design that:\n\n\
      \  • takes the strongest valid points from BOTH sides\n\
      \  • makes a concrete decision where they disagree (justify why)\n\
      \  • leaves genuinely-open questions explicit (don't pretend you've \
       solved everything)\n\n\
       End your response by calling submit_design with the structured \
       fields. The [proposal] field should be the full design text \
       (multiple paragraphs). Don't restate the debate — write the \
       design as if it were a tech-spec section."
      transcript
  in
  let conv =
    match
      Conversation.of_messages
        [ { role = User; content = [ Text user_input ] } ]
    with
    | Ok c -> c
    | Error msg ->
        failwith ("debate: malformed synthesizer input — " ^ msg)
  in
  let ctx =
    Context.empty
    |> Context.with_tools (submit_design_tool :: extra_tools)
    |> Context.apply_system ~system_prompt:architect_prompt
    |> Context.with_conversation conv
  in
  let design =
    try
      match
        Agent.run_loop ~max_iterations:5
          ~terminal_tools:[ submit_design_name ]
          ~name ~ctx ()
      with
      | Ok (answer, _) ->
          (* No submit_design — fall back to using the End_turn text
             as the proposal. *)
          {
            proposal =
              (if answer = "" then "(architect ended turn empty)"
               else answer);
            key_decisions = [];
            open_questions = [];
          }
      | Error (e, _) ->
          {
            proposal = Printf.sprintf "[ERROR] %s" (agent_error_pp e);
            key_decisions = [];
            open_questions = [];
          }
    with Agent.Task_terminal_called { input; _ } -> parse_design input
  in
  { state with final_design = Some design }

(* ===== Topology builder ===== *)

let make_debate_loop ~name_a ~node_a ~name_b ~node_b : t Topology.shape =
  let loop_cond s =
    s.consensus || List.length s.turns >= s.max_turns
  in
  Topology.Loop_until
    {
      cond = loop_cond;
      body =
        Topology.Sequence
          [
            Topology.Node { name = name_a; run = node_a };
            Topology.Node { name = name_b; run = node_b };
          ];
      max_iters = None;
    }

(** The full workflow: debate → synthesize. The synthesizer reads the
    finished transcript and produces a [final_design]. *)
let make_workflow
    ~name_a ~node_a
    ~name_b ~node_b
    ~synthesizer_name ~synthesizer_node : t Topology.shape =
  Topology.Sequence
    [
      make_debate_loop ~name_a ~node_a ~name_b ~node_b;
      Topology.Node { name = synthesizer_name; run = synthesizer_node };
    ]

(* ===== Transcript printer ===== *)

let print_transcript state =
  print_endline "";
  print_endline "================ DEBATE TRANSCRIPT ================";
  Printf.printf "Topic: %s\n\n" state.topic;
  let chronological = List.rev state.turns in
  List.iteri
    (fun i r ->
      Printf.printf "─── Turn %d — %s ───\n" (i + 1) r.speaker;
      print_endline r.argument;
      if r.key_points <> [] then begin
        print_endline "";
        List.iter (fun p -> Printf.printf "  • %s\n" p) r.key_points
      end;
      if r.agree then print_endline "\n(this speaker conceded)";
      print_endline "")
    chronological;
  print_endline "";
  if state.consensus then
    Printf.printf "*** CONSENSUS reached after %d turn(s) ***\n"
      (List.length state.turns)
  else
    Printf.printf "*** STALEMATE after %d turn(s) (max=%d) ***\n"
      (List.length state.turns) state.max_turns;
  print_endline "==================================================="

let print_design state =
  match state.final_design with
  | None ->
      print_endline "";
      print_endline "(no synthesizer ran — final_design is None)"
  | Some d ->
      print_endline "";
      print_endline "================== FINAL DESIGN ==================";
      print_endline d.proposal;
      if d.key_decisions <> [] then begin
        print_endline "";
        print_endline "Key decisions:";
        List.iter (fun k -> Printf.printf "  • %s\n" k) d.key_decisions
      end;
      if d.open_questions <> [] then begin
        print_endline "";
        print_endline "Open questions:";
        List.iter (fun q -> Printf.printf "  ? %s\n" q) d.open_questions
      end;
      print_endline "==================================================="
