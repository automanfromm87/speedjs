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

type t = {
  topic : string;
  rounds : round list;
      (** Most recent first. [List.rev] for chronological order. *)
  consensus : bool;
  max_rounds : int;
}

let initial ~topic ~max_rounds =
  { topic; rounds = []; consensus = false; max_rounds }

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

let build_input ~speaker ~max_rounds state =
  let round_no = List.length state.rounds + 1 in
  match state.rounds with
  | [] ->
      Printf.sprintf
        "DEBATE TOPIC: %s\n\n\
         You are speaking FIRST (round 1 of max %d). Open with your \
         strongest substantive position. Be specific — cite concrete \
         tradeoffs, mechanisms, or examples from your domain. End your \
         turn by calling submit_argument."
        state.topic max_rounds
  | last :: _ ->
      Printf.sprintf
        "DEBATE TOPIC: %s\n\n\
         Round %d of max %d. You are %s.\n\n\
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
        state.topic round_no max_rounds speaker last.speaker last.argument
        (List.map (fun p -> "  • " ^ p) last.key_points |> String.concat "\n")

(* ===== Debater node factory ===== *)

let make_debater_node ?(extra_tools = []) ~name ~role_prompt () :
    t -> t =
 fun state ->
  if state.consensus then state
  else
    let user_input = build_input ~speaker:name ~max_rounds:state.max_rounds state in
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
    let new_round =
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
      rounds = new_round :: state.rounds;
      consensus = new_round.agree;
    }

(* ===== Topology builder ===== *)

let make_topology ~name_a ~node_a ~name_b ~node_b : t Topology.shape =
  let loop_cond s =
    s.consensus || List.length s.rounds >= s.max_rounds
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

(* ===== Transcript printer ===== *)

let print_transcript state =
  print_endline "";
  print_endline "================ DEBATE TRANSCRIPT ================";
  Printf.printf "Topic: %s\n\n" state.topic;
  let chronological = List.rev state.rounds in
  List.iteri
    (fun i r ->
      Printf.printf "─── Round %d — %s ───\n" (i + 1) r.speaker;
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
    Printf.printf "*** CONSENSUS reached after %d round(s) ***\n"
      (List.length state.rounds)
  else
    Printf.printf "*** STALEMATE after %d round(s) (max=%d) ***\n"
      (List.length state.rounds) state.max_rounds;
  print_endline "==================================================="
