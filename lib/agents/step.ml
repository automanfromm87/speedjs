(** Single ReAct step. See [step.mli] for design rationale. *)

open Types

type result =
  | Continue of Context.t
  | Terminal_text of { answer : string; ctx : Context.t }
  | Failed of { reason : agent_error; ctx : Context.t }

let extract_final_text (content : content_block list) : string =
  content
  |> List.filter_map (function Text s -> Some s | _ -> None)
  |> String.concat "\n"

let log_tool_result ~name ~is_error result =
  let preview =
    if String.length result > 300 then String.sub result 0 300 ^ "..."
    else result
  in
  Effect.perform
    (Effects.Log
       (Printf.sprintf "  %s %s: %s"
          (if is_error then "✗" else "←")
          name preview))

let dispatch_tool_uses ~(tools : tool_def list)
    (content : content_block list) : content_block list =
  let tool_uses =
    List.filter_map
      (function
        | Tool_use { id; name; input } -> Some (id, name, input) | _ -> None)
      content
  in
  match tool_uses with
  | [] -> []
  | uses ->
      let log_msg =
        match uses with
        | [ (_, name, input) ] ->
            Printf.sprintf "  → tool: %s(%s)" name
              (Yojson.Safe.to_string input)
        | _ ->
            Printf.sprintf "  → batch[%d]: %s" (List.length uses)
              (String.concat ", " (List.map (fun (_, n, _) -> n) uses))
      in
      Effect.perform (Effects.Log log_msg);
      let results =
        try Effect.perform (Effects.Tool_calls (tools, uses))
        with e ->
          List.map
            (fun (id, _, _) -> (id, Error (Printexc.to_string e)))
            uses
      in
      List.map
        (fun (id, name, _input) ->
          let result =
            try List.assoc id results
            with Not_found ->
              Error
                (Printf.sprintf "no result for use_id %s"
                   (Id.Tool_use_id.to_string id))
          in
          let content_str, is_error =
            match result with Ok s -> (s, false) | Error e -> (e, true)
          in
          log_tool_result ~name ~is_error content_str;
          Tool_result { tool_use_id = id; content = content_str; is_error })
        uses

(** Look for a tool_use whose tool (resolved via the spec-provided
    [tools] registry) declares [Pause] capability. Such a tool
    suspends the agent loop with [Wait_for_user]; the caller resumes
    by appending a [Tool_result] for [tool_use_id]. The "is this a
    pause tool?" question is answered by [tool.capabilities],
    NOT by hardcoded name match — any spec that lists a tool with
    [Pause] gets pause semantics; specs without one can never pause. *)
let find_pause_block ~(tools : tool_def list)
    (content : content_block list) : (Id.Tool_use_id.t * string) option =
  let is_pause name =
    match List.find_opt (fun (t : tool_def) -> t.name = name) tools with
    | Some t -> List.mem Pause t.capabilities
    | None -> false
  in
  List.find_map
    (function
      | Tool_use { id; name; input } when is_pause name ->
          let question =
            match input with
            | `Assoc fs -> (
                match List.assoc_opt "question" fs with
                | Some (`String s) -> s
                | _ -> "(no question text)")
            | _ -> "(invalid pause input)"
          in
          Some (id, question)
      | _ -> None)
    content

let once ?(strategy = Context.Strategy.flat) ?(tool_choice = Tc_auto)
    ?(terminal_tools : string list = []) ~(ctx : Context.t) () : result =
  let args = Context.to_llm_args ~strategy ~tool_choice ctx in
  let response = Effect.perform (Effects.Llm_complete args) in
  Effect.perform
    (Effects.Log
       (Printf.sprintf "  [step] %d in / %d out tokens"
          response.usage.input_tokens response.usage.output_tokens));
  let final_text () = extract_final_text response.content in
  match response.stop_reason with
  | End_turn | Stop_sequence ->
      let ctx = Context.push_assistant response.content ctx in
      Terminal_text { answer = final_text (); ctx }
  | Tool_use_stop -> begin
      let ctx_with_assistant = Context.push_assistant response.content ctx in
      let terminal_call =
        List.find_map
          (function
            | Tool_use { name; input; _ } when List.mem name terminal_tools
              ->
                Some (name, input)
            | _ -> None)
          response.content
      in
      match terminal_call with
      | Some (tool_name, input) ->
          (* Polymorphic [Terminal] effect: the standard handler in
             [Agent.execute] abandons the continuation, so this perform
             never actually returns. Type-wise it unifies with [result]
             (the surrounding match's branch type). If a custom handler
             resumes anyway, OCaml types stay sound — the resumed
             [result] becomes our return value. *)
          Effect.perform
            (Effects.Terminal
               { tool_name; payload = input; ctx_so_far = ctx_with_assistant })
      | None -> begin
          let ctx_tools = Context.tools ctx_with_assistant in
          match find_pause_block ~tools:ctx_tools response.content with
          | Some (tool_use_id, question) ->
              (* Pause is a resumable effect: handler may return a
                 [message] (the user's reply, typically a User turn
                 carrying a [Tool_result]) and the loop continues, OR
                 the handler abandons the continuation (default
                 [Agent.execute] behavior). *)
              let resume_msg : message =
                Effect.perform
                  (Effects.Pause
                     { tool_use_id; question; ctx_so_far = ctx_with_assistant })
              in
              let conv =
                Conversation.push_user_with_results
                  (Context.conversation ctx_with_assistant)
                  resume_msg.content
              in
              Continue (Context.with_conversation conv ctx_with_assistant)
          | None ->
              let tool_results =
                dispatch_tool_uses ~tools:ctx_tools response.content
              in
              let ctx =
                Context.push_tool_results tool_results ctx_with_assistant
              in
              Continue ctx
        end
    end
  | Max_tokens -> Failed { reason = Llm_max_tokens; ctx }
  | Other "refusal" ->
      let s = final_text () in
      Failed
        {
          reason =
            Llm_refused (if s = "" then "no reason given" else s);
          ctx;
        }
  | Other s -> Failed { reason = Stop_reason_unexpected s; ctx }
