(** Single ReAct step. See [step.mli] for design rationale. *)

open Types

type outcome =
  | Continue of Context.t
  | Terminal_text of { answer : string; ctx : Context.t }
  | Terminal_tool of {
      tool_name : string;
      input : Yojson.Safe.t;
      ctx : Context.t;
    }
  | Wait_for_user of {
      tool_use_id : Id.Tool_use_id.t;
      question : string;
      ctx : Context.t;
    }
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

let dispatch_tool_uses (content : content_block list) : content_block list =
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
        try Effect.perform (Effects.Tool_calls uses)
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

let find_ask_user_block (content : content_block list) :
    (Id.Tool_use_id.t * string) option =
  List.find_map
    (function
      | Tool_use { id; name; input } when name = Tools.ask_user_name ->
          let question =
            match input with
            | `Assoc fs -> (
                match List.assoc_opt "question" fs with
                | Some (`String s) -> s
                | _ -> "(no question text)")
            | _ -> "(invalid ask_user input)"
          in
          Some (id, question)
      | _ -> None)
    content

let once ?(strategy = Context.Strategy.flat) ?(tool_choice = Tc_auto)
    ?(terminal_tools : string list = []) ~(ctx : Context.t) () : outcome =
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
          Terminal_tool { tool_name; input; ctx = ctx_with_assistant }
      | None -> begin
          match find_ask_user_block response.content with
          | Some (tool_use_id, question) ->
              Wait_for_user
                { tool_use_id; question; ctx = ctx_with_assistant }
          | None ->
              let tool_results = dispatch_tool_uses response.content in
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
