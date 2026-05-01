(** The ReAct agent loop, written in pure effect-perform style.

    Knows nothing about HTTP, LLM APIs, or tool execution — it just performs
    effects. The handler installed at runtime decides everything. *)

open Types

(** Default iteration cap per agent invocation. helix uses 100; we match.
    Lower values (e.g. 10 — the old hardcoded value) starve any task that
    needs a build → fix → rebuild loop. *)
let default_max_iterations = 100

let extract_final_text (content : content_block list) : string =
  content
  |> List.filter_map (function Text s -> Some s | _ -> None)
  |> String.concat "\n"

(** Pretty-print a tool result for log preview (truncate to 300 chars). *)
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

(** Dispatch all [Tool_use] blocks via a single [Tool_calls] effect.
    Handlers may run them in parallel (production does for 2+); single
    tool case is just a 1-element list. Returns [Tool_result] blocks in
    the original tool_use order — the LLM expects 1:1 alignment. *)
let execute_tool_calls (content : content_block list) : content_block list =
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
          (* Handler itself blew up — fail every tool with the same
             diagnostic so the model can attempt recovery. *)
          List.map
            (fun (id, _, _) -> (id, Error (Printexc.to_string e)))
            uses
      in
      List.map
        (fun (id, name, _input) ->
          let result =
            try List.assoc id results
            with Not_found ->
              Error (Printf.sprintf "no result for use_id %s" id)
          in
          let content_str, is_error =
            match result with Ok s -> (s, false) | Error e -> (e, true)
          in
          log_tool_result ~name ~is_error content_str;
          Tool_result { tool_use_id = id; content = content_str; is_error })
        uses

(** Raised when the LLM calls the [ask_user] pause-tool. The agent loop
    halts mid-conversation; [run_session] catches this and returns
    [Outcome_waiting]. *)
exception Wait_for_user of {
  tool_use_id : string;
  question : string;
  ctx_so_far : Context.t;
}

(** Raised when the LLM calls a tool listed in the loop's
    [terminal_tools] set — e.g. [submit_task_result]. The loop short-circuits
    and the caller (typically [Plan_act]) parses the structured input. *)
exception Task_terminal_called of {
  tool_name : string;
  input : Yojson.Safe.t;
  ctx_so_far : Context.t;
}

(** Look for an [ask_user] tool_use block in the model's response. Returns
    [(id, question)] for the first match, or [None]. *)
let find_ask_user_block (content : content_block list) :
    (string * string) option =
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

(** Core ReAct loop driven by a [Context.t].

    The context carries system prompt, env blocks, tools, and the
    conversation. [strategy] decides how the conversation materializes
    on each LLM call (flat, sliding window, compacted).

    Returns [Ok (answer, final_ctx)] or [Error (reason, partial_ctx)].
    May raise [Wait_for_user] (ask_user pause) or [Task_terminal_called]
    (terminal tool short-circuit).

    Endgame protocol: when [terminal_tools] is non-empty and we're
    within 2 iterations of [max_iterations], force [tool_choice] to the
    first terminal tool. Prevents stuck loops from running to the cap
    without ever submitting a result. *)
let run_loop ?(max_iterations = default_max_iterations)
    ?(terminal_tools : string list = [])
    ?(strategy = Context.Strategy.flat) ?(name = "agent")
    ~(ctx : Context.t) () :
    (string * Context.t, agent_error * Context.t) Result.t =
  let endgame_iter_threshold = max (max_iterations - 2) 1 in
  (* Best-effort Governor tick — tests run [run_loop] directly without
     installing a Governor; swallow [Effect.Unhandled] so unsandboxed
     callers still work. The production runtime always has Governor at
     the outermost layer (see [bin/setup.ml]). *)
  let safe_tick ev =
    try Effect.perform (Governor.Tick ev) with _ -> ()
  in
  let rec loop ctx iter =
    if iter > max_iterations then
      Error (Max_iterations_reached max_iterations, ctx)
    else begin
      safe_tick (Iteration_started { agent = name; iter });
      Effect.perform
        (Effects.Log (Printf.sprintf "[iter %d] calling LLM..." iter));
      let in_endgame =
        terminal_tools <> [] && iter >= endgame_iter_threshold
      in
      let tool_choice =
        if in_endgame then begin
          let terminal_name = List.hd terminal_tools in
          Effect.perform
            (Effects.Log
               (Printf.sprintf
                  "[iter %d] ENDGAME — forcing tool_choice=%s (within %d iters of cap)"
                  iter terminal_name (max_iterations - iter + 1)));
          Tc_tool terminal_name
        end
        else Tc_auto
      in
      let args = Context.to_llm_args ~strategy ~tool_choice ctx in
      let response = Effect.perform (Effects.Llm_complete args) in
      Effect.perform
        (Effects.Log
           (Printf.sprintf "[iter %d] usage: %d in / %d out tokens" iter
              response.usage.input_tokens response.usage.output_tokens));
      let final_text () = extract_final_text response.content in
      match response.stop_reason with
      | End_turn | Stop_sequence ->
          let ctx = Context.push_assistant response.content ctx in
          Ok (final_text (), ctx)
      | Tool_use_stop -> (
          let ctx_with_assistant =
            Context.push_assistant response.content ctx
          in
          (* Terminal tools first — short-circuits before dispatch. *)
          let terminal_call =
            List.find_map
              (function
                | Tool_use { name; input; _ }
                  when List.mem name terminal_tools ->
                    Some (name, input)
                | _ -> None)
              response.content
          in
          match terminal_call with
          | Some (tool_name, input) ->
              raise
                (Task_terminal_called
                   { tool_name; input; ctx_so_far = ctx_with_assistant })
          | None -> (
              match find_ask_user_block response.content with
              | Some (tool_use_id, question) ->
                  raise
                    (Wait_for_user
                       {
                         tool_use_id;
                         question;
                         ctx_so_far = ctx_with_assistant;
                       })
              | None ->
                  let tool_results = execute_tool_calls response.content in
                  let ctx =
                    Context.push_tool_results tool_results ctx_with_assistant
                  in
                  loop ctx (iter + 1)))
      | Max_tokens -> Error (Llm_max_tokens, ctx)
      | Other "refusal" ->
          Error
            ( Llm_refused
                (let s = final_text () in
                 if s = "" then "no reason given" else s),
              ctx )
      | Other s -> Error (Stop_reason_unexpected s, ctx)
    end
  in
  loop ctx 1

(** Build a [Context] for a single user-query / tools combination. *)
let make_ctx ~user_query ~tools =
  Context.empty
  |> Context.with_tools tools
  |> Context.push_user_text user_query

(** One-shot entry point. Returns just the final answer string. *)
let run ?max_iterations ~user_query ~tools () : agent_result =
  let ctx = make_ctx ~user_query ~tools in
  match run_loop ?max_iterations ~ctx () with
  | Ok (answer, _) -> Ok answer
  | Error (e, _) -> Error e

(** Multi-turn entry point. Seeds with [messages], runs the loop, and
    returns an [agent_outcome] carrying the updated history (including
    any pending ask_user tool_use). *)
let run_session ?max_iterations ~messages ~tools () : agent_outcome =
  let ctx_of_messages msgs =
    let conv =
      match Conversation.of_messages msgs with
      | Ok c -> c
      | Error err ->
          failwith ("run_session: malformed input messages — " ^ err)
    in
    Context.empty
    |> Context.with_tools tools
    |> Context.with_conversation conv
  in
  let final_messages ctx =
    Conversation.to_messages (Context.conversation ctx)
  in
  try
    match run_loop ?max_iterations ~ctx:(ctx_of_messages messages) () with
    | Ok (answer, ctx) ->
        Outcome_done { answer; final_messages = final_messages ctx }
    | Error (reason, ctx) ->
        Outcome_failed { reason; messages = final_messages ctx }
  with Wait_for_user { tool_use_id; question; ctx_so_far } ->
    Outcome_waiting
      {
        tool_use_id;
        question;
        messages = final_messages ctx_so_far;
      }
  (* [Governor.Governor_aborted] escapes through to the top-level
     invoker, which catches via [Protection.catch_protection_errors]. *)

let default_system_prompt =
  "You are a helpful assistant. Use the available tools when they help \
   answer the user's question. Think step-by-step and only call tools when \
   they are needed."

