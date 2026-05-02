(** The ReAct agent loop, written in pure effect-perform style.

    Knows nothing about HTTP, LLM APIs, or tool execution — it just performs
    effects. The handler installed at runtime decides everything. *)

open Types

(** Default iteration cap per agent invocation. helix uses 100; we match.
    Lower values (e.g. 10 — the old hardcoded value) starve any task that
    needs a build → fix → rebuild loop. *)
let default_max_iterations = 100

(** Raised when the LLM calls the [ask_user] pause-tool. The agent loop
    halts mid-conversation; [run_session] catches this and returns
    [Outcome_waiting]. *)
exception Wait_for_user of {
  tool_use_id : Id.Tool_use_id.t;
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

(** Core ReAct loop. Drives [Step.once] up to [max_iterations] times,
    handling endgame [tool_choice] forcing and per-iteration logging /
    Governor ticks. Returns [Ok (answer, final_ctx)] or
    [Error (reason, partial_ctx)]. May raise [Wait_for_user] /
    [Task_terminal_called] for callers that want to suspend. *)
let run_loop ?(max_iterations = default_max_iterations)
    ?(terminal_tools : string list = [])
    ?(strategy = Context.Strategy.flat) ?(name = "agent")
    ~(ctx : Context.t) () :
    (string * Context.t, agent_error * Context.t) Result.t =
  let endgame_iter_threshold = max (max_iterations - 2) 1 in
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
      let capture (o : Step.result) : Trace.capture_result =
        let label, ok =
          match o with
          | Step.Continue _ -> ("continue", true)
          | Step.Terminal_text _ -> ("terminal_text", true)
          | Step.Terminal_tool { tool_name; _ } ->
              (Printf.sprintf "terminal_tool=%s" tool_name, true)
          | Step.Wait_for_user _ -> ("wait_for_user", true)
          | Step.Failed { reason; _ } ->
              (Printf.sprintf "failed: %s" (agent_error_pp reason), false)
        in
        {
          output = label;
          tokens = Trace.zero_tokens;
          cost_delta = 0.0;
          ok;
          error = None;
        }
      in
      let outcome =
        Trace.span_current ~kind:Trace.Iteration
          ~name:(Printf.sprintf "%s:iter:%d" name iter)
          ~input_summary:"" ~capture
          (fun () ->
            Step.once ~strategy ~tool_choice ~terminal_tools ~ctx ())
      in
      match outcome with
      | Step.Continue ctx -> loop ctx (iter + 1)
      | Step.Terminal_text { answer; ctx } -> Ok (answer, ctx)
      | Step.Terminal_tool { tool_name; input; ctx } ->
          raise
            (Task_terminal_called
               { tool_name; input; ctx_so_far = ctx })
      | Step.Wait_for_user { tool_use_id; question; ctx } ->
          raise
            (Wait_for_user
               { tool_use_id; question; ctx_so_far = ctx })
      | Step.Failed { reason; ctx } -> Error (reason, ctx)
    end
  in
  loop ctx 1

let default_system_prompt =
  "You are a helpful assistant. Use the available tools when they help \
   answer the user's question. Think step-by-step and only call tools when \
   they are needed."

(** Build a [Context] for a single user-query / tools combination. *)
let make_ctx ?(system_prompt = default_system_prompt) ?system_blocks
    ~user_query ~tools () =
  Context.empty
  |> Context.with_tools tools
  |> Context.apply_system ~system_prompt ?system_blocks
  |> Context.push_user_text user_query

(** One-shot entry point. Returns just the final answer string. *)
let run ?max_iterations ?system_prompt ?system_blocks ~user_query ~tools
    () : agent_result =
  let ctx = make_ctx ?system_prompt ?system_blocks ~user_query ~tools () in
  match run_loop ?max_iterations ~ctx () with
  | Ok (answer, _) -> Ok answer
  | Error (e, _) -> Error e

(** Run a ReAct loop until a specific terminal tool is called and
    return its parsed JSON input. The terminal tool's [handler] is
    never invoked — the loop intercepts via [terminal_tools] and
    raises [Task_terminal_called]. Used by orchestrators (planner,
    recovery, plan_act executor) that need a structured submit
    payload, not a free-form text answer.

    Outcomes:
    - terminal tool called → [Ok input]
    - End_turn / Max_iterations / Failed without terminal call →
      [Error agent_error]
    - [Wait_for_user] propagates as the [Wait_for_user] exception
      (caller decides how to suspend / resume). *)
let run_until_terminal_tool ?(max_iterations = default_max_iterations)
    ?(system_prompt = default_system_prompt)
    ?(system_blocks : (string * string) list = [])
    ?(name = "agent") ~terminal_tool_name ~user_query ~tools () :
    (Yojson.Safe.t, agent_error) Result.t =
  let ctx = make_ctx ~system_prompt ~system_blocks ~user_query ~tools () in
  try
    match
      run_loop ~max_iterations ~terminal_tools:[ terminal_tool_name ]
        ~name ~ctx ()
    with
    | Ok (_text, _ctx) ->
        Error
          (Plan_invalid
             (Printf.sprintf
                "%s ended turn without calling %s — model lost the \
                 protocol"
                name terminal_tool_name))
    | Error (e, _) -> Error e
  with Task_terminal_called { input; _ } -> Ok input

(** Multi-turn entry point. Seeds with [messages], runs the loop, and
    returns an [session_result] carrying the updated history (including
    any pending ask_user tool_use). *)
let run_session ?max_iterations ?system_prompt ?system_blocks ~messages
    ~tools () : session_result =
  let final_messages ctx =
    Conversation.to_messages (Context.conversation ctx)
  in
  (* A persisted session file (or memory blob) can be corrupt — strict
     alternation broken, dangling tool_uses without matching results,
     etc. Return [Outcome_failed] with a typed [Plan_invalid] instead
     of [failwith]'ing — callers can recover (delete + restart) rather
     than the whole process crashing. *)
  match Conversation.of_messages messages with
  | Error err ->
      Outcome_failed
        {
          reason =
            Plan_invalid
              ("run_session: malformed input messages — " ^ err);
          messages;
        }
  | Ok conv ->
      let ctx =
        Context.empty
        |> Context.with_tools tools
        |> Context.apply_system
             ~system_prompt:
               (Option.value system_prompt
                  ~default:default_system_prompt)
             ?system_blocks
        |> Context.with_conversation conv
      in
      (try
         match run_loop ?max_iterations ~ctx () with
         | Ok (answer, ctx) ->
             Outcome_done
               { answer; final_messages = final_messages ctx }
         | Error (reason, ctx) ->
             Outcome_failed { reason; messages = final_messages ctx }
       with Wait_for_user { tool_use_id; question; ctx_so_far } ->
         Outcome_waiting
           {
             tool_use_id;
             question;
             messages = final_messages ctx_so_far;
           })
  (* [Governor.Governor_aborted] escapes through to the top-level
     invoker, which catches via [Protection.catch_protection_errors]. *)
