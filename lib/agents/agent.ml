(** The ReAct agent loop, written in pure effect-perform style.

    Knows nothing about HTTP, LLM APIs, or tool execution — it just
    performs effects. The handler installed at runtime decides
    everything. See [agent.mli] for the public surface. *)

open Types

let default_max_iterations = Agent_spec.default_max_iters
let default_system_prompt = Agent_spec.default_system_prompt

(** Core ReAct loop. Drives [Step.once] up to [max_iterations] times,
    handling endgame [tool_choice] forcing and per-iteration logging /
    Governor ticks.

    Pause / Terminal-tool surface through {!Effects.Pause} /
    {!Effects.Terminal} effects performed inside [Step.once]; this
    loop never sees those branches. The handler is installed by
    [execute] (one level up). *)
let run_loop ?(max_iterations = default_max_iterations)
    ?(terminal_tools : string list = [])
    ?(force_terminal_in_last_n = 2)
    ?(strategy = Context.Strategy.flat) ?(name = "agent")
    ?(model : string option = None)
    ?(purpose : llm_purpose = `Other)
    ~(ctx : Context.t) () :
    (string * Context.t, agent_error * Context.t) Result.t =
  let endgame_iter_threshold =
    if force_terminal_in_last_n <= 0 then max_iterations + 1  (* never trip *)
    else max (max_iterations - force_terminal_in_last_n) 1
  in
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
            Step.once ~strategy ~tool_choice ~terminal_tools ~model
              ~purpose ~ctx ())
      in
      match outcome with
      | Step.Continue ctx -> loop ctx (iter + 1)
      | Step.Terminal_text { answer; ctx } -> Ok (answer, ctx)
      | Step.Failed { reason; ctx } -> Error (reason, ctx)
    end
  in
  loop ctx 1

(* ===== Unified entry point ===== *)

type input =
  | Fresh of string
  | Resume of message list

type output =
  | Done of { answer : string; messages : message list }
  | Terminal_tool of {
      name : string;
      payload : Yojson.Safe.t;
      messages : message list;
    }
  | Waiting of {
      tool_use_id : Id.Tool_use_id.t;
      question : string;
      messages : message list;
    }
  | Failed of { reason : agent_error; messages : message list }

(** Build a [Conversation] from the input. [Resume msgs] is rejected
    if the trailing turn is a dangling tool_use — the caller must
    close it (with a [Tool_result] for the pending tool_use_id, or
    {!Conversation.close_dangling_with_ack}) before calling execute.
    This avoids silently ack-closing user-facing pauses (ask_user). *)
let conversation_of_input (input : input) :
    (Conversation.t, string) Result.t =
  match input with
  | Fresh q -> Ok (Conversation.push_user_text Conversation.empty q)
  | Resume msgs ->
      (match Conversation.of_messages msgs with
       | Error _ as e -> e
       | Ok conv when Conversation.is_dangling conv ->
           let names =
             String.concat "," (Conversation.dangling_tool_use_names conv)
           in
           Error
             (Printf.sprintf
                "Resume: prior conversation has dangling tool_use(s) \
                 [%s]; close them (Tool_result block, or \
                 Conversation.close_dangling_with_ack) before calling \
                 execute"
                names)
       | Ok conv -> Ok conv)

let input_messages_for_diagnostic = function
  | Fresh _ -> []
  | Resume m -> m

let terminal_tools_of_spec (spec : Agent_spec.t) : string list =
  match spec.terminal with
  | Agent_spec.Free_text -> []
  | Agent_spec.Tool { name } -> [ name ]

(** No-handler execute: performs Pause/Terminal effects through to
    the outer fiber. Caller installs the handler. *)
let execute_raw ~(spec : Agent_spec.validated) ~(input : input) : output =
  let inner = spec.spec in
  let filtered_tools = spec.visible_tools in
  let terminal_tools = terminal_tools_of_spec inner in
  match conversation_of_input input with
  | Error msg ->
      Failed
        {
          reason =
            Plan_invalid
              ("Agent.execute: malformed input messages — " ^ msg);
          messages = input_messages_for_diagnostic input;
        }
  | Ok conv ->
      let ctx =
        let base =
          Context.empty
          |> Context.with_tools filtered_tools
          |> Context.apply_system ~system_prompt:inner.system_prompt
               ~system_blocks:inner.system_blocks
          |> Context.with_conversation conv
        in
        List.fold_left
          (fun c (tag, body) -> Context.with_env ~tag ~body c)
          base inner.env
      in
      let final_messages c =
        Conversation.to_messages (Context.conversation c)
      in
      (match
         run_loop ~max_iterations:inner.max_iters
           ~strategy:inner.strategy ~terminal_tools
           ~force_terminal_in_last_n:inner.force_terminal_in_last_n
           ~name:inner.name ~model:inner.model
           ~purpose:inner.purpose ~ctx ()
       with
       | Ok (answer, ctx) -> Done { answer; messages = final_messages ctx }
       | Error (reason, ctx) -> Failed { reason; messages = final_messages ctx })

(** Default-handler execute: wraps [execute_raw] with the pause →
    Waiting / terminal → Terminal_tool translator AND converts
    [Llm_error.Llm_api_error] exceptions into [Failed] outputs. The
    exception conversion is what lets workflow / plan-act recovery
    layers see LLM API failures as data ([Result.Error]) — without
    it, the exception flies past every workflow combinator
    ([with_retry] / [recover] / etc. only catch [Result.Error], not
    exceptions) all the way to [Protection.catch_protection_errors]
    and kills the whole run. [Governor.Governor_aborted] is a
    global abort signal and is intentionally NOT caught here — it
    propagates to Protection so the run terminates cleanly. *)
let execute ~(spec : Agent_spec.validated) ~(input : input) : output =
  let final_messages c =
    Conversation.to_messages (Context.conversation c)
  in
  try
    Effect.Deep.try_with
      (fun () -> execute_raw ~spec ~input)
      ()
      {
        effc =
          (fun (type a) (eff : a Effect.t) ->
            match eff with
            | Effects.Pause { tool_use_id; question; ctx_so_far } ->
                Some
                  (fun (_k : (a, _) Effect.Deep.continuation) ->
                    Waiting
                      {
                        tool_use_id;
                        question;
                        messages = final_messages ctx_so_far;
                      })
            | Effects.Terminal { tool_name; payload; ctx_so_far } ->
                Some
                  (fun (_k : (a, _) Effect.Deep.continuation) ->
                    Terminal_tool
                      {
                        name = tool_name;
                        payload;
                        messages = final_messages ctx_so_far;
                      })
            | _ -> None);
      }
  with Llm_error.Llm_api_error e ->
    Failed
      {
        reason = Llm_call_failed (Llm_error.pp e);
        messages = input_messages_for_diagnostic input;
      }

(* ===== Output helpers ===== *)

let expect_done ~name (out : output) :
    (string * message list, agent_error) Result.t =
  match out with
  | Done { answer; messages } -> Ok (answer, messages)
  | Failed { reason; _ } -> Error reason
  | Terminal_tool { name = tool_name; _ } ->
      Error
        (Plan_invalid
           (Printf.sprintf
              "%s: expected free-text answer but model called terminal \
               tool %S"
              name tool_name))
  | Waiting { question; _ } ->
      Error
        (Plan_invalid
           (Printf.sprintf
              "%s: model unexpectedly called ask_user (%s) — chat-shaped \
               flow only"
              name question))

