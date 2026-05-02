(** Sub-agent: spawn a focused agent with its own message history.

    Implementation approach: NO new effect needed. The [delegate] tool's
    handler simply calls [Agent.run] internally — this gives us:

    - Isolated message history (the sub-agent's [Agent.run] starts fresh)
    - Shared LLM provider (sub-agent's [Effects.Llm_complete] propagates up
      to the parent's production handler — same API key, same cost tracking)
    - Shared tools dispatch (sub-agent's [Effects.Tool_call] also propagates,
      but uses the [tools_for_subagent] list passed to the sub-run, NOT
      the parent's full list)
    - Shared protection layer (parent's LoopGuard sees sub-agent calls too,
      which is usually what we want for "stuck the model" detection)

    Cost: sub-agent calls count against the parent's budget — by design.
    The parent's [Governor] sees [Subagent_entered/Exited] ticks and
    enforces [max_subagent_depth] at the boundary.

    Tool list for sub-agent typically = parent's tools MINUS [delegate]
    itself, to prevent unbounded recursion. The [tools_for_subagent]
    argument is closed over at construction time. *)

open Types

let delegate_name = "delegate"

let make_delegate_tool ~(tools_for_subagent : tool_def list) : tool_def =
  {
    (* Sub-agent dispatch — not idempotent (the sub-agent has side
       effects). Long-running by nature. *)
    idempotent = false;
    timeout_sec = None;
    category = "meta";
    capabilities = [ Meta ];
    (* Executor + Subagent — chained delegation is allowed up to the
       depth cap enforced by the parent's Governor. *)
    allowed_modes = [ Executor; Subagent ];
    classify_error = default_classify_error;
    name = delegate_name;
    description =
      "Spawn a focused sub-agent to handle a specific subtask. The \
       sub-agent has its OWN fresh message history (does NOT see this \
       conversation), runs its own ReAct loop, and returns only its final \
       answer string. Use this when you need to do isolated research or \
       multi-step work that shouldn't pollute the main conversation. The \
       sub-agent has access to all tools EXCEPT delegate itself.";
    input_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "task",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Self-contained task description for the \
                           sub-agent (no prior conversation context)" );
                    ] );
              ] );
          ("required", `List [ `String "task" ]);
        ];
    handler =
      (fun input ->
        match input with
        | `Assoc fields -> (
            match List.assoc_opt "task" fields with
            | Some (`String task) when task <> "" ->
                (* [Tool_handler.install] re-installs its handler stack
                   around each tool dispatch, so any effects we perform
                   here — Log from our notice, plus the sub-agent's
                   Llm/Tool/Log effects — propagate to the parent's
                   stack. No local silencing needed. *)
                Effect.perform
                  (Effects.Log
                     (Printf.sprintf "  [sub-agent] task: %s"
                        (if String.length task > 100 then
                           String.sub task 0 100 ^ "..."
                         else task)));
                (* Best-effort Governor tick: tests / dev runs without an
                   installed Governor will see [Effect.Unhandled] from
                   [perform] — swallow it so unsandboxed callers still
                   work. The production runtime always installs a
                   Governor at the outermost layer (see [bin/setup.ml]). *)
                let safe_tick ev =
                  try Effect.perform (Governor.Tick ev) with _ -> ()
                in
                safe_tick Subagent_entered;
                let capture (r : (string, agent_error) result) :
                    Trace.capture_result =
                  match r with
                  | Ok answer ->
                      Trace.ok_capture ~output:answer
                        ~tokens:Trace.zero_tokens ~cost_delta:0.0
                  | Error e ->
                      {
                        output = "";
                        tokens = Trace.zero_tokens;
                        cost_delta = 0.0;
                        ok = false;
                        error = Some (agent_error_pp e);
                      }
                in
                let result =
                  Fun.protect
                    ~finally:(fun () -> safe_tick Subagent_exited)
                    (fun () ->
                      Trace.span_current ~kind:Trace.Agent_spawn
                        ~name:"delegate" ~input_summary:task ~capture
                        (fun () ->
                          Agent.run ~user_query:task
                            ~tools:tools_for_subagent ()))
                in
                Effect.perform
                  (Effects.Log "  [sub-agent] returning to parent");
                Result.map_error agent_error_pp result
            | _ -> Error "missing or empty 'task' field")
        | _ -> Error "input must be JSON object");
  }

(** Convenience: build the standard tool list with delegate added. The
    sub-agent gets the [base] list (without delegate); the parent gets
    [base @ [delegate]]. *)
let with_delegate (base : tool_def list) : tool_def list =
  let delegate = make_delegate_tool ~tools_for_subagent:base in
  base @ [ delegate ]
