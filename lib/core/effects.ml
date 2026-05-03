(** Effect declarations.

    The agent code performs these effects without knowing how they are
    handled. Different handlers (production / mock / replay / cost-tracking)
    give the same agent code completely different runtime behavior. *)

open Types

type path_kind = [ `File | `Dir | `Missing ]

type _ Effect.t +=
  | Llm_complete : llm_call_args -> llm_response Effect.t
      (** Call the LLM with messages + tools, optionally overriding the
          system prompt and forcing a specific [tool_choice]. *)
  | Tool_calls :
      tool_def list * (Id.Tool_use_id.t * string * Yojson.Safe.t) list
      -> (Id.Tool_use_id.t * tool_handler_result) list Effect.t
      (** Execute one or more tools and return their results in input
          order. The first component is the tool registry the
          dispatcher must look names up in — this comes from the
          caller (typically [Context.tools ctx]) so the same
          [Agent_spec] decides BOTH what the model sees and what
          actually executes. Each tuple is [(tool_use_id, name,
          input)]; output is [(tool_use_id, result)]. The production
          handler dispatches sequentially regardless of batch size —
          OCaml 5 effect handlers don't propagate to threads, so a
          worker-thread fan-out path crashes any tool that performs
          File_*/Time/Log. Domain-based parallelism is a future
          drop-in (see [Parallel_subagent]). *)
  | Log : string -> unit Effect.t
      (** Emit a log line. Handler decides where it goes. *)
  | Event_log : Event.t -> unit Effect.t
      (** Emit a structured [Event.t]. Observers can branch on the
          variant. The default handler in [Log_handler] forwards a
          rendered string via [to_log_line] so the line still lands in
          the log sink even with no structured observer installed. *)
  | Time_now : float Effect.t
      (** Current Unix timestamp (seconds since epoch). Mockable for
          deterministic tests / Checkpoint replay. *)
  | File_read : string -> (string, string) result Effect.t
      (** Read the entire contents of a regular file. Path must be
          absolute (validation is the caller's job). *)
  | File_write : { path : string; content : string }
      -> (int, string) result Effect.t
      (** Create-or-overwrite a regular file with [content]. Returns
          bytes written. Production handler [mkdir -p]s the parent. *)
  | File_list_dir : string -> (string list, string) result Effect.t
      (** Sorted directory entries (basenames). *)
  | File_stat : string -> path_kind Effect.t
      (** Classify a path: file / dir / missing. Never raises. *)
  | Pause : {
      tool_use_id : Id.Tool_use_id.t;
      question : string;
      ctx_so_far : Context.t;
    } -> message Effect.t
      (** Suspend the agent loop. Performed by [Step.once] when the
          model calls a tool with [Pause] capability. The handler
          decides resumption:

          - The default handler ([Agent.execute] installs it) does
            NOT resume — it abandons the continuation and returns
            [Agent.Waiting] up the stack. Used by chat sessions
            that must persist + ask the user.
          - A combinator wrapper can install a handler that resumes
            with a synthetic [Tool_result] message (auto-answer
            mode for tests / dry-run / batch mode).

          The return value, when resumed, is the [message] to splice
          back as the user's reply (typically a User turn carrying
          a [Tool_result] block answering [tool_use_id]). *)
  | Terminal : {
      tool_name : string;
      payload : Yojson.Safe.t;
      ctx_so_far : Context.t;
    } -> 'a Effect.t
      (** End the agent loop with a structured payload from a tool
          call (the model invoked one of the spec's [terminal] tools).
          Polymorphic return: there is no resume path — handlers
          abandon the continuation and produce an [Agent.output]
          directly. Used by [Specs.executor] / [Specs.planner] /
          [Specs.recovery] flows that terminate on a structured
          submission rather than free text. *)
  | Get_budget_progress : budget_progress Effect.t
      (** Snapshot current cost / wall-time vs configured caps.
          Handled by [Governor.install] which has the [cost_state] and
          [Limits.t] in scope. Returns zeros when no Governor is
          installed (e.g. in unit tests bypassing the runtime). *)

(** Best-effort [Event_log] emit: silently drops if no Log_handler is
    installed (typical in unit tests). Use this from observational
    bookkeeping where the sender shouldn't crash if nobody is
    listening. *)
let safe_event_log ev =
  try Effect.perform (Event_log ev) with Effect.Unhandled _ -> ()
