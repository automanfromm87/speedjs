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
      (Id.Tool_use_id.t * string * Yojson.Safe.t) list
      -> (Id.Tool_use_id.t * tool_handler_result) list Effect.t
      (** Execute one or more tools and return their results in input
          order. Each tuple is [(tool_use_id, name, input)]; output is
          [(tool_use_id, result)]. Single-tool case is just a 1-element
          list (handlers may dispatch sequentially); 2+ tools may run in
          parallel (production handler does this). *)
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
