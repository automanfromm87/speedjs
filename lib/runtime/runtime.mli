(** Wire the effect-handler stack and run a thunk inside it. *)

open Types

type config = {
  model : string;
  cost : cost_state;
  on_log : string -> unit;
  on_text_delta : string -> unit;
  governor_limits : Governor.Limits.t;
  llm_max_retries : int;
  tape_path : string option;
      (** When [Some path], LLM responses and tool batches are recorded
          to JSONL for replay/resume. *)
  crash_after : int option;
      (** Demo / testing only — abort after N live LLM calls. *)
  emit_governor_events_to_log : bool;
      (** Stream Governor ticks through [on_log] (the ["[gov] ..."] lines). *)
}

(** Compose the effect handler stack and run [thunk] inside it. Returns
    whatever [thunk] returns. *)
val install : tools:tool_def list -> config:config -> (unit -> 'a) -> 'a
