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
  sandbox_root : string option;
      (** UX guardrail (NOT a security boundary). When set, layers
          [File_handler.with_sandbox ~root] on top of the production
          File handler so file ops with paths outside [root] are
          rejected with [Error]. The check is purely TEXTUAL prefix
          + [.]/[..] normalization — symlinks are NOT resolved, so
          a symlink anywhere under [root] pointing outside it WILL
          escape. Use to keep the agent from accidentally writing
          to [/etc] / [~/.bashrc] during dev runs; do NOT rely on
          it for adversarial isolation (use a container /
          chroot / namespace at the OS level for that). *)
  tracer : Trace.tracer;
      (** Sink for structured trace frames (one per LLM call / tool
          dispatch). Use [Trace.make_noop ()] to disable. *)
  on_event : Event.t -> unit;
      (** Observer for structured control-plane events. Default no-op
          ([fun _ -> ()]) — set to drive UI / telemetry / journal. *)
}

(** Compose the effect handler stack and run [thunk] inside it. Returns
    whatever [thunk] returns. *)
val install : tools:tool_def list -> config:config -> (unit -> 'a) -> 'a
