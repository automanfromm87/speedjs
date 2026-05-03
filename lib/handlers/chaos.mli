(** Chaos-engineering middleware: probabilistic failure injection on
    [Llm_handler] / [Tool_handler] chains.

    Goal: exercise the resilience layers (LLM retry, tool retry,
    plan-act recovery, workflow [recover]) by triggering real failure
    paths without relying on the upstream LLM / tools to misbehave.
    Run a normal agent task with [llm_failure_rate=0.05] and you'll
    see [Llm_handler.with_retry] / [Plan_act.handle_failure_flow] /
    [Planner.recover] all engage.

    Determinism: a single [Random.State] is seeded from [config.seed]
    and threaded across all injection sites — same seed + same agent
    inputs produce the same chaos sequence. *)

open Types

type llm_kind =
  [ `Rate_limit | `Transient | `Auth | `Context_window | `Overloaded ]

type tool_kind = [ `Transient of string | `Permanent of string ]

type config = {
  seed : int;
      (** RNG seed. Same seed + same inputs → reproducible chaos sequence. *)
  llm_failure_rate : float;
      (** Probability per LLM call of injecting a failure (0.0 – 1.0). *)
  llm_failure_kinds : llm_kind list;
      (** Pool of LLM failure types to pick from when injecting. Empty
          list = use all kinds. *)
  tool_failure_rate : float;
      (** Probability per tool dispatch of injecting a failure. *)
  tool_failure_kinds : tool_kind list;
      (** Pool of tool failure types. Empty = use both Transient and
          Permanent. *)
  on_inject : kind:string -> detail:string -> unit;
      (** Called whenever a chaos failure is injected. Default emits
          a tagged [Effects.Log] line so traces and the standard log
          show "[chaos] LLM rate_limit injected" etc. *)
}

(** All-zero rates, default seed 42, on_inject logs via [Effects.Log]. *)
val default : config

(** Convenience: same probability injected on each surface, all kinds
    enabled. *)
val uniform : ?seed:int -> rate:float -> unit -> config

(** Wrap an LLM handler chain. Inserted typically OUTSIDE
    [with_retry] so injected failures bypass per-call retry and
    propagate up to workflow / plan-act recovery. *)
val with_llm : config -> Llm_handler.t -> Llm_handler.t

(** Wrap a tool dispatch chain. Errors are surfaced as [Error.t]
    (Transient or Permanent) so per-tool [with_retry] / circuit
    breaker / plan-act submit_task_result paths can react. *)
val with_tool : config -> Tool_handler.t -> Tool_handler.t

(** Whether any surface has a non-zero rate. Used to skip wiring
    entirely on the default config. *)
val is_active : config -> bool

(** [show config] — one-line summary suitable for the active-flags
    banner. Returns "" when [is_active] is false. *)
val show : config -> string
