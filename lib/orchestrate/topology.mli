(** Pluggable agent topology executor.

    A [shape] describes how user-defined nodes (each: ['state -> 'state])
    compose: sequence, parallel, loop, conditional branch. Topology runs
    via the [Topo_run] effect, so middleware can intercept every
    sub-step — logging, retry, replay, tracing, step-debugger — without
    touching the core executor.

    Same composition pattern as [Llm_handler] / [Tool_handler] /
    [File_handler]: a [type t] runner record + [direct] default + [|>]
    middleware + [install] effect bridge.

    State is polymorphic via first-class polymorphism: each topology
    run pins one ['state] type; the runner record threads it
    parametrically. *)

(** A pure transformation of user-defined state. *)
type 'state node = 'state -> 'state

(** Topology shapes. Compose freely — every constructor recurses, and
    the executor performs [Topo_run] for each sub-step so middleware
    sees every level. *)
type 'state shape =
  | Node of { name : string; run : 'state node }
      (** Atomic step: just call [run]. *)
  | Sequence of 'state shape list
      (** Run each shape in order, threading state. *)
  | Parallel of {
      merge : 'state -> 'state -> 'state;
      branches : 'state shape list;
    }
      (** Run all branches over the same starting state, then [merge]
          their results pairwise into the original state.

          MVP: synchronous fold. Domain-based real parallelism is a
          drop-in replacement of the [Parallel] handler (see
          [Parallel_subagent.run]) — kept synchronous for now to ship
          the core abstraction first. *)
  | Loop_until of {
      cond : 'state -> bool;
      body : 'state shape;
      max_iters : int option;
    }
      (** Repeat [body] until [cond] is true, or [max_iters] reached. *)
  | Branch of ('state -> 'state shape)
      (** Conditional routing: choose the next shape from current state. *)

(** [Topo_run (state, shape)] dispatches one shape under a state and
    returns the resulting state. Use [run] / [perform_topo_run] —
    direct [Effect.perform] is also fine if you've installed a runner. *)
type _ Effect.t += Topo_run : 'state * 'state shape -> 'state Effect.t

(** Effect-handler chain. Each runner is a polymorphic
    state-transformer: it works for any ['a] state type provided in
    the perform. Middleware composes via [|>]. *)
type runner = { run : 'a. 'a -> 'a shape -> 'a }

(** Default semantics: walk the shape, recursively performing
    [Topo_run] for every sub-step. This makes middleware visible at
    every level, not just the top call. *)
val direct : runner

(** Wrap a runner so [on_event] fires before every shape executes
    (entry) and once after (exit, with the resulting state's
    name-tag for grep-ability). *)
val with_logging :
  on_event:(string -> unit) -> runner -> runner

(** Wrap a runner so transient failures inside any shape retry up to
    [max_attempts] times (with exponential backoff capped at [cap]).
    A failure is any exception thrown out of the inner runner. *)
val with_retry :
  ?max_attempts:int ->
  ?base_delay:float ->
  ?cap:float ->
  runner ->
  runner

(** Install [chain] as the [Topo_run] handler. Every nested perform
    inside the thunk goes through [chain]. *)
val install : runner -> (unit -> 'a) -> 'a

(** User-facing entry: install the [direct] runner and dispatch
    [shape] over [state]. *)
val run : 'state shape -> 'state -> 'state

(** Same as [run] but with a custom runner chain. *)
val run_with : runner -> 'state shape -> 'state -> 'state

(** Helper: pretty name of a shape (for logging). *)
val shape_name : _ shape -> string
