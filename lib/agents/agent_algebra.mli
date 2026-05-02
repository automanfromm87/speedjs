(** Agent algebra — agents as composable values.

    An [Agent_algebra.t] is a NODE in an inspectable tree, not a function.
    You build it with combinators ([with_retry], [with_skill], [pipe],
    [replicate], ...) and then [execute] it. The same value can be
    rendered with [show] for offline inspection or run multiple times
    with different inputs.

    Why an ADT instead of `string -> agent_result` closures:
    - [show] renders the structure for review before running ($$$)
    - Sub-Governors / sub-Tracers can be installed at the right level
      based on the tree shape (not possible from inside a closure)
    - Future: rewrites and optimization passes (e.g. fold consecutive
      [with_max_iters] to the tighter cap)

    Reuses the existing [Runtime] handler stack as the leaf — every
    [execute] runs inside whatever runtime is currently installed
    (so cost / tape / sandbox / governor / trace are inherited).

    Combinators are PURE — building an agent has no side effects.
    Only [execute] reads / writes shared state. *)

open Types

(** Opaque agent value. *)
type t

(** {1 Constructors} *)

val base :
  ?tools:tool_def list ->
  ?system_prompt:string ->
  ?system_blocks:(string * string) list ->
  ?max_iters:int ->
  unit ->
  t
(** A leaf agent — one [Agent.run] invocation. Tools default to the
    standard CLI tool set; system_prompt defaults to
    [Agent.default_system_prompt]; max_iters defaults to
    [Agent.default_max_iterations]. *)

(** {1 Combinators} *)

val with_retry : ?max_attempts:int -> t -> t
(** Wrap with retry: re-run on [Error _] up to [max_attempts] times.
    Default 3. Successive attempts share no state — same input each time. *)

val with_max_iters : int -> t -> t
(** Override the inner [max_iterations] cap of a [base] agent. Other
    combinators pass through. *)

val with_skill : string -> t -> t
(** Inject a skill body into the agent's [system_blocks] before running.
    Looks the skill up by name from the skills directory configured at
    runtime; raises if not found. The skill index doesn't get re-rendered
    so the agent doesn't see [<available_skills>] — it sees ONLY this
    skill's body. Use to pin a specific skill instead of letting the
    agent decide which to load. *)

val replicate : int -> t -> t
(** Spawn [n] independent runs of the agent on the SAME input, one per
    OCaml [Domain]. Output is the concatenation of results, ordered by
    spawn index. Use for ensemble sampling or speculative execution.
    Children inherit the parent governor and share the parent
    cost_state (same semantics as [parallel_delegate]). *)

val pipe : t -> t -> t
(** [pipe a b]: feed [a]'s output as [b]'s input. If [a] errors, [b] is
    not run and the error propagates. *)

(** {1 Execution + inspection} *)

val execute : t -> string -> agent_result
(** Run the agent on [input]. Reads the currently-installed [Runtime]
    handlers; install one with [Runtime.install] in the caller. *)

val with_skills : Skill.t list -> (unit -> 'a) -> 'a
(** Provide the list of available skills to the inner thunk. [with_skill]
    inside [execute] looks up by name from this list. Wrap your top-level
    [execute] call in [with_skills loaded_skills (fun () -> ...)] when the
    agent uses [with_skill]. Restored on return. *)

val show : t -> string
(** Render the agent tree as human-readable text. Use to verify what
    will run before paying for it. *)
