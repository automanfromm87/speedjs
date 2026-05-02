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
(** A leaf agent — one [Agent.run] invocation.

    [?tools] defaults to the EMPTY list. The library does not pull
    in a "standard tool set" implicitly because the algebra is
    library code; [bin/main.ml] / [bin/algebra_demo.ml] / your own
    CLI is responsible for picking the tool surface. Pass
    [Tools.all] (or the result of [Setup.build_tools]) explicitly
    when you want them.

    [?system_prompt] / [?system_blocks] / [?max_iters] fall through
    to [Agent.run]'s defaults when omitted. *)

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
(** Run [n] copies of the agent on the SAME input. Output is the
    concatenation of results, ordered by index.

    {b Implementation status (v0):} SEQUENTIAL — runs are executed
    one after another in the same Domain. The contract still says
    "n independent runs"; v1 will switch this to a Domain-based
    fan-out using [Setup.build_child_stack]'s shared cost_state +
    forked tracer wiring (same as [parallel_delegate]). The change
    will be observable in wall time and trace structure but not in
    the value type. Until then, [replicate] is mostly useful for
    {e ensemble sampling under retry} (e.g. paired with
    [with_retry]), not for parallel speedup. *)

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
