(** Workflow algebra: leaf agents are validated [Agent_spec]s, and
    combinators compose them into orchestrations.

    A [Workflow.t] is a Reader-Result thunk. [run] materializes it
    against the currently-installed runtime stack:
    [run flow : ('a, agent_error) result]. Each leaf calls
    [Agent.execute] (which itself uses {!Effects.Pause} /
    {!Effects.Terminal}); combinators bind, retry, recover, fan out
    via [List.map] / [Foreach] / [Parallel].

    Failure model: any leaf returning [Agent.Failed] short-circuits
    to [Result.Error]. Output projections ({!expect_done},
    {!expect_terminal_tool}) tighten the contract — anything that
    isn't the expected shape becomes a typed [Plan_invalid] error.
    {!recover} catches and substitutes a replacement flow.

    Typical usage (from [Plan_act]'s actual driver):
    {[
      let task_flow ~spec ~prior_messages task_description =
        let open Workflow in
        let* (payload, _) =
          leaf spec (Agent.Fresh task_description)
          |> expect_terminal_tool ~name:Specs.submit_task_result_name
        in
        of_result (parse_task_submit payload)

      let drive plan tasks =
        let open Workflow in
        let* results = foreach tasks (fun t ->
          task_flow ~spec ~prior_messages:[] t.description
          |> with_retry ~max_attempts:2
          |> recover (fun _ -> pure default_outcome)) in
        pure results
    ]}

    Real plan-act orchestration (with retry / recovery / pivot of
    pending list on Replan/Split) lives in {!Plan_act} and is
    expressed as a recursive workflow plus state record. *)

open Types

(** Opaque workflow value. *)
type 'a t

(** {1 Constructors} *)

val pure : 'a -> 'a t

(** Lift a pre-existing [result] into a flow. Useful for splicing
    parsers / decoders that produce [(_, agent_error) result] into a
    flow chain. *)
val of_result : ('a, agent_error) Result.t -> 'a t

(** Lift a thunk producing a [result] into a flow (deferred
    evaluation). *)
val of_thunk : (unit -> ('a, agent_error) Result.t) -> 'a t

(** Run a single agent. Returns the raw [Agent.output] without
    inspection; combine with {!expect_done} or
    {!expect_terminal_tool} to get a typed value. *)
val leaf : Agent_spec.validated -> Agent.input -> Agent.output t

(** Pure transformation. *)
val map : 'a t -> ('a -> 'b) -> 'b t

(** Sequential composition. Short-circuits on error. *)
val bind : 'a t -> ('a -> 'b t) -> 'b t

(** {1 Output projections} *)

(** Expect [Agent.Done]. Returns answer + final messages. Anything
    else becomes [Plan_invalid] / propagated [Failed.reason]. [name]
    is included in diagnostics ("planner: …", "executor: …"); defaults
    to ["agent"]. *)
val expect_done :
  ?name:string ->
  Agent.output t ->
  (string * message list) t

(** Expect [Agent.Terminal_tool { name; ... }]. Returns payload + final
    messages. Anything else becomes a typed error. *)
val expect_terminal_tool :
  name:string ->
  ?label:string ->
  Agent.output t ->
  (Yojson.Safe.t * message list) t

(** {1 Combinators} *)

(** Retry on [Failed] up to [max_attempts] times (default 3). *)
val with_retry : ?max_attempts:int -> 'a t -> 'a t

(** Catch a typed error and substitute a replacement flow. The
    replacement runs against the current runtime stack with no
    inherited state from the failed body. *)
val recover : 'a t -> (agent_error -> 'a t) -> 'a t

(** Reify the result: any inner error becomes [Ok (Error e)], any
    inner success becomes [Ok (Ok v)]. Lets callers branch on
    success / failure with full pattern matching, instead of being
    forced to short-circuit on error. Used by orchestrators (plan-act
    drive) where failure triggers different control flow than retry. *)
val attempt : 'a t -> ('a, agent_error) Result.t t

(** Imperative side-effect lifted into a flow. Useful for
    [Memory.persist] / [Plan_state.save] / [Effects.Event_log] calls
    between binds without leaving the workflow expression. *)
val action : (unit -> unit) -> unit t

(** Sequential fold: run [body] on each item, collect results in
    order. Short-circuits on the first error. For "collect partial
    successes" semantics, wrap [body] in [recover] before passing. *)
val foreach : 'a list -> ('a -> 'b t) -> 'b list t

(** Run flows in parallel. Returns [Ok results] if every branch
    succeeded, [Error _] (the first error) otherwise.

    Each branch runs in its own Domain with a freshly-installed
    handler stack — OCaml 5 effects don't cross Domain boundaries.
    [Runtime.install] wires this automatically: every call to
    [Runtime.install] sets a branch wrapper that re-installs the
    runtime against a forked child config (see
    [Runtime.fork_child_config]). Tests bypassing [Runtime.install]
    fall back to the identity wrapper, which works fine for mock
    handlers installed on the parent fiber. *)
val parallel : 'a t list -> 'a list t

(** Override the per-branch handler-stack wrapper. Normally not
    needed — [Runtime.install] sets it automatically. Use this only
    for custom child-stack semantics (e.g. per-branch log prefixes,
    as [parallel_delegate] does). The rank-2 polymorphic record type
    is bridged via [Obj.magic] at this single boundary. *)
val set_branch_wrapper : ((unit -> 'a) -> 'a) -> unit

(** {1 Syntax sugar} *)

val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

(** {1 Interpreter} *)

(** Materialize the workflow against the currently-installed runtime
    stack. *)
val run : 'a t -> ('a, agent_error) result

(** Returns ["<workflow>"]. Workflows are opaque thunks; full
    structural pretty-printing isn't supported. Read the workflow
    expression in the source for review. *)
val show : 'a t -> string
