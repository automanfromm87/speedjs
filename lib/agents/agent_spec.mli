(** Declarative description of a leaf agent's identity AND executable
    surface.

    A [t] answers "who is this agent?" — its mode (= tool-surface
    profile), prompt, tool list, strategy, iteration cap, and
    terminal protocol. The runtime is registry-agnostic; the
    dispatcher reads tools from [Tool_calls] effect payloads whose
    list comes from this spec, so a spec is a self-contained
    executable definition.

    Field-update helpers ([with_skill], [with_max_iters], ...)
    return a new spec with one field replaced. Composition lives in
    {!Workflow}. *)

open Types

(** How the loop terminates when the model decides it's done.
    - [Free_text]: end the run on [End_turn] with the final text answer.
    - [Tool name]: end the run when the model calls [name]; the tool's
      handler is NOT invoked, the input JSON is returned to the caller
      via [Agent.output.Terminal_tool]. Exactly one element of
      [terminal_tools] in the underlying loop. *)
val default_system_prompt : string
val default_max_iters : int

type terminal =
  | Free_text
  | Tool of { name : string }

(** {1 Two-phase types}

    [Agent_spec.t] is the construction-friendly intermediate type —
    chainable through [with_*] field updaters. [Agent_spec.validated]
    is what the agent loop consumes; the only way to get one is via
    {!validate}, which checks that:
    - [terminal = Tool { name }] resolves to a tool visible after
      [tools_for_mode mode tools] filtering
    - that tool declares [Terminal] capability

    Specs in {!Specs} return [validated] directly (their construction
    is known-valid). Modify-then-execute looks like:

    {[
      let s : Agent_spec.t =
        (Specs.executor ~tools () :> Agent_spec.t)
        |> Agent_spec.with_max_iters 50
      in
      match Agent_spec.validate s with
      | Ok v -> Agent.execute ~spec:v ~input
      | Error msg -> ...
    ]} *)

type t = {
  name : string;                              (** Telemetry tag — "planner" / "executor" / etc. *)
  mode : agent_mode;
      (** Tool-surface profile, not a role tag. See [Types.agent_mode]. *)
  system_prompt : string;
  system_blocks : (string * string) list;
  env : (string * string) list;
      (** Ambient context blocks ([(tag, body)]) rendered as
          [<tag>body</tag>] inside the system prompt at the [env]
          layer (workspace_brief, current_time, ...). Stable across
          calls so they ride at the prompt cache prefix. *)
  tools : tool_def list;
      (** Full tool surface BEFORE mode-filtering. [Agent.execute]
          applies [tools_for_mode mode] before installing. Callers
          shouldn't pre-filter by mode — the spec carries the
          authority. *)
  strategy : Context.Strategy.t;
  max_iters : int;
  model : string option;
      (** Per-spec model override. [None] inherits the runtime
          default; [Some "claude-..."] routes this leaf agent's LLM
          calls to a specific model. The whole handler stack (cost
          tracking, cache, retry, trace) still applies — only the
          target model changes. *)
  purpose : llm_purpose;
      (** Caller-intent tag set by [Specs.{planner,executor,recovery,
          chat,subagent}]. Plumbed through to [llm_call_args.purpose]
          on every LLM call this spec drives, so middleware can branch
          on role (e.g. per-purpose chaos rates). Default [`Other] for
          ad-hoc constructions. *)
  terminal : terminal;
  force_terminal_in_last_n : int;
      (** When [terminal = Tool { name }] and the loop reaches its
          last [force_terminal_in_last_n] iterations, the next LLM
          call sets [tool_choice = Tc_tool name] to push the model
          off [End_turn] / off non-terminal tools and onto the
          submit. Set to [0] to disable. Ignored when
          [terminal = Free_text]. Default 2 — enough headroom for
          the model to wrap up but tight enough to prevent a
          [Max_iterations_reached] without a submission. *)
}

(** Validated spec: passed to [Agent.execute]. Constructed only via
    {!validate}; carries the post-mode-filter tool list precomputed
    so [execute] doesn't refilter. The shape is [private] so external
    code can read fields ([v.spec.name], [v.visible_tools]) but not
    construct one outside this module. *)
type validated = private {
  spec : t;
  visible_tools : tool_def list;
}

(** Validate a [t]: checks terminal-tool visibility + capability. On
    success, returns [validated] carrying the precomputed filtered
    tool list. *)
val validate : t -> (validated, string) Result.t

(** Constructor with sensible defaults.

    [name] defaults to ["agent"]. [mode] defaults to [Executor].
    [terminal] defaults to [Free_text]. [strategy] defaults to
    [Context.Strategy.flat]. [max_iters] defaults to
    [Agent.default_max_iterations]. [system_blocks] / [env] default
    to empty. [system_prompt] defaults to a generic helpful-assistant
    prompt. *)
val make :
  ?name:string ->
  ?mode:agent_mode ->
  ?system_prompt:string ->
  ?system_blocks:(string * string) list ->
  ?env:(string * string) list ->
  ?strategy:Context.Strategy.t ->
  ?max_iters:int ->
  ?model:string ->
  ?purpose:llm_purpose ->
  ?terminal:terminal ->
  ?force_terminal_in_last_n:int ->
  tools:tool_def list ->
  unit ->
  t

(** {1 Field-update helpers}

    All return a new [t] with one field replaced. Pure. *)

val with_skill : Skill.t -> t -> t
(** Prepend the skill body as a system_block tagged with [skill.name].
    Order: most recently added skill appears FIRST in the rendered
    system prompt. *)

val with_max_iters : int -> t -> t
val with_mode : agent_mode -> t -> t
val with_model : string option -> t -> t
val with_terminal : terminal -> t -> t
val with_terminal_tool : string -> t -> t
val with_strategy : Context.Strategy.t -> t -> t
val with_system_prompt : string -> t -> t
val add_tool : tool_def -> t -> t
val add_tools : tool_def list -> t -> t
val add_system_block : string * string -> t -> t
val add_env : tag:string -> body:string -> t -> t

(** Render the spec as one-line text for logs / inspection. *)
val show : t -> string
