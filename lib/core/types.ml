(** Shared types: messages, content blocks, tool definitions, LLM responses.
    Mirrors the Anthropic Messages API shape. *)

type role = User | Assistant
(* System is sent separately in Anthropic API, so we don't put it in messages *)

type content_block =
  | Text of string
  | Tool_use of {
      id : Id.Tool_use_id.t;
      name : string;
        (** Tool name remains plain [string] because dispatch routinely
            pattern-matches against literals (e.g. ["submit_plan"]). A
            phantom type here would force [Tool_name.of_string "literal"]
            wrappers in every match arm — net-negative ergonomically. *)
      input : Yojson.Safe.t;
    }
  | Tool_result of {
      tool_use_id : Id.Tool_use_id.t;
      content : string;
      is_error : bool;
    }

type message = {
  role : role;
  content : content_block list;
}

type stop_reason =
  | End_turn
  | Tool_use_stop
  | Max_tokens
  | Stop_sequence
  | Other of string

type usage = {
  input_tokens : int;
  output_tokens : int;
  cache_creation_input_tokens : int;
      (** Tokens written to prompt cache on this call. Nonzero indicates a
          fresh cache write — third call onwards usually drops to zero. *)
  cache_read_input_tokens : int;
      (** Tokens served from prompt cache on this call. Nonzero proves the
          [cache_control: ephemeral] markers are working. *)
}

(** Convenience: minimal usage with only [input_tokens] / [output_tokens] set,
    cache fields zeroed. Useful in tests. *)
let usage_of_basic ~input_tokens ~output_tokens : usage =
  {
    input_tokens;
    output_tokens;
    cache_creation_input_tokens = 0;
    cache_read_input_tokens = 0;
  }

type llm_response = {
  content : content_block list;
  stop_reason : stop_reason;
  usage : usage;
}

(** Tool handler result: success carries text observation, error carries a
    diagnostic. The agent forwards both back to the LLM but flags errors via
    [Tool_result.is_error]. *)
type tool_handler_result = (string, string) Result.t

(** Tool semantic capabilities. A tool may have multiple — e.g. [bash]
    is both [Exec] and [Mutating]; [http_get] is both [Read_only] and
    [Network]. The set is used to filter the tool surface per
    [agent_mode] (Planner can't see [Mutating]) and to drive policy
    middleware (e.g. [Network] tools opt into transient retry by
    default). *)
type capability =
  | Read_only       (** No filesystem / network / subprocess effect. *)
  | Mutating        (** Writes to filesystem or external state. *)
  | Exec            (** Spawns subprocess. *)
  | Network         (** Performs network I/O. *)
  | Meta            (** Orchestration: spawns sub-agents / loads skills. *)
  | Pause           (** Synthetic — interrupts the agent loop (ask_user). *)
  | Terminal        (** Synthetic terminal tool (submit_task_result, etc).
                        Handler never invoked; intercepted by run_loop. *)

(** Where a tool may be exposed. The runtime filters [tool_def]s by
    cross-referencing [agent_mode] with the tool's [allowed_modes].

    - [Planner]   : decomposes the user goal. Read-only research only.
    - [Recovery]  : decides REPLAN/SPLIT/SKIP/ABANDON when a task fails.
                    Same surface as Planner.
    - [Executor]  : the per-task ReAct loop. Full surface.
    - [Subagent]  : delegated focused task. Full surface minus [Meta]
                    (no recursive delegate, no parallel_delegate). *)
type agent_mode = Planner | Recovery | Executor | Subagent

let agent_mode_to_string = function
  | Planner -> "planner"
  | Recovery -> "recovery"
  | Executor -> "executor"
  | Subagent -> "subagent"

type tool_def = {
  name : string;
  description : string;
  input_schema : Yojson.Safe.t;
  handler : Yojson.Safe.t -> tool_handler_result;
  (* === Metadata for fault-tolerance middleware === *)
  idempotent : bool;
      (** Safe to retry on transient failures. Default [false] for
          conservative behavior — only mark [true] for read-only tools
          (calculator, view_file, http_get) and clearly side-effect-free
          ops. Write / exec tools should stay [false] to avoid
          duplicating mutations on retry. *)
  timeout_sec : float option;
      (** Per-tool wall-clock budget (advisory). The handler is expected
          to honor this internally (e.g. [bash] uses it for
          [run_with_timeout]). Middleware can also enforce / log against
          it. *)
  category : string;
      (** Free-form coarse grouping for audit / metrics. The hard
          gating happens via [capabilities] + [allowed_modes];
          [category] is just a human-readable hint shown in logs and
          surfaced through the trace. *)
  capabilities : capability list;
      (** What the tool is allowed to do. Used by the runtime to
          enforce mode-based filtering: a [Planner] never sees a
          [Mutating] tool even if the prompt fails to gate it. *)
  allowed_modes : agent_mode list;
      (** Which agent modes may call this tool. Defaults are derived
          from [capabilities] in [make_typed_tool] — synthetic tools
          (terminal, pause, meta) override explicitly. *)
  classify_error : string -> [ `Transient | `Permanent ];
      (** Map a tool's string error to a retryability class.
          [Tool_handler.with_retry] honors [`Transient] (when the tool
          is also [idempotent]) and propagates [`Permanent] verbatim.
          Default classifier returns [`Permanent] — conservative, since
          most tool errors really are deterministic. Tools with known
          transient failure modes (network calls, subprocesses) override
          via [make_typed_tool ~error_classifier]. *)
}

(** Default mode allowance from a capability set. [Read_only] and
    [Network] (also read-only by convention here) are seen by every
    mode; anything else is restricted to [Executor] / [Subagent].
    Synthetic kinds ([Pause] / [Terminal] / [Meta]) override
    explicitly via [make_typed_tool ~allowed_modes]. *)
let default_modes_for_capabilities (caps : capability list) : agent_mode list =
  let read_only_only =
    List.for_all
      (fun c ->
        match c with
        | Read_only | Network -> true
        | Mutating | Exec | Meta | Pause | Terminal -> false)
      caps
  in
  if read_only_only then [ Planner; Recovery; Executor; Subagent ]
  else [ Executor; Subagent ]

(** Filter a tool list to those a given mode may use. *)
let tools_for_mode (mode : agent_mode) (tools : tool_def list) :
    tool_def list =
  List.filter (fun t -> List.mem mode t.allowed_modes) tools

(** Build a [tool_def] from a TYPED handler. The decoder owns
    JSON-to-OCaml-value translation; the handler then operates on
    well-typed input. Output is [string] (sent back to the LLM as the
    Tool_result content) — for structured outputs, the handler does
    its own [to_string].

    Compiles to the same untyped [tool_def] under the hood — pure
    ergonomic win, no GADT, no existential. The ['input] type variable
    exists only during this call site, then gets erased into
    [Yojson.Safe.t].

    Errors from [input_decoder] are wrapped in
    ["invalid input: <reason>"] so the LLM gets actionable feedback. *)
let default_classify_error : string -> [ `Transient | `Permanent ] =
 fun _ -> `Permanent

let make_typed_tool ?(idempotent = false) ?(timeout_sec = None)
    ?(category = "general")
    ?(capabilities = [ Read_only ])
    ?allowed_modes
    ?(classify_error = default_classify_error)
    ~name ~description ~input_schema
    ~(input_decoder : Yojson.Safe.t -> ('input, string) result)
    ~(handler : 'input -> (string, string) result) () : tool_def =
  let wrapped (json : Yojson.Safe.t) : tool_handler_result =
    match input_decoder json with
    | Error e -> Error ("invalid input: " ^ e)
    | Ok parsed -> handler parsed
  in
  let allowed_modes =
    match allowed_modes with
    | Some m -> m
    | None -> default_modes_for_capabilities capabilities
  in
  {
    name;
    description;
    input_schema;
    handler = wrapped;
    idempotent;
    timeout_sec;
    category;
    capabilities;
    allowed_modes;
    classify_error;
  }

(** Anthropic [tool_choice] field. [Tc_auto] is API default — we omit it
    from the request body. *)
type tool_choice =
  | Tc_auto
  | Tc_any (* must call some tool *)
  | Tc_tool of string (* must call this specific tool *)
  | Tc_none (* tools available but model must answer in text *)

(** Top-level error type returned by [Agent.run] / [Plan_act.run]. *)
type agent_error =
  | Max_iterations_reached of int
  | Llm_max_tokens
  | Stop_reason_unexpected of string
  | Llm_call_failed of string
  | Governor_aborted of { limit : string; reason : string }
      (** Cross-cutting Governor stopped the run. [limit] is the cap
          name ("max_steps", "max_wall_time", "max_cost",
          "max_tool_calls", "max_subagent_depth",
          "max_repeated_tool_calls"); [reason] is the diagnostic. *)
  | Llm_refused of string
      (** stop_reason == "refusal". *)
  | Plan_invalid of string
      (** Planner did not produce a usable plan (didn't call submit_plan,
          or plan structure was malformed). *)
  | Plan_task_failed of {
      task_index : int;
      description : string;
      reason : agent_error;
    }
      (** A subtask in a plan-act flow ran but returned [Error]. *)

let rec agent_error_pp = function
  | Max_iterations_reached n -> Printf.sprintf "max iterations reached (%d)" n
  | Llm_max_tokens -> "LLM hit max_tokens before completing"
  | Stop_reason_unexpected s -> Printf.sprintf "unexpected stop_reason: %s" s
  | Llm_call_failed msg -> Printf.sprintf "LLM call failed: %s" msg
  | Governor_aborted { limit; reason } ->
      Printf.sprintf "governor aborted (%s): %s" limit reason
  | Llm_refused reason -> Printf.sprintf "model refused: %s" reason
  | Plan_invalid reason -> Printf.sprintf "plan invalid: %s" reason
  | Plan_task_failed { task_index; description; reason } ->
      Printf.sprintf "task %d (%s) failed: %s" task_index description
        (agent_error_pp reason)

(** Agent result: Ok of final text answer, or Error of typed reason. *)
type agent_result = (string, agent_error) Result.t

(** Multi-turn session outcome.

    [Outcome_waiting] is produced when the model calls the [ask_user]
    pause-tool: the agent loop halts mid-conversation, the messages so far
    are returned, and the next user input becomes the [Tool_result] that
    answers [tool_use_id]. *)
type session_result =
  | Outcome_done of {
      answer : string;
      final_messages : message list;
    }
  | Outcome_waiting of {
      tool_use_id : Id.Tool_use_id.t;
      question : string;
      messages : message list;
    }
  | Outcome_failed of {
      reason : agent_error;
      messages : message list;
    }

(** Arguments to one LLM call. Carried by the [Llm_complete] effect.

    [system_override] = [None] means use whatever default the production
    handler was configured with. Set [Some "..."] to override per-call —
    the planner agent uses this to inject a planning-specific system prompt
    while keeping the same handler stack (cost tracking, streaming, etc.). *)
type llm_call_args = {
  messages : message list;
  tools : tool_def list;
  system_override : string option;
  tool_choice : tool_choice;
}

(** Common case: no overrides, model picks tools freely. *)
let basic_call ~messages ~tools : llm_call_args =
  { messages; tools; system_override = None; tool_choice = Tc_auto }

(* ===== Planner / plan-act types ===== *)

type task = {
  index : int;        (** 1-indexed position in the plan *)
  description : string;
}

(** Structured task outcome submitted via the [submit_task_result] terminal
    tool. Captures explicit success / failure semantics — distinct from
    "agent ran out of things to say" (End_turn). *)
type task_submit = {
  ts_success : bool;
  ts_result : string;
  ts_error : string;
}

(** What [Agent.run_for_task] returns. Differentiates between:
    - explicit success/failure via submit_task_result
    - implicit "End_turn" without submit (still useful — captures the answer)
    - structural failures (max iter, etc.) that need recovery *)
type task_run_outcome =
  | Task_done_explicit of {
      submit : task_submit;
      messages : message list;
    }
  | Task_done_implicit of {
      answer : string;
      messages : message list;
    }
  | Task_run_failed of { reason : agent_error; messages : message list }
  | Task_run_waiting of {
      tool_use_id : Id.Tool_use_id.t;
      question : string;
      messages : message list;
    }

type plan = {
  title : string;
  goal : string;
  tasks : task list;
}

(** Mutable token / call counter shared across handler stack. *)
type cost_state = {
  mutable input_tokens : int;
  mutable output_tokens : int;
  mutable cache_creation_tokens : int;
  mutable cache_read_tokens : int;
  mutable calls : int;
  (* Step + tool counters live here too so [Governor.max_steps] and
     [Governor.max_tool_calls] are GLOBAL caps across parallel
     sub-agents. Each child Domain installs its own Governor that
     reads + increments these counters under [mu]. *)
  mutable steps : int;
  mutable tool_calls : int;
  mutable start_time : float;
      (** Wall-clock origin for [Governor.max_wall_time_sec]. Set
          ONCE at the outermost [Runtime.install]; child Domains
          inherit the same value via [cost_state] so their walltime
          checks measure age-since-RUN-start, not age-since-spawn.
          A value of [0.0] means "not yet anchored" — first
          Governor that sees this fills it from its clock. *)
  mu : Mutex.t;
      (** Guards mutations from parallel sub-agents, which all share
          the parent's cost_state so the parent governor sees cumulative
          spend in real time. Single-Domain runs lock + unlock once per
          LLM call — a few hundred nanoseconds of overhead, negligible
          next to the LLM call itself. *)
}

let new_cost_state () =
  {
    input_tokens = 0;
    output_tokens = 0;
    cache_creation_tokens = 0;
    cache_read_tokens = 0;
    calls = 0;
    steps = 0;
    tool_calls = 0;
    start_time = 0.0;  (* unanchored; first Governor anchors *)
    mu = Mutex.create ();
  }

(** Atomic increment of a single counter — pair with the existing
    [cost_state_add] which folds an entire LLM call's usage. *)
let cost_state_inc_step (cost : cost_state) =
  Mutex.lock cost.mu;
  cost.steps <- cost.steps + 1;
  Mutex.unlock cost.mu

let cost_state_inc_tool_call (cost : cost_state) =
  Mutex.lock cost.mu;
  cost.tool_calls <- cost.tool_calls + 1;
  Mutex.unlock cost.mu

(** Mutex-guarded accumulation. Use this from [Llm_handler.with_cost_tracking]
    so parallel sub-agents (which share the parent cost_state) don't race. *)
let cost_state_add (cost : cost_state)
    ~input_tokens ~output_tokens ~cache_creation ~cache_read =
  Mutex.lock cost.mu;
  cost.input_tokens <- cost.input_tokens + input_tokens;
  cost.output_tokens <- cost.output_tokens + output_tokens;
  cost.cache_creation_tokens <-
    cost.cache_creation_tokens + cache_creation;
  cost.cache_read_tokens <- cost.cache_read_tokens + cache_read;
  cost.calls <- cost.calls + 1;
  Mutex.unlock cost.mu

(* Sonnet 4.5 pricing (USD per 1M tokens). Cache writes cost 1.25x base
   input, cache reads cost 0.1x. Adjust per model. *)
let price_input_per_m = 3.0
let price_output_per_m = 15.0
let price_cache_write_per_m = 3.75
let price_cache_read_per_m = 0.30

let cost_usd (s : cost_state) =
  (float_of_int s.input_tokens *. price_input_per_m
  +. float_of_int s.output_tokens *. price_output_per_m
  +. float_of_int s.cache_creation_tokens *. price_cache_write_per_m
  +. float_of_int s.cache_read_tokens *. price_cache_read_per_m)
  /. 1_000_000.0

(** Trivial constructors for the common case of a one-block text
    message. JSON serialization lives in [Codec]. *)
let user_text_message text = { role = User; content = [ Text text ] }
let assistant_text_message text = { role = Assistant; content = [ Text text ] }

(* ===== Pretty printing helpers (for logs / debug) ===== *)

let role_pp = function User -> "user" | Assistant -> "assistant"

let content_block_pp = function
  | Text s ->
      let s =
        if String.length s > 200 then String.sub s 0 200 ^ "..." else s
      in
      Printf.sprintf "Text(%S)" s
  | Tool_use { id; name; input } ->
      Printf.sprintf "ToolUse(%s, %s, %s)"
        (Id.Tool_use_id.to_string id) name
        (Yojson.Safe.to_string input)
  | Tool_result { tool_use_id; content; is_error } ->
      let c =
        if String.length content > 200 then String.sub content 0 200 ^ "..."
        else content
      in
      Printf.sprintf "ToolResult(%s, %S, err=%b)"
        (Id.Tool_use_id.to_string tool_use_id)
        c is_error
