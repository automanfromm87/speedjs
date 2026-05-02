(** Run sub-agents in parallel using OCaml 5 [Domain]s.

    Each thunk runs in a FRESH domain with its OWN effect-handler
    scope. The parent's installed handlers are NOT visible inside the
    child — each child must install its own stack (mock LLM in tests,
    real Anthropic chain in production, etc.). This mirrors how
    [Tool_handler.install] / [Llm_handler.install] work but at a
    coarser granularity.

    Sharing policy is the caller's choice:
    - State the caller wants shared (cost_state, log sink): pass a
      [Mutex.t] alongside the data and synchronize manually.
    - State the caller wants isolated (Memory, Conversation, file
      handlers with per-domain backing): build it fresh inside each
      thunk.

    The runtime cost of [Domain.spawn] is non-trivial (~milliseconds);
    use this for tasks where the LLM call dominates, not for cheap
    fan-out. *)

(** Spawn each [thunk] in its own domain, [Domain.join] all of them,
    and return results in input order. Exceptions raised by a child
    thunk propagate when its result is joined. *)
let run (thunks : (unit -> 'a) list) : 'a list =
  match thunks with
  | [] -> []
  | [ single ] -> [ single () ]
  | _ ->
      let domains = List.map (fun thunk -> Domain.spawn thunk) thunks in
      List.map Domain.join domains

(** A Mutex-guarded log sink. Pass the [sink] into each child's
    [Log_handler.to_function]; call [drain ()] from the parent after
    [run] returns to read the captured lines in chronological order
    (best-effort; threads can interleave by line). *)
type shared_sink = {
  sink : string -> unit;
  drain : unit -> string list;
}

let make_shared_sink () : shared_sink =
  let mu = Mutex.create () in
  let buf = ref [] in
  let sink line =
    Mutex.lock mu;
    buf := line :: !buf;
    Mutex.unlock mu
  in
  let drain () =
    Mutex.lock mu;
    let r = List.rev !buf in
    Mutex.unlock mu;
    r
  in
  { sink; drain }

(** Wrap a sink so every line gets a [\[prefix\]] tag. Useful for
    distinguishing per-domain logs in a shared sink: each child wraps
    [shared.sink] with its own prefix. *)
let with_prefix prefix (sink : string -> unit) : string -> unit =
 fun line -> sink (Printf.sprintf "[%s] %s" prefix line)

(* ===== parallel_delegate tool ===== *)

open Types

let parallel_delegate_name = "parallel_delegate"

(** Build a tool the LLM can call to fan-out N independent tasks into
    N domains, each with its own freshly-installed handler stack.

    [build_child_stack ~prefix thunk] is the caller's responsibility:
    it should install LLM / Tool / File / Time / Log handlers inside
    [thunk] using a child config (separate cost_state, log sink with
    [prefix], no tape, muted streaming) and run [thunk]. The standard
    wiring lives in [bin/setup.ml].

    [tools_for_subagent] is the tool list each child sees. By
    convention it should NOT include [parallel_delegate] itself (to
    prevent unbounded recursion). *)
let aggregate_cost_into ~parent ~child =
  let p : cost_state = parent in
  let c : cost_state = child in
  p.input_tokens <- p.input_tokens + c.input_tokens;
  p.output_tokens <- p.output_tokens + c.output_tokens;
  p.cache_creation_tokens <-
    p.cache_creation_tokens + c.cache_creation_tokens;
  p.cache_read_tokens <- p.cache_read_tokens + c.cache_read_tokens;
  p.calls <- p.calls + c.calls

let make_delegate_tool
    ~(tools_for_subagent : tool_def list)
    ~(build_child_stack :
       prefix:string -> child_cost:cost_state -> (unit -> string) -> string)
    ~(parent_cost : cost_state) :
    tool_def =
  let parse_tasks (input : Yojson.Safe.t) : (string list, string) result =
    let open Json_decode in
    with_object_input input (fun fs ->
        match List.assoc_opt "tasks" fs with
        | Some (`List items) ->
            let strs =
              List.filter_map
                (function `String s when s <> "" -> Some s | _ -> None)
                items
            in
            if strs = [] then Error "tasks list must contain at least one non-empty string"
            else Ok strs
        | Some _ -> Error "field 'tasks' must be a JSON array of strings"
        | None -> Error "missing required field 'tasks'")
  in
  {
    idempotent = false;
    timeout_sec = None;
    category = "meta";
    capabilities = [ Meta ];
    (* Only the Executor (top-level main agent) may fan out — sub-agents
       can't recursively spawn parallel sub-agents (prevents unbounded
       fan-out). Planner / Recovery never fan out. *)
    allowed_modes = [ Executor ];
    classify_error = default_classify_error;
    name = parallel_delegate_name;
    description =
      "Spawn MULTIPLE independent sub-agents in parallel (each in its \
       own OS-level domain). Use this when you have N self-contained \
       tasks that can run concurrently — e.g. researching unrelated \
       topics, processing independent files. Each sub-agent has its \
       own fresh message history, runs its own ReAct loop, and returns \
       its final answer. Results come back in a single concatenated \
       block ordered by task index. Sub-agents have access to all \
       tools EXCEPT parallel_delegate itself.";
    input_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "tasks",
                  `Assoc
                    [
                      ("type", `String "array");
                      ( "description",
                        `String
                          "Array of self-contained task descriptions. \
                           Each runs as a separate sub-agent. NO shared \
                           context between tasks." );
                      ("items", `Assoc [ ("type", `String "string") ]);
                    ] );
              ] );
          ("required", `List [ `String "tasks" ]);
        ];
    handler =
      (fun input ->
        match parse_tasks input with
        | Error msg -> Error msg
        | Ok tasks ->
            let n = List.length tasks in
            Effect.perform
              (Effects.Log
                 (Printf.sprintf
                    "  [parallel_delegate] spawning %d domain(s)" n));
            (* Emit one Subagent_entered tick per child BEFORE spawn so
               the parent governor's [max_subagent_depth] check fires
               on the parent fiber (effects don't reach Domains).
               Also emit a structured Event so observers can correlate
               fan-out events with cost / timing in the journal. *)
            let safe_tick ev =
              try Effect.perform (Governor.Tick ev) with _ -> ()
            in
            let safe_event ev =
              try Effect.perform (Effects.Event_log ev) with _ -> ()
            in
            for _ = 1 to n do
              safe_tick Subagent_entered
            done;
            safe_event
              (Event.Subagent_entered
                 { mode = "parallel_delegate"; n_children = n });
            let thunks =
              List.mapi
                (fun i task () ->
                  let prefix = Printf.sprintf "sub:%d" i in
                  build_child_stack ~prefix
                    ~child_cost:parent_cost (fun () ->
                      let capture (s : string) : Trace.capture_result =
                        let ok =
                          not (String.starts_with ~prefix:"[ERROR]" s)
                        in
                        Trace.ok_capture ~output:s
                          ~tokens:Trace.zero_tokens ~cost_delta:0.0
                        |> fun c -> { c with ok }
                      in
                      Trace.span_current ~kind:Trace.Agent_spawn
                        ~name:prefix ~input_summary:task ~capture
                        (fun () ->
                          match
                            Agent.run ~user_query:task
                              ~tools:tools_for_subagent ()
                          with
                          | Ok s -> s
                          | Error e -> "[ERROR] " ^ agent_error_pp e)))
                tasks
            in
            let answers =
              Fun.protect
                ~finally:(fun () ->
                  for _ = 1 to n do
                    safe_tick Subagent_exited
                  done;
                  safe_event
                    (Event.Subagent_exited { mode = "parallel_delegate" }))
                (fun () -> run thunks)
            in
            let combined =
              answers
              |> List.mapi (fun i a ->
                     Printf.sprintf "=== sub:%d ===\n%s" i a)
              |> String.concat "\n\n"
            in
            Ok combined);
  }
