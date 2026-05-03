(** Top-level protection boundary: tool-result truncation + the catch
    site that converts runtime exceptions ([Governor], [Llm_api_error])
    into typed [agent_error] values.

    Per-call concerns (retry, circuit breaker) live in [Llm_handler] /
    [Tool_handler] middleware. Global concerns (max steps / wall time /
    cost / death loops / sub-agent depth) live in [Governor]. This
    module is just the boundary glue. *)

(** Cap a single tool_result string at [limit] chars by keeping head + tail.
    Long outputs (browser snapshots, file_read, shell stdout) blow up the
    next prompt past max_tokens within a few turns; the head usually has
    the command echo and the tail usually has the actual answer / error. *)
let max_tool_content_chars = 12_000

let truncate_tool_content ?(limit = max_tool_content_chars) text =
  let n = String.length text in
  if n <= limit then text
  else
    let head = limit / 2 in
    let tail = limit - head in
    let omitted = n - head - tail in
    Printf.sprintf
      "%s\n...\n[truncated %d chars]\n...\n%s"
      (String.sub text 0 head) omitted
      (String.sub text (n - tail) tail)

(** Run an [agent_result]-returning thunk and convert runtime exceptions
    into typed [Error _] values. Use at the OUTERMOST call site. *)
let catch_protection_errors (f : unit -> Types.agent_result) :
    Types.agent_result =
  try f () with
  | Governor.Governor_aborted { limit; reason } ->
      Error (Types.Governor_aborted { limit; reason })
  | Llm_error.Llm_api_error err ->
      Error (Types.Llm_call_failed (Llm_error.pp err))

(** [Agent.output]-flavored variant for the chat / session mode. *)
let catch_protection_errors_output (f : unit -> Agent.output) : Agent.output =
  try f () with
  | Governor.Governor_aborted { limit; reason } ->
      Agent.Failed
        {
          reason = Types.Governor_aborted { limit; reason };
          messages = [];
        }
  | Llm_error.Llm_api_error err ->
      Agent.Failed
        {
          reason = Types.Llm_call_failed (Llm_error.pp err);
          messages = [];
        }
