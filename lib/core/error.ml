(** Unified error type for both LLM calls and tool calls.

    The handler chains ([Llm_handler], [Tool_handler]) all classify
    failures into the same shape, so retry / circuit-breaker / audit
    middleware can branch on error semantics uniformly:

    - [kind = Transient]  → safe to retry (rate limit, 5xx, timeout, ...)
    - [kind = Permanent]  → don't retry (auth, bad input, schema mismatch, ...)

    The [domain] tells where the failure came from (LLM API, specific
    tool, network, validation, system). The [code] is a machine-readable
    short token for telemetry; [message] is the diagnostic text. *)

type kind =
  | Transient  (** Retryable: same call MAY succeed if retried. *)
  | Permanent  (** Non-retryable: same call WILL fail again. *)

type domain =
  | Llm
  | Tool of string  (** [tool name] *)
  | Network
  | Validation
  | System

type t = {
  kind : kind;
  domain : domain;
  code : string;       (** Machine-readable short token, e.g. "rate_limit". *)
  message : string;
  retry_after : float option;  (** Hint from server, if any. *)
}

let make ?(retry_after = None) ~kind ~domain ~code message =
  { kind; domain; code; message; retry_after }

let transient ?retry_after ~domain ~code message =
  make ~retry_after ~kind:Transient ~domain ~code message

let permanent ~domain ~code message =
  make ~kind:Permanent ~domain ~code message

let is_retryable t = match t.kind with Transient -> true | Permanent -> false

let kind_to_string = function
  | Transient -> "transient"
  | Permanent -> "permanent"

let domain_to_string = function
  | Llm -> "llm"
  | Tool name -> "tool:" ^ name
  | Network -> "network"
  | Validation -> "validation"
  | System -> "system"

let pp t =
  let suffix =
    match t.retry_after with
    | Some s -> Printf.sprintf " (retry-after=%.1fs)" s
    | None -> ""
  in
  Printf.sprintf "[%s/%s/%s] %s%s"
    (domain_to_string t.domain)
    (kind_to_string t.kind)
    t.code t.message suffix

(** Convert from the typed LLM-specific error into the unified shape.
    [Llm_error.kind] already produces the canonical short token (e.g.
    "rate_limit", "auth"); [is_retryable] decides Transient vs Permanent. *)
let of_llm_error (e : Llm_error.t) : t =
  let kind = if Llm_error.is_retryable e then Transient else Permanent in
  let code, message, retry_after =
    match e with
    | Rate_limit { retry_after; message } ->
        ("rate_limit", message, retry_after)
    | Overloaded { retry_after; message } ->
        ("overloaded", message, retry_after)
    | Server_error { status; message } ->
        (Printf.sprintf "http_%d" status, message, None)
    | Transport msg -> ("transport", msg, None)
    | Bad_request msg -> ("bad_request", msg, None)
    | Auth msg -> ("auth", msg, None)
    | Not_found msg -> ("not_found", msg, None)
    | Context_window msg -> ("context_window", msg, None)
  in
  make ~kind ~domain:Llm ~code ~retry_after message

(** Wrap an arbitrary tool error string into the unified shape.
    Defaults to [Permanent] (most tool errors are configuration /
    bad-input failures); pass [~kind:Transient] for retryable cases
    (e.g. external service blip). *)
let of_tool_error ?(kind = Permanent) ?(code = "tool_error") ~tool_name
    message =
  make ~kind ~domain:(Tool tool_name) ~code message
