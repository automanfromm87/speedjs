(** Typed taxonomy of LLM API failures.

    Inspired by helix's [classify_api_exception] — every non-2xx response
    or transport error gets bucketed into one of these. The retry handler
    only needs the bucket; the concrete value carries diagnostic info. *)

type t =
  | Rate_limit of { retry_after : float option; message : string }
      (** HTTP 429. [retry_after] from header (seconds). *)
  | Overloaded of { retry_after : float option; message : string }
      (** HTTP 529 (Anthropic-specific). Retry with longer backoff. *)
  | Server_error of { status : int; message : string }
      (** HTTP 5xx (other). Retry with backoff. *)
  | Transport of string
      (** Network / DNS / TLS / read timeout / curl crash. Retry, often
          transient. *)
  | Bad_request of string
      (** HTTP 400 / 422 — our request was malformed. Don't retry. *)
  | Auth of string
      (** HTTP 401 / 403 — credentials issue. Don't retry, alert ops. *)
  | Not_found of string
      (** HTTP 404 — usually wrong model name. Don't retry. *)
  | Context_window of string
      (** Special 400: input exceeded the model's context window. Retrying
          won't help; the agent loop must compact memory or split work. *)

exception Llm_api_error of t

let is_retryable = function
  | Rate_limit _ | Overloaded _ | Server_error _ | Transport _ -> true
  | Bad_request _ | Auth _ | Not_found _ | Context_window _ -> false

let retry_after_hint = function
  | Rate_limit { retry_after; _ } | Overloaded { retry_after; _ } ->
      retry_after
  | _ -> None

let pp = function
  | Rate_limit { message; retry_after } ->
      Printf.sprintf "rate-limited (retry-after=%s): %s"
        (match retry_after with Some f -> Printf.sprintf "%.1fs" f | None -> "—")
        message
  | Overloaded { message; retry_after } ->
      Printf.sprintf "overloaded (retry-after=%s): %s"
        (match retry_after with Some f -> Printf.sprintf "%.1fs" f | None -> "—")
        message
  | Server_error { status; message } ->
      Printf.sprintf "server error %d: %s" status message
  | Transport msg -> Printf.sprintf "transport: %s" msg
  | Bad_request msg -> Printf.sprintf "bad request: %s" msg
  | Auth msg -> Printf.sprintf "auth: %s" msg
  | Not_found msg -> Printf.sprintf "not found: %s" msg
  | Context_window msg -> Printf.sprintf "context window exceeded: %s" msg

let kind = function
  | Rate_limit _ -> "rate_limit"
  | Overloaded _ -> "overloaded"
  | Server_error _ -> "server_error"
  | Transport _ -> "transport"
  | Bad_request _ -> "bad_request"
  | Auth _ -> "auth"
  | Not_found _ -> "not_found"
  | Context_window _ -> "context_window"

(* ===== Classification ===== *)

let context_window_markers =
  [
    "context window";
    "input is too long";
    "prompt is too long";
    "exceeds the maximum";
    "max_tokens_to_sample";
  ]

let lowercase_contains_any ~haystack needles =
  let lower = String.lowercase_ascii haystack in
  let h_len = String.length lower in
  List.exists
    (fun n ->
      let n_len = String.length n in
      if n_len > h_len then false
      else
        let rec scan i =
          if i + n_len > h_len then false
          else if String.sub lower i n_len = n then true
          else scan (i + 1)
        in
        scan 0)
    needles

let classify_status ~status ~body ~retry_after : t =
  match status with
  | 200 | 201 | 202 | 204 ->
      failwith
        (Printf.sprintf
           "Llm_error.classify_status called on success status %d" status)
  | 400 ->
      if lowercase_contains_any ~haystack:body context_window_markers then
        Context_window body
      else Bad_request body
  | 401 | 403 -> Auth body
  | 404 -> Not_found body
  | 422 -> Bad_request body
  | 429 -> Rate_limit { retry_after; message = body }
  | 529 -> Overloaded { retry_after; message = body }
  | s when s >= 500 && s < 600 -> Server_error { status = s; message = body }
  | s when s >= 400 && s < 500 -> Bad_request body
  | s -> Server_error { status = s; message = body }

let classify_transport ~message : t = Transport message
