(** Structured representation of one LLM call's input.

    The wire-level Anthropic API takes [system + tools + messages]. The
    agent-level conceptual model is richer: there's a [system_prompt],
    [env] context blocks (workspace brief, project state, current time,
    ...), [tools], and the actual conversation [history + running].

    [Context.t] models these components separately so a [Strategy] can
    decide how they materialize into the wire format — preserving
    cache-friendly identity for the front while applying compaction or
    sliding window to the back. Each agent loop picks its strategy;
    multi-agent systems can mix strategies. *)

open Types

type env_block = { tag : string; body : string }
type system_block = { name : string; body : string }

type t = {
  system_prompt : string;
  system_blocks : system_block list;
  env : env_block list;
  tools : tool_def list;
  conversation : Conversation.t;
}

module Strategy = struct
  type t =
    | Flat
    | Sliding_window of { keep_recent : int }
    | Sliding_window_at of { trigger_at : int; keep_recent : int }
    | Compacted of {
        compact_at : int;
        keep_recent : int;
        compactor : message list -> string;
      }

  (* ===== Convenience aliases ===== *)

  let flat = Flat
  let sliding_window ~keep_recent = Sliding_window { keep_recent }
  let sliding_window_at ~trigger_at ~keep_recent =
    Sliding_window_at { trigger_at; keep_recent }
  let compacted ~compact_at ~keep_recent ~compactor =
    Compacted { compact_at; keep_recent; compactor }

  (* ===== Internals ===== *)

  let drop_first_n n msgs =
    let rec skip k = function
      | [] -> []
      | _ :: rest when k > 0 -> skip (k - 1) rest
      | rest -> rest
    in
    skip n msgs

  let drop_leading_user msgs =
    let rec loop = function
      | { role = User; _ } :: rest -> loop rest
      | other -> other
    in
    loop msgs

  let truncated_marker n_dropped =
    user_text_message
      (Printf.sprintf
         "[earlier conversation truncated — %d message(s) dropped to keep \
          context bounded]"
         n_dropped)

  let safe_cut ~drop msgs =
    let after_drop = drop_first_n drop msgs in
    let kept = drop_leading_user after_drop in
    let n_dropped = List.length msgs - List.length kept in
    truncated_marker n_dropped :: kept

  (** Closed-form anchor for [Sliding_window_at]. Replaces the old
      stateful [cut_at] ref. The previous behavior re-anchored every
      time the kept-suffix length crossed [trigger_at]; that
      progression is an arithmetic series at fixed step
      δ = trigger + 1 - keep_recent, so we can derive the current
      anchor from the message-list length [n] alone:

        N ≤ trigger          → anchor = 0
        N > trigger, δ > 0   → anchor = floor((N - keep_recent) / δ) * δ
        keep_recent ≥ trigger → anchor = 0  (degenerate; never trim)

      Verified to match the closure-stateful version on monotonically
      growing message sequences (the only call pattern in practice). *)
  let sliding_window_at_anchor ~trigger_at ~keep_recent ~n =
    if n <= trigger_at then 0
    else
      let delta = trigger_at + 1 - keep_recent in
      if delta <= 0 then 0
      else (n - keep_recent) / delta * delta

  let split_at k xs =
    let rec loop k acc = function
      | [] -> (List.rev acc, [])
      | rest when k <= 0 -> (List.rev acc, rest)
      | x :: rest -> loop (k - 1) (x :: acc) rest
    in
    loop k [] xs

  (* ===== Interpreter ===== *)

  let apply (s : t) (msgs : message list) : message list =
    match s with
    | Flat -> msgs
    | Sliding_window { keep_recent } ->
        let n = List.length msgs in
        if n <= keep_recent then msgs
        else safe_cut ~drop:(n - keep_recent) msgs
    | Sliding_window_at { trigger_at; keep_recent } ->
        let n = List.length msgs in
        let cut_at =
          sliding_window_at_anchor ~trigger_at ~keep_recent ~n
        in
        if cut_at > 0 then safe_cut ~drop:cut_at msgs else msgs
    | Compacted { compact_at; keep_recent; compactor } ->
        let n = List.length msgs in
        if n <= compact_at then msgs
        else
          let to_summarize, to_keep = split_at (n - keep_recent) msgs in
          let summary = compactor to_summarize in
          user_text_message
            (Printf.sprintf
               "<compacted_history>\n%s\n</compacted_history>" summary)
          :: to_keep

  let label = function
    | Flat -> "flat"
    | Sliding_window { keep_recent } ->
        Printf.sprintf "sliding_window{keep=%d}" keep_recent
    | Sliding_window_at { trigger_at; keep_recent } ->
        Printf.sprintf "sliding_window_at{trigger=%d, keep=%d}"
          trigger_at keep_recent
    | Compacted { compact_at; keep_recent; _ } ->
        Printf.sprintf "compacted{at=%d, keep=%d, compactor=<fun>}"
          compact_at keep_recent
end

(* ===== Construction ===== *)

let empty =
  {
    system_prompt = "";
    system_blocks = [];
    env = [];
    tools = [];
    conversation = Conversation.empty;
  }

let with_system_prompt s t = { t with system_prompt = s }
let with_tools tools t = { t with tools }
let add_tool tool t = { t with tools = t.tools @ [ tool ] }
let add_tools tools t = { t with tools = t.tools @ tools }
let with_env ~tag ~body t = { t with env = { tag; body } :: t.env }
let add_system_block ~name ~body t =
  { t with system_blocks = { name; body } :: t.system_blocks }
let with_conversation c t = { t with conversation = c }

(** Convenience: set the base system prompt and append all
    [(name, body)] pairs as system blocks. Skips entries whose body is
    empty so callers can pass [(name, "")] without polluting the
    rendered prompt. *)
let apply_system ?system_prompt ?(system_blocks = []) t =
  let t =
    match system_prompt with
    | Some s -> with_system_prompt s t
    | None -> t
  in
  List.fold_left
    (fun c (name, body) ->
      if body = "" then c else add_system_block ~name ~body c)
    t system_blocks

(* ===== Inspection ===== *)

let conversation t = t.conversation
let tools t = t.tools
let system_prompt t = t.system_prompt
let length t = Conversation.length t.conversation

(* ===== Append (pipe-friendly: arg first, t last) ===== *)

let push_assistant blocks t =
  { t with conversation = Conversation.push_assistant t.conversation blocks }

let push_tool_results blocks t =
  {
    t with
    conversation = Conversation.push_user_with_results t.conversation blocks;
  }

let push_user_text text t =
  { t with conversation = Conversation.push_user_text t.conversation text }

(* ===== Materialize ===== *)

(** Render the final system string. Order:
    {ol
    {- [system_prompt] (the base — most stable, prompt-cache prefix)}
    {- [system_blocks] in registration order (extension-contributed)}
    {- [env] blocks (ambient context, less stable)}}

    Stability order matters: any change in earlier components
    invalidates everything after, so put the most-stable content first.

    Returns "" when all three are empty so callers can branch. *)
let render_system t =
  let block_strs =
    List.map
      (fun { name; body } -> Printf.sprintf "<%s>\n%s\n</%s>" name body name)
      (List.rev t.system_blocks)
  in
  let env_strs =
    List.map
      (fun { tag; body } -> Printf.sprintf "<%s>\n%s\n</%s>" tag body tag)
      (List.rev t.env)
  in
  let parts =
    [ t.system_prompt ] @ block_strs @ env_strs
    |> List.filter (fun s -> s <> "")
  in
  String.concat "\n\n" parts

let to_llm_args ?(strategy = Strategy.flat) ?(tool_choice = Tc_auto)
    ?(model : string option = None) ?(purpose : llm_purpose = `Other)
    t : llm_call_args =
  let messages = Strategy.apply strategy (Conversation.to_messages t.conversation) in
  let system_override =
    let s = render_system t in
    if s = "" then None else Some s
  in
  { messages; tools = t.tools; system_override; tool_choice; model; purpose }
