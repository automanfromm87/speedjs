(** Structured representation of one LLM call's input.

    The wire-level Anthropic API takes [system + tools + messages]. The
    agent-level conceptual model is richer: there's a [system_prompt],
    [env] context blocks (workspace brief, project state, current time,
    ...), [tools], and the actual conversation [history + running].

    [Context.t] models these components separately so a [Strategy] can
    decide how they materialize into the wire format — preserving
    cache-friendly identity for the front while applying compaction or
    sliding window to the back. Each agent loop picks its strategy;
    multi-agent systems can mix strategies.

    Three-stage strategy example:
    {[
      System Prompt ──┐
      <env>...</env>  │── system field
                      ┘
      [Compacted History] ──┐
      Recent History        │── messages field
      Running               ┘
      Tools ────────────────── tools field
    ]}

    The conversation invariants (alternation, tool_use/result pairing)
    are enforced by [Conversation.t] internally. A [Strategy] may
    produce a list that violates Conversation invariants (e.g. dropping
    the first User turn in sliding_window); [Anthropic.validate] catches
    that at the API boundary. *)

open Types

type env_block = { tag : string; body : string }

type t = {
  system_prompt : string;
  env : env_block list;
      (** Stored in reverse insertion order; [render_system] reverses
          before formatting. *)
  tools : tool_def list;
  conversation : Conversation.t;
}

module Strategy = struct
  (** A [Strategy] adjusts the materialized conversation. It's a plain
      [message list -> message list] so callers can compose / fold their
      own. Pre-built strategies cover the common cases. *)
  type t = message list -> message list

  let flat : t = Fun.id

  let drop_first_n n msgs =
    let rec skip k = function
      | [] -> []
      | _ :: rest when k > 0 -> skip (k - 1) rest
      | rest -> rest
    in
    skip n msgs

  (** Drop leading User turns (which after a cut may carry [Tool_result]
      blocks pointing at now-dropped [Tool_use]s — Anthropic 400s). The
      first kept turn must be Assistant so the prepended synthetic User
      preserves alternation. *)
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

  (** Cut [drop] messages off the front, then repair invariants:
        - first message must be User
        - User turns can't carry tool_results without matching tool_use
      Strategy: skip leading User turns post-cut (they may have orphan
      tool_results), then prepend a synthetic User text turn so the
      kept stream starts User → Assistant → User → ... *)
  let safe_cut ~drop msgs =
    let after_drop = drop_first_n drop msgs in
    let kept = drop_leading_user after_drop in
    let n_dropped = List.length msgs - List.length kept in
    truncated_marker n_dropped :: kept

  (** Keep only the LAST [keep_recent] messages (approximately —
      orphan-User turns are dropped and a synthetic prefix is added,
      so final length may be slightly off). The synthetic prefix
      preserves Anthropic's "first message = User" invariant and
      avoids dangling tool_result blocks. *)
  let sliding_window ~keep_recent : t =
   fun msgs ->
    let n = List.length msgs in
    if n <= keep_recent then msgs
    else safe_cut ~drop:(n - keep_recent) msgs

  (** Soft-trigger sliding window with a frozen cut anchor.
      Stateful: each call to [sliding_window_at] returns a fresh
      closure with its own [cut_at] ref. Cache stability requires that
      successive calls drop the SAME prefix, so callers must reuse the
      SAME closure within a run (and create a NEW one per run).

      Algorithm:
      - On first call where messages exceed [trigger_at], freeze
        [cut_at = n - keep_recent] so the kept suffix starts after a
        stable index.
      - On subsequent calls, keep applying that [cut_at] until the
        effective (un-cut) length crosses [trigger_at] again, at which
        point we re-anchor.

      Result: between trims, the cached prompt prefix (system + tools +
      first kept turn) stays stable across many LLM calls. *)
  let sliding_window_at ~trigger_at ~keep_recent : t =
    let cut_at = ref 0 in
    fun msgs ->
      let n = List.length msgs in
      let effective = n - !cut_at in
      if effective > trigger_at then cut_at := n - keep_recent;
      if !cut_at > 0 then safe_cut ~drop:!cut_at msgs else msgs

  (** When the conversation has more than [compact_at] messages, replace
      the oldest [n - keep_recent] with a single User turn containing
      the [compactor]'s summary text.

      [compactor] receives the messages-to-be-summarized and returns the
      summary string. Typical implementations call an LLM via
      [Effects.Llm_complete] to produce a tight briefing. *)
  let compacted ~compact_at ~keep_recent ~compactor : t =
   fun msgs ->
    let n = List.length msgs in
    if n <= compact_at then msgs
    else
      let rec split k = function
        | [] -> ([], [])
        | rest when k <= 0 -> ([], rest)
        | x :: rest ->
            let l, r = split (k - 1) rest in
            (x :: l, r)
      in
      let to_summarize, to_keep = split (n - keep_recent) msgs in
      let summary = compactor to_summarize in
      user_text_message
        (Printf.sprintf
           "<compacted_history>\n%s\n</compacted_history>" summary)
      :: to_keep
end

(* ===== Construction ===== *)

let empty =
  {
    system_prompt = "";
    env = [];
    tools = [];
    conversation = Conversation.empty;
  }

let with_system_prompt s t = { t with system_prompt = s }
let with_tools tools t = { t with tools }
let with_env ~tag ~body t = { t with env = { tag; body } :: t.env }
let with_conversation c t = { t with conversation = c }

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

(** Render the final system string: [system_prompt] followed by env
    blocks formatted as [<tag>body</tag>]. Returns "" when both are
    empty so callers can branch on emptiness. *)
let render_system t =
  let env_in_order = List.rev t.env in
  let env_strs =
    List.map
      (fun { tag; body } -> Printf.sprintf "<%s>\n%s\n</%s>" tag body tag)
      env_in_order
  in
  let parts =
    (if t.system_prompt = "" then [] else [ t.system_prompt ]) @ env_strs
  in
  String.concat "\n\n" parts

let to_llm_args ?(strategy = Strategy.flat) ?(tool_choice = Tc_auto) t :
    llm_call_args =
  let system = render_system t in
  let messages = strategy (Conversation.to_messages t.conversation) in
  {
    system_override = (if system = "" then None else Some system);
    tools = t.tools;
    messages;
    tool_choice;
  }
