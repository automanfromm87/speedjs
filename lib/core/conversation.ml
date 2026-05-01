(** Smart-constructor wrapper around [Types.message list].

    Encodes Anthropic's API contract as an internal state machine. The type
    [t] is abstract, so the only way to BUILD a conversation is via the
    constructors below — and every constructor checks invariants.

    Invariants enforced:

    1. **Strict alternation**: User → Assistant → User → ...
    2. **First message is User** (or empty)
    3. **Tool_use blocks live ONLY in Assistant turns**
    4. **Tool_result blocks live ONLY in User turns**
    5. **Tool_use ↔ Tool_result pairing**: every [Tool_use] in an Assistant
       turn has a matching [Tool_result] (by [tool_use_id]) in the
       IMMEDIATELY following User turn
    6. **Trailing dangling allowed**: if the last message is an Assistant
       turn with unanswered tool_uses, that's a legitimate "waiting"
       state — caller MUST close it before sending to Anthropic

    The dangling-tool_use bug class ([submit_task_result] tool_use never
    paired with a tool_result, accumulated into next task's prompt → 500
    mid-stream) is caught structurally: any code path that builds
    messages via these constructors literally cannot produce a malformed
    list.

    Code paths that still use raw [list @ [...]] operations get caught
    by [validate] at the Anthropic API boundary. *)

open Types

(** Internal state machine. Public API exposes [is_dangling] and
    [pending_tool_use_ids] for inspection. *)
type state =
  | S_empty
  | S_awaiting_assistant
      (** Last turn was User; expecting an Assistant turn next. *)
  | S_awaiting_user
      (** Last turn was Assistant text-only (no tool_uses); fresh User
          input or text continuation is the next valid step. *)
  | S_has_dangling of string list
      (** Last turn was Assistant with unanswered tool_uses. The list
          carries the [tool_use_id]s. Caller MUST close them via
          [push_user_with_results] or [close_dangling_with_ack] before
          [to_messages] is sent to the API. *)

type t = {
  messages : message list;
  state : state;
}

exception Invariant_violated of string

let viol msg = raise (Invariant_violated msg)

(* ===== Constructors / Accessors ===== *)

let empty = { messages = []; state = S_empty }

let length t = List.length t.messages
let to_messages t = t.messages
let is_empty t = match t.state with S_empty -> true | _ -> false

let pending_tool_use_ids t =
  match t.state with S_has_dangling ids -> ids | _ -> []

let is_dangling t =
  match t.state with S_has_dangling _ -> true | _ -> false

(* ===== Block validation helpers ===== *)

let assistant_blocks_ok blocks =
  List.iter
    (function
      | Tool_result _ ->
          viol "Assistant turn cannot contain Tool_result block"
      | _ -> ())
    blocks

let user_blocks_ok blocks =
  List.iter
    (function
      | Tool_use _ -> viol "User turn cannot contain Tool_use block"
      | _ -> ())
    blocks

let extract_tool_use_ids blocks =
  List.filter_map
    (function Tool_use { id; _ } -> Some id | _ -> None)
    blocks

let extract_tool_result_ids blocks =
  List.filter_map
    (function Tool_result { tool_use_id; _ } -> Some tool_use_id | _ -> None)
    blocks

(* ===== Push: Assistant turn ===== *)

(** Append an Assistant turn with [blocks]. Valid only when
    [state = S_awaiting_assistant]. Detects [Tool_use] blocks and updates
    state to [S_has_dangling] accordingly. *)
let push_assistant t (blocks : content_block list) : t =
  (match t.state with
  | S_awaiting_assistant -> ()
  | S_empty -> viol "push_assistant: conversation is empty (need User first)"
  | S_awaiting_user ->
      viol "push_assistant: last turn was already Assistant text-only"
  | S_has_dangling _ ->
      viol "push_assistant: prior Assistant has unanswered tool_uses");
  assistant_blocks_ok blocks;
  let dangling = extract_tool_use_ids blocks in
  let new_state =
    if dangling = [] then S_awaiting_user else S_has_dangling dangling
  in
  {
    messages = t.messages @ [ { role = Assistant; content = blocks } ];
    state = new_state;
  }

(* ===== Push: User turn closing dangling ===== *)

(** Append a User turn that closes the currently-pending dangling
    tool_uses. [blocks] must contain a [Tool_result] for every dangling
    [tool_use_id] (validated). Other block types ([Text]) may be mixed in
    alongside the tool_results — this is the Anthropic-friendly way to
    submit "tool_results AND new user instruction" as ONE turn (preserves
    strict alternation).

    Errors:
    - state is not [S_has_dangling] → [Invariant_violated]
    - missing [Tool_result] for some dangling id → [Invariant_violated]
    - [Tool_result] with id not in dangling set → [Invariant_violated] *)
let push_user_with_results t (blocks : content_block list) : t =
  let dangling_ids =
    match t.state with
    | S_has_dangling ids -> ids
    | _ ->
        viol
          "push_user_with_results: no dangling tool_uses (state is not \
           S_has_dangling)"
  in
  user_blocks_ok blocks;
  let result_ids = extract_tool_result_ids blocks in
  let missing =
    List.filter (fun id -> not (List.mem id result_ids)) dangling_ids
  in
  if missing <> [] then
    viol
      (Printf.sprintf "push_user_with_results: missing tool_result for: %s"
         (String.concat ", " missing));
  let surplus =
    List.filter (fun id -> not (List.mem id dangling_ids)) result_ids
  in
  if surplus <> [] then
    viol
      (Printf.sprintf
         "push_user_with_results: tool_result for unknown tool_use_ids: %s"
         (String.concat ", " surplus));
  {
    messages = t.messages @ [ { role = User; content = blocks } ];
    state = S_awaiting_assistant;
  }

(* ===== Push: User turn (fresh) ===== *)

(** Append a fresh User turn. Valid when state = [S_empty] or
    [S_awaiting_user] (i.e. there are NO pending tool_uses to close).
    [blocks] may contain Text (typical) but no Tool_use; Tool_result is
    rejected here too because it would be stale (no matching tool_use). *)
let push_user t (blocks : content_block list) : t =
  (match t.state with
  | S_empty | S_awaiting_user -> ()
  | S_awaiting_assistant ->
      viol "push_user: last turn was already User"
  | S_has_dangling _ ->
      viol
        "push_user: pending tool_uses must be closed first via \
         push_user_with_results or close_dangling_with_ack");
  user_blocks_ok blocks;
  if extract_tool_result_ids blocks <> [] then
    viol "push_user: stray Tool_result with no matching pending Tool_use";
  {
    messages = t.messages @ [ { role = User; content = blocks } ];
    state = S_awaiting_assistant;
  }

(** Convenience: append a fresh User turn carrying a single text block. *)
let push_user_text t text = push_user t [ Text text ]

(* ===== Helix-style synthetic close ===== *)

(** Close any pending dangling tool_uses with a synthetic [Tool_result]
    using [ack] as content; optionally append [extra] blocks (e.g. a
    [Text] for the next task) in the SAME User turn (preserves strict
    alternation). Idempotent if no dangling.

    Used at task boundaries in plan-act: the previous task ended via the
    [submit_task_result] terminal short-circuit (whose handler never ran),
    leaving a dangling tool_use. The synthetic ack closes it cleanly so
    the next task's user_query lands in a well-formed conversation. *)
let close_dangling_with_ack ?(ack = "(acknowledged)") ?(extra = []) t =
  match t.state with
  | S_has_dangling ids ->
      let synthetic =
        List.map
          (fun id ->
            Tool_result
              { tool_use_id = id; content = ack; is_error = false })
          ids
      in
      push_user_with_results t (synthetic @ extra)
  | _ -> t

(* ===== Build from raw list (with validation) ===== *)

(** Construct a [t] from a raw [message list], validating every invariant.
    Accepts a "trailing dangling" state (last message is Assistant with
    unanswered tool_uses) — this is the legitimate state of persisted
    executor memory after a [Task_terminal_called] short-circuit.

    Returns [Error msg] for any malformed sequence so callers can choose
    whether to abort, repair, or surface diagnostically. *)
let of_messages (msgs : message list) : (t, string) result =
  try
    let rec walk acc (st : state) = function
      | [] -> Ok { messages = List.rev acc; state = st }
      | m :: rest -> (
          match (st, m.role) with
          | S_empty, User ->
              user_blocks_ok m.content;
              if extract_tool_result_ids m.content <> [] then
                viol
                  "first User turn cannot contain Tool_result (no prior \
                   tool_use)";
              walk (m :: acc) S_awaiting_assistant rest
          | S_empty, Assistant -> viol "first message must be User"
          | S_awaiting_assistant, Assistant ->
              assistant_blocks_ok m.content;
              let dangling = extract_tool_use_ids m.content in
              let new_state =
                if dangling = [] then S_awaiting_user
                else S_has_dangling dangling
              in
              walk (m :: acc) new_state rest
          | S_awaiting_assistant, User -> viol "two consecutive User turns"
          | S_awaiting_user, User ->
              user_blocks_ok m.content;
              if extract_tool_result_ids m.content <> [] then
                viol
                  "stray Tool_result in User turn (no preceding tool_use)";
              walk (m :: acc) S_awaiting_assistant rest
          | S_awaiting_user, Assistant ->
              viol "two consecutive Assistant turns"
          | S_has_dangling _, Assistant ->
              viol
                "Assistant turn after dangling tool_use (expected User \
                 with matching Tool_results)"
          | S_has_dangling dangling, User ->
              user_blocks_ok m.content;
              let result_ids = extract_tool_result_ids m.content in
              let missing =
                List.filter (fun id -> not (List.mem id result_ids)) dangling
              in
              if missing <> [] then
                viol
                  (Printf.sprintf
                     "dangling tool_use(s) not answered in next User turn: \
                      %s"
                     (String.concat ", " missing));
              let surplus =
                List.filter (fun id -> not (List.mem id dangling)) result_ids
              in
              if surplus <> [] then
                viol
                  (Printf.sprintf
                     "tool_result(s) for unknown tool_use_id(s): %s"
                     (String.concat ", " surplus));
              walk (m :: acc) S_awaiting_assistant rest)
    in
    walk [] S_empty msgs
  with Invariant_violated msg -> Error msg

(** Validate without constructing. Convenience for the API boundary. *)
let validate (msgs : message list) : (unit, string) result =
  Result.map (fun _ -> ()) (of_messages msgs)
