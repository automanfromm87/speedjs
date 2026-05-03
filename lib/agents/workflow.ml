(** See [workflow.mli]. *)

open Types

(* The workflow type is intentionally simple: a thunk that returns a
   [Result]. Composition is just [Result.bind]. The [Agent.output] sum
   stays as the value that flows through; output projections lift the
   output's [Failed] arm to [Result.Error]. *)
type 'a t = unit -> ('a, agent_error) result

(* ===== Constructors ===== *)

let pure x : 'a t = fun () -> Ok x
let of_result r : 'a t = fun () -> r
let of_thunk f : 'a t = f

let leaf (spec : Agent_spec.validated) (input : Agent.input) :
    Agent.output t =
 fun () -> Ok (Agent.execute ~spec ~input)

let map (m : 'a t) (f : 'a -> 'b) : 'b t =
 fun () ->
  match m () with
  | Ok x -> Ok (f x)
  | Error _ as e -> e

let bind (m : 'a t) (f : 'a -> 'b t) : 'b t =
 fun () ->
  match m () with
  | Ok x -> f x ()
  | Error _ as e -> e

(* ===== Output projections ===== *)

let expect_done ?(name = "agent") (m : Agent.output t) :
    (string * message list) t =
 fun () ->
  match m () with
  | Error _ as e -> e
  | Ok (Agent.Done { answer; messages }) -> Ok (answer, messages)
  | Ok (Agent.Failed { reason; _ }) -> Error reason
  | Ok (Agent.Terminal_tool { name = tool_name; _ }) ->
      Error
        (Plan_invalid
           (Printf.sprintf
              "%s: expected free-text answer but model called terminal \
               tool %S"
              name tool_name))
  | Ok (Agent.Waiting { question; _ }) ->
      Error
        (Plan_invalid
           (Printf.sprintf
              "%s: model unexpectedly called ask_user (%s)" name question))

let expect_terminal_tool ~name ?(label = "agent") (m : Agent.output t) :
    (Yojson.Safe.t * message list) t =
 fun () ->
  match m () with
  | Error _ as e -> e
  | Ok (Agent.Terminal_tool { name = got; payload; messages })
    when got = name ->
      Ok (payload, messages)
  | Ok (Agent.Terminal_tool { name = got; _ }) ->
      Error
        (Plan_invalid
           (Printf.sprintf
              "%s: expected terminal tool %S but model called %S" label
              name got))
  | Ok (Agent.Failed { reason; _ }) -> Error reason
  | Ok (Agent.Done _) ->
      Error
        (Plan_invalid
           (Printf.sprintf
              "%s: model ended turn without calling %S — protocol lost"
              label name))
  | Ok (Agent.Waiting { question; _ }) ->
      Error
        (Plan_invalid
           (Printf.sprintf "%s: unexpected ask_user (%s)" label question))

(* ===== Combinators ===== *)

let with_retry ?(max_attempts = 3) (body : 'a t) : 'a t =
 fun () ->
  let rec attempt n =
    match body () with
    | Ok _ as r -> r
    | Error _ as r when n >= max_attempts -> r
    | Error _ -> attempt (n + 1)
  in
  attempt 1

let recover (body : 'a t) (handler : agent_error -> 'a t) : 'a t =
 fun () ->
  match body () with
  | Ok _ as r -> r
  | Error e -> handler e ()

let with_checkpoint ~cwd ~message (inner : 'a t) : 'a t =
 fun () ->
  if not (Git_checkpoint.is_git_repo ~cwd) then (
    (try
       Effect.perform
         (Effects.Log
            (Printf.sprintf
               "[checkpoint] %s is not a git repo — running without per-task \
                rollback"
               cwd))
     with Effect.Unhandled _ -> ());
    inner ())
  else
    match Git_checkpoint.create ~cwd with
    | Error e ->
        (try
           Effect.perform
             (Effects.Log
                (Printf.sprintf
                   "[checkpoint] create failed at %s — running without rollback: \
                    %s"
                   cwd e))
         with Effect.Unhandled _ -> ());
        inner ()
    | Ok ckpt ->
        (match inner () with
         | Ok _ as r ->
             (match Git_checkpoint.commit ckpt ~message:(message ()) with
              | Ok _ -> ()
              | Error e ->
                  (try
                     Effect.perform
                       (Effects.Log
                          (Printf.sprintf "[checkpoint] commit failed: %s" e))
                   with Effect.Unhandled _ -> ()));
             r
         | Error _ as r ->
             Git_checkpoint.rollback ckpt;
             r)

let attempt (body : 'a t) : ('a, agent_error) Result.t t =
 fun () -> Ok (body ())

let action (f : unit -> unit) : unit t =
 fun () -> f (); Ok ()

let foreach (items : 'a list) (body : 'a -> 'b t) : 'b list t =
 fun () ->
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | x :: rest -> (
        match body x () with
        | Error _ as e -> e
        | Ok y -> loop (y :: acc) rest)
  in
  loop [] items

(* Per-branch handler-stack wrapper. Default identity (test path);
   production [bin/setup.ml] overrides via [set_branch_wrapper] to
   call [Runtime.install] with a child config so each Domain gets
   its own LLM / Tool / Trace stack. The slot type is erased to
   avoid making this module depend on [Runtime]; the magic is
   confined to the [set_branch_wrapper] / read pair. *)
type branch_wrapper = { wrap : 'a. (unit -> 'a) -> 'a }

let identity_wrapper = { wrap = (fun thunk -> thunk ()) }

let branch_wrapper_slot : branch_wrapper Domain.DLS.key =
  Domain.DLS.new_key (fun () -> identity_wrapper)

let set_branch_wrapper (w : (unit -> 'a) -> 'a) : unit =
  (* Polymorphic wrap: cast through Obj to bridge the rank-2
     [{ wrap : 'a. ... }] field. The wrapper is invoked with
     whatever [unit -> 'a] each branch is, and 'a varies — but the
     wrapper code is uniform (typically a Runtime.install). *)
  let magic_w : 'a. (unit -> 'a) -> 'a =
    fun thunk -> (Obj.magic w : (unit -> _) -> _) thunk
  in
  Domain.DLS.set branch_wrapper_slot { wrap = magic_w }

let parallel (flows : 'a t list) : 'a list t =
 fun () ->
  let { wrap } = Domain.DLS.get branch_wrapper_slot in
  let thunks = List.map (fun f () -> wrap f) flows in
  let results = Parallel_subagent.run thunks in
  let rec collect acc = function
    | [] -> Ok (List.rev acc)
    | (Error _ as e) :: _ -> e
    | Ok x :: rest -> collect (x :: acc) rest
  in
  collect [] results

(* ===== Syntax sugar ===== *)

let ( let* ) = bind
let ( let+ ) = map

(* ===== Interpreter ===== *)

let run (m : 'a t) : ('a, agent_error) result = m ()

(* ===== Show =====
   The thunk type [unit -> result] is opaque; we can't introspect it.
   This is a trade-off: simpler implementation, but [show] is shallow.
   A GADT-based [_ flow] type with explicit nodes would let us walk
   the tree, at the cost of more boilerplate per combinator. We can
   upgrade later if review-ability becomes the priority. *)
let show (_ : 'a t) : string = "<workflow>"
