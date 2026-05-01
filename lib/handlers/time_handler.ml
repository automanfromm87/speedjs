(** Handler for [Effects.Time_now], composable via [|>].

    [t] is just a thunk producing a timestamp. [direct] reads the real
    clock; [fixed] returns a canned value. Middleware (none yet) would
    take a [t] and return a wrapped [t] — same pattern as
    [Llm_handler] / [Tool_handler]. *)

type t = unit -> float

let direct : t = Unix.gettimeofday

let fixed (now : float) : t = fun () -> now

let install (chain : t) (thunk : unit -> 'a) : 'a =
  Effect.Deep.try_with thunk ()
    {
      effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Effects.Time_now ->
              Some
                (fun (k : (a, _) Effect.Deep.continuation) ->
                  Effect.Deep.continue k (chain ()))
          | _ -> None);
    }

(** Convenience for tests / Checkpoint replay. *)
let install_fixed ~(now : float) thunk = install (fixed now) thunk
