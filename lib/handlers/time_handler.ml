(** Production handler for [Effects.Time_now].

    Pulls real wall-clock time from [Unix.gettimeofday]. Mockable
    handlers (tests, Checkpoint replay) install a different one that
    returns canned values for determinism. *)

let install (thunk : unit -> 'a) : 'a =
  Effect.Deep.try_with thunk ()
    {
      effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Effects.Time_now ->
              Some
                (fun (k : (a, _) Effect.Deep.continuation) ->
                  Effect.Deep.continue k (Unix.gettimeofday ()))
          | _ -> None);
    }

(** Test handler: returns a fixed value for every [Time_now] perform. *)
let install_fixed ~(now : float) (thunk : unit -> 'a) : 'a =
  Effect.Deep.try_with thunk ()
    {
      effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Effects.Time_now ->
              Some
                (fun (k : (a, _) Effect.Deep.continuation) ->
                  Effect.Deep.continue k now)
          | _ -> None);
    }
