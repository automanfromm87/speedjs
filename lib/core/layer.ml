(** Generic thunk-wrapper composition.

    A "layer" is any function with shape [(unit -> 'a) -> 'a]: it takes
    a thunk, runs it under some handler, returns the value. Effect-handler
    installers ([Llm_handler.install], [Tool_handler.install],
    [Log_handler.install], [Governor.install]) match this shape once
    curried with their config; [Handlers.silent] matches it directly.

    Composition forms the handler-stack pattern: outer layers wrap
    inner layers wrap the agent thunk. *)

type 'a t = (unit -> 'a) -> 'a

(** Wrap [thunk] with [layers], FIRST element OUTERMOST. Reads
    top-to-bottom in source as outer-to-inner. *)
let rec compose (layers : 'a t list) (thunk : unit -> 'a) : 'a =
  match layers with
  | [] -> thunk ()
  | l :: rest -> l (fun () -> compose rest thunk)

(** [enabled cond layer]: include [layer] when [cond] is true. *)
let enabled (cond : bool) (layer : 'a t) : 'a t option =
  if cond then Some layer else None

(** [of_opt opt build]: when [opt = Some v], include [build v] as a
    layer; otherwise none. Convenient for layers gated on an optional
    config value. *)
let of_opt opt build : 'a t option = Option.map build opt

(** Drop the [None] entries from a list of optional layers. *)
let collect (xs : 'a t option list) : 'a t list =
  List.filter_map Fun.id xs
