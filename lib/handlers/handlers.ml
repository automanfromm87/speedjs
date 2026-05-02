(** Test-only effect handlers.

    Production runtime composes [Llm_handler.install] / [Tool_handler.install]
    / [Log_handler.install] in [bin/setup.ml]. The two handlers below are
    used by tests:

    - [mock] — canned LLM responses + canned tool results
    - [silent] — drops [Effects.Log] (composable, transparent for everything else) *)

open Types
open Effect.Deep

type cost_state = Types.cost_state = {
  mutable input_tokens : int;
  mutable output_tokens : int;
  mutable cache_creation_tokens : int;
  mutable cache_read_tokens : int;
  mutable calls : int;
  mutable steps : int;
  mutable tool_calls : int;
  mu : Mutex.t;
}

let new_cost_state = Types.new_cost_state
let cost_usd = Types.cost_usd

(* ===== mock handler =====

   Pre-supplied LLM responses + tool results. Tool results are also typed:
   pass [(name, Ok "obs")] or [(name, Error "msg")]. *)

let mock ~(llm_responses : llm_response list)
    ?(tool_results : (string * tool_handler_result) list = [])
    ?(on_log = fun _ -> ()) f =
  let pending_llm : llm_response list ref = ref llm_responses in
  try_with f ()
    {
      effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Effects.Llm_complete _ ->
              Some
                (fun (k : (a, _) continuation) ->
                  match !pending_llm with
                  | [] ->
                      failwith
                        "mock: no more LLM responses (agent called LLM \
                         beyond what was supplied)"
                  | r :: rest ->
                      pending_llm := rest;
                      continue k r)
          | Effects.Tool_calls uses ->
              Some
                (fun (k : (a, _) continuation) ->
                  let lookup name =
                    match List.assoc_opt name tool_results with
                    | Some r -> r
                    | None ->
                        Error
                          (Printf.sprintf
                             "[mock] no result configured for tool %s"
                             name)
                  in
                  let results =
                    List.map (fun (id, name, _input) -> (id, lookup name)) uses
                  in
                  continue k results)
          | Effects.Log msg ->
              Some
                (fun (k : (a, _) continuation) ->
                  on_log msg;
                  continue k ())
          | _ -> None);
    }

(* ===== silent: drops Log effects, otherwise transparent ===== *)

let silent f =
  try_with f ()
    {
      effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Effects.Log _ ->
              Some (fun (k : (a, _) continuation) -> continue k ())
          | _ -> None);
    }
