(** See [chaos.mli]. *)

open Types

type llm_kind =
  [ `Rate_limit | `Transient | `Auth | `Context_window | `Overloaded ]

type tool_kind = [ `Transient of string | `Permanent of string ]

type config = {
  seed : int;
  llm_failure_rate : float;
  llm_failure_rate_for : llm_purpose -> float option;
  llm_failure_kinds : llm_kind list;
  tool_failure_rate : float;
  tool_failure_kinds : tool_kind list;
  on_inject : kind:string -> detail:string -> unit;
}

(* Default on_inject: route through Effects.Log when a handler is
   installed (production / dev runs); silently drop otherwise (unit
   tests that don't install Log_handler). *)
let default_on_inject ~kind ~detail =
  try
    Effect.perform
      (Effects.Log (Printf.sprintf "[chaos] %s injected: %s" kind detail))
  with Effect.Unhandled _ -> ()

let default : config =
  {
    seed = 42;
    llm_failure_rate = 0.0;
    llm_failure_rate_for = (fun _ -> None);
    llm_failure_kinds = [];
    tool_failure_rate = 0.0;
    tool_failure_kinds = [];
    on_inject = default_on_inject;
  }

let uniform ?(seed = 42) ~rate () : config =
  {
    seed;
    llm_failure_rate = rate;
    llm_failure_rate_for = (fun _ -> None);
    llm_failure_kinds = [];
    tool_failure_rate = rate;
    tool_failure_kinds = [];
    on_inject = default_on_inject;
  }

let purposes_with_per_purpose_rate (config : config) : (llm_purpose * float) list =
  List.filter_map
    (fun p ->
      match config.llm_failure_rate_for p with
      | Some r when r > 0.0 -> Some (p, r)
      | _ -> None)
    [ `Planner; `Executor; `Recovery; `Summarizer; `Subagent; `Other ]

let is_active config =
  config.llm_failure_rate > 0.0
  || config.tool_failure_rate > 0.0
  || purposes_with_per_purpose_rate config <> []

let show config =
  if not (is_active config) then ""
  else
    let per_purpose = purposes_with_per_purpose_rate config in
    let extras =
      if per_purpose = [] then ""
      else
        let parts =
          List.map
            (fun (p, r) ->
              Printf.sprintf "%s=%.2f" (llm_purpose_to_string p) r)
            per_purpose
        in
        Printf.sprintf " | per-purpose: %s" (String.concat ", " parts)
    in
    Printf.sprintf "chaos(seed=%d, llm=%.2f, tool=%.2f)%s" config.seed
      config.llm_failure_rate config.tool_failure_rate extras

(* ===== RNG state ===== *)

(* We keep a single state per [with_*] call site (one for LLM, one for
   tools). Both are seeded from the same config.seed but diverge after
   first draw — that's fine, we just need determinism per surface for
   a given seed. The state is a mutable ref captured by the handler
   closure. *)
let make_rng seed = Random.State.make [| seed |]

let pick_one (rng : Random.State.t) (xs : 'a list) : 'a option =
  match xs with
  | [] -> None
  | _ ->
      let n = List.length xs in
      Some (List.nth xs (Random.State.int rng n))

(* ===== LLM injection ===== *)

let all_llm_kinds : llm_kind list =
  [ `Rate_limit; `Transient; `Auth; `Context_window; `Overloaded ]

let llm_kind_name = function
  | `Rate_limit -> "rate_limit"
  | `Transient -> "transient"
  | `Auth -> "auth"
  | `Context_window -> "context_window"
  | `Overloaded -> "overloaded"

let llm_error_of_kind (k : llm_kind) : Llm_error.t =
  match k with
  | `Rate_limit ->
      Llm_error.Rate_limit
        { retry_after = Some 0.05; message = "chaos: simulated rate limit" }
  | `Transient ->
      Llm_error.Server_error
        { status = 500; message = "chaos: simulated transient server error" }
  | `Auth ->
      Llm_error.Auth "chaos: simulated auth failure"
  | `Context_window ->
      Llm_error.Context_window "chaos: simulated context window overflow"
  | `Overloaded ->
      Llm_error.Overloaded
        { retry_after = Some 0.05; message = "chaos: simulated overload" }

let with_llm (config : config) (inner : Llm_handler.t) : Llm_handler.t =
  if not (is_active config) then inner
  else
    let rng = make_rng config.seed in
    let pool =
      if config.llm_failure_kinds = [] then all_llm_kinds
      else config.llm_failure_kinds
    in
    fun args ->
      let effective_rate =
        match config.llm_failure_rate_for args.purpose with
        | Some r -> r
        | None -> config.llm_failure_rate
      in
      if effective_rate > 0.0
         && Random.State.float rng 1.0 < effective_rate then begin
        match pick_one rng pool with
        | None -> inner args
        | Some k ->
            let err = llm_error_of_kind k in
            config.on_inject ~kind:"LLM"
              ~detail:
                (Printf.sprintf "%s/%s"
                   (llm_purpose_to_string args.purpose)
                   (llm_kind_name k));
            raise (Llm_error.Llm_api_error err)
      end
      else inner args

(* ===== Tool injection ===== *)

let all_tool_kinds : tool_kind list =
  [
    `Transient "chaos: simulated network blip";
    `Transient "chaos: simulated upstream timeout";
    `Permanent "chaos: simulated tool crash";
    `Permanent "chaos: simulated bad-input";
  ]

let tool_kind_name = function
  | `Transient _ -> "transient"
  | `Permanent _ -> "permanent"

let tool_error_of_kind (tool_name : string) (k : tool_kind) : Error.t =
  match k with
  | `Transient msg ->
      Error.transient ~domain:(Tool tool_name) ~code:"chaos" msg
  | `Permanent msg ->
      Error.permanent ~domain:(Tool tool_name) ~code:"chaos" msg

let with_tool (config : config) (inner : Tool_handler.t) : Tool_handler.t =
  if config.tool_failure_rate <= 0.0 then inner
  else
    (* Different seed offset so LLM + tool RNG sequences don't collide
       and produce identical hit/miss patterns; +1 keeps determinism. *)
    let rng = make_rng (config.seed + 1) in
    let pool =
      if config.tool_failure_kinds = [] then all_tool_kinds
      else config.tool_failure_kinds
    in
    fun args ->
      if Random.State.float rng 1.0 < config.tool_failure_rate then begin
        match pick_one rng pool with
        | None -> inner args
        | Some k ->
            config.on_inject ~kind:"tool"
              ~detail:
                (Printf.sprintf "%s/%s" args.tool.name (tool_kind_name k));
            Error (tool_error_of_kind args.tool.name k)
      end
      else inner args
