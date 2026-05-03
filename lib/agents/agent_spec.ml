(** See [agent_spec.mli]. Pure data; no behavior. *)

open Types

type terminal =
  | Free_text
  | Tool of { name : string }

type t = {
  name : string;
  mode : agent_mode;
  system_prompt : string;
  system_blocks : (string * string) list;
  env : (string * string) list;
  tools : tool_def list;
  strategy : Context.Strategy.t;
  max_iters : int;
  model : string option;
  purpose : llm_purpose;
  terminal : terminal;
  force_terminal_in_last_n : int;
}

type validated = {
  spec : t;
  visible_tools : tool_def list;
}

(** Find tool names that appear more than once in [tools]. Returns
    the unique colliding names. Anywhere downstream (LLM tool-choice,
    [Step]'s pause-cap lookup, [Tool_handler.dispatch_one]) a
    duplicate name causes silent shadowing of all but the first
    matching tool — surface that explicitly at the spec boundary. *)
let duplicate_tool_names (tools : tool_def list) : string list =
  let counts = Hashtbl.create 16 in
  List.iter
    (fun (t : tool_def) ->
      let n = try Hashtbl.find counts t.name with Not_found -> 0 in
      Hashtbl.replace counts t.name (n + 1))
    tools;
  Hashtbl.fold
    (fun name n acc -> if n > 1 then name :: acc else acc)
    counts []
  |> List.sort compare

let validate (spec : t) : (validated, string) Result.t =
  match duplicate_tool_names spec.tools with
  | _ :: _ as dups ->
      Error
        (Printf.sprintf
           "spec.tools contains duplicate name(s) [%s] — would silently \
            shadow during dispatch / pause-capability lookup"
           (String.concat "," dups))
  | [] ->
      let visible = tools_for_mode spec.mode spec.tools in
      (match spec.terminal with
       | Free_text -> Ok { spec; visible_tools = visible }
       | Tool { name } -> (
           match List.find_opt (fun (t : tool_def) -> t.name = name) visible with
           | None ->
               let in_full =
                 List.exists (fun (t : tool_def) -> t.name = name) spec.tools
               in
               Error
                 (Printf.sprintf
                    "spec.terminal=%S not visible in mode=%s after \
                     tools_for_mode filtering (declared in spec.tools? %b — \
                     if true, the tool's allowed_modes excludes %s)"
                    name (agent_mode_to_string spec.mode) in_full
                    (agent_mode_to_string spec.mode))
           | Some t when not (List.mem Terminal t.capabilities) ->
               Error
                 (Printf.sprintf
                    "spec.terminal=%S exists but lacks Terminal capability \
                     — its handler will be invoked instead of the loop \
                     intercepting the call"
                    name)
           | Some _ -> Ok { spec; visible_tools = visible }))

let default_max_iters = 100

let default_system_prompt =
  "You are a helpful assistant. Use the available tools when they help \
   answer the user's question. Think step-by-step and only call tools when \
   they are needed."

let make ?(name = "agent") ?(mode = Executor)
    ?(system_prompt = default_system_prompt) ?(system_blocks = [])
    ?(env = []) ?(strategy = Context.Strategy.flat)
    ?(max_iters = default_max_iters) ?(model : string option)
    ?(purpose : llm_purpose = `Other)
    ?(terminal = Free_text)
    ?(force_terminal_in_last_n = 2) ~tools () =
  {
    name;
    mode;
    system_prompt;
    system_blocks;
    env;
    tools;
    strategy;
    max_iters;
    model;
    purpose;
    terminal;
    force_terminal_in_last_n;
  }

let with_skill (s : Skill.t) (t : t) : t =
  { t with system_blocks = (s.name, s.body) :: t.system_blocks }

let with_max_iters n t = { t with max_iters = n }
let with_mode m t = { t with mode = m }
let with_model m t = { t with model = m }
let with_terminal x t = { t with terminal = x }
let with_terminal_tool name t = { t with terminal = Tool { name } }
let with_strategy s t = { t with strategy = s }
let with_system_prompt p t = { t with system_prompt = p }
let add_tool tool t = { t with tools = t.tools @ [ tool ] }
let add_tools more t = { t with tools = t.tools @ more }
let add_system_block (name, body) t =
  { t with system_blocks = t.system_blocks @ [ (name, body) ] }
let add_env ~tag ~body t = { t with env = t.env @ [ (tag, body) ] }

let show (t : t) : string =
  let term =
    match t.terminal with
    | Free_text -> "free_text"
    | Tool { name } -> Printf.sprintf "tool=%s" name
  in
  let total_tools = List.length t.tools in
  let visible_tools =
    List.length (tools_for_mode t.mode t.tools)
  in
  let tools_str =
    if visible_tools = total_tools then string_of_int total_tools
    else Printf.sprintf "%d/%d" visible_tools total_tools
  in
  let endgame =
    match t.terminal with
    | Free_text -> ""
    | Tool _ ->
        Printf.sprintf ", force_last=%d" t.force_terminal_in_last_n
  in
  Printf.sprintf
    "spec(name=%s, mode=%s, tools=%s, blocks=%d, env=%d, max_iters=%d, \
     strategy=%s, terminal=%s%s)"
    t.name (agent_mode_to_string t.mode) tools_str
    (List.length t.system_blocks) (List.length t.env) t.max_iters
    (Context.Strategy.label t.strategy) term endgame
