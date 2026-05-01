(** Skill — lazy-loaded chunk of domain expertise the agent can pull on demand.

    Modeled after helix's skill mechanism (which itself mirrors Claude Code's
    "skill" feature). Each skill is a markdown file with YAML frontmatter.
    The [name] + [description] live in the system-prompt index; the heavy
    [body] only enters context when the model decides it's relevant.

    File format:

    {v
    ---
    name: react-testing
    description: |
      React Testing Library best practices.
      Use when writing or fixing React component tests.
    ---
    # React Testing Library
    ...
    v}
*)

type t = {
  name : string;
  description : string;
  body : string;
  source_path : string;
}

exception Parse_error of string

(* ===== Hand-rolled YAML-subset parser =====

   Supports the two forms we actually use:
   - scalar: [key: value]
   - block scalar: [key: |] followed by indented continuation lines

   Anything fancier (anchors, lists, nested maps) raises [Parse_error]. *)

let split_lines s =
  String.split_on_char '\n' s
  |> List.map (fun l ->
         (* Strip trailing CR for Windows line endings *)
         if String.length l > 0 && l.[String.length l - 1] = '\r' then
           String.sub l 0 (String.length l - 1)
         else l)

let leading_spaces s =
  let n = String.length s in
  let rec loop i =
    if i >= n then n else if s.[i] = ' ' then loop (i + 1) else i
  in
  loop 0

let strip = String.trim

let strip_quotes s =
  let n = String.length s in
  if n >= 2
     && ((s.[0] = '"' && s.[n - 1] = '"')
         || (s.[0] = '\'' && s.[n - 1] = '\''))
  then String.sub s 1 (n - 2)
  else s

(** Parse YAML-subset frontmatter lines into a (key, value) list. *)
let parse_simple_yaml (lines : string list) : (string * string) list =
  let arr = Array.of_list lines in
  let n = Array.length arr in
  let out = ref [] in
  let i = ref 0 in
  while !i < n do
    let line = arr.(!i) in
    if strip line = "" then incr i
    else
      match String.index_opt line ':' with
      | None ->
          raise
            (Parse_error
               (Printf.sprintf "unparseable frontmatter line: %S" line))
      | Some idx ->
          let key = strip (String.sub line 0 idx) in
          let rest =
            strip (String.sub line (idx + 1) (String.length line - idx - 1))
          in
          if key = "" then
            raise
              (Parse_error (Printf.sprintf "empty key in line: %S" line));
          if rest = "|" || rest = ">" then begin
            (* Block scalar: collect indented continuation lines. *)
            incr i;
            let indent = ref None in
            let block = Buffer.create 256 in
            let first_added = ref false in
            let stop = ref false in
            while (not !stop) && !i < n do
              let nxt = arr.(!i) in
              if strip nxt = "" then begin
                if !first_added then Buffer.add_char block '\n';
                incr i
              end
              else
                let lead = leading_spaces nxt in
                let cur_indent =
                  match !indent with
                  | Some v -> v
                  | None ->
                      let v = if lead > 0 then lead else 1 in
                      indent := Some v;
                      v
                in
                if lead < cur_indent then stop := true
                else begin
                  if !first_added then Buffer.add_char block '\n';
                  Buffer.add_string block
                    (String.sub nxt cur_indent (String.length nxt - cur_indent));
                  first_added := true;
                  incr i
                end
            done;
            let value =
              if rest = "|" then Buffer.contents block
              else
                (* > folded: replace newlines with spaces *)
                String.map (fun c -> if c = '\n' then ' ' else c)
                  (Buffer.contents block)
            in
            (* Trim trailing whitespace (helix's convention). *)
            let trimmed_right =
              let len = String.length value in
              let j = ref (len - 1) in
              while !j >= 0 && (value.[!j] = ' ' || value.[!j] = '\n') do
                decr j
              done;
              String.sub value 0 (!j + 1)
            in
            out := (key, trimmed_right) :: !out
          end
          else begin
            out := (key, strip_quotes rest) :: !out;
            incr i
          end
  done;
  List.rev !out

let delim = "---"

(** Split markdown text into (frontmatter_metadata, body). *)
let split_frontmatter (text : string) : (string * string) list * string =
  let lines = split_lines text in
  match lines with
  | [] | _ when List.length lines = 0 ->
      raise (Parse_error "empty file")
  | first :: _ when strip first <> delim ->
      raise (Parse_error "missing leading `---` frontmatter delimiter")
  | _ ->
      let arr = Array.of_list lines in
      let n = Array.length arr in
      let end_idx = ref (-1) in
      let i = ref 1 in
      while !end_idx < 0 && !i < n do
        if strip arr.(!i) = delim then end_idx := !i;
        incr i
      done;
      if !end_idx < 0 then
        raise (Parse_error "missing closing `---` frontmatter delimiter");
      let frontmatter_lines =
        Array.sub arr 1 (!end_idx - 1) |> Array.to_list
      in
      let body_lines =
        Array.sub arr (!end_idx + 1) (n - !end_idx - 1) |> Array.to_list
      in
      let meta = parse_simple_yaml frontmatter_lines in
      let body = String.concat "\n" body_lines in
      (meta, body)

(** Parse a markdown skill file's contents into a [Skill.t]. *)
let parse_text ?(source_path = "") (text : string) : t =
  let meta, body = split_frontmatter text in
  let assoc_or_empty key =
    match List.assoc_opt key meta with Some s -> strip s | None -> ""
  in
  let name = assoc_or_empty "name" in
  let description = assoc_or_empty "description" in
  if name = "" then
    raise
      (Parse_error
         (Printf.sprintf "skill at %s has no `name`"
            (if source_path = "" then "?" else source_path)));
  if description = "" then
    raise
      (Parse_error (Printf.sprintf "skill %s has no `description`" name));
  { name; description; body = String.trim body; source_path }

let parse_file (path : string) : (t, string) Result.t =
  try
    let ic = open_in path in
    let n = in_channel_length ic in
    let text = really_input_string ic n in
    close_in ic;
    Ok (parse_text ~source_path:path text)
  with
  | Parse_error msg -> Error msg
  | Sys_error msg -> Error msg

(* ===== Filesystem registry =====

   Layout:
     <root>/
       react-testing/
         SKILL.md
       tailwind-patterns/
         SKILL.md
       ...

   Skills with parse errors are logged + skipped — a single broken skill
   mustn't stall startup. *)

(** Scan a root directory for [<name>/SKILL.md] files. Returns parsed
    skills; bad files are logged via [on_error] and excluded. *)
let load_dir ?(on_error = fun _ -> ()) (root : string) : t list =
  if not (Sys.file_exists root) then begin
    on_error (Printf.sprintf "skills root %s does not exist" root);
    []
  end
  else if not (Sys.is_directory root) then begin
    on_error (Printf.sprintf "skills root %s is not a directory" root);
    []
  end
  else
    let entries =
      try Sys.readdir root |> Array.to_list with _ -> []
    in
    let skill_dirs =
      entries
      |> List.filter (fun name ->
             let p = Filename.concat root name in
             Sys.is_directory p)
      |> List.sort compare
    in
    List.filter_map
      (fun dir ->
        let skill_file = Filename.concat (Filename.concat root dir) "SKILL.md"
        in
        if not (Sys.file_exists skill_file) then None
        else
          match parse_file skill_file with
          | Ok skill -> Some skill
          | Error msg ->
              on_error
                (Printf.sprintf "skipping invalid skill at %s: %s" skill_file msg);
              None)
      skill_dirs

(* ===== load_skill tool builder =====

   Returns a [tool_def] that, when called by the model, produces the full
   body of the named skill as a tool_result. The handler captures the
   skill list at construction time (no live mutation). *)

let load_skill_tool_name = "load_skill"

let make_load_skill_tool (skills : t list) : Types.tool_def =
  let table = Hashtbl.create (List.length skills) in
  List.iter (fun s -> Hashtbl.replace table s.name s) skills;
  let names = List.map (fun s -> s.name) skills in
  (* Per-run memo: skill name -> first-load message preview. Subsequent
     loads of the same skill return a short stub instead of the full
     ~9KB body, which would otherwise pile up in conversation history
     across plan-act sub-agents (planner / executor / recovery each
     freshly invoke load_skill, but the body is already cached by the
     LLM via prompt-cache). *)
  let loaded : (string, unit) Hashtbl.t = Hashtbl.create 4 in
  Types.
    {
      idempotent = true;
      timeout_sec = Some 1.0;
      category = "meta";
      name = load_skill_tool_name;
      description =
        "Load a skill's full body into the conversation. Use this when the \
         current task falls into the skill's domain (see the skill index in \
         the system prompt). Don't pre-load skills speculatively — only \
         when you're about to act in that domain. The body stays in context \
         for the rest of the task. (Subsequent calls in the same run \
         return a short stub — the original body is already in your \
         conversation history.)";
      input_schema =
        `Assoc
          [
            ("type", `String "object");
            ( "properties",
              `Assoc
                [
                  ( "name",
                    `Assoc
                      [
                        ("type", `String "string");
                        ( "description",
                          `String
                            "Exact skill name from the index (e.g. \
                             \"react-testing\")" );
                      ] );
                ] );
            ("required", `List [ `String "name" ]);
          ];
      handler =
        (fun input ->
          match input with
          | `Assoc fields -> (
              match List.assoc_opt "name" fields with
              | Some (`String name) -> (
                  match Hashtbl.find_opt table name with
                  | Some s ->
                      if Hashtbl.mem loaded name then
                        Ok
                          (Printf.sprintf
                             "[skill %S already loaded earlier in this \
                              run — full body remains in your conversation \
                              history; consult it there]"
                             name)
                      else begin
                        Hashtbl.add loaded name ();
                        Ok s.body
                      end
                  | None ->
                      let avail =
                        match names with
                        | [] -> "(none)"
                        | _ -> String.concat ", " names
                      in
                      Error
                        (Printf.sprintf "Unknown skill %S. Available: %s"
                           name avail))
              | _ -> Error "missing 'name' field")
          | _ -> Error "input must be JSON object");
    }

(** Build the [<available_skills>] block we splice into the system prompt.

    Returns empty string when no skills, so callers can append unconditionally. *)
let render_index (skills : t list) : string =
  match skills with
  | [] -> ""
  | _ ->
      let header =
        "<available_skills>\n\
         Domain knowledge libraries you can pull on demand via the \
         `load_skill` tool. The body becomes available on the next turn and \
         stays in context for the rest of the task. Only load a skill when \
         the work falls into its domain — don't load all of them.\n"
      in
      let lines =
        List.map
          (fun s ->
            let first_line =
              match String.split_on_char '\n' s.description with
              | l :: _ -> strip l
              | [] -> ""
            in
            Printf.sprintf "- **%s** — %s" s.name first_line)
          skills
      in
      header ^ String.concat "\n" lines ^ "\n</available_skills>"
