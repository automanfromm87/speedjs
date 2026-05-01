(** Skill — lazy-loaded chunk of domain expertise. Each skill is a
    markdown file with YAML frontmatter at [<root>/<name>/SKILL.md]. The
    name + description live in the system-prompt index; the heavy body
    only enters context when the model decides it's relevant via the
    [load_skill] tool. *)

type t = {
  name : string;
  description : string;
  body : string;
  source_path : string;
}

(** Scan a root directory for [<name>/SKILL.md] files. Skills with parse
    errors are reported via [on_error] and excluded — a single broken
    skill mustn't stall startup. *)
val load_dir : ?on_error:(string -> unit) -> string -> t list

(** Build a [load_skill] tool whose dispatch returns the requested skill
    body. Captures [skills] at construction time. *)
val make_load_skill_tool : t list -> Types.tool_def

(** The [<available_skills>] block to splice into the system prompt.
    Returns [""] when no skills, so callers can append unconditionally. *)
val render_index : t list -> string

val load_skill_tool_name : string

(* ===== Internal (exposed for tests) ===== *)

(** Parse skill markdown text directly. Used by tests; production code
    should use [load_dir] which reads from disk. *)
val parse_text : ?source_path:string -> string -> t

exception Parse_error of string
