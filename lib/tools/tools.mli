(** Built-in tools the agent can call.

    Each tool is a [Types.tool_def] whose handler returns
    [(string, string) result]: [Ok text] for successful observations,
    [Error msg] for failures. *)

open Types

(* ===== Path / directory helpers (used by builtin tools and Plan_act) ===== *)

val require_absolute_path : field:string -> string -> (string, string) result
val ensure_dir : string -> unit

(* ===== Built-in tools ===== *)

val calculator : tool_def
val http_get : tool_def
val current_time : tool_def
val bash : tool_def
val view_file : tool_def
val write_file : tool_def
val str_replace : tool_def
val ask_user : tool_def

(** The pause-tool name; agent loops compare against this to detect
    [ask_user] invocations. *)
val ask_user_name : string

(** All built-in tools, in registration order. *)
val all : tool_def list

(** Look up a tool by name. *)
val find : string -> tool_def option
