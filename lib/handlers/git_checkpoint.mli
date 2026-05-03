(** Per-task git transaction: capture HEAD, commit on success, hard-reset
    + clean on failure. Lets the chaos / retry layers absorb side effects
    that span the file system without leaving orphan files or partially
    edited content from earlier failed attempts.

    Contract: [cwd] must be the root of a git repo with at least one
    commit and a clean working tree (the caller — [Setup] / [Plan_act] —
    runs {!validate_clean_repo} once at startup, then {!create} per task). *)

type t

(** Capture [HEAD] at [cwd]. Caller should have validated the tree first;
    [create] still propagates the error if [cwd] isn't a usable repo. *)
val create : cwd:string -> (t, string) result

(** Restore the working tree to the captured HEAD and remove untracked
    non-ignored files (so chaos's orphan files like [api/api/notes.py]
    disappear). [.gitignore]'d paths ([node_modules], [.venv]) are left
    alone. Logs warnings on partial failures, never raises. *)
val rollback : t -> unit

(** Stage all changes and create a commit with [message]. No-op (returns
    [Ok prev_head]) when there's nothing to commit. *)
val commit : t -> message:string -> (string, string) result

(** True when [cwd] is a git repo (has a [.git] dir or file). Cheap —
    no shell-out. *)
val is_git_repo : cwd:string -> bool

(** Validate that [cwd] is a git repo with at least one commit and a
    clean working tree. Returns a user-facing error message on failure
    — startup paths print this and exit, so it must be self-explanatory. *)
val validate_clean_repo : cwd:string -> (unit, string) result

(** Create an empty initial commit at [cwd] if the repo has no commits
    yet. Used by startup to ensure {!create} can read [HEAD]. No-op
    when commits already exist. *)
val ensure_initial_commit : cwd:string -> (unit, string) result
