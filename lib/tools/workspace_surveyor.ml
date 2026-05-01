(** Pre-plan workspace surveyor.

    Builds a tight markdown brief of an existing project directory so the
    planner can reason about real file structure instead of guessing
    component paths from memory.

    Two-step pipeline (modeled on helix's WorkspaceSurveyor):

    1. **Collect raw context** via one shell call: directory tree + known
       manifests + README + top-level entry files. Cheap, deterministic,
       no LLM. Capped at ~12K chars.

    2. **Compress to markdown** with a single [Effects.Llm_complete] call.
       The prompt forces a compact form (purpose + tech stack + top-level
       layout) so the result fits cleanly into the planner's system prompt.

    If the project dir is missing or empty, [survey] returns the empty
    string and the planner prompt omits the workspace section entirely.

    Failures are logged and swallowed — a survey error must never block
    planning. The planner falls back to no-context behavior. *)

open Types

(** Cap on raw collected text fed to the summarizer LLM. *)
let max_raw_chars = 12_000

(** Shell script used to collect project state. Mirrors helix's collect
    script (find tree, manifests, README, entry files) and intentionally
    skips the same noise dirs. The placeholder [{project}] is filled in
    via [Printf.sprintf "%s"] before execution. *)
let collect_script_template =
  {|set -u
PROJ='{project}'
if [ ! -d "$PROJ" ] || [ -z "$(ls -A "$PROJ" 2>/dev/null)" ]; then
  exit 0
fi

echo "=== TREE ==="
find "$PROJ" -maxdepth 3 \
  \( -name node_modules -o -name .git -o -name __pycache__ \
     -o -name dist -o -name build -o -name .next -o -name .venv \
     -o -name venv -o -name target -o -name .pytest_cache \
     -o -name _build -o -name .ocamlformat-ignore \) -prune -o \
  -print 2>/dev/null | head -200

echo ""
echo "=== MANIFESTS ==="
for f in package.json pyproject.toml requirements.txt Cargo.toml go.mod \
         tsconfig.json setup.py setup.cfg Pipfile poetry.lock \
         pnpm-workspace.yaml turbo.json vite.config.ts vite.config.js \
         dune-project tailwind.config.js postcss.config.js; do
  if [ -f "$PROJ/$f" ]; then
    echo "--- $f ---"
    head -c 3000 "$PROJ/$f"
    echo ""
  fi
done

echo ""
echo "=== README ==="
for f in README.md README.MD readme.md README.rst README.txt README; do
  if [ -f "$PROJ/$f" ]; then
    echo "--- $f ---"
    head -n 80 "$PROJ/$f"
    break
  fi
done

echo ""
echo "=== ENTRY FILES ==="
for f in $(find "$PROJ" -maxdepth 3 \
            \( -name node_modules -o -name .git -o -name __pycache__ \
               -o -name dist -o -name build -o -name .next \
               -o -name .venv -o -name venv -o -name _build \) -prune -o \
            \( -name 'index.ts' -o -name 'index.tsx' -o -name 'index.js' \
               -o -name 'App.tsx' -o -name 'App.jsx' -o -name 'main.py' \
               -o -name 'main.ts' -o -name 'main.tsx' \
               -o -name '__init__.py' -o -name 'app.py' \
               -o -name 'server.py' \) -print 2>/dev/null | head -10); do
  echo "--- $f ---"
  head -n 30 "$f"
  echo ""
done|}

let summarize_system_prompt =
  "You are a senior engineer briefing a teammate on an unfamiliar repo. \
   Output is read by a planner LLM that hasn't seen any code yet."

let build_summarize_prompt ~working_dir ~raw =
  let body =
    {|Read the raw collected context below and produce a TIGHT markdown
workspace brief for a planning agent. The brief must:

- Start with one line: the project's apparent purpose (inferred from README/manifests).
- List the tech stack: language(s), framework(s), key libraries, build tool. One line.
- Show the top-level layout as a `dir/ — responsibility` list (max ~12 entries, only meaningful directories — skip caches/build).
- List the source files actually present under src/ or equivalent (path + 5-word note each). Critical: the planner uses this to reference files by absolute path, so include the full path under the project root.
- Mention any test framework + test directory if present.
- Stay under 400 words. No fluff, no preamble. Markdown only.

If the raw context is clearly NOT a code project (e.g. just docs, just data), say so in one line and stop.

|}
  in
  body ^ "Project root: " ^ working_dir ^ "\n\nRaw context:\n```\n" ^ raw
  ^ "\n```"

(** Substitute the [{project}] placeholder in the collect script. We use a
    plain string template (not a format string) so the body can contain
    bare [%] without escaping ([head -c 3000] etc.). *)
let render_collect_script working_dir =
  let placeholder = "{project}" in
  let plen = String.length placeholder in
  let template = collect_script_template in
  let buf = Buffer.create (String.length template + 64) in
  let i = ref 0 in
  let len = String.length template in
  while !i < len do
    if !i + plen <= len && String.sub template !i plen = placeholder then begin
      Buffer.add_string buf working_dir;
      i := !i + plen
    end
    else begin
      Buffer.add_char buf template.[!i];
      incr i
    end
  done;
  Buffer.contents buf

(** Run the collection script via [Sys.command] (no LLM, no tool effect).
    Returns the script's stdout, or "" on any error. *)
let collect_raw ~working_dir : string =
  let script = render_collect_script working_dir in
  (* Write the script to a tempfile rather than passing it through [-c]
     so we don't have to deal with shell-quoting the heredoc body. *)
  let tmp = Filename.temp_file "speedjs-survey-" ".sh" in
  let cleanup () = try Sys.remove tmp with _ -> () in
  try
    let oc = open_out tmp in
    output_string oc script;
    close_out oc;
    let cmd =
      Printf.sprintf "/bin/sh %s 2>/dev/null" (Filename.quote tmp)
    in
    let ic = Unix.open_process_in cmd in
    let buf = Buffer.create 4096 in
    (try
       while true do
         Buffer.add_channel buf ic 4096
       done
     with End_of_file -> ());
    let _ = Unix.close_process_in ic in
    cleanup ();
    let raw = Buffer.contents buf in
    if String.length raw <= max_raw_chars then raw
    else
      String.sub raw 0 max_raw_chars
      ^ Printf.sprintf "\n... (truncated %d chars)"
          (String.length raw - max_raw_chars)
  with e ->
    cleanup ();
    Effect.perform
      (Effects.Log
         (Printf.sprintf "[surveyor] collect_raw failed: %s"
            (Printexc.to_string e)));
    ""

(** Compress raw context into a tight markdown brief via one LLM call.
    Goes through [Effects.Llm_complete] so the production handler stack
    (cost tracking, retry, walltime, budget) applies automatically. *)
let summarize ~working_dir ~raw : string =
  let prompt = build_summarize_prompt ~working_dir ~raw in
  let args : llm_call_args =
    {
      messages = [ user_text_message prompt ];
      tools = [];
      system_override = Some summarize_system_prompt;
      tool_choice = Tc_auto;
    }
  in
  let response = Effect.perform (Effects.Llm_complete args) in
  String.trim (Agent.extract_final_text response.content)

(** Top-level: scan [working_dir] and return a markdown workspace brief.
    Empty string when the directory doesn't exist, is empty, or any step
    fails — the caller (planner prompt) treats "" as "no brief available"
    and skips the section. *)
let survey ~working_dir : string =
  if not (Sys.file_exists working_dir) then begin
    Effect.perform
      (Effects.Log
         (Printf.sprintf "[surveyor] working_dir does not exist: %s"
            working_dir));
    ""
  end
  else if not (Sys.is_directory working_dir) then begin
    Effect.perform
      (Effects.Log
         (Printf.sprintf "[surveyor] working_dir is not a directory: %s"
            working_dir));
    ""
  end
  else begin
    Effect.perform
      (Effects.Log
         (Printf.sprintf "[surveyor] scanning %s ..." working_dir));
    let raw = collect_raw ~working_dir in
    if raw = "" then begin
      Effect.perform
        (Effects.Log "[surveyor] empty workspace — skipping summarize");
      ""
    end
    else begin
      Effect.perform
        (Effects.Log
           (Printf.sprintf "[surveyor] collected %d chars, summarizing..."
              (String.length raw)));
      try
        let brief = summarize ~working_dir ~raw in
        Effect.perform
          (Effects.Log
             (Printf.sprintf "[surveyor] brief produced (%d chars)"
                (String.length brief)));
        brief
      with e ->
        Effect.perform
          (Effects.Log
             (Printf.sprintf "[surveyor] summarize failed: %s"
                (Printexc.to_string e)));
        ""
    end
  end

(** Wrap a brief into the [<workspace_brief>] block injected into planner
    + executor system prompts. Returns "" when brief is empty so callers
    can append unconditionally. *)
let render_brief ~working_dir brief =
  if brief = "" then ""
  else
    Printf.sprintf
      "\n\n<workspace_brief>\nProject root: %s\n\n%s\n</workspace_brief>"
      working_dir brief
