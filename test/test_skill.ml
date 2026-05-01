(** Tests for [Skill] frontmatter parser, render_index, and load_skill tool. *)

open Speedjs

let test_skill_parse_basic () =
  let text =
    "---\n\
     name: react-testing\n\
     description: |\n\
    \  React Testing Library best practices.\n\
    \  Use when writing component tests.\n\
     ---\n\
     # body\n\n\
     content here"
  in
  let s = Skill.parse_text text in
  assert (s.name = "react-testing");
  assert (
    String.length s.description > 0
    && String.starts_with ~prefix:"React Testing" s.description);
  assert (String.starts_with ~prefix:"# body" s.body);
  print_endline "✓ Skill.parse_text reads YAML frontmatter + body"

let test_skill_parse_missing_name () =
  let text = "---\ndescription: foo\n---\nbody" in
  match
    try Ok (Skill.parse_text text)
    with Skill.Parse_error msg -> Error msg
  with
  | Error _ ->
      print_endline "✓ Skill.parse_text rejects file with no `name`"
  | Ok _ -> failwith "expected Parse_error for missing name"

let test_skill_render_index_empty () =
  assert (Skill.render_index [] = "");
  print_endline "✓ Skill.render_index returns empty string when no skills"

let test_skill_render_index_with_skills () =
  let skills =
    [
      Skill.{
        name = "alpha";
        description = "First skill.\nLong description.";
        body = "...";
        source_path = "";
      };
      Skill.{
        name = "beta";
        description = "Second skill.";
        body = "...";
        source_path = "";
      };
    ]
  in
  let idx = Skill.render_index skills in
  assert (String.length idx > 0);
  (* render_index returns RAW body — the caller wraps via add_system_block.
     So <available_skills> tag should NOT be in the body. *)
  assert (not (Test_helpers.contains idx "<available_skills>"));
  assert (Test_helpers.contains idx "**alpha**");
  assert (Test_helpers.contains idx "**beta**");
  assert (Test_helpers.contains idx "First skill.");
  print_endline "✓ Skill.render_index produces well-formed index body"

let test_load_skill_tool_dispatch () =
  let skills =
    [
      Skill.{
        name = "demo";
        description = "demo skill";
        body = "BODY CONTENT";
        source_path = "";
      };
    ]
  in
  let tool = Skill.make_load_skill_tool skills in
  assert (tool.name = "load_skill");
  (match tool.handler (`Assoc [ ("name", `String "demo") ]) with
  | Ok body -> assert (body = "BODY CONTENT")
  | Error e -> failwith ("unexpected error: " ^ e));
  (match tool.handler (`Assoc [ ("name", `String "missing") ]) with
  | Error msg -> assert (Test_helpers.contains msg "Unknown skill")
  | Ok _ -> failwith "expected Unknown skill error");
  print_endline
    "✓ load_skill tool returns body on hit, lists available on miss"

let test_load_skill_tool_memoizes_within_run () =
  let skills =
    [
      Skill.{
        name = "demo";
        description = "demo skill";
        body = "BIG BODY CONTENT";
        source_path = "";
      };
      Skill.{
        name = "other";
        description = "other skill";
        body = "OTHER BODY";
        source_path = "";
      };
    ]
  in
  let tool = Skill.make_load_skill_tool skills in
  (match tool.handler (`Assoc [ ("name", `String "demo") ]) with
  | Ok body -> assert (body = "BIG BODY CONTENT")
  | Error e -> failwith ("first call: " ^ e));
  (match tool.handler (`Assoc [ ("name", `String "demo") ]) with
  | Ok msg ->
      assert (msg <> "BIG BODY CONTENT");
      assert (Test_helpers.contains msg "already loaded")
  | Error e -> failwith ("second call: " ^ e));
  (match tool.handler (`Assoc [ ("name", `String "other") ]) with
  | Ok body -> assert (body = "OTHER BODY")
  | Error e -> failwith ("other call: " ^ e));
  let tool2 = Skill.make_load_skill_tool skills in
  (match tool2.handler (`Assoc [ ("name", `String "demo") ]) with
  | Ok body -> assert (body = "BIG BODY CONTENT")
  | Error e -> failwith ("fresh tool: " ^ e));
  print_endline
    "✓ load_skill memoizes per-tool: 2nd call to same skill returns stub"

let run () =
  test_skill_parse_basic ();
  test_skill_parse_missing_name ();
  test_skill_render_index_empty ();
  test_skill_render_index_with_skills ();
  test_load_skill_tool_dispatch ();
  test_load_skill_tool_memoizes_within_run ()
