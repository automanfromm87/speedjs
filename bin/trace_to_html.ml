(** Standalone tool: convert an existing trace.ndjson into a
    self-contained HTML report.

    Usage:
      dune exec trace_to_html -- <input.ndjson> [output.html]

    If [output.html] is omitted, defaults to [<input>.html] (same dir,
    [.ndjson] replaced with [.html]). *)

let usage () =
  prerr_endline
    "usage: trace_to_html <input.ndjson> [output.html]";
  exit 2

let default_out_path ndjson =
  let base =
    try Filename.chop_extension ndjson
    with Invalid_argument _ -> ndjson
  in
  base ^ ".html"

let () =
  let argv = Sys.argv in
  if Array.length argv < 2 then usage ();
  let ndjson_path = argv.(1) in
  let out_path =
    if Array.length argv >= 3 then argv.(2)
    else default_out_path ndjson_path
  in
  match
    Speedjs.Trace.write_html_report ~ndjson_path ~out_path
  with
  | Ok () ->
      Printf.printf "→ %s\n" out_path;
      exit 0
  | Error msg ->
      Printf.eprintf "trace_to_html: %s\n" msg;
      exit 1
