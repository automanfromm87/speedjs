(** CLI log sink.

    Default writer: stderr. With [setup_file path], all output (including
    streaming text deltas) goes to the file; stdout stays clean for the
    final answer. Module-level mutable so any module can [f "..."] /
    [line "..."] without parameter threading. *)

let writer : (string -> unit) ref =
  ref (fun s ->
      output_string stderr s;
      flush stderr)

let line s = !writer (s ^ "\n")
let f fmt = Printf.ksprintf line fmt

(** Write without trailing newline. Used for streaming text deltas
    coming from the LLM, where line breaks are part of the model output. *)
let str s = !writer s

(** Redirect output to [path]. Truncates the file. Registered [at_exit]
    closes it on shutdown. *)
let setup_file path =
  let oc = open_out_gen [ Open_wronly; Open_creat; Open_trunc ] 0o644 path in
  Printf.eprintf "[log] writing logs to %s\n%!" path;
  writer :=
    (fun s ->
      output_string oc s;
      flush oc);
  at_exit (fun () -> close_out oc)
