(** Composable Log handler chain.

    {[
      let log =
        Log_handler.to_function on_log
        |> Log_handler.with_prefix "[speedjs] "
      Log_handler.install log thunk
    ]} *)

type t = string -> unit

(* ===== Sinks ===== *)

val to_stderr : t
val to_channel : out_channel -> t
val to_function : (string -> unit) -> t

(** Drop all log lines. *)
val null : t

(* ===== Middleware ===== *)

val with_prefix : string -> t -> t
val with_filter : accept:(string -> bool) -> t -> t

(** Fan out one log line to multiple sinks. *)
val tee : t list -> t

(* ===== Install ===== *)

val install : t -> (unit -> 'a) -> 'a
