(** Tiny parallel-map utility built on OCaml's [Thread] module.

    Threads share GC and runtime lock. They give true parallelism only when
    a thread blocks on a syscall (HTTP/IO/process), which is exactly our
    tool case (curl shell-out, bash subprocess). For pure CPU-bound work
    this would not parallelize. *)

(** Run [f] on each item in parallel using a dedicated thread per item.
    Returns results in input order. *)
let map_threaded (f : 'a -> 'b) (items : 'a list) : 'b list =
  let arr = Array.of_list items in
  let n = Array.length arr in
  if n = 0 then []
  else if n = 1 then [ f arr.(0) ]
  else begin
    let results : 'b option array = Array.make n None in
    let exns : exn option array = Array.make n None in
    let threads =
      Array.init n (fun i ->
          Thread.create
            (fun () ->
              try results.(i) <- Some (f arr.(i))
              with e -> exns.(i) <- Some e)
            ())
    in
    Array.iter Thread.join threads;
    Array.to_list
      (Array.mapi
         (fun i r ->
           match r with
           | Some v -> v
           | None -> (
               match exns.(i) with
               | Some e -> raise e
               | None -> failwith "Parallel.map_threaded: thread vanished"))
         results)
  end
