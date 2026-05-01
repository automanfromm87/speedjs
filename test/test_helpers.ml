(** Shared utilities used across test modules. *)

let contains haystack needle =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  let rec scan i =
    if i + nlen > hlen then false
    else if String.sub haystack i nlen = needle then true
    else scan (i + 1)
  in
  scan 0
