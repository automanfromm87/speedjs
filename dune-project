(lang dune 3.20)

(name speedjs)

(generate_opam_files true)

(source
 (github yourname/speedjs))

(authors "He Bin")

(maintainers "He Bin")

(license MIT)

(package
 (name speedjs)
 (synopsis "Effect-based LLM agent runtime")
 (description
  "First effect-based ReAct agent runtime, built on OCaml 5 algebraic effects.")
 (depends
  (ocaml
   (>= 5.1.0))
  dune
  yojson)
 (tags
  (llm agent effects ocaml5 react)))
