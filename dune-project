(lang dune 3.19)

(using menhir 3.0)

(name ocl)

(generate_opam_files true)

(package
 (name ocl)
 (synopsis "OCaml Logic expression calculator")
 (description "OCaml Logic expression calculator built with OCaml and Menhir")
 (depends ocaml dune menhir alcotest)
 (tags
  ("logic" "calculator" "menhir")))
