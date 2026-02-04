(lang dune 3.21)

(name map)

(generate_opam_files true)

(source
 (github debarchito/map))

(authors "Debarchito Nath <debarchiton@proton.me>")

(maintainers "Debarchito Nath <debarchiton@proton.me>")

(license LICENSE)

(package
 (name map)
 (description "A small scheme-like Lisp")
 (depends
  (ocaml
   (>= "5.4.0"))
  (menhir
   (>= "20250912")))
 (tags
  ("lisp" "scheme")))

(using menhir 3.0)
