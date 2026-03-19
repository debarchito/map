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
 (description "A visualizable ISA and a Lisp")
 (depends
  (ocaml
   (>= "5.4.1"))
  (menhir
   (>= "20260209"))
  (raylib
   (>= "1.6.0")))
 (tags ("lisp")))

(using menhir 3.0)
