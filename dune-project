(lang dune 2.00)
(name goblint-cil)
(implicit_transitive_deps false)
(generate_opam_files true)
(version 1.7.8)
(source (github goblint/cil))
(homepage "https://cil-project.github.io/cil/")
; (documentation "https://goblint.github.io/cil")
(authors "gabriel@kerneis.info")
(maintainers "Michael Schwarz <michael.schwarz93@gmail.com>" "Ralf Vogler <ralf.vogler@gmail.com>")
(license BSD)

(package
  (name goblint-cil)
  (synopsis "A front-end for the C programming language that facilitates program analysis and transformation")
  (description "\
This is a fork of the 'cil' package needed to build 'goblint'.
Changes:
- some warnings are made optional
- truncated integer constants have a string representation
- compiles with OCaml >=4.06.0, use zarith instead of num")
  (depends
    (ocaml (>= 4.02.3))
    ocamlfind
    zarith
    (hevea :with-doc)
    (dune (>= 2.3.0))
    (odoc :with-doc)
    stdlib-shims
    ppx_deriving_yojson
    yojson
    batteries
  )
  (conflicts cil)
)
