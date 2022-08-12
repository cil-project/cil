(lang dune 2.7)
(name goblint-cil)
(implicit_transitive_deps false)
(generate_opam_files true)
(version 2.0.0)
(source (github goblint/cil))
; (documentation "https://goblint.github.io/cil")
(authors "Gabriel Kerneis" "Ralf Vogler" "Michael Schwarz" "Simmo Saan")
(maintainers "Michael Schwarz <michael.schwarz93@gmail.com>" "Simmo Saan <simmo.saan@gmail.com>")
(license "BSD-3-Clause")

(package
  (name goblint-cil)
  (synopsis "A front-end for the C programming language that facilitates program analysis and transformation")
  (description "\
This is a fork of the 'cil' package used for 'goblint'. Major changes include:
* Support for C99 and C11.
* Compatibility with modern OCaml versions.
* Use Zarith instead of Num and use that for integer constants.
* Improved locations with columns and spans.
* Removal of unmaintained extensions and MSVC support.
* Use dune instead of make and ocamlbuild.
* Many bug fixes.")
  (depends
    (ocaml (>= 4.05.0))
    (ocamlfind :with-test)
    zarith
    (hevea :with-doc)
    dune
    dune-configurator
    (odoc :with-doc)
    stdlib-shims
    (ppx_deriving_yojson (>= 3.2))
    yojson
    conf-perl
    cppo
  )
  (conflicts cil) ; only because we both install the cilly executable
)
