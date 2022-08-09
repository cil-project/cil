(lang dune 2.7)
(name goblint-cil)
(implicit_transitive_deps false)
(generate_opam_files true)
(version 1.8.2)
(source (github goblint/cil))
; (documentation "https://goblint.github.io/cil")
(authors "gabriel@kerneis.info")
(maintainers "Michael Schwarz <michael.schwarz93@gmail.com>" "Simmo Saan <simmo.saan@gmail.com>" "Ralf Vogler <ralf.vogler@gmail.com>")
(license "BSD-3-Clause")

(package
  (name goblint-cil)
  (synopsis "A front-end for the C programming language that facilitates program analysis and transformation")
  (description "\
This is a fork of the 'cil' package needed to build 'goblint'.
Changes:
- Proper support for C99 and VLAs in particular
- It uses Zarith instead of the deprecated Num
- Support for more recent OCaml versions (≥ 4.06)
- Large integer constants that do not fit in an OCaml int are represented as a string instead of getting truncated
- Syntactic search extension
- More precise locations (with columns)
- Some warnings were made optional
- Unmaintained extensions were removed
- Many bug fixes")
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
