(lang dune 2.7)
(name goblint-cil)
(implicit_transitive_deps false)
(generate_opam_files true)
(version 1.8.1)
(source (github goblint/cil))
(homepage "https://cil-project.github.io/cil/")
; (documentation "https://goblint.github.io/cil")
(authors "gabriel@kerneis.info")
(maintainers "Michael Schwarz <michael.schwarz93@gmail.com>" "Ralf Vogler <ralf.vogler@gmail.com>")
(license "BSD-3-Clause")

(package
  (name goblint-cil)
  (synopsis "A front-end for the C programming language that facilitates program analysis and transformation")
  (description "\
This is a fork of the 'cil' package needed to build 'goblint'.
Changes:
- Proper support for C99, (#9) and VLAs in particular (#5, #7)
- It uses Zarith instead of the deprecated Num
- Support for more recent OCaml versions (≥ 4.06)
- Large integer constants that do not fit in an OCaml int are represented as a string instead of getting truncated
- Syntactic search extension (#21)
- Some warnings were made optional
- Unmaintained extensions (#30) were removed
- Many bug fixes")
  (depends
    (ocaml (>= 4.04.2))
    ocamlfind
    zarith
    (hevea :with-doc)
    dune
    (odoc :with-doc)
    stdlib-shims
    (ppx_deriving_yojson (>= 3.2))
    yojson
    (batteries (>= 3.2.0))
    conf-perl
  )
  (conflicts cil)
)
