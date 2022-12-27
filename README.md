C Intermediate Language (CIL)
============================
[![tests workflow status](https://github.com/goblint/cil/actions/workflows/tests.yml/badge.svg)](https://github.com/goblint/cil/actions/workflows/tests.yml)
[![docs workflow status](https://github.com/goblint/cil/actions/workflows/docs.yml/badge.svg)](https://goblint.github.io/cil/)
[![GitHub release status](https://img.shields.io/github/v/release/goblint/cil)](https://github.com/goblint/cil/releases)
[![opam package status](https://badgen.net/opam/v/goblint-cil)](https://opam.ocaml.org/packages/goblint-cil)

CIL is a front-end for the C programming language that facilitates
program analysis and transformation. CIL will parse and typecheck a
program, and compile it into a simplified subset of C.

`goblint-cil` is a fork of CIL that supports C99, C11 as well as most of the
extensions of the GNU C. It makes many changes to the original CIL in an effort
to modernize it and keep up with the latest versions of the C language. Here is
an incomplete list of some of the ways `goblint-cil` improves upon CIL:
* Support for C99 and C11.
* Compatibility with modern OCaml versions.
* Use Zarith instead of Num and use that for integer constants.
* Improved locations with columns and spans.
* Removal of unmaintained extensions and MSVC support.
* Use dune instead of make and ocamlbuild.
* Many bug fixes.

Quickstart
----------

Install the latest release of `goblint-cil` with [opam][]:

    opam install goblint-cil

Read the excellent [CIL tutorial][tuto] by Zachary Anderson, much of which
still applies to `goblint-cil`. The repository referenced in that document has now moved [here][repo].

**ATTENTION:** Don't install the `cil` package. This is the unmaintained
original version of CIL.

[opam]: https://opam.ocaml.org/
[tuto]: https://web.eecs.umich.edu/~weimerw/2011-6610/reading/ciltut.pdf
[repo]: https://github.com/zanderso/cil-template

Installation from Source
------------------------

Prerequisites:
- opam
- GCC
- Perl

First create a local opam switch and install all dependencies:

    opam switch create .

Then, you can use [dune] to build `goblint-cil`. Run the following
commands to build and test `goblint-cil`:

    dune build
    dune runtest    # runs the regression test suite

To run a single test go to the build directory (e.g. `_build/default/test`) and run e.g.:

    dune exec -- make test/array1

You can also install `goblint-cil` into the opam switch:

    dune build @install
    dune install

[dune]: https://github.com/ocaml/dune

Usage
-----

You can use cilly (installed in the opam switch) as a drop-in
replacement for `gcc` to compile and link your programs.

You can also use `goblint-cil` as a library to write your own programs. For
instance in the OCaml toplevel using [Findlib][findlib]:

    $ ocaml
    OCaml version 4.14.0

    # #use "topfind";;
    [...]
    # #require "goblint-cil";;
    [...]
    # GoblintCil.cilVersion;;
    - : string = "2.0.1"

[findlib]: http://projects.camlcity.org/projects/findlib.html

License
-------
`goblint-cil` is licensed under the BSD license. See [LICENSE][license].

[license]: https://github.com/goblint/cil/blob/develop/LICENSE
