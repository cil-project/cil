C Intermediate Language (CIL)
============================

CIL is a front-end for the C programming language that facilitates
program analysis and transformation. CIL will parse and typecheck a
program, and compile it into a simplified subset of C.

`goblint-cil` is a fork of CIL that supports C99 as well as most of the
extensions of the GNU C. It makes many changes to the original CIL in an effort
to modernize it and keep up with the latest versions of the C language. Here is
an incomplete list of some of the ways `goblint-cil` improves upon CIL:
- Proper support for C99, ([#9][i9]) and VLAs in particular ([#5][i5], [#7][pr7])
- It uses [Zarith][zarith] instead of the deprecated [Num][num]
- Support for more recent OCaml versions (≥ 4.06)
- Large integer constants that do not fit in an OCaml `int` are represented as a
  `string` instead of getting truncated
- Syntactic search extension ([#21][pr21])
- Some warnings were made optional
- Unmaintained extensions ([#30][pr30]) were removed
- Many bug fixes

[zarith]: https://github.com/ocaml/Zarith
[num]: https://github.com/ocaml/num
[i5]: https://github.com/goblint/cil/issues/5
[pr7]: https://github.com/goblint/cil/pull/7
[i9]: https://github.com/goblint/cil/issues/9
[pr21]: https://github.com/goblint/cil/pull/21
[pr30]: https://github.com/goblint/cil/pull/30

Quickstart
----------

Install the latest release of `goblint-cil` with [opam][]:

    opam install goblint-cil

Read the excellent [CIL tutorial][tuto] by Zachary Anderson, much of which
still applies to `goblint-cil`.

**ATTENTION:** Don't install the `cil` package. This is the unmaintained
original version of CIL.

[tuto]: https://web.eecs.umich.edu/~weimerw/2011-6610/reading/ciltut.pdf

Installation from Source
------------------------

Prerequisites:
- opam
- Some C compiler (preferably `gcc`)
- Perl

First create a local opam switch and install all dependencies:

    opam switch create .

Then, run the following commands to build and install `goblint-cil`:

    ./configure
    make
    make test       # runs the regression test suite, optional
    make install    # as root or using sudo

If you want to install to some other directory, you can tweak the prefix
during the configure step.  For instance, to install in your local [opam][]
directory:

    ./configure --prefix=`opam config var prefix`

[opam]: https://opam.ocaml.org/

Build with Dune
---------------
Alternatively, you can use [dune] to build `goblint-cil`. Run the following
commands to build and test `goblint-cil`:

    dune build
    dune runtest    # runs the regression test suite

[dune]: https://github.com/ocaml/dune

Usage
-----

You can use cilly (installed in `/usr/local/bin` by default) as a drop-in
replacement for `gcc` to compile and link your programs.

You can also use `goblint-cil` as a library to write your own programs. For
instance in the OCaml toplevel using [Findlib][findlib]:

    $ ocaml
            Objective Caml version 4.00.1

    # #use "topfind";;
    [...]
    # #require "cil";;
    [...]
    # Cil.cilVersion;;
    - : string = "1.8.1"

[findlib]: http://projects.camlcity.org/projects/findlib.html

TODO
----

- C11 support ([#13][i13])

[i13]: https://github.com/goblint/cil/issues/13

License
-------
`goblint-cil` is licensed under the BSD license. See [LICENSE][license].

[license]: https://github.com/goblint/cil/blob/develop/LICENSE
