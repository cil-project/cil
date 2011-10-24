C Intermediate Language
=======================

Installation
-----------

1. If you use Windows, you must first install cygwin. 

2. You must install OCaml version 3.10 or higher (see instructions at
    http://caml.inria.fr/ocaml). The recommended build process is using 
    the cygwin version of ocaml. 

    You can also build with Microsoft Visual Studio, but you must still have
    cygwin during the build process. See msvcbuild.cmd. 

3. Download and unpack the distribution. 

4. Run ./configure (from within bash if on Windows)

5. Run make

6. Run make check

More documentation
------------------

Build the doc:

    make doc

You can also [browse it online](http://kerneis.github.com/cil/doc).
