# configure.in for CIL           -*- sh -*-
# Process this file with autoconf to produce a configure script.

# Autoconf runs this through the M4 macroprocessor first; lines
# starting with "dnl" are comments to M4.  The result is a bash
# script; any text which isn't an M4/autoconf directive gets
# copied verbatim to that script.

# also, in general, watch out: the M4 quoting charactes are
# the square brackets: [ and ].  if you want to pass brackets
# to something, you can quote the brackets with more brackets.
# I don't know how to pass a single (unbalanced) bracket ..

# sm: changed this file to use '#' for comments, since that's
# just as good (since this becomes an 'sh' script)




# -------------- usual initial stuff -------------
# this simply names a file somewhere in the source tree to verify
# we're in the right directory
AC_INIT
AC_CONFIG_SRCDIR([src/cil.mli])
AC_CONFIG_HEADERS(config.h)
AC_CONFIG_FILES([stamp-h], [echo timestamp > stamp-h])

# sm: require a late-enough autoconf; this is the version number
# that's on manju, so I assume it's ok
AC_PREREQ([2.69])

#
# Assign here the CIL version numbers
CIL_VERSION_MAJOR=1
CIL_VERSION_MINOR=7
CIL_VERSION_REV=0
CIL_VERSION=$CIL_VERSION_MAJOR.$CIL_VERSION_MINOR.$CIL_VERSION_REV

# make sure I haven't forgotten to run autoconf
if test configure -ot configure.in; then
  AC_MSG_ERROR(configure is older than configure.in; you forgot to run autoconf)
fi

# check for C compiler; this typically finds gcc; it sets the
# variable CC to whatever it finds, which then gets substituted
# for @CC@ in output files; you have to do this even if you don't
# care about @CC@, because system feature tests later on in
# the ./configure script will expect $CC to be set right
AC_PROG_CC

AC_PROG_INSTALL

# find system type (using this macro means we must include
# the files install-sh, config.sub, and config.guess (all from
# the autoconf distribution) in our source tree!)
AC_CANONICAL_TARGET

# Check if executables should end with .exe (windows)
#AC_EXEEXT


# ---------------- generic functions -----------------
# debugging diagnostic; set to 'echo' to debug or 'true' for production
# (technically you're not supposed to use shell functions in
# configure scripts, because some-obscure-sh somewhere doesn't
# support them.. but they're too convenient to not use)
diagnostic() {
  #echo "$@"
  true "$@"
}

# determine if a binary is in the path
binaryExists() {
  # on cygwin, 'which' always returns success, so use 'type' instead
  if type "$1" >/dev/null 2>&1; then
    return 0
  else
    return 1
  fi
}


# -------------- portable configuration ----------------

DEFAULT_COMPILER=_GNUCC
DEFAULT_CIL_MODE=GNUCC

# is the microsoft compiler available?
# hmm.. I think we should check the version or something, because
# sometimes people have Common Lisp's interpreter called 'cl' ..
AC_MSG_CHECKING(for msvc cl.exe (optional))
# See if CC points to the MS compiler
if "$CC" 2>&1 | grep "Microsoft" >/dev/null; then 
  AC_MSG_RESULT([found, set as default])
  HAS_MSVC=yes
  DEFAULT_COMPILER=_MSVC
  DEFAULT_CIL_MODE=MSVC
  CFLAGS="-WX"
else
  if cl 2>&1 | grep "Microsoft" >/dev/null ;then
     AC_MSG_RESULT(found)
     HAS_MSVC=yes
  else
     AC_MSG_RESULT(not found)
     HAS_MSVC=no
  fi
fi

# Testing OCaml availability
AC_PROG_OCAML
if test "$OCAMLC" = "no"; then
  AC_MSG_ERROR([You must install the OCaml compiler])
fi

if test "$OCAMLBUILD" = "no"; then
  AC_MSG_ERROR([You must install ocamlbuild])
fi

AC_PROG_OCAMLLEX
if test "$OCAMLLEX" = "no"; then
  AC_MSG_ERROR([You must install ocamllex])
fi

AC_PROG_OCAMLYACC
if test "$OCAMLYACC" = "no"; then
  AC_MSG_ERROR([You must install ocamlyacc])
fi

AC_PROG_FINDLIB
if test "$OCAMLFIND" = "no"; then
  AC_MSG_ERROR([You must install OCaml findlib (the ocamlfind command)])
fi

#
# ------------------- Perl ----------------
#
AC_MSG_CHECKING([for Perl])
  if ! binaryExists perl; then
    AC_MSG_ERROR([
      perl not found.
      You need perl version 5.6.1 or later for CIL.
      You can get perl at http://www.cpan.org/src/index.html .
    ])
  fi

  # sm: oh how nice it would be to just say "use English;
  # print($PERL_VERSION)", but that appears broken on 5.6.1.. so I'm
  # trying to say "caret right-bracket", but then that would run afoul
  # of autoconf's quoting characters, so I use the "quadrigraph" @:>@
  # to stand for right-bracket.  what a mess.
  perlver=`perl -e 'print($@:>@);'`
  if perl -e "exit( $perlver >= 5.006001 );"; then
    AC_MSG_ERROR([
      Found perl version $perlver, but at least 5.6.1 is required.
      You can get a newer perl at http://www.cpan.org/src/index.html .
    ])
  fi

  perlport=`perl -e "print $^O;"`
  case "$perlport" in
    cygwin)
        ;;
    msys)
        ;;
    MSWin32) # ActivePerl
        ;;
    linux)
        ;;
    freebsd)
        ;;
    openbsd)
        ;;
    darwin) # Mac OS X
        ;;
    solaris)
	;;
    *)
      AC_MSG_ERROR([
        Unsupported Perl port $perlport -- sorry.
        cygwin, MSWin32 (ActivePerl), linux, freebsd, openbsd, darwin,
        and solaris are the supported ports.
      ])
  esac
AC_MSG_RESULT([found version $perlver, port $perlport])

        # The cygwin port has some bugs in the File::Spec module
if test "$perlport" = "cygwin" ;then
  AC_MSG_CHECKING([for known cygwin Perl bug in File::Spec])
  perlfixres=[`perl -e '
      use File::Spec;
      if(File::Spec->file_name_is_absolute("C:/test")) {
        print "no bug found"; exit 0;
      } else {
        print "bug";
        foreach $d (@INC) {
           if(-f "$d/File/Spec/Unix.pm") {
              open(IN, "<$d/File/Spec/Unix.pm");
              open(OUT, ">$d/File/Spec/Unix.pm.fixed") 
                       || die "Cannot open $d/File/Spec/Unix.pm.fixed";
              while(<IN>) {
                if($_ =~ m|sub file_name_is_absolute|) {
                   print OUT $_;
                   print OUT scalar(<IN>);
                   print OUT <<EOF;
   if(\\$^O eq \"cygwin\") { 
       return scalar(\\$file =~ m{^(\\[a-z\\]:)?\\[\\\\\\\\/\\]}is);
};
EOF
                   next;
                }
                print OUT $_;
              }
              close(OUT);
              close(IN);
              system("mv -f $d/File/Spec/Unix.pm.fixed $d/File/Spec/Unix.pm");
           }
        }
      }
  '`]
  # See if it was indeed fixed
  if test "$perlfixres" = "bug" ;then
     perlfixres=`perl -e '
      use File::Spec;
      if(File::Spec->file_name_is_absolute("C:/test")) {
        print "bug fixed"; exit 0;
      } else {
        print "cannot fix bug"; exit 1;
      }'`
  fi
  if test "x$perlfixres" = "x" ;then
      AC_MSG_ERROR([
        Cannot run perl
      ])
  elif test "$perlfixres" = "cannot fix bug" ;then
      AC_MSG_ERROR([
        Found a bug but cannot fix it.
      ])
  else
     AC_MSG_RESULT([$perlfixres])
 fi
fi

# additional tools we might check for:
#   - gnu make

#
# -------------------- GCC --------------
#

AC_MSG_CHECKING([for gcc version])
AC_CHECK_TYPE(__builtin_va_list,
              HAVE_BUILTIN_VA_LIST=true,
              HAVE_BUILTIN_VA_LIST=false)
AC_MSG_CHECKING([if __thread is a keyword])
AC_COMPILE_IFELSE([AC_LANG_SOURCE([int main(int __thread) { return 0; }])],
                  THREAD_IS_KEYWORD=false,
                  THREAD_IS_KEYWORD=true)
AC_MSG_RESULT($THREAD_IS_KEYWORD)

# Does gcc add underscores to identifiers to make assembly labels?
# (I think MSVC always does)
AC_MSG_CHECKING([if gcc adds underscores to assembly labels.])
AC_LINK_IFELSE([AC_LANG_SOURCE([int main() { __asm__("jmp _main"); }])],
                  UNDERSCORE_NAME=true,
                  UNDERSCORE_NAME=false)
AC_MSG_RESULT($UNDERSCORE_NAME)


# ----------- some stuff 'autoscan' put here --------------
# (autoscan is part of the autoconf distribution)

# checks for header files
AC_HEADER_STDC
AC_CHECK_HEADERS(stdlib.h strings.h sys/time.h unistd.h wchar.h stdbool.h)

# checks for typedefs, structures, and compiler characteristics
AC_C_CONST
AC_C_INLINE
AC_HEADER_TIME

# checks for library functions; more autoscan stuff
AC_FUNC_MEMCMP
AC_CHECK_FUNCS(mkdir select socket __sysv_signal)

# Find out the true definitions of some integer types
CIL_CHECK_INTEGER_TYPE(size_t, TYPE_SIZE_T)
CIL_CHECK_INTEGER_TYPE(wchar_t, TYPE_WCHAR_T)


# ----------- platform-specific code -------------
# $target is typically processor-vendor-os
case "$target" in

  # cygwin
  *86*cygwin*)
    AC_MSG_RESULT(configuring for Cygwin on win32/x86)

    CC=`which $CC`
    CC=`cygpath -wa "$CC"  | sed -e "s/\\\\\/\\//g"`
    ;;

  # MinGW
  *86*mingw32*)
    AC_MSG_RESULT(configuring for MinGW on win32/x86)

    CC=`which $CC`
    ;;

  *mipsel*linux*)
    AC_MSG_RESULT(configuring for MIPSel/Linux)

    if test $cross_compiling = "yes"; then
      AC_PATH_PROG([EMUL], [qemu-mipsel])
    fi
    ;;

  *mips*linux*)
    AC_MSG_RESULT(configuring for MIPS/Linux)

    if test $cross_compiling = "yes"; then
      AC_PATH_PROG([EMUL], [qemu-mips])
    fi
    ;;

  *)
    ;;
esac

#
# CIL features
#
#
   
# Set the defaults


# Give a space-separated list of features with the defaults
features="blockinggraph=no zrapp=no llvm=no"

AC_ARG_WITH(blockinggraph,
    AS_HELP_STRING([--with-blockinggraph],[enable the blocking graph feature]))
AC_ARG_WITH(zrapp,
    AS_HELP_STRING([--with-zrapp],[enable the zrapp pretty-printer]))

AC_ARG_WITH(llvm,
    AS_HELP_STRING([--with-llvm],[enable the LLVM code generator]))

# Now add any features specified in the command-line

features="$features $EXTRAFEATURES"

rm -f Makefile.features
echo "# -*- Mode: makefile -*-">Makefile.features
echo "# This file was generated automatically by ./configure.in">>Makefile.features
echo "# We will not need this once configure supports multiline variables">>Makefile.features

for f_val in $features
do
   # If there is no =, then we default to yes
   if ! (echo $f_val | grep "=" >/dev/null) ;then f_val="$f_val=yes"; fi
   # echo "Testing feature $f_val"
   f=`echo $f_val | sed -e s%=.*$%%`
   AC_MSG_CHECKING(whether to use CIL feature $f)
   # default value from "features"
   defval=`echo $f_val | sed -e s%^.*=%%`
   # current value
   getcurval="echo \${with_$f:=$defval}"
   curval=`eval $getcurval`
   AC_MSG_RESULT($curval)
   if test $curval = yes ;then
      CIL_FEATURES="$CIL_FEATURES $f"
      f_up=`echo $f | tr a-z A-Z`
      echo "export USE_$f_up=yes">>Makefile.features
   fi
done
chmod -w Makefile.features


# ----------------- finish up -------------------
# names of the variables that get substituted in files; for example,
# write @CIL_VERSION@ somewhere in a written file to get it substituted
AC_SUBST(EMUL)
AC_SUBST(HAS_MSVC)
AC_SUBST(DEFAULT_COMPILER)
AC_SUBST(DEFAULT_CIL_MODE)
AC_SUBST(CIL_VERSION_MAJOR)
AC_SUBST(CIL_VERSION_MINOR)
AC_SUBST(CIL_VERSION_REV)
AC_SUBST(CIL_VERSION)
AC_SUBST(HAVE_BUILTIN_VA_LIST)
AC_SUBST(THREAD_IS_KEYWORD)
AC_SUBST(UNDERSCORE_NAME)
AC_SUBST(EXTRAFEATURES)
AC_SUBST(EXTRASRCDIRS)

# finish the configure script and generate various files; ./configure
# will apply variable substitutions to <filename>.in to generate <filename>;

CIL_CONFIG_FILES(Makefile)
CIL_CONFIG_FILES(META)
CIL_CONFIG_FILES(config.mk)
CIL_CONFIG_FILES(test/Makefile)
CIL_CONFIG_FILES(lib/App/Cilly/CilConfig.pm)
CIL_CONFIG_FILES(doc/index.html)
CIL_CONFIG_FILES(doc/header.html)
CIL_CONFIG_FILES(src/machdep-ml.c)

AC_OUTPUT

# show the user what the variables have been set to
cat <<EOF

CIL configuration:
  (optional) cl.exe found:    HAS_MSVC           $HAS_MSVC
  gcc to use                  CC                 $CC
  default compiler            DEFAULT_COMPILER   $DEFAULT_COMPILER
  CIL version                 CIL_VERSION        $CIL_VERSION
  CIL features                CIL_FEATURES       $CIL_FEATURES
  Extra source directories    EXTRASRCDIRS       $EXTRASRCDIRS
EOF
