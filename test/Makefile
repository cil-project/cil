# -*- Mode: makefile -*-
# Makefile.in for running the test cases for the CIL compiler
# Use from the test directory inside the _build folder (e.g. cil/_build/default/test), by running e.g. `dune exec -- make test/array1`
# author: George Necula
# hacks here and there by Wes and Scott

# this Makefile makes use of several GNU Make extensions; see
#   http://www.gnu.org/manual/make/html_chapter/make_toc.html

export _GNUCC=1

TESTDIR := .

unexport CIL_FEATURES

defaulttarget:
	@echo "This Makefile is intended to be run with an explicit target."


# sm: find and remove all the intermediate files from translation
# sm: removed *box.c from those removed since $(TESTDIR)/PCC/src/pccbox.c should be kept
clean:
	-find . \( \
		-name '*~' -o \
		-name '*.cil.c' -o \
		-name '*.s' -o \
		-name '*.a' -o \
		-name '*.stackdump' -o \
		-name '*.exe' -o \
		\( -name '*.i' -a -not -name 'lineno.i' -a -not -name 'empty.i' \) -o \
		-name '*.cmi' -o \
		-name '*.cmo' -o \
		-name '*.err' -o \
		-name '*.out' -o \
		-name '*_ppp.c' -o \
		-name '*.origi' -o \
		-name '*.o' -o \
		-name '*.obj' -o \
		-name '*.cabs.c' -o \
	        -name "*-tmp.c" -o \
		-name '*_comb*.c' -o \
		-name 'libmerge.a*' -o \
		-name 'merge-ar' \
	\) -exec rm -f {} \;
	-find . -name '*.browser' -exec rm -rf {} \;

distclean: clean
	rm -f cil.log*

CILHOME := $(CURDIR)/..
CILLY := $(CILHOME)/bin/cilly

ifdef _MSVC
	include Makefile.msvc
else
ifdef _GNUCC
	include Makefile.gcc
endif
endif

CILLY += --mode=$(COMPILERNAME) --decil

export EXTRAARGS


# CILLY contains arguments that are passed to cil
# Pass such arguments in the command line as EXTRAARGS="..."
# NOTE: you should *never* set EXTRAARGS within this Makefile,
# because *any* such settings will be overridden if someone
# specified EXTRAARGS on the command line
CILLY+= --save-temps $(EXTRAARGS)

ifndef NOCHECK
  CILLY += --strictcheck
endif

ifdef OCAMLDEBUG
  CILLY+= --ocamldebug
endif

ifdef NOLINES
  CILLY+= --noPrintLn
endif
ifdef COMMLINES
  CILLY+= --commPrintLn
endif

ifdef USECFG
	CILLY+= --domakeCFG
endif
ifdef USECABS
  CILLY+= --usecabs
endif
ifdef VERBOSE
  CILLY+= --verbose --warnall
endif
ifdef KEEPMERGED
  CILLY+= --keepmerged
endif
ifdef MERGEONLY
  CILLY+= --keepmerged  --onlyMerge --mergeKeepAnnotations
endif
ifdef CABSONLY
  CILLY+= --cabsonly
endif
ifdef COMPUTEDGOTO
  CILLY+= --useComputedGoto
endif
ifdef LOCALINIT
  CILLY+= --noMakeStaticGlobal
endif
ifdef OPTIMIZE
	CILLY+= $(OPT_O2)
endif



# This is a way to enable the stats, allowing the command line to override it
# Do STATS= to disable the stats.
STATS := 1
ifdef STATS
  CILLY+= --stats
endif

# when SEPARATE is defined, merging is disabled
ifdef SEPARATE
  CILLY+= --nomerge
endif

# sm: this will make gcc warnings into errors; it's almost never
# what we want, but for a particular testcase (combine_copyptrs)
# I need it to show the difference between something which works
# and something which will cause inference problems later
ifdef WARNINGS_ARE_ERRORS
  CFLAGS += -Werror
endif

ifdef USE_LOGICAL_OPERATORS
  CILLY+= --useLogicalOperators
endif

# Enable profiling
ifdef PROFILE
  CILLY+= -pg
endif

ifdef SHELL_ESCAPE
	INCLUDE_DEF := -DSTDIO=<stdio.h>
	INCLUDE_DEF2 := -DTEST="testharness.h"
	CFLAGS += "$(INCLUDE_DEF)" '$(INCLUDE_DEF2)'
endif

# sm: use this instead of "sh ./testit" for those self-tests which can't
# be made to work on windows; it does nothing, and has no output
UNIXTESTIT := sh ./testit


# ----------- below here are rules for building benchmarks --------

CC_OPTIMVARIANT:= $(CC) $(DEF)_$(COMPILERNAME) \
                 $(DEF)CIL \
                 $(INC)$(CILHOME)/include \
                 $(OPT_O2)

# use this dependency for those targets that must be built with GCC
mustbegcc :
ifndef _GNUCC
	@echo This test case works only with _GNUCC=1; exit 3
endif

############ Small tests
SMALL1 := $(TESTDIR)/small1

test/% : $(SMALL1)/%.c
	cd $(SMALL1); $(CILLY) --nomerge --commPrintLn \
	       $(CONLY) -std=gnu90 $(CFLAGS) $(ASMONLY)$*.s $*.c
	echo SUCCESS

testobj/% : $(SMALL1)/%.c
	cd $(SMALL1); $(CILLY) --nomerge  --commPrintLn  \
	       $(CONLY) $(CFLAGS) $(OBJOUT)$*.o $*.c

testrun/% : $(SMALL1)/%.c
	cd $(SMALL1); $(CILLY) --nomerge  --commPrintLn  \
	       $(CFLAGS) -std=gnu90 $(EXEOUT)$*.exe $*.c
	cd $(SMALL1); ./$*.exe
	echo SUCCESS

testrunc99/% : $(SMALL1)/%.c
	cd $(SMALL1); $(CILLY) --nomerge  --commPrintLn  \
	       $(CFLAGS) -std=c99 $(EXEOUT)$*.exe $*.c -lm
	cd $(SMALL1); ./$*.exe
	echo SUCCESS

# TODO: how to make this just run cil, not gcc?
testc11/% : $(SMALL1)/%.c
	cd $(SMALL1); $(CILLY) --nomerge  --commPrintLn  \
	       $(CFLAGS) -std=c11 $(EXEOUT)$*.exe $*.c -lm
	echo SUCCESS

testrunc11/% : $(SMALL1)/%.c
	cd $(SMALL1); $(CILLY) --nomerge  --commPrintLn  \
	       $(CFLAGS) -std=c11 $(EXEOUT)$*.exe $*.c -lm
	cd $(SMALL1); ./$*.exe
	echo SUCCESS

testrungcc/% : $(SMALL1)/%.c mustbegcc
	cd $(SMALL1); $(CILLY) --nomerge  --commPrintLn  \
	       $(CFLAGS) $(EXEOUT)$*.exe $*.c
	cd $(SMALL1); ./$*.exe
	echo SUCCESS

#preprocessed files:
test_i/% : $(SMALL1)/%.i
	cd $(SMALL1); $(CILLY) --nomerge --commPrintLn \
	       $(CONLY) $(CFLAGS) $(ASMONLY)$*.s $*.i
	echo SUCCESS


# sm: some project members don't want the testing targets to depend
# on quickbuild (which rebuilds translator components whose dependencies
# have changed), and others do..
ifdef TARGETS_DEP_QUICKBUILD
  # with switch to test/Makefile, doesn't do what I want, so no-op
  #TARGET_DEP := quickbuild
  TARGET_DEP :=
endif

ifndef NOPRINTLN
 NOPRINTLN := --commPrintLn
endif

# sm: attempt at a single rule for my testing purposes
# gn: I added .exe so that I can delete the executables
scott/%: $(TESTDIR)/small2/%.c $(TARGET_DEP)
	rm -f $(TESTDIR)/small2/$*.exe
	cd $(TESTDIR)/small2; $(CC) $(CONLY) $(CFLAGS) $(WARNALL) $*.c
	cd $(TESTDIR)/small2; $(CILLY) --nomerge --save-temps=. \
                 $(CFLAGS) $(NOPRINTLN) \
                 $*.c \
                 $(EXEOUT)$*.exe
	bash -c "time $(TESTDIR)/small2/$*.exe"


# There are a few tests that CIL supports but recent versions of gcc don't.
# So don't try calling gcc on these.
scott-nogcc/%: $(TESTDIR)/small2/%.c $(TARGET_DEP)
	rm -f $(TESTDIR)/small2/$*.exe
	cd $(TESTDIR)/small2; $(CILLY) --nomerge --save-temps=. \
                 $(CFLAGS) $(NOPRINTLN) \
                 $*.c \
                 $(EXEOUT)$*.exe
	bash -c "time $(TESTDIR)/small2/$*.exe"

scott-nolink/%: $(TESTDIR)/small2/%.c $(TARGET_DEP)
	rm -f $(TESTDIR)/small2/$*.exe
	cd $(TESTDIR)/small2; $(CC) $(CONLY) $(WARNALL) $*.c
	cd $(TESTDIR)/small2; $(CILLY) --nomerge $(CONLY) --save-temps=. \
                 $(CFLAGS) $(WARNALL) $(NOPRINTLN) \
                 $*.c \
                 $(EXEOUT)$*.exe



OBJDIR_DUNE := $(CILHOME)/../install/default/lib/goblint-cil
OBJDIR_MAKE := $(CILHOME)/_build/src
ifdef NATIVECAML
 CMXA := cmxa
 CAMLC := ocamlopt
else
 CMXA := cma
 CAMLC := ocamlc
endif

testrun/% : $(SMALL1)/%.ml
	ocamlfind $(CAMLC) -I $(OBJDIR_DUNE) -I $(OBJDIR_MAKE) -package zarith unix.$(CMXA) str.$(CMXA) zarith.$(CMXA) \
                 goblintCil.$(CMXA) \
                 $(EXEOUT) $(basename $<).exe $<
	$(basename $<).exe
	echo SUCCESS


combine%: $(SMALL1)/combine%_1.c
	cd $(SMALL1); \
          $(CILLY) $(CFLAGS) -std=gnu90 -fcommon \
                    $(notdir $(wildcard $(SMALL1)/combine$*_[1-9].c)) \
	            $(EXEOUT)combine$*.exe
	cd $(SMALL1); ./combine$*.exe

combinegnuinline:
	cd $(SMALL1); \
          $(CILLY) --merge $(CFLAGS)  -std=gnu90 -fcommon \
                    combinegnuinline_1.c combinegnuinline_2.c \
	            $(EXEOUT)combinegnuinline.exe
	cd $(SMALL1); ./combinegnuinline.exe

arcombine: mustbegcc
	cd $(SMALL1); $(CILLY) -c array1.c array2.c
	cd $(SMALL1); $(CILHOME)/bin/cilly \
                           --mode=AR --merge --verbose crv array.a array1.o array2.o
	cd $(SMALL1); $(CILLY) -o matrix.exe array.a matrix.c
	cd $(SMALL1); ./matrix.exe


combinec99%: $(SMALL1)/combine-c99%_1.c
	cd $(SMALL1); \
          $(CILLY) $(CFLAGS) -O1 -std=c99 --verbose -fcommon --merge \
                    $(notdir $(wildcard $(SMALL1)/combine-c99$*_[1-9].c)) \
	            $(EXEOUT)combine-c99$*.exe
	cd $(SMALL1); ./combine-c99$*.exe

combinec99mergeinline%: $(SMALL1)/combine-c99-mergeinline%_1.c
	cd $(SMALL1); \
          $(CILLY) $(CFLAGS) -O1 -std=c99 --verbose -fcommon --merge --mergeinlines \
                    $(notdir $(wildcard $(SMALL1)/combine-c99-mergeinline$*_[1-9].c)) \
	            $(EXEOUT)combine-c99-mergeinline$*.exe
	cd $(SMALL1); ./combine-c99-mergeinline$*.exe

# ww: Scott's structs-edg-stl.c example
structs : mustbemanju
	cd /usr/src/big-examples/; $(CILLY) --nomerge \
	       $(CONLY) $(CFLAGS) structs-edg-stl.c
	echo SUCCESS



# sm: yet another failure-test target, this time utilizing a nomerge
# script capable of testing multiple failures per file
test-bad/%: $(TESTDIR)/small2/%.c $(TARGET_DEP)
	cd $(TESTDIR)/small2; \
	  CILHOME="$(CILHOME)" \
	  CILLY="$(CILLY) --nomerge --commPrintLn" \
	  CFLAGS="$(CFLAGS) $(WARNALL)" \
          TESTBADONCE="$(TESTBADONCE)" \
	  _MSVC="$(_MSVC)" \
	  bash ../test-bad $*.c

test-bad1/%: $(TESTDIR)/small1/%.c $(TARGET_DEP)
	cd $(TESTDIR)/small1; \
	  CILHOME="$(CILHOME)" \
	  CILLY="$(CILLY) --nomerge --commPrintLn" \
	  CFLAGS=" $(CFLAGS) $(WARNALL)" \
          TESTBADONCE="$(TESTBADONCE)" \
	  _MSVC="$(_MSVC)" \
	  bash ../test-bad $*.c


#CCURED_NO_SIGABRT prevents cygwin from doing a memory dump on every failure
# case.
runall/%: $(TESTDIR)/small2/%.c $(TARGET_DEP)
	cd $(TESTDIR)/small2; \
	CCURED_NO_SIGABRT=1 \
	COMMAND="$(CILLY) \
	   $(CFLAGS) $(WARNALL) __FILE__  $(EXEOUT)__BASENAME__.exe \
	   && ./__BASENAME__.exe" \
	COMMENT="//" \
	 perl ../runall.pl $*.c


runall_syntax/%: $(TESTDIR)/small2/%.c $(TARGET_DEP)
	cd $(TESTDIR)/small2; \
	CCURED_NO_SIGABRT=1 \
	COMMAND="$(CILLY) \
	   $(CFLAGS) $(WARNALL) __FILE__  $(EXEOUT)__BASENAME__.exe \
	   && ./__BASENAME__.exe && cat __BASENAME__.cured.c" \
	COMMENT="//" \
	 perl ../runall.pl $*.c

# sm: trivial test of combiner
MYSAFECC := $(CILLY)
comb: $(TESTDIR)/small2/comb1.c $(TESTDIR)/small2/comb2.c
	rm -f $(TESTDIR)/small2/comb.exe
	cd $(TESTDIR)/small2; \
	  $(MYSAFECC) -fcommon comb1.c $(CONLY) $(OBJOUT) comb1.o; \
	  $(MYSAFECC) -fcommon comb2.c $(CONLY) $(OBJOUT) comb2.o; \
	  $(MYSAFECC) -fcommon comb3.c $(CONLY) $(OBJOUT) comb3.o; \
	  $(MYSAFECC) -fcommon comb4.c $(CONLY) $(OBJOUT) comb4.o; \
          $(MYSAFECC) -fcommon comb1.o comb2.o comb3.o comb4.o $(EXEOUT)comb.exe
	$(TESTDIR)/small2/comb.exe

#call cilly on a .c file, a .i file, a .s file, and a .o file.
#Of course, only the first two are merged.
mixedcomb: $(TESTDIR)/small2/comb1.c $(TESTDIR)/small2/comb2.c
	rm -f $(TESTDIR)/small2/comb.exe
	cd $(TESTDIR)/small2; \
	  gcc -fcommon -E -o comb2.i comb2.c; \
	  gcc -fcommon -S -o comb3.s comb3.c; \
	  gcc -fcommon -c -o comb4.o comb4.c; \
	  $(MYSAFECC) -fcommon  comb1.c comb2.i comb3.s comb4.o $(EXEOUT)comb.exe
	$(TESTDIR)/small2/comb.exe

# sm: another merger test
mergestruct: $(TESTDIR)/small2/mergestruct1.c $(TESTDIR)/small2/mergestruct2.c
	cd $(TESTDIR)/small2; \
	  $(CILLY) -fcommon mergestruct1.c mergestruct2.c -o mergestruct.exe
	$(TESTDIR)/small2/mergestruct.exe

# sc: this tests for a merger bug in global variables initializations
mergeinit: $(TESTDIR)/small2/mergeinit1.h $(TESTDIR)/small2/mergeinit1.c \
           $(TESTDIR)/small2/mergeinit2.h $(TESTDIR)/small2/mergeinit2_1_reftable.c $(TESTDIR)/small2/mergeinit2_2_definition.c \
           $(TESTDIR)/small2/mergeinit3.h $(TESTDIR)/small2/mergeinit3.c \
           $(TESTDIR)/small2/mergeinit4.c
	cd $(TESTDIR)/small2; \
	  $(CILLY) --merge --strictcheck --keepunused mergeinit1.c mergeinit2_1_reftable.c mergeinit2_2_definition.c mergeinit3.c mergeinit4.c

# sm: yet another merger test (I know there's a target somewhere)
mergeinline: $(TESTDIR)/small2/mergeinline1.c $(TESTDIR)/small2/mergeinline2.c
	cd $(TESTDIR)/small2; \
	  $(CILLY) mergeinline1.c mergeinline2.c -o mergeinline.exe
	$(TESTDIR)/small2/mergeinline.exe

# sm: test of combiner's ability to report inconsistencies
baddef: $(TESTDIR)/small2/baddef1.c $(TESTDIR)/small2/baddef2.c
	cd $(TESTDIR)/small2; $(CC) baddef1.c baddef2.c -o baddef.exe \
           && ./baddef.exe
	rm -f $(TESTDIR)/small2/baddef.exe
	cd $(TESTDIR)/small2; \
	  $(MYSAFECC)  baddef1.c $(CONLY) $(OBJOUT) baddef1.o; \
	  $(MYSAFECC)  baddef2.c $(CONLY) $(OBJOUT) baddef2.o; \
          $(MYSAFECC)  baddef1.o baddef2.o $(EXEOUT)baddef.exe
	$(TESTDIR)/small2/baddef.exe




### Generic test
testfile/% :
	$(CILLY) /TC $*

testdir/% :
	make -C CC="ccured" $*


merge-ar:
	cd small2; $(CILHOME)/bin/cilly --merge -c merge-ar.c merge-twice-1.c
	cd small2; $(CILHOME)/bin/cilly --merge --mode=AR cr libmerge.a merge-ar.o merge-twice-1.o
	cd small2; $(CILHOME)/bin/cilly --merge libmerge.a -o merge-ar
