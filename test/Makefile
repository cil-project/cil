# Makefile.in for running the test cases for the CCured compiler
# Use from the test directory !!!
# author: George Necula
# hacks here and there by Wes and Scott

# this Makefile makes use of several GNU Make extensions; see
#   http://www.gnu.org/manual/make/html_chapter/make_toc.html

# pull in definitions from ./configure
export ARCHOS := @ARCHOS@
export CCUREDHOME := @CCUREDHOME@
export HAS_MSVC := @HAS_MSVC@

TESTDIR := .

ifneq ($(ARCHOS), x86_WIN32)
ifneq ($(ARCHOS), x86_LINUX) 
   $(error You must set the ARCHOS variable to either x86_WIN32 or x86_LINUX)
endif
endif

defaulttarget:
	@echo "This Makefile is intended to be run with an explicit target."

# watch for out-of-date Makefile
Makefile: Makefile.in
	cd ..; ./config.status

# sm: find and remove all the intermediate files from translation
# sm: removed *box.c from those removed since $(TESTDIR)/PCC/src/pccbox.c should be kept
clean:
	-find . \( \
		-name '*cil.c' -o \
		-name '*cured.c' -o \
                -name '*cured.*optim.c' -o \
		-name '*.exe' -o \
		-name '*.i' -o \
		-name '*_ppp.c' -o \
		-name '*.origi' -o \
		-name '*.o' -o \
		-name '*.obj' -o \
		-name '*cabs.c' -o \
		-name '*infer.c' -o \
		-name '*_all*.c' -o \
		-name '*_comb*.c' \
	\) -exec rm {} \;


# sm: infer CCUREDHOME when not set, to ease having multiple trees
ifndef CCUREDHOME
  ifeq ($(ARCHOS), x86_WIN32)
    $(error You have not defined the CCUREDHOME variable)
  else
    export CCUREDHOME := $(shell pwd)/..
  endif
endif

CCURED := $(CCUREDHOME)/bin/ccured
PATCHER := $(CCUREDHOME)/bin/patcher

# Now do the user-specific customization
# It is Ok if this file does not exist
-include $(CCUREDHOME)/.ccuredrc

# as a convenience, let RELEASE=1 on the command line imply
# all options designed to make things fast (at least when not in
# the middle of development)
ifdef RELEASE
  # use native code tools
  export NATIVECAML := 1
  # enable the optimizer inside our translator
  export OPTIM := 1
  # use runtime library with minimal debugging checks
  export RELEASELIB := 1
  # turn on gcc's optimizer for the resulting executables
  export CCURED += -O2
endif

# By default we are on Linux
ifndef ARCHOS
  ARCHOS := x86_WIN32
endif

# By default use the old patcher
ifndef NEWPATCH
  OLDPATCH := 1
endif

# Set the compiler, as in ../Makefile.in
ifndef _MSVC
  ifndef _GNUCC 
     @DEFAULT_COMPILER@=1
  endif
endif

# Now include the compiler specific stuff
ifdef _MSVC
  include ../Makefile.msvc
else
  ifdef _GNUCC
    include ../Makefile.gcc
  endif
endif


CCURED  += --mode=$(COMPILERNAME)
PATCHER += --mode=$(COMPILERNAME)

export EXTRAARGS
export INFERBOX

ifndef INFERBOX
  INFERBOX := none
endif

STANDARDPATCH := --includedir=$(CCUREDHOME)/include

# CCURED contains arguments that are passed to ccured
# Pass such arguments in the command line as EXTRAARGS="..."
# NOTE: you should *never* set EXTRAARGS within this Makefile,
# because *any* such settings will be overridden if someone
# specified EXTRAARGS on the command line
CCURED+= $(EXTRAARGS)

ifdef NEWOPTIM
  CCURED += --optimize --newoptim
endif
ifneq ($(INFERBOX),none)
  MANUALBOX := 1
  CCURED+= --curetype=$(INFERBOX) $(DEF)INFERBOX
  CCURED+= --emitinfer 
else
  CCURED+= --curetype=none
  ifndef MANUALBOX
    CCURED+= --boxdefaultwild
  endif
endif

ifdef MANUALBOX
  CCURED+= $(DEF)MANUALBOX
endif


ifdef NOGC
  CCURED+= --nogc
endif

ifeq ($(TABLE), A)
  CCURED+= --tableAll
endif
ifeq ($(TABLE), I)
  CCURED+= --tableInterface
endif
ifdef NOLINES
  CCURED+= --noPrintLn
endif
ifdef COMMLINES
  CCURED+= --commPrintLn
endif

ifdef USECABS
  CCURED+= --usecabs
endif
ifdef USECIL
  CCURED+= --usecil
endif	
ifdef NO_TAGS
  CCURED+= $(DEF)NO_TAGS
endif
ifdef CHECK
  CCURED += --check
endif
ifndef NATIVECAML
  CCURED+= --bytecode
endif
ifdef ANNOT
  CCURED+= --annotate
endif
ifdef OPTIM
  CCURED+= --optimize
endif
ifdef ANN
  CCURED+= --annotate
endif
ifdef RELEASELIB
  CCURED+= --releaselib -DRELEASELIB
endif
ifdef VERBOSE
  CCURED+= --verbose
endif
ifdef KEEPMERGED
  CCURED+= --keepmerged
endif
ifdef PRINTSTAGES
  CCURED+= --stages
endif
ifdef FP_FAIL_IS_VERBOSE
  # note that we also add this to CCUREDLIBARG in Makefile.ccured; this
  # affects the instrumented code *and* the support library
  CCURED+= $(DEF)FP_FAIL_IS_VERBOSE
endif

# sm: pass tracing directives on 'make' command line like TRACE=usedVars
# I don't quote $(TRACE) because it actually appears inside another quoted
# string already when passed as CC="perl ...."
ifdef TRACE
  CCURED+= --tr=$(TRACE)
endif

# This is a way to enable the stats, allowing the command line to override it
# Do STATS= to disable the stats.
STATS := 1
ifdef STATS
  CCURED+= --stats
endif

# enable logging of all fn calls in the application
# (see LOGSTYLE, below)
ifdef LOGCALLS
  CCURED+= --logcalls
endif

# sm: specify style of function logging, as integer sum of:
#   linux        1       use printk instead of printf
#   allInsts     2       log every *instruction* (very verbose!)
#   printPtrs    4       print raw pointer values
#   printStrings 8       try to print char* as string
#   noCFuncs     16      omit printing calls to functions whose name 
#                        starts with "C" (only relevant when allInsts)
# this does not imply LOGCALLS, so one can set a preferred style
# in (e.g.) .ccuredrc without always enabling logging
ifdef LOGSTYLE
  CCURED+= --logstyle=$(LOGSTYLE)
endif

# when this is turned on, it should disable any source changes we've
# made that are purely in the interest of performance
ifdef NO_PERF_CHANGES
  CCURED+= $(DEF)NO_PERF_CHANGES
endif

# enable the new tree-based patcher
ifdef NEWPATCH
  # at the moment, the new patcher is kinda shoehorned into this Makefile..
  # perhaps ccured is the right place to deal with telling ccured.exe
  # about this file

  # hack: possible append 'd' to the name, so we get different versions
  # for with and without CCURED; otherwise it would appear to be
  # up-to-date but for the wrong way
  ifdef PATCHDEFS
    PATCHFILE2=$(CCUREDHOME)/lib/$(PATCHFILE)2.id
  else
    PATCHFILE2=$(CCUREDHOME)/lib/$(PATCHFILE)2.i
  endif

  # tell ccured.byte.exe (via ccured) where to find the patch file
  CCURED+= --patchFile=$(PATCHFILE2)

  # and turn off the other patcher
  PATCHECHO := true
endif

# sm: use this instead of "sh ./testit" for those self-tests which can't
# be made to work on windows; it does nothing, and has no output
UNIXTESTIT := sh ./testit


# ----------- below here are rules for building benchmarks --------

OPTIMVARIANT:= $(CC) $(DEF)_$(COMPILERNAME) \
                 $(DEF)CIL \
                 $(DEF)CCURED \
                 $(INC)$(CCUREDHOME)/lib \
                 $(OPT_O2)

# use this dependency for those targets that must be built with GCC
mustbegcc :
ifndef _GNUCC
	@echo This test case works only with _GNUCC=1; exit 3
endif

mustbelinux:
ifneq ($(ARCHOS), x86_LINUX)
	@echo This test case works only on Linux; exit 3
endif

# ITERATIONS is the number of iterations
ifndef ITERATIONS
ITERATIONS := 1
endif

ifeq '$(ITERATIONS)' '1'
  ITERATION_ELEMS := 1
else
  ifeq '$(ITERATIONS)' '3'
    ITERATION_ELEMS := 1 2 3
  else
    ifeq '$(ITERATIONS)' '5'
      ITERATION_ELEMS := 1 2 3 4 5
    else
      ifeq '$(ITERATIONS)' '7'
	ITERATION_ELEMS := 1 2 3 4 5 6 7
      else
	error ITERATIONS value is not legal
      endif
    endif
  endif
endif


####### Test with PCC sources
PCCDIR := $(CCUREDHOME)/test/PCC
PCCTEST := PCCout
# sm: didn't update following use of RELEASE b/c I don't understand it
ifdef RELEASE
  PCCTYPE := RELEASE
  SPJARG :=
else
  PCCTYPE := _DEBUG
  SPJARG := --gory --save-temps=pccout
endif
ifdef _GNUCC
  PCCCOMP := _GNUCC
else
  PCCCOMP := _MSVC
endif

testpcc/% : $(PCCDIR)/src/%.c 
	cd $(CCUREDHOME)/test/PCCout; $(CCURED) --keep=. $(DEF)$(ARCHOS) \
                  $(DEF)$(PCCTYPE) $(CONLY) \
                  $(PCCDIR)/src/$*.c \
                  $(OBJOUT)$(notdir $*).o



ifdef _MSVC
  MSLINK := --mode=mscl
endif
PCCSAFECC=$(CCURED) $(DEF)CCURED \
                    $(STANDARDPATCH)  \
                    --keep=$(CCUREDHOME)/test/PCCout \
                    --leavealone=pccbox --leavealone=alloc
pcc : 
#	-rm $(PCCDIR)/$(ARCHOS)$(PCCCOMP)/$(PCCTYPE)/*.o
	-rm $(PCCDIR)/$(ARCHOS)$(PCCCOMP)/$(PCCTYPE)/*.exe
	-rm $(PCCDIR)/bin/*.exe
	make -C $(PCCDIR) \
             CC="$(PCCSAFECC) $(CONLY)" \
             LD="$(CCURED) $(MSLINK) --keep=$(CCUREDHOME)/test/PCCout" \
             USE_JAVA=1 USE_JUMPTABLE=1 TYPE=$(PCCTYPE) \
             COMPILER=$(PCCCOMP) \
	     clean  

pcc-noclean : 
#	-rm $(PCCDIR)/$(ARCHOS)$(PCCCOMP)/$(PCCTYPE)/*.o
	-rm $(PCCDIR)/$(ARCHOS)$(PCCCOMP)/$(PCCTYPE)/*.exe
	-rm $(PCCDIR)/bin/*.exe
	make -C $(PCCDIR) \
             CC="$(PCCSAFECC) $(CONLY)" \
             LD="$(CCURED) $(MSLINK)  --keep=$(CCUREDHOME)/test/PCCout" \
             USE_JAVA=1 USE_JUMPTABLE=1 TYPE=$(PCCTYPE) \
             COMPILER=$(PCCCOMP) \

pcc-combined: 
	cd $(PCCDIR)/bin; \
          $(CCURED) engine.$(ARCHOS)$(PCCCOMP).$(PCCTYPE).exe_all.c \
              $(EXEOUT)engine.$(ARCHOS)$(PCCCOMP).$(PCCTYPE).exe


pccclean :
	make -C $(PCCDIR) clean


SPJDIR := C:/Necula/Source/Touchstone/test
SPJARG +=  -WV,"-H,4000000,-noindent" -WC,"-H,4000000,-noindent"
# sm: didn't update following use of RELEASE b/c I don't understand it
ifndef RELEASE
  SPJARG += --pccdebug
endif
ifdef SPJTIME
  SPJARG += -WC,"-T,1000" 
endif

runspj.fact :
ifdef _GNUCC
	rm $(PCCDIR)/bin/*_MSVC*
endif
	cd $(SPJDIR); spj Arith/Fact.java --gory $(SPJARG) --pcchome=$(PCCDIR)

runspj.linpack :
ifdef _GNUCC
	rm $(PCCDIR)/bin/*_MSVC*
endif
	cd $(SPJDIR); spj linpack/Linpack.java --gory  \
                      $(SPJARG) --pcchome=$(PCCDIR)

runspj.quicksort :
ifdef _GNUCC
	rm $(PCCDIR)/bin/*_MSVC*
endif
	cd $(SPJDIR); spj arrays/QuickSort.java --gory \
                      $(SPJARG) --pcchome=$(PCCDIR)

runspj.simplex :
ifdef _GNUCC
	rm $(PCCDIR)/bin/*_MSVC*
endif
	cd $(SPJDIR); spj simplex/Simplex.java --gory  \
                      $(SPJARG) --pcchome=$(PCCDIR)

runspj.getopt :
ifdef _GNUCC
	rm $(PCCDIR)/bin/*_MSVC*
endif
	cd $(SPJDIR); spj gnu/getopt --gory  \
                      $(SPJARG) --pcchome=$(PCCDIR)

runspj.antlr :
ifdef _GNUCC
	rm $(PCCDIR)/bin/*_MSVC*
endif
	cd $(SPJDIR); spj antlr --gory  "-WV,-H,10000000" "-WC,-H,10000000" \
                      $(SPJARG) --pcchome=$(PCCDIR)

############ Small tests
SMALL1 := $(TESTDIR)/small1
test/% : $(SMALL1)/%.c 
	cd $(SMALL1); $(CCURED) --separate \
               $(STANDARDPATCH) \
	       $(CONLY) $(CFLAGS) $(ASMONLY)$*.s $*.c 
	echo SUCCESS

testnopatch/% : $(SMALL1)/%.c 
	cd $(SMALL1); $(CCURED)   \
               $(CONLY) $(CFLAGS) $(ASMONLY)$*.s $*.c 

testexe/% : $(SMALL1)/%.c  
	cd $(SMALL1); $(CCURED)   \
               $(STANDARDPATCH) \
	       $(CFLAGS) $(EXEOUT)$*.exe $*.c 


testrun/% : $(SMALL1)/%.c  
	cd $(SMALL1); $(CCURED)   \
               $(STANDARDPATCH) \
	       $(CFLAGS) $(EXEOUT)$*.exe $*.c
	cd $(SMALL1); ./$*.exe
	echo SUCCESS



testmodel/%: $(SMALL1)/%.c $(SMALL1)/modelextern.c 
	cd $(SMALL1); $(CCURED) \
                         $(STANDARDPATCH) \
                         --leavealone=modelextern  \
                         $(CFLAGS) $(EXEOUT)$*.exe $*.c modelextern.c
	cd $(SMALL1); ./$*.exe

testwrapper/%: $(SMALL1)/%.c $(SMALL1)/wrapperextern.c 
	cd $(SMALL1); $(CCURED) \
                         $(STANDARDPATCH) \
                         --leavealone=wrapperextern  \
                         $(CFLAGS) $(EXEOUT)$*.exe $*.c wrapperextern.c
	cd $(SMALL1); ./$*.exe


combine%: $(SMALL1)/combine%_1.c
	cd $(SMALL1); \
          $(CCURED) $(CFLAGS) \
                    $(notdir $(wildcard $(SMALL1)/combine$*_[1-9].c)) \
                    $(STANDARDPATCH) \
	            $(EXEOUT)combine$*.exe
	cd $(SMALL1); ./combine$*.exe

arcombine: mustbegcc
	cd $(SMALL1); $(CCURED) -c array1.c array2.c
	cd $(SMALL1); $(CCUREDHOME)/bin/cilly \
                           --mode=AR crv array.a array1.o array2.o
	cd $(SMALL1); $(CCURED) -o matrix.exe array.a matrix.c
	cd $(SMALL1); ./matrix.exe

# weimer: test, compile and run
testc/% : $(SMALL1)/%.c  
	cd $(SMALL1); $(CCURED) --separate  \
               $(STANDARDPATCH) \
	       $(CFLAGS) $(EXEOUT)$*.exe $*.c ; ./$*.exe

# Aman's optim tests
OPTIMTESTDIR := $(TESTDIR)/optim
optim/% : $(OPTIMTESTDIR)/%.c 
	cd $(OPTIMTESTDIR); $(CCURED) --separate  \
               $(STANDARDPATCH) \
	       $(CFLAGS) $*.c $(EXEOUT)$*.exe
	$(OPTIMTESTDIR)/$*.exe



hashtest: $(TESTDIR)/small2/hashtest.c 
	rm -f $(PCCTEST)/hashtest.exe
	cd $(PCCTEST); $(CCURED)  \
                                 --keep=. $(DEF)x86_LINUX $(DEF)$(PCCTYPE) \
                 $(CFLAGS) \
                 `$(PATCHECHO) $(STANDARDPATCH)` \
                 $(INC)$(PCCDIR)/src \
                 $(PCCDIR)/src/hash.c \
                 ../small2/hashtest.c \
                 $(EXEOUT)hashtest.exe
	cd $(PCCTEST); sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./hashtest.exe $(HUFFINPUT) ; done"

CCUREDLIB := $(CCUREDHOME)/obj/$(ARCHOS)/ccured_$(COMPILERNAME)_releaselib.$(LIBEXT)

hashtest-optimvariant.%: mustbegcc
	cd $(PCCTEST); \
           $(OPTIMVARIANT) \
                 hashtest.exe_combcured.$*.optim.c \
                 $(CCUREDLIB) \
                 $(EXEOUT)hashtest.exe
	cd $(PCCTEST); sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./hashtest.exe $(HUFFINPUT) ; done"


rbtest: $(TESTDIR)/small2/rbtest.c 
	rm -f $(PCCTEST)/rbtest.exe
	@true "compile with gcc for better error diagnostics (ha!)"
	cd $(PCCTEST); $(CCURED)  \
                                 --keep=. $(DEF)$(ARCHOS) $(DEF)$(PCCTYPE) \
                 `$(PATCHECHO) $(STANDARDPATCH)` \
                 $(OPT_O2) \
                 $(INC)$(PCCDIR)/src \
                 $(PCCDIR)/src/redblack.c \
                 ../small2/rbtest.c \
                 $(EXEOUT)rbtest.exe
	cd $(PCCTEST); sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./rbtest.exe letGcFree ; done"

rbtest-optimvariant.%: mustbegcc
	cd $(PCCTEST); \
           $(OPTIMVARIANT) \
                 rbtest.exe_combcured.$*.optim.c \
                 $(CCUREDLIB) \
                 $(EXEOUT)rbtest.exe
	cd $(PCCTEST); sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./rbtest.exe letGcFree ; done"

btreetest: $(TESTDIR)/small2/testbtree.c \
           $(TESTDIR)/small2/btree.c 
	rm -f $(TESTDIR)/small2/btreetest.exe
	cd $(TESTDIR)/small2; $(CCURED) --keep=. \
                 $(OPT_O2) \
                 $(STANDARDPATCH) \
                 btree.c testbtree.c \
                 $(EXEOUT)btreetest.exe
	cd $(TESTDIR)/small2; sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./btreetest.exe ; done"


btreetest-optimvariant.%: mustbegcc
	cd $(TESTDIR)/small2; \
           $(OPTIMVARIANT) \
                 btreetest.exe_combcured.$*.optim.c \
                 $(CCUREDLIB) \
                 $(EXEOUT)btreetest.exe
	cd $(TESTDIR)/small2; sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./btreetest.exe ; done"

# sm: this is my little test program
hola: scott/hola

# ww: Scott's structs-edg-stl.c example
structs : mustbemanju
	cd /usr/src/big-examples/; $(CCURED) --separate \
               $(STANDARDPATCH) \
	       $(CONLY) $(CFLAGS) structs-edg-stl.c 
	echo SUCCESS

# sm: some project members don't want the testing targets to depend
# on quickbuild (which rebuilds translator components whose dependencies
# have changed), and others do..
ifdef TARGETS_DEP_QUICKBUILD
  # with switch to test/Makefile, doesn't do what I want, so no-op
  #TARGET_DEP := quickbuild
  TARGET_DEP := 
endif

# sm: attempt at a single rule for my testing purposes
scott/%: $(TESTDIR)/small2/%.c $(TARGET_DEP)
	rm -f $(TESTDIR)/small2/$*
	cd $(TESTDIR)/small2; $(CC) $(CONLY) $(WARNALL) $*.c
	cd $(TESTDIR)/small2; $(CCURED) --separate --keep=. \
                 `$(PATCHECHO) $(STANDARDPATCH)` \
                 $(CFLAGS) `true $(WARNALL)` $(NOPRINTLN) \
                 $*.c \
                 $(EXEOUT)$*
	sh -c "time $(TESTDIR)/small2/$*"

scott-nolink/%: $(TESTDIR)/small2/%.c $(TARGET_DEP)
	rm -f $(TESTDIR)/small2/$*
	cd $(TESTDIR)/small2; $(CC) $(CONLY) $(WARNALL) $*.c
	cd $(TESTDIR)/small2; $(CCURED) --separate $(CONLY) --keep=. \
                 `$(PATCHECHO) $(STANDARDPATCH)` \
                 $(CFLAGS) $(WARNALL) $(NOPRINTLN) \
                 $*.c \
                 $(EXEOUT)$*

# sm: a target for programs which are *supposed* to fail, because
# they intentionally violate the type system; but this is only
# when FAIL is #defined, otherwise they should exit ok
# 2/14/02: renamed from bad/% because with that name (and now that
# test/Makefile is being used) it actually collides with the name of
# the generated binary, so sometimes make thinks it's up-to-date!
badd/%: $(TESTDIR)/bad/%.c $(TARGET_DEP)
	rm -f $(TESTDIR)/bad/$*
	cd $(TESTDIR)/bad; $(CC) $(CONLY) $(WARNALL) $*.c
	@true "first try the succeed case"
	cd $(TESTDIR)/bad; $(CCURED) --separate --keep=. \
                 `$(PATCHECHO) $(STANDARDPATCH)` \
                 $(CFLAGS) $(WARNALL) $(NOPRINTLN) \
                 $*.c \
                 $(EXEOUT)$*
	echo $(TESTDIR)/bad/$*
	@if $(TESTDIR)/bad/$*; then \
		echo "(worked as expected, when FAIL not defined)"; exit 0; \
	else \
		echo "That should have worked; FAIL was not defined!"; exit 2; \
	fi
	@true "now try the failure case"
	cd $(TESTDIR)/bad; $(CCURED) --separate --keep=. \
                 `$(PATCHECHO) $(STANDARDPATCH)` \
                 $(CFLAGS) $(WARNALL) $(NOPRINTLN) -DFAIL \
                 $*.c \
                 $(EXEOUT)$*
	echo $(TESTDIR)/bad/$*
	@if $(TESTDIR)/bad/$*; then \
		echo "That should have failed!"; exit 2; \
	else \
		echo "(failed as expected)"; exit 0; \
	fi

# same rules, this time in 'scott' directory, since it's a pain to
# move the file just to add a failure case
bads/%: $(TESTDIR)/small2/%.c $(TARGET_DEP)
	rm -f $(TESTDIR)/small2/$*
	cd $(TESTDIR)/small2; $(CC) $(CONLY) $(WARNALL) $*.c
	@true "first try the succeed case"
	cd $(TESTDIR)/small2; $(CCURED) --separate --keep=. \
                 `$(PATCHECHO) $(STANDARDPATCH)` \
                 $(CFLAGS) $(WARNALL) $(NOPRINTLN) \
                 $*.c \
                 $(EXEOUT)$*
	if $(TESTDIR)/small2/$*; then \
		echo "(worked as expected, when FAIL not defined)"; exit 0; \
	else \
		echo "That should have worked; FAIL was not defined!"; exit 2; \
	fi
	@true "now try the failure case"
	cd $(TESTDIR)/small2; $(CCURED) --separate --keep=. \
                 `$(PATCHECHO) $(STANDARDPATCH)` \
                 $(CFLAGS) $(WARNALL) $(NOPRINTLN) -DFAIL \
                 $*.c \
                 $(EXEOUT)$*
	if $(TESTDIR)/small2/$*; then \
		echo "That should have failed!"; exit 2; \
	else \
		echo "(failed as expected)"; exit 0; \
	fi

# sm: yet another failure-test target, this time utilizing a separate
# script capable of testing multiple failures per file
test-bad/%: $(TESTDIR)/small2/%.c $(TARGET_DEP)
	CCUREDHOME="$(CCUREDHOME)" CCURED="$(CCURED) --separate --noPrintLn" \
	  CFLAGS=""`$(PATCHECHO) $(STANDARDPATCH)`" $(CFLAGS) $(WARNALL)" \
          TESTBADONCE="$(TESTBADONCE)" \
	  bash ../bin/test-bad $*.c

# sm: trivial test of combiner
MYSAFECC := $(CCURED) --keep=. $(STANDARDPATCH)
comb: $(TESTDIR)/small2/comb1.c $(TESTDIR)/small2/comb2.c 
	rm -f $(TESTDIR)/small2/comb
	cd $(TESTDIR)/small2; \
	  $(MYSAFECC)  comb1.c $(CONLY) $(OBJOUT) comb1.o; \
	  $(MYSAFECC)  comb2.c $(CONLY) $(OBJOUT) comb2.o; \
          $(MYSAFECC)  comb1.o comb2.o $(EXEOUT)comb
	$(TESTDIR)/small2/comb

# sm: another merger test
mergestruct: $(TESTDIR)/small2/mergestruct1.c $(TESTDIR)/small2/mergestruct2.c
	cd $(TESTDIR)/small2; \
	  $(CCURED) mergestruct1.c mergestruct2.c -o mergestruct
	$(TESTDIR)/small2/mergestruct

# sm: yet another merger test (I know there's a target somewhere)
mergeinline: $(TESTDIR)/small2/mergeinline1.c $(TESTDIR)/small2/mergeinline2.c
	cd $(TESTDIR)/small2; \
	  $(CCURED) mergeinline1.c mergeinline2.c -o mergeinline
	$(TESTDIR)/small2/mergeinline

# sm: test of combiner's ability to report inconsistencies
baddef: $(TESTDIR)/small2/baddef1.c $(TESTDIR)/small2/baddef2.c 
	cd $(TESTDIR)/small2; $(CC) baddef1.c baddef2.c -o baddef && ./baddef
	rm -f $(TESTDIR)/small2/baddef
	cd $(TESTDIR)/small2; \
	  $(MYSAFECC)  baddef1.c $(CONLY) $(OBJOUT) baddef1.o; \
	  $(MYSAFECC)  baddef2.c $(CONLY) $(OBJOUT) baddef2.o; \
          $(MYSAFECC)  baddef1.o baddef2.o $(EXEOUT)baddef
	$(TESTDIR)/small2/baddef

# cfrac: a memory benchmark which factorizes into products of primes
CFRACDIR := $(CCUREDHOME)/../bench/cfrac
cfrac: 
	-rm $(CFRACDIR)/*.o
	-rm $(CFRACDIR)/cfrac
	make -C $(CFRACDIR) \
	  CC="$(CCURED) --separate --keep=$(CFRACDIR)" \
	  LD="$(CCURED) --separate --keep=$(CFRACDIR)"
	csh -c "time $(CFRACDIR)/cfrac 327905606740421458831903"

comcfrac: 
	-rm $(CFRACDIR)/*.o
	-rm $(CFRACDIR)/cfrac
	make -C $(CFRACDIR) \
	  CC="$(CCURED) --keep=$(CFRACDIR)" \
	  LD="$(CCURED) --keep=$(CFRACDIR)"
	csh -c "time $(CFRACDIR)/cfrac 327905606740421458831903"

# espresso: memory benchmark that does logic minimization
ESPRESSODIR := $(CCUREDHOME)/../bench/espresso
espresso: 
	@true -rm $(ESPRESSODIR)/*.o
	@true -rm $(ESPRESSODIR)/espresso
	make -C $(ESPRESSODIR) \
	  CC="$(CCURED) --keep=$(ESPRESSODIR)" \
	  LD="$(CCURED) --keep=$(ESPRESSODIR)"
	csh -c "time $(ESPRESSODIR)/espresso -t $(ESPRESSODIR)INPUT/Z5xp1.espresso >/dev/null"




HUFFCOMPILE := $(CCURED) $(DEF)NOVARARG --keep=. --keepmerged
# HUFFCOMPILE := cl /MLd
ifdef _GNUCC
  HUFFOTHERS += -lm
endif
ifndef HUFFINPUT
  HUFFINPUT=$(CCUREDHOME)/src/frontc/cparser.output
endif
hufftest: $(TESTDIR)/small2/hufftest.c 
	rm -f $(PCCTEST)/hufftest.exe \
              $(PCCTEST)/huffman.compressed \
              $(PCCTEST)/huffman.code \
              $(PCCTEST)/huffman.freq
	cd $(PCCTEST); $(HUFFCOMPILE) \
                 $(DEF)$(ARCHOS) $(DEF)$(PCCTYPE) $(DEF)$(PCCCOMP) \
                 $(CFLAGS) \
                 $(STANDARDPATCH) \
                 $(INC)$(PCCDIR)/src \
                 $(PCCDIR)/src/io.c \
                 $(PCCDIR)/src/huffman.c \
                 $(PCCDIR)/src/hash.c \
                 ../small2/hufftest.c \
                 $(HUFFOTHERS) \
                 $(EXEOUT)hufftest.exe
	cd $(PCCTEST); sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./hufftest.exe $(HUFFINPUT) ; done"


hufftest-optimvariant.%: mustbegcc
	cd $(PCCTEST); \
           $(OPTIMVARIANT) \
                 hufftest.exe_combcured.$*.optim.c \
                 $(CCUREDLIB) \
                 $(EXEOUT)hufftest.exe
	cd $(PCCTEST); sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./hufftest.exe $(HUFFINPUT) ; done"

wes-rbtest: $(TESTDIR)/small2/wes-rbtest.c 
	rm -f $(PCCTEST)/wes-rbtest.exe
	cd $(PCCTEST); $(CCURED) --keep=. $(DEF)$(ARCHOS) $(DEF)$(PCCTYPE) \
                 $(CFLAGS) \
                 $(STANDARDPATCH) \
                 $(INC)$(PCCDIR)/src \
                 ../small2/wes-rbtest.c \
                 $(EXEOUT)wes-rbtest.exe
	$(PCCTEST)/wes-rbtest.exe

wes-hashtest: $(TESTDIR)/small2/wes-hashtest.c 
	rm -f $(PCCTEST)/wes-hashtest.exe
	cd $(PCCTEST); $(CCURED) --keep=. $(DEF)$(ARCHOS) $(DEF)$(PCCTYPE) \
                 $(CFLAGS) \
                 $(STANDARDPATCH) \
                 $(INC)$(PCCDIR)/src \
                 ../small2/wes-hashtest.c \
                 $(EXEOUT)wes-hashtest.exe
	$(PCCTEST)/wes-hashtest.exe


### Generic test
testfile/% : 
	$(CCURED) /TC $*

testdir/% : 
	make -C CC="ccured" $*


################## Linux device drivers
testlinux/% : $(TESTDIR)/linux/%.cpp 
	cd $(TESTDIR)/linux; $(CCURED) -o $*.o $*.cpp 

testqp : testlinux/qpmouse
testserial: testlinux/generic_serial

################## Rahul's test cases
SPR-TESTDIR := $(TESTDIR)/spr

# Compile .c to .exe and run it
sprrun/% : $(SPR-TESTDIR)/%.c  
	cd $(SPR-TESTDIR); $(CCURED)   \
               $(STANDARDPATCH) \
	       $(CFLAGS) $(EXEOUT)$*.exe $*.c
	cd $(SMALL1); ./$*.exe
	echo SUCCESS

# Compile .c to .s
sprs/% : $(SPR-TESTDIR)/%.c  
	cd $(SPR-TESTDIR); $(CCURED) --separate  \
               $(STANDARDPATCH) \
	       $(ASMONLY)$*.s $*.c
	echo SUCCESS

# Compile .c to .o
spro/% : $(SPR-TESTDIR)/%.c 
	cd $(SPR-TESTDIR); $(CCURED) --separate  \
               $(STANDARDPATCH) \
	       $(CONLY) $*.c 
	echo SUCCESS
################# Apache test cases
APACHETEST := $(TESTDIR)/apache
APACHEBASE := apache_1.3.19/src
APATCHES := --patch=apache.patch --patch=apache_$(COMPILERNAME).patch
ifdef _MSVC
  APACHECFLAGS := /nologo /MDd /W3 /GX /Zi /Od \
         $(INC)"$(APACHEBASE)\include" $(INC)"$(APACHEBASE)\os\win32" \
         $(DEF)"_DEBUG" $(DEF)"WIN32" $(DEF)"_WINDOWS" \
         $(DEF)"NO_DBM_REWRITEMAP" $(DEF)"SHARED_MODULE" \
         $(DEF)"WIN32_LEAN_AND_MEAN"
else
  APACHECFLAGS := -Wall -D_GNUCC -g \
         $(INC)"$(APACHEBASE)/include" $(INC)"$(APACHEBASE)/os/unix" \
         $(DEF)"_DEBUG" \
         $(DEF)"NO_DBM_REWRITEMAP" $(DEF)"SHARED_MODULE"
endif

APACHE_INCLUDES := httpd.h ap_alloc.h http_config.h http_log.h http_protocol.h
apachesetup:
	cd $(APACHETEST); \
            $(PATCHER)  $(APACHECFLAGS) \
                        $(APATCHES) \
                        --dest=$(APACHEBASE)/include \
	                $(foreach file,$(APACHE_INCLUDES), --ufile=$(file))

APATCH := $(STANDARDPATCH) --includedir=$(APACHEBASE)/include

apache/% : $(APACHETEST)/mod_%.c
	rm -f $(APACHETEST)/mod_$*.$(OBJEXT)
	cd $(APACHETEST) ; $(CCURED) \
                       --keep=. $(APATCH) \
                        $(CFLAGS) \
                        $(APACHECFLAGS) \
                        $(CONLY) $(OBJOUT)./mod_$*.$(OBJEXT) \
                        mod_$*.c

# sm: removed CFLAGS since I want to specify optimization in the
# benchmark's Makefile (helps to ensure consistency between the
# non-ccured build and the ccured build, and also some programs
# take too long on -O3)
COMBINECCURED := $(CCURED) --keepmerged

# sm: trying to collapse where are specifications are
PATCHARG := `$(PATCHECHO) $(STANDARDPATCH)`

#
# OLDEN benchmarks
#
all-olden: mustbegcc bh bisort em3d health mst perimeter power treeadd tsp
	@echo "All the Olden benchmarks work"

# Barnes-Hut
BHDIR := $(TESTDIR)/olden/bh
bh:  mustbegcc
	cd $(BHDIR); rm -f code.exe *.o; \
               make CC="$(COMBINECCURED) --leavealone=trusted_bh $(PATCHARG)"
	make runbh $(MAKEOVERRIDES)

bh-combined:  mustbegcc
	cd $(BHDIR); \
	    $(CCURED) --leavealone=trusted_bh \
                      code.exe_comb.c trusted_bh.c $(EXEOUT)code.exe
	make runbh $(MAKEOVERRIDES)

bh-optimvariant.%: mustbegcc
	cd $(BHDIR); \
           $(OPTIMVARIANT) \
                 code.exe_combcured.$*.optim.c \
                 trusted_bh.c -lm \
                 $(CCUREDLIB) \
                 $(EXEOUT)code.exe
	make runbh $(MAKEOVERRIDES)

# sm: rather than make it every time, I've added data.in (with 12 CRs)
# to the repository
runbh:
	cd $(BHDIR); sh ./testit ./code.exe
	cd $(BHDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                             do time ./code.exe < data.in >data.out ; done"


# Power pricing
PWDIR := $(TESTDIR)/olden/power
ifdef _GNUCC
  PWEXTRA += -lm
endif
power:  mustbegcc
	cd $(PWDIR); \
               make PLAIN=1 clean defaulttarget \
                    CC="$(COMBINECCURED) $(PATCHARG)"
	cd $(PWDIR); sh ./testit ./power.exe
	cd $(PWDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                    do time ./power.exe ; done"

power-optimvariant.%: mustbegcc
	cd $(PWDIR); \
           $(OPTIMVARIANT) $(PWEXTRA) \
                 power.exe_combcured.$*.optim.c \
                 $(CCUREDLIB) \
                 $(EXEOUT)power.exe
	cd $(PWDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                    do time ./power.exe ; done"

power-combined :  mustbegcc
	cd $(PWDIR); \
             $(CCURED) power.exe_comb.c $(EXEOUT)power.exe


# Health care simulation
HEALTHDIR=$(TESTDIR)/olden/health
ifdef _MSVC
  HEALTHARGS := _MSVC=1
endif
health: 
	cd $(HEALTHDIR); \
               make PLAIN=1 clean defaulttarget \
                    $(HEALTHARGS) \
                    CC="$(COMBINECCURED) $(PATCHARG)"
	cd $(HEALTHDIR); sh ./testit ./health.exe
	cd $(HEALTHDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                    do time ./health.exe 5 500 1 1; done"

health-optimvariant.%: mustbegcc
	cd $(HEALTHDIR); \
           $(OPTIMVARIANT) -lm \
                 health.exe_combcured.$*.optim.c \
                 $(CCUREDLIB) \
                 $(EXEOUT)health.exe
	cd $(HEALTHDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                    do time ./health.exe 5 500 1 1; done"


# Perimeter of regions in images
PERIMDIR := $(TESTDIR)/olden/perimeter
ifdef _MSVC
  PERIMARGS := _MSVC=1
endif
perimeter: 
	cd $(PERIMDIR); \
               make PLAIN=1 clean defaulttarget  \
                    $(PERIMARGS) \
                    CC="$(COMBINECCURED) \
			$(PATCHARG)"
	cd $(PERIMDIR); sh ./testit ./perimeter.exe
	cd $(PERIMDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./perimeter.exe ; done"

perimeter-optimvariant.%: mustbegcc
	cd $(PERIMDIR); \
           $(OPTIMVARIANT) \
                 perimeter.exe_combcured.$*.optim.c \
                 $(CCUREDLIB) \
                 $(EXEOUT)perimeter.exe
	cd $(PERIMDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./perimeter.exe ; done"

# Voronoi diagrams
VORONDIR := $(TESTDIR)/olden/voronoi
ifdef _MSVC
  VORONARGS := _MSVC=1
endif
voronoi : 
	cd $(VORONDIR); \
               make PLAIN=1 clean voronoi.exe \
                    $(VORONARGS) \
                    CC="$(COMBINECCURED) \
                        --leavealone=trusted_voronoi \
			$(PATCHARG)"
	cd $(VORONDIR); sh -c "time ./voronoi.exe 60000 1"

# Traveling salesman
TSPDIR := $(TESTDIR)/olden/tsp
ifdef _MSVC
  TSPARGS := _MSVC=1
endif
ifdef _GNUCC
  TSPEXTRA += -lm
endif

# sm: tsp's output isn't useful to check, so no explicit self-test
tsp: 
	cd $(TSPDIR); \
               make PLAIN=1 clean defaulttarget \
                    $(TSPARGS) \
                    CC="$(COMBINECCURED) \
			$(PATCHARG)"
	cd $(TSPDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./tsp.exe ; done"

tsp-optimvariant.%: mustbegcc
	cd $(TSPDIR); \
           $(OPTIMVARIANT) $(TSPEXTRA) \
                 tsp.exe_combcured.$*.optim.c \
                 $(CCUREDLIB) \
                 $(EXEOUT)tsp.exe
	cd $(TSPDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./tsp.exe ; done"



# Bitonic sort
# sm: to the best of my knowledge, bisort tests itself, so there is no
# explicit self-test
BISORTDIR := $(TESTDIR)/olden/bisort
ifdef _MSVC
  BISORTARGS = _MSVC=1
endif
bisort:  mustbegcc
	cd $(BISORTDIR); \
               make PLAIN=1 clean defaulttarget \
                    $(BISORTARGS) \
                    CC="$(COMBINECCURED) \
			$(PATCHARG)"
	cd $(BISORTDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./bisort.exe 100000; done"

bisort-optimvariant.%: mustbegcc
	cd $(BISORTDIR); \
           $(OPTIMVARIANT) -lm \
                 bisort.exe_combcured.$*.optim.c \
                 $(CCUREDLIB) \
                 $(EXEOUT)bisort.exe
	cd $(BISORTDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./bisort.exe 100000; done"



OLDENMSTDIR := $(TESTDIR)/olden/mst
OLDENMSTSAFECC := $(COMBINECCURED) $(PATCHARG)
ifdef _MSVC
  OLDENMSTSAFECC += $(DEF)WIN32 $(DEF)MSDOS
  MSTARGS := _MSVC=1
endif
mst-clean: 	
	cd $(OLDENMSTDIR); make clean
	cd $(OLDENMSTDIR); rm -f *cil.c *box.c *.i *_ppp.c *.origi *_all.c

mst: 
	-cd $(OLDENMSTDIR); rm gmon.out
	cd $(OLDENMSTDIR); \
            make clean mst.exe $(MSTARGS) \
                               CC="$(OLDENMSTSAFECC)" \
                               LD="$(OLDENMSTSAFECC)"
	cd $(OLDENMSTDIR); sh ./testit ./mst.exe
	cd $(OLDENMSTDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./mst.exe 2048 1; done"
	cd $(OLDENMSTDIR); if test -f gmon.out; then gprof mst.exe gmon.out; fi


mst-optimvariant.%: mustbegcc
	cd $(OLDENMSTDIR); \
           $(OPTIMVARIANT) \
                 mst.exe_combcured.$*.optim.c \
                 $(CCUREDLIB) \
                 $(EXEOUT)mst.exe
	cd $(OLDENMSTDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./mst.exe 2048 1; done"




TREEADDIR := $(TESTDIR)/olden/treeadd
TREEADDSAFECC := $(CCURED)  $(PATCHARG) $(NOPRINTLN)
ifeq ($(ARCHOS), x86_WIN32)
  TREEADDSAFECC += $(DEF)WIN32 $(DEF)MSDOS
endif
treeadd-clean: 	
	cd $(TREEADDIR); make clean
	cd $(TREEADDIR); rm -f *cil.c *box.c *.i *_ppp.c *.origi *_all.c

treeadd:  mustbegcc
	cd $(TREEADDIR); \
            make clean treeadd.exe CC="$(TREEADDSAFECC)" \
                                   LD="$(TREEADDSAFECC)"
	cd $(TREEADDIR); sh ./testit ./treeadd.exe
	cd $(TREEADDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./treeadd.exe 21 1; done"

treeadd-optimvariant.%: mustbegcc
	cd $(TRERADDIR); \
           $(OPTIMVARIANT) -lm \
                 treeadd.exe_combcured.$*.optim.c \
                 $(CCUREDLIB) \
                 $(EXEOUT)treeadd.exe
	cd $(TREEADDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                    do time ./treeadd.exe 21 1; done"


EM3DDIR := $(TESTDIR)/olden/em3d
EM3DSAFECC := $(CCURED)  $(PATCHARG)
ifeq ($(ARCHOS), x86_WIN32)
  EM3DSAFECC += $(DEF)WIN32 $(DEF)MSDOS
  SS_RAND := TRUE
endif
em3d-clean: 	
	cd $(EM3DDIR); make clean
	cd $(EM3DDIR); rm -f *cil.c *box.c *.i *_ppp.c *.origi *_all.c

# sm: em3d's output doesn't have much to check, so again I have no
# explicit self-test...
em3d:  mustbegcc
	cd $(EM3DDIR); \
            make clean em3d.exe CC="$(EM3DSAFECC)" \
                                LD="$(EM3DSAFECC)"
	cd $(EM3DDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./em3d.exe 2000 100 6; done"

em3d-optimvariant.%: mustbegcc
	cd $(EM3DDIR); \
           $(OPTIMVARIANT) -lm \
                 em3d.exe_combcured.$*.optim.c \
                 $(CCUREDLIB) \
                 $(EXEOUT)em3d.exe
	cd $(EM3DDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                    do time ./em3d.exe 2000 100 6; done"



# SPEC95
SPECDIR := $(TESTDIR)/spec95

# sm: target to build of them that I think work
all-spec95: compress go ijpeg li
	@echo "All the SPEC95 benchmarks seem to work"

COMPRESSDIR := $(SPECDIR)/129.compress
spec-compress : 
	cd $(COMPRESSDIR)/src; make build
	cd $(COMPRESSDIR)/src; ./compress < input.data > output.txt

old-compress :  $(COMPRESSDIR)/src/combine-compress.c
	rm -f $(COMPRESSDIR)/combine-compress.exe
	cd $(COMPRESSDIR)/src ; $(CCURED) --keep=. $(DEF)$(ARCHOS) $(DEF)$(PCCTYPE) \
                 $(CFLAGS) \
                 combine-compress.c \
                 $(EXEOUT)combine-compress.exe
	cd $(COMPRESSDIR)/src; sh -c "time ./combine-compress.exe < input.data > combine-compress.out"

compress-noclean:  mustbegcc
	cd $(COMPRESSDIR)/src; make CC="$(COMBINECCURED)" build
	cd $(COMPRESSDIR)/src; sh -c "time ./compress.exe < input.data > combine-compress.out"

compress:  mustbegcc
	cd $(COMPRESSDIR)/src; \
               make CFLAGS= CC="$(COMBINECCURED) $(PATCHARG)" clean build
	cd $(COMPRESSDIR)/src; sh -c "for i in $(ITERATION_ELEMS) ; \
              do time ./compress.exe <input.data >combine-compress.out ;done"
	cd $(COMPRESSDIR)/src; diff combine-compress.out output.data >/dev/null

compress-optimvariant.%: mustbegcc
	cd $(COMPRESSDIR)/src; \
           $(OPTIMVARIANT) \
                 compress.exe_combcured.$*.optim.c \
                 $(CCUREDLIB) \
                 $(EXEOUT)compress.exe
	cd $(COMPRESSDIR)/src; sh -c "for i in $(ITERATION_ELEMS) ; \
              do time ./compress.exe <input.data >combine-compress.out ;done"


LIDIR := $(SPECDIR)/130.li
LISAFECC := $(CCURED)  $(PATCHARG)
li:  mustbegcc
	cd $(LIDIR)/src; \
            make clean build CC="$(LISAFECC) $(CONLY)" \
                             LD="$(LISAFECC)"
	cd $(LIDIR)/src; sh ./testit ./li.exe
	cd $(LIDIR)/src; sh -c "for i in $(ITERATION_ELEMS) ; \
              do time ./li.exe \
                      <../data/train/input/train.lsp \
                      >../data/train/input/train.out ; done"

li-optimvariant.%: mustbegcc
	cd $(LIDIR)/src; \
           $(OPTIMVARIANT) -lm \
                li.exe_combcured.$*.optim.c \
                $(CCUREDLIB) \
                $(EXEOUT)li.exe
	cd $(LIDIR)/src; sh -c "for i in $(ITERATION_ELEMS) ; \
              do time ./li.exe \
                      <../data/train/input/train.lsp \
                      >../data/train/input/train.out ; done"

li-combined:  mustbegcc
	cd $(LIDIR)/src; \
            $(CCURED) li.exe_comb.c $(LIEXTRA) $(EXEOUT)li.exe

li-noclean:  mustbegcc
	cd $(LIDIR)/src; \
            make build CC="$(LISAFECC) $(CONLY)" \
                       LD="$(LISAFECC)" \
                       EXTRA_LIBS=$(LIEXTRA) 
	sh -c "time $(LIDIR)/src/li.exe \
            <$(LIDIR)/data/train/input/train.lsp \
            >$(LIDIR)/data/train/input/train.out"

liclean: 
	cd $(LIDIR)/src; make clean
	cd $(LIDIR)/src; rm -f *cil.c *box.c *.i *_ppp.c *.origi trial_li_all.c

liinfer: li
	cd $(LIDIR)/src ; $(CCURED) --keep=. $(DEF)$(ARCHOS) $(DEF)$(PCCTYPE) \
                 $(CFLAGS) \
                 trial_li.c \
                 $(EXEOUT)li.exe


### SPEC95 GO
GODIR := $(SPECDIR)/099.go
GOSAFECC := $(CCURED)   $(PATCHARG) $(NOPRINTLN) $(OPT_O2)

goclean: 	
	cd $(GODIR)/src; make clean
	cd $(GODIR)/src; rm -f *cil.c *box.c *.i *_ppp.c *.origi


go:  mustbegcc
	cd $(GODIR)/src; \
            make clean build CC="$(GOSAFECC) $(CONLY)" \
                             LD="$(GOSAFECC)"
	cd $(GODIR)/src; sh ./testit ./go.exe
	cd $(GODIR)/src; sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./go.exe 50 9; done"

go-combined:  mustbegcc
	cd $(GODIR)/src; \
	   $(CCURED) $(CONLY) go_all.c


go-noclean:  mustbegcc
	cd $(GODIR)/src; \
            make build CC="$(GOSAFECC) $(CONLY)" \
                       LD="$(GOSAFECC)" \
                             EXTRA_LIBS=$(GOEXTRA) 
	sh -c "time $(GODIR)/src/go.exe 50 9"

go-optimvariant.%: mustbegcc
	cd $(GODIR)/src; \
           $(OPTIMVARIANT) \
                go.exe_combcured.$*.optim.c \
                $(CCUREDLIB) \
                $(EXEOUT)compress.exe
	cd $(GODIR)/src; sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./go.exe 50 9; done"



### SPEC95 vortex
VORDIR := $(SPECDIR)/147.vortex
VORSAFECC := $(CCURED)    $(PATCHARG)
#VORSAFECC := $(CCURED)  $(PATCHARG)
ifdef _GNUCC
  VOREXTRA := -lm
endif

vortexclean: 	
	cd $(VORDIR)/src; make clean
	cd $(VODIR)/src; rm -f *cil.c *box.c *.i *_ppp.c *.origi

vortex:  mustbegcc
	cd $(VORDIR)/src; \
            make clean build CC="$(VORSAFECC) $(CONLY)" \
                             LD="$(VORSAFECC)"
	cd $(VORDIR)/src; sh -c "for i in $(ITERATION_ELEMS) ; \
                                      do time ./testit vortex.exe; done"

vortex-gcc:  mustbegcc
	cd $(VORDIR)/src; \
            make clean build CC="gcc $(CONLY)" \
                             LD="gcc"
	cd $(VORDIR)/src; sh -c "./testit vortex.exe"

vortex-cabs:   mustbegcc
	cd $(VORDIR)/src; \
            make clean build CC="$(CCURED) --mode=gcc --cabs $(CONLY)" \
                             LD="gcc"
	cd $(VORDIR)/src; sh -c "./testit vortex.exe"

vortex-cil:   mustbegcc
	cd $(VORDIR)/src; \
            make clean build CC="$(CCURED) --cil $(CONLY)" \
                             LD="gcc"
	cd $(VORDIR)/src; sh -c "./testit vortex.exe"
vortex-run:
	cd $(VORDIR)/src; sh -c "./testit vortex.exe"

vortex-noclean:  mustbegcc
	cd $(VORDIR)/src; \
            make build CC="$(VORSAFECC) $(CONLY)" \
                       LD="$(VORSAFECC)"
	cd $(VORDIR)/src; sh -c "./testit vortex.exe"

vortex-combined:  mustbegcc
	cd $(VORDIR)/src; \
            $(CCURED) vortex_all.c -g $(VOREXTRA) $(EXEOUT)vortex.exe
	cd $(VORDIR)/src; sh -c "./testit vortex.exe"

vortex-combined-gcc: mustbegcc
	cd $(VORDIR)/src; \
            gcc vortex_all.c -g \
               $(CCUREDLIB) $(VOREXTRA) $(EXEOUT)vortex.exe
	cd $(VORDIR)/src; sh -c "./testit vortex.exe"

vortex-combined-compare: mustbegcc
	-make vortex-combined-gcc _GNUCC=1
	cp $(VORDIR)/src/data/vortex.out $(VORDIR)/src/data/vortex.gcc.out
	cp $(VORDIR)/src/vortex.exe $(VORDIR)/src/vortex.gcc.exe
	-make vortex-combined _GNUCC=1
	cp $(VORDIR)/src/data/vortex.out $(VORDIR)/src/data/vortex.cil.out
	diff $(VORDIR)/src/data/vortex.cil.out $(VORDIR)/src/data/vortex.gcc.out

vortex-makertl: mustbegcc
	-make vortex-combined _GNUCC=1 TV=1

ifdef _GNUCC
  TVCOMMAND := $(TVDIR)/obj/$(ARCHOS)/transval.asm
else
  TVCOMMAND := $(TVDIR)/obj/$(ARCHOS)/transval.asm.exe
endif

vortex-tv:
	$(TVCOMMAND) -L $(VORDIR)/tv.log $(VORDIR)/src/vortex_all.i.rtl $(VORDIR)/src/vortex_allcil.c.rtl 

### SPEC95 m88ksim
M88DIR := $(SPECDIR)/124.m88ksim

# sm: 12/23/01: found that -I. and -I- don't work with m88k.. as usual I
# have absolutely no idea why it hasn't been a problem until now ..
M88SAFECC := $(CCURED)  $(PATCHARG) \
               --leavealone=m88k_trusted --no-idashi --no-idashdot
m88kclean: 	
	cd $(M88DIR)/src; make clean
	cd $(M88DIR)/src; rm -f *cil.c *box.c *.i *_ppp.c *.origi

m88k:  mustbegcc m88kclean
	cd $(M88DIR)/src; \
            make    build CC="$(M88SAFECC) $(CONLY)" \
                          LD="$(M88SAFECC)" 
	cd $(M88DIR)/src; sh -c "time ./m88k -c < ctl.in > out"
	cd $(M88DIR)/src; diff correct.output out >/dev/null

m88k-noclean:  mustbegcc
	cd $(M88DIR)/src; \
            make       build CC="$(M88SAFECC) $(CONLY)" \
                             LD="$(M88SAFECC)" \
                             EXTRA_LIBS=$(M88EXTRA) 
	cd $(M88DIR)/src; sh -c "time ./m88k -c < ctl.in > out"
	cd $(M88DIR)/src; diff correct.output out >/dev/null

# sm: changed the target below to correspond with not putting the
# executable in exe/base, but didn't test it (don't know what
# it is for)
m88k-combined:  mustbegcc
	cd $(M88DIR)src; \
            $(CCURED) m88k_all.c $(CONLY)

### SPEC95 ijpeg
IJPEGDIR := $(SPECDIR)/132.ijpeg
IJPEGSAFECC := $(CCURED)  $(PATCHARG)
ifeq ($(ARCHOS), x86_WIN32)
  IJPEGSAFECC += -DWIN32 -DMSDOS
endif
ijpegclean: 	
	cd $(IJPEGDIR)/src; make clean
	cd $(IJPEGDIR)/src; rm -f *cil.c *box.c *.i *_ppp.c *.origi

ijpeg:  mustbegcc
	cd $(IJPEGDIR)/src; \
            make clean build CC="$(IJPEGSAFECC) $(CONLY)" \
                             LD="$(IJPEGSAFECC)"
	cd $(IJPEGDIR)/src; sh ./testit ijpeg.exe
	cd $(IJPEGDIR)/src; \
              sh -c "for i in $(ITERATION_ELEMS) ; \
                     do time ./ijpeg.exe \
                          -image_file ../data/ref/input/vigo.ppm \
                          -GO; done"

ijpeg-optimvariant.%: mustbegcc
	cd $(IJPEGDIR)/src; \
           $(OPTIMVARIANT) \
                ijpeg.exe_combcured.$*.optim.c \
                $(CCUREDLIB) \
                $(EXEOUT)ijpeg.exe
	cd $(IJPEGDIR)/src; \
              sh -c "for i in $(ITERATION_ELEMS) ; \
                     do time ./ijpeg.exe \
                          -image_file ../data/ref/input/vigo.ppm \
                          -GO; done"


ijpeg-combined:  mustbegcc
	cd $(IJPEGDIR)/src; \
            $(CCURED) ijpeg_all.c $(IJPEGEXTRA) \
                $(EXEOUT)ijpeg
	sh -c "time $(IJPEGDIR)/src/ijpeg \
            -image_file $(IJPEGDIR)/data/ref/input/penguin.ppm \
            -GO"

ijpeg-noclean:  mustbegcc
	cd $(IJPEGDIR)/src; \
            make       build CC="$(IJPEGSAFECC) $(CONLY)" \
                             LD="$(IJPEGSAFECC)"
	sh -c "time $(IJPEGDIR)/src/ijpeg \
            -image_file $(IJPEGDIR)/data/ref/input/penguin.ppm \
            -GO"

#### SPEC95 gcc
GCCDIR := $(SPECDIR)/126.gcc
GCCSAFECC := $(CCURED)  $(PATCHARG)


gccclean: 	
	cd $(GCCDIR)/src; make clean
	cd $(GCCDIR)/src; rm -f *cil.c *box.c *.i *_ppp.c *.origi

gcc:  mustbegcc
	cd $(GCCDIR)/src; \
            make clean build CC="$(GCCSAFECC) $(CONLY)" \
                             LD="$(GCCSAFECC)" 

gcc-combined:  mustbegcc
	cd $(GCCDIR)/src; \
            $(CCURED) $(CONLY) cc1_comb.c


gcc-gcc:  mustbegcc
	cd $(GCCDIR)/src; \
            make clean build CC="gcc -c" \
                             LD="gcc" 

gcc-gcc-noclean:  mustbegcc
	cd $(GCCDIR)/src; \
            make       build CC="gcc -c" \
                             LD="gcc" 

gcc-noclean:  mustbegcc
	cd $(GCCDIR)/src; \
            make       build CC="$(GCCSAFECC) $(CONLY)" \
                             LD="$(GCCSAFECC)" 
gcc-run:
	cd $(GCCDIR)/src; ./cc1.exe combine.i

#
# Spec2000 gzip
#
SPEC00DIR=$(TESTDIR)/spec00
GZIPDIR=$(SPEC00DIR)/164.gzip
GZIPSOURCES   = bits.c deflate.c gzip.c getopt.c inflate.c lzw.c \
	        spec.c trees.c unlzh.c unlzw.c unpack.c unzip.c util.c zip.c
gzip-clean: 
	cd $(GZIPDIR)/src; rm -f *.$(OBJEXT) *.$(EXEEXT)

gzip-build: gzip-clean
	cd $(GZIPDIR)/src; $(CCURED)  $(GZIPSOURCES) $(EXEOUT)gzip.exe

gzip-run:
	cd $(GZIPDIR)/src; time ./gzip.exe trees.c

gzip-noclean: gzip-build gzip-run

gzip: gzip-clean gzip-build gzip-run

#
# Linux
LINUXDIR := /home/project/linux-2.2.9

linuxstandard: 
	$(MAKE) -C $(LINUXDIR) clean vmlinux \
              MY-CC="gcc"

LINUXCC := $(CCUREDHOME)/bin/ccured --mode=gcc
ifdef NOLINES
  LINUXCC+= --noPrintLn
endif
ifdef COMMLINES
  LINUXCC+= --commPrintLn
endif
linux-cabs:  mustbegcc
	$(MAKE) -C $(LINUXDIR) vmlinux \
              MY-CC="$(LINUXCC) --cabs"

linux-cil:  mustbegcc
	$(MAKE) -C $(LINUXDIR) vmlinux \
              MY-CC="$(LINUXCC) --cil"

linux-clean:
	$(MAKE) -C $(LINUXDIR) clean

combinetest: 
	cd $(TESTDIR)/small1; $(CCURED)  /Fet.exe t.c t1.c

constrainttest:
	$(CAMLC) -o obj/constraint.exe src/constraint.ml
	obj/constraint.exe

### ftpd-BSD-0.3.2-5
FTPDDIR := $(TESTDIR)/ftpd/ftpd
FTPDSAFECC := $(CCURED)  $(PATCHARG)
ifeq ($(ARCHOS), x86_WIN32)
  FTPDSAFECC += $(DEF)WIN32 $(DEF)MSDOS
endif
ftpd-clean: 	
	cd $(FTPDDIR); make clean
	cd $(FTPDDIR); rm -f *cil.c *box.c *.i *_ppp.c *.origi *_all.c

ftpd: ftpd-clean mustbegcc
	cd $(FTPDDIR); \
            make CC="$(FTPDSAFECC)" \
                 LD="$(FTPDSAFECC)"

### wu-ftpd-2.6.1
# sm: commenting this out because there's a section later on
# which talks about 2.6.2
# WUFTPDDIR := $(TESTDIR)/wu-ftpd-2.6.1
# WUFTPDSAFECC := $(CCURED)  $(PATCHARG)
# ifeq ($(ARCHOS), x86_WIN32)
#   WUFTPDSAFECC += $(DEF)WIN32 $(DEF)MSDOS
# endif
# wuftpd-clean: 	
# 	cd $(WUFTPDDIR); make clean; ./configure
# 	cd $(WUFTPDDIR); rm -f *cil.c *box.c *.i *_ppp.c *.origi *_all.c

# wuftpd: mustbegcc
# 	cd $(WUFTPDDIR); \
#             make CC="$(WUFTPDSAFECC)" \
#                  LD="$(WUFTPDSAFECC)"

######################### PTRDIST Benchmarks
ANAGRAMDIR := $(TESTDIR)/ptrdist-1.1/anagram
anagram: mustbegcc
	cd $(ANAGRAMDIR); rm -f *.o; \
             make CC="$(CCURED) $(STANDARDPATCH) "
	cd $(ANAGRAMDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                      do make test; done"

anagram-optimvariant.%: mustbegcc
	cd $(ANAGRAMDIR); \
           $(OPTIMVARIANT) \
                 anagram_combcured.$*.optim.c \
                 $(CCUREDLIB) \
                 $(EXEOUT)anagram
	cd $(ANAGRAMDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                      do make test; done"




BCDIR := $(TESTDIR)/ptrdist-1.1/bc
bc: mustbegcc
	cd $(BCDIR); rm -f *.o; \
            make CC="$(CCURED) $(STANDARDPATCH) "
	cd $(BCDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                      do make test; done"

bc-optimvariant.%: mustbegcc
	cd $(BCDIR); \
           $(OPTIMVARIANT) \
                 bc_combcured.$*.optim.c \
                 $(CCUREDLIB) \
                 $(EXEOUT)bc
	cd $(BCDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                      do make test; done"

FTDIR := $(TESTDIR)/ptrdist-1.1/ft
ft: mustbegcc
	cd $(FTDIR); rm -f *.o; \
           make CC="$(CCURED) $(STANDARDPATCH) "
	cd $(FTDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                      do make test; done"

ft-optimvariant.%: mustbegcc
	cd $(FTDIR); \
           $(OPTIMVARIANT) \
                 ft_combcured.$*.optim.c \
                 $(CCUREDLIB) \
                 $(EXEOUT)ft
	cd $(FTDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                      do make test; done"


KSDIR := $(TESTDIR)/ptrdist-1.1/ks
ks: mustbegcc
	cd $(KSDIR); rm -f *.o; \
           make CC="$(CCURED) $(STANDARDPATCH) "
	cd $(KSDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                      do make test; done"

ks-optimvariant.%: mustbegcc
	cd $(KSDIR); \
           $(OPTIMVARIANT) \
                 ks_combcured.$*.optim.c \
                 $(CCUREDLIB) \
                 $(EXEOUT)ks
	cd $(KSDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                      do make test; done"



YACRDIR := $(TESTDIR)/ptrdist-1.1/yacr2
yacr: mustbegcc
	cd $(YACRDIR); rm -f *.o; \
           make CC="$(CCURED) $(STANDARDPATCH) "
	cd $(YACRDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                      do make test; done"

yacr-optimvariant.%: mustbegcc
	cd $(YACRDIR); \
           $(OPTIMVARIANT) \
                 yacr2_combcured.$*.optim.c \
                 $(CCUREDLIB) \
                 $(EXEOUT)yacr2
	cd $(YACRDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                      do make test; done"
################# LINUX
LINUX_INCLUDES := $(CCUREDHOME)/$(TESTDIR)/linux/include
LINUX_TOPATCH := asm/uaccess.h asm/atomic.h asm/bitops.h \
	         asm/current.h asm/string.h asm/semaphore.h \
                 linux/config.h linux/list.h linux/skbuff.h \
		 linux/etherdevice.h linux/irq_cpustat.h \
		 linux/netdevice.h linux/ide.h linux/cdrom.h \
		 linux/blkdev.h linux/fs.h linux/reiserfs_fs.h \
                 linux/tqueue.h

linuxsetup: mustbelinux
	$(PATCHER)  -D MODULE -D __KERNEL__ -I /usr/src/linux/include \
                    --patch=$(TESTDIR)/linux/linux.patch \
                    --dest=$(LINUX_INCLUDES) \
	            $(foreach file,$(LINUX_TOPATCH), --sfile=$(file))

LINUXPATCH := $(STANDARDPATCH) --includedir=$(LINUX_INCLUDES) \
	-D FP_FAIL_IS_VERBOSE=1 --noStackChecks

# CCured support library for linux modules. Holds wrappers and definitions
# for things like fp_fail(). 
LINUXMODULELIBDIR := $(CCUREDHOME)/test/linux/
LINUXMODULELIB := $(LINUXMODULELIBDIR)/ccured_LinuxModule_release.o
$(LINUXMODULELIB) : 
	cd $(CCUREDHOME)/test/linux ; make

################# LINUX DEVICE DRIVERS
SBULLDIR := $(TESTDIR)/linux/sbull
sbull: mustbegcc mustbelinux $(LINUXMODULELIB)
	cd $(SBULLDIR); ( make clean && make .depend && \
           make CC="$(CCURED) $(LINUXPATCH) --entryPoint='sbull_init'" ) ;
	cd $(LINUXMODULELIBDIR) ; make sbull_cured.o

PCNET32DIR := $(TESTDIR)/linux/pcnet32
pcnet32: mustbegcc mustbelinux $(LINUXMODULELIB)
	cd $(PCNET32DIR); ( make clean && \
           make CC="$(CCURED) $(LINUXPATCH) --entryPoint='pcnet32_init_module'" ) ;
	cd $(LINUXMODULELIBDIR) ; make pcnet32_cured.o

IDECDDIR := $(TESTDIR)/linux/ide-cd
ide-cd: mustbegcc mustbelinux $(LINUXMODULELIB)
	cd $(IDECDDIR); ( make clean && \
           make CC="$(CCURED) $(LINUXPATCH) --entryPoint='ide_cdrom_init'" ) ;
	cd $(LINUXMODULELIBDIR) ; make ide-cd_cured.o

REISERFSDIR := $(TESTDIR)/linux/reiserfs
reiserfs: mustbegcc mustbelinux $(LINUXMODULELIB)
	cd $(REISERFSDIR); ( make clean && \
           make CC="$(CCURED)  \
                       $(LINUXPATCH) --entryPoint='init_reiserfs_fs'" ) ;
	cd $(LINUXMODULELIBDIR) ; make reiserfs_cured.o

reiserfs-combined: mustbegcc mustbelinux \
                        $(LINUXMODULELIB) $(REISERFSDIR)/reiserfs.o_comb.c
	cd $(REISERFSDIR); \
           make fromcomb CC="$(CCURED) \
                            $(LINUXPATCH) --entryPoint='init_reiserfs_fs'"
	cd $(LINUXMODULELIBDIR) ; make reiserfs_cured.o


################# THE LINUX KERNEL
ifndef LINUXSRC
  # it turns out our sources have the path hardcoded, so changing
  # this isn't as easy as it looks..
  LINUXSRC := /usr/src/linux-cil
endif
mustbemanju: 
	@if ! test -d $(LINUXSRC) ; then  \
                echo You dont have the Linux sources in $(LINUXSRC); exit 3; fi

CILLY := $(CCUREDHOME)/bin/cilly
ifdef NOLINES
  CILLY+= --noPrintLn
endif
ifdef COMMLINES
  CILLY+= --commPrintLn
endif
ifdef VERBOSE
  CILLY+= --verbose
endif
ifdef PRINTSTAGES
  CILLY+= --stages
endif
ifdef TV
  CILLY+= --transval=/home/necula/TransVal/obj/x86_LINUX/transval.asm.exe
endif
ifdef USECABS
  CILLY+= --usecabs
endif
ifdef NOCIL
  CILLY+= --nocil=$(NOCIL)
endif

linuxclean: 
	cd $(LINUXSRC); make clean
	-cd $(LINUXSRC); find . \( \
		-name '*cil.c' -o \
		-name '*.exe' -o \
		-name '*.i' -o \
		-name '*.o' -o \
		-name '*.obj' -o \
		-name '*cabs.c' -o \
		-name '*_comb*.c' \
	      	\) -exec rm -f {} \;

# sm: removed the -k option to make since I want it to stop
# when there's an error

linux: mustbegcc mustbelinux mustbemanju linuxclean
	cd $(LINUXSRC) ; make CC="$(CILLY) --separate " \
                              HOSTCC="$(CILLY) --separate " bzImage

linux-noclean:  mustbegcc mustbelinux mustbemanju
	cd $(LINUXSRC) ; make CC="$(CILLY) --separate " \
                              HOSTCC="$(CILLY) --separate "

linux-gcc: mustbelinux mustbemanju linuxclean
	cd $(LINUXSRC); make CC=gcc HOSTCC=gcc

# ww: merge all of the key files in the linux kernel
# "linux-merge" tries to merge the *cil.c versions of the files 
# "linux-merge2" tries to merge the *.i versions of the files
# in the end, both should work 
LINUX_MERGE_SRC = /home/weimer/linux
linux-merge: mustbegcc mustbelinux mustbemanju 
	cd $(LINUX_MERGE_SRC)/merger ; \
        $(CCURED) --commPrintLn --verbose --curetype=none `cat merge-file-list`
linux-merge2: mustbegcc mustbelinux mustbemanju 
	cd $(LINUX_MERGE_SRC)/merger ; \
        $(CCURED) --verbose --curetype=none `cat merge-file-list2`

linux-merge3: mustbegcc mustbelinux mustbemanju 
	cd $(LINUX_MERGE_SRC) ; \
           make CC="$(CILLY) --merge --verbose  " \
                HOSTCC="$(CILLY) --merge --verbose " \
                LD="$(CILLY) --merge --verbose  " \
                AR="$(CILLY) --mode=AR --merge --verbose "


linux-clean3: 
	cd $(LINUX_MERGE_SRC); make clean

######################## SENDMAIL
# source available at www.sendmail.org

SENDMAILSRC := /usr/local/src/sendmail-8.12.1/obj.Linux.2.4.5.i686/sendmail

sendmailclean:
	cd $(SENDMAILSRC); make clean
	-cd $(SENDMAILSRC); find . \( \
		-name '*cil.c' -o \
		-name '*.exe' -o \
		-name '*.i' -o \
		-name '*.o' -o \
		-name '*.obj' -o \
		-name '*cabs.c' -o \
		-name '*_comb*.c' \
	      	\) -exec rm -f {} \;

sendmail: mustbegcc mustbelinux mustbemanju sendmailclean
	cd $(SENDMAILSRC) ; make CC="$(CCURED)"

sendmail-noclean: mustbegcc mustbelinux mustbemanju
	cd $(SENDMAILSRC) ; make CC="$(CCURED)"

sendmail-gcc: mustbelinux mustbemanju linuxclean
	cd $(SENDMAILSRC) ; make CC=gcc

#### ------------- EMACS ---------------------------

# source available at ftp.gnu.org/gnu/emacs

EMACSSRC := /usr/local/src/emacs-21.1

emacsclean:
	cd $(EMACSSRC); make clean
	-cd $(EMACSSRC); find . \( \
		-name '*cil.c' -o \
		-name '*.exe' -o \
		-name '*.i' -o \
		-name '*.o' -o \
		-name '*.obj' -o \
		-name '*cabs.c' -o \
		-name '*_comb*.c' \
	      	\) -exec rm -f {} \;

emacs: mustbegcc mustbelinux mustbemanju emacsclean
	cd $(EMACSSRC) ; make CC="$(CCURED)"

emacs-noclean: mustbegcc mustbelinux mustbemanju
	cd $(EMACSSRC) ; make CC="$(CCURED)"

emacs-gcc: mustbelinux mustbemanju emacsclean
	cd $(EMACSSRC) ; make CC=gcc

#### ------------- PERL ----------------------------

# source available at www.perl.com/pub/a/language/info/software.html#sourcecode

PERLSRC := /usr/local/src/perl-5.6.1
# Must edit the config.sh to change the definition of cc
#

perlclean:
	cd $(PERLSRC); make clean
	-cd $(PERLSRC); find . \( \
		-name '*cil.c' -o \
		-name '*.exe' -o \
		-name '*.i' -o \
		-name '*.o' -o \
		-name '*.obj' -o \
		-name '*cabs.c' -o \
		-name '*_comb*.c' \
	      	\) -exec rm -f {} \;

perl: mustbegcc mustbelinux mustbemanju perlclean
	cd $(PERLSRC) ; make CC="$(CILLY)"

perl-noclean: mustbegcc mustbelinux mustbemanju
	cd $(PERLSRC) ; make CC="$(CILLY)"

perl-gcc: mustbelinux mustbemanju perlclean
	cd $(PERLSRC) ; make CC=gcc

#### ------------- BIND ----------------------------

BINDSRC := /usr/local/src/bind-9.2.0

bindclean:
	cd $(BINDSRC); make clean
	-cd $(BINDSRC); find . \( \
		-name '*cil.c' -o \
		-name '*.exe' -o \
		-name '*.i' -o \
		-name '*.o' -o \
		-name '*.obj' -o \
		-name '*cabs.c' -o \
		-name '*_comb*.c' \
	      	\) -exec rm -f {} \;

bind: mustbegcc mustbelinux mustbemanju bindclean
	cd $(BINDSRC) ; make CC="$(CCURED)"

bind-noclean: mustbegcc mustbelinux mustbemanju
	cd $(BINDSRC) ; make CC="$(CCURED)"

bind-gcc: mustbelinux mustbemanju bindclean
	cd $(BINDSRC) ; make CC=gcc

#### ------------- WU-FTPD -------------------------

# source available at www.wu-ftpd.org


## compiled binaries can be found under /bin ##

WUFTPDSRC := /usr/local/src/wu-ftpd-2.6.2

wuftpdclean:
	cd $(WUFTPDSRC); ./build clean
	-cd $(WUFTPDSRC); find . \( \
		-name '*cil.c' -o \
		-name '*.exe' -o \
		-name '*.i' -o \
		-name '*.o' -o \
		-name '*.obj' -o \
		-name '*cabs.c' -o \
		-name '*_comb*.c' \
	      	\) -exec rm -f {} \;

wuftpd: mustbegcc mustbelinux mustbemanju wuftpdclean
	cd $(WUFTPDSRC) ; ./build CC="$(CCUREDHOME)/bin/cilly --separate" lnx

wuftpd-noclean: mustbegcc mustbelinux mustbemanju
	cd $(WUFTPDSRC) ; ./build CC="$(CCUREDHOME)/bin/cilly --separate" lnx

wuftpd-gcc: mustbelinux mustbemanju wuftpdclean
	cd $(WUFTPDSRC) ; ./build CC=gcc lnx

#### ------------- OPENSSH -------------------------

# downloads available at: 
# ftp://download.sourceforge.net/pub/mirrors/OpenBSD/OpenSSH/portable/

# website at www.openssh.org

OPENSSHSRC := /usr/local/src/openssh-3.0.2p1

opensshclean:
	cd $(OPENSSHSRC); make clean
	-cd $(OPENSSHSRC); find . \( \
		-name '*cil.c' -o \
		-name '*.exe' -o \
		-name '*.i' -o \
		-name '*.o' -o \
		-name '*.obj' -o \
		-name '*cabs.c' -o \
		-name '*_comb*.c' \
	      	\) -exec rm -f {} \;

openssh: mustbegcc mustbelinux mustbemanju opensshclean
	cd $(OPENSSHSRC) ; make CC="$(CCURED)"

openssh-noclean: mustbegcc mustbelinux mustbemanju
	cd $(OPENSSHSRC) ; make CC="$(CCURED)"

openssh-gcc: mustbelinux mustbemanju opensshclean
	cd $(OPENSSHSRC) ; make CC=gcc

#### ------------- APACHE HTTP SERVER --------------

#website: www.apache.org

## It is configured to install binaries under 
## /usr/local/src/apache_1.3.22/bin/bin
## To change, run ./configure --prefix=DIRECTORY_YOU_WANT_TO_PUT_BINARIES

APACHESRC := /usr/local/src/apache_1.3.22

apacheclean:
	cd $(APACHESRC); make clean
	-cd $(APACHESRC); find . \( \
		-name '*cil.c' -o \
		-name '*.exe' -o \
		-name '*.i' -o \
		-name '*.o' -o \
		-name '*.obj' -o \
		-name '*cabs.c' -o \
		-name '*_comb*.c' \
	      	\) -exec rm -f {} \;

apache: mustbegcc mustbelinux mustbemanju apacheclean
	cd $(APACHESRC) ; make CC="$(CCURED)" ; make install

apache-noclean: mustbegcc mustbelinux mustbemanju
	cd $(APACHESRC) ; make CC="$(CCURED)" ; make install

apache-gcc: mustbelinux mustbemanju apacheclean
	cd $(APACHESRC) ; make CC=gcc ; make install

#### ------------- GIMP AND FRIENDS ----------------

# downloads:
#   ftp://ftp.gtk.org/pub/gtk/v1.2/glib-1.2.9.tar.gz
#   ftp://ftp.gtk.org/pub/gtk/v1.2/gtk+-1.2.9.tar.gz
#   ftp://ftp.gimp.org/pub/gimp/v1.2/v1.2.2/gimp-1.2.2.tar.gz
#   ftp://ftp.gimp.org/pub/gimp/v1.2/v1.2.2/gimp-data-extras-1.2.0.tar.gz
#   ftp://ftp.gimp.org/pub/gimp/libs/jpegsrc.v6b.tar.gz
#   ftp://ftp.gimp.org/pub/gimp/libs/libpng-1.0.8.tar.gz
#   ftp://ftp.gimp.org/pub/gimp/libs/mpeg_lib-1.3.1.tar.gz
#   ftp://ftp.gimp.org/pub/gimp/libs/tiff-v3.5.5.tar.gz
#   ftp://ftp.gimp.org/pub/gimp/libs/zlib.tar.gz         (this is zlib-1.1.3)

# library dependency tree:
#
#                        gimp
#                         |
#       +------+-----+----+---+-------+
#       |      |     |        |       |
#      png    jpeg  tiff     gtk+   mpeg
#       |                     |
#      zlib                  glib
#
# (note: this is *not* the directory hierarchy!  directory structure is flat)

# GIMPBLD is the directory which holds each of the directories of
# the unpacked source tarballs
ifndef GIMPBLD
  GIMPBLD := /usr/local/src
endif

# GIMPDEST is the directory where the installed gimp files go
ifndef GIMPDEST
  # sm: feel free to change this default, I set mine in .ccuredrc
  GIMPDEST := $(CCUREDHOME)/gimp
endif

# I'm not sure the best way to do this; I need LD_LIBRARY_PATH to point
# to $(GIMPDEST)/lib.  This Makefile could add it to the environment
# itself, but then the hapless user copying+pasting commands from a
# 'make' invocation would be very confused when they behave differently.
# So I'll try to enforce that it's set right anytime it's needed.
checkgimplib:
	@if echo $$LD_LIBRARY_PATH | grep $(GIMPDEST)/lib >/dev/null; then \
	  echo "ok: LD_LIBRARY_PATH contains $(GIMPDEST)/lib"; \
	else \
	  echo ""; \
	  echo "Your LD_LIBRARY_PATH must contain $(GIMPDEST)/lib."; \
	  echo "Humor me by saying (for bash):"; \
	  echo "  export LD_LIBRARY_PATH=$(GIMPDEST)/lib"; \
	  echo "or (for tcsh):"; \
	  echo "  setenv LD_LIBRARY_PATH $(GIMPDEST)/lib"; \
	  echo ""; \
	  exit 2; \
	fi

# -------- zlib ---------
ifndef ZLIBSRC
  ZLIBSRC := $(GIMPBLD)/zlib-1.1.3
endif

zlib-configure:
	cd $(ZLIBSRC); ./configure --prefix=$(GIMPDEST)

zlibclean:
	cd $(ZLIBSRC); make clean
	-cd $(ZLIBSRC); find . \( \
		-name '*cil.c' -o \
		-name '*.exe' -o \
		-name '*.i' -o \
		-name '*.o' -o \
		-name '*.obj' -o \
		-name '*cabs.c' -o \
		-name '*_comb*.c' \
	      	\) -exec rm -f {} \;

zlib: mustbegcc $(ZLIBSRC)/Makefile mustbelinux zlibclean
	cd $(ZLIBSRC); make CC="$(CILLY)" test

zlib-install:
	cd $(ZLIBSRC); make install

zlib-world: zlib-configure zlib zlib-install


# -------- tiff ---------
ifndef TIFFSRC
  TIFFSRC := $(GIMPBLD)/tiff-v3.5.5
endif

# since tiff's configure doesn't accept a --prefix argument, we go to
# some lengths to specify the destination directories by editing the
# configure script directly
tiff-configure:
	@cd $(TIFFSRC); \
          if [ ! -f configure.orig ]; then \
            echo cp configure configure.orig; \
            cp configure configure.orig; \
          fi
	cd $(TIFFSRC); \
          cp configure configure.tmp; \
	  sed -e 's,/usr/local,$(GIMPDEST),' \
              -e 's,/var/httpd/htdocs/tiff,$(GIMPDEST)/html/tiff,' \
            <configure.tmp >configure; \
          rm configure.tmp
	cd $(TIFFSRC); echo yes | ./configure

tiffclean:
	cd $(TIFFSRC); make clean
	-cd $(TIFFSRC); find . \( \
		-name '*cil.c' -o \
		-name '*.exe' -o \
		-name '*.i' -o \
		-name '*.o' -o \
		-name '*.obj' -o \
		-name '*cabs.c' -o \
		-name '*_comb*.c' \
	      	\) -exec rm -f {} \;

# To test tiff make sure you get and unpack v3.4pics.tar.gz
#   http://uiarchive.uiuc.edu/mirrors/ftp/ftp.uu.net/graphics/tiff/v3.4pics.tar.gz
# This will create a pics subdirectory 
# Then run test_pics.sh -f pics/*.tif
# This will create the .rpt files to be used later in comparisons
#
# sm: since testing tiff requires the .rpt files, and it's a bit of a hassle to
# publish a reference set, I'm disabling the test unless the pics/ directory
# exists
tiff: mustbegcc $(TIFFSRC)/Makefile mustbelinux tiffclean
	cd $(TIFFSRC); make CC="$(CILLY)"
	cd $(TIFFSRC); \
          if [ -d pics ]; then \
            PATH=$$PATH:. make test; \
          fi

tiff-gcc: mustbegcc $(TIFFSRC)/Makefile mustbelinux tiffclean
	cd $(TIFFSRC); make CC="gcc"
	cd $(TIFFSRC); \
          if [ -d pics ]; then \
            PATH=$$PATH:. make test; \
          fi

tiff-install:
	cd $(TIFFSRC); make install

tiff-world: tiff-configure tiff tiff-install


# -------- jpeg ---------
ifndef JPEGSRC
  JPEGSRC := $(GIMPBLD)/jpeg-6b
endif

jpeg-configure:
	cd $(JPEGSRC); ./configure --prefix=$(GIMPDEST)

jpegclean:
	cd $(JPEGSRC); make clean
	-cd $(JPEGSRC); find . \( \
		-name '*cil.c' -o \
		-name '*.exe' -o \
		-name '*.i' -o \
		-name '*.o' -o \
		-name '*.obj' -o \
		-name '*cabs.c' -o \
		-name '*_comb*.c' \
	      	\) -exec rm -f {} \;

jpeg: mustbegcc $(JPEGSRC)/Makefile mustbelinux jpegclean
	cd $(JPEGSRC); make CC="$(CILLY)"; make test

jpeg-gcc: mustbegcc $(JPEGSRC)/Makefile mustbelinux jpegclean
	cd $(JPEGSRC); make CC=gcc; make test

# NOTE: must say 'install-lib' too!
jpeg-install:
	cd $(JPEGSRC); make install install-lib

jpeg-world: jpeg-configure jpeg jpeg-install


# -------- png ---------
ifndef LIBPNGSRC
  LIBPNGSRC := $(GIMPBLD)/libpng-1.0.8
endif

# Make zlib first
# We do not use the makefile for linux because we do
# not want to use DLLs

# the dependency on libz.a should catch build order errors here
libpng-configure: $(GIMPDEST)/lib/libz.a
	cd $(LIBPNGSRC)/..; \
          if [ ! -L zlib ]; then \
            echo "linking zlib"; \
            ln -s $(ZLIBSRC) zlib; \
          fi; \
          if [ ! -L libpng ]; then \
            echo "linking libpng"; \
            ln -s $(LIBPNGSRC) libpng; \
          fi
	cd $(LIBPNGSRC); \
          if [ ! -L Makefile ]; then \
            echo "linking Makefile"; \
            ln -s scripts/makefile.std Makefile; \
          fi

libpngclean:
	cd $(LIBPNGSRC); make clean
	-cd $(LIBPNGSRC); find . \( \
		-name '*cil.c' -o \
		-name '*.exe' -o \
		-name '*.i' -o \
		-name '*.o' -o \
		-name '*.obj' -o \
		-name '*cabs.c' -o \
		-name '*_comb*.c' \
	      	\) -exec rm -f {} \;

libpng: mustbegcc $(LIBPNGSRC)/Makefile mustbelinux libpngclean
	cd $(LIBPNGSRC); make prefix=$(GIMPDEST) CC="$(CILLY)"
	cd $(LIBPNGSRC); make prefix=$(GIMPDEST) test

libpng-gcc: mustbegcc $(LIBPNGSRC)/Makefile mustbelinux libpngclean
	cd $(LIBPNGSRC); make prefix=$(GIMPDEST) CC=gcc; make test

libpng-install:
	cd $(LIBPNGSRC); make prefix=$(GIMPDEST) install

libpng-world: libpng-configure libpng libpng-install


# -------- mpeg ---------
ifndef MPEGSRC
  MPEGSRC := $(GIMPBLD)/mpeg_lib-1.3.1
endif

mpeg-configure:
	cd $(MPEGSRC); ./configure --prefix=$(GIMPDEST)

mpegclean:
	cd $(MPEGSRC); make clean
	-cd $(MPEGSRC); find . \( \
		-name '*cil.c' -o \
		-name '*.exe' -o \
		-name '*.i' -o \
		-name '*.o' -o \
		-name '*.obj' -o \
		-name '*cabs.c' -o \
		-name '*_comb*.c' \
	      	\) -exec rm -f {} \;

mpeg: mustbegcc $(MPEGSRC)/Makefile mustbelinux mpegclean
	cd $(MPEGSRC); make CC="$(CILLY)"; ./mpegtest test.mpg

mpeg-gcc: mustbegcc $(MPEGSRC)/Makefile mustbelinux mpegclean
	cd $(MPEGSRC); make CC=gcc;  ./mpegtest test.mpg

mpeg-install:
	cd $(MPEGSRC); make install

mpeg-world: mpeg-configure mpeg mpeg-install


# -------- glib ---------
ifndef GLIBSRC
  GLIBSRC := $(GIMPBLD)/glib-1.2.9
endif

glib-configure:
	cd $(GLIBSRC); ./configure --prefix=$(GIMPDEST)

glibclean:
	cd $(GLIBSRC); make clean
	-cd $(GLIBSRC); find . \( \
		-name '*cil.c' -o \
		-name '*.exe' -o \
		-name '*.i' -o \
		-name '*.o' -o \
		-name '*.obj' -o \
		-name '*cabs.c' -o \
		-name '*_comb*.c' \
	      	\) -exec rm -f {} \;

# sm: changed 'mustbemanju' to '$(GLIBSRC)/Makefile'
glib: mustbegcc $(GLIBSRC)/Makefile mustbelinux glibclean
	cd $(GLIBSRC); make CC="$(CILLY)"; 
	cd $(GLIBSRC)/tests; make check-TESTS

glib-gcc: mustbegcc $(GLIBSRC)/Makefile mustbelinux glibclean
	cd $(GLIBSRC); make CC=gcc; 
	cd $(GLIBSRC)/tests; make check-TESTS

# this doesn't depend on 'glib' because if it did then 'make glib'
# followed by 'make glib-install' would rebuild everything on 2nd time
# since we don't have proper dependency tracking here
glib-install:
	cd $(GLIBSRC); make install

glib-world: glib-configure glib glib-install


# -------- gtk ---------
ifndef GTKSRC
  GTKSRC := $(GIMPBLD)/gtk+-1.2.9
endif

gtk-configure: checkgimplib
	cd $(GTKSRC); ./configure --prefix=$(GIMPDEST) \
                                  --with-glib-prefix=$(GIMPDEST)

# old: Must invoke 
# old:   ./configure --with-glib=../glib-1.2.9

gtkclean:
	cd $(GTKSRC); make clean
	-cd $(GTKSRC); find . \( \
		-name '*cil.c' -o \
		-name '*.exe' -o \
		-name '*.i' -o \
		-name '*.o' -o \
		-name '*.obj' -o \
		-name '*cabs.c' -o \
		-name '*_comb*.c' \
	      	\) -exec rm -f {} \;

gtk: mustbegcc checkgimplib $(GTKSRC)/Makefile mustbelinux gtkclean
	cd $(GTKSRC); make CC="$(CILLY)"; 

gtk-gcc: mustbegcc checkgimplib $(GTKSRC)/Makefile mustbelinux gtkclean
	cd $(GTKSRC); make CC=gcc; 

gtk-install:
	cd $(GTKSRC); make install

gtk-world: gtk-configure gtk gtk-install


# -------- gimp ---------
# does NOT go through CIL yet ..
ifndef GIMPSRC
  GIMPSRC := $(GIMPBLD)/gimp-1.2.2
endif

gimp-configure:
	cd $(GIMPSRC); ./configure --prefix=$(GIMPDEST) \
                                   --with-gtk-prefix=$(GIMPDEST)

# an unfortunate name collision with "pencil.c" complicates the rule below...
gimpclean:
	cd $(GIMPSRC); make clean
	-cd $(GIMPSRC); find . \( \
		\( -name '*cil.c' -a \! -name 'pencil.c' \) -o \
		-name '*.exe' -o \
		-name '*.i' -o \
		-name '*.o' -o \
		-name '*.obj' -o \
		-name '*cabs.c' -o \
		-name '*_comb*.c' \
	      	\) -exec rm -f {} \;

gimp: mustbegcc checkgimplib $(GIMPSRC)/Makefile mustbelinux gimpclean
	cd $(GIMPSRC); make CC="$(CILLY)"

gimp-resume:
	cd $(GIMPSRC); make CC="$(CILLY)"

gimp-gcc: mustbegcc checkgimplib $(GIMPSRC)/Makefile mustbelinux gimpclean
	cd $(GIMPSRC); make CC=gcc; 

gimp-install:
	cd $(GIMPSRC); make install

gimp-world: gimp-configure gimp gimp-install



# ---------- gimp-data ----------
ifndef GIMPDATASRC
  GIMPDATASRC := $(GIMPBLD)/gimp-data-extras-1.2.0
endif

gimp-data-configure:
	cd $(GIMPDATASRC); ./configure --prefix=$(GIMPDEST) \
                                       --with-gimp-prefix=$(GIMPDEST)

gimp-data-install:
	cd $(GIMPDATASRC); make install

gimp-data-world: gimp-data-configure gimp-data-install


# ----------- GIMP and all libraries ----------
# compile them all; assumes they're already configured
gimpall: zlib tiff libpng jpeg mpeg glib gtk gimp

# go from just-unpacked to completely installed
gimpall-world: checkgimplib
	$(MAKE) glib-world
	$(MAKE) gtk-world
	$(MAKE) jpeg-world
	$(MAKE) zlib-world
	$(MAKE) libpng-world
	$(MAKE) mpeg-world
	$(MAKE) tiff-world
	$(MAKE) gimp-world
	$(MAKE) gimp-data-world


#######
#######  C-TORTURE
#######
# To run all c-torture tests do
# test> testsafec --group=ctorture --run
CTORTDIR := /usr/local/src/gcc/gcc/testsuite/gcc.c-torture
# CILLY := gcc
tort/compile/%: $(CTORTDIR)/compile/%.c mustbemanju mustbegcc
	cd $(CTORTDIR)/compile; $(CILLY) --separate $*.c -c -o $*.o

tort/execute/%: $(CTORTDIR)/execute/%.c mustbemanju mustbegcc
	rm -f $(CTORTDIR)/execute/a.exe
	cd $(CTORTDIR)/execute; $(CILLY) $*.c -o a.exe
	echo Cure complete
	$(CTORTDIR)/execute/a.exe

tort/compat/%: $(CTORTDIR)/compat/%.c mustbemanju mustbegcc
	rm -f $(CTORTDIR)/compat/a.exe
	cd $(CTORTDIR)/compat; $(CILLY) $*.c -o a.exe
	echo Cure complete
	$(CTORTDIR)/compat/a.exe


# ping, from netkit-base-0.17
PINGDIR := $(CCUREDHOME)/test/ping

ping:
	cd $(PINGDIR); rm -f ping.o; make CC="$(CCURED) $(STANDARDPATCH)"
