# -*- Mode: makefile -*- 
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

ifndef BIG_EXAMPLES
 BIG_EXAMPLES := ../../big-examples/
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
		-name '*~' -o \
		-name '*.cil.c' -o \
		-name '*.cured.c' -o \
		-name '*cured.c' -o \
		-name '*.s' -o \
		-name '*.a' -o \
		-name '*.stackdump' -o \
		-name '*.optimcured.c' -o \
                -name '*.cured.*.optim.c' -o \
		-name '*.exe' -o \
		-name '*.i' -o \
		-name '*_ppp.c' -o \
		-name '*.origi' -o \
		-name '*.o' -o \
		-name '*.obj' -o \
		-name '*.cabs.c' -o \
		-name '*.infer.c' -o \
	        -name "*-tmp.c" -o \
		-name '*_comb*.c' -o \
		-name 'libmerge.a*' \
	\) -exec rm -f {} \;
	-find . -name '*.browser' -exec rm -rf {} \;


# sm: infer CCUREDHOME when not set, to ease having multiple trees
ifndef CCUREDHOME
  ifeq ($(ARCHOS), x86_WIN32)
    $(error You have not defined the CCUREDHOME variable)
  else
    export CCUREDHOME := $(shell pwd)/..
  endif
endif

ifdef USECILLY
 CCURED := $(CCUREDHOME)/bin/cilly
else
 CCURED := $(CCUREDHOME)/bin/ccured
endif

CILLY := $(CCUREDHOME)/bin/cilly
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
  ifndef USECILLY
    export OPTIM := 1
    # use runtime library with minimal debugging checks
    export RELEASELIB := 1
    # turn on gcc's optimizer for the resulting executables
    ifndef FAILISVERBOSE
      export FAILISTERSE := 1
      CCURED += --alwaysStopOnError
    endif
  endif
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


# CCURED contains arguments that are passed to ccured
# Pass such arguments in the command line as EXTRAARGS="..."
# NOTE: you should *never* set EXTRAARGS within this Makefile,
# because *any* such settings will be overridden if someone
# specified EXTRAARGS on the command line
CCURED+= --save-temps $(EXTRAARGS)

ifneq ($(INFERBOX),none)
  ifeq ($(INFERBOX),wild)
    # sm: some of my edg-related source changes need to know whether
    # they're being processed for the all-WILD mode
    CCURED+= --allwild $(DEF)WILDBOX
  endif
  ifdef NOEMITBROWSER
     CCURED+= --noemitbrowser
  endif
else
  ifndef USECILLY
     CCURED+= --nocure
  endif
endif

ifdef NOCURE
  CCURED+= --nocure
endif

ifdef NOMANUALBOX
  CCURED+= $(DEF)NOMANUALBOX
endif


ifdef OCAMLDEBUG
  CCURED+= --ocamldebug
endif
ifdef NOGC
  CCURED+= --nogc
endif

ifdef MAXSPLIT
  CCURED+= --maxSplit
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
  CCURED += --optimize
endif
ifdef ANN
  CCURED+= --annotate
endif
ifdef RELEASELIB
  CCURED+= --releaselib -DRELEASELIB
endif
ifdef VERBOSE
  CCURED+= --verbose --warnall
endif
ifdef KEEPMERGED
  CCURED+= --keepmerged
endif
ifdef PRINTSTAGES
  CCURED+= --stages
endif
ifdef FAILISTERSE
  CCURED+= --failIsTerse
endif
ifdef EMITINFER
  CCURED+= --emitinfer
endif
ifdef EMITINFERDETAIL
  CCURED+= --emitinfer --emitGraphDetailLevel=3
endif
ifdef BROWSER_SOURCE_FILE_SIZE
  CCURED+= --browserSourceFileSize=$(BROWSER_SOURCE_FILE_SIZE)
endif
ifdef BROWSER_NODE_FILE_SIZE
  CCURED+= --browserNodeFileSize=$(BROWSER_NODE_FILE_SIZE)
endif
ifdef CABSONLY
  CCURED+= --cabsonly
  CILLY+= --cabsonly
endif
ifdef CCURED_FAIL_IS_VERBOSE
  # note that we also add this to CCUREDLIBARG in Makefile.ccured; this
  # affects the instrumented code *and* the support library
  CCURED+= $(DEF)CCURED_FAIL_IS_VERBOSE
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
  ifndef USECILLY
    CCURED+= --stats
  endif
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

# when SEPARATE is defined, merging is disabled
ifdef SEPARATE
  CCURED+= --nomerge
endif

# when UNTAGGEDFNS is defined, and the WILD solver is being used,
# functions are *never* tagged, even when they're used unsafely
ifdef UNTAGGEDFNS
  CCURED+= --wildUntaggedFns
endif

# when TAGALLFNS is defined, and the WILD solver is being used,
# functions are *always* tagged
ifdef TAGALLFNS
  CCURED+= --wildTagAllFns
endif

# when TAGFILE is defined, it names a file that lists, one per
# line, the functions which should be tagged
ifdef TAGFILE
  CCURED+= --wildTagTheseFns=$(TAGFILE)
endif

# sm: this will make gcc warnings into errors; it's almost never
# what we want, but for a particular testcase (combine_copyptrs)
# I need it to show the difference between something which works
# and something which will cause inference problems later
ifdef WARNINGS_ARE_ERRORS
  CFLAGS += -Werror
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

# Enable profiling
ifdef PROFILE
  CCURED+= -pg
endif

# sm: use this instead of "sh ./testit" for those self-tests which can't
# be made to work on windows; it does nothing, and has no output
UNIXTESTIT := sh ./testit


# ----------- below here are rules for building benchmarks --------

CC_OPTIMVARIANT:= $(CC) $(DEF)_$(COMPILERNAME) \
                 $(DEF)CIL \
                 $(DEF)CCURED $(DEF)RELEASELIB \
                 $(INC)$(CCUREDHOME)/include \
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
# Set ITERATION_ELEMS to a list
ITERATION_ELEMS := $(shell perl -e 'for ($$i=1; $$i <= $(ITERATIONS); $$i++) { print("$$i "); }')


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
CXXDIR := $(TESTDIR)/cxx

test/% : $(SMALL1)/%.c 
	cd $(SMALL1); $(CCURED) --nomerge --commPrintLn \
	       $(CONLY) $(CFLAGS) $(ASMONLY)$*.s $*.c 
	echo SUCCESS

testnopatch/% : $(SMALL1)/%.c 
	cd $(SMALL1); $(CCURED)    --commPrintLn \
               $(CONLY) $(CFLAGS) $(ASMONLY)$*.s $*.c 

testexe/% : $(SMALL1)/%.c  
	cd $(SMALL1); $(CCURED) --nomerge  --commPrintLn  \
	       $(CFLAGS) $(EXEOUT)$*.exe $*.c 


testrun/% : $(SMALL1)/%.c  
	cd $(SMALL1); $(CCURED) --nomerge  --commPrintLn  \
	       $(CFLAGS) $(EXEOUT)$*.exe $*.c
	cd $(SMALL1); ./$*.exe
	echo SUCCESS

test_withtrusted/% : $(SMALL1)/%.c $(SMALL1)/%_trusted.c
	cd $(SMALL1); $(CCURED) --leavealone=$*_trusted  --commPrintLn \
	       $(CFLAGS) $(EXEOUT)$*.exe $*.c $*_trusted.c 
	cd $(SMALL1); ./$*.exe
	echo SUCCESS

cxx/% : $(CXXDIR)/%.cc  
	cd $(CXXDIR); $(CILLY) --nomerge --cxx \
               $(CFLAGS) $(EXEOUT)$*.exe $*.cc
	cd $(CXXDIR); ./$*.exe
	echo SUCCESS



testmodel/%: $(SMALL1)/%.c $(SMALL1)/modelextern.c 
	cd $(SMALL1); $(CCURED) \
                         --leavealone=modelextern  \
                         $(CFLAGS) $(EXEOUT)$*.exe $*.c modelextern.c
	cd $(SMALL1); ./$*.exe

testwrapper/%: $(SMALL1)/%.c $(SMALL1)/wrapperextern.c 
	cd $(SMALL1); $(CCURED) \
                         --leavealone=wrapperextern  \
                         $(CFLAGS) $(EXEOUT)$*.exe $*.c wrapperextern.c
	cd $(SMALL1); ./$*.exe


#test of "cilly --heapify"
test_heapify: $(SMALL1)/heapify.c  
	cd $(SMALL1); $(CILLY) --doheapify --keepunused  \
	       $(CFLAGS) $(EXEOUT)heapify.exe heapify.c
	cd $(SMALL1); ./heapify.exe
	echo SUCCESS


combine%: $(SMALL1)/combine%_1.c
	cd $(SMALL1); \
          $(CCURED) $(CFLAGS) \
                    $(notdir $(wildcard $(SMALL1)/combine$*_[1-9].c)) \
	            $(EXEOUT)combine$*.exe
	cd $(SMALL1); ./combine$*.exe

arcombine: mustbegcc
	cd $(SMALL1); $(CCURED) -c array1.c array2.c
	cd $(SMALL1); $(CCUREDHOME)/bin/cilly \
                           --mode=AR --merge --verbose crv array.a array1.o array2.o
	cd $(SMALL1); $(CCURED) -o matrix.exe array.a matrix.c
	cd $(SMALL1); ./matrix.exe

# weimer: test, compile and run
testc/% : $(SMALL1)/%.c  
	cd $(SMALL1); $(CCURED) --nomerge  \
	       $(CFLAGS) $(EXEOUT)$*.exe $*.c ; ./$*.exe

# Aman's optim tests
OPTIMTESTDIR := $(TESTDIR)/optim
optim/% : $(OPTIMTESTDIR)/%.c 
	cd $(OPTIMTESTDIR); $(CCURED) --nomerge  \
	       $(CFLAGS) $*.c $(EXEOUT)$*.exe
	$(OPTIMTESTDIR)/$*.exe



hashtest: $(TESTDIR)/small2/hashtest.c 
	rm -f $(PCCTEST)/hashtest.exe
	cd $(PCCTEST); $(CCURED)  \
                                 --keep=. $(DEF)x86_LINUX $(DEF)$(PCCTYPE) \
                 $(CFLAGS) \
                 $(INC)$(PCCDIR)/src \
                 $(PCCDIR)/src/hash.c \
                 ../small2/hashtest.c \
                 $(EXEOUT)hashtest.exe
	cd $(PCCTEST); sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./hashtest.exe $(HUFFINPUT) ; done"

CCUREDLIB := $(CCUREDHOME)/obj/$(ARCHOS)/ccured_$(COMPILERNAME)_releaselib.$(LIBEXT)

hashtest-optimvariant: mustbegcc
	cd $(PCCTEST); rm -f hashtest.exe 
	cd $(PCCTEST); \
           $(CC_OPTIMVARIANT) \
                 hashtest.exe_comboptimcured.$(OPTIMVARIANT).c \
                 $(CCUREDLIB) \
                 $(EXEOUT)hashtest.exe
	cd $(PCCTEST); sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./hashtest.exe $(HUFFINPUT) ; done"


rbtest: $(TESTDIR)/small2/rbtest.c 
	rm -f $(PCCTEST)/rbtest.exe
	@true "compile with gcc for better error diagnostics (ha!)"
	cd $(PCCTEST); $(CCURED)  \
                                 --keep=. $(DEF)$(ARCHOS) $(DEF)$(PCCTYPE) \
                 $(CFLAGS) \
                 $(INC)$(PCCDIR)/src \
                 $(PCCDIR)/src/redblack.c \
                 ../small2/rbtest.c \
                 $(EXEOUT)rbtest.exe
	cd $(PCCTEST); sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./rbtest.exe letGcFree ; done"

rbtest-optimvariant: mustbegcc
	cd $(PCCTEST); rm -f rbtest.exe
	cd $(PCCTEST); \
           $(CC_OPTIMVARIANT) \
                 rbtest.exe_comboptimcured.$(OPTIMVARIANT).c \
                 $(CCUREDLIB) \
                 $(EXEOUT)rbtest.exe
	cd $(PCCTEST); sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./rbtest.exe letGcFree ; done"

btreetest: $(TESTDIR)/small2/testbtree.c \
           $(TESTDIR)/small2/btree.c 
	rm -f $(TESTDIR)/small2/btreetest.exe
	cd $(TESTDIR)/small2; $(CCURED) --keep=. \
                 $(CFLAGS) \
                 btree.c testbtree.c \
                 $(EXEOUT)btreetest.exe
	cd $(TESTDIR)/small2; sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./btreetest.exe ; done"


btreetest-optimvariant: mustbegcc
	cd $(TESTDIR)/small2; rm -f btreetest.exe
	cd $(TESTDIR)/small2; \
           $(CC_OPTIMVARIANT) \
                 btreetest.exe_comboptimcured.$(OPTIMVARIANT).c \
                 $(CCUREDLIB) \
                 $(EXEOUT)btreetest.exe
	cd $(TESTDIR)/small2; sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./btreetest.exe ; done"

# sm: this is my little test program
hola: scott/hola

# ww: Scott's structs-edg-stl.c example
structs : mustbemanju
	cd /usr/src/big-examples/; $(CCURED) --nomerge \
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

ifndef NOPRINTLN
 NOPRINTLN := --commPrintLn
endif

# sm: attempt at a single rule for my testing purposes
# gn: I added .exe so that I can delete the executables
scott/%: $(TESTDIR)/small2/%.c $(TARGET_DEP)
	rm -f $(TESTDIR)/small2/$*.exe
	cd $(TESTDIR)/small2; $(CC) $(CONLY) $(WARNALL) $*.c
	cd $(TESTDIR)/small2; $(CCURED) --nomerge --keep=. \
                 $(CFLAGS) $(NOPRINTLN) \
                 $*.c \
                 $(EXEOUT)$*.exe
	sh -c "time $(TESTDIR)/small2/$*.exe"

scott-nolink/%: $(TESTDIR)/small2/%.c $(TARGET_DEP)
	rm -f $(TESTDIR)/small2/$*.exe
	cd $(TESTDIR)/small2; $(CC) $(CONLY) $(WARNALL) $*.c
	cd $(TESTDIR)/small2; $(CCURED) --nomerge $(CONLY) --keep=. \
                 $(CFLAGS) $(WARNALL) $(NOPRINTLN) \
                 $*.c \
                 $(EXEOUT)$*.exe

#
# Some performance tests
perf/%: $(TESTDIR)/small1/%.c
	rm -f $(TESTDIR)/small1/$*
	cd $(TESTDIR)/small1; \
             $(CCURED) --nomerge --keep=. \
                 $(CFLAGS) `true $(WARNALL)` $(NOPRINTLN) \
                 $*.c \
                 $(EXEOUT)$*.exe
	cd $(TESTDIR)/small1; sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./$*.exe ; done"

perf/%-optimvariant: $(TESTDIR)/small1/%.optimcured.c
	cd $(TESTDIR)/small1; rm -f $*.exe
	cd $(TESTDIR)/small1; \
           $(CC_OPTIMVARIANT) \
                 $*optimcured.$(OPTIMVARIANT).c \
                 $(CCUREDLIB) \
                 $(EXEOUT)$*.exe
	cd $(TESTDIR)/small1; sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./$*.exe ; done"


# sm: a target for programs which are *supposed* to fail, because
# they intentionally violate the type system; but this is only
# when FAIL is #defined, otherwise they should exit ok
# 2/14/02: renamed from bad/% because with that name (and now that
# test/Makefile is being used) it actually collides with the name of
# the generated binary, so sometimes make thinks it's up-to-date!
badd/%: $(TESTDIR)/bad/%.c $(TARGET_DEP)
	rm -f $(TESTDIR)/bad/$*.exe
	cd $(TESTDIR)/bad; $(CC) $(CONLY) $(WARNALL) $*.c
	@true "first try the succeed case"
	cd $(TESTDIR)/bad; $(CCURED) --nomerge --keep=. \
                 -g $(CFLAGS) $(WARNALL) $(NOPRINTLN) \
                 $*.c \
                 $(EXEOUT)$*.exe
	echo $(TESTDIR)/bad/$*.exe
	@if $(TESTDIR)/bad/$*.exe; then \
		echo "(worked as expected, when FAIL not defined)"; exit 0; \
	else \
		echo "That should have worked; FAIL was not defined!"; exit 2; \
	fi
	@true "now try the failure case"
	cd $(TESTDIR)/bad; $(CCURED) --nomerge --keep=. \
                 $(CFLAGS) $(WARNALL) $(NOPRINTLN) -DFAIL \
                 $*.c \
                 $(EXEOUT)$*.exe
	echo $(TESTDIR)/bad/$*.exe
	@if $(TESTDIR)/bad/$*.exe; then \
		echo "That should have failed!"; exit 2; \
	else \
		echo "(failed as expected)"; exit 0; \
	fi

# same rules, this time in 'scott' directory, since it's a pain to
# move the file just to add a failure case
bads/%: $(TESTDIR)/small2/%.c $(TARGET_DEP)
	rm -f $(TESTDIR)/small2/$*.exe
	cd $(TESTDIR)/small2; $(CC) $(CONLY) $(WARNALL) $*.c
	@true "first try the succeed case"
	cd $(TESTDIR)/small2; $(CCURED) --nomerge --keep=. \
                 $(CFLAGS) $(WARNALL) $(NOPRINTLN) \
                 $*.c \
                 $(EXEOUT)$*.exe
	if $(TESTDIR)/small2/$*.exe; then \
		echo "(worked as expected, when FAIL not defined)"; exit 0; \
	else \
		echo "That should have worked; FAIL was not defined!"; exit 2; \
	fi
	@true "now try the failure case"
	cd $(TESTDIR)/small2; $(CCURED) --nomerge --keep=. \
                 $(CFLAGS) $(WARNALL) $(NOPRINTLN) -DFAIL \
                 $*.c \
                 $(EXEOUT)$*.exe
	if $(TESTDIR)/small2/$*.exe; then \
		echo "That should have failed!"; exit 2; \
	else \
		echo "(failed as expected)"; exit 0; \
	fi

# sm: yet another failure-test target, this time utilizing a nomerge
# script capable of testing multiple failures per file
test-bad/%: $(TESTDIR)/small2/%.c $(TARGET_DEP)
ifdef _MSVC
ifndef RELEASE
	@echo ">>>>> Set RELEASE=1 when using MSVC to avoid those\
	pesky dialog boxes. <<<<<"
endif
endif
	cd $(TESTDIR)/small2; \
	  CCUREDHOME="$(CCUREDHOME)" \
	  CCURED="$(CCURED) --nomerge --commPrintLn" \
	  CFLAGS="$(CFLAGS) $(WARNALL)" \
          TESTBADONCE="$(TESTBADONCE)" \
	  _MSVC="$(_MSVC)" \
	  bash ../../bin/test-bad $*.c

test-bad1/%: $(TESTDIR)/small1/%.c $(TARGET_DEP)
	cd $(TESTDIR)/small1; \
	  CCUREDHOME="$(CCUREDHOME)" \
	  CCURED="$(CCURED) --nomerge --commPrintLn" \
	  CFLAGS=" $(CFLAGS) $(WARNALL)" \
          TESTBADONCE="$(TESTBADONCE)" \
	  _MSVC="$(_MSVC)" \
	  bash ../../bin/test-bad $*.c

test-bad-ln/%: $(TESTDIR)/small2/%.c $(TARGET_DEP)
	cd $(TESTDIR)/small2; \
	  CCUREDHOME="$(CCUREDHOME)" \
	  CCURED="$(CCURED) --nomerge " \
	  CFLAGS="$(CFLAGS) $(WARNALL)" \
          TESTBADONCE="$(TESTBADONCE)" \
	  _MSVC="$(_MSVC)" \
	  bash ../../bin/test-bad $*.c

test-bad-wrapper/%: $(TESTDIR)/small2/%.c $(TESTDIR)/small2/wrapperextern.c 
	cd $(TESTDIR)/small2; \
	  CCUREDHOME="$(CCUREDHOME)" \
	  CCURED="$(CCURED) --nomerge --leavealone=wrapperextern --commPrintLn" \
	  CFLAGS="wrapperextern.c $(CFLAGS) $(WARNALL)" \
          TESTBADONCE="$(TESTBADONCE)" \
	  _MSVC="$(_MSVC)" \
	  bash ../../bin/test-bad $*.c 


# sm: trivial test of combiner
MYSAFECC := $(CCURED) --keep=.
comb: $(TESTDIR)/small2/comb1.c $(TESTDIR)/small2/comb2.c 
	rm -f $(TESTDIR)/small2/comb.exe
	cd $(TESTDIR)/small2; \
	  $(MYSAFECC)  comb1.c $(CONLY) $(OBJOUT) comb1.o; \
	  $(MYSAFECC)  comb2.c $(CONLY) $(OBJOUT) comb2.o; \
          $(MYSAFECC)  comb1.o comb2.o $(EXEOUT)comb.exe
	$(TESTDIR)/small2/comb.exe

# sm: another merger test
mergestruct: $(TESTDIR)/small2/mergestruct1.c $(TESTDIR)/small2/mergestruct2.c
	cd $(TESTDIR)/small2; \
	  $(CCURED) mergestruct1.c mergestruct2.c -o mergestruct.exe
	$(TESTDIR)/small2/mergestruct.exe

# sm: yet another merger test (I know there's a target somewhere)
mergeinline: $(TESTDIR)/small2/mergeinline1.c $(TESTDIR)/small2/mergeinline2.c
	cd $(TESTDIR)/small2; \
	  $(CCURED) mergeinline1.c mergeinline2.c -o mergeinline.exe
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

# sm: Run CCured in all-wild mode, and see if we get the right answer.
# The final grep will succeed only once the inference correctly says
# the argument to __throw_setup is WILD.
doThrowFv:
	cd $(TESTDIR)/small2; $(CCURED) --nomerge -c --keep=. doThrowFv.c
	cd $(TESTDIR)/small2; grep throw_setup doThrowFv.cured.c
	cd $(TESTDIR)/small2; grep throw_setup_ww doThrowFv.cured.c

# sm: compile two files separately, then link them using a tagfile
# (the tagfile is called tagfile.txt)
# update: run this with TAGFILE=tagfile.txt
test-tagfile:
	cd $(TESTDIR)/small2; $(CCURED) --nomerge -o tagfile.exe \
	  tagfile1.c tagfile2.c && ./tagfile.exe

# cfrac: a memory benchmark which factorizes into products of primes
CFRACDIR := $(CCUREDHOME)/../bench/cfrac
cfrac: 
	-rm $(CFRACDIR)/*.o
	-rm $(CFRACDIR)/cfrac.exe
	make -C $(CFRACDIR) \
	  CC="$(CCURED) --nomerge --keep=$(CFRACDIR)" \
	  LD="$(CCURED) --nomerge --keep=$(CFRACDIR)"
	csh -c "time $(CFRACDIR)/cfrac.exe 327905606740421458831903"

comcfrac: 
	-rm $(CFRACDIR)/*.o
	-rm $(CFRACDIR)/cfrac.exe
	make -C $(CFRACDIR) \
	  CC="$(CCURED) --keep=$(CFRACDIR)" \
	  LD="$(CCURED) --keep=$(CFRACDIR)"
	csh -c "time $(CFRACDIR)/cfrac.exe 327905606740421458831903"

# espresso: memory benchmark that does logic minimization
ESPRESSODIR := $(CCUREDHOME)/../bench/espresso.exe
espresso: 
	@true -rm $(ESPRESSODIR)/*.o
	@true -rm $(ESPRESSODIR)/espresso.exe
	make -C $(ESPRESSODIR) \
	  CC="$(CCURED) --keep=$(ESPRESSODIR)" \
	  LD="$(CCURED) --keep=$(ESPRESSODIR)"
	csh -c "time $(ESPRESSODIR)/espresso.exe -t $(ESPRESSODIR)INPUT/Z5xp1.espresso >/dev/null"




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
                 $(INC)$(PCCDIR)/src \
                 $(PCCDIR)/src/io.c \
                 $(PCCDIR)/src/huffman.c \
                 $(PCCDIR)/src/hash.c \
                 ../small2/hufftest.c \
                 $(HUFFOTHERS) \
                 $(EXEOUT)hufftest.exe
	cd $(PCCTEST); sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./hufftest.exe $(HUFFINPUT) ; done"


hufftest-optimvariant: mustbegcc
	cd $(PCCTEST); rm -f hufftest.exe
	cd $(PCCTEST); \
           $(CC_OPTIMVARIANT) \
                 hufftest.exe_comboptimcured.$(OPTIMVARIANT).c \
                 $(CCUREDLIB) \
                 $(EXEOUT)hufftest.exe
	cd $(PCCTEST); sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./hufftest.exe $(HUFFINPUT) ; done"

wes-rbtest: $(TESTDIR)/small2/wes-rbtest.c 
	rm -f $(PCCTEST)/wes-rbtest.exe
	cd $(PCCTEST); $(CCURED) --keep=. $(DEF)$(ARCHOS) $(DEF)$(PCCTYPE) \
                 $(CFLAGS) \
                 $(INC)$(PCCDIR)/src \
                 ../small2/wes-rbtest.c \
                 $(EXEOUT)wes-rbtest.exe
	$(PCCTEST)/wes-rbtest.exe

wes-hashtest: $(TESTDIR)/small2/wes-hashtest.c 
	rm -f $(PCCTEST)/wes-hashtest.exe
	cd $(PCCTEST); $(CCURED) --keep=. $(DEF)$(ARCHOS) $(DEF)$(PCCTYPE) \
                 $(CFLAGS) \
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
	       $(CFLAGS) $(EXEOUT)$*.exe $*.c
	cd $(SMALL1); ./$*.exe
	echo SUCCESS

# Compile .c to .s
sprs/% : $(SPR-TESTDIR)/%.c  
	cd $(SPR-TESTDIR); $(CCURED) --nomerge  \
	       $(ASMONLY)$*.s $*.c
	echo SUCCESS

# Compile .c to .o
spro/% : $(SPR-TESTDIR)/%.c 
	cd $(SPR-TESTDIR); $(CCURED) --nomerge  \
	       $(CONLY) $*.c 
	echo SUCCESS

###########################################################################
#                                  apache 1.3.9
###########################################################################
APACHESRC := $(BIG_EXAMPLES)/apache_1.3.9

apacheclean:
	cd $(APACHESRC); if test -f Makefile ; then $(MAKE) clean ; else ./configure ; fi
	-cd $(APACHESRC); find . \( \
		-name '*.cil.c' -o \
		-name '*.exe' -o \
		-name '*.i' -o \
		-name '*.o' -o \
		-name '*.obj' -o \
		-name '*.cabs.c' -o \
		-name '*_comb*.c' \
	      	\) -exec rm -f {} \;
	(cd $(APACHESRC)/target/bin ; ./apachectl stop) || echo "Not running."

apache-cil: mustbegcc mustbelinux 
	cd $(APACHESRC) ; ./configure --disable-module=asis --enable-module=so --prefix=`pwd`/target ; make -e \
              CC="$(CILLY) $(STANDARDPATCH)" \
              MAKE="make -e" \
                LD="$(CILLY) --merge --verbose --commPrintLn " \
                AR="$(CILLY) --mode=AR --merge --verbose " \
                RANLIB="echo" all install

# This test builds Apache using CIL and then runs the HTTP daemon and
# serves up a webpage. We then compared the web page against what we know
# it should look like and shut down the web server. 
apache-cil-test: apache-cil
	echo "Port 9999" >> $(APACHESRC)/target/conf/httpd.conf
	cd $(APACHESRC)/target/bin ; ./apachectl start
	cd $(APACHESRC)/test-files ; sleep 3s ; rm -f index.html ; wget http://localhost:9999/index.html 
	cd $(APACHESRC)/target/bin ; ./apachectl stop
	cd $(APACHESRC)/test-files ; diff index.html index.html.default 
	echo "apache-cil-test: SUCCESS"

APATCHES := --patch=apache.patch --patch=apache_$(COMPILERNAME).patch
APACHE_INCLUDES := httpd.h alloc.h http_config.h http_log.h http_protocol.h

apache-setup:
	cd $(APACHESRC)/ccured-modules ; \
          $(PATCHER)  $(APACHECFLAGS) \
          $(INC)"../../$(APACHESRC)/src/include" $(INC)"../../$(APACHESRC)/src/os/unix" \
          $(APATCHES) \
          --dest=include \
          $(foreach file,$(APACHE_INCLUDES), --ufile=$(file))

# This test cures the give module (e.g., make a-m-c/asis loads mod_asis)
# and then loads it into a normal apache and serves up a webpage. The
# result is compared against a gold standard. Then we shut down teh web
# server. 
a-m-c/%: apache-setup
	cd $(APACHESRC)/ccured-modules ; ( $(CCURED) --includedir=include --assumePrintf --curetype=infer -DINFERBOX mod_$*.c -I ../target/include/ --curedout=mod_$*.cured.c || echo "Undefined references to non-mangled apache functions are OK at this stage" ) ; ../target/bin/apxs -c mod_$*cured.i /home/weimer/cil/obj/x86_LINUX/ccured_GNUCC_debug.a ; ../target/bin/apxs -i mod_$*cured.so 
	cd $(APACHESRC)/test-files ; cp httpd.conf.local ../target/conf/httpd.conf ; echo "LoadModule $*_module libexec/mod_$*cured.so" >> ../target/conf/httpd.conf
	cd $(APACHESRC)/target/bin ; echo "Failing to load because of undefined references here is not good." ; ./apachectl start
	cd $(APACHESRC)/test-files ; sleep 3s ; rm -f index.html ; wget http://localhost:9999/index.html ; 
	cd $(APACHESRC)/target/bin ; ./apachectl stop
	cd $(APACHESRC)/test-files ; diff index.html index.html.default 
	echo "a-m-c/$*: SUCCESS" 

apache-modules-ccured: a-m-c/asis a-m-c/expires a-m-c/gzip a-m-c/headers a-m-c/info a-m-c/layout a-m-c/random a-m-c/urlcount a-m-c/usertrack

apache-gcc: mustbegcc mustbelinux bindclean
	cd $(APACHESRC) ; ./configure --disable-module=asis --prefix=`pwd`/target ; make -e CC=gcc | tee bind-gcc.log



apache-cil: apacheclean apache-cil-test


################## Apache test cases
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

APATCH := --includedir=$(APACHEBASE)/include

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


#
# OLDEN benchmarks
#
all-olden: mustbegcc bh bisort em3d health mst perimeter power treeadd tsp
	@echo "All the Olden benchmarks work"

# Barnes-Hut
BHDIR := $(TESTDIR)/olden/bh
bh:  mustbegcc
	cd $(BHDIR); rm -f code.exe *.o; \
               make CC="$(COMBINECCURED) --leavealone=trusted_bh "
	(export ONE_VARIABLE=1 ; make runbh $(MAKEOVERRIDES) )

bh-combined:  mustbegcc
	cd $(BHDIR); \
	    $(CCURED) --leavealone=trusted_bh \
                      code.exe_comb.c trusted_bh.c $(EXEOUT)code.exe
	(export ONE_VARIABLE=1 ; make runbh $(MAKEOVERRIDES) )

bh-optimvariant: mustbegcc
	cd $(BHDIR); rm -f code.exe
	cd $(BHDIR); \
           $(CC_OPTIMVARIANT) \
                 code.exe_comboptimcured.$(OPTIMVARIANT).c \
                 trusted_bh.c -lm \
                 $(CCUREDLIB) \
                 $(EXEOUT)code.exe
	(export ONE_VARIABLE=1 ; make runbh $(MAKEOVERRIDES) )

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
                    CC="$(COMBINECCURED) "
	cd $(PWDIR); sh ./testit ./power.exe
	cd $(PWDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                    do time ./power.exe ; done"

power-optimvariant: mustbegcc
	cd $(PWDIR); rm -f power.exe
	cd $(PWDIR); \
           $(CC_OPTIMVARIANT) $(PWEXTRA) \
                 power.exe_comboptimcured.$(OPTIMVARIANT).c \
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
                    CC="$(COMBINECCURED) "
	cd $(HEALTHDIR); sh ./testit ./health.exe
	cd $(HEALTHDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                    do time ./health.exe 5 500 1 1; done"

health-optimvariant: mustbegcc
	cd $(HEALTHDIR); rm -f health.exe
	cd $(HEALTHDIR); \
           $(CC_OPTIMVARIANT) -lm \
                 health.exe_comboptimcured.$(OPTIMVARIANT).c \
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
                    CC="$(COMBINECCURED) $(CFLAGS) "
	cd $(PERIMDIR); sh ./testit ./perimeter.exe
	cd $(PERIMDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./perimeter.exe ; done"

perimeter-optimvariant: mustbegcc
	cd $(PERIMDIR); rm -f perimeter.exe
	cd $(PERIMDIR); \
           $(CC_OPTIMVARIANT) \
                 perimeter.exe_comboptimcured.$(OPTIMVARIANT).c \
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
                        --leavealone=trusted_voronoi "
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
                    CC="$(COMBINECCURED) "
	cd $(TSPDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./tsp.exe ; done"

tsp-optimvariant: mustbegcc
	cd $(TSPDIR); rm -f tsp.exe
	cd $(TSPDIR); \
           $(CC_OPTIMVARIANT) $(TSPEXTRA) \
                 tsp.exe_comboptimcured.$(OPTIMVARIANT).c \
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
                    CC="$(COMBINECCURED) "
	cd $(BISORTDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./bisort.exe 100000; done"

bisort-optimvariant: mustbegcc
	cd $(BISORTDIR); rm -f bisort.exe
	cd $(BISORTDIR); \
           $(CC_OPTIMVARIANT) -lm \
                 bisort.exe_comboptimcured.$(OPTIMVARIANT).c \
                 $(CCUREDLIB) \
                 $(EXEOUT)bisort.exe
	cd $(BISORTDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./bisort.exe 100000; done"



OLDENMSTDIR := $(TESTDIR)/olden/mst
OLDENMSTSAFECC := $(COMBINECCURED)
ifdef _MSVC
  OLDENMSTSAFECC += $(DEF)WIN32 $(DEF)MSDOS
  MSTARGS := _MSVC=1
endif
mst-clean: 	
	cd $(OLDENMSTDIR); make clean
	cd $(OLDENMSTDIR); rm -f *.cil.c *box.c *.i *_ppp.c *.origi *_all.c

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


mst-optimvariant: mustbegcc
	cd $(OLDENMSTDIR); rm -f mst.exe
	cd $(OLDENMSTDIR); \
           $(CC_OPTIMVARIANT) \
                 mst.exe_comboptimcured.$(OPTIMVARIANT).c \
                 $(CCUREDLIB) \
                 $(EXEOUT)mst.exe
	cd $(OLDENMSTDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./mst.exe 2048 1; done"




TREEADDIR := $(TESTDIR)/olden/treeadd
TREEADDSAFECC := $(CCURED) $(NOPRINTLN)
ifeq ($(ARCHOS), x86_WIN32)
  TREEADDSAFECC += $(DEF)WIN32 $(DEF)MSDOS
endif
treeadd-clean: 	
	cd $(TREEADDIR); make clean
	cd $(TREEADDIR); rm -f *.cil.c *box.c *.i *_ppp.c *.origi *_all.c

treeadd:  mustbegcc
	cd $(TREEADDIR); \
            make clean treeadd.exe CC="$(TREEADDSAFECC)" \
                                   LD="$(TREEADDSAFECC)"
	cd $(TREEADDIR); sh ./testit ./treeadd.exe
	cd $(TREEADDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./treeadd.exe 21 1; done"

treeadd-optimvariant: mustbegcc
	cd $(TRERADDIR); rm -f treeadd.exe
	cd $(TRERADDIR); \
           $(CC_OPTIMVARIANT) -lm \
                 treeadd.exe_comboptimcured.$(OPTIMVARIANT).c \
                 $(CCUREDLIB) \
                 $(EXEOUT)treeadd.exe
	cd $(TREEADDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                    do time ./treeadd.exe 21 1; done"


EM3DDIR := $(TESTDIR)/olden/em3d
EM3DSAFECC := $(CCURED)  
ifeq ($(ARCHOS), x86_WIN32)
  EM3DSAFECC += $(DEF)WIN32 $(DEF)MSDOS
  SS_RAND := TRUE
endif
em3d-clean: 	
	cd $(EM3DDIR); make clean
	cd $(EM3DDIR); rm -f *.cil.c *box.c *.i *_ppp.c *.origi *_all.c

# sm: em3d's output doesn't have much to check, so again I have no
# explicit self-test...
em3d:  mustbegcc
	cd $(EM3DDIR); \
            make clean em3d.exe CC="$(EM3DSAFECC)" \
                                LD="$(EM3DSAFECC)"
	cd $(EM3DDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./em3d.exe 2000 100 50; done"

em3d-optimvariant: mustbegcc
	cd $(EM3DDIR); rm -f em3d.exe
	cd $(EM3DDIR); \
           $(CC_OPTIMVARIANT) -lm \
                 em3d.exe_comboptimcured.$(OPTIMVARIANT).c \
                 $(CCUREDLIB) \
                 $(EXEOUT)em3d.exe
	cd $(EM3DDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                    do time ./em3d.exe 2000 100 50; done"



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
               make CFLAGS= CC="$(COMBINECCURED) " clean build
	cd $(COMPRESSDIR)/src; sh -c "for i in $(ITERATION_ELEMS) ; \
              do time ./compress.exe <input.data >combine-compress.out ;done"
	cd $(COMPRESSDIR)/src; if [ -e combine-compress.out ]; then diff combine-compress.out output.data >/dev/null; fi

compress-optimvariant: mustbegcc
	cd $(COMPRESSDIR)/src; rm -f compress.exe
	cd $(COMPRESSDIR)/src; \
           $(CC_OPTIMVARIANT) \
                 compress.exe_comboptimcured.$(OPTIMVARIANT).c \
                 $(CCUREDLIB) \
                 $(EXEOUT)compress.exe
	cd $(COMPRESSDIR)/src; sh -c "for i in $(ITERATION_ELEMS) ; \
              do time ./compress.exe <input.data >combine-compress.out ;done"


LIDIR := $(SPECDIR)/130.li
LISAFECC := $(CCURED)  
li:  mustbegcc
	cd $(LIDIR)/src; \
            make clean build CC="$(LISAFECC) $(CONLY)" \
                             LD="$(LISAFECC)"
	cd $(LIDIR)/src; sh ./testit ./li.exe
	cd $(LIDIR)/src; sh -c "for i in $(ITERATION_ELEMS) ; \
              do time ./li.exe \
                      <../data/train/input/train.lsp \
                      >../data/train/input/train.out ; done"

li-optimvariant: mustbegcc
	cd $(LIDIR)/src; rm -f li.exe
	cd $(LIDIR)/src; \
           $(CC_OPTIMVARIANT) -lm \
                li.exe_comboptimcured.$(OPTIMVARIANT).c \
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
	cd $(LIDIR)/src; rm -f *.cil.c *box.c *.i *_ppp.c *.origi trial_li_all.c

liinfer: li
	cd $(LIDIR)/src ; $(CCURED) --keep=. $(DEF)$(ARCHOS) $(DEF)$(PCCTYPE) \
                 $(CFLAGS) \
                 trial_li.c \
                 $(EXEOUT)li.exe


### SPEC95 GO
GODIR := $(SPECDIR)/099.go
GOSAFECC := $(CCURED)  --commPrintLn $(CFLAGS) $(OPT_O2)

goclean: 	
	cd $(GODIR)/src; make clean
	cd $(GODIR)/src; rm -f *.cil.c *box.c *.i *_ppp.c *.origi


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

go-optimvariant: mustbegcc
	cd $(GODIR)/src; rm -f go.exe
	cd $(GODIR)/src; \
           $(CC_OPTIMVARIANT) \
                go.exe_comboptimcured.$(OPTIMVARIANT).c \
                $(CCUREDLIB) \
                $(EXEOUT)go.exe
	cd $(GODIR)/src; sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./go.exe 50 9; done"


# sm: commented-out this section as per 21-aug-02 email discussion
# with George; it's a near duplicate of the section below
# ### SPEC95 GCC
# GCCDIR := $(SPECDIR)/126.gcc
# GCCSAFECC := $(CCURED)  --commPrintLn $(CFLAGS) $(OPT_O2)

# gccclean: 	
# 	cd $(GCCDIR)/src; make clean
# 	cd $(GCCDIR)/src; rm -f *.cil.c *box.c *.i *_ppp.c *.origi


# gcc:  mustbegcc
# 	cd $(GCCDIR)/src; \
#             make clean build CC="$(GCCSAFECC) $(CONLY)" \
#                              LD="$(GCCSAFECC)"
# 	cd $(GCCDIR)/src; sh ./testit ./cc1.exe
# 	cd $(GCCDIR)/src; sh -c "for i in $(ITERATION_ELEMS) ; \
#                                    do time ./cc1.exe ; done"

# gcc-combined:  mustbegcc
# 	cd $(GCCDIR)/src; \
# 	   $(CCURED) $(CONLY) gcc_all.c


# gcc-noclean:  mustbegcc
# 	cd $(GCCDIR)/src; \
#             make build CC="$(GCCSAFECC) $(CONLY)" \
#                        LD="$(GCCSAFECC)" \
#                              EXTRA_LIBS=$(GCCEXTRA) 
# 	sh -c "time $(GCCDIR)/src/cc1.exe "

# gcc-optimvariant: mustbegcc
# 	cd $(GCCDIR)/src; rm -f cc1.exe
# 	cd $(GCCDIR)/src; \
#            $(CC_OPTIMVARIANT) \
#                 cc1.exe_comboptimcured.$(OPTIMVARIANT).c \
#                 $(CCUREDLIB) \
#                 $(EXEOUT)cc1.exe
# 	cd $(GCCDIR)/src; sh -c "for i in $(ITERATION_ELEMS) ; \
#                                    do time ./cc1.exe ; done"



### SPEC95 vortex
VORDIR := $(SPECDIR)/147.vortex
VORSAFECC := $(CCURED)  --verbose --noRTTI 
#VORSAFECC := $(CCURED)  
ifdef _GNUCC
  VOREXTRA := -lm
endif

vortexclean: 	
	cd $(VORDIR)/src; make clean
	cd $(VODIR)/src; rm -f *.cil.c *box.c *.i *_ppp.c *.origi

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
	$(TVCOMMAND) -L $(VORDIR)/tv.log $(VORDIR)/src/vortex_all.i.rtl $(VORDIR)/src/vortex_all.cil.c.rtl 

### SPEC95 m88ksim
M88DIR := $(SPECDIR)/124.m88ksim

# sm: 12/23/01: found that -I. and -I- don't work with m88k.. as usual I
# have absolutely no idea why it hasn't been a problem until now ..
M88SAFECC := $(CCURED)   \
               --leavealone=m88k_trusted --no-idashi --no-idashdot
m88kclean: 	
	cd $(M88DIR)/src; make clean
	cd $(M88DIR)/src; rm -f *.cil.c *box.c *.i *_ppp.c *.origi

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
IJPEGSAFECC := $(CCURED)  
ifeq ($(ARCHOS), x86_WIN32)
  IJPEGSAFECC += -DWIN32 -DMSDOS
endif
ijpegclean: 	
	cd $(IJPEGDIR)/src; make clean
	cd $(IJPEGDIR)/src; rm -f *.cil.c *box.c *.i *_ppp.c *.origi

ijpeg:  mustbegcc
	cd $(IJPEGDIR)/src; rm -f ijpeg.exe
	cd $(IJPEGDIR)/src; \
            make clean build CC="$(IJPEGSAFECC) $(CONLY)" \
                             LD="$(IJPEGSAFECC)"
	cd $(IJPEGDIR)/src; sh ./testit ijpeg.exe
	cd $(IJPEGDIR)/src; \
              sh -c "for i in $(ITERATION_ELEMS) ; \
                     do time ./ijpeg.exe \
                          -image_file ../data/ref/input/vigo.ppm \
                          -GO; done"

ijpeg-optimvariant: mustbegcc
	cd $(IJPEGDIR)/src; rm -f ijpeg.exe
	cd $(IJPEGDIR)/src; \
           $(CC_OPTIMVARIANT) \
                ijpeg.exe_comboptimcured.$(OPTIMVARIANT).c \
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
GCCSAFECC := $(CCURED)  


gccclean: 	
	cd $(GCCDIR)/src; make clean
	cd $(GCCDIR)/src; rm -f *.cil.c *box.c *.i *_ppp.c *.origi

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
LINUXDIR := /home/weimer/linux-2.4.19

linuxstandard: 
	$(MAKE) -C $(LINUXDIR) clean vmlinux \
              MY-CC="gcc"

LINUXCC := $(CCUREDHOME)/bin/ccured --mode=GNUCC
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

linux-ccured: mustbegcc
	$(MAKE) -C $(LINUXDIR) vmlinux \
              CC="$(LINUXCC) --nomerge "


linux-clean:
	$(MAKE) -C $(LINUXDIR) clean

combinetest: 
	cd $(TESTDIR)/small1; $(CCURED)  /Fet.exe t.c t1.c

constrainttest:
	$(CAMLC) -o obj/constraint.exe src/constraint.ml
	obj/constraint.exe

### ftpd-BSD-0.3.2-5
FTPDDIR := $(TESTDIR)/ftpd/ftpd
FTPDSAFECC := $(CCURED)  
ifeq ($(ARCHOS), x86_WIN32)
  FTPDSAFECC += $(DEF)WIN32 $(DEF)MSDOS
endif
ftpd-clean: 	
	cd $(FTPDDIR); make clean
	cd $(FTPDDIR); rm -f *.cil.c *box.c *.i *_ppp.c *.origi *_all.c

ftpd: ftpd-clean mustbegcc
	cd $(FTPDDIR); \
            make CC="$(FTPDSAFECC)" \
                 LD="$(FTPDSAFECC)"

### wu-ftpd-2.6.1
# sm: commenting this out because there's a section later on
# which talks about 2.6.2
# WUFTPDDIR := $(TESTDIR)/wu-ftpd-2.6.1
# WUFTPDSAFECC := $(CCURED)  
# ifeq ($(ARCHOS), x86_WIN32)
#   WUFTPDSAFECC += $(DEF)WIN32 $(DEF)MSDOS
# endif
# wuftpd-clean: 	
# 	cd $(WUFTPDDIR); make clean; ./configure
# 	cd $(WUFTPDDIR); rm -f *.cil.c *box.c *.i *_ppp.c *.origi *_all.c

# wuftpd: mustbegcc
# 	cd $(WUFTPDDIR); \
#             make CC="$(WUFTPDSAFECC)" \
#                  LD="$(WUFTPDSAFECC)"

######################### PTRDIST Benchmarks
ANAGRAMDIR := $(TESTDIR)/ptrdist-1.1/anagram
anagram: mustbegcc
	cd $(ANAGRAMDIR); rm -f *.o; \
             make CC="$(CCURED)  "
	cd $(ANAGRAMDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                      do make test; done"

anagram-optimvariant: mustbegcc
	cd $(ANAGRAMDIR); rm -f anagram.exe
	cd $(ANAGRAMDIR); \
           $(CC_OPTIMVARIANT) \
                 anagram.exe_comboptimcured.$(OPTIMVARIANT).c \
                 $(CCUREDLIB) \
                 $(EXEOUT)anagram.exe
	cd $(ANAGRAMDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                      do make test; done"




BCDIR := $(TESTDIR)/ptrdist-1.1/bc
bc: mustbegcc
	cd $(BCDIR); rm -f *.o; \
            make CC="$(CCURED)  "
	cd $(BCDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                      do make test; done"

bc-optimvariant: mustbegcc
	cd $(BCDIR); rm -f bc.exe
	cd $(BCDIR); \
           $(CC_OPTIMVARIANT) \
                 bc.exe_comboptimcured.$(OPTIMVARIANT).c \
                 $(CCUREDLIB) \
                 $(EXEOUT)bc.exe
	cd $(BCDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                      do make test; done"

FTDIR := $(TESTDIR)/ptrdist-1.1/ft
ft: mustbegcc
	cd $(FTDIR); rm -f *.o; \
           make CC="$(CCURED)  "
	cd $(FTDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                      do make test; done"

ft-optimvariant: mustbegcc
	cd $(FTDIR); rm -f ft.exe
	cd $(FTDIR); \
           $(CC_OPTIMVARIANT) \
                 ft.exe_comboptimcured.$(OPTIMVARIANT).c \
                 $(CCUREDLIB) \
                 $(EXEOUT)ft.exe
	cd $(FTDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                      do make test; done"


KSDIR := $(TESTDIR)/ptrdist-1.1/ks
ks: mustbegcc
	cd $(KSDIR); rm -f *.o; \
           make CC="$(CCURED)  "
	cd $(KSDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                      do make test; done"

ks-optimvariant: mustbegcc
	cd $(KSDIR); rm -f ks.exe
	cd $(KSDIR); \
           $(CC_OPTIMVARIANT) \
                 ks.exe_comboptimcured.$(OPTIMVARIANT).c \
                 $(CCUREDLIB) \
                 $(EXEOUT)ks.exe
	cd $(KSDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                      do make test; done"



YACRDIR := $(TESTDIR)/ptrdist-1.1/yacr2
yacr: mustbegcc
	cd $(YACRDIR); rm -f *.o; \
           make CC="$(CCURED)  "
	cd $(YACRDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                      do make test; done"

yacr-optimvariant: mustbegcc
	cd $(YACRDIR); rm -f yacr2.exe
	cd $(YACRDIR); \
           $(CC_OPTIMVARIANT) \
                 yacr2.exe_comboptimcured.$(OPTIMVARIANT).c \
                 $(CCUREDLIB) \
                 $(EXEOUT)yacr2.exe
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

LINUXPATCH :=  --includedir=$(LINUX_INCLUDES) \
	-D CCURED_FAIL_IS_VERBOSE=1 --noStackChecks

# CCured support library for linux modules. Holds wrappers and definitions
# for things like ccured_fail(). 
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
		-name '*.cil.c' -o \
		-name '*.exe' -o \
		-name '*.i' -o \
		-name '*.o' -o \
		-name '*.obj' -o \
		-name '*.cabs.c' -o \
		-name '*_comb*.c' \
	      	\) -exec rm -f {} \;

# sm: removed the -k option to make since I want it to stop
# when there's an error

linux: mustbegcc mustbelinux mustbemanju linuxclean
	cd $(LINUXSRC) ; make CC="$(CILLY) --nomerge " \
                              HOSTCC="$(CILLY) --nomerge " bzImage

linux-noclean:  mustbegcc mustbelinux mustbemanju
	cd $(LINUXSRC) ; make CC="$(CILLY) --nomerge " \
                              HOSTCC="$(CILLY) --nomerge "

linux-gcc: mustbelinux mustbemanju linuxclean
	cd $(LINUXSRC); make CC=gcc HOSTCC=gcc

# ww: merge all of the key files in the linux kernel
# "linux-merge" tries to merge the *.cil.c versions of the files 
# "linux-merge2" tries to merge the *.i versions of the files
# in the end, both should work 
LINUX_MERGE_SRC = /home/weimer/linux
linux-merge: mustbegcc mustbelinux mustbemanju 
	cd $(LINUX_MERGE_SRC)/merger ; \
        $(CCURED) --commPrintLn --verbose --nocure `cat merge-file-list`
linux-merge2: mustbegcc mustbelinux mustbemanju 
	cd $(LINUX_MERGE_SRC)/merger ; \
        $(CCURED) --verbose --nocure `cat merge-file-list2`

linux-merge3: mustbegcc mustbelinux mustbemanju 
	cd $(LINUX_MERGE_SRC) ; \
           make CC="$(CILLY) --merge --verbose  --commPrintLn " \
                HOSTCC="$(CILLY) --merge --verbose " \
                LD="$(CILLY) --merge --verbose --commPrintLn " \
                AR="$(CILLY) --mode=AR --merge --verbose "


linux-clean3: 
	cd $(LINUX_MERGE_SRC); make clean

###########################################################################
#                              Sendmail
###########################################################################

# this directory contains the makefile for compiling with CCured:
SENDMAILDIR=$(BIG_EXAMPLES)/sendmail-8.12.1/comb

# if the links haven't been created yet, create them
sendmaillinks:
	if test ! -L $(SENDMAILDIR)/main.c; \
	then cd $(SENDMAILDIR); ./configure; fi

sendmailclean:
	cd $(SENDMAILDIR); make clean


sendmail-inferbox: sendmaillinks sendmailclean
	cd $(SENDMAILDIR); make USE_CCURED=1 CCURED_DIR=$(CCUREDHOME)

sendmail-cil: sendmaillinks sendmailclean
	cd $(SENDMAILDIR); make USE_CIL=1 CCURED_DIR=$(CCUREDHOME)

sendmail: mustbegcc 
ifeq ($(INFERBOX),none)
	$(MAKE) sendmail-cil
else
 ifeq ($(INFERBOX),infer)
	$(MAKE) sendmail-inferbox
 else
	@echo "Sendmail supports INFERBOX=none or INFERBOX=infer."
 endif
endif



###########################################################################
#                                OpenSSL
###########################################################################
# 
OPENSSLSRC := $(BIG_EXAMPLES)/openssl

#don't use assembly or dynamic linking
OPENSSL_CONFIG=./config no-asm no-dso

CC_SSL="$(CCURED) $(STANDARDPATCH) --stages --verbose --merge --mode=GNUCC \
-DCCURED_FIX -DUSE_POLYMORPHIC_QSORT"

AR_SSL="$(CCURED) $(STANDARDPATCH) --stages --verbose --merge --mode=AR rv"

openssl-clean:
	cd $(OPENSSLSRC); if test -f Makefile ; then $(MAKE) clean ; else $(OPENSSL_CONFIG); fi
	-cd $(OPENSSLSRC); find . \( \
		-name '*.cil.c' -o \
		-name '*.exe' -o \
		-name '*.i' -o \
		-name '*.o' -o \
		-name '*.obj' -o \
		-name '*.cabs.c' -o \
		-name '*_comb*.c' \
	      	\) -exec rm -f {} \;

#make openssl-clean before making this the first time to run ./config
openssl: mustbegcc
	cd $(OPENSSLSRC) ; make -e \
              CC=$(CC_SSL) \
              MAKE="make -e" \
              LD=$(CC_SSL) \
              AR=$(AR_SSL) \
              RANLIB="echo" 2>&1 | tee openssl.log

#also runs the tests
openssl-test: mustbegcc
	cd $(OPENSSLSRC) ; make -e \
              CC=$(CC_SSL) \
              MAKE="make -e" \
              LD=$(CC_SSL) \
              AR=$(AR_SSL) \
              RANLIB="echo" \
	      test       2>&1 | tee openssl-test.log

openssl-test-withclean: mustbegcc openssl-clean
	cd $(OPENSSLSRC) ; make -e \
              CC=$(CC_SSL) \
              MAKE="make -e" \
              LD=$(CC_SSL) \
              AR=$(AR_SSL) \
              RANLIB="echo" \
	      test       2>&1 | tee openssl-test.log

openssl-gcc: mustbegcc openssl-clean
	cd $(OPENSSLSRC) ; make -e CC=gcc 2>&1 | tee openssl-gcc.log

openssl-gcc-test: mustbegcc openssl-clean
	cd $(OPENSSLSRC) ; make -e CC=gcc test 2>&1 | tee openssl-gcc-test.log

.PHONY: openssl-clean openssl openssl-test openssl-test-withclean openssl-gcc openssl-gcc-test

#### ------------- EMACS ---------------------------

# source available at ftp.gnu.org/gnu/emacs

EMACSSRC := /usr/local/src/emacs-21.1

emacsclean:
	cd $(EMACSSRC); make clean
	-cd $(EMACSSRC); find . \( \
		-name '*.cil.c' -o \
		-name '*.exe' -o \
		-name '*.i' -o \
		-name '*.o' -o \
		-name '*.obj' -o \
		-name '*.cabs.c' -o \
		-name '*_comb*.c' \
	      	\) -exec rm -f {} \;

emacs: mustbegcc mustbelinux mustbemanju emacsclean
	cd $(EMACSSRC) ; make CC="$(CILLY) --nomerge -DCIL_HACKS"

emacs-noclean: mustbegcc mustbelinux mustbemanju
	cd $(EMACSSRC) ; make CC="$(CILLY) --nomerge -DCIL_HACKS"

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
		-name '*.cil.c' -o \
		-name '*.exe' -o \
		-name '*.i' -o \
		-name '*.o' -o \
		-name '*.obj' -o \
		-name '*.cabs.c' -o \
		-name '*_comb*.c' \
	      	\) -exec rm -f {} \;

perl: mustbegcc mustbelinux mustbemanju perlclean
	cd $(PERLSRC) ; make CC="$(CILLY) --nomerge"

perl-noclean: mustbegcc mustbelinux mustbemanju
	cd $(PERLSRC) ; make CC="$(CILLY) --nomerge"

perl-gcc: mustbelinux mustbemanju perlclean
	cd $(PERLSRC) ; make CC=gcc


###########################################################################
#                                  BIND
###########################################################################
# http://www.isc.org/products/BIND/
BINDSRC := $(BIG_EXAMPLES)/bind

bindclean:
	cd $(BINDSRC); if test -f Makefile ; then $(MAKE) clean ; else ./configure ; fi
	-cd $(BINDSRC); find . \( \
		-name '*.cil.c' -o \
		-name '*.exe' -o \
		-name '*.i' -o \
		-name '*.o' -o \
		-name '*.obj' -o \
		-name '*.cabs.c' -o \
		-name '*_comb*.c' \
	      	\) -exec rm -f {} \;

#              CC="$(CCURED) --assumePrintf $(STANDARDPATCH)" 
# lib/isc/include/isc/event.h has commented out __RTTI annotations
bind: mustbegcc mustbelinux bindclean
	cd $(BINDSRC) ; make -e \
              CC="$(CCURED) --noWILD --assumePrintf " \
              MAKE="make -e" \
                LD="$(CCURED) --merge --verbose --commPrintLn " \
                AR="$(CCURED) --mode=AR --merge --verbose " \
                RANLIB="echo" | tee bind.log

bind-noclean: mustbegcc mustbelinux 
	cd $(BINDSRC) ; make -e \
              CC="$(CCURED) --noWILD --assumePrintf " \
              MAKE="make -e" \
                LD="$(CCURED) --merge --verbose --commPrintLn " \
                AR="$(CCURED) --mode=AR --merge --verbose " \
                RANLIB="echo" | tee bind-noclean.log

bind-gcc: mustbegcc mustbelinux bindclean
	cd $(BINDSRC) ; make -e CC=gcc | tee bind-gcc.log

#bindclean: mustbegcc mustbelinux mustbemanju
#	 cd $(BIG_EXAMPLES); $(MAKE) bindclean
#
#bind: mustbegcc mustbelinux mustbemanju 
#	 cd $(BIG_EXAMPLES) ; $(MAKE) bind
#
#bind-noclean: mustbegcc mustbelinux mustbemanju
#	 cd $(BIG_EXAMPLES) ; $(MAKE) bind-noclean
#
#bind-gcc: mustbegcc mustbelinux mustbemanju 
#	 cd $(BIG_EXAMPLES)/bind ; $(MAKE)
#
#### ------------- WU-FTPD -------------------------

# source available at www.wu-ftpd.org


## compiled binaries can be found under /bin ##

WUFTPDSRC := /usr/local/src/wu-ftpd-2.6.2

wuftpdclean:
	cd $(WUFTPDSRC); ./build clean
	-cd $(WUFTPDSRC); find . \( \
		-name '*.cil.c' -o \
		-name '*.exe' -o \
		-name '*.i' -o \
		-name '*.o' -o \
		-name '*.obj' -o \
		-name '*.cabs.c' -o \
		-name '*_comb*.c' \
	      	\) -exec rm -f {} \;

wuftpd: mustbegcc mustbelinux mustbemanju wuftpdclean
	cd $(WUFTPDSRC) ; ./build CC="$(CCUREDHOME)/bin/cilly --nomerge" lnx

wuftpd-noclean: mustbegcc mustbelinux mustbemanju
	cd $(WUFTPDSRC) ; ./build CC="$(CCUREDHOME)/bin/cilly --nomerge" lnx

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
		-name '*.cil.c' -o \
		-name '*.exe' -o \
		-name '*.i' -o \
		-name '*.o' -o \
		-name '*.obj' -o \
		-name '*.cabs.c' -o \
		-name '*_comb*.c' \
	      	\) -exec rm -f {} \;

openssh: mustbegcc mustbelinux mustbemanju opensshclean
	cd $(OPENSSHSRC) ; make CC="$(CCURED)"

openssh-noclean: mustbegcc mustbelinux mustbemanju
	cd $(OPENSSHSRC) ; make CC="$(CCURED)"

openssh-gcc: mustbelinux mustbemanju opensshclean
	cd $(OPENSSHSRC) ; make CC=gcc


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

# GIMPNOCLEAN must be set to avoid cleaning before building (for testing)
ifdef GIMPNOCLEAN
  GIMP_DO_CLEAN := make clean
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
ifndef GIMPNOCLEAN
	cd $(ZLIBSRC); make clean
	-cd $(ZLIBSRC); find . \( \
		-name '*.cil.c' -o \
		-name '*.exe' -o \
		-name '*.i' -o \
		-name '*.o' -o \
		-name '*.obj' -o \
		-name '*.cabs.c' -o \
		-name '*_comb*.c' \
	      	\) -exec rm -f {} \;
endif

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
ifndef GIMPNOCLEAN
	cd $(TIFFSRC);  make clean
	-cd $(TIFFSRC); find . \( \
		-name '*.cil.c' -o \
		-name '*.exe' -o \
		-name '*.i' -o \
		-name '*.o' -o \
		-name '*.obj' -o \
		-name '*.cabs.c' -o \
		-name '*_comb*.c' \
	      	\) -exec rm -f {} \;
endif

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
ifndef GIMPNOCLEAN
	cd $(JPEGSRC); make clean
	-cd $(JPEGSRC); find . \( \
		-name '*.cil.c' -o \
		-name '*.exe' -o \
		-name '*.i' -o \
		-name '*.o' -o \
		-name '*.obj' -o \
		-name '*.cabs.c' -o \
		-name '*_comb*.c' \
	      	\) -exec rm -f {} \;
endif

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
ifndef GIMPNOCLEAN
	cd $(LIBPNGSRC); make clean
	-cd $(LIBPNGSRC); find . \( \
		-name '*.cil.c' -o \
		-name '*.exe' -o \
		-name '*.i' -o \
		-name '*.o' -o \
		-name '*.obj' -o \
		-name '*.cabs.c' -o \
		-name '*_comb*.c' \
	      	\) -exec rm -f {} \;
endif

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
ifndef GIMPNOCLEAN
	cd $(MPEGSRC); make clean
	-cd $(MPEGSRC); find . \( \
		-name '*.cil.c' -o \
		-name '*.exe' -o \
		-name '*.i' -o \
		-name '*.o' -o \
		-name '*.obj' -o \
		-name '*.cabs.c' -o \
		-name '*_comb*.c' \
	      	\) -exec rm -f {} \;
endif

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
ifndef GIMPNOCLEAN
	cd $(GLIBSRC); make clean
	-cd $(GLIBSRC); find . \( \
		-name '*.cil.c' -o \
		-name '*.exe' -o \
		-name '*.i' -o \
		-name '*.o' -o \
		-name '*.obj' -o \
		-name '*.cabs.c' -o \
		-name '*_comb*.c' \
	      	\) -exec rm -f {} \;
endif

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
ifndef GIMPNOCLEAN
	cd $(GTKSRC); make clean
	-cd $(GTKSRC); find . \( \
		-name '*.cil.c' -o \
		-name '*.exe' -o \
		-name '*.i' -o \
		-name '*.o' -o \
		-name '*.obj' -o \
		-name '*.cabs.c' -o \
		-name '*_comb*.c' \
	      	\) -exec rm -f {} \;
endif

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
ifndef GIMPNOCLEAN
	cd $(GIMPSRC); make clean
	-cd $(GIMPSRC); find . \( \
		-name '*.cil.c' -o \
		-name '*.exe' -o \
		-name '*.i' -o \
		-name '*.o' -o \
		-name '*.obj' -o \
		-name '*.cabs.c' -o \
		-name '*_comb*.c' \
	      	\) -exec rm -f {} \;
endif

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
	cd $(CTORTDIR)/compile; $(CILLY) --nomerge $*.c -c -o $*.o

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
.PHONY: ping
ping:
	cd $(PINGDIR); rm -f ping.o; make CC="$(CCURED) "


big-arrays : mustbemanju
	cd /usr/src/big-examples/; $(CCURED) --verbose --stages --nomerge \
	       $(CONLY) $(CFLAGS) big-arrays.c
	echo SUCCESS

# sm: stuff I've put into the misc/ directory
misc-tests:
	cd $(CCUREDHOME)/test/misc; \
	  make clean all CILLY_EXE=$(CCUREDHOME)/obj/$(ARCHOS)/cilly.byte.exe

# sm: test ability to merge already-merged files
merge-twice:
	$(CCUREDHOME)/bin/merger.pl --ccured --out small2/merged-once.c \
	  --in small2/merge-twice-[12].c
	gcc -c -w small2/merged-once.c
	$(CCUREDHOME)/bin/merger.pl --ccured --out small2/merged-twice.c \
	  --in small2/merged-once.c small2/merge-twice-3.c
	gcc -c -w small2/merged-twice.c

#merge-twice-1.c is just included here so that we use the merger.  The
# interesting code is in merge-ar.c
#3 steps: compile to *.o, merge to libmerge.a, and compile
merge-ar:
	cd small2; $(CCUREDHOME)/bin/cilly --merge -c merge-ar.c merge-twice-1.c
	cd small2; $(CCUREDHOME)/bin/cilly --merge --mode=AR cr libmerge.a merge-ar.o merge-twice-1.o
	cd small2; $(CCUREDHOME)/bin/cilly --merge libmerge.a -o merge-ar

blockattr: small1/blockattr.c
	cd small1; $(CCUREDHOME)/obj/$(ARCHOS)/ccured.byte.exe \
                             --onlyMerge --mergeKeepAnnotations \
                             --commPrintLn --verbose \
                             --mergedout - blockattr.c | grep  -v '/\\* __blockattribute' | grep '__blockattribute'

###########################################################################
#
#                Javascript - SpiderMonkey
#
###########################################################################

JSDIR=$(BIG_EXAMPLES)/js

js-orig: mustbegcc mustbelinux 
	cd $(JSDIR)/src ; make -f Makefile.ref

js-clean: 
	-cd $(JSDIR)/src; find . \( \
	        -name '*.OBJ' -o \
		-name '*.cil.c' -o \
		-name '*.cured.c' -o \
		-name '*.exe' -o \
		-name '*.i' -o \
		-name '*.o' -o \
		-name '*.obj' -o \
		-name '*.infer.c' -o \
		-name '*_all*.c' -o \
		-name '*_comb*.c' \
	\) -exec rm -rf {} \;

js: mustbegcc mustbelinux js-clean
	cd $(JSDIR)/src ; make -f Makefile.ref CCURED=1 \
                CC="$(CCURED) $(STANDARDPATCH) --verbose" \
	        LD="$(CCURED) $(STANDARDPATCH) --verbose --keepmerged" \
                AR="$(CILLY) --mode=AR --merge --verbose --keepmerged"
	cd $(JSDIR); ./src/Linux_All_DBG.OBJ/js -f test.js

TILEDIR:=$(TESTDIR)/tile
.PHONY: tile
tile: 	
	cd $(TILEDIR); rm -f tile.exe *.o; \
               make CC="$(COMBINECCURED) --allowInlineAssembly"
	cd $(TILEDIR); make run

#----------------------------------------------------------------------
# Generate a distribution of the CIL/CCured regression tests
# This is done AUTOMATICALLY as part of distributing CCured !

.PHONY: distrib distrib-nocheck checkdistrib
TESTS_TAR_GZ:=ccured-tests-@CIL_VERSION@.tar.gz

# Work in a temporary directory
TEMP_DIR = ../TEMP_ccured-distrib

# The tar archive members will be relative to this directory
TOP_DIR = $(TEMP_DIR)/ccured/test

DISTRIB_TEST = Makefile.in RegTest.pm testsafec testsafec.pl testcil.h

distrib: clean
        # Clean the test directory
        # Create the distribution from scratch
	-mkdir $(TEMP_DIR)
	-mkdir $(TEMP_DIR)/ccured
	rm -rf $(TOP_DIR)
	mkdir $(TOP_DIR) 

	rm -rf ../$(TESTS_TAR_GZ)

	cp $(patsubst %,%,$(DISTRIB_TEST)) $(TOP_DIR)
	cp -r tile small1 small2 olden spec95 \
              ptrdist-1.1 bad ftpd ping $(TOP_DIR)
# Delete all CVS directories
	if find $(TEMP_DIR) -name CVS -print >cvss.txt ; then \
            rm -rf `cat cvss.txt` ;fi
# Now make the TAR ball
	cd $(TEMP_DIR)/ccured; tar cfz ../../$(TESTS_TAR_GZ) test

#	rm -rf $(TEMP_DIR)

