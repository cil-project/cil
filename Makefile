# Makefile building and using the CCured compiler
# author: George Necula
# hacks here and there by Wes and Scott

# this Makefile makes use of several GNU Make extensions; see
#   http://www.gnu.org/manual/make/html_chapter/make_toc.html

# tweak to cause commit to think this is different

ifneq ($(ARCHOS), x86_WIN32)
ifneq ($(ARCHOS), x86_LINUX) 
   $(error You must set the ARCHOS variable to either x86_WIN32 or x86_LINUX)
endif
endif

# sm: moved this before setup and am now arranging things so this
# can be the primary target; must rethink what 'setup' means
quickbuild:
	make -r -f Makefile.ccured   quickbuild $(MAKEOVERRIDES)
	make -r -f Makefile.cil      quickbuild $(MAKEOVERRIDES)

setup:
	make -r -f Makefile.ccured   setup $(MAKEOVERRIDES)
	make -r -f Makefile.cil      setup $(MAKEOVERRIDES)
	make -r -f Makefile.merger   setup $(MAKEOVERRIDES)




# sm: find and remove all the intermediate files from translation
# sm: removed *box.c from those removed since test/PCC/src/pccbox.c should be kept
clean:
	-make -f Makefile.ccured clean CLEANING=1 $(MAKEOVERRIDES)
	rm -rf cil.tar.gz
	-find test \( \
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

# build ocamldoc documentation tree

.PHONY: doc cil-distrib quickbuild setup clean
doc:
	make -f Makefile.cil doc $(MAKEOVERRIDES)
	make -f Makefile.ccured doc $(MAKEOVERRIDES)

cil-distrib:
	make -f Makefile.cil cil-distrib $(MAKEOVERRIDES)

# sm: infer CCUREDHOME when not set, to ease having multiple trees
ifndef CCUREDHOME
 ifeq ($(ARCHOS), x86_WIN32)
  $(error You have not defined the CCUREDHOME variable)
 else
  export CCUREDHOME := $(shell pwd)
 endif
endif

CCURED := perl $(CCUREDHOME)/lib/ccured.pl 
PATCHER := perl $(CCUREDHOME)/lib/patcher.pl

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
  export EXTRAARGS += -O2
endif

# By default we are on Linux
ifndef ARCHOS
  ARCHOS := x86_WIN32
endif

# By default use the old patcher
ifndef NEWPATCH
  OLDPATCH := 1
endif

# By default use GCC, unless you set _MSVC on the command line on in .ccuredrc
ifndef _MSVC
  _GNUCC := 1
endif

# Now include the compiler specific stuff
ifdef _MSVC
  include Makefile.msvc
else
  ifdef _GNUCC
    include Makefile.gcc
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

# CCURED contains arguments that are passed to ccured.pl
# Pass such arguments in the command line as EXTRAARGS="..."
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
ifdef OPTIM
  CCURED+= --optimize
endif
ifdef RELEASELIB
  CCURED+= --releaselib -DRELEASELIB
endif
ifdef VERBOSE
  CCURED+= --verbose
endif
ifdef PRINTSTAGES
  CCURED+= --stages
endif
# sm: pass tracing directives on 'make' command line like TRACE=usedVars
ifdef TRACE
  CCURED+= --tr="$(TRACE)"
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
  # perhaps safecc.pl is the right place to deal with telling safec.exe
  # about this file

  # hack: possible append 'd' to the name, so we get different versions
  # for with and without CCURED; otherwise it would appear to be
  # up-to-date but for the wrong way
  ifdef PATCHDEFS
    PATCHFILE2=$(CCUREDHOME)/lib/$(PATCHFILE)2.id
  else
    PATCHFILE2=$(CCUREDHOME)/lib/$(PATCHFILE)2.i
  endif

  # tell safec.byte.exe (via safecc.pl) where to find the patch file
  CCURED+= --patchFile=$(PATCHFILE2)

  # and turn off the other patcher
  PATCHECHO := true
endif

# sm: use this instead of "sh ./testit" for those self-tests which can't
# be made to work on windows; it does nothing, and has no output
UNIXTESTIT := sh ./testit
ifeq ($(ARCHOS), x86_WIN32)
  UNIXTESTIT := @true
endif


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
PCCTEST := test/PCCout
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
                    $(STANDARDPATCH) --merge \
                    --keep=$(CCUREDHOME)/test/PCCout \
                    --leavealone=pccbox --leavealone=alloc
pcc : 
#	-rm $(PCCDIR)/$(ARCHOS)$(PCCCOMP)/$(PCCTYPE)/*.o
	-rm $(PCCDIR)/$(ARCHOS)$(PCCCOMP)/$(PCCTYPE)/*.exe
	-rm $(PCCDIR)/bin/*.exe
	make -C $(PCCDIR) \
             CC="$(PCCSAFECC) $(CONLY)" \
             LD="$(CCURED) $(MSLINK) --merge --keep=$(CCUREDHOME)/test/PCCout" \
             USE_JAVA=1 USE_JUMPTABLE=1 TYPE=$(PCCTYPE) \
             COMPILER=$(PCCCOMP) \
	     clean  

pcc-noclean : 
#	-rm $(PCCDIR)/$(ARCHOS)$(PCCCOMP)/$(PCCTYPE)/*.o
	-rm $(PCCDIR)/$(ARCHOS)$(PCCCOMP)/$(PCCTYPE)/*.exe
	-rm $(PCCDIR)/bin/*.exe
	make -C $(PCCDIR) \
             CC="$(PCCSAFECC) $(CONLY)" \
             LD="$(CCURED) $(MSLINK) --merge --keep=$(CCUREDHOME)/test/PCCout" \
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
SMALL1 := test/small1
test/% : $(SMALL1)/%.c 
	cd $(SMALL1); $(CCURED)   \
               $(STANDARDPATCH) \
	       $(CONLY) $(CFLAGS) $(ASMONLY)$*.s $*.c 

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



testmodel/%: $(SMALL1)/%.c $(SMALL1)/modelextern.c 
	cd $(SMALL1); \
            $(CC) $(CONLY) $(OBJOUT)modelextern.$(OBJEXT) modelextern.c
	cd $(SMALL1); $(CCURED) \
                         $(STANDARDPATCH) \
                         --leavealone=modelextern --merge \
                         $(CFLAGS) $(EXEOUT)$*.exe $*.c modelextern.$(OBJEXT)
	cd $(SMALL1); ./$*.exe

combine%: 
	cd $(SMALL1); \
          $(CCURED) $(CFLAGS) \
                    $(notdir $(wildcard $(SMALL1)/combine$*_[1-9].c)) \
                    --merge  \
                    $(STANDARDPATCH) \
	            $(EXEOUT)combine$*.exe
	cd $(SMALL1); ./combine$*.exe

arcombine: mustbegcc
	cd $(SMALL1); $(CCURED) --merge -c array1.c array2.c
	cd $(SMALL1); perl $(CCUREDHOME)/lib/cilly.pl --merge \
                           --mode=AR crv array.a array1.o array2.o
	cd $(SMALL1); $(CCURED) --merge -o matrix.exe array.a matrix.c
	cd $(SMALL1); ./matrix.exe

# weimer: test, compile and run
testc/% : $(SMALL1)/%.c  
	cd $(SMALL1); $(CCURED)   \
               $(STANDARDPATCH) \
	       $(CFLAGS) $(EXEOUT)$*.exe $*.c ; ./$*.exe

# Aman's optim tests
OPTIMTESTDIR := test/optim
optim/% : $(OPTIMTESTDIR)/%.c 
	cd $(OPTIMTESTDIR); $(CCURED)   \
               $(STANDARDPATCH) \
	       $(CFLAGS) $*.c $(EXEOUT)$*.exe
	$(OPTIMTESTDIR)/$*.exe



hashtest: test/small2/hashtest.c 
	rm -f $(PCCTEST)/hashtest.exe
	cd $(PCCTEST); $(CCURED) --merge \
                                 --keep=. $(DEF)$(ARCHOS) $(DEF)$(PCCTYPE) \
                 $(CFLAGS) \
                 `$(PATCHECHO) $(STANDARDPATCH)` \
                 $(INC)$(PCCDIR)/src \
                 $(PCCDIR)/src/hash.c \
                 ../small2/hashtest.c \
                 $(EXEOUT)hashtest.exe
	cd $(PCCTEST); sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./hashtest.exe $(HUFFINPUT) ; done"

hashtest-optimvariant.%: mustbegcc
	cd $(PCCTEST); \
           $(OPTIMVARIANT) \
                 hashtest.exe_combcured.$*.optim.c \
                 $(CCUREDHOME)/obj/ccured_$(COMPILERNAME)_releaselib.$(LIBEXT) \
                 $(EXEOUT)hashtest.exe
	cd $(PCCTEST); sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./hashtest.exe $(HUFFINPUT) ; done"


rbtest: test/small2/rbtest.c 
	rm -f $(PCCTEST)/rbtest.exe
	@true "compile with gcc for better error diagnostics (ha!)"
	cd $(PCCTEST); $(CCURED) --merge \
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
                 $(CCUREDHOME)/obj/ccured_$(COMPILERNAME)_releaselib.$(LIBEXT) \
                 $(EXEOUT)rbtest.exe
	cd $(PCCTEST); sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./rbtest.exe letGcFree ; done"

btreetest: test/small2/testbtree.c \
           test/small2/btree.c 
	rm -f test/small2/btreetest.exe
	cd test/small2; $(CCURED) --merge --keep=. \
                 $(OPT_O2) \
                 $(STANDARDPATCH) \
                 btree.c testbtree.c \
                 $(EXEOUT)btreetest.exe
	cd test/small2; sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./btreetest.exe ; done"


btreetest-optimvariant.%: mustbegcc
	cd test/small2; \
           $(OPTIMVARIANT) \
                 btreetest.exe_combcured.$*.optim.c \
                 $(CCUREDHOME)/obj/ccured_$(COMPILERNAME)_releaselib.$(LIBEXT) \
                 $(EXEOUT)btreetest.exe
	cd test/small2; sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./btreetest.exe ; done"

# sm: this is my little test program
hola: scott/hola

# sm: some project members don't want the testing targets to depend
# on quickbuild (which rebuilds translator components whose dependencies
# have changed), and others do..
ifdef TARGETS_DEP_QUICKBUILD
  TARGET_DEP := quickbuild
endif

# sm: attempt at a single rule for my testing purposes
scott/%: test/small2/%.c $(TARGET_DEP)
	rm -f test/small2/$*
	cd test/small2; $(CC) $(CONLY) $(WARNALL) $(DEF)$(ARCHOS) $*.c
	cd test/small2; $(CCURED) --keep=. $(DEF)$(ARCHOS) \
                 `$(PATCHECHO) $(STANDARDPATCH)` \
                 $(CFLAGS) `true $(WARNALL)` $(NOPRINTLN) \
                 $*.c \
                 $(EXEOUT)$*
	sh -c "time test/small2/$*"

scott-nolink/%: test/small2/%.c $(TARGET_DEP)
	rm -f test/small2/$*
	cd test/small2; $(CC) $(CONLY) $(WARNALL) $(DEF)$(ARCHOS) $*.c
	cd test/small2; $(CCURED) $(CONLY) --keep=. $(DEF)$(ARCHOS) \
                 `$(PATCHECHO) $(STANDARDPATCH)` \
                 $(CFLAGS) $(WARNALL) $(NOPRINTLN) \
                 $*.c \
                 $(EXEOUT)$*

# sm: a target for programs which are *supposed* to fail, because
# they intentionally violate the type system; but this is only
# when FAIL is #defined, otherwise they should exit ok
bad/%: test/bad/%.c $(TARGET_DEP)
	rm -f test/bad/$*
	cd test/bad; $(CC) $(CONLY) $(WARNALL) $(DEF)$(ARCHOS) $*.c
	@true "first try the succeed case"
	cd test/bad; $(CCURED) --keep=. $(DEF)$(ARCHOS) \
                 `$(PATCHECHO) $(STANDARDPATCH)` \
                 $(CFLAGS) $(WARNALL) $(NOPRINTLN) \
                 $*.c \
                 $(EXEOUT)$*
	if test/bad/$*; then \
		echo "(worked as expected, when FAIL not defined)"; exit 0; \
	else \
		echo "That should have worked; FAIL was not defined!"; exit 2; \
	fi
	@true "now try the failure case"
	cd test/bad; $(CCURED) --keep=. $(DEF)$(ARCHOS) \
                 `$(PATCHECHO) $(STANDARDPATCH)` \
                 $(CFLAGS) $(WARNALL) $(NOPRINTLN) -DFAIL \
                 $*.c \
                 $(EXEOUT)$*
	if test/bad/$*; then \
		echo "That should have failed!"; exit 2; \
	else \
		echo "(failed as expected)"; exit 0; \
	fi

# same rules, this time in 'scott' directory, since it's a pain to
# move the file just to add a failure case
bads/%: test/small2/%.c $(TARGET_DEP)
	rm -f test/small2/$*
	cd test/small2; $(CC) $(CONLY) $(WARNALL) $(DEF)$(ARCHOS) $*.c
	@true "first try the succeed case"
	cd test/small2; $(CCURED) --keep=. $(DEF)$(ARCHOS) \
                 `$(PATCHECHO) $(STANDARDPATCH)` \
                 $(CFLAGS) $(WARNALL) $(NOPRINTLN) \
                 $*.c \
                 $(EXEOUT)$*
	if test/small2/$*; then \
		echo "(worked as expected, when FAIL not defined)"; exit 0; \
	else \
		echo "That should have worked; FAIL was not defined!"; exit 2; \
	fi
	@true "now try the failure case"
	cd test/small2; $(CCURED) --keep=. $(DEF)$(ARCHOS) \
                 `$(PATCHECHO) $(STANDARDPATCH)` \
                 $(CFLAGS) $(WARNALL) $(NOPRINTLN) -DFAIL \
                 $*.c \
                 $(EXEOUT)$*
	if test/small2/$*; then \
		echo "That should have failed!"; exit 2; \
	else \
		echo "(failed as expected)"; exit 0; \
	fi

# sm: yet another failure-test target, this time utilizing a separate
# script capable of testing multiple failures per file
test-bad/%: quickbuild test/small2/%.c $(TARGET_DEP)
	CCUREDHOME="$(CCUREDHOME)" CCURED="$(CCURED) --noPrintLn" \
	  CFLAGS="$(DEF)$(ARCHOS) "`$(PATCHECHO) $(STANDARDPATCH)`" $(CFLAGS) $(WARNALL)" \
          TESTBADONCE="$(TESTBADONCE)" \
	  bash lib/test-bad $*.c

# sm: trivial test of combiner
MYSAFECC := $(CCURED) --keep=. $(DEF)$(ARCHOS) $(STANDARDPATCH)
comb: test/small2/comb1.c test/small2/comb2.c 
	rm -f test/small2/comb
	cd test/small2; \
	  $(MYSAFECC) --merge comb1.c $(CONLY) $(OBJOUT) comb1.o; \
	  $(MYSAFECC) --merge comb2.c $(CONLY) $(OBJOUT) comb2.o; \
          $(MYSAFECC) --merge comb1.o comb2.o $(EXEOUT)comb
	test/small2/comb

# sm: test of combiner's ability to report inconsistencies
baddef: test/small2/baddef1.c test/small2/baddef2.c 
	cd test/small2; $(CC) baddef1.c baddef2.c -o baddef && ./baddef
	rm -f test/small2/baddef
	cd test/small2; \
	  $(MYSAFECC) --merge baddef1.c $(CONLY) $(OBJOUT) baddef1.o; \
	  $(MYSAFECC) --merge baddef2.c $(CONLY) $(OBJOUT) baddef2.o; \
          $(MYSAFECC) --merge baddef1.o baddef2.o $(EXEOUT)baddef \
	  > baddef.rept 2>&1
	cat test/small2/baddef.rept
	test/small2/baddef
	if grep -i conflicting test/small2/baddef.rept >/dev/null; then \
	  echo "OK: conflict detected"; \
	else \
	  echo "FAIL: missed the conflict!"; exit 1; \
	fi

# cfrac: a memory benchmark which factorizes into products of primes
CFRACDIR := $(CCUREDHOME)/../bench/cfrac
cfrac: 
	-rm $(CFRACDIR)/*.o
	-rm $(CFRACDIR)/cfrac
	make -C $(CFRACDIR) \
	  CC="$(CCURED) --keep=$(CFRACDIR)" \
	  LD="$(CCURED) --keep=$(CFRACDIR)"
	csh -c "time $(CFRACDIR)/cfrac 327905606740421458831903"

comcfrac: 
	-rm $(CFRACDIR)/*.o
	-rm $(CFRACDIR)/cfrac
	make -C $(CFRACDIR) \
	  CC="$(CCURED) --merge --keep=$(CFRACDIR)" \
	  LD="$(CCURED) --merge --keep=$(CFRACDIR)"
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




HUFFCOMPILE := $(CCURED) $(DEF)NOVARARG --merge --keep=. 
# HUFFCOMPILE := cl /MLd
ifdef _GNUCC
  HUFFOTHERS += -lm
endif
ifndef HUFFINPUT
  HUFFINPUT=$(CCUREDHOME)/src/frontc/cparser.output
endif
hufftest: test/small2/hufftest.c 
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
                 $(CCUREDHOME)/obj/ccured_$(COMPILERNAME)_releaselib.$(LIBEXT) \
                 $(EXEOUT)hufftest.exe
	cd $(PCCTEST); sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./hufftest.exe $(HUFFINPUT) ; done"

wes-rbtest: test/small2/wes-rbtest.c 
	rm -f $(PCCTEST)/wes-rbtest.exe
	cd $(PCCTEST); $(CCURED) --keep=. $(DEF)$(ARCHOS) $(DEF)$(PCCTYPE) \
                 $(CFLAGS) \
                 $(STANDARDPATCH) \
                 $(INC)$(PCCDIR)/src \
                 ../small2/wes-rbtest.c \
                 $(EXEOUT)wes-rbtest.exe
	$(PCCTEST)/wes-rbtest.exe

wes-hashtest: test/small2/wes-hashtest.c 
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
	make -C CC="perl safecc.pl" $*


################## Linux device drivers
testlinux/% : test/linux/%.cpp 
	cd test/linux; $(CCURED) -o $*.o $*.cpp 

testqp : testlinux/qpmouse
testserial: testlinux/generic_serial

################## Rahul's test cases
SPR-TESTDIR := test/spr
spr/% : 
	cd $(SPR-TESTDIR); $(CCURED) $*.c $(CONLY) $(CFLAGS) $(ASMONLY)$*.s


################# Apache test cases
APACHETEST := test/apache
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
COMBINECCURED := $(CCURED) --merge

# sm: trying to collapse where are specifications are
PATCHARG := `$(PATCHECHO) $(STANDARDPATCH)`

#
# OLDEN benchmarks
#
all-olden: mustbegcc bh bisort em3d health mst perimeter power treeadd tsp
	@echo "All the Olden benchmarks work"

# Barnes-Hut
BHDIR := test/olden/bh
bh:  mustbegcc
	cd $(BHDIR); rm -f code.exe *.o; \
               make CC="$(COMBINECCURED) --leavealone=trusted_bh $(PATCHARG)"
	make runbh $(MAKEOVERRIDES)

bh-combined:  mustbegcc
	cd $(BHDIR); \
	    $(CCURED) code.exe_comb.c trusted_bh.c $(EXEOUT)code.exe
	make runbh $(MAKEOVERRIDES)

bh-optimvariant.%: mustbegcc
	cd $(BHDIR); \
           $(OPTIMVARIANT) \
                 code.exe_combcured.$*.optim.c \
                 trusted_bh.c -lm \
                 $(CCUREDHOME)/obj/ccured_$(COMPILERNAME)_releaselib.$(LIBEXT) \
                 $(EXEOUT)code.exe
	make runbh $(MAKEOVERRIDES)

# sm: rather than make it every time, I've added data.in (with 12 CRs)
# to the repository
runbh:
	cd $(BHDIR); sh ./testit ./code.exe
	cd $(BHDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                             do time ./code.exe < data.in >data.out ; done"


# Power pricing
PWDIR := test/olden/power
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
                 $(CCUREDHOME)/obj/ccured_$(COMPILERNAME)_releaselib.$(LIBEXT) \
                 $(EXEOUT)power.exe
	cd $(PWDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                    do time ./power.exe ; done"

power-combined :  mustbegcc
	cd $(PWDIR); \
             $(CCURED) power.exe_comb.c $(EXEOUT)power.exe


# Health care simulation
HEALTHDIR=test/olden/health
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
                 $(CCUREDHOME)/obj/ccured_$(COMPILERNAME)_releaselib.$(LIBEXT) \
                 $(EXEOUT)health.exe
	cd $(HEALTHDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                    do time ./health.exe 5 500 1 1; done"


# Perimeter of regions in images
PERIMDIR := test/olden/perimeter
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
                 $(CCUREDHOME)/obj/ccured_$(COMPILERNAME)_releaselib.$(LIBEXT) \
                 $(EXEOUT)perimeter.exe
	cd $(PERIMDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./perimeter.exe ; done"

# Voronoi diagrams
VORONDIR := test/olden/voronoi
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
TSPDIR := test/olden/tsp
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
                 $(CCUREDHOME)/obj/ccured_$(COMPILERNAME)_releaselib.$(LIBEXT) \
                 $(EXEOUT)tsp.exe
	cd $(TSPDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./tsp.exe ; done"



# Bitonic sort
# sm: to the best of my knowledge, bisort tests itself, so there is no
# explicit self-test
BISORTDIR := test/olden/bisort
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
                 $(CCUREDHOME)/obj/ccured_$(COMPILERNAME)_releaselib.$(LIBEXT) \
                 $(EXEOUT)bisort.exe
	cd $(BISORTDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./bisort.exe 100000; done"



OLDENMSTDIR := test/olden/mst
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
                 $(CCUREDHOME)/obj/ccured_$(COMPILERNAME)_releaselib.$(LIBEXT) \
                 $(EXEOUT)mst.exe
	cd $(OLDENMSTDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./mst.exe 2048 1; done"




TREEADDIR := test/olden/treeadd
TREEADDSAFECC := $(CCURED) --merge $(PATCHARG) $(NOPRINTLN)
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
                 $(CCUREDHOME)/obj/ccured_$(COMPILERNAME)_releaselib.$(LIBEXT) \
                 $(EXEOUT)treeadd.exe
	cd $(TREEADDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                    do time ./treeadd.exe 21 1; done"


EM3DDIR := test/olden/em3d
EM3DSAFECC := $(CCURED) --merge $(PATCHARG)
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
                 $(CCUREDHOME)/obj/ccured_$(COMPILERNAME)_releaselib.$(LIBEXT) \
                 $(EXEOUT)em3d.exe
	cd $(EM3DDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                    do time ./em3d.exe 2000 100 6; done"



# SPEC95
SPECDIR := test/spec95

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
                 $(CCUREDHOME)/obj/ccured_$(COMPILERNAME)_releaselib.$(LIBEXT) \
                 $(EXEOUT)compress.exe
	cd $(COMPRESSDIR)/src; sh -c "for i in $(ITERATION_ELEMS) ; \
              do time ./compress.exe <input.data >combine-compress.out ;done"


LIDIR := $(SPECDIR)/130.li
LISAFECC := $(CCURED) --merge $(PATCHARG)
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
                $(CCUREDHOME)/obj/ccured_$(COMPILERNAME)_releaselib.$(LIBEXT) \
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
GOSAFECC := $(CCURED) --merge  $(PATCHARG) $(NOPRINTLN) $(OPT_O2)

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
                $(CCUREDHOME)/obj/ccured_$(COMPILERNAME)_releaselib.$(LIBEXT) \
                $(EXEOUT)compress.exe
	cd $(GODIR)/src; sh -c "for i in $(ITERATION_ELEMS) ; \
                                   do time ./go.exe 50 9; done"



### SPEC95 vortex
VORDIR := $(SPECDIR)/147.vortex
VORSAFECC := $(CCURED) --merge   $(PATCHARG)
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
               $(CCUREDHOME)/cil/obj/cillibdebug.a $(VOREXTRA) $(EXEOUT)vortex.exe
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
  TVCOMMAND := $(TVDIR)/obj/transval.asm
else
  TVCOMMAND := $(TVDIR)/obj/transval.asm.exe
endif

vortex-tv:
	$(TVCOMMAND) -L $(VORDIR)/tv.log $(VORDIR)/src/vortex_all.i.rtl $(VORDIR)/src/vortex_allcil.c.rtl 

### SPEC95 m88ksim
M88DIR := $(SPECDIR)/124.m88ksim
M88SAFECC := $(CCURED) --merge $(PATCHARG) \
               --leavealone=m88k_trusted
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
IJPEGSAFECC := $(CCURED) --merge $(PATCHARG)
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
                $(CCUREDHOME)/obj/ccured_$(COMPILERNAME)_releaselib.$(LIBEXT) \
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
GCCSAFECC := $(CCURED) --merge $(PATCHARG)


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
SPEC00DIR=test/spec00
GZIPDIR=$(SPEC00DIR)/164.gzip
GZIPSOURCES   = bits.c deflate.c gzip.c getopt.c inflate.c lzw.c \
	        spec.c trees.c unlzh.c unlzw.c unpack.c unzip.c util.c zip.c
gzip-clean: 
	cd $(GZIPDIR)/src; rm -f *.$(OBJEXT) *.$(EXEEXT)

gzip-build: gzip-clean
	cd $(GZIPDIR)/src; $(CCURED) --merge $(GZIPSOURCES) $(EXEOUT)gzip.exe

gzip-run:
	cd $(GZIPDIR)/src; ./gzip.exe trees.c

gzip: gzip-clean gzip-build gzip-run

#
# Linux
LINUXDIR := /home/project/linux-2.2.9

linuxstandard: 
	$(MAKE) -C $(LINUXDIR) clean vmlinux \
              MY-CC="gcc"

LINUXCC := perl $(CCUREDHOME)/lib/safecc.pl --mode=gcc
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
	cd test/small1; $(CCURED) --merge /Fet.exe t.c t1.c

obj/prettytest.exe: src/pretty.mli src/pretty.ml src/prettytest.ml
	$(CAMLC) -I src -o obj/prettytest.exe src/pretty.mli src/pretty.ml src/prettytest.ml

prettytest:  obj/prettytest.exe
	time obj/prettytest.exe ; echo

constrainttest:
	$(CAMLC) -o obj/constraint.exe src/constraint.ml
	obj/constraint.exe

### ftpd-BSD-0.3.2-5
FTPDDIR := test/ftpd/ftpd
FTPDSAFECC := $(CCURED) --merge $(PATCHARG)
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
WUFTPDDIR := test/wu-ftpd-2.6.1
WUFTPDSAFECC := $(CCURED) --merge $(PATCHARG)
ifeq ($(ARCHOS), x86_WIN32)
  WUFTPDSAFECC += $(DEF)WIN32 $(DEF)MSDOS
endif
wuftpd-clean: 	
	cd $(WUFTPDDIR); make clean; ./configure
	cd $(WUFTPDDIR); rm -f *cil.c *box.c *.i *_ppp.c *.origi *_all.c

wuftpd: mustbegcc
	cd $(WUFTPDDIR); \
            make CC="$(WUFTPDSAFECC)" \
                 LD="$(WUFTPDSAFECC)"

######################################################################
# Rahul's test cases

spr/%:  test/spr/%.c 
	rm -f test/spr/$*
	cd test/spr; $(CC) $(CONLY) $(WARNALL) $(DEF)$(ARCHOS) $*.c
	cd test/spr; $(SAFECC) --keep=. $(DEF)$(ARCHOS) \
                 `$(PATCHECHO) $(STANDARDPATCH)` \
                 $(CFLAGS) `true $(WARNALL)` $(NOPRINTLN) \
                 $*.c \
                 $(EXEOUT)$*



######################### PTRDIST Benchmarks
ANAGRAMDIR := test/ptrdist-1.1/anagram
anagram: mustbegcc
	cd $(ANAGRAMDIR); rm -f *.o; \
             make CC="$(CCURED) $(STANDARDPATCH) --merge"
	cd $(ANAGRAMDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                      do make test; done"

anagram-optimvariant.%: mustbegcc
	cd $(ANAGRAMDIR); \
           $(OPTIMVARIANT) \
                 anagram_combcured.$*.optim.c \
                 $(CCUREDHOME)/obj/ccured_$(COMPILERNAME)_releaselib.$(LIBEXT) \
                 $(EXEOUT)anagram
	cd $(ANAGRAMDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                      do make test; done"




BCDIR := test/ptrdist-1.1/bc
bc: mustbegcc
	cd $(BCDIR); rm -f *.o; \
            make CC="$(CCURED) $(STANDARDPATCH) --merge"
	cd $(BCDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                      do make test; done"

bc-optimvariant.%: mustbegcc
	cd $(BCDIR); \
           $(OPTIMVARIANT) \
                 bc_combcured.$*.optim.c \
                 $(CCUREDHOME)/obj/ccured_$(COMPILERNAME)_releaselib.$(LIBEXT) \
                 $(EXEOUT)bc
	cd $(BCDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                      do make test; done"

FTDIR := test/ptrdist-1.1/ft
ft: mustbegcc
	cd $(FTDIR); rm -f *.o; \
           make CC="$(CCURED) $(STANDARDPATCH) --merge"
	cd $(FTDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                      do make test; done"

ft-optimvariant.%: mustbegcc
	cd $(FTDIR); \
           $(OPTIMVARIANT) \
                 ft_combcured.$*.optim.c \
                 $(CCUREDHOME)/obj/ccured_$(COMPILERNAME)_releaselib.$(LIBEXT) \
                 $(EXEOUT)ft
	cd $(FTDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                      do make test; done"


KSDIR := test/ptrdist-1.1/ks
ks: mustbegcc
	cd $(KSDIR); rm -f *.o; \
           make CC="$(CCURED) $(STANDARDPATCH) --merge"
	cd $(KSDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                      do make test; done"

ks-optimvariant.%: mustbegcc
	cd $(KSDIR); \
           $(OPTIMVARIANT) \
                 ks_combcured.$*.optim.c \
                 $(CCUREDHOME)/obj/ccured_$(COMPILERNAME)_releaselib.$(LIBEXT) \
                 $(EXEOUT)ks
	cd $(KSDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                      do make test; done"



YACRDIR := test/ptrdist-1.1/yacr2
yacr: mustbegcc
	cd $(YACRDIR); rm -f *.o; \
           make CC="$(CCURED) $(STANDARDPATCH) --merge"
	cd $(YACRDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                      do make test; done"

yacr-optimvariant.%: mustbegcc
	cd $(YACRDIR); \
           $(OPTIMVARIANT) \
                 yacr2_combcured.$*.optim.c \
                 $(CCUREDHOME)/obj/ccured_$(COMPILERNAME)_releaselib.$(LIBEXT) \
                 $(EXEOUT)yacr2
	cd $(YACRDIR); sh -c "for i in $(ITERATION_ELEMS) ; \
                                      do make test; done"
################# LINUX
LINUX_INCLUDES := $(CCUREDHOME)/test/linux/include
LINUX_TOPATCH := asm/uaccess.h asm/atomic.h asm/bitops.h \
	         asm/current.h asm/string.h asm/semaphore.h \
                 linux/config.h linux/list.h linux/skbuff.h \
		 linux/etherdevice.h linux/irq_cpustat.h \
		 linux/netdevice.h linux/ide.h linux/cdrom.h \
		 linux/blkdev.h linux/fs.h linux/reiserfs_fs.h \
                 linux/tqueue.h

linuxsetup: mustbelinux
	$(PATCHER)  -D MODULE -D __KERNEL__ -I /usr/src/linux/include \
                    --patch=test/linux/linux.patch \
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
SBULLDIR := test/linux/sbull
sbull: mustbegcc mustbelinux $(LINUXMODULELIB)
	cd $(SBULLDIR); ( make clean && make .depend && \
           make CC="$(CCURED) $(LINUXPATCH) --entryPoint='sbull_init'" ) ;
	cd $(LINUXMODULELIBDIR) ; make sbull_cured.o

PCNET32DIR := test/linux/pcnet32
pcnet32: mustbegcc mustbelinux $(LINUXMODULELIB)
	cd $(PCNET32DIR); ( make clean && \
           make CC="$(CCURED) $(LINUXPATCH) --entryPoint='pcnet32_init_module'" ) ;
	cd $(LINUXMODULELIBDIR) ; make pcnet32_cured.o

IDECDDIR := test/linux/ide-cd
ide-cd: mustbegcc mustbelinux $(LINUXMODULELIB)
	cd $(IDECDDIR); ( make clean && \
           make CC="$(CCURED) $(LINUXPATCH) --entryPoint='ide_cdrom_init'" ) ;
	cd $(LINUXMODULELIBDIR) ; make ide-cd_cured.o

REISERFSDIR := test/linux/reiserfs
reiserfs: mustbegcc mustbelinux $(LINUXMODULELIB)
	cd $(REISERFSDIR); ( make clean && \
           make CC="$(CCURED) --merge \
                       $(LINUXPATCH) --entryPoint='init_reiserfs_fs'" ) ;
	cd $(LINUXMODULELIBDIR) ; make reiserfs_cured.o

reiserfs-combined: mustbegcc mustbelinux \
                        $(LINUXMODULELIB) $(REISERFSDIR)/reiserfs.o_comb.c
	cd $(REISERFSDIR); \
           make fromcomb CC="$(CCURED) \
                            $(LINUXPATCH) --entryPoint='init_reiserfs_fs'"
	cd $(LINUXMODULELIBDIR) ; make reiserfs_cured.o


################# THE LINUX KERNEL
LINUXSRC := /usr/src/linux-cil
mustbemanju: 
	if ! test -d $(LINUXSRC) ; then  \
                echo You dont have the Linux sources; exit 3; fi

CILLY := perl $(CCUREDHOME)/lib/cilly.pl
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

linux: mustbegcc mustbelinux mustbemanju linuxclean
	cd $(LINUXSRC) ; make -k CC="$(CILLY) --keepunused" HOSTCC="$(CILLY)"

linux-noclean:  mustbegcc mustbelinux mustbemanju
	cd $(LINUXSRC) ; make -k CC="$(CILLY)" HOSTCC="$(CILLY)"

linux-gcc: mustbelinux mustbemanju linuxclean
	cd $(LINUXSRC); make -k CC=gcc HOSTCC=gcc

######################## SENDMAIL
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

#### GIMP AND FRIENDS
ZLIBSRC := /usr/local/src/zlib-1.1.3
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

zlib: mustbegcc mustbemanju mustbelinux zlibclean
	cd $(ZLIBSRC); make CC="$(CILLY)" test


TIFFSRC := /usr/local/src/tiff-v3.5.5
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

# To install tiff make sure you get and install v3.4pics.tar.gz
# This will create a pics subdirectory 
# Then run test_pics.sh -f pics/*.tif
# This will create the .rpt files to be used later in comparisons
tiff:  mustbegcc mustbemanju mustbelinux tiffclean
	cd $(TIFFSRC); make CC="$(CILLY)"; make test


tiff-gcc:  mustbegcc mustbemanju mustbelinux tiffclean
	cd $(TIFFSRC); make CC="gcc"; make test 



JPEGSRC :=/usr/local/src/jpeg-6b
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

jpeg: mustbegcc mustbemanju mustbelinux jpegclean
	cd $(JPEGSRC); make CC="$(CILLY)"; make test

jpeg-gcc: mustbegcc mustbemanju mustbelinux jpegclean
	cd $(JPEGSRC); make CC=gcc; make test


# Make zlib first
# Create link ../zlib -> ../zlib-0.0.0
# Create link ../libpng -> ../libpng-0.0.0
# Create link ./scripts/makefile.std ./Makefile
# We do not use the makefile for linux because we do
# not want to use DLLs
LIBPNGSRC :=/usr/local/src/libpng-1.0.8
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

libpng: mustbegcc mustbemanju mustbelinux libpngclean
	cd $(LIBPNGSRC); make CC="$(CILLY)"; make test

libpng-gcc: mustbegcc mustbemanju mustbelinux libpngclean
	cd $(LIBPNGSRC); make CC=gcc; make test



# MPEG
MPEGSRC :=/usr/local/src/mpeg_lib-1.3.1
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

mpeg: mustbegcc mustbemanju mustbelinux mpegclean
	cd $(MPEGSRC); make CC="$(CILLY)"; ./mpegtest test.mpg

mpeg-gcc: mustbegcc mustbemanju mustbelinux mpegclean
	cd $(MPEGSRC); make CC=gcc;  ./mpegtest test.mpg




# GLIB
GLIBSRC :=/usr/local/src/glib-1.2.9
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

glib: mustbegcc mustbemanju mustbelinux glibclean
	cd $(GLIBSRC); make CC="$(CILLY) --keepunused"; 
	cd $(GLIBSRC)/tests; make check-TESTS

glib-gcc: mustbegcc mustbemanju mustbelinux glibclean
	cd $(GLIBSRC); make CC=gcc; 
	cd $(GLIBSRC)/tests; make check-TESTS

glib-install: glib


# GTK
# Must invoke 
# ./configure --with-glib=../glib-1.2.9
GTKSRC :=/usr/local/src/gtk+-1.2.9
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

gtk: mustbegcc mustbemanju mustbelinux gtkclean
	cd $(GTKSRC); make CC="$(CILLY)"; 

gtk-gcc: mustbegcc mustbemanju mustbelinux gtkclean
	cd $(GTKSRC); make CC=gcc; 




gimpall: zlib tiff libpgn jpeg mpeg glib gtk






#######
#######  C-TORTURE
#######
CTORTDIR := /usr/local/src/gcc/gcc/testsuite/gcc.c-torture
# CILLY := gcc
tort/compile/%: $(CTORTDIR)/compile/%.c mustbemanju mustbegcc
	$(CILLY) $(CTORTDIR)/compile/$*.c -c -o $(CTORTDIR)/compile/$*.o

tort/execute/%: $(CTORTDIR)/execute/%.c mustbemanju mustbegcc
	$(CILLY) $(CTORTDIR)/execute/$*.c -o $(CTORTDIR)/compile/a.exe
	echo Cure complete
	$(CTORTDIR)/compile/a.exe

tort/compat/%: $(CTORTDIR)/compat/%.c mustbemanju mustbegcc
	$(CILLY) $(CTORTDIR)/compat/$*.c -o $(CTORTDIR)/compile/a.exe
	echo Cure complete
	$(CTORTDIR)/compile/a.exe

