# toplevel Makefile for cil project
# author: George Necula
#
# 3/06/01 sm: made the rules depend on environment variable ARCHOS,
#             so I can say x86_LINUX
# 3/17/01 sm: replaced a few more instances of x86_WIN32 with $(ARCHOS)

# Debugging. Set ECHO= to debug this Makefile 
ECHO = @

# USECCGR = 1
USEFRONTC = 1

# First stuff that makes the executable 
# Define the ARCHOS in your environemt : [x86_LINUX, x86_WIN32, SUNOS]

SOURCEDIRS  = src
OBJDIR      = obj
MLLS        = 
MLYS        = 
# ast clex cparse
# sm: trace: utility for debug-time printfs
MODULES     = pretty trace errormsg stats util clist \
              cil logcalls check ptrnode \
              solveutil solver globinit \
              oneret boxsplit boxstats box markptr \
              rmtmps optim
EXECUTABLE  = $(OBJDIR)/ccured
CAMLUSEUNIX = 1
ifdef RELEASE
UNSAFE      = 1
endif
CAMLLIBS    = 

ifdef USECCGR
MLLS      += mllex.mll
MLYS      += cilparse.mly
MODULES   += mllex cilparse
PARSELIBS += ../parsgen/libccgr.a ../smbase/libsmbase.a \
             libstdc++-3-libc6.1-2-2.10.0.a
endif

ifdef USEFRONTC
SOURCEDIRS += src/frontc
MLLS       += clexer.mll
MLYS       += cparser.mly
MODULES    += cabs cprint combine clexer cparser cabs2cil cabsvisit \
              patch frontc
endif

# Add main late
MODULES    += main

# What should we put into the OCAML CIL library? 
# everything by main.cl, basically
OCAML_CIL_LIB_MODULES = $(MODULES:main=)

# Additional things to clean
EXTRACLEAN += $(OBJDIR)/*.obj $(OBJDIR)/*.a $(OBJDIR)/*.o


    # Include now the common set of rules for OCAML
    # This file will add the rules to make $(EXECUTABLE).$(EXE)
include Makefile.ocaml

# Now do the machine-specific customization
-include $(CCUREDHOME)/.ccuredrc

# By default use the old patcher
ifndef NEWPATCH
OLDPATCH = 1
PATCHINCLUDES=1
endif

# By default use GCC
ifndef _MSVC
_GNUCC = 1
endif



PCCDIR=$(CCUREDHOME)/test/PCC
CCURED=perl $(CCUREDHOME)/lib/ccured.pl 
COMBINECC=perl $(CCUREDHOME)/lib/combiner.pl


# sm: I keep getting bit by this
ifndef CCUREDHOME
# sm: why doesn't this do what the manual says?
#HMM=$(error "wtf")
HMM="you_have_to_set_the_CCUREDHOME_environment_variable"
BASEDIR=$(HMM)
CCUREDHOME=$(HMM)
PCCDIR=$(HMM)
TVDIR=$(HMM)
CCUREDHOME=$(HMM)
_GNUCC=$(HMM)
endif


export EXTRAARGS
export INFERBOX

ifdef _GNUCC
DEBUGCCL=gcc -Wall -x c -g -ggdb -D_GNUCC 
RELEASECCL=gcc -x c -O3 -fomit-frame-pointer -D_RELEASE -D_GNUCC -Wall 
#LIB=lib
#LIBOUT=-o
ifdef RELEASE
# sm: I'll leave this here, but only use it for compiling our runtime lib
DOOPT=-O3
else
DOOPT=-g
endif
OPT_O2=-O2
CONLY=-c
OBJOUT=-o
OBJ=o
LIBEXT=a
EXEOUT=-o
LDEXT=
DEF=-D
ASMONLY=-S -o 
WARNALL=-Wall
# sm: shuffled around a couple things so I could use CPPSTART for patch2
CPPSTART=gcc -E -x c -Dx86_LINUX -D_GNUCC  -I/usr/include/sys
CPPOUT=-o %o
CPP=$(CPPSTART) -include fixup.h %i $(CPPOUT)
INC=-I
PATCHFILE=safec_gcc.patch
# sm: disable patching for now ('true' has no output)
# (set it to 'echo' to re-enable)
ifndef PATCHECHO
  PATCHECHO=echo
endif
endif


ifdef _MSVC
DEBUGCCL=cl /TC /Zi /MLd /I./lib /DEBUG
RELEASECCL=cl /TC /ML /I./lib
ifdef RELEASE
DOOPT=/Ox /Ob2 /G6
else
DOOPT=/Zi /MLd
endif
CONLY=/c
OBJOUT=/Fo
OBJ=obj
LIBEXT=lib
EXEOUT=/Fe
# sm: the extension added by the linker automatically
LDEXT=.exe
DEF=/D
ASMONLY=/Fa
INC=/I
CPPSTART=cl /Dx86_WIN32 /D_MSVC /E /TC /I./lib /DCCURED
CPPOUT=  >%o
CPP=$(CPPSTART) /FI fixup.h %i $(CPPOUT)
CCURED += --mode=mscl
PATCHFILE=safec_msvc.patch
PATCHECHO=echo
endif

ifdef RELEASE
CCL=$(RELEASECCL)
else
CCL=$(DEBUGCCL)
endif
CC=$(CCL) $(CONLY)


ifdef RELEASE
SAFECLIB=obj/ccured.$(LIBEXT)
CILLIB=obj/cillib.$(LIBEXT)
else
SAFECLIB=obj/ccureddebug.$(LIBEXT)
CILLIB=obj/cillibdebug.$(LIBEXT)
endif

ifdef PATCHINCLUDES
STANDARDPATCH= --includedir=$(CCUREDHOME)/include
else
STANDARDPATCH= --patch=$(CCUREDHOME)/cil/lib/$(PATCHFILE)
endif

# By default take manual box definitions into consideration
ifdef INFERBOX
MANUALBOX=1
endif

######################
.PHONY : defaulttarget
ifdef NOREMAKE
defaulttarget : 
else
defaulttarget : $(EXECUTABLE)$(EXE) $(SAFECLIB) $(CILLIB)
endif

combiner:
	make -f Makefile.combiner RELEASE=$(RELEASE)

cilly: 
	make -f Makefile.cil RELEASE=$(RELEASE)

setup: combiner cilly defaulttarget includes




# sm: my options
ifdef USER_SCOTT
  # I like -g always
  CCURED+= -g
  # currently the #line directives are inaccurate, so
  # they are counterproductive
  #CCURED+= --noPrintLn

  # trace patching process
  #TRACE=patch
endif

# bite the bullet and make it the default
ifndef OLDPATCH
  NEWPATCH=1
endif


# weimer: support for other solvers
ifdef INFERBOX
  CCURED+= --curetype=$(INFERBOX) $(DEF)INFERBOX
  PATCHDEFS= $(DEF)CCURED
  CCURED+= --emitinfer 
else
ifndef MANUALBOX
CCURED+= --boxdefaultwild
endif
endif

ifdef MANUALBOX
CCURED+= $(DEF)MANUALBOX
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

ifdef SHOWCABS
CCURED+= --usecabs
endif
ifdef SHOWCIL
CCURED+= --usecil
endif	
ifdef NO_TAGS
CCURED+= $(DEF)NO_TAGS
endif
ifdef CHECK
CCURED += --check
endif
ifndef RELEASE
CCURED+= --debug
endif
ifdef VERBOSE
CCURED+= --verbose
endif
# sm: pass tracing directives on 'make' command line like TRACE=usedVars
ifdef TRACE
CCURED+= --tr="$(TRACE)"
endif

ifdef OPTIM
CCURED+= --optimize
endif

# This is a way to disable the stats, allowing the command line to override it
# Do STATS= to disable the stats.
STATS=1
ifdef STATS
CCURED+= --stats
endif

# sm: can't figure out why passing this via EXTRAARGS screws
# up other things (e.g. -DMANUALBOX)
# update: reason was we were modifying EXTRAARGS in here -- we
# should only set EXTRAARGS when invoking the Makefile, and
# in here just use SAFECC+= ...
ifdef LOGCALLS
CCURED+= --logcalls
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

  # hack: include PATCHDEFS in the name, so we get different versions
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
  PATCHECHO=true
endif

# sm: user-specific configuration; the leading '-' means it's ok
# if this file doesn't exist; this file is *not* checked in to
# the CVS repository (please be careful to avoid putting things
# in here which will cause things to break when it's missing)
-include site-config.mk


# ----------- above here is configuration -------------------
# ----------- below here are rules to build the translator ---------
# (actually, mostly they're in the MODULES line above and in Makefile.ocaml)


.PHONY : defaulttarget
ifdef NOREMAKE
defaulttarget: 
else
defaulttarget: $(EXECUTABLE)$(EXE) $(SAFECLIB) $(CILLIB) $(PATCHFILE2)
endif

.PHONY: trval
trval: 
	make -C $(TVDIR)
	make -C $(TVDIR) RELEASE=1

# ww: build an OCAML library (CMA / CMXA) that exports our Cil stuff
# kudos to George for this lovely patsubst code ...
obj/cil.$(CMXA): $(OCAML_CIL_LIB_MODULES:%=$(OBJDIR)/%.$(CMO))
	$(CAMLLINK) -a -o $@ $^


# garbage collector options
ifneq ($(COMPUTERNAME), RAW)   # George's workstation
ifneq ($(COMPUTERNAME), FETA)   # George's workstation
ifdef _GNUCC
  ifndef NO_GC
  #ifdef USE_GC
    # enable the garbage collector by default for gcc
    CCURED+= $(DEF)USE_GC
    DEBUGCCL+= $(DEF)USE_GC
    RELEASECCL+= $(DEF)USE_GC
    GCLIB = $(CCUREDHOME)/lib/gc/gc.a

$(GCLIB):
	cd lib/gc; make && ./gctest

  else
    GCLIB =
  endif
else
  # on msvc, what needs to be done to get gc working:
  #  - make sure we can compile gc.a, and that gctests works
  #  - modify the commands which build $(SAFECLIB) so they
  #    include gc.a
  GCLIB =
endif
endif
endif

CCURED+= $(EXTRAARGS)

###
###
###    # Now the rules to make the library
###
###
ifndef RELEASE
SAFECLIBARG=$(DEF)_DEBUG
endif

SAFECPATCHER=perl $(CCUREDHOME)/lib/patcher.pl
ifdef _MSVC
$(SAFECLIB) : lib/safec.c lib/safec.h lib/safeccheck.h lib/splay.c 
	cl $(DOOPT) /I./lib /c $(DEF)_MSVC $(SAFECLIBARG) \
                                           $(OBJOUT)obj/safec.o lib/safec.c
	cl $(DOOPT) /I./lib /c $(DEF)_MSVC $(SAFECLIBARG) \
                                           $(OBJOUT)obj/splay.o lib/splay.c
	lib /OUT:$@ obj/safec.o obj/splay.o 
$(CILLIB) : lib/cillib.c
	cl $(DOOPT) /I./lib /Gy /c $(DEF)_MSVC $(SAFECLIBARG) \
                                           $(OBJOUT)obj/cillib.o lib/cillib.c
	lib /OUT:$@ obj/cillib.o

SAFECPATCHER += --mode=mscl 
PATCH_SYSINCLUDES=stdio.h ctype.h string.h io.h stdarg.h crtdbg.h
includes: cleanincludes
	$(SAFECPATCHER) --patch=$(CCUREDHOME)/lib/safec_msvc.patch \
                        --dest=$(CCUREDHOME)/include \
	                $(foreach file,$(PATCH_SYSINCLUDES), --sfile=$(file))
cleanincludes: 
	$(SAFECPATCHER) --dest=$(CCUREDHOME)/include --clean
endif

# Libraries on GCC
# sm: if GC is enabled, we just add it to the runtime library
# (or rather, we add safec.o to gc.a's contents)
ifdef _GNUCC
$(SAFECLIB) : lib/safec.c $(GCLIB) lib/splay.o
	$(CC) $(SAFECLIBARG) $(OBJOUT)obj/safec.o $<
	if echo $(GCLIB) | grep / >/dev/null; then \
		cp -f $(GCLIB) $@; echo "using GC"; \
	else \
		rm -f $@; echo "not using GC"; \
	fi
	ar -r $@ obj/safec.o lib/splay.o
	ranlib $@

$(CILLIB) : lib/cillib.c
	$(CC) $(SAFECLIBARG) $(OBJOUT)obj/cillib.o $<
	ar -r $@ obj/cillib.o
	ranlib $@

SAFECPATCHER += --mode gcc
PATCH_SYSINCLUDES=stdio.h ctype.h sys/fcntl.h fcntl.h string.h stdarg.h
includes: cleanincludes
	$(SAFECPATCHER) --patch=$(CCUREDHOME)/lib/safec_gcc.patch \
                        --dest=$(CCUREDHOME)/include \
	                $(foreach file,$(PATCH_SYSINCLUDES), --sfile=$(file))
cleanincludes: 
	$(SAFECPATCHER) --dest=$(CCUREDHOME)/include --clean
endif
# new patching specification wants to be run through preprocessor before use
ifdef NEWPATCH
$(PATCHFILE2): lib/$(PATCHFILE)2
	$(CPPSTART) $(PATCHDEFS) lib/$(PATCHFILE)2 > $(PATCHFILE2)
endif

# ----------- above here are rules for building the translator ----------
# ----------- below here are rules for building benchmarks --------

mustbegcc :
ifndef _GNUCC
	@echo This test case works only with _GNUCC=1; exit 3
endif

# sm: find and remove all the intermediate files from translation
clean-byproducts:
	find test \( \
		-name '*cil.c' -o \
		-name '*box.c' -o \
		-name '*cured.c' -o \
		-name '*.i' -o \
		-name '*_ppp.c' -o \
		-name '*.origi' -o \
		-name '*.o' -o \
		-name '*cabs.c' -o \
		-name '*infer.c' -o \
		-name '*_all*.c' \
		-name '*_comb*.c' \
	\) -exec rm {} \;

####### Test with PCC sources
PCCTEST=test/PCCout
ifdef RELEASE
PCCTYPE=RELEASE
SPJARG=
else
PCCTYPE=_DEBUG
SPJARG=--gory --save-temps=pccout
endif
ifdef _GNUCC
PCCCOMP=_GNUCC
else
PCCCOMP=_MSVC
endif

testpcc/% : $(PCCDIR)/src/%.c defaulttarget
	cd $(CCUREDHOME)/test/PCCout; $(CCURED) --keep=. $(DEF)$(ARCHOS) \
                  $(DEF)$(PCCTYPE) $(CONLY) \
                  $(PCCDIR)/src/$*.c \
                  $(OBJOUT)$(notdir $*).o



ifdef _MSVC
MSLINK=--mode=mscl
endif
PCCSAFECC=$(CCURED) $(DEF)CCURED \
                    $(STANDARDPATCH) --combine \
                    --keep=$(CCUREDHOME)/test/PCCout \
                    --nocure=pccbox --nocure=alloc
pcc : defaulttarget
#	-rm $(PCCDIR)/$(ARCHOS)$(PCCCOMP)/$(PCCTYPE)/*.o
	-rm $(PCCDIR)/$(ARCHOS)$(PCCCOMP)/$(PCCTYPE)/*.exe
	-rm $(PCCDIR)/bin/*.exe
	make -C $(PCCDIR) \
             CC="$(PCCSAFECC) $(CONLY)" \
             LD="$(CCURED) $(MSLINK) --combine --keep=$(CCUREDHOME)/test/PCCout" \
             USE_JAVA=1 USE_JUMPTABLE=1 TYPE=$(PCCTYPE) \
             COMPILER=$(PCCCOMP) \
	     clean defaulttarget 

pcc-noclean : defaulttarget
#	-rm $(PCCDIR)/$(ARCHOS)$(PCCCOMP)/$(PCCTYPE)/*.o
	-rm $(PCCDIR)/$(ARCHOS)$(PCCCOMP)/$(PCCTYPE)/*.exe
	-rm $(PCCDIR)/bin/*.exe
	make -C $(PCCDIR) \
             CC="$(PCCSAFECC) $(CONLY)" \
             LD="$(CCURED) $(MSLINK) --combine --keep=$(CCUREDHOME)/test/PCCout" \
             USE_JAVA=1 USE_JUMPTABLE=1 TYPE=$(PCCTYPE) \
             COMPILER=$(PCCCOMP) \
	     defaulttarget 

pcc-combined: defaulttarget
	cd $(PCCDIR)/bin; \
          $(CCURED) engine.$(ARCHOS)$(PCCCOMP).$(PCCTYPE).exe_all.c \
              $(EXEOUT)engine.$(ARCHOS)$(PCCCOMP).$(PCCTYPE).exe


pccclean :
	make -C $(PCCDIR) clean


SPJDIR=C:/Necula/Source/Touchstone/test
SPJARG +=  -WV,"-H,4000000,-noindent" -WC,"-H,4000000,-noindent"
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
SMALL1=test/small1
test/% : $(SMALL1)/%.c defaulttarget
	cd $(SMALL1); $(CCURED)   \
               $(STANDARDPATCH) \
	       $(CONLY) $(DOOPT) $(ASMONLY)$*.s $*.c 

testnopatch/% : $(SMALL1)/%.c defaulttarget
	cd $(SMALL1); $(CCURED)   \
               $(CONLY) $(DOOPT) $(ASMONLY)$*.s $*.c 

testexe/% : $(SMALL1)/%.c  defaulttarget
	cd $(SMALL1); $(CCURED)   \
               $(STANDARDPATCH) \
	       $(DOOPT) $(EXEOUT)$*.exe $*.c 


testrun/% : $(SMALL1)/%.c  defaulttarget
	cd $(SMALL1); $(CCURED)   \
               $(STANDARDPATCH) \
	       $(DOOPT) $(EXEOUT)$*.exe $*.c
	cd $(SMALL1); ./$*.exe



testmodel/%: $(SMALL1)/%.c $(SMALL1)/modelextern.c defaulttarget
	cd $(SMALL1); \
            $(DEBUGCCL) $(CONLY) $(OBJOUT)modelextern.$(OBJ) modelextern.c
	cd $(SMALL1); $(CCURED) \
                         $(STANDARDPATCH) \
                         --nocure=modelextern --combine \
                         $(DOOPT) $(EXEOUT)$*.exe $*.c modelextern.$(OBJ)
	cd $(SMALL1); ./$*.exe

combine%_3: defaulttarget
	cd $(SMALL1); \
          $(CCURED) $(DOOPT) \
                    combine$*_1.c combine$*_2.c combine$*_3.c \
                    --combine  \
                    $(STANDARDPATCH) \
	            $(EXEOUT)combine$*.exe
	cd $(SMALL1); ./combine$*.exe

# weimer: test, compile and run
testc/% : $(SMALL1)/%.c  defaulttarget
	cd $(SMALL1); $(CCURED)   \
               $(STANDARDPATCH) \
	       $(DOOPT) $(EXEOUT)$*.exe $*.c ; ./$*.exe

# Aman's optim tests
OPTIMTESTDIR=test/optim
optim/% : $(OPTIMTESTDIR)/%.c defaulttarget
	cd $(OPTIMTESTDIR); $(CCURED)   \
               $(STANDARDPATCH) \
	       $(DOOPT) $*.c $(EXEOUT)$*.exe
	$(OPTIMTESTDIR)/$*.exe



hashtest: test/small2/hashtest.c defaulttarget
	rm -f $(PCCTEST)/hashtest.exe
	cd $(PCCTEST); $(CCURED) --combine \
                                 --keep=. $(DEF)$(ARCHOS) $(DEF)$(PCCTYPE) \
                 $(DOOPT) \
                 `$(PATCHECHO) $(STANDARDPATCH)` \
                 $(INC)$(PCCDIR)/src \
                 $(PCCDIR)/src/hash.c \
                 ../small2/hashtest.c \
                 $(EXEOUT)hashtest.exe
	$(PCCTEST)/hashtest.exe


rbtest: test/small2/rbtest.c defaulttarget
	rm -f $(PCCTEST)/rbtest.exe
	@true "compile with gcc for better error diagnostics (ha!)"
	cd $(PCCTEST); $(CCURED) --combine \
                                 --keep=. $(DEF)$(ARCHOS) $(DEF)$(PCCTYPE) \
                 `$(PATCHECHO) $(STANDARDPATCH)` \
                 $(DOOPT) \
                 $(INC)$(PCCDIR)/src \
                 $(PCCDIR)/src/redblack.c \
                 ../small2/rbtest.c \
                 $(EXEOUT)rbtest.exe
	$(PCCTEST)/rbtest.exe letGcFree

btreetest: test/small2/testbtree.c \
           test/small2/btree.c defaulttarget
	rm -f test/small2/btreetest.exe
	cd test/small2; $(CCURED) --combine --keep=. \
                 $(DOOPT) \
                 $(STANDARDPATCH) \
                 btree.c testbtree.c \
                 $(EXEOUT)btreetest.exe
	test/small2/btreetest.exe


# sm: this is my little test program
hola: scott/hola

# sm: attempt at a single rule for my testing purposes
scott/%: test/small2/%.c defaulttarget
	rm -f test/small2/$*
	cd test/small2; $(CC) $(CONLY) $(WARNALL) $(DEF)$(ARCHOS) $*.c
	cd test/small2; $(CCURED) --verbose --keep=. $(DEF)$(ARCHOS) \
                 `$(PATCHECHO) $(STANDARDPATCH)` \
                 $(DOOPT) `true $(WARNALL)` $(NOPRINTLN) \
                 $*.c \
                 $(EXEOUT)$*
	sh -c "time test/small2/$*"

scott-nolink/%: test/small2/%.c defaulttarget
	rm -f test/small2/$*
	cd test/small2; $(CC) $(CONLY) $(WARNALL) $(DEF)$(ARCHOS) $*.c
	cd test/small2; $(CCURED) $(CONLY) --verbose --keep=. $(DEF)$(ARCHOS) \
                 `$(PATCHECHO) $(STANDARDPATCH)` \
                 $(DOOPT) $(WARNALL) $(NOPRINTLN) \
                 $*.c \
                 $(EXEOUT)$*

# a target for programs which are *supposed* to fail, because
# they intentionally violate the type system; but this is only
# when FAIL is #defined, otherwise they should exit ok
bad/%: test/bad/%.c defaulttarget
	rm -f test/bad/$*
	cd test/bad; $(CC) $(CONLY) $(WARNALL) $(DEF)$(ARCHOS) $*.c
	@true "first try the succeed case"
	cd test/bad; $(CCURED) --verbose --keep=. $(DEF)$(ARCHOS) \
                 `$(PATCHECHO) $(STANDARDPATCH)` \
                 $(DOOPT) $(WARNALL) $(NOPRINTLN) \
                 $*.c \
                 $(EXEOUT)$*
	if test/bad/$*; then \
		echo "(worked as expected, when FAIL not defined)"; exit 0; \
	else \
		echo "That should have worked; FAIL was not defined!"; exit 2; \
	fi
	@true "now try the failure case"
	cd test/bad; $(CCURED) --verbose --keep=. $(DEF)$(ARCHOS) \
                 `$(PATCHECHO) $(STANDARDPATCH)` \
                 $(DOOPT) $(WARNALL) $(NOPRINTLN) -DFAIL \
                 $*.c \
                 $(EXEOUT)$*
	if test/bad/$*; then \
		echo "That should have failed!"; exit 2; \
	else \
		echo "(failed as expected)"; exit 0; \
	fi

# same rules, this time in 'scott' directory, since it's a pain to
# move the file just to add a failure case
bads/%: test/small2/%.c defaulttarget
	rm -f test/small2/$*
	cd test/small2; $(CC) $(CONLY) $(WARNALL) $(DEF)$(ARCHOS) $*.c
	@true "first try the succeed case"
	cd test/small2; $(CCURED) --verbose --keep=. $(DEF)$(ARCHOS) \
                 `$(PATCHECHO) $(STANDARDPATCH)` \
                 $(DOOPT) $(WARNALL) $(NOPRINTLN) \
                 $*.c \
                 $(EXEOUT)$*
	if test/small2/$*; then \
		echo "(worked as expected, when FAIL not defined)"; exit 0; \
	else \
		echo "That should have worked; FAIL was not defined!"; exit 2; \
	fi
	@true "now try the failure case"
	cd test/small2; $(CCURED) --verbose --keep=. $(DEF)$(ARCHOS) \
                 `$(PATCHECHO) $(STANDARDPATCH)` \
                 $(DOOPT) $(WARNALL) $(NOPRINTLN) -DFAIL \
                 $*.c \
                 $(EXEOUT)$*
	if test/small2/$*; then \
		echo "That should have failed!"; exit 2; \
	else \
		echo "(failed as expected)"; exit 0; \
	fi



# sm: trivial test of combiner
MYSAFECC = $(CCURED) --keep=. $(DEF)$(ARCHOS) $(STANDARDPATCH)
comb: test/small2/comb1.c test/small2/comb2.c defaulttarget
	rm -f test/small2/comb
	cd test/small2; \
	  $(MYSAFECC) --combine comb1.c $(CONLY) $(OBJOUT) comb1.o; \
	  $(MYSAFECC) --combine comb2.c $(CONLY) $(OBJOUT) comb2.o; \
          $(MYSAFECC) --combine comb1.o comb2.o $(EXEOUT)comb
	test/small2/comb

# sm: test of combiner's ability to report inconsistencies
baddef: test/small2/baddef1.c test/small2/baddef2.c defaulttarget
	cd test/small2; $(CCL) baddef1.c baddef2.c -o baddef && ./baddef
	rm -f test/small2/baddef
	cd test/small2; \
	  $(MYSAFECC) --combine baddef1.c $(CONLY) $(OBJOUT) baddef1.o; \
	  $(MYSAFECC) --combine baddef2.c $(CONLY) $(OBJOUT) baddef2.o; \
          $(MYSAFECC) --combine baddef1.o baddef2.o $(EXEOUT)baddef \
	  > baddef.rept 2>&1
	cat test/small2/baddef.rept
	test/small2/baddef
	if grep conflicting test/small2/baddef.rept >/dev/null; then \
	  echo "OK: conflict detected"; \
	else \
	  echo "FAIL: missed the conflict!"; exit 1; \
	fi

# cfrac: a memory benchmark which factorizes into products of primes
CFRACDIR = $(CCUREDHOME)/../bench/cfrac
cfrac: defaulttarget
	-rm $(CFRACDIR)/*.o
	-rm $(CFRACDIR)/cfrac
	make -C $(CFRACDIR) \
	  CC="$(CCURED) --keep=$(CFRACDIR)" \
	  LD="$(CCURED) --keep=$(CFRACDIR)"
	csh -c "time $(CFRACDIR)/cfrac 327905606740421458831903"

comcfrac: defaulttarget
	-rm $(CFRACDIR)/*.o
	-rm $(CFRACDIR)/cfrac
	make -C $(CFRACDIR) \
	  CC="$(CCURED) --combine --keep=$(CFRACDIR)" \
	  LD="$(CCURED) --combine --keep=$(CFRACDIR)"
	csh -c "time $(CFRACDIR)/cfrac 327905606740421458831903"

# espresso: memory benchmark that does logic minimization
ESPRESSODIR = $(CCUREDHOME)/../bench/espresso
espresso: defaulttarget
	@true -rm $(ESPRESSODIR)/*.o
	@true -rm $(ESPRESSODIR)/espresso
	make -C $(ESPRESSODIR) \
	  CC="$(CCURED) --keep=$(ESPRESSODIR)" \
	  LD="$(CCURED) --keep=$(ESPRESSODIR)"
	csh -c "time $(ESPRESSODIR)/espresso -t $(ESPRESSODIR)INPUT/Z5xp1.espresso >/dev/null"




HUFFCOMPILE=$(CCURED) $(DEF)NOVARARG --combine --keep=. 
# HUFFCOMPILE=cl /MLd
ifdef _GNUCC
HUFFOTHERS += -lm
endif
ifndef HUFFINPUT
  HUFFINPUT=$(CCUREDHOME)/src/frontc/cparser.output
endif
hufftest: test/small2/hufftest.c defaulttarget
	rm -f $(PCCTEST)/hufftest.exe \
              $(PCCTEST)/huffman.compressed \
              $(PCCTEST)/huffman.code \
              $(PCCTEST)/huffman.freq
	cd $(PCCTEST); $(HUFFCOMPILE) \
                 $(DEF)$(ARCHOS) $(DEF)$(PCCTYPE) $(DEF)$(PCCCOMP) \
                 $(DOOPT) \
                 $(STANDARDPATCH) \
                 $(INC)$(PCCDIR)/src \
                 $(PCCDIR)/src/io.c \
                 $(PCCDIR)/src/huffman.c \
                 $(PCCDIR)/src/hash.c \
                 ../small2/hufftest.c \
                 $(HUFFOTHERS) \
                 $(EXEOUT)hufftest.exe
	cd $(PCCTEST); ./hufftest.exe $(HUFFINPUT)


wes-rbtest: test/small2/wes-rbtest.c defaulttarget
	rm -f $(PCCTEST)/wes-rbtest.exe
	cd $(PCCTEST); $(CCURED) --keep=. $(DEF)$(ARCHOS) $(DEF)$(PCCTYPE) \
                 $(DOOPT) \
                 $(STANDARDPATCH) \
                 $(INC)$(PCCDIR)/src \
                 ../small2/wes-rbtest.c \
                 $(EXEOUT)wes-rbtest.exe
	$(PCCTEST)/wes-rbtest.exe

wes-hashtest: test/small2/wes-hashtest.c defaulttarget
	rm -f $(PCCTEST)/wes-hashtest.exe
	cd $(PCCTEST); $(CCURED) --keep=. $(DEF)$(ARCHOS) $(DEF)$(PCCTYPE) \
                 $(DOOPT) \
                 $(STANDARDPATCH) \
                 $(INC)$(PCCDIR)/src \
                 ../small2/wes-hashtest.c \
                 $(EXEOUT)wes-hashtest.exe
	$(PCCTEST)/wes-hashtest.exe


### Generic test
testfile/% : defaulttarget
	$(CCURED) /TC $*

testdir/% : defaulttarget
	make -C CC="perl safecc.pl" $*


################## Linux device drivers
testlinux/% : test/linux/%.cpp defaulttarget
	cd test/linux; $(CCURED) -o $*.o $*.cpp 

testqp : testlinux/qpmouse
testserial: testlinux/generic_serial

################## Rahul's test cases
SPR-TESTDIR = test/spr
spr/% : defaulttarget
	cd $(SPR-TESTDIR); $(CCURED) $*.c $(CONLY) $(DOOPT) $(ASMONLY)$*.s


################# Apache test cases
APACHETEST=test/apache
APACHEBASE=apache_1.3.19/src
APATCHES=--patch=apache.patch 
ifdef _MSVC
APACHECFLAGS=/nologo /MDd /W3 /GX /Zi /Od \
         $(INC)"$(APACHEBASE)\include" $(INC)"$(APACHEBASE)\os\win32" \
         $(DEF)"_DEBUG" $(DEF)"WIN32" $(DEF)"_WINDOWS" \
         $(DEF)"NO_DBM_REWRITEMAP" $(DEF)"SHARED_MODULE" \
         $(DEF)"WIN32_LEAN_AND_MEAN"
APATCHES += --patch=apache_msvc.patch
else
APACHECFLAGS=-Wall -D_GNUCC -g \
         $(INC)"$(APACHEBASE)/include" $(INC)"$(APACHEBASE)/os/unix" \
         $(DEF)"_DEBUG" \
         $(DEF)"NO_DBM_REWRITEMAP" $(DEF)"SHARED_MODULE"
APATCHES += --patch=apache_gcc.patch
endif

APACHE_INCLUDES=httpd.h ap_alloc.h http_config.h http_log.h http_protocol.h
apachesetup:
	cd $(APACHETEST); \
            $(SAFECPATCHER) \
                        $(APACHECFLAGS) \
                        $(APATCHES) --patch=$(CCUREDHOME)/lib/$(PATCHFILE) \
                        --dest=$(APACHEBASE)/include \
	                $(foreach file,$(APACHE_INCLUDES), --ufile=$(file))

ifdef PATCHINCLUDES
APATCH = $(STANDARDPATCH) --includedir=$(APACHEBASE)/include
else
APATCH = $(STANDARDPATCH) $(APATHCES)
endif

apache/urlcount : defaulttarget
	rm -f $(APACHETEST)/mod_urlcount.$(OBJ)
	cd $(APACHETEST); $(CCURED) \
                       --keep=. $(APATCH) \
                        $(DOOPT) \
                        $(APACHECFLAGS) \
                        $(CONLY) $(OBJOUT)./mod_urlcount.$(OBJ) \
                        mod_urlcount.c

apache/layout : defaulttarget
	rm -f $(APACHETEST)/mod_layout.$(OBJ)
	cd $(APACHETEST); $(CCURED) \
                       --keep=. $(APATCH) \
                        $(DOOPT) \
                        $(APACHECFLAGS) \
                        $(CONLY) $(OBJOUT)./mod_layout.$(OBJ) \
                        mod_layout.c

apache/random : defaulttarget
	rm -f $(APACHETEST)/mod_random.$(OBJ)
	cd $(APACHETEST); $(CCURED) \
                       --keep=. $(APATCH) \
                        $(DOOPT) \
                        $(APACHECFLAGS) \
                        $(CONLY) $(OBJOUT)./mod_random.$(OBJ) \
                        mod_random.c

apache/info : defaulttarget
	rm -f $(APACHETEST)/mod_info.$(OBJ)
	cd $(APACHETEST); $(CCURED) \
                       --keep=. $(APATCH) \
                        $(DOOPT) \
                        $(APACHECFLAGS) \
                        $(CONLY) $(OBJOUT)./mod_info.$(OBJ) \
                        mod_info.c

apache/gzip : defaulttarget
	rm -f $(APACHETEST)/mod_gzip.$(OBJ)
	cd $(APACHETEST); $(CCURED) \
                       --keep=. $(APATCH) \
                        --usecil --cilout=this.cil $(DOOPT) \
                        $(APACHECFLAGS) \
                        $(CONLY) $(OBJOUT)./mod_gzip.$(OBJ) \
                        mod_gzip.c

apache/t : defaulttarget
	rm -f $(APACHETEST)/t.obj
	cd $(APACHETEST); $(CCURED) \
                       --keep=. $(APATCH) \
                        $(APACHECFLAGS) \
                        $(OBJOUT)./t.obj \
                        t.c

apache/rewrite: defaulttarget
	rm -f $(APACHETEST)/mod_gzip.$(OBJ)
	cd $(APACHETEST); $(CCURED) \
                       --keep=. $(APATCH) \
                        $(DOOPT) \
                        $(APACHECFLAGS) \
                        $(OBJOUT)./mod_rewrite.$(OBJ) \
                        $(APACHEBASE)/modules/standard/mod_rewrite.c





# sm: removed DOOPT since I want to specify optimization in the
# benchmark's Makefile (helps to ensure consistency between the
# non-ccured build and the ccured build, and also some programs
# take too long on -O3)
COMBINESAFECC = $(CCURED) --combine

# sm: trying to collapse where are specifications are
PATCHARG=`$(PATCHECHO) $(STANDARDPATCH)`

#
# OLDEN benchmarks
#
# Barnes-Hut
BHDIR=test/olden/bh
bh: defaulttarget mustbegcc
	cd $(BHDIR); rm -f code.exe *.o; \
               make CC="$(COMBINESAFECC) --nocure=bhbox $(PATCHARG)"
	echo  >$(BHDIR)/data.in
	echo  >>$(BHDIR)/data.in
	echo  >>$(BHDIR)/data.in
	echo  >>$(BHDIR)/data.in
	echo  >>$(BHDIR)/data.in
	echo  >>$(BHDIR)/data.in
	echo  >>$(BHDIR)/data.in
	echo  >>$(BHDIR)/data.in
	echo  >>$(BHDIR)/data.in
	echo  >>$(BHDIR)/data.in
	echo  >>$(BHDIR)/data.in
	echo  >>$(BHDIR)/data.in
	cd $(BHDIR); sh -c "time code < data.in > data.out"
	@true "sm: added next line to compare output to expected output"
#	cd $(BHDIR); sh -c "perl normalize.pl < data.out | diff data.cil.out - | head"

bh-combined: defaulttarget mustbegcc
	cd $(BHDIR); \
	    $(CCURED) code_all.c bhbox.c $(EXEOUT)code.exe
	echo  >$(BHDIR)/data.in
	echo  >>$(BHDIR)/data.in
	echo  >>$(BHDIR)/data.in
	echo  >>$(BHDIR)/data.in
	echo  >>$(BHDIR)/data.in
	echo  >>$(BHDIR)/data.in
	echo  >>$(BHDIR)/data.in
	echo  >>$(BHDIR)/data.in
	echo  >>$(BHDIR)/data.in
	echo  >>$(BHDIR)/data.in
	echo  >>$(BHDIR)/data.in
	echo  >>$(BHDIR)/data.in
	cd $(BHDIR); sh -c "time code < data.in >dat.out"

# Power pricing
PWDIR=test/olden/power
ifdef _GNUCC
PWEXTRA += -lm
endif
power: defaulttarget mustbegcc
	cd $(PWDIR); \
               make PLAIN=1 clean defaulttarget \
                    CC="$(COMBINESAFECC) $(PATCHARG)"
	cd $(PWDIR); sh -c "time ./power.exe"

power-combined : defaulttarget mustbegcc
	cd $(PWDIR); \
             $(CCURED) power.exe_all.c $(EXEOUT)power.exe

# Health care simulation
HEALTHDIR=test/olden/health
ifdef _MSVC
HEALTHARGS = _MSVC=1
endif
health: defaulttarget
	cd $(HEALTHDIR); \
               make PLAIN=1 clean defaulttarget \
                    $(HEALTHARGS) \
                    CC="$(COMBINESAFECC) \
                        --nocure=trusted_health \
			 $(PATCHARG)"
	cd $(HEALTHDIR); sh -c "time ./health$(LDEXT) 5 500 1 1"



# Perimeter of regions in images
PERIMDIR=test/olden/perimeter
ifdef _MSVC
PERIMARGS = _MSVC=1
endif
perimeter: defaulttarget
	cd $(PERIMDIR); \
               make PLAIN=1 clean defaulttarget \
                    $(PERIMARGS) \
                    CC="$(COMBINESAFECC) \
			$(PATCHARG)"
	cd $(PERIMDIR); sh -c "time ./perimeter.exe"


# Voronoi diagrams
VORONDIR=test/olden/voronoi
ifdef _MSVC
VORONARGS = _MSVC=1
endif
voronoi : defaulttarget
	cd $(VORONDIR); \
               make PLAIN=1 clean voronoi.exe \
                    $(VORONARGS) \
                    CC="$(COMBINESAFECC) \
                        --nocure=trusted_voronoi \
			$(PATCHARG)"
	cd $(VORONDIR); sh -c "time ./voronoi.exe 60000 1"

# Traveling salesman
TSPDIR=test/olden/tsp
ifdef _MSVC
TSPARGS = _MSVC=1
endif
ifdef _GNUCC
TSPEXTRA += -lm
endif
tsp: defaulttarget
	cd $(TSPDIR); \
               make PLAIN=1 clean defaulttarget \
                    $(TSPARGS) \
                    CC="$(COMBINESAFECC) \
			$(PATCHARG)"
	cd $(TSPDIR); sh -c "time ./tsp.exe"


# Bitonic sort
BISORTDIR=test/olden/bisort
ifdef _MSVC
BISORTARGS = _MSVC=1
endif
bisort : defaulttarget mustbegcc
	cd $(BISORTDIR); \
               make PLAIN=1 clean defaulttarget \
                    $(BISORTARGS) \
                    CC="$(COMBINESAFECC) \
                        --nocure=trusted_bisort \
			$(PATCHARG)"
	cd $(BISORTDIR); sh -c "time ./bisort.exe 100"




OLDENMSTDIR=test/olden/mst
OLDENMSTSAFECC=$(COMBINESAFECC) $(PATCHARG)
ifdef _MSVC
OLDENMSTSAFECC += $(DEF)WIN32 $(DEF)MSDOS
MSTARGS= _MSVC=1
endif
mst-clean: 	
	cd $(OLDENMSTDIR); make clean
	cd $(OLDENMSTDIR); rm -f *cil.c *box.c *.i *_ppp.c *.origi *_all.c

mst: defaulttarget
	-cd $(OLDENMSTDIR); rm gmon.out
	cd $(OLDENMSTDIR); \
            make clean mst.exe $(MSTARGS) \
                               CC="$(OLDENMSTSAFECC)" \
                               LD="$(OLDENMSTSAFECC)"
	cd $(OLDENMSTDIR); sh -c "time ./mst.exe 2048 1"
	cd $(OLDENMSTDIR); if test -f gmon.out; then gprof mst.exe gmon.out; fi




TREEADDIR=test/olden/treeadd
TREEADDSAFECC=$(CCURED) --combine --keep=safeccout  \
                  --nocure=ta_trusted \
                  $(PATCHARG) \
                  $(NOPRINTLN)
ifeq ($(ARCHOS), x86_WIN32)
TREEADDSAFECC += $(DEF)WIN32 $(DEF)MSDOS
endif
treeadd-clean: 	
	cd $(TREEADDIR); make clean
	cd $(TREEADDIR); rm -f *cil.c *box.c *.i *_ppp.c *.origi *_all.c

treeadd: defaulttarget mustbegcc
	cd $(TREEADDIR); \
            make clean treeadd CC="$(TREEADDSAFECC)" \
                       LD="$(TREEADDSAFECC)"
	cd $(TREEADDIR); sh -c "time ./treeadd$(LDEXT) 21 1"

NEWBISORTDIR=test/olden/newbisort
NEWBISORTSAFECC=$(CCURED) --combine --keep=safeccout  \
                   --nocure=ta_trusted \
                  $(PATCHARG) \
                  $(NOPRINTLN)
ifeq ($(ARCHOS), x86_WIN32)
NEWBISORTSAFECC += $(DEF)WIN32 $(DEF)MSDOS
endif
newbisort-clean: 	
	cd $(NEWBISORTDIR); make clean
	cd $(NEWBISORTDIR); rm -f *cil.c *box.c *.i *_ppp.c *.origi *_all.c

newbisort: defaulttarget mustbegcc
	cd $(NEWBISORTDIR); \
            make clean; make bisort CC="$(NEWBISORTSAFECC)" \
                       LD="$(NEWBISORTSAFECC)"
	cd $(NEWBISORTDIR); ./bisort 21 1




EM3DDIR=test/olden/em3d
EM3DDSAFECC=$(CCURED) --combine --keep=safeccout  \
                  $(PATCHARG) \
                  --nocure=trusted_em3d \
                  $(NOPRINTLN)
ifeq ($(ARCHOS), x86_WIN32)
EM3DSAFECC += $(DEF)WIN32 $(DEF)MSDOS
SS_RAND=TRUE
endif
em3d-clean: 	
	cd $(EM3DDIR); make clean
	cd $(EM3DDIR); rm -f *cil.c *box.c *.i *_ppp.c *.origi *_all.c

em3d: defaulttarget mustbegcc
	cd $(EM3DDIR); \
            make clean em3d CC="$(EM3DDSAFECC)" \
                            LD="$(EM3DDSAFECC)"
	cd $(EM3DDIR); sh -c "time ./em3d 2000 100 6"



# SPEC95
SPECDIR=test/spec95

COMPRESSDIR=$(SPECDIR)/129.compress
spec-compress : defaulttarget
	cd $(COMPRESSDIR)/src; make build
	cd $(COMPRESSDIR)/src; ./compress < input.data > output.txt

old-compress : defaulttarget $(COMPRESSDIR)/src/combine-compress.c
	rm -f $(COMPRESSDIR)/combine-compress.exe
	cd $(COMPRESSDIR)/src ; $(CCURED) --keep=. $(DEF)$(ARCHOS) $(DEF)$(PCCTYPE) \
                 $(DOOPT) \
                 combine-compress.c \
                 $(EXEOUT)combine-compress.exe
	cd $(COMPRESSDIR)/src; sh -c "time ./combine-compress.exe < input.data > combine-compress.out"

compress-noclean: defaulttarget mustbegcc
	cd $(COMPRESSDIR)/src; make CC="$(COMBINESAFECC)" build
	cd $(COMPRESSDIR)/src; sh -c "time ./compress < input.data > combine-compress.out"

compress: defaulttarget mustbegcc
	cd $(COMPRESSDIR)/src; \
               make CC="$(COMBINESAFECC) $(PATCHARG)" clean build
	cd $(COMPRESSDIR)/src; sh -c "time ./compress < input.data > combine-compress.out"

LIDIR=$(SPECDIR)/130.li
LISAFECC=$(CCURED) --combine $(PATCHARG) --keep=safeccout
li: defaulttarget mustbegcc
	cd $(LIDIR)/src; \
            make clean build CC="$(LISAFECC) $(CONLY)" \
                             LD="$(LISAFECC)"
	sh -c "time $(LIDIR)/src/trial_li \
            <$(LIDIR)/data/train/input/train.lsp \
            >$(LIDIR)/data/train/input/train.out"

li-combined: defaulttarget mustbegcc
	cd $(LIDIR)/src; \
            $(CCURED) trial_li_all.c $(LIEXTRA) $(EXEOUT)trial_li_all.exe

li-noclean: defaulttarget mustbegcc
	cd $(LIDIR)/src; \
            make build CC="$(LISAFECC) $(CONLY)" \
                       LD="$(LISAFECC)" \
                       EXTRA_LIBS=$(LIEXTRA) 
	sh -c "time $(LIDIR)/src/trial_li \
            <$(LIDIR)/data/train/input/train.lsp \
            >$(LIDIR)/data/train/input/train.out"

liclean: 
	cd $(LIDIR)/src; make clean
	cd $(LIDIR)/src; rm -f *cil.c *box.c *.i *_ppp.c *.origi trial_li_all.c

liinfer: li
	cd $(LIDIR)/src ; $(CCURED) --keep=. $(DEF)$(ARCHOS) $(DEF)$(PCCTYPE) \
                 $(DOOPT) \
                 trial_li.c \
                 $(EXEOUT)trial_li.exe


### SPEC95 GO
GODIR=$(SPECDIR)/099.go
GOSAFECC=$(CCURED) --combine  $(PATCHARG) \
                   --keep=safeccout $(NOPRINTLN) $(OPT_O2)

goclean: 	
	cd $(GODIR)/src; make clean
	cd $(GODIR)/src; rm -f *cil.c *box.c *.i *_ppp.c *.origi


go: defaulttarget mustbegcc
	cd $(GODIR)/src; \
            make clean build CC="$(GOSAFECC) $(CONLY)" \
                             LD="$(GOSAFECC)"
	sh -c "time $(GODIR)/src/go 50 9"

go-combined: defaulttarget mustbegcc
	cd $(GODIR)/src; \
	   $(CCURED) $(CONLY) go_all.c


go-noclean: defaulttarget mustbegcc
	cd $(GODIR)/src; \
            make build CC="$(GOSAFECC) $(CONLY)" \
                       LD="$(GOSAFECC)" \
                             EXTRA_LIBS=$(GOEXTRA) 
	sh -c "time $(GODIR)/src/go 50 9"



### SPEC95 vortex
VORDIR=$(SPECDIR)/147.vortex
VORSAFECC=$(CCURED) --combine   $(PATCHARG)
#VORSAFECC=$(CCURED)  $(PATCHARG)
ifdef _GNUCC
VOREXTRA=-lm
endif

vortexclean: 	
	cd $(VORDIR)/src; make clean
	cd $(VODIR)/src; rm -f *cil.c *box.c *.i *_ppp.c *.origi

vortex: defaulttarget mustbegcc
	cd $(VORDIR)/src; \
            make clean build CC="$(VORSAFECC) $(CONLY)" \
                             LD="$(VORSAFECC)"
	cd $(VORDIR)/src; sh -c "./testit vortex$(LDEXT)"

vortex-gcc: defaulttarget mustbegcc
	cd $(VORDIR)/src; \
            make clean build CC="gcc $(CONLY)" \
                             LD="gcc"
	cd $(VORDIR)/src; sh -c "./testit vortex.exe"

vortex-cabs:  defaulttarget mustbegcc
	cd $(VORDIR)/src; \
            make clean build CC="$(CCURED) --mode=gcc --cabs $(CONLY)" \
                             LD="gcc"
	cd $(VORDIR)/src; sh -c "./testit vortex.exe"

vortex-cil:  defaulttarget mustbegcc
	cd $(VORDIR)/src; \
            make clean build CC="$(CCURED) --cil $(CONLY)" \
                             LD="gcc"
	cd $(VORDIR)/src; sh -c "./testit vortex.exe"
vortex-run:
	cd $(VORDIR)/src; sh -c "./testit vortex.exe"

vortex-noclean: defaulttarget mustbegcc
	cd $(VORDIR)/src; \
            make build CC="$(VORSAFECC) $(CONLY)" \
                       LD="$(VORSAFECC)"
	cd $(VORDIR)/src; sh -c "./testit vortex.exe"

vortex-combined: defaulttarget mustbegcc
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
TVCOMMAND=$(TVDIR)/obj/transval.asm
else
TVCOMMAND=$(TVDIR)/obj/transval.asm.exe
endif

vortex-tv:
	$(TVCOMMAND) -L $(VORDIR)/tv.log $(VORDIR)/src/vortex_all.i.rtl $(VORDIR)/src/vortex_allcil.c.rtl 

### SPEC95 m88ksim
M88DIR=$(SPECDIR)/124.m88ksim
M88SAFECC=$(CCURED) --combine --keep=safeccout \
                    $(PATCHARG) \
                    --nocure=m88k_trusted --noPrintInferbox
m88kclean: 	
	cd $(M88DIR)/src; make clean
	cd $(M88DIR)/src; rm -f *cil.c *box.c *.i *_ppp.c *.origi

m88k: defaulttarget mustbegcc m88kclean
	cd $(M88DIR)/src; \
            make    build CC="$(M88SAFECC) $(CONLY)" \
                          LD="$(M88SAFECC)" 
	cd $(M88DIR)/src; sh -c "time ./m88k -c < ctl.in > out"
	cd $(M88DIR)/src; diff correct.output out >/dev/null

m88k-noclean: defaulttarget mustbegcc
	cd $(M88DIR)/src; \
            make       build CC="$(M88SAFECC) $(CONLY)" \
                             LD="$(M88SAFECC)" \
                             EXTRA_LIBS=$(M88EXTRA) 
	cd $(M88DIR)/src; sh -c "time ./m88k -c < ctl.in > out"
	cd $(M88DIR)/src; diff correct.output out >/dev/null

# sm: changed the target below to correspond with not putting the
# executable in exe/base, but didn't test it (don't know what
# it is for)
m88k-combined: defaulttarget mustbegcc
	cd $(M88DIR)src; \
            $(CCURED) m88k_all.c $(CONLY)

### SPEC95 ijpeg
IJPEGDIR=$(SPECDIR)/132.ijpeg
IJPEGSAFECC=$(CCURED) --combine --keep=safeccout  \
                  $(PATCHARG) \
                  --nocure=ijpeg_trusted -include ijpeg.fixup.h
ifeq ($(ARCHOS), x86_WIN32)
IJPEGSAFECC += -DWIN32 -DMSDOS
endif
ijpegclean: 	
	cd $(IJPEGDIR)/src; make clean
	cd $(IJPEGDIR)/src; rm -f *cil.c *box.c *.i *_ppp.c *.origi

ijpeg: defaulttarget mustbegcc
	cd $(IJPEGDIR)/src; \
            make clean build CC="$(IJPEGSAFECC) $(CONLY)" \
                             LD="$(IJPEGSAFECC)"
	sh -c "time $(IJPEGDIR)/src/ijpeg \
            -image_file $(IJPEGDIR)/data/ref/input/vigo.ppm \
            -GO"

ijpeg-combined: defaulttarget mustbegcc
	cd $(IJPEGDIR)/src; \
            $(CCURED) ijpeg_all.c $(IJPEGEXTRA) \
                $(EXEOUT)ijpeg
	sh -c "time $(IJPEGDIR)/src/ijpeg \
            -image_file $(IJPEGDIR)/data/ref/input/penguin.ppm \
            -GO"

ijpeg-noclean: defaulttarget mustbegcc
	cd $(IJPEGDIR)/src; \
            make       build CC="$(IJPEGSAFECC) $(CONLY)" \
                             LD="$(IJPEGSAFECC)" \
                             EXTRA_LIBS=$(IJPEGEXTRA)
	sh -c "time $(IJPEGDIR)/src/ijpeg \
            -image_file $(IJPEGDIR)/data/ref/input/penguin.ppm \
            -GO"

#### SPEC95 gcc
GCCDIR=$(SPECDIR)/126.gcc
# sm: --noPrintInferbox works around an infinite loop in our data structure
GCCSAFECC=$(CCURED) --combine --keep=safeccout --noPrintInferbox \
                    $(PATCHARG)


gccclean: 	
	cd $(GCCDIR)/src; make clean
	cd $(GCCDIR)/src; rm -f *cil.c *box.c *.i *_ppp.c *.origi

gcc: defaulttarget mustbegcc
	cd $(GCCDIR)/src; \
            make clean build CC="$(GCCSAFECC) -DCCURED $(CONLY)" \
                             LD="$(GCCSAFECC)" 

gcc-noclean: defaulttarget mustbegcc
	cd $(GCCDIR)/src; \
            make       build CC="$(GCCSAFECC) -DCCURED  $(CONLY)" \
                             LD="$(GCCSAFECC)" 

gcc-combined: defaulttarget mustbegcc
	cd $(GCCDIR)/exe/base; \
            $(CCURED) cc1.v8_all.c $(GCCEXTRA) \
                $(EXEOUT)cc1.v8.exe

allcc1: defaulttarget mustbegcc
	cd $(GCCDIR)/exe/base; $(CCURED) $(DOOPT) cc1.v8_all.c


#
# Linux
LINUXDIR=/home/project/linux-2.2.9

linuxstandard: 
	$(MAKE) -C $(LINUXDIR) clean vmlinux \
              MY-CC="gcc"

LINUXCC=perl $(CCUREDHOME)/lib/safecc.pl --mode=gcc
ifdef NOLINES
LINUXCC+= --noPrintLn
endif
ifdef COMMLINES
LINUXCC+= --commPrintLn
endif
linux-cabs: defaulttarget mustbegcc
	$(MAKE) -C $(LINUXDIR) vmlinux \
              MY-CC="$(LINUXCC) --cabs"

linux-cil: defaulttarget mustbegcc
	$(MAKE) -C $(LINUXDIR) vmlinux \
              MY-CC="$(LINUXCC) --cil"

linux-clean:
	$(MAKE) -C $(LINUXDIR) clean

combinetest: defaulttarget
	cd test/small1; $(CCURED) --combine /Fet.exe t.c t1.c

obj/prettytest.exe: src/pretty.mli src/pretty.ml src/prettytest.ml
	$(CAMLC) -I src -o obj/prettytest.exe src/pretty.mli src/pretty.ml src/prettytest.ml

prettytest:  obj/prettytest.exe
	time obj/prettytest.exe ; echo

constrainttest:
	$(CAMLC) -o obj/constraint.exe src/constraint.ml
	obj/constraint.exe

### ftpd-BSD-0.3.2-5
FTPDDIR=test/ftpd/ftpd
FTPDSAFECC=$(CCURED) --combine --keep=safeccout  \
                  $(PATCHARG) \
                  $(NOPRINTLN)
ifeq ($(ARCHOS), x86_WIN32)
FTPDSAFECC += $(DEF)WIN32 $(DEF)MSDOS
endif
ftpd-clean: 	
	cd $(FTPDDIR); make clean
	cd $(FTPDDIR); rm -f *cil.c *box.c *.i *_ppp.c *.origi *_all.c

ftpd: defaulttarget mustbegcc
	cd $(FTPDDIR); \
            make CC="$(FTPDSAFECC)" \
                 LD="$(FTPDSAFECC)"







