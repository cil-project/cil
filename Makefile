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
              oneret box markptr \
              rmtmps optim
EXECUTABLE  = $(OBJDIR)/safec
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
MODULES    += cabs cprint combine clexer cparser cabs2cil frontc
endif

# Add main late
MODULES    += main


# Additional things to clean
EXTRACLEAN += $(OBJDIR)/*.obj $(OBJDIR)/*.a $(OBJDIR)/*.o

    # Include now the common set of rules for OCAML
    # This file will add the rules to make $(EXECUTABLE).$(EXE)
include Makefile.ocaml



##### Settings that depend on the computer we are on
##### Make sure the COMPUTERNAME environment variable is set
ifeq ($(COMPUTERNAME), RAW)   # George's workstation
BASEDIR=C:/Necula
TVDIR=$(BASEDIR)/Source/TransVal
CILDIR=$(SAFECCDIR)/cil
SAFECCDIR=$(BASEDIR)/SafeC
PCCDIR=$(SAFECCDIR)/cil/test/PCC
endif
ifeq ($(COMPUTERNAME), FETA) # George's home machine
BASEDIR=C:/Necula
TVDIR=$(BASEDIR)/Source/TransVal
CILDIR=$(SAFECCDIR)/cil
SAFECCDIR=$(BASEDIR)/SafeC
PCCDIR=$(SAFECCDIR)/cil/test/PCC
endif
ifeq ($(COMPUTERNAME), tenshi) # Wes's laptop
BASEDIR=/home/weimer/cvs/
SAFECCDIR=$(BASEDIR)/safeC
PCCDIR=$(BASEDIR)/cil/test/PCC
TVDIR=$(BASEDIR)/TransVal
CILDIR=$(BASEDIR)/cil
_GNUCC=1
endif
ifeq ($(COMPUTERNAME), galvatron) # jlee's 
BASEDIR=/home/jlee/summer
SAFECCDIR=$(BASEDIR)
PCCDIR=$(SAFECCDIR)/cil/test/PCC
TVDIR=$(BASEDIR)/TransVal
CILDIR=$(BASEDIR)/cil
_GNUCC=1
USE_GC=1
endif
ifeq ($(COMPUTERNAME), madroneprime) # jlee on madrone
BASEDIR=/home/jlee/research
SAFECCDIR=$(BASEDIR)
PCCDIR=$(SAFECCDIR)/cil/test/PCC
TVDIR=$(BASEDIR)/TransVal
CILDIR=$(BASEDIR)/cil
_GNUCC=1
USE_GC=1
USER_SCOTT=1
endif
ifeq ($(COMPUTERNAME), madrone) # scott's desktop
BASEDIR=/home/scott/wrk/safec
SAFECCDIR=$(BASEDIR)
PCCDIR=$(SAFECCDIR)/cil/test/PCC
TVDIR=$(BASEDIR)/TransVal
CILDIR=$(BASEDIR)/cil
_GNUCC=1
USE_GC=1
USER_SCOTT=1
endif
ifeq ($(COMPUTERNAME), leetch) # scott's laptop
BASEDIR=/home/scott/wrk/safec
SAFECCDIR=$(BASEDIR)
PCCDIR=$(SAFECCDIR)/cil/test/PCC
TVDIR=$(BASEDIR)/TransVal
CILDIR=$(BASEDIR)/cil
_GNUCC=1
USE_GC=1
USER_SCOTT=1
endif
ifeq ($(COMPUTERNAME), seamonkey) # another for scott
BASEDIR=/home/scott/wrk/safec
SAFECCDIR=$(BASEDIR)
PCCDIR=$(SAFECCDIR)/cil/test/PCC
TVDIR=$(BASEDIR)/TransVal
CILDIR=$(BASEDIR)/cil
_GNUCC=1
USE_GC=1
USER_SCOTT=1
endif
ifeq ($(COMPUTERNAME), brooksie_scott) # scott on brooksie
BASEDIR=/home/smcpeak
SAFECCDIR=$(BASEDIR)
PCCDIR=$(SAFECCDIR)/cil/test/PCC
TVDIR=$(BASEDIR)/TransVal
CILDIR=$(BASEDIR)/cil
_GNUCC=1
USE_GC=1
USER_SCOTT=1
endif
ifeq ($(COMPUTERNAME), fuji) # Rahul's laptop
BASEDIR=/home/sprahul/research
SAFECCDIR=$(BASEDIR)
PCCDIR=$(BASEDIR)/cil/test/PCC
TVDIR=$(BASEDIR)/TransVal
CILDIR=$(BASEDIR)/cil
_GNUCC=1
endif
ifeq ($(COMPUTERNAME), brooksie) # Rahul's desktop
BASEDIR=/home/sprahul/research
SAFECCDIR=$(BASEDIR)
PCCDIR=$(BASEDIR)/cil/test/PCC
TVDIR=$(BASEDIR)/TransVal
CILDIR=$(BASEDIR)/cil
_GNUCC=1
endif
ifeq ($(COMPUTERNAME), brooksie_george) # Rahul's desktop, for George
BASEDIR=/home/necula/Source
SAFECCDIR=$(BASEDIR)
PCCDIR=$(SAFECCDIR)/cil/test/PCC
TVDIR=$(BASEDIR)/TransVal
CILDIR=$(BASEDIR)/cil
_GNUCC=1
endif

ifeq ($(COMPUTERNAME), madrone_danny) # dannys desktop
BASEDIR=/home/dannyant
SAFECCDIR=$(BASEDIR)
PCCDIR=$(SAFECCDIR)/cil/test/PCC
TVDIR=$(BASEDIR)/TransVal
CILDIR=$(BASEDIR)/cil
_GNUCC=1
endif

ifeq ($(COMPUTERNAME), danny_desk) # dannys desktop
BASEDIR=/home/danny/project
SAFECCDIR=$(BASEDIR)
PCCDIR=$(SAFECCDIR)/cil/test/PCC
TVDIR=$(BASEDIR)/TransVal
CILDIR=$(BASEDIR)/cil
_GNUCC=1
endif

ifeq ($(COMPUTERNAME), brooksie_raygto) # Rahul's desktop, for Raymond
BASEDIR=/home/raygto/
SAFECCDIR=$(BASEDIR)
PCCDIR=$(SAFECCDIR)/cil/test/PCC
TVDIR=$(BASEDIR)/TransVal
CILDIR=$(BASEDIR)/cil
_GNUCC=1
endif
ifeq ($(COMPUTERNAME), madrone_amanb) # Aman's *top
ifndef BASEDIR
BASEDIR=/home/amanb/safec
endif
SAFECCDIR=$(BASEDIR)
PCCDIR=$(SAFECCDIR)/cil/test/PCC
TVDIR=$(BASEDIR)/TransVal
CILDIR=$(BASEDIR)/cil
_GNUCC=1
USE_GC=1
endif

# sm: I keep getting bit by this
ifndef BASEDIR
# sm: why doesn't this do what the manual says?
#HMM=$(error "wtf")
HMM="you_have_to_set_the_COMPUTERNAME_environment_variable"
BASEDIR=$(HMM)
SAFECCDIR=$(HMM)
PCCDIR=$(HMM)
TVDIR=$(HMM)
CILDIR=$(HMM)
_GNUCC=$(HMM)
endif


export EXTRAARGS
export INFERBOX
ifndef _GNUCC
_MSVC = 1			# Use the MSVC compiler by default
endif

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
CPPSTART=gcc -E %i -Dx86_LINUX -D_GNUCC -include fixup.h  -I/usr/include/sys
CPPOUT=-o %o
CPP=$(CPPSTART) $(CPPOUT)
INC=-I
PATCHFILE=safec_gcc.patch
# sm: disable patching for now ('true' has no output)
# (set it to 'echo' to re-enable)
ifndef PATCHECHO
  PATCHECHO=echo
endif
endif


ifdef _MSVC
DEBUGCCL=cl /TC /O0 /Zi /MLd /I./lib /DEBUG
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
CPPSTART=cl /Dx86_WIN32 /D_MSVC /E /TC /I./lib /FI fixup.h /DBEFOREBOX
CPPOUT= %i >%o
CPP=$(CPPSTART) $(CPPOUT)
SAFECC += --safec=-msvc
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
SAFECLIB=obj/safec.$(LIBEXT)
CILLIB=obj/cillib.$(LIBEXT)
else
SAFECLIB=obj/safecdebug.$(LIBEXT)
CILLIB=obj/cillibdebug.$(LIBEXT)
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

.PHONY: trval
trval: 
	make -C $(TVDIR)
	make -C $(TVDIR) RELEASE=1




SAFECC=perl $(CILDIR)/lib/safecc.pl


# sm: my options
ifdef USER_SCOTT
  # I like -g always
  SAFECC+= -g

  # currently the #line directives are inaccurate, so
  # they are counterproductive
  SAFECC+= --safec=-noPrintLn
endif


ifdef PROFILE
SAFECC+= --profile 
endif

# weimer: support for other solvers
ifeq ($(INFERBOX), 1)
    SAFECC+= --box --safec=-solver --safec=first
endif
ifeq ($(INFERBOX), 2)
    SAFECC+= --box --safec=-solver --safec=second
endif
ifeq ($(INFERBOX), 3)
    SAFECC+= --box --safec=-solver --safec=third
endif
ifeq ($(INFERBOX), 4)
    SAFECC+= --box --safec=-solver --safec=fourth
endif
ifeq ($(INFERBOX), wild)
    SAFECC+= --box --safec=-solver --safec=wild
endif
ifeq ($(INFERBOX), wildsafe)
    SAFECC+= --box --safec=-solver --safec=wildsafe
endif
ifeq ($(TABLE), A)
    SAFECC+= --safec=-tableAll
endif
ifeq ($(TABLE), I)
    SAFECC+= --safec=-tableInterface
endif
ifdef NOLINES
    SAFECC+= --safec=-noPrintLn
endif
ifdef COMMLINES
    SAFECC+= --safec=-commPrintLn
endif

ifdef SHOWCABS
SAFECC+= --cabs
endif
ifdef SHOWCIL
SAFECC+= --cil
endif	
ifdef INFERBOX
SAFECC+= $(DEF)INFERBOX
SAFECC+= --inferbox --box
else
ifndef MANUALBOX
SAFECC+= --safec=-boxdefaultwild
endif
endif
ifdef MANUALBOX
SAFECC+= $(DEF)MANUALBOX
endif
ifdef NO_TAGS
SAFECC+= $(DEF)NO_TAGS
endif
ifdef CHECK
SAFECC += --safec=-check
endif
ifdef RELEASE
SAFECC+= --release
endif
ifdef TV
SAFECC+= --tv="$(TV)"
TVEXE=trval
endif
# sm: pass tracing directives on 'make' command line like TRACE=usedVars
ifdef TRACE
SAFECC+= --tr="$(TRACE)"
endif

ifdef OPTIM
SAFECC += --optim
endif

# sm: can't figure out why passing this via EXTRAARGS screws
# up other things (e.g. -DMANUALBOX)
# update: reason was we were modifying EXTRAARGS in here -- we
# should only set EXTRAARGS when invoking the Makefile, and
# in here just use SAFECC+= ...
ifdef LOGCALLS
SAFECC+= --safec=-logcalls
endif

# when this is turned on, it should disable any source changes we've
# made that are purely in the interest of performance
ifdef NO_PERF_CHANGES
SAFECC+= $(DEF)NO_PERF_CHANGES
endif

# sm: user-specific configuration; the leading '-' means it's ok
# if this file doesn't exist; this file is *not* checked in to
# the CVS repository (please be careful to avoid putting things
# in here which will cause things to break when it's missing)
-include site-config.mk


# ----------- above here is configuration -------------------
# ----------- below here are rules to build the translator ---------
# (actually, mostly they're in the MODULES line above and in Makefile.ocaml)


# garbage collector options
ifneq ($(COMPUTERNAME), RAW)   # George's workstation
ifneq ($(COMPUTERNAME), FETA)   # George's workstation
ifdef _GNUCC
  ifndef NO_GC
  #ifdef USE_GC
    # enable the garbage collector by default for gcc
    SAFECC+= $(DEF)USE_GC
    DEBUGCCL+= $(DEF)USE_GC
    RELEASECCL+= $(DEF)USE_GC
    GCLIB = $(CILDIR)/lib/gc/gc.a

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

SAFECC+= $(EXTRAARGS)

###
###
###    # Now the rules to make the library
###
###
ifdef _MSVC
ifndef RELEASE
SAFECLIBARG=$(DEF)_DEBUG
endif

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
endif

# Libraries on GCC
# sm: if GC is enabled, we just add it to the runtime library
# (or rather, we add safec.o to gc.a's contents)
ifdef _GNUCC
$(SAFECLIB) : lib/safec.c $(GCLIB) lib/splay.o
	$(CC) $(OBJOUT)obj/safec.o $<
	if echo $(GCLIB) | grep / >/dev/null; then \
		cp -f $(GCLIB) $@; echo "using GC"; \
	else \
		rm -f $@; echo "not using GC"; \
	fi
	ar -r $@ obj/safec.o lib/splay.o
	ranlib $@

$(CILLIB) : lib/cillib.c
	$(CC) $(OBJOUT)obj/cillib.o $<
	ar -r $@ obj/cillib.o
	ranlib $@

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
		-name '*.i' -o \
		-name '*_ppp.c' -o \
		-name '*.origi' -o \
		-name '*.o' -o \
		-name '*.cabs' -o \
		-name '*infer.c' -o \
		-name '*_all*.c' \
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
	cd $(CILDIR)/test/PCCout; $(SAFECC) --keep=. $(DEF)$(ARCHOS) \
                  $(DEF)$(PCCTYPE) $(CONLY) \
                  $(PCCDIR)/src/$*.c \
                  $(OBJOUT)$(notdir $*).o



ifdef _MSVC
MSLINK=--mode=mscl
endif
PCCSAFECC=$(SAFECC) $(DEF)CCURED \
                    --patch=$(SAFECCDIR)/cil/lib/$(PATCHFILE) --combine \
                    --keep=$(CILDIR)/test/PCCout \
                    --nobox=pccbox --nobox=alloc
pcc : defaulttarget
#	-rm $(PCCDIR)/$(ARCHOS)$(PCCCOMP)/$(PCCTYPE)/*.o
	-rm $(PCCDIR)/$(ARCHOS)$(PCCCOMP)/$(PCCTYPE)/*.exe
	-rm $(PCCDIR)/bin/*.exe
	make -C $(PCCDIR) \
             CC="$(PCCSAFECC) $(CONLY)" \
             LD="$(SAFECC) $(MSLINK) --combine --keep=$(CILDIR)/test/PCCout" \
             USE_JAVA=1 USE_JUMPTABLE=1 TYPE=$(PCCTYPE) \
             COMPILER=$(PCCCOMP) \
	     clean defaulttarget 

pcc-noclean : defaulttarget
#	-rm $(PCCDIR)/$(ARCHOS)$(PCCCOMP)/$(PCCTYPE)/*.o
	-rm $(PCCDIR)/$(ARCHOS)$(PCCCOMP)/$(PCCTYPE)/*.exe
	-rm $(PCCDIR)/bin/*.exe
	make -C $(PCCDIR) \
             CC="$(PCCSAFECC) $(CONLY)" \
             LD="$(SAFECC) $(MSLINK) --combine --keep=$(CILDIR)/test/PCCout" \
             USE_JAVA=1 USE_JUMPTABLE=1 TYPE=$(PCCTYPE) \
             COMPILER=$(PCCCOMP) \
	     defaulttarget 

pcc-combined: defaulttarget
	cd $(PCCDIR)/bin; \
          $(SAFECC) engine.$(ARCHOS)$(PCCCOMP).$(PCCTYPE).exe_all.c \
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
	cd $(SMALL1); $(SAFECC)   \
               --patch=../../lib/$(PATCHFILE) \
	       $(CONLY) $(DOOPT) $(ASMONLY)$*.s $*.c 

testnopatch/% : $(SMALL1)/%.c defaulttarget
	cd $(SMALL1); $(SAFECC)   \
               $(CONLY) $(DOOPT) $(ASMONLY)$*.s $*.c 

testexe/% : $(SMALL1)/%.c  defaulttarget
	cd $(SMALL1); $(SAFECC)   \
               --patch=../../lib/$(PATCHFILE) \
	       $(DOOPT) $(EXEOUT)$*.exe $*.c 


testrun/% : $(SMALL1)/%.c  defaulttarget
	cd $(SMALL1); $(SAFECC)   \
               --patch=../../lib/$(PATCHFILE) \
	       $(DOOPT) $(EXEOUT)$*.exe $*.c
	cd $(SMALL1); ./$*.exe

combine%_3: defaulttarget
	cd $(SMALL1); \
          $(SAFECC) $(DOOPT) \
                    combine$*_1.c combine$*_2.c combine$*_3.c \
                    --combine  \
                    --patch=../../lib/$(PATCHFILE) \
	            $(EXEOUT)combine$*.exe
	cd $(SMALL1); ./combine$*.exe

# weimer: test, compile and run
testc/% : $(SMALL1)/%.c  defaulttarget
	cd $(SMALL1); $(SAFECC)   \
               --patch=../../lib/$(PATCHFILE) \
	       $(DOOPT) $(EXEOUT)$*.exe $*.c ; ./$*.exe

# Aman's optim tests
OPTIMTESTDIR=test/optim
optim/% : $(OPTIMTESTDIR)/%.c defaulttarget
	cd $(OPTIMTESTDIR); $(SAFECC)   \
               --patch=../../lib/$(PATCHFILE) \
	       $(DOOPT) $*.c $(EXEOUT)$*.exe
	$(OPTIMTESTDIR)/$*.exe



hashtest: test/small2/hashtest.c defaulttarget
	rm -f $(PCCTEST)/hashtest.exe
	cd $(PCCTEST); $(SAFECC) --combine \
                                 --keep=. $(DEF)$(ARCHOS) $(DEF)$(PCCTYPE) \
                 $(DOOPT) \
                 `$(PATCHECHO) --patch=../../lib/$(PATCHFILE)` \
                 $(INC)$(PCCDIR)/src \
                 $(PCCDIR)/src/hash.c \
                 ../small2/hashtest.c \
                 $(EXEOUT)hashtest.exe
	$(PCCTEST)/hashtest.exe


rbtest: test/small2/rbtest.c defaulttarget
	rm -f $(PCCTEST)/rbtest.exe
	@true "compile with gcc for better error diagnostics (ha!)"
	cd $(PCCTEST); $(SAFECC) --combine \
                                 --keep=. $(DEF)$(ARCHOS) $(DEF)$(PCCTYPE) \
                 `$(PATCHECHO) --patch=../../lib/$(PATCHFILE)` \
                 $(DOOPT) \
                 $(INC)$(PCCDIR)/src \
                 $(PCCDIR)/src/redblack.c \
                 ../small2/rbtest.c \
                 $(EXEOUT)rbtest.exe
	$(PCCTEST)/rbtest.exe letGcFree

btreetest: test/small2/testbtree.c \
           test/small2/btree.c defaulttarget
	rm -f test/small2/btreetest.exe
	cd test/small2; $(SAFECC) --combine --keep=. \
                 $(DOOPT) \
                 --patch=../../lib/$(PATCHFILE) \
                 btree.c testbtree.c \
                 $(EXEOUT)btreetest.exe
	test/small2/btreetest.exe


# sm: this is my little test program
hola: scott/hola

# sm: attempt at a single rule for my testing purposes
scott/%: test/small2/%.c defaulttarget
	rm -f test/small2/$*
	cd test/small2; $(CC) $(CONLY) $(WARNALL) $(DEF)$(ARCHOS) $*.c
	cd test/small2; $(SAFECC) --verbose --keep=. $(DEF)$(ARCHOS) \
                 `$(PATCHECHO) --patch=../../lib/$(PATCHFILE)` \
                 $(DOOPT) `true $(WARNALL)` $(NOPRINTLN) \
                 $*.c \
                 $(EXEOUT)$*
	test/small2/$*

scott-nolink/%: test/small2/%.c defaulttarget
	rm -f test/small2/$*
	cd test/small2; $(CC) $(CONLY) $(WARNALL) $(DEF)$(ARCHOS) $*.c
	cd test/small2; $(SAFECC) $(CONLY) --verbose --keep=. $(DEF)$(ARCHOS) \
                 `$(PATCHECHO) --patch=../../lib/$(PATCHFILE)` \
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
	cd test/bad; $(SAFECC) --verbose --keep=. $(DEF)$(ARCHOS) \
                 `$(PATCHECHO) --patch=../../lib/$(PATCHFILE)` \
                 $(DOOPT) $(WARNALL) $(NOPRINTLN) \
                 $*.c \
                 $(EXEOUT)$*
	if test/bad/$*; then \
		echo "(worked as expected, when FAIL not defined)"; exit 0; \
	else \
		echo "That should have worked; FAIL was not defined!"; exit 2; \
	fi
	@true "now try the failure case"
	cd test/bad; $(SAFECC) --verbose --keep=. $(DEF)$(ARCHOS) \
                 `$(PATCHECHO) --patch=../../lib/$(PATCHFILE)` \
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
	cd test/small2; $(SAFECC) --verbose --keep=. $(DEF)$(ARCHOS) \
                 `$(PATCHECHO) --patch=../../lib/$(PATCHFILE)` \
                 $(DOOPT) $(WARNALL) $(NOPRINTLN) \
                 $*.c \
                 $(EXEOUT)$*
	if test/small2/$*; then \
		echo "(worked as expected, when FAIL not defined)"; exit 0; \
	else \
		echo "That should have worked; FAIL was not defined!"; exit 2; \
	fi
	@true "now try the failure case"
	cd test/small2; $(SAFECC) --verbose --keep=. $(DEF)$(ARCHOS) \
                 `$(PATCHECHO) --patch=../../lib/$(PATCHFILE)` \
                 $(DOOPT) $(WARNALL) $(NOPRINTLN) -DFAIL \
                 $*.c \
                 $(EXEOUT)$*
	if test/small2/$*; then \
		echo "That should have failed!"; exit 2; \
	else \
		echo "(failed as expected)"; exit 0; \
	fi



# sm: trivial test of combiner
MYSAFECC = $(SAFECC) --keep=. $(DEF)$(ARCHOS) --patch=$(SAFECCDIR)/cil/lib/$(PATCHFILE)
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
CFRACDIR = $(CILDIR)/../bench/cfrac
cfrac: defaulttarget
	-rm $(CFRACDIR)/*.o
	-rm $(CFRACDIR)/cfrac
	make -C $(CFRACDIR) \
	  CC="$(SAFECC) --keep=$(CFRACDIR)" \
	  LD="$(SAFECC) --keep=$(CFRACDIR)"
	csh -c "time $(CFRACDIR)/cfrac 327905606740421458831903"

comcfrac: defaulttarget
	-rm $(CFRACDIR)/*.o
	-rm $(CFRACDIR)/cfrac
	make -C $(CFRACDIR) \
	  CC="$(SAFECC) --combine --keep=$(CFRACDIR)" \
	  LD="$(SAFECC) --combine --keep=$(CFRACDIR)"
	csh -c "time $(CFRACDIR)/cfrac 327905606740421458831903"

# espresso: memory benchmark that does logic minimization
ESPRESSODIR = $(CILDIR)/../bench/espresso
espresso: defaulttarget
	@true -rm $(ESPRESSODIR)/*.o
	@true -rm $(ESPRESSODIR)/espresso
	make -C $(ESPRESSODIR) \
	  CC="$(SAFECC) --keep=$(ESPRESSODIR)" \
	  LD="$(SAFECC) --keep=$(ESPRESSODIR)"
	csh -c "time $(ESPRESSODIR)/espresso -t $(ESPRESSODIR)INPUT/Z5xp1.espresso >/dev/null"




HUFFCOMPILE=$(SAFECC) $(DEF)NOVARARG --combine --keep=. 
# HUFFCOMPILE=cl /MLd
ifdef _GNUCC
HUFFOTHERS += -lm
endif
ifndef HUFFINPUT
  HUFFINPUT=$(CILDIR)/src/frontc/cparser.output
endif
hufftest: test/small2/hufftest.c defaulttarget
	rm -f $(PCCTEST)/hufftest.exe \
              $(PCCTEST)/huffman.compressed \
              $(PCCTEST)/huffman.code \
              $(PCCTEST)/huffman.freq
	cd $(PCCTEST); $(HUFFCOMPILE) \
                 $(DEF)$(ARCHOS) $(DEF)$(PCCTYPE) $(DEF)$(PCCCOMP) \
                 $(DOOPT) \
                 --patch=../../lib/$(PATCHFILE) \
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
	cd $(PCCTEST); $(SAFECC) --keep=. $(DEF)$(ARCHOS) $(DEF)$(PCCTYPE) \
                 $(DOOPT) \
                 --patch=../../lib/$(PATCHFILE) \
                 $(INC)$(PCCDIR)/src \
                 ../small2/wes-rbtest.c \
                 $(EXEOUT)wes-rbtest.exe
	$(PCCTEST)/wes-rbtest.exe

wes-hashtest: test/small2/wes-hashtest.c defaulttarget
	rm -f $(PCCTEST)/wes-hashtest.exe
	cd $(PCCTEST); $(SAFECC) --keep=. $(DEF)$(ARCHOS) $(DEF)$(PCCTYPE) \
                 $(DOOPT) \
                 --patch=../../lib/$(PATCHFILE) \
                 $(INC)$(PCCDIR)/src \
                 ../small2/wes-hashtest.c \
                 $(EXEOUT)wes-hashtest.exe
	$(PCCTEST)/wes-hashtest.exe


### Generic test
testfile/% : defaulttarget
	$(SAFECC) /TC $*

testdir/% : defaulttarget
	make -C CC="perl safecc.pl" $*


################## Linux device drivers
testlinux/% : test/linux/%.cpp defaulttarget
	cd test/linux; $(SAFECC) -o $*.o $*.cpp 

testqp : testlinux/qpmouse
testserial: testlinux/generic_serial

################## Rahul's test cases
SPR-TESTDIR = test/spr
spr/% : defaulttarget
	cd $(SPR-TESTDIR); $(SAFECC) $*.c $(CONLY) $(DOOPT) $(ASMONLY)$*.s


################# Apache test cases
APACHETEST=test/apache
APACHEBASE=apache_1.3.19/src
APATCH=--patch=$(SAFECCDIR)/cil/lib/$(PATCHFILE) --patch=apache.patch 
ifdef _MSVC
APACHECFLAGS=/nologo /MDd /W3 /GX /Zi /Od \
         $(INC)"$(APACHEBASE)\include" $(INC)"$(APACHEBASE)\os\win32" \
         $(DEF)"_DEBUG" $(DEF)"WIN32" $(DEF)"_WINDOWS" \
         $(DEF)"NO_DBM_REWRITEMAP" $(DEF)"SHARED_MODULE" \
         $(DEF)"WIN32_LEAN_AND_MEAN"
APATCH += --patch=apache_msvc.patch
else
APACHECFLAGS=-Wall -D_GNUCC -g \
         $(INC)"$(APACHEBASE)/include" $(INC)"$(APACHEBASE)/os/unix" \
         $(DEF)"_DEBUG" \
         $(DEF)"NO_DBM_REWRITEMAP" $(DEF)"SHARED_MODULE"
APATCH += --patch=apache_gcc.patch
endif

apache/urlcount : defaulttarget
	rm -f $(APACHETEST)/mod_urlcount.$(OBJ)
	cd $(APACHETEST); $(SAFECC) \
                       --keep=. $(APATCH) \
                        $(DOOPT) \
                        $(APACHECFLAGS) \
                        $(CONLY) $(OBJOUT)./mod_urlcount.$(OBJ) \
                        mod_urlcount.c

apache/layout : defaulttarget
	rm -f $(APACHETEST)/mod_layout.$(OBJ)
	cd $(APACHETEST); $(SAFECC) \
                       --keep=. $(APATCH) \
                        $(DOOPT) \
                        $(APACHECFLAGS) \
                        $(CONLY) $(OBJOUT)./mod_layout.$(OBJ) \
                        mod_layout.c

apache/random : defaulttarget
	rm -f $(APACHETEST)/mod_random.$(OBJ)
	cd $(APACHETEST); $(SAFECC) \
                       --keep=. $(APATCH) \
                        $(DOOPT) \
                        $(APACHECFLAGS) \
                        $(CONLY) $(OBJOUT)./mod_random.$(OBJ) \
                        mod_random.c

apache/gzip : defaulttarget
	rm -f $(APACHETEST)/mod_gzip.$(OBJ)
	cd $(APACHETEST); $(SAFECC) \
                       --keep=. $(APATCH) \
                        $(DOOPT) \
                        $(APACHECFLAGS) \
                        $(CONLY) $(OBJOUT)./mod_gzip.$(OBJ) \
                        mod_gzip.c

apache/t : defaulttarget
	rm -f $(APACHETEST)/t.obj
	cd $(APACHETEST); $(SAFECC) \
                       --keep=. $(APATCH) \
                        $(APACHECFLAGS) \
                        $(OBJOUT)./t.obj \
                        t.c

apache/rewrite: defaulttarget
	rm -f $(APACHETEST)/mod_gzip.$(OBJ)
	cd $(APACHETEST); $(SAFECC) \
                       --keep=. $(APATCH) \
                        $(DOOPT) \
                        $(APACHECFLAGS) \
                        $(OBJOUT)./mod_rewrite.$(OBJ) \
                        $(APACHEBASE)/modules/standard/mod_rewrite.c





# sm: removed DOOPT since I want to specify optimization in the
# benchmark's Makefile (helps to ensure consistency between the
# non-ccured build and the ccured build, and also some programs
# take too long on -O3)
COMBINESAFECC = $(SAFECC) --combine

#
# OLDEN benchmarks
#
# Barnes-Hut
BHDIR=test/olden/bh
bh: defaulttarget mustbegcc
	cd $(BHDIR); rm -f code.exe *.o; \
               make CC="$(COMBINESAFECC) --nobox=bhbox \
			--patch=$(SAFECCDIR)/cil/lib/$(PATCHFILE)"
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
	cd $(BHDIR); sh -c "time ./code < data.in > data.out"
	@true "sm: added next line to compare output to expected output"
#	cd $(BHDIR); sh -c "perl normalize.pl < data.out | diff data.cil.out - | head"


# Power pricing
PWDIR=test/olden/power
ifdef _GNUCC
PWEXTRA += -lm
endif
power: defaulttarget mustbegcc
	cd $(PWDIR); \
               make PLAIN=1 clean defaulttarget \
                    CC="$(COMBINESAFECC) \
			--patch=$(SAFECCDIR)/cil/lib/$(PATCHFILE)"
	cd $(PWDIR); sh -c "time ./power.exe"

power-combined : defaulttarget mustbegcc
	cd $(PWDIR); \
             $(SAFECC) power.exe_all.c $(EXEOUT)power.exe

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
                        --nobox=trusted_health \
			--patch=$(SAFECCDIR)/cil/lib/$(PATCHFILE)"
	cd $(HEALTHDIR); sh -c "time ./health.exe 5 500 1 1"



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
			--patch=$(SAFECCDIR)/cil/lib/$(PATCHFILE)"
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
                        --nobox=trusted_voronoi \
			--patch=$(SAFECCDIR)/cil/lib/$(PATCHFILE)"
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
			--patch=$(SAFECCDIR)/cil/lib/$(PATCHFILE)"
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
                        --nobox=trusted_bisort \
			--patch=$(SAFECCDIR)/cil/lib/$(PATCHFILE)"
	cd $(BISORTDIR); sh -c "time ./bisort.exe 100 1"




OLDENMSTDIR=test/olden/mst
OLDENMSTSAFECC=$(COMBINESAFECC) \
                  --nobox=trusted_mst
	          --patch=$(SAFECCDIR)/cil/lib/$(PATCHFILE)
ifdef _MSVC
OLDENMSTSAFECC += $(DEF)WIN32 $(DEF)MSDOS
MSTARGS= _MSVC=1
endif
mst-clean: 	
	cd $(OLDENMSTDIR); make clean
	cd $(OLDENMSTDIR); rm -f *cil.c *box.c *.i *_ppp.c *.origi *_all.c

mst: defaulttarget
	cd $(OLDENMSTDIR); \
            make clean mst.exe $(MSTARGS) \
                               CC="$(OLDENMSTSAFECC)" \
                               LD="$(OLDENMSTSAFECC)"
	cd $(OLDENMSTDIR); sh -c "time ./mst.exe 1024 1"




TREEADDIR=test/olden/treeadd
TREEADDSAFECC=$(SAFECC) --combine --keep=safeccout  \
                  --nobox=ta_trusted \
                  --patch=$(SAFECCDIR)/cil/lib/$(PATCHFILE) \
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
	cd $(TREEADDIR); sh -c "time ./treeadd.exe 21 1"

NEWBISORTDIR=test/olden/newbisort
NEWBISORTSAFECC=$(SAFECC) --combine --keep=safeccout  \
                   --nobox=ta_trusted \
                  --patch=$(SAFECCDIR)/cil/lib/$(PATCHFILE) \
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
EM3DDSAFECC=$(SAFECC) --combine --keep=safeccout  \
                  --patch=$(SAFECCDIR)/cil/lib/$(PATCHFILE) \
                  --nobox=trusted_em3d \
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
	cd $(COMPRESSDIR)/src ; $(SAFECC) --keep=. $(DEF)$(ARCHOS) $(DEF)$(PCCTYPE) \
                 $(DOOPT) \
                 combine-compress.c \
                 $(EXEOUT)combine-compress.exe
	cd $(COMPRESSDIR)/src; sh -c "time ./combine-compress.exe < input.data > combine-compress.out"

compress-noclean: defaulttarget mustbegcc
	cd $(COMPRESSDIR)/src; make CC="$(COMBINESAFECC)" build
	cd $(COMPRESSDIR)/src; sh -c "time ./compress < input.data > combine-compress.out"

# sm: removed this because it's now just a cvs'd file: 
#   echo "1400000 q 2231" >$(COMPRESSDIR)/exe/base/input.data 
compress: defaulttarget mustbegcc
	cd $(COMPRESSDIR)/src; \
               make CC="$(COMBINESAFECC) --patch=$(SAFECCDIR)/cil/lib/$(PATCHFILE)" clean build
	cd $(COMPRESSDIR)/src; sh -c "time ./compress < input.data > combine-compress.out"

LIDIR=$(SPECDIR)/130.li
LISAFECC=$(SAFECC) --combine --patch=$(SAFECCDIR)/cil/lib/$(PATCHFILE) \
                   --keep=safeccout
li: defaulttarget mustbegcc
	cd $(LIDIR)/src; \
            make clean build CC="$(LISAFECC) $(CONLY)" \
                             LD="$(LISAFECC)"
	sh -c "time $(LIDIR)/src/trial_li \
            <$(LIDIR)/data/train/input/train.lsp \
            >$(LIDIR)/data/train/input/train.out"

li-combined: defaulttarget mustbegcc
	cd $(LIDIR)/src; \
            $(SAFECC) trial_li_all.c $(LIEXTRA) $(EXEOUT)trial_li_all.exe

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
	cd $(LIDIR)/src ; $(SAFECC) --keep=. $(DEF)$(ARCHOS) $(DEF)$(PCCTYPE) \
                 $(DOOPT) \
                 trial_li.c \
                 $(EXEOUT)trial_li.exe


### SPEC95 GO
GODIR=$(SPECDIR)/099.go
GOSAFECC=$(SAFECC) --combine  --patch=$(SAFECCDIR)/cil/lib/$(PATCHFILE) \
                   --keep=safeccout $(NOPRINTLN) $(OPT_O2)

goclean: 	
	cd $(GODIR)/src; make clean
	cd $(GODIR)/src; rm -f *cil.c *box.c *.i *_ppp.c *.origi


go: defaulttarget mustbegcc
	cd $(GODIR)/src; \
            make clean build CC="$(GOSAFECC) $(CONLY)" \
                             LD="$(GOSAFECC)"
	$(GODIR)/src/go 50 9

go-combined: defaulttarget mustbegcc
	cd $(GODIR)/src; \
	   $(SAFECC) $(CONLY) go_all.c


go-noclean: defaulttarget mustbegcc
	cd $(GODIR)/src; \
            make build CC="$(GOSAFECC) $(CONLY)" \
                       LD="$(GOSAFECC)" \
                             EXTRA_LIBS=$(GOEXTRA) 
	sh -c "time $(GODIR)/src/go 50 9"



### SPEC95 vortex
VORDIR=$(SPECDIR)/147.vortex
VORSAFECC=$(SAFECC) --combine   --patch=$(SAFECCDIR)/cil/lib/$(PATCHFILE)
#VORSAFECC=$(SAFECC)  --patch=$(SAFECCDIR)/cil/lib/$(PATCHFILE)
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
            make clean build CC="$(SAFECC) --mode=gcc --cabs $(CONLY)" \
                             LD="gcc"
	cd $(VORDIR)/src; sh -c "./testit vortex.exe"

vortex-cil:  defaulttarget mustbegcc
	cd $(VORDIR)/src; \
            make clean build CC="$(SAFECC) --cil $(CONLY)" \
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
            $(SAFECC) vortex_all.c -g $(VOREXTRA) $(EXEOUT)vortex.exe
	cd $(VORDIR)/src; sh -c "./testit vortex.exe"

vortex-combined-gcc: mustbegcc
	cd $(VORDIR)/src; \
            gcc vortex_all.c -g \
               $(SAFECCDIR)/cil/obj/cillibdebug.a $(VOREXTRA) $(EXEOUT)vortex.exe
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
M88SAFECC=$(SAFECC) --combine --keep=safeccout \
                    --patch=$(SAFECCDIR)/cil/lib/$(PATCHFILE) \
                    --nobox=m88k_trusted
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
            $(SAFECC) m88k_all.c $(CONLY)

### SPEC95 ijpeg
IJPEGDIR=$(SPECDIR)/132.ijpeg
IJPEGSAFECC=$(SAFECC) --combine --keep=safeccout  \
                  --patch=$(SAFECCDIR)/cil/lib/$(PATCHFILE) \
                  --nobox=ijpeg_trusted $(NOPRINTLN)
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
            -image_file $(IJPEGDIR)/data/ref/input/penguin.ppm \
            -GO"

ijpeg-combined: defaulttarget mustbegcc
	cd $(IJPEGDIR)/src; \
            $(SAFECC) ijpeg_all.c $(IJPEGEXTRA) \
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
GCCSAFECC=$(SAFECC) --combine --keep=safeccout \
                    --patch=$(SAFECCDIR)/cil/lib/$(PATCHFILE)


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
            $(SAFECC) cc1.v8_all.c $(GCCEXTRA) \
                $(EXEOUT)cc1.v8.exe

allcc1: defaulttarget mustbegcc
	cd $(GCCDIR)/exe/base; $(SAFECC) $(DOOPT) cc1.v8_all.c


#
# Linux
LINUXDIR=/home/project/linux-2.2.9

linuxstandard: 
	$(MAKE) -C $(LINUXDIR) clean vmlinux \
              MY-CC="gcc"

LINUXCC=perl $(CILDIR)/lib/safecc.pl --mode=gcc
ifdef NOLINES
LINUXCC+= --safec=-noPrintLn
endif
ifdef COMMLINES
LINUXCC+= --safec=-commPrintLn
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
	cd test/small1; $(SAFECC) --combine /Fet.exe t.c t1.c

obj/prettytest.exe: src/pretty.mli src/pretty.ml src/prettytest.ml
	$(CAMLC) -I src -o obj/prettytest.exe src/pretty.mli src/pretty.ml src/prettytest.ml

prettytest:  obj/prettytest.exe
	time obj/prettytest.exe ; echo



### ftpd-BSD-0.3.2-5
FTPDDIR=test/ftpd/ftpd
FTPDSAFECC=$(SAFECC) --combine --keep=safeccout  \
                  --patch=$(SAFECCDIR)/cil/lib/$(PATCHFILE) \
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


