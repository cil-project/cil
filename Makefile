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
MODULES     = pretty trace errormsg stats util cil check ptrnode \
              solveutil simplesolve secondsolve thirdsolve solver globinit \
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
BASEDIR=/home/amanb/safec
SAFECCDIR=$(BASEDIR)
PCCDIR=$(SAFECCDIR)/cil/test/PCC
TVDIR=$(BASEDIR)/TransVal
CILDIR=$(BASEDIR)/cil
_GNUCC=1
USE_GC=1
endif

# sm: I keep getting bit by this
ifndef COMPUTERNAME
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
export BOX
ifndef _GNUCC
_MSVC = 1			# Use the MSVC compiler by default
endif

ifdef _GNUCC
DEBUGCCL=gcc -Wall -x c -g -ggdb -D_GNUCC 
RELEASECCL=gcc -x c -O3 -fomit-frame-pointer -D_RELEASE -D_GNUCC -Wall 
#LIB=lib
#LIBOUT=-o
ifdef RELEASE
DOOPT=-O3
else
DOOPT=-g
endif
CONLY=-c
OBJOUT=-o
OBJ=o
LIBEXT=a
EXEOUT=-o
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
DEF=/D
ASMONLY=/Fa
INC=/I
CPPSTART=cl /Dx86_WIN32 /D_MSVC /E /TC /I./lib /FI fixup.h /DBEFOREBOX
CPPOUT= %i >%o
CPP=$(CPPSTART) $(CPPOUT)
EXTRAARGS += --safec=-msvc
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
else
SAFECLIB=obj/safecdebug.$(LIBEXT)
endif
SAFEMAINLIB=obj/safecmain.$(LIBEXT)


# By default take manual box definitions into consideration
ifdef INFERBOX
MANUALBOX=1
endif

######################
.PHONY : defaulttarget
ifdef NOREMAKE
defaulttarget : 
else
defaulttarget : $(EXECUTABLE)$(EXE) $(SAFECLIB) $(SAFEMAINLIB)
endif

.PHONY: trval
trval: $(TVDIR)/obj/transval.asm.exe
	make -C $(TVDIR) RELEASE=1




SAFECC=perl $(CILDIR)/lib/safecc.pl

# sm: I like -g always
ifdef USER_SCOTT
  SAFECC+= -g
endif

ifdef PROFILE
SAFECC+= --profile 
endif

# weimer: support for other solvers
ifeq ($(INFERBOX), 1)
    SAFECC+= --safec=-solver --safec=first
endif
ifeq ($(INFERBOX), 2)
    SAFECC+= --safec=-solver --safec=second
endif
ifeq ($(INFERBOX), 3)
    SAFECC+= --safec=-solver --safec=third
endif
ifeq ($(INFERBOX), 4)
    SAFECC+= --safec=-solver --safec=fourth
endif
ifeq ($(INFERBOX), wild)
    SAFECC+= --safec=-solver --safec=wild
endif
ifeq ($(INFERBOX), wildsafe)
    SAFECC+= --safec=-solver --safec=wildsafe
endif
ifeq ($(TABLE), A)
    SAFECC+= --safec=-tableAll
endif
ifeq ($(TABLE), I)
    SAFECC+= --safec=-tableInterface
endif

ifdef INFERBOX
BOX=1
endif


ifdef SHOWCABS
SAFECC+= --cabs
endif
ifdef SHOWCIL
SAFECC+= --cil
endif	
ifdef BOX
SAFECC+= --box
endif
ifdef INFERBOX
EXTRAARGS+= $(DEF)INFERBOX
SAFECC+= --inferbox --box
else
ifndef MANUALBOX
SAFECC+= --safec=-boxdefaultwild
endif
endif
ifdef MANUALBOX
EXTRAARGS+= $(DEF)MANUALBOX
endif
ifdef NO_TAGS
SAFECC+= $(DEF)NO_TAGS
endif
ifdef CHECK
EXTRAARGS += --safec=-check
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


# sm: user-specific configuration; the leading '-' means it's ok
# if this file doesn't exist; this file is *not* checked in to
# the CVS repository (please be careful to avoid putting things
# in here which will cause things to break when it's missing)
-include site-config.mk


# ----------- above here is configuration -------------------
# ----------- below here are rules to build targets ---------


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

$(SAFEMAINLIB) : lib/safecmain.c lib/safec.h lib/safeccheck.h
	cl $(DOOPT) /I./lib /c $(DEF)_MSVC $(OBJOUT)obj/safecmain.o $<
	lib /OUT:$@ obj/safecmain.o 
endif

# Libraries on GCC
# sm: if GC is enabled, we just add it to the runtime library
# (or rather, we add safec.o to gc.a's contents)
ifdef _GNUCC
$(SAFECLIB) : lib/safec.c $(GCLIB) lib/splay.o
	$(CC) $(OBJOUT)obj/safec.o $<
	if echo $(GCLIB) | grep / >/dev/null; then \
		cp -f $(GCLIB) $@; \
	else \
		rm -f $@; \
	fi
	ar -r $@ obj/safec.o lib/splay.o

$(SAFEMAINLIB) : lib/safecmain.c lib/safec.h lib/safeccheck.h
	$(CC) $(OBJOUT)obj/safecmain.o $<
	ar -r $@ obj/safecmain.o
endif


mustbegcc :
ifndef _GNUCC
	@echo This test case works only with _GNUCC=1; exit 3
endif

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
MSLINK=--mode=mslink
endif
pcc : defaulttarget
	-rm $(PCCDIR)/$(ARCHOS)$(PCCCOMP)/$(PCCTYPE)/*.o
	-rm $(PCCDIR)/$(ARCHOS)$(PCCCOMP)/$(PCCTYPE)/*.exe
	make -C $(PCCDIR) \
             CC="$(SAFECC) --combine --keep=$(CILDIR)/test/PCCout $(CONLY)" \
             LD="$(SAFECC) $(MSLINK) --combine --keep=$(CILDIR)/test/PCCout" \
             USE_JAVA=1 USE_JUMPTABLE=1 TYPE=$(PCCTYPE) \
             COMPILER=$(PCCCOMP) \
             ENGINE_OTHERS="$(CILDIR)/$(SAFECLIB) $(CILDIR)/$(SAFEMAINLIB)" \
             TRANSLF_OTHERS="$(CILDIR)/$(SAFECLIB) $(CILDIR)/$(SAFEMAINLIB)" \
	     clean defaulttarget 

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

# weimer: test, compile and run
testc/% : $(SMALL1)/%.c  defaulttarget
	cd $(SMALL1); $(SAFECC)   \
               --patch=../../lib/$(PATCHFILE) \
	       $(DOOPT) $(EXEOUT)$*.exe $*.c ; ./$*.exe



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
                 $(DOOPT) $(WARNALL) \
                 $*.c \
                 $(EXEOUT)$*
	test/small2/$*


# sm: trivial test of combiner
MYSAFECC = $(SAFECC) --keep=. $(DEF)$(ARCHOS)
comb: test/small2/comb1.c test/small2/comb2.c defaulttarget
	rm -f test/small2/comb
	cd test/small2; \
	  $(MYSAFECC) --combine comb1.c $(CONLY) $(OBJOUT) comb1.o; \
	  $(MYSAFECC) --combine comb2.c $(CONLY) $(OBJOUT) comb2.o; \
          $(MYSAFECC) --combine comb1.o comb2.o $(EXEOUT)comb
	test/small2/comb

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
ifdef BOX
HUFFOTHERS=$(CILDIR)/$(SAFEMAINLIB) 
else
HUFFOTHERS=
endif
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
APATCH=--patch=apache.patch
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





COMBINESAFECC = $(SAFECC) --combine $(DOOPT)

#
# OLDEN benchmarks
#
# Barnes-Hut
BHDIR=test/olden/bh
ifdef BOX
BHEXTRA=$(CILDIR)/$(SAFEMAINLIB)
else
BHEXTRA=
endif
bh : defaulttarget mustbegcc
	cd $(BHDIR); rm code.exe *.o; \
               make EXTRA_LIBS=$(BHEXTRA) \
                    CC="$(COMBINESAFECC) --nobox=bhbox \
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
	cd $(BHDIR);sh -c "time code < data.in > data.out"



# Power pricing
PWDIR=test/olden/power
ifdef BOX
PWEXTRA=$(CILDIR)/$(SAFEMAINLIB)
else
PWEXTRA=
endif
power : defaulttarget mustbegcc
	cd $(PWDIR); \
               make PLAIN=1 clean defaulttarget \
                    EXTRA_LIBS=$(PWEXTRA) \
                    CC="$(COMBINESAFECC) \
			--patch=$(SAFECCDIR)/cil/lib/$(PATCHFILE)"
	cd $(PWDIR); ./power.exe


# Health care simulation
HEALTHDIR=test/olden/health
ifdef BOX
HEALTHEXTRA=$(CILDIR)/$(SAFEMAINLIB)
else
HEALTHEXTRA=
endif
ifdef _MSVC
HEALTHARGS = _MSVC=1
endif
health : defaulttarget
	cd $(HEALTHDIR); \
               make PLAIN=1 clean defaulttarget \
                    EXTRA_LIBS=$(HEALTHEXTRA) \
	            $(HEALTHARGS) \
                    CC="$(COMBINESAFECC) \
			--patch=$(SAFECCDIR)/cil/lib/$(PATCHFILE)"
	cd $(HEALTHDIR); ./health.exe 10 10 10 1



# Perimeter of regions in images
PERIMDIR=test/olden/perimeter
ifdef BOX
PERIMEXTRA=$(CILDIR)/$(SAFEMAINLIB)
else
PERIMEXTRA=
endif
ifdef _MSVC
PERIMARGS = _MSVC=1
endif
perimeter : defaulttarget
	cd $(PERIMDIR); \
               make PLAIN=1 clean defaulttarget \
                    EXTRA_LIBS=$(PERIMEXTRA) \
	            $(PERIMARGS) \
                    CC="$(COMBINESAFECC) \
			--patch=$(SAFECCDIR)/cil/lib/$(PATCHFILE)"
	cd $(PERIMDIR); ./perimeter.exe


# Voronoi diagrams
VORONDIR=test/olden/voronoi
ifdef BOX
VORONEXTRA=$(CILDIR)/$(SAFEMAINLIB)
else
VORONEXTRA=
endif
ifdef _MSVC
VORONARGS = _MSVC=1
endif
voronoi : defaulttarget
	cd $(VORONDIR); \
               make PLAIN=1 clean defaulttarget \
                    EXTRA_LIBS=$(VORONEXTRA) \
	            $(VORONARGS) \
                    CC="$(COMBINESAFECC) \
			--patch=$(SAFECCDIR)/cil/lib/$(PATCHFILE)"
	cd $(VORONDIR); ./voronoi.exe



# SPEC95
SPECDIR=test/spec95

COMPRESSDIR=$(SPECDIR)/129.compress
spec-compress : defaulttarget
	cd $(COMPRESSDIR)/src; make build
	echo "14000000 q 2231" >$(COMPRESSDIR)/exe/base/input.data 
	$(COMPRESSDIR)/exe/base/compress95.v8 \
              <$(COMPRESSDIR)/exe/base/input.data \
              >$(COMPRESSDIR)/exe/base/output.txt

old-compress : defaulttarget $(COMPRESSDIR)/src/combine-compress.c
	rm -f $(COMPRESSDIR)/combine-compress.exe
	cd $(COMPRESSDIR)/src ; $(SAFECC) --keep=. $(DEF)$(ARCHOS) $(DEF)$(PCCTYPE) \
                 $(DOOPT) \
                 combine-compress.c \
                 $(EXEOUT)combine-compress.exe
	sh -c "time $(COMPRESSDIR)/src/combine-compress.exe < $(COMPRESSDIR)/src/input.data > $(COMPRESSDIR)/src/combine-compress.out"

compress-noclean: defaulttarget mustbegcc
	cd $(COMPRESSDIR)/src; make CC="$(COMBINESAFECC)" build
	echo "14000000 q 2231" >$(COMPRESSDIR)/exe/base/input.data 
	sh -c "time $(COMPRESSDIR)/exe/base/compress95.v8 < $(COMPRESSDIR)/exe/base/input.data > $(COMPRESSDIR)/src/combine-compress.out"

ifdef BOX
COMPRESSEXTRA=$(CILDIR)/$(SAFEMAINLIB) 
else
COMPRESSEXTRA=
endif
compress: defaulttarget mustbegcc
	cd $(COMPRESSDIR)/src; \
               make EXTRA_LIBS=$(COMPRESSEXTRA) \
                    CC="$(COMBINESAFECC) --patch=$(SAFECCDIR)/cil/lib/$(PATCHFILE)" clean build
	echo "14000000 q 2231" >$(COMPRESSDIR)/exe/base/input.data 
	sh -c "time $(COMPRESSDIR)/exe/base/compress95.v8 < $(COMPRESSDIR)/exe/base/input.data > $(COMPRESSDIR)/src/combine-compress.out"

LIDIR=$(SPECDIR)/130.li
LISAFECC=$(SAFECC) --combine --patch=$(SAFECCDIR)/cil/lib/$(PATCHFILE) --keep=safeccout
ifdef BOX
LIEXTRA=$(CILDIR)/$(SAFEMAINLIB)
else
LIEXTRA=
endif
li: defaulttarget mustbegcc
	cd $(LIDIR)/src; \
            make clean build CC="$(LISAFECC) $(CONLY)" \
                             LD="$(LISAFECC)" \
                             EXTRA_LIBS=$(LIEXTRA)
	sh -c "time $(LIDIR)/src/trial_li \
            <$(LIDIR)/data/train/input/train.lsp \
            >$(LIDIR)/data/train/input/train.out"

licombined: defaulttarget mustbegcc
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
GOSAFECC=$(SAFECC) --combine  --patch=$(SAFECCDIR)/cil/lib/$(PATCHFILE) --keep=safeccout
ifdef BOX
GOEXTRA=$(CILDIR)/$(SAFEMAINLIB)
else
GOEXTRA=
endif

goclean: 	
	cd $(GODIR)/src; make clean
	cd $(GODIR)/src; rm -f *cil.c *box.c *.i *_ppp.c *.origi


go: defaulttarget mustbegcc
	cd $(GODIR)/src; \
            make clean build CC="$(GOSAFECC) $(CONLY)" \
                             LD="$(GOSAFECC)" \
                             EXTRA_LIBS=$(GOEXTRA)
	$(GODIR)/exe/base/go.ultra 50 9

go-noclean: defaulttarget mustbegcc
	cd $(GODIR)/src; \
            make build CC="$(GOSAFECC) $(CONLY)" \
                       LD="$(GOSAFECC)" \
                             EXTRA_LIBS=$(GOEXTRA) 
	sh -c "time $(GODIR)/exe/base/go.ultra 50 9"



### SPEC95 vortex
VORDIR=$(SPECDIR)/147.vortex
VORSAFECC=$(SAFECC) --combine   --patch=$(SAFECCDIR)/cil/lib/$(PATCHFILE) --keep=safeccout
ifdef BOX
VOREXTRA=$(CILDIR)/$(SAFEMAINLIB)
else
VOREXTRA=
endif

vortexclean: 	
	cd $(VORDIR)/src; make clean
	cd $(VODIR)/src; rm -f *cil.c *box.c *.i *_ppp.c *.origi

vortex: defaulttarget mustbegcc
	cd $(VORDIR)/src; \
            make clean build CC="$(VORSAFECC) $(CONLY)" \
                             LD="$(VORSAFECC)" \
                             EXTRA_LIBS=$(VOREXTRA)
	sh -c "time $(VORDIR)/exe/base/vortex.ultra \
            <$(VORDIR)/data/test/input/vortex.raw"

vortex-noclean: defaulttarget mustbegcc
	cd $(VORDIR)/src; \
            make build CC="$(VORSAFECC) $(CONLY)" \
                       LD="$(VORSAFECC)" \
                       EXTRA_LIBS=$(VOREXTRA) 
	sh -c "time $(VORDIR)/exe/base/vortex.ultra \
            <$(VORDIR)/data/test/input/vortex.raw"

vortex-combined: defaulttarget mustbegcc
	cd $(VORDIR)/exe/base; \
            $(SAFECC) vortex.ultra_all.c $(VOREXTRA) $(EXEOUT)vortex.ultra.exe


### SPEC95 m88ksim
M88DIR=$(SPECDIR)/124.m88ksim
M88SAFECC=$(SAFECC) --combine --keep=safeccout
ifdef BOX
M88EXTRA=$(CILDIR)/$(SAFEMAINLIB)
else
M88EXTRA=
endif

m88kclean: 	
	cd $(M88DIR)/src; make clean
	cd $(M88DIR)/src; rm -f *cil.c *box.c *.i *_ppp.c *.origi

m88k: defaulttarget mustbegcc
	cd $(M88DIR)/src; \
            make clean build CC="$(M88SAFECC) $(CONLY)" \
                             LD="$(M88SAFECC)" \
                             EXTRA_LIBS=$(M88EXTRA) 
	sh -c "time $(M88DIR)/exe/base/m88ksim.ultra"

m88k-noclean: defaulttarget mustbegcc
	cd $(M88DIR)/src; \
            make       build CC="$(M88SAFECC) $(CONLY)" \
                             LD="$(M88SAFECC)" \
                             EXTRA_LIBS=$(M88EXTRA) 
	sh -c "time $(M88DIR)/exe/base/m88ksim.ultra"


### SPEC95 ijpeg
IJPEGDIR=$(SPECDIR)/132.ijpeg
IJPEGSAFECC=$(SAFECC) --combine --keep=safeccout  \
                  --patch=$(SAFECCDIR)/cil/lib/$(PATCHFILE) \
                  --nobox=ijpegbox
ifdef BOX
IJPEGEXTRA=$(CILDIR)/$(SAFEMAINLIB)
else
IJPEGEXTRA=
endif
ifeq ($(ARCHOS), x86_WIN32)
IJPEGSAFECC += -DWIN32 -DMSDOS
endif
ijpegclean: 	
	cd $(IJPEGDIR)/src; make clean
	cd $(IJPEGDIR)/src; rm -f *cil.c *box.c *.i *_ppp.c *.origi

ijpeg: defaulttarget mustbegcc
	cd $(IJPEGDIR)/src; \
            make clean build CC="$(IJPEGSAFECC) $(CONLY)" \
                             LD="$(IJPEGSAFECC)" \
                             EXTRA_LIBS=$(IJPEGEXTRA)
	sh -c "time $(IJPEGDIR)/exe/base/ijpeg.ultra \
            -image_file $(IJPEGDIR)/data/ref/input/penguin.ppm \
            -GO"

ijpeg-combined: defaulttarget mustbegcc
	cd $(IJPEGDIR)/exe/base; \
            $(SAFECC) ijpeg.ultra_all.c $(IJPEGEXTRA) \
                $(EXEOUT)ijpeg.ultra
	sh -c "time $(IJPEGDIR)/exe/base/ijpeg.ultra \
            -image_file $(IJPEGDIR)/data/ref/input/penguin.ppm \
            -GO"

ijpeg-noclean: defaulttarget mustbegcc
	cd $(IJPEGDIR)/src; \
            make       build CC="$(IJPEGSAFECC) $(CONLY)" \
                             LD="$(IJPEGSAFECC)" \
                             EXTRA_LIBS=$(IJPEGEXTRA)
	sh -c "time $(IJPEGDIR)/exe/base/ijpeg.ultra \
            -image_file $(IJPEGDIR)/data/ref/input/penguin.ppm \
            -GO"

#### SPEC95 gcc
GCCDIR=$(SPECDIR)/126.gcc
GCCSAFECC=$(SAFECC) --combine --keep=safeccout

gccclean: 	
	cd $(GCCDIR)/src; make clean
	cd $(GCCDIR)/src; rm -f *cil.c *box.c *.i *_ppp.c *.origi

gcc: defaulttarget mustbegcc
	cd $(GCCDIR)/src; \
            make clean build CC="$(GCCSAFECC) $(CONLY)" \
                             LD="$(GCCSAFECC)" 

gcc-noclean: defaulttarget mustbegcc
	cd $(GCCDIR)/src; \
            make       build CC="$(GCCSAFECC) $(CONLY)" \
                             LD="$(GCCSAFECC)" 

allcc1: defaulttarget mustbegcc
	cd $(GCCDIR)/exe/base; $(SAFECC) $(DOOPT) cc1.v8_all.c


combinetest: defaulttarget
	cd test/small1; $(SAFECC) --combine /Fet.exe t.c t1.c
