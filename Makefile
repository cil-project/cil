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
              simplesolve secondsolve globinit box markptr optim
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
MODULES    += cabs clexer cparser cprint cabs2cil combine frontc
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
endif
ifeq ($(COMPUTERNAME), leetch) # scott's laptop
BASEDIR=/home/scott/wrk/safec
SAFECCDIR=$(BASEDIR)
PCCDIR=$(SAFECCDIR)/cil/test/PCC
TVDIR=$(BASEDIR)/TransVal
CILDIR=$(BASEDIR)/cil
_GNUCC=1
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
ifeq ($(COMPUTERNAME), brooksie_raygto) # Rahul's desktop, for Raymond
BASEDIR=/home/raygto/
SAFECCDIR=$(BASEDIR)
PCCDIR=$(SAFECCDIR)/cil/test/PCC
TVDIR=$(BASEDIR)/TransVal
CILDIR=$(BASEDIR)/cil
_GNUCC=1
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
DEBUGCCL=gcc -x c -O0 -g -ggdb -D_GNUCC 
RELEASECCL=gcc -x c -O3 -Wall -I/usr/include/sys
#LIB=lib
#LIBOUT=-o
DOOPT=-O3
CONLY=-c
OBJOUT=-o
OBJ=o
LIBEXT=a
EXEOUT=-o
DEF=-D
ASMONLY=-S -o 
CPPSTART=gcc -E %i -Dx86_LINUX -D_GNUCC -include fixup.h  -I/usr/include/sys
CPPOUT=-o %o
CPP=$(CPPSTART) $(CPPOUT)
INC=-I
PATCHFILE=safec_gcc.patch
# sm: disable patching for now ('true' has no output)
# (set it to 'echo' to re-enable)
PATCHECHO=true
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



######################
.PHONY : defaulttarget
defaulttarget : $(EXECUTABLE)$(EXE) $(SAFECLIB) $(SAFEMAINLIB)

.PHONY: trval
trval: $(TVDIR)/obj/transval.asm.exe
	make -C $(TVDIR) RELEASE=1




SAFECC=perl $(CILDIR)/lib/safecc.pl


# weimer: support for other solvers
ifeq ($(INFERBOX), 2)
    SAFECC+= --safec="-solver second"
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

$(SAFECLIB) : lib/safec.c lib/safec.h lib/safeccheck.h
	cl /Ox /Zi /I./lib /c $(DEF)_MSVC $(SAFECLIBARG) \
                                          $(OBJOUT)obj/safec.o $<
	lib /OUT:$@ obj/safec.o 

$(SAFEMAINLIB) : lib/safecmain.c lib/safec.h lib/safeccheck.h
	cl /Ox /Zi /I./lib /c $(DEF)_MSVC $(OBJOUT)obj/safecmain.o $<
	lib /OUT:$@ obj/safecmain.o 
endif

# Libraries on GCC
ifdef _GNUCC
$(SAFECLIB) : lib/safec.c
	$(CC) $(OBJOUT)obj/safec.o $<
	ar -r $@ obj/safec.o

$(SAFEMAINLIB) : lib/safecmain.c lib/safec.h lib/safeccheck.h
	$(CC) $(OBJOUT)obj/safecmain.o $<
	ar -r $@ obj/safecmain.o
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

testpcc/% : $(PCCDIR)/src/%.c $(EXECUTABLE)$(EXE) $(TVEXE)
	cd $(CILDIR)/test/PCCout; $(SAFECC) --keep=. $(DEF)$(ARCHOS) \
                  $(DEF)$(PCCTYPE) $(CONLY) \
                  $(PCCDIR)/src/$*.c \
                  $(OBJOUT)$(notdir $*).o



testallspj: $(EXECUTABLE)$(EXE) $(TVEXE) $(SAFECLIB) $(SAFEMAINLIB) 
	-rm $(PCCDIR)/$(ARCHOS)$(PCCCOMP)/$(PCCTYPE)/*.o
	-rm $(PCCDIR)/$(ARCHOS)$(PCCCOMP)/$(PCCTYPE)/*.exe
	make -C $(PCCDIR) \
             CC="$(SAFECC) --keep=$(CILDIR)/test/PCCout $(CONLY)" \
             USE_JAVA=1 USE_JUMPTABLE=1 TYPE=$(PCCTYPE) \
             COMPILER=$(PCCCOMP) \
             ENGINE_OTHERS="$(CILDIR)/$(SAFECLIB) $(CILDIR)/$(SAFEMAINLIB)" \
             TRANSLF_OTHERS="$(CILDIR)/$(SAFECLIB) $(CILDIR)/$(SAFEMAINLIB)" \
	     defaulttarget 

ifdef _MSVC
MSLINK=--mode=mslink
endif
combinepcc: defaulttarget
	-rm $(PCCDIR)/$(ARCHOS)$(PCCCOMP)/$(PCCTYPE)/*.o
	-rm $(PCCDIR)/$(ARCHOS)$(PCCCOMP)/$(PCCTYPE)/*.exe
	make -C $(PCCDIR) \
             CC="$(SAFECC) --patch=$(CILDIR)/lib/$(PATCHFILE) --combine --keep=$(CILDIR)/test/PCCout $(CONLY)" \
             LD="$(SAFECC) $(MSLINK) --combine --keep=$(CILDIR)/test/PCCout" \
             USE_JAVA=1 USE_JUMPTABLE=1 TYPE=$(PCCTYPE) \
             COMPILER=$(PCCCOMP) \
             ENGINE_OTHERS="$(CILDIR)/$(SAFECLIB) $(CILDIR)/$(SAFEMAINLIB)" \
             TRANSLF_OTHERS="$(CILDIR)/$(SAFECLIB) $(CILDIR)/$(SAFEMAINLIB)" \
	     defaulttarget 

.PHONY : allpcc
allpcc: $(EXECUTABLE)$(EXE) $(SAFEMAINLIB) $(SAFECLIB)
	cd $(PCCTEST); \
           $(SAFECC) --keep=. \
                 $(DOOPT) \
                 ../PCC/bin/engine.$(ARCHOS)$(PCCCOMP).$(PCCTYPE).exe.c \
                 $(EXEOUT)allengine.exe

runpcc:
ifdef _GNUCC
	rm $(PCCDIR)/bin/*_MSVC*
endif
	cd $(PCCDIR)/test; test.cmd fact --save-temps=pccout --gory


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
test/% : $(SMALL1)/%.c $(EXECUTABLE)$(EXE) $(TVEXE)
	cd $(SMALL1); $(SAFECC)   \
               --patch=../../lib/$(PATCHFILE) \
	       $(CONLY) $(DOOPT) $(ASMONLY)$*.s $*.c 

# weimer: test, compile and run
testc/% : $(SMALL1)/%.c $(EXECUTABLE)$(EXE) $(TVEXE)
	cd $(SMALL1); $(SAFECC)   \
               --patch=../../lib/$(PATCHFILE) \
	       $(DOOPT) $(EXEOUT)$*.exe $*.c ; ./$*.exe


HASHTESTCC = $(SAFECC) --keep=.  --patch=../../lib/$(PATCHFILE) \
              $(DOOPT) $(DEF)$(ARCHOS) $(DEF)$(PCCTYPE) \
              $(INC)$(PCCDIR)/src 


hashtest: test/small2/hashtest.c $(EXECUTABLE)$(EXE) \
                                 $(SAFECLIB) $(SAFEMAINLIB)  $(TVEXE)
	rm -f $(PCCTEST)/hashtest.exe
	cd $(PCCTEST); $(SAFECC) --keep=. $(DEF)$(ARCHOS) $(DEF)$(PCCTYPE) \
                 $(DOOPT) \
                 `$(PATCHECHO) --patch=../../lib/$(PATCHFILE)` \
                 $(INC)$(PCCDIR)/src \
                 $(PCCDIR)/src/hash.c \
                 ../small2/hashtest.c \
                 $(EXEOUT)hashtest.exe
	$(PCCTEST)/hashtest.exe

combinehashtest: defaulttarget
	rm -f $(PCCTEST)/hashtest.exe
	cd $(PCCTEST); \
          $(HASHTESTCC) $(CONLY) --combine $(PCCDIR)/src/hash.c \
                                           $(OBJOUT)hash.obj; \
          $(HASHTESTCC) $(CONLY) --combine ../small2/hashtest.c \
                                           $(OBJOUT)hashtest.obj;\
          $(HASHTESTCC) --combine ./hash.obj ./hashtest.obj \
                                           $(EXEOUT)hashtest.exe

rbtest: test/small2/rbtest.c $(EXECUTABLE)$(EXE) \
                                 $(SAFECLIB) $(SAFEMAINLIB)  $(TVEXE)
	rm -f $(PCCTEST)/rbtest.exe
	cd $(PCCTEST); $(SAFECC) --keep=. $(DEF)$(ARCHOS) $(DEF)$(PCCTYPE) \
                 `$(PATCHECHO) --patch=../../lib/$(PATCHFILE)` \
                 $(DOOPT) \
                 $(INC)$(PCCDIR)/src \
                 $(PCCDIR)/src/redblack.c \
                 ../small2/rbtest.c \
                 $(EXEOUT)rbtest.exe
	$(PCCTEST)/rbtest.exe

btreetest: test/small2/testbtree.c \
           test/small2/btree.c \
                                 $(EXECUTABLE)$(EXE) \
                                 $(SAFECLIB) $(SAFEMAINLIB)  $(TVEXE)
	rm -f test/small2/btreetest.exe
	cd test/small2; $(SAFECC) --keep=. \
                 $(DOOPT) \
                 --patch=../../lib/$(PATCHFILE) \
                 btree.c testbtree.c \
                 $(EXEOUT)btreetest.exe
	test/small2/btreetest.exe


# sm: this is my little test program
hola: test/small2/hola.c $(EXECUTABLE)$(EXE) \
                                 $(SAFECLIB) $(SAFEMAINLIB)
	rm -f test/small2/hola
	cd test/small2; $(SAFECC) --keep=. $(DEF)$(ARCHOS) \
                 `$(PATCHECHO) --patch=../../lib/$(PATCHFILE)` \
                 $(DOOPT) \
                 hola.c \
                 $(EXEOUT)hola
	test/small2/hola


HUFFCOMPILE=$(SAFECC) --keep=. 
# HUFFCOMPILE=cl /MLd
ifdef BOX
HUFFOTHERS=$(CILDIR)/$(SAFEMAINLIB) 
else
HUFFOTHERS=
endif
hufftest: test/small2/hufftest.c $(EXECUTABLE)$(EXE) \
                                 $(SAFECLIB) $(SAFEMAINLIB) $(TVEXE)
	rm -f $(PCCTEST)/hufftest.exe
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
	cd $(PCCTEST); ./hufftest.exe \
                             $(CILDIR)/src/frontc/cparser.output

wes-rbtest: test/small2/wes-rbtest.c $(EXECUTABLE)$(EXE) $(TVEXE)\
            $(SAFECLIB)
	rm -f $(PCCTEST)/wes-rbtest.exe
	cd $(PCCTEST); $(SAFECC) --keep=. $(DEF)$(ARCHOS) $(DEF)$(PCCTYPE) \
                 $(DOOPT) \
                 --patch=../../lib/$(PATCHFILE) \
                 $(INC)$(PCCDIR)/src \
                 ../small2/wes-rbtest.c \
                 $(EXEOUT)wes-rbtest.exe
	$(PCCTEST)/wes-rbtest.exe

wes-hashtest: test/small2/wes-hashtest.c $(EXECUTABLE)$(EXE) $(TVEXE) \
              $(SAFECLIB)
	rm -f $(PCCTEST)/wes-hashtest.exe
	cd $(PCCTEST); $(SAFECC) --keep=. $(DEF)$(ARCHOS) $(DEF)$(PCCTYPE) \
                 $(DOOPT) \
                 --patch=../../lib/$(PATCHFILE) \
                 $(INC)$(PCCDIR)/src \
                 ../small2/wes-hashtest.c \
                 $(EXEOUT)wes-hashtest.exe
	$(PCCTEST)/wes-hashtest.exe


### Generic test
testfile/% : $(EXECUTABLE)$(EXE) %  $(TVEXE)
	$(SAFECC) /TC $*

testdir/% : $(EXECUTABLE)$(EXE)
	make -C CC="perl safecc.pl" $*


################## Linux device drivers
testlinux/% : $(EXECUTABLE)$(EXE) test/linux/%.cpp
	cd test/linux; $(SAFECC) -o $*.o $*.cpp 

testqp : testlinux/qpmouse
testserial: testlinux/generic_serial

################## Rahul's test cases
SPR-TESTDIR = test/spr
spr/% : $(EXECUTABLE)$(EXE)
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

apache/urlcount : $(EXECUTABLE)$(EXE)
	rm -f $(APACHETEST)/mod_urlcount.$(OBJ)
	cd $(APACHETEST); $(SAFECC) \
                       --keep=. $(APATCH) \
                        $(DOOPT) \
                        $(APACHECFLAGS) \
                        $(CONLY) $(OBJOUT)./mod_urlcount.$(OBJ) \
                        mod_urlcount.c

apache/layout : $(EXECUTABLE)$(EXE)
	rm -f $(APACHETEST)/mod_layout.$(OBJ)
	cd $(APACHETEST); $(SAFECC) \
                       --keep=. $(APATCH) \
                        $(DOOPT) \
                        $(APACHECFLAGS) \
                        $(CONLY) $(OBJOUT)./mod_layout.$(OBJ) \
                        mod_layout.c

apache/random : $(EXECUTABLE)$(EXE)
	rm -f $(APACHETEST)/mod_random.$(OBJ)
	cd $(APACHETEST); $(SAFECC) \
                       --keep=. $(APATCH) \
                        $(DOOPT) \
                        $(APACHECFLAGS) \
                        $(CONLY) $(OBJOUT)./mod_random.$(OBJ) \
                        mod_random.c

apache/gzip : $(EXECUTABLE)$(EXE)
	rm -f $(APACHETEST)/mod_gzip.$(OBJ)
	cd $(APACHETEST); $(SAFECC) \
                       --keep=. $(APATCH) \
                        $(DOOPT) \
                        $(APACHECFLAGS) \
                        $(CONLY) $(OBJOUT)./mod_gzip.$(OBJ) \
                        mod_gzip.c

apache/t : $(EXECUTABLE)$(EXE)
	rm -f $(APACHETEST)/t.obj
	cd $(APACHETEST); $(SAFECC) \
                       --keep=. $(APATCH) \
                        $(APACHECFLAGS) \
                        $(OBJOUT)./t.obj \
                        t.c

apache/rewrite: $(EXECUTABLE)$(EXE)
	rm -f $(APACHETEST)/mod_gzip.$(OBJ)
	cd $(APACHETEST); $(SAFECC) \
                       --keep=. $(APATCH) \
                        $(DOOPT) \
                        $(APACHECFLAGS) \
                        $(OBJOUT)./mod_rewrite.$(OBJ) \
                        $(APACHEBASE)/modules/standard/mod_rewrite.c




# SPEC95
SPECDIR=test/spec95

COMPRESSDIR=$(SPECDIR)/129.compress
compress : defaulttarget
	cd $(COMPRESSDIR)/src; make build
	echo "10000 32 2365" >$(COMPRESSDIR)/exe/base/input.data 
	$(COMPRESSDIR)/exe/base/compress95.v8 \
              <$(COMPRESSDIR)/exe/base/input.data \
              >$(COMPRESSDIR)/exe/base/output.txt


LIDIR=$(SPECDIR)/130.li
li: defaulttarget
	cd $(LIDIR)/src; make build
	$(LIDIR)/src/trial_li \
            <$(LIDIR)/data/train/input/train.lsp \
            >$(LIDIR)/data/train/input/train.out