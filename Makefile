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
MODULES     = pretty errormsg stats cil check box
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
MODULES    += cabs clexer cparser cprint cabs2cil frontc
endif

# Add main late
MODULES    += main
    # Include now the common set of rules for OCAML
    # This file will add the rules to make $(EXECUTABLE).$(EXE)
include Makefile.ocaml



##### Settings that depend on the computer we are on
##### Make sure the COMPUTERNAME environment variable is set
ifeq ($(COMPUTERNAME), RAW)   # George's workstation
BASEDIR=C:/Necula
TVDIR=$(BASEDIR)/Source/TransVal
endif
ifeq ($(COMPUTERNAME), FETA) # George's home machine
BASEDIR=C:/Necula
TVDIR=$(BASEDIR)/Source/TransVal
endif
SAFECCDIR=$(BASEDIR)/SafeC
PCCDIR=$(SAFECCDIR)/cil/test/PCC



#####################3
.PHONY : safec
safec : $(EXECUTABLE)$(EXE)

.PHONE: trval
trval: $(TVDIR)/obj/transval.asm.exe
	make -C $(TVDIR) RELEASE=1

export EXTRAARGS
export BOX
ifndef _GNUCC
_MSVC = 1			# Use the MSVC compiler by default
endif

ifdef _GNUCC
DEBUGCCL=gcc -x c -O0 -g -Wall
RELEASECCL=gcc -x c -O3 -Wall
#LIB=lib
#LIBOUT=-o
DOOPT=-O3
CONLY=-c
OBJOUT=-o
EXEOUT=-o
DEF=-D
ASMONLY=-S -o 
CPPSTART=gcc -E %i -Dx86_WIN32 -D_GNUCC
CPPOUT=-o %o
CPP=$(CPPSTART) $(CPPOUT)
INC=-I
endif


ifdef _MSVC
DEBUGCCL=cl /TC /O0 /Zi /MLd /I./lib /DEBUG
RELEASECCL=cl /TC /ML /I./lib
DOOPT=/O2
CONLY=/c
OBJOUT=/Fo
EXEOUT=/Fe
DEF=/D
ASMONLY=/Fa
INC=/I
CPPSTART=cl /Dx86_WIN32 /D_MSVC /E /TC /I./lib /FI fixup.h /DBEFOREBOX
CPPOUT= %i >%o
CPP=$(CPPSTART) $(CPPOUT)
EXTRAARGS += -msvc
endif

ifdef RELEASE
CCL=RELEASECCL
else
CCL=DEBUGCCL
endif
CC=$(CCL) $(CONLY)

SAFECC=perl $(SAFECCDIR)/cil/lib/safecc.pl
ifndef NOCABS
SAFECC+= --cabs
endif
ifndef NOCIL
SAFECC+= --cil
endif	
ifdef BOX
SAFECC+= --box
endif
ifdef CHECK
EXTRAARGS += -check
endif
ifdef RELEASE
SAFECC+= --release
endif
ifdef TV
SAFECC+= --tv="$(TV)"
TVEXE=trval
endif
SAFECC+= $(EXTRAARGS:%= --safec=%)

    # Now the rules to make the library
ifdef _MSVC
SAFECLIB=/Necula/SafeC/cil/obj/safec.lib
$(SAFECLIB) : $(SAFECCDIR)/cil/lib/safec.c
	cl /O2 /Zi /I./lib /c $(DEF)_MSVC $(OBJOUT)$(OBJDIR)/safec.o $<
	lib /OUT:$(SAFECLIB) $(OBJDIR)/safec.o 
SAFEMAINLIB=/Necula/SafeC/cil/obj/safecmain.lib
$(SAFEMAINLIB) : $(SAFECCDIR)/cil/lib/safecmain.c
	cl /O2 /Zi /I./lib /c $(DEF)_MSVC $(OBJOUT)$(OBJDIR)/safecmain.o $<
	lib /OUT:$(SAFEMAINLIB) $(OBJDIR)/safecmain.o 
endif
ifdef _GNUCC
SAFECLIB=$(OBJDIR)/safeclib.a
$(SAFECLIBRARY) : $(SAFECCDIR)/cil/lib/safec.c
	$(CC) $(OBJOUT)$(OBJDIR)/safec.o $<
	$(LIB) $(LIBOUT)$(SAFECLIB) $(OBJOUT)$(OBJDIR)/safec.o 
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
	cd $(PCCTEST); $(SAFECC) --keep=. $(DEF)x86_WIN32 \
                  $(DEF)$(PCCTYPE) $(CONLY) \
                  $(PCCDIR)/src/$*.c \
                  $(OBJOUT)$(notdir $*).o


testallpcc: $(EXECUTABLE)$(EXE) $(TVEXE) $(SAFECLIB) $(SAFEMAINLIB) 
	-rm $(PCCDIR)/x86_WIN32$(PCCCOMP)/$(PCCTYPE)/*.o
	-rm $(PCCDIR)/x86_WIN32$(PCCCOMP)/$(PCCTYPE)/*.exe
	make -C $(PCCDIR) \
             CC="$(SAFECC) --keep=$(SAFECCDIR)/cil/test/PCCout $(CONLY)" \
             USE_JAVA=1 USE_JUMPTABLE=1 TYPE=$(PCCTYPE) \
             COMPILER=$(PCCCOMP) \
             ENGINE_OTHERS="C:$(SAFECLIB) C:$(SAFEMAINLIB)" \
             TRANSLF_OTHERS="C:$(SAFECLIB) C:$(SAFEMAINLIB)" \
	     defaulttarget 

runpcc:
ifdef _GNUCC
	rm $(PCCDIR)/bin/*_MSVC*
endif
	cd $(PCCDIR)/test; test.cmd fact

############ Small tests
SMALL1=test/small1
test/% : $(SMALL1)/%.c $(EXECUTABLE)$(EXE) $(TVEXE)
	cd $(SMALL1); $(SAFECC) $*.c $(CONLY) $(DOOPT) $(ASMONLY)$*.s

SMALL2=test/small2
hashtest: test/small2/hashtest.c $(EXECUTABLE)$(EXE) \
                    $(SAFECLIB) $(SAFEMAINLIB) $(TVEXE)
	rm -f $(SMALL2)/hashtest.exe
	cd $(SMALL2); $(SAFECC) --keep=. $(DEF)x86_WIN32 $(DEF)$(PCCTYPE) \
                 $(DOOPT) \
                 hash.c hashtest.c \
                 $(EXEOUT)hashtest.exe
	$(SMALL2)/hashtest.exe

rbtest: test/small2/rbtest.c $(EXECUTABLE)$(EXE) $(TVEXE)
	rm -f $(SMALL2)/rbtest.exe
	cd $(SMALL2); $(SAFECC) --keep=. $(DEF)x86_WIN32 $(DEF)$(PCCTYPE) \
                 $(DOOPT) \
                 redblack.c rbtest.c \
                 $(EXEOUT)rbtest.exe
	$(SMALL2)/rbtest.exe

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
