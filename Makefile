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
PCCDIR=$(BASEDIR)/PCC
TVDIR=$(BASEDIR)/TransVal
CILDIR=$(BASEDIR)/cil
_GNUCC=1
endif
ifeq ($(COMPUTERNAME), madrone) # scott's desktop
BASEDIR=/home/scott/wrk
SAFECCDIR=$(BASEDIR)/safec
PCCDIR=$(BASEDIR)/PCC
TVDIR=$(BASEDIR)/TransVal
CILDIR=$(BASEDIR)/cil
_GNUCC=1
endif

######################
.PHONY : spec
spec : $(EXECUTABLE)$(EXE)

.PHONE: trval
trval: $(TVDIR)/obj/transval.asm.exe
	make -C $(TVDIR) RELEASE=1

export EXTRAARGS
export BOX
ifndef _GNUCC
_MSVC = 1			# Use the MSVC compiler by default
endif

ifdef _GNUCC
DEBUGCCL=gcc -x c -O0 -g -Wall -I/usr/include/sys
RELEASECCL=gcc -x c -O3 -Wall -I/usr/include/sys
#LIB=lib
#LIBOUT=-o
DOOPT=-O3
CONLY=-c
OBJOUT=-o
EXEOUT=-o
DEF=-D
ASMONLY=-S -o 
CPPSTART=gcc -E %i -Dx86_WIN32 -D_GNUCC  -I/usr/include/sys
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


SAFECC=perl $(CILDIR)/lib/safecc.pl
ifndef NOCABS
SAFECC+= --cabs
endif
ifndef NOCIL
SAFECC+= --cil
endif	
ifdef BOX
SAFECC+= --box
endif
ifdef NO_TAGS
SAFECC+= $(DEF)NO_TAGS
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
ifdef RELEASE
SAFECLIB=/Necula/SafeC/cil/obj/safec.lib
else
SAFECLIB=/Necula/SafeC/cil/obj/safecdebug.lib
SAFECLIBARG=$(DEF)_DEBUG
endif
$(SAFECLIB) : $(SAFECCDIR)/cil/lib/safec.c \
              $(SAFECCDIR)/cil/lib/safec.h \
              $(SAFECCDIR)/cil/lib/safeccheck.h
	cl /O2 /Zi /I./lib /c $(DEF)_MSVC $(SAFECLIBARG) \
                                          $(OBJOUT)$(OBJDIR)/safec.o $<
	lib /OUT:$(SAFECLIB) $(OBJDIR)/safec.o 

SAFEMAINLIB=/Necula/SafeC/cil/obj/safecmain.lib
$(SAFEMAINLIB) : $(SAFECCDIR)/cil/lib/safecmain.c \
                 $(SAFECCDIR)/cil/lib/safec.h \
                 $(SAFECCDIR)/cil/lib/safeccheck.h
	cl /O2 /Zi /I./lib /c $(DEF)_MSVC $(OBJOUT)$(OBJDIR)/safecmain.o $<
	lib /OUT:$(SAFEMAINLIB) $(OBJDIR)/safecmain.o 
endif
ifdef _GNUCC
SAFECLIB=$(OBJDIR)/safeclib.a
$(SAFECLIB) : $(SAFECCDIR)/cil/lib/safec.c
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
	cd $(SAFECCDIR)/cil/test/PCCout; $(SAFECC) --keep=. $(DEF)x86_WIN32 \
                  $(DEF)$(PCCTYPE) $(CONLY) \
                  $(PCCDIR)/src/$*.c \
                  $(OBJOUT)$(notdir $*).o

testallpcc: $(EXECUTABLE)$(EXE) $(TVEXE) $(SAFECLIB) $(SAFEMAINLIB) 
	-rm $(PCCDIR)/x86_WIN32$(PCCCOMP)/$(PCCTYPE)/*.o
	-rm $(PCCDIR)/x86_WIN32$(PCCCOMP)/$(PCCTYPE)/*.exe
	make -C $(PCCDIR) \
             CC="$(SAFECC) --keep=$(CILDIR)/test/PCCout $(CONLY)" \
             USE_JAVA= USE_JUMPTABLE= TYPE=$(PCCTYPE) \
             COMPILER=$(PCCCOMP) \
             ENGINE_OTHERS="C:$(SAFECLIB) C:$(SAFEMAINLIB)" \
             TRANSLF_OTHERS="C:$(SAFECLIB) C:$(SAFEMAINLIB)" \
	     defaulttarget 

runpcc:
ifdef _GNUCC
	rm $(PCCDIR)/bin/*_MSVC*
endif
	cd $(PCCDIR)/test; test.cmd fact --save-temps=pccout --gory

############ Small tests
SMALL1=test/small1
test/% : $(SMALL1)/%.c $(EXECUTABLE)$(EXE) $(TVEXE)
	cd $(SMALL1); $(SAFECC) $*.c $(CONLY) $(DOOPT) $(ASMONLY)$*.s

SMALL2=test/small2

hashtest: test/small2/hashtest.c $(EXECUTABLE)$(EXE) $(TVEXE)
	rm -f $(PCCTEST)/hashtest.exe
	cd $(PCCTEST); $(SAFECC) --keep=. $(DEF)x86_WIN32 $(DEF)$(PCCTYPE) \
                 $(DOOPT) \
                 $(INC)$(PCCDIR)/src \
                 $(PCCDIR)/src/hash.c \
                 ../small2/hashtest.c \
                 $(EXEOUT)hashtest.exe
	$(PCCTEST)/hashtest.exe

rbtest: test/small2/rbtest.c $(EXECUTABLE)$(EXE) $(TVEXE)
	rm -f $(PCCTEST)/rbtest.exe
	cd $(PCCTEST); $(SAFECC) --keep=. $(DEF)x86_WIN32 $(DEF)$(PCCTYPE) \
                 $(DOOPT) \
                 $(INC)$(PCCDIR)/src \
                 $(PCCDIR)/src/redblack.c \
                 ../small2/rbtest.c \
                 $(EXEOUT)rbtest.exe
	$(PCCTEST)/rbtest.exe

HUFFCOMPILE=$(SAFECC) --keep=. 
# HUFFCOMPILE=cl /MLd
ifdef BOX
HUFFOTHERS="C:$(SAFEMAINLIB)" 
else
HUFFOTHERS=
endif
hufftest: test/small2/hufftest.c $(EXECUTABLE)$(EXE) \
                                 $(SAFECLIB) $(SAFEMAINLIB) $(TVEXE)
	rm -f $(PCCTEST)/hufftest.exe
	cd $(PCCTEST); $(HUFFCOMPILE) \
                 $(DEF)x86_WIN32 $(DEF)$(PCCTYPE) $(DEF)$(PCCCOMP) \
                 $(DOOPT) \
                 $(INC)$(PCCDIR)/src \
                 $(PCCDIR)/src/io.c \
                 $(PCCDIR)/src/huffman.c \
                 $(PCCDIR)/src/hash.c \
                 ../small2/hufftest.c \
                 $(HUFFOTHERS) \
                 $(EXEOUT)hufftest.exe
	cd $(PCCTEST); ./hufftest.exe \
                             $(SAFECCDIR)/cil/src/frontc/cparser.output

wes-rbtest: test/small2/wes-rbtest.c $(EXECUTABLE)$(EXE) $(TVEXE)
	rm -f $(PCCTEST)/wes-rbtest.exe
	cd $(PCCTEST); $(SAFECC) --keep=. $(DEF)x86_WIN32 $(DEF)$(PCCTYPE) \
                 $(DOOPT) \
                 $(INC)$(PCCDIR)/src \
                 ../small2/wes-rbtest.c \
                 $(EXEOUT)wes-rbtest.exe
	$(PCCTEST)/wes-rbtest.exe

wes-hashtest: test/small2/wes-hashtest.c $(EXECUTABLE)$(EXE) $(TVEXE)
	rm -f $(PCCTEST)/wes-hashtest.exe
	cd $(PCCTEST); $(SAFECC) --keep=. $(DEF)x86_WIN32 $(DEF)$(PCCTYPE) \
                 $(DOOPT) \
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
