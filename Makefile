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
MODULES     = pretty errormsg stats cil box
EXECUTABLE  = $(OBJDIR)/spec
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
include Makefile.ocaml

.PHONY : spec
spec : $(EXECUTABLE)$(EXE)

export EXTRAARGS
export BOX
_MSVC = 1			# Use the MSVC compiler

ifdef BOX
SRCEXT=box
else
SRCEXT=cil
endif

ifdef _GNUCC
CCL=gcc -x c -O3 -Wall
CC=$(CC) -c
CONLY=-c
OUT=-o
EXEOUT=-o
DEF=-D
ASMONLY=-S -o
CPPSTART=gcc -E %i -Dx86_WIN32 -D_GNUCC
CPPOUT=-o %o
CPP=$(CPPSTART) $(CPPOUT)
INC=-I
endif


ifdef _MSVC
CCL=cl /TC /O2 /Zi /MLd /I./lib /DEBUG
CC=$(CCL) /c
CONLY=/c
OUT=/Fo
EXEOUT=/Fe
DEF=/D
ASMOUT=/Fa
INC=/I
CPPSTART=cl /Dx86_WIN32 /D_MSVC /E /TC /I./lib /FI fixup.h /DBEFOREBOX
CPPOUT= %i >%o
CPP=$(CPPSTART) $(CPPOUT)
EXTRAARGS += -msvc
endif

ifdef BOX
CPPSTART += /FI safec.h
CCL += /FI safec.h
endif

SAFECC=perl /Necula/SafeC/cil/lib/safecc.pl --cabs --cil
ifdef BOX
SAFECC+= --box
endif
ifdef RELEASE
SAFECC+= --release
endif
####### Test with PCC sources
PCCSOURCES = hash lf huffman x86/x86SE extensions/javaclasses
PCCTEST=test/PCC
testpcc: $(PCCSOURCES:%=testpcc/%)
testpcc/% : ../../Source/Touchstone/PCC/src/%.c $(EXECUTABLE)$(EXE) 
	$(SAFECC) --keep=$(PCCTEST) /Dx86_WIN32 /D_DEBUG /c \
                  ../../Source/Touchstone/PCC/src/$*.c \
                  /Fo$(PCCTEST)/$(notdir $*).o

testhash: $(PCCTEST)/main.c $(EXECUTABLE)$(EXE)
	$(SAFECC) --keep=$(PCCTEST) /Dx86_WIN32 \
                 /I../../Source/Touchstone/PCC/src \
                 ../../Source/Touchstone/PCC/src/hash.c \
                 ../../Source/Touchstone/PCC/src/redblack.c \
                 $(PCCTEST)/hashtest.c \
                 /Fe$(PCCTEST)/hashtest.exe

testallpcc: $(EXECUTABLE)$(EXE)
	-rm ../../Source/Touchstone/PCC/x86_WIN32_MSVC/_DEBUG/*.o
	make -C ../../Source/Touchstone/PCC \
             CC="$(SAFECC) --keep=D:/Necula/SafeC/cil/test/PCC /c" \
             USE_JAVA=1 USE_JUMPTABLE=1 TYPE=_DEBUG \
	     defaulttarget

############ Small tests
SMALL1=test/small1
test/% : $(SMALL1)/%.c $(EXECUTABLE)$(EXE)
	$(SAFECC) $(SMALL1)/$*.c $(EXEOUT)$(SMALL1)/$*.exe


### Generic test
ifdef BOX
TESTBOX=-boxout $(basename $*).box
endif
testfile/% : $(EXECUTABLE)$(EXE) %
	$(EXECUTABLE)$(EXE) $(EXTRAARGS) -verbose \
             -cabsindent 2 \
             -cabsout $(basename $*).cabs \
	     -cilout $(basename $*).cil \
             $(TESTBOX) \
             $(TESTARGS) \
             $*


testdir/% : $(EXECUTABLE)$(EXE)
	make -C CC="perl safecc.pl" $*


################## Linux device drivers
testlinux/% : $(EXECUTABLE)$(EXE) test/linux/%.cpp
	$(SAFECC) -o test/linux/$*.o \
                  test/linux/$*.cpp 

testqp : testlinux/qpmouse

