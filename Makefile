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

ifdef BOX
SRCEXT=box
else
SRCEXT=cil
endif

####### Test with PCC sources
PCCSOURCES = hash lf huffman x86/x86SE extensions/javaclasses
ifdef BOX
PCCBOX=-boxout ../test/PCC/$(notdir $*).box
endif
PCCTEST=../test/PCC
testpcc: $(PCCSOURCES:%=testpcc/%)
testpcc/% : ../../Source/Touchstone/PCC/src/%.c $(EXECUTABLE)$(EXE) 
	$(EXECUTABLE)$(EXE) $(EXTRAARGS) -verbose \
           -p "gcc -E %i -Dx86_WIN32 -D_GNUCC -o %o" -cabsindent 2 \
           -cabsout $(PCCTEST)/$(notdir $*).cabs \
           -cilout $(PCCTEST)/$(notdir $*).cil \
           $(PCCBOX) \
           ../../Source/Touchstone/PCC/src/$*.c

testhash: testpcc/hash $(EXECUTABLE)$(EXE)
	make testfile/$(PCCTEST)/main.c \
            TESTARGS="-p \"gcc -Dx86_WIN32 -D_GNUCC -I../../Source/Touchstone/PCC/src -E %i -o %o\""
	gcc -x c -O3 -o $(PCCTEST)/hashtest.exe \
               $(PCCTEST)/hash.$(SRCEXT) \
               $(PCCTEST)/main.$(SRCEXT)

############ Small tests
ifdef BOX
SMALL1BOX=-boxout ../test/small1/$(notdir $*).box
endif
test/% : ../test/small1/$* $(EXECUTABLE)$(EXE)
	$(EXECUTABLE)$(EXE) $(EXTRAARGS) -verbose \
           -p "gcc -E %i -o %o" -cabsindent 2 \
           -cabsout ../test/small1/$(notdir $*).cabs \
	   -cilout ../test/small1/$(notdir $*).cil \
           $(SMALL1BOX) \
           ../test/small1/$*.c
	gcc -x c -c -O3 -S ../test/small1/$(notdir $*).$(SRCEXT) \
             -o ../test/small1/$(notdir $*).s


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
ifdef BOX
LINUXBOX=-boxout ../test/linux/$(notdir $*).box
endif
testlinux/% : $(EXECUTABLE)$(EXE) ../test/linux/%.cpp
	$(EXECUTABLE)$(EXE) $(EXTRAARGS) -verbose \
           -P -cabsindent 2 \
	   -cabsout ../test/linux/$(notdir $*).cabs \
           -cilout ../test/linux/$(notdir $*).cil \
	   $(LINUXBOX) \
           ../test/linux/$*.cpp

testqp : testlinux/qpmouse

