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
MODULES     = pretty errormsg stats cil
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


####### Test with PCC sources
PCCSOURCES = hash lf huffman x86/x86SE extensions/javaclasses
testpcc: $(PCCSOURCES:%=testpcc/%)
testpcc/% : ../../Source/Touchstone/PCC/src/%.c $(EXECUTABLE)$(EXE) 
	$(EXECUTABLE)$(EXE) -verbose \
           -p "gcc -E %i -Dx86_WIN32 -D_GNUCC -o %o" -cabsindent 2 \
           -cabsout ../test/PCC/$(notdir $*).cabs \
           -o ../test/PCC/$(notdir $*).cil \
           ../../Source/Touchstone/PCC/src/$*.c



############ Small tests
testlist : $(EXECUTABLE)$(EXE)
	$(EXECUTABLE)$(EXE) -verbose \
           -p "gcc -E %i -o %o" -cabsindent 2 \
           -cabsout ../test/small1/list.cabs \
	   -o ../test/small1/list.cil \
           ../test/small1/list.c

testfile/% : $(EXECUTABLE)$(EXE) $*
	$(EXECUTABLE)$(EXE) -v -V $*

testdir/% : $(EXECUTABLE)$(EXE)
	make -C CC="perl safecc.pl" $*


################## Linux device drivers
testlinux/% : $(EXECUTABLE)$(EXE) ../test/linux/%.cpp
	$(EXECUTABLE)$(EXE) -verbose \
           -P -cabsindent 2 \
	   -cabsout ../test/linux/$(notdir $*).cabs \
           -o ../test/linux/$(notdir $*).cil \
           ../test/linux/$*.cpp

testqp : testlinux/qpmouse

