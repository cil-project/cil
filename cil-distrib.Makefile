# Makefile to build the CIL distribution
#----------------------------------------------------------------------

# Debugging. Set ECHO= to debug this Makefile 
ECHO = @

SOURCEDIRS  = src src/frontc
OBJDIR      = obj
MLLS        = clexer.mll
MLYS        = cparser.mly
MODULES     = util pretty errormsg trace stats clist \
              cabs cprint clexer cparser cabsvisit \
              cil cabs2cil frontc logcalls logwrites \
	      heapify main

OCAML_CIL_LIB_MODULES = $(MODULES:main=)

EXECUTABLE  = $(OBJDIR)/cilly
CAMLUSEUNIX = 1
ifdef RELEASE
UNSAFE      = 1
endif
CAMLLIBS    = 


# Additional things to clean
EXTRACLEAN += $(OBJDIR)/*.obj $(OBJDIR)/*.a $(OBJDIR)/*.o

    # Include now the common set of rules for OCAML
    # This file will add the rules to make $(EXECUTABLE)$(EXE), which 
    # will be the default target
include Makefile.ocaml

cillib: $(OBJDIR)/cil.$(CMXA)

$(OBJDIR)/cil.$(CMXA): $(OCAML_CIL_LIB_MODULES:%=$(OBJDIR)/%.$(CMO))
	$(CAMLLINK) -a -o $@ $^


