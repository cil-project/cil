# Makefile building and using the CCured compiler
# author: George Necula
# hacks here and there by Wes and Scott

# this Makefile makes use of several GNU Make extensions; see
#   http://www.gnu.org/manual/make/html_chapter/make_toc.html

# tweak to cause commit to think this is different

ifneq ($(ARCHOS), x86_WIN32)
ifneq ($(ARCHOS), x86_LINUX) 
   $(error You must set the ARCHOS variable to either x86_WIN32 or x86_LINUX)
endif
endif

# sm: moved this before setup and am now arranging things so this
# can be the primary target; must rethink what 'setup' means
# CIL must be made before CCURED
quickbuild:
	make -r -f Makefile.cil      quickbuild $(MAKEOVERRIDES)
	make -r -f Makefile.ccured   quickbuild $(MAKEOVERRIDES)

setup:
	make -r -f Makefile.cil      setup $(MAKEOVERRIDES)
	make -r -f Makefile.ccured   setup $(MAKEOVERRIDES)




# sm: find and remove all the intermediate files from translation
# sm: removed *box.c from those removed since test/PCC/src/pccbox.c should be kept
clean:
	-make -f Makefile.ccured clean CLEANING=1 $(MAKEOVERRIDES)
	rm -rf cil.tar.gz
	-find test \( \
		-name '*cil.c' -o \
		-name '*cured.c' -o \
                -name '*cured.*optim.c' -o \
		-name '*.exe' -o \
		-name '*.i' -o \
		-name '*_ppp.c' -o \
		-name '*.origi' -o \
		-name '*.o' -o \
		-name '*.obj' -o \
		-name '*cabs.c' -o \
		-name '*infer.c' -o \
		-name '*_all*.c' -o \
		-name '*_comb*.c' \
	\) -exec rm {} \;

# build ocamldoc documentation tree

.PHONY: doc cil-distrib quickbuild setup clean
doc:
	make -f Makefile.cil doc $(MAKEOVERRIDES)
	make -f Makefile.ccured doc $(MAKEOVERRIDES)

cil-distrib:
	make -f Makefile.cil cil-distrib $(MAKEOVERRIDES)

ccured-distrib:
	make -f Makefile.ccured ccured-distrib $(MAKEOVERRIDES)


##
## The test cases have been moved to test/Makefile
##