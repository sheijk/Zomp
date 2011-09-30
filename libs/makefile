#
# Makefile to build libs. Only contains targets for testing compilation of libs
# atm, as the libs will not be compiled seperately
#

ifneq "$(ZOMP_MAIN_MAKEFILE)" "1"
$(error "Call make from the main zomp dir, only (examples)")
endif

ZOMP_LIBS_SRC = $(wildcard libs/*.zomp)

libs/test: all $(ZOMP_LIBS_SRC:.zomp=.ll)

# not using ZOMP_LIBS_SRC w/ repl. to avoid accidental deletion of source files if it fails
libs/clean:
	$(RM) -f $(wildcard libs/*.ll)

libs/%.ll: libs/%.zomp has_llvm
	$(ECHO) Compiling $(<) to .ll...
	$(ZOMPC) -c $< $(ZOMPCFLAGS) || rm -f $@

