#
# Makefile to build libs. Only contains targets for testing compilation of libs
# atm, as the libs will not be compiled seperately
#

ZOMP_LIBS_SRC = $(wildcard libs/*.zomp) $(GENERATED_LIBRARY_SOURCES)

.PHONY: libs/all
libs/all: $(ZOMPC) $(ZOMP_LIBS_SRC:.zomp=.ll)

TEST_SUB_TARGETS += libs/test
.PHONY: libs/test
libs/test: libs/all

CLEAN_SUB_TARGETS += libs/clean
.PHONY: libs/clean
# not using ZOMP_LIBS_SRC w/ repl. to avoid accidental deletion of source files if it fails
libs/clean:
	$(RM) -f $(wildcard libs/*.ll)

