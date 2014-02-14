#
# Makefile to build libs. Only contains targets for testing compilation of libs
# atm, as the libs will not be compiled seperately
#

GENERATED_LIBRARY_SKELS = $(wildcard libs/*.skel)
GENERATED_LIBRARY_SOURCES = $(GENERATED_LIBRARY_SKELS:.skel=.zomp) libs/opengl20print.zomp
ZOMP_LIBS_SRC = $(wildcard libs/*.zomp) $(GENERATED_LIBRARY_SOURCES)

.PHONY: libs/all
ALL_TARGETS += libs/all
libs/all: $(ZOMPC_FILE) libbindings $(ZOMP_LIBS_SRC:.zomp=.ll)

TEST_SUB_TARGETS += libs/test
.PHONY: libs/test
libs/test: libs/all

CLEAN_SUB_TARGETS += libs/clean
.PHONY: libs/clean
# not using ZOMP_LIBS_SRC w/ repl. to avoid accidental deletion of source files if it fails
libs/clean:
	$(DELETE_FILE) $(GENERATED_LIBRARY_SOURCES)
	$(DELETE_FILE) $(ZOMP_LIBS_SRC:.zomp=.ll)
	$(DELETE_FILE) $(ZOMP_LIBS_SRC:.zomp=.compile_output)
	$(DELETE_FILE) $(ZOMP_LIBS_SRC:.zomp=.result)

