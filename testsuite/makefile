#
# Makefile to run the test suite
#

include ../makefile.config

################################################################################
# Targets
################################################################################

TEST_CASES = sexpr.zomp overloaded_ops.zomp structs.zomp minimal.zomp   \
simple-func.zomp variables.zomp astmatch.zomp cee.zomp                  \
bug-macro-func-samemodule.zomp strings.zomp prelude.zomp selftest.zomp  \
float.zomp std_bindings.zomp indent.comments.zomp

all: $(foreach FILE, $(TEST_CASES), $(FILE:.zomp=.test))

fail: fail.test crash.test

clean:
	rm -f *.bc *.o.bc *.ll *.test gmon.out

clear-results:
	rm -f *.test

redo: clear-results all

################################################################################
# Additional dependencies
################################################################################

cee.ll: ../libs/libcee.zomp
astmatch.ll: ../libs/libcee.zomp

################################################################################
# Rules
################################################################################

.PRECIOUS: %.ll

%.ll: %.zomp ../zompc.native ../stdlib.zomp
	@$(ECHO) Compiling $(<) to .ll...
	@../zomp-check-syntax.sh $<

%.bc: %.ll ../stdlib.zomp ../stdlib.c
	@echo Compiling $< to $@
	@llvm-as -f $< -o $(<:.ll=.o.bc)
	@llvm-link -f ../stdlib.bc $(<:.ll=.o.bc) -o $@

%.test: %.bc
	@$(ECHO) Running test $(<:.bc=)...
	@lli $<
	@touch $@

################################################################################
# Additional Dependencies
################################################################################

astmatch.ll: ../libs/libcee.zomp

