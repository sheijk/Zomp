#
# Makefile to run the test suite
#

include ../makefile.config

################################################################################
# Targets
################################################################################

all: sexpr.test structs.test minimal.test simple-func.test variables.test
fail: crash.test

clean:
	rm -f *.bc *.o.bc *.ll *.test gmon.out

clear-results:
	rm -f *.test

redo: clear-results all

################################################################################
# Rules
################################################################################

.PRECIOUS: %.ll

%.ll: %.zomp ../zompc.native
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


