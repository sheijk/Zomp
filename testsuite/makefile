#
# Makefile to run the test suite
#

include ../makefile.config

################################################################################
# Targets
################################################################################

all: minimal.test simple-func.test
fail: crash.test

clean:
	rm -f *.bc *.o.bc *.ll *.test

clear-results:
	rm -f *.test

redo: clear-results all

################################################################################
# Rules
################################################################################

.SUFFIXES: .zomp .ll .bc .o.bc .test
.PRECIOUS: %.ll

.zomp.ll:
	@$(ECHO) Compiling $(<) to .ll...
	@../zomp-check-syntax.sh $<

# .ll.bc:
# 	@$(ECHO) Compiling $(<) to .bc...
# 	@llvm-as -f $< -o $@

.ll.bc:
	@echo Compiling $< to $@
	@llvm-as -f $< -o $(<:.ll=.o.bc)
	@llvm-link -f ../stdlib.bc $(<:.ll=.o.bc) -o $@

.bc.test:
	@$(ECHO) Running test $(<:.bc=)...
	@lli $<
	@touch $@


