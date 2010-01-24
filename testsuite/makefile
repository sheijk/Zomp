#
# Makefile to run the test suite
#

ZOMP_TOOL_PATH=$(PWD)/../tools
include ../config.mk

################################################################################
# Targets
################################################################################

TEST_CASES = sexpr.zomp overloaded_ops.zomp structs.zomp minimal.zomp   \
simple-func.zomp variables.zomp astmatch.zomp cee.zomp                  \
bug-macro-func-samemodule.zomp strings.zomp prelude.zomp selftest.zomp  \
float.zomp std_bindings.zomp indent.comments.zomp require.zomp          \
arrays.zomp testframework.zomp math.zomp stackoverflow.zomp

all: $(foreach FILE, $(TEST_CASES), $(FILE:.zomp=.test))

# experimental, results are incorrect because failed executions and compilations
# still produce files
report: report_start $(foreach FILE, $(TEST_CASES), $(FILE:.zomp=.print_report))
report_start:
	@echo "zomp test suite results (warning, report is incorrect!):"
%.print_report:
	@./print_test_results.sh $(@:.print_report=)

fail: fail.test crash.test

clean:
	rm -f *.bc *.o.bc *.ll *.test gmon.out

clear-results:
	rm -f *.test

redo: clear-results all

################################################################################
# Additional dependencies
################################################################################

cee.ll: ../libs/libcee.zomp ../libs/unittest.zomp
astmatch.ll: ../libs/libcee.zomp

################################################################################
# Rules
################################################################################

.PRECIOUS: .ll
.PHONY: preludevalid

# Tests whether the prelude alone can be compiled. Is a dependency for all other
# tests to avoid having all tests run and fail because the prelude/compiler is
# completely broken
preludevalid: ../zompc.native ../zomp-check-syntax.sh ../stdlib.zomp ../stdlib.c
	@$(ECHO) Checking whether prelude compiles ...
	@rm -f $@.zomp $@.ll $@.o.bc $@.bc $@
	@echo "std:base:func int main():\n  ret 0\nend\n" > preludevalid.zomp
	@../zomp-check-syntax.sh preludevalid.zomp
	@$(LLVM_AS) -f preludevalid.ll -o preludevalid.o.bc
	@$(LLVM_LINK) -f ../stdlib.bc preludevalid.o.bc -o preludevalid.bc
	@$(LLVM_LLI) preludevalid.bc
	@touch preludevalid

%.ll: %.zomp ../zompc.native ../stdlib.zomp preludevalid
	@$(ECHO) Compiling $(<) to .ll ...
	@../zomp-check-syntax.sh $<

%.bc: %.ll ../stdlib.zomp ../stdlib.c
	@echo Compiling $< to $@
	@$(LLVM_AS) -f $< -o $(<:.ll=.o.bc)
	@$(LLVM_LINK) -f ../stdlib.bc $(<:.ll=.o.bc) -o $@

%.test: %.bc
	@$(ECHO) Running test $(<:.bc=) ...
	@$(LLVM_LLI) $<
	@touch $@

