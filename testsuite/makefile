#
# Makefile to run the test suite
#

ifneq "$(ZOMP_MAIN_MAKEFILE)" "1"
$(error "Call make from the main zomp dir, only (testsuite)")
endif

################################################################################
# Targets
################################################################################

TESTSUITE_CASES = sexpr.zomp overloaded_ops.zomp structs.zomp minimal.zomp   \
simple-func.zomp variables.zomp astmatch.zomp cee.zomp                  \
bug-macro-func-samemodule.zomp strings.zomp prelude.zomp selftest.zomp  \
float.zomp std_bindings.zomp indent.comments.zomp require.zomp          \
arrays.zomp testframework.zomp math.zomp stackoverflow.zomp

testsuite/all: $(foreach FILE, $(TESTSUITE_CASES), testsuite/$(FILE:.zomp=.test))

# # experimental, results are incorrect because failed executions and compilations
# # still produce files
# testsuite/report: testsuite/report_start $(foreach FILE, $(TESTSUITE_CASES), testsuite/$(FILE:.zomp=.print_report))
# testsuite/report_start:
# 	@echo "zomp test suite results (warning, report is incorrect!):"
# %.print_report:
# 	@./testsuite/print_test_results.sh $(@:.print_report=)

testsuite/fail: fail.test crash.test

.PHONY: testsuite/clean testsuite/clear-results testsuite/redo testsuite/all

testsuite/clean:
	cd testsuite && rm -f *.bc *.o.bc *.ll *.test gmon.out

testsuite/clear-results:
	cd testsuite && rm -f *.test

testsuite/redo: clear-results all

################################################################################
# Additional dependencies
################################################################################

testsuite/cee.ll: libs/libcee.zomp libs/unittest.zomp
testsuite/astmatch.ll: libs/libcee.zomp

################################################################################
# Rules
################################################################################

.PRECIOUS: .ll
.PHONY: testsuite/preludevalid testsuite/all

# Tests whether the prelude alone can be compiled. Is a dependency for all other
# tests to avoid having all tests run and fail because the prelude/compiler is
# completely broken
testsuite/preludevalid: $(ZOMPC) stdlib.zomp stdlib.c
ifeq "$(TESTSUITE_NO_PRELUDE)" ""
	@$(ECHO) Checking whether prelude compiles ...
	@rm -f testsuite/$@.zomp testsuite/$@.ll testsuite/$@.o.bc testsuite/$@.bc testsuite/$@
	@echo "std:base:func int main():\n  ret 0\nend\n" > testsuite/preludevalid.zomp
	@$(ZOMPC) -c testsuite/preludevalid.zomp
	@$(LLVM_AS) -f testsuite/preludevalid.ll -o testsuite/preludevalid.o.bc
	@$(LLVM_LINK) -f stdlib.bc testsuite/preludevalid.o.bc -o testsuite/preludevalid.bc
	@$(LLVM_LLI) testsuite/preludevalid.bc
	@touch $@
else
	@$(ECHO) Ignoring whether prelude compiles ...
	@$(ECHO) "Checking of prelude validity disabled by TESTSUITE_NO_PRELUDE" > $@
endif

testsuite/%.ll: testsuite/%.zomp zompc.native stdlib.zomp testsuite/preludevalid $(ZOMPC)
	@$(ECHO) Compiling $(<) to .ll ...
	@$(ZOMPC) -c $<

testsuite/%.bc: testsuite/%.ll stdlib.zomp stdlib.c
	@echo Compiling $< to $@
	@$(LLVM_AS) -f $< -o $(<:.ll=.o.bc)
	@$(LLVM_LINK) -f stdlib.bc $(<:.ll=.o.bc) -o $@

testsuite/%.test: testsuite/%.bc
	@$(ECHO) Running test $(<:.bc=) ...
	@$(LLVM_LLI) $<
	@touch $@

