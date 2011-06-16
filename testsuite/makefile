#
# Makefile to run the test suite. This file is included by ../makefile, thus all
# targets need to be prefixed with testsuite/
#

ifneq "$(ZOMP_MAIN_MAKEFILE)" "1"
$(error "Call make from the main zomp dir, only (testsuite)")
endif

################################################################################
# Targets
################################################################################

TESTSUITE_SOURCES = overloaded_ops.zomp structs.zomp minimal.zomp           \
simple-func.zomp variables.zomp astmatch.zomp cee.zomp                      \
bug-macro-func-samemodule.zomp strings.zomp prelude.zomp selftest.zomp      \
float.zomp std_bindings.zomp indent.comments.zomp require.zomp arrays.zomp  \
testframework.zomp math.zomp stackoverflow.zomp

TESTSUITE_CASES = $(foreach FILE, $(TESTSUITE_SOURCES), testsuite/$(FILE:.zomp=.test))

testsuite/success: $(TESTSUITE_CASES)
testsuite/all: testsuite/success testsuite/error_reporting/all
testsuite/quick: testsuite/simple-func.test testsuite/cee.test testsuite/error_reporting/all

# # experimental, results are incorrect because failed executions and compilations
# # still produce files
# testsuite/report: testsuite/report_start $(foreach FILE, $(TESTSUITE_CASES), testsuite/$(FILE:.zomp=.print_report))
# testsuite/report_start:
# 	@echo "zomp test suite results (warning, report is incorrect!):"
# %.print_report:
# 	@./testsuite/print_test_results.sh $(@:.print_report=)

testsuite/fail: fail.test crash.test

.PHONY: testsuite/clean testsuite/clear-results testsuite/redo testsuite/all

testsuite/clean: testsuite/error_reporting/clean
	cd testsuite && rm -f *.bc *.o.bc *.ll *.test gmon.out

################################################################################
# Additional dependencies
################################################################################

testsuite/cee.ll: libs/libcee.zomp libs/unittest.zomp
testsuite/astmatch.ll: libs/libcee.zomp

################################################################################
# Rules
################################################################################

.PRECIOUS: testsuite/%.ll
.PHONY: testsuite/all

# Tests whether the prelude alone can be compiled. Is a dependency for all other
# tests to avoid having all tests run and fail because the prelude/compiler is
# completely broken
testsuite/preludevalid: $(ZOMPC) prelude.zomp source/runtime.c
ifeq "$(TESTSUITE_NO_PRELUDE)" ""
	@$(ECHO) Checking whether prelude compiles ...
	@rm -f testsuite/$@.zomp testsuite/$@.ll testsuite/$@.o.bc testsuite/$@.bc testsuite/$@
	@echo "std:base:func int main():\n  std:base:ret 0\nend\n" > testsuite/preludevalid.zomp
	$(ZOMPC) -c testsuite/preludevalid.zomp
	$(LLVM_AS) -f testsuite/preludevalid.ll -o testsuite/preludevalid.o.bc
	$(LLVM_LINK) -f source/runtime.bc testsuite/preludevalid.o.bc -o testsuite/preludevalid.bc
	$(LLVM_LLI) testsuite/preludevalid.bc
	touch $@
else
	@$(ECHO) Ignoring whether prelude compiles ...
	@$(ECHO) "Checking of prelude validity disabled by TESTSUITE_NO_PRELUDE" > $@
endif

testsuite/%.ll: testsuite/%.zomp zompc.native prelude.zomp testsuite/preludevalid $(ZOMPC)
	@$(ECHO) Compiling $(<) to .ll ...
	$(ZOMPC) -c $<

testsuite/%.bc: testsuite/%.ll prelude.zomp source/runtime.c
	@echo Compiling $< to $@
	$(LLVM_AS) -f $< -o $(<:.ll=.o.bc)
	$(LLVM_LINK) -f source/runtime.bc $(<:.ll=.o.bc) -o $@

testsuite/%.test: testsuite/%.bc
	@$(ECHO) Running test $(<:.bc=) ...
	$(LLVM_LLI) $<
	touch $@

# see also examples/makefile
testsuite/%.s: testsuite/%.bc
	@$(ECHO) Assembling $@ ...
	$(LLVM_LLC) -o $@ -march=x86 $<
# testsuite/%.o: testsuite/%.s
# 	@$(ECHO) Making $@ ...
# 	$(AS) -o $@ $< -arch i386
# testsuite/runtime.s: runtime.zomp
# 	@$(ECHO) Making $@ ...
# 	$(LLVM_LLC) -o $@ runtime.bc -f -march=x86
# testsuite/%.exe: testsuite/%.o runtime.o
# 	@$(ECHO) Making $@ ...
# 	$(CC) $(CFLAGS) -o $@ -L. -L./testsuite $(LIBS) $< -arch i386


################################################################################
# Validating error location reporting
################################################################################

TESTSUITE_ERROR_SOURCES = $(wildcard testsuite/error_reporting/*.zomp)
TESTSUITE_ERROR_CASES = $(TESTSUITE_ERROR_SOURCES:.zomp=.testreport)

testsuite/error_reporting/all: $(TESTSUITE_ERROR_CASES)
.PHONY: testsuite/error_reporting/all

testsuite/error_reporting/clean:
	cd testsuite/error_reporting && rm -f *.ll *.bc *.testreport *.exe
.PHONY: testsuite/error_reporting/clean

.PRECIOUS: testsuite/error_reporting/%.zompc-log

CHECK_TEST_FILE = testsuite/check_test.ml
CHECK_TEST = $(OCAML) str.cma $(CHECK_TEST_FILE)

PRINT_TESTREPORT = 0

testsuite/check_test_verify/works_fine.testreport: testsuite/check_test_verify/test_check_test_error_report.zomp testsuite/makefile
	$(CHECK_TEST) ${<:.zomp=.testreport} 2>&1 > $@.tmp
	cat $@.tmp | grep 'testsuite/check_test_verify/test_check_test_error_report.zomp:2:' | grep warning | grep invalid > /dev/null
	mv $@.tmp $@

testsuite/error_reporting/%.testreport: testsuite/error_reporting/%.zomp $(ZOMPC) $(CHECK_TEST_FILE) testsuite/makefile
	@$(ECHO) "Running unit test ${@:.testreport=} ..."
	$(CHECK_TEST) $@
ifeq "$(PRINT_TESTREPORT)" "1"
	echo "--- Content of $@"
	cat $@
	echo "--- end of $@"
endif

