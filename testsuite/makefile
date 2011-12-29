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
 simple-func.zomp variables.zomp astmatch.zomp cee.zomp                     \
 bug-macro-func-samemodule.zomp strings.zomp prelude.zomp selftest.zomp     \
 float.zomp std_bindings.zomp indent.comments.zomp require.zomp arrays.zomp \
 testframework.zomp math.zomp stackoverflow.zomp std_base.zomp              \
 numeric_literals.zomp std_base_struct_literals.zomp nested_macro.zomp      \
 libcee_enum.zomp std_base_globalVar.zomp paramtypes.zomp pointers.zomp     \
 varargs.zomp

TESTSUITE_IGNORED_SOURCES = crash.zomp fail.zomp \
 std_base_struct_literals_errors.zomp builtin_is_interactive.zomp empty.zomp \
 preludevalid.zomp require_lib.zomp

TESTSUITE_CASES = $(foreach FILE, $(TESTSUITE_SOURCES), testsuite/$(FILE:.zomp=.test))

.PHONY: testsuite/selftest
testsuite/selftest:
	echo $(foreach TEST_FILE,$(TESTSUITE_SOURCES) $(TESTSUITE_IGNORED_SOURCES), "\ntestsuite/$(TEST_FILE)") | sort > tests.tmp.txt
	echo > files.tmp.txt
	ls testsuite/*.zomp | sort >> files.tmp.txt
	-diff -U 0 -b files.tmp.txt tests.tmp.txt | grep -v "^\\(@\\|---\\|+++\\)"
	diff -b files.tmp.txt tests.tmp.txt

testsuite/success: testsuite/selftest $(TESTSUITE_CASES)
testsuite/test: testsuite/selftest testsuite/success testsuite/error_reporting/test
testsuite/quick: testsuite/simple-func.test testsuite/cee.test testsuite/error_reporting/test

# # experimental, results are incorrect because failed executions and compilations
# # still produce files
# testsuite/report: testsuite/report_start $(foreach FILE, $(TESTSUITE_CASES), testsuite/$(FILE:.zomp=.print_report))
# testsuite/report_start:
# 	@echo "zomp test suite results (warning, report is incorrect!):"
# %.print_report:
# 	@./testsuite/print_test_results.sh $(@:.print_report=)

testsuite/fail: fail.test crash.test

.PHONY: testsuite/clean testsuite/clear-results testsuite/redo testsuite/test

CLEAN_SUB_TARGETS += testsuite/clean
testsuite/clean:
	cd testsuite && rm -f *.bc *.o.bc *.ll *.s *.o *.exe *.test gmon.out

################################################################################
# Additional dependencies
################################################################################

testsuite/cee.ll: libs/libcee.zomp libs/unittest.zomp
testsuite/astmatch.ll: libs/libcee.zomp
testsuite/math.ll: libs/math.zomp

################################################################################
# Rules
################################################################################

.PRECIOUS: testsuite/%.ll testsuite/%.s

# testsuite/%.ll: testsuite/%.zomp zompc.native prelude.zomp $(ZOMPC) testsuite/preludevalid.test libs/unittest.zomp libs/libcee.zomp libs/basic_ops.zomp has_llvm
testsuite/%.ll: testsuite/%.zomp prelude.zomp $(ZOMPC) libs/unittest.zomp libs/libcee.zomp libs/basic_ops.zomp testsuite/prelude_is_valid prelude.zomp source/runtime.c testsuite/makefile

# the same w/o dependency on itself
testsuite/preludevalid.ll: testsuite/preludevalid.zomp prelude.zomp $(ZOMPC)

# Additional rule to avoid make getting stuck for unknown reason. If
# testsuite/%.ll depends on testsuite/preludevalid.test testsuite/%.bc will
# fail, if the dependency is done indirectly it does work
testsuite/prelude_is_valid: testsuite/preludevalid.test
	$(TOUCH) $@

testsuite/%.test: testsuite/%.exe
	@$(ECHO) Running test $(<:.bc=) ...
	./$(@:.test=.exe)
	touch $@

################################################################################
# Validating error location reporting
################################################################################

TESTSUITE_ERROR_SOURCES = $(wildcard testsuite/error_reporting/*.zomp)
TESTSUITE_ERROR_CASES = $(TESTSUITE_ERROR_SOURCES:.zomp=.testreport)

testsuite/error_reporting/test: $(TESTSUITE_ERROR_CASES)
.PHONY: testsuite/error_reporting/test

CLEAN_SUB_TARGETS += testsuite/error_reporting/clean
testsuite/error_reporting/clean:
	cd testsuite/error_reporting && rm -f *.ll *.bc *.testreport *.exe *.output
.PHONY: testsuite/error_reporting/clean

CHECK_TEST_FILE = testsuite/check_test.ml
CHECK_TEST = $(OCAML) str.cma $(CHECK_TEST_FILE)

PRINT_TESTREPORT = 0

testsuite/check_test_verify/works_fine.testreport: testsuite/check_test_verify/test_check_test_error_report.zomp testsuite/makefile
	$(CHECK_TEST) ${<:.zomp=.testreport} 2>&1 > $@.tmp
	cat $@.tmp | grep 'testsuite/check_test_verify/test_check_test_error_report.zomp:2:' | grep warning | grep invalid > /dev/null
	mv $@.tmp $@

.PRECIOUS: %.ll %.bc %.exe

%.test_output: %.exe %.testreport
	@$(ECHO) Running test $(<:.exe=)
	$< > $@

testsuite/error_reporting/%.testreport: testsuite/error_reporting/%.zomp $(ZOMPC) $(CHECK_TEST_FILE) testsuite/makefile source/runtime.ll
	@$(ECHO) "Running unit test ${@:.testreport=} ..."
	$(CHECK_TEST) $@
ifeq "$(PRINT_TESTREPORT)" "1"
	echo "--- Content of $@"
	cat $@
	echo "--- end of $@"
endif

# Running test disabled for now. TODO: unify .test and .testreport and always
# store the output of zompc builds into a file

# 	$(LLVM_LINK) -o $(@:.testreport=-tmp.bc) source/runtime.ll $(<:.zomp=.ll)
# 	$(LLVM_LLI) $(@:.testreport=-tmp.bc) > $(@:.testreport=.output)
# ifeq "$(PRINT_OUTPUT)" "1"
# 	echo "--- Output of $@"
# 	cat $(@:.testreport=.output)
# 	echo "--- end of $@"
# endif

