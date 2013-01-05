#
# Makefile to run the test suite. This file is included by ../makefile, thus all
# targets need to be prefixed with testsuite/
#

################################################################################
# Targets
################################################################################

TESTSUITE_SOURCES_X = overloaded_ops.zomp structs.zomp minimal.zomp         \
 simple-func.zomp variables.zomp astmatch.zomp libcee_misc.zomp             \
 bug-macro-func-samemodule.zomp strings.zomp prelude.zomp selftest.zomp     \
 float.zomp std_bindings.zomp indent.comments.zomp require.zomp arrays.zomp \
 testframework.zomp math.zomp stackoverflow.zomp std_base.zomp              \
 numeric_literals.zomp std_base_struct_literals.zomp nested_macro.zomp      \
 libcee_enum.zomp std_base_globalVar.zomp parametric_types.zomp pointers.zomp\
 varargs.zomp forward_declare_function.zomp include/relative.zomp           \
 std_base_primitive_types.zomp parametric_functions.zomp type_errors.zomp   \
 std_base_generic.zomp std_base_cast.zomp                                   \
 zmp_compiler_linkclib_error_invalid_name.zomp                              \
 zmp_compiler_linkclib_error_non_existing_lib.zomp                          \
 $(LEXER_SOURCES_X)

LEXER_SOURCES_X = \
 lexer/invalid_escape_sequence.zomp lexer/invalid_token.zomp                \
 lexer/invalid_token_bof.zomp lexer/invalid_token_multi_line_comment.zomp   \
 lexer/invalid_token_not_first_line.zomp                                    \
 lexer/invalid_token_single_line_comment.zomp                               \
 lexer/unterminated_comment.zomp lexer/unterminated_string.zomp

TESTSUITE_SOURCES = \
 $(foreach FILE, $(TESTSUITE_SOURCES_X), testsuite/$(FILE:.zomp=.testreport)) \
 $(TESTSUITE_ERROR_REPORTING_SOURCES)

TESTSUITE_ERROR_REPORTING_SOURCES = $(wildcard testsuite/error_reporting/*.zomp)

TESTSUITE_IGNORED_SOURCES = crash.zomp fail.zomp \
 std_base_struct_literals_errors.zomp builtin_is_interactive.zomp empty.zomp \
 preludevalid.zomp require_lib.zomp

TESTSUITE_CASES = $(TESTSUITE_SOURCES:.zomp=.testreport)

.PHONY: testsuite/selftest
testsuite/selftest:
	echo $(foreach TEST_FILE, $(TESTSUITE_SOURCES), "\n$(TEST_FILE:.testreport=.zomp)") $(foreach TEST_FILE,$(TESTSUITE_IGNORED_SOURCES), "\ntestsuite/$(TEST_FILE)") | sort > tests.tmp.txt
	echo > files.tmp.txt
	find testsuite -iname "*.zomp" | grep -v check_test_verify | sort >> files.tmp.txt
	-diff -U 0 -b files.tmp.txt tests.tmp.txt | grep -v "^\\(@\\|---\\|+++\\)"
	diff -b files.tmp.txt tests.tmp.txt

testsuite/success: testsuite/selftest $(TESTSUITE_CASES)
testsuite/test: testsuite/selftest testsuite/success
testsuite/quick: testsuite/simple-func.testreport testsuite/libcee_misc.testreport

.PHONY: testsuite/error_reporting/all
testsuite/error_reporting/all: $(TESTSUITE_ERROR_REPORTING_SOURCES:.zomp=.testreport)

.PHONY: testsuite/lexer/all
testsuite/lexer/all: $(foreach FILE, $(LEXER_SOURCES_X), testsuite/$(FILE:.zomp=.testreport))

# # experimental, results are incorrect because failed executions and compilations
# # still produce files
# testsuite/report: testsuite/report_start $(foreach FILE, $(TESTSUITE_CASES), testsuite/$(FILE:.zomp=.print_report))
# testsuite/report_start:
# 	@echo "zomp test suite results (warning, report is incorrect!):"
# %.print_report:
# 	@./testsuite/print_test_results.sh $(@:.print_report=)

testsuite/fail: fail.testreport crash.testreport

.PHONY: testsuite/clean testsuite/clear-results testsuite/redo testsuite/test

TESTSUITE_CLEAN_PATTERNS = \
  *.bc *.opt-bc *.ll \
  *.s *.o *.exe \
  *.testreport *.test_output *.result \
  gmon.out

CLEAN_SUB_TARGETS += testsuite/clean
testsuite/clean:
	cd testsuite && rm -f $(TESTSUITE_CLEAN_PATTERNS)
	cd testsuite/error_reporting && rm -f $(TESTSUITE_CLEAN_PATTERNS)
	cd testsuite/check_test_verify && rm -f $(TESTSUITE_CLEAN_PATTERNS)
	cd testsuite/include && rm -f $(TESTSUITE_CLEAN_PATTERNS)
	rm -f testsuite/check_test.annot
	rm -f testsuite/prelude_is_valid

################################################################################
# Additional dependencies
################################################################################

testsuite/libcee_misc.ll: libs/libcee.zomp libs/unittest.zomp
testsuite/astmatch.ll: libs/libcee.zomp
testsuite/math.ll: libs/math.zomp

testsuite/%.ll: testsuite/%.zomp prelude.zomp $(ZOMPC) libs/unittest.zomp libs/libcee.zomp libs/basic_ops.zomp testsuite/prelude_is_valid prelude.zomp source/runtime.c testsuite/testsuite.mk

# the same w/o dependency on itself
testsuite/preludevalid.ll: testsuite/preludevalid.zomp prelude.zomp $(ZOMPC)

################################################################################
# Rules
################################################################################

CHECK_TEST_FILE = testsuite/check_test.ml
CHECK_TEST = $(OCAML) str.cma unix.cma $(CHECK_TEST_FILE)

.PRECIOUS: %.test_output
%.test_output: %.exe $(ZOMPC) $(CHECK_TEST_FILE) testsuite/testsuite.mk
	@$(ECHO) Running test $(<:.exe=) ...
	$< > $@

.PRECIOUS: %.ll
testsuite/%.ll: libs/unittest.zomp libs/libcee.zomp

%.testreport %.result: %.zomp $(ZOMPC) $(CHECK_TEST_FILE) testsuite/testsuite.mk source/runtime.ll libs/unittest.zomp libs/libcee.zomp
	@$(ECHO) Running test suite case $< ...
	rm -f ${@:.testreport=.}{bc,op-bc,ll,exe,test_output,result,testreport}
	$(CHECK_TEST) $@ "$(MAKE) SILENT=1"
ifeq "$(PRINT_TESTREPORT)" "1"
	echo "--- Content of $@"
	cat $@
	echo "--- end"
	echo "--- Content of ${@:.testreport=.test_output}"
	cat ${@:.testreport=.test_output}
	echo "--- end"
endif

# Additional rule to avoid make getting stuck for unknown reason. If
# testsuite/%.ll depends on testsuite/preludevalid.testreport testsuite/%.bc will
# fail, if the dependency is done indirectly it does work
testsuite/prelude_is_valid: testsuite/preludevalid.testreport
	$(TOUCH) $@

PRINT_TESTREPORT = 0

testsuite/check_test_verify/works_fine.testreport: testsuite/check_test_verify/test_check_test_error_report.zomp testsuite/testsuite.mk
	rm -f ${@:.testreport=.}{bc,op-bc,ll,exe,test_output}
	$(CHECK_TEST) ${<:.zomp=.testreport} 2>&1 > $@.tmp
	cat $@.tmp | grep 'testsuite/check_test_verify/test_check_test_error_report.zomp:2:' | grep warning | grep invalid > /dev/null
	mv $@.tmp $@



