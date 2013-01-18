#
# Makefile to run the test suite. This file is included by ../makefile, thus all
# targets need to be prefixed with testsuite/
#

# Print output of test case runs to console. If this is 0 only errors will be
# printed.
PRINT_TESTREPORT = 0

################################################################################
# Targets
#
# testsuite/test - will run all tests
# testsuite/$dir/all - will build all targets in that directory
################################################################################

TESTSUITE_SUBDIRS = \
  simple fundamental \
  check_test_verify error_reporting generics include lexer libs parser \
  source_locations std std_base zompsh

TESTSUITE_IGNORED_SOURCES = preludevalid.zomp

TESTSUITE_SOURCES = $(wildcard $(TESTSUITE_SUBDIRS:%=testsuite/%/*.zomp))
TESTSUITE_CASES = $(TESTSUITE_SOURCES:.zomp=.testreport)

testsuite/%/all:
	@$(ECHO) "Running tests in $@ ..."
	$(MAKE) $(foreach SOURCE, $(wildcard testsuite/$(*)/*.zomp), $(SOURCE:.zomp=.testreport))

.PHONY: testsuite/success
testsuite/success: testsuite/selftest $(TESTSUITE_CASES)
.PHONY: testsuite/test
testsuite/test: testsuite/selftest $(TESTSUITE_SUBDIRS:%=testsuite/%/all)
.PHONY: testsuite/quick
testsuite/quick: testsuite/simple-func.testreport testsuite/libcee_misc.testreport

.PHONY: testsuite/selftest
testsuite/selftest: $(BUILD_DIR)/.exists
	echo $(foreach TEST_FILE, $(TESTSUITE_SOURCES), "\n$(TEST_FILE:.testreport=.zomp)") $(foreach TEST_FILE,$(TESTSUITE_IGNORED_SOURCES), "\ntestsuite/$(TEST_FILE)") | sort > $(OUT_DIR)/tests.tmp.txt
	echo > $(OUT_DIR)/files.tmp.txt
	find testsuite -iname "*.zomp" | grep -v check_test_verify | sort >> $(OUT_DIR)/files.tmp.txt
	-diff -U 0 -b $(OUT_DIR)/files.tmp.txt $(OUT_DIR)/tests.tmp.txt | grep -v "^\\(@\\|---\\|+++\\)"
	diff -b $(OUT_DIR)/files.tmp.txt $(OUT_DIR)/tests.tmp.txt

CLEAN_SUB_TARGETS += testsuite/clean
.PHONY: testsuite/clean
testsuite/clean:
	rm -f $(TESTSUITE_CASES)
	rm -f $(TESTSUITE_CASES:.testreport=.result)
	rm -f $(TESTSUITE_CASES:.testreport=.test_output)
	rm -f $(TESTSUITE_CASES:.testreport=.ll)
	rm -f $(TESTSUITE_CASES:.testreport=.bc)
	rm -f $(TESTSUITE_CASES:.testreport=.opt-bc)
	rm -f $(TESTSUITE_CASES:.testreport=.s)
	rm -f $(TESTSUITE_CASES:.testreport=.o)
	rm -f $(TESTSUITE_CASES:.testreport=.exe)
	rm -f testsuite/check_test.annot
	rm -f testsuite/prelude_is_valid

################################################################################
# Additional dependencies
################################################################################

testsuite/libs/libcee_misc.ll: libs/libcee.zomp libs/unittest.zomp
testsuite/libs/libcee_astmatch.ll: libs/libcee.zomp
testsuite/libs/math.ll: libs/math.zomp

.PRECIOUS: %.ll
testsuite/%.ll: testsuite/%.zomp $(TESTREPORT_LIB_DEPS) $(ZOMPC) \
  testsuite/prelude_is_valid testsuite/check_test_verify/all source/runtime.c \
  testsuite/testsuite.mk makefile

# the same w/o dependency on itself
testsuite/preludevalid.ll: testsuite/preludevalid.zomp prelude.zomp $(ZOMPC)

################################################################################
# Rules
################################################################################

CHECK_TEST_FILE = testsuite/check_test.ml
CHECK_TEST = $(OCAML) str.cma unix.cma $(CHECK_TEST_FILE)

TESTREPORT_COMPILE_CMD = "$(MAKE) SILENT=1 $(@:.testreport=.exe)"
TESTREPORT_RUN_CMD = "./$(@:.testreport=.exe)"

# Don't compile tests for zompsh (they have "!" commands in them and won't compile using zompc)
testsuite/zompsh/%.testreport: TESTREPORT_COMPILE_CMD=\
  "echo \"Skipping compilation of $(@:.testreport=.zomp), not needed for zompsh test\""
# Run zompsh tests using zompsh instead of the executable
testsuite/zompsh/%.testreport: TESTREPORT_RUN_CMD="cat \"$<\" testsuite/zompsh/append.txt | $(ZOMPSH)"

TESTREPORT_LIB_DEPS = prelude.zomp source/runtime.ll libs/unittest.zomp libs/libcee.zomp libs/basic_ops.zomp
TESTREPORT_DEPS = zompc.native zompsh.native $(CHECK_TEST_FILE) makefile testsuite/testsuite.mk $(TESTREPORT_LIB_DEPS)

.PRECIOUS: %.test_output
%.testreport %.result %.test_output: %.zomp $(TESTREPORT_DEPS)
	@$(ECHO) Running test suite case $< ...
	rm -f ${@:.testreport=.}{bc,op-bc,ll,exe,test_output,result,testreport}
	$(CHECK_TEST) $@ $(TESTREPORT_COMPILE_CMD) $(TESTREPORT_RUN_CMD)
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

.PHONY: testsuite/check_test_verify/all
testsuite/check_test_verify/all: testsuite/check_test_verify/test_check_test_error_report.testreport

# A simple self-test.
testsuite/check_test_verify/test_check_test_error_report.testreport: testsuite/check_test_verify/test_check_test_error_report.zomp testsuite/testsuite.mk $(CHECK_TEST_FILE)
	rm -f ${@:.testreport=.}{bc,op-bc,ll,exe,test_output}
	($(CHECK_TEST) $@ "$(MAKE) SILENT=1" 2>&1) > $@.tmp
# suppress printing of command to avoid it being recognized as an error pattern
	@(cat $@.tmp | grep 'testsuite/check_test_verify/test_check_test_error_report.zomp:2:' | grep warning | grep invalid) 2>&1 > /dev/null
	mv $@.tmp $@



