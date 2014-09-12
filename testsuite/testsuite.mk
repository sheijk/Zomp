#
# Makefile to run the test suite. This file is included by ../makefile, thus all
# targets need to be prefixed with testsuite/
#

# Print output of test case runs to console. If this is 0 only errors will be
# printed.
PRINT_TESTREPORT = 0

CHECK_TEST = $(OUT_DIR)/testsuite/check_test
ALL_TARGETS += $(CHECK_TEST)
FILES_TO_DELETE_ON_CLEAN += testsuite/check_test{.cmi,.cmo,.cmx,.o,}
$(CHECK_TEST): CAML_OBJS += source/common source/basics
$(CHECK_TEST): CAML_LIBS += str unix bigarray

ALL_TARGETS += testsuite/zomp_source_to_html
FILES_TO_DELETE_ON_CLEAN += testsuite/zomp_source_to_html{.cmx,.cmi,.cmo,.o,}
testsuite/zomp_source_to_html: CAML_OBJS += source/common source/basics
testsuite/zomp_source_to_html: CAML_LIBS += str unix bigarray

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

TESTSUITE_SOURCES = $(wildcard $(TESTSUITE_SUBDIRS:%=testsuite/%/test_*.zomp))
TESTSUITE_CASES = $(TESTSUITE_SOURCES:.zomp=.testreport)

.PHONY: $(BUILD_DIR)/testsuite/summary.txt
$(BUILD_DIR)/testsuite/summary.txt: $(BUILD_DIR)/report.html
	@$(ECHO) Generating $@ ...
	$(PERL) -0777 -p -e 's#    <th>&nbsp;&nbsp;&nbsp;&nbsp;<a href="../../(.*\.zomp)">.*</a></th>\n    <th class="[a-z]+">([a-z]+)</th>#XXXX\1=\2#g' $(BUILD_DIR)/report.html |grep XXXX |sed 's/XXXX//' > $@

testsuite/%/all: $(CHECK_TEST)
	@$(ECHO) "Running tests in $@ ..."
	if [ ! -d $(@:all=) ]; then echo "$(@:all=):0: error: directory does not exist"; exit 1; fi
	$(MAKE) $(foreach SOURCE, $(wildcard testsuite/$(*)/test_*.zomp), $(SOURCE:.zomp=.testreport))

.PHONY: testsuite/success
testsuite/success: $(CHECK_TEST) $(TESTSUITE_CASES)

TEST_SUB_TARGETS += testsuite/test
.PHONY: testsuite/test
testsuite/test: $(CHECK_TEST) $(TESTSUITE_SUBDIRS:%=testsuite/%/all)

.PHONY: testsuite/quick
testsuite/quick: testsuite/fundamental/simple-func.testreport testsuite/libs/libcee_misc.testreport testsuite/zompsh/std_vm_zompsh.testreport

CLEAN_SUB_TARGETS += testsuite/clean
.PHONY: testsuite/clean
testsuite/clean:
	@$(ECHO) "Cleaning testsuite ..."
	@$(DELETE_FILE) -f $(TESTSUITE_CASES)
	@$(DELETE_FILE) -f $(TESTSUITE_CASES:.testreport=.result)
	@$(DELETE_FILE) -f $(TESTSUITE_CASES:.testreport=.last_result)
	@$(DELETE_FILE) -f $(TESTSUITE_CASES:.testreport=.test_output)
	@$(DELETE_FILE) -f $(TESTSUITE_CASES:.testreport=.compile_output)
	@$(DELETE_FILE) -f $(TESTSUITE_CASES:.testreport=.compile_stats)
	@$(DELETE_FILE) -f $(TESTSUITE_CASES:.testreport=.ll)
	@$(DELETE_FILE) -f $(TESTSUITE_CASES:.testreport=.bc)
	@$(DELETE_FILE) -f $(TESTSUITE_CASES:.testreport=.opt-bc)
	@$(DELETE_FILE) -f $(TESTSUITE_CASES:.testreport=.s)
	@$(DELETE_FILE) -f $(TESTSUITE_CASES:.testreport=.o)
	@$(DELETE_FILE) -f $(TESTSUITE_CASES:.testreport=.exe)
	@$(DELETE_FILE) -f testsuite/prelude_is_valid
	@$(DELETE_FILE) -f testsuite/zompsh/no_zompsh_test_*.zomp
	@$(DELETE_FILE) -f testsuite/check_test_verify/test_check_test_error_report.testreport.tmp

################################################################################
# Additional flags
################################################################################

testsuite/std/test_std_compiler_link_lib.exe: override LIBS += $(LINK_UTILS)
testsuite/std/test_std_compiler_link_gl_lib.exe: override LIBS += $(GL_LIBS)

testsuite/zompsh/test_link_lib.exe: override LIBS += $(LINK_UTILS)
testsuite/zompsh/test_link_gl_lib.exe: override LIBS += $(GL_LIBS)

################################################################################
# Additional dependencies
################################################################################

testsuite/libs/libcee_misc.ll: libs/libcee.zomp libs/unittest.zomp
testsuite/libs/libcee_astmatch.ll: libs/libcee.zomp
testsuite/libs/math.ll: libs/math.zomp

.PRECIOUS: %.ll
testsuite/%.ll: testsuite/%.zomp $(TESTREPORT_LIB_DEPS) $(ZOMPC_FILE) \
  testsuite/prelude_is_valid testsuite/check_test_verify/all source/runtime.c \
  testsuite/testsuite.mk makefile

testsuite/std/test_std_compiler_link_lib.ll: libs/libutils.dylib
testsuite/std/test_std_compiler_link_gl_lib.ll: libs/opengl20.zomp libs/glfw.zomp

# the same w/o dependency on itself
testsuite/preludevalid.ll: testsuite/preludevalid.zomp source/prelude.zomp $(ZOMPC_FILE)

################################################################################
# Rules
################################################################################

# Filter '!' commands and run through zompc for zompsh tests
testsuite/zompsh/%.ll: testsuite/zompsh/%.zomp $(ZOMPC_FILE) $(OUT_DIR)/has_llvm
	$(ECHO) "Compiling $(<) to .ll (removing zompsh commands)..."
	./testsuite/zompc_skip_zompsh_commands.sh $(ZOMPC) -c $< $(ZOMPCFLAGS)

TESTREPORT_DO_NOT_COMPILE_CMD = "echo \"Skipping compilation of $(@:.testreport=.zomp), this file is expected to fail in zompc\""
testsuite/zompsh/test_redefine_function.testreport: TESTREPORT_COMPILE_CMD=$(TESTREPORT_DO_NOT_COMPILE_CMD)

# Do not pipe testsuite files through tee as this inserts random characters.
testsuite/%.ll: testsuite/%.zomp $(ZOMPC_FILE) $(OUT_DIR)/has_llvm
	$(ECHO) "Compiling $(<) to .ll (testsuite) ..."
	$(ZOMPC) -c $< $(ZOMPCFLAGS)

testsuite/*/test_%.ll: ZOMPCFLAGS += --stats $(@:.ll=.compile_stats)

ZOMPCFLAGS_W_TESTSUITE_INCLUDE = --zomp-include-dir testsuite/include
testsuite/include/test_%.ll: ZOMPCFLAGS += $(ZOMPCFLAGS_W_TESTSUITE_INCLUDE)

TESTREPORT_COMPILE_CMD = "$(MAKE) SILENT=1 $(@:.testreport=.exe)"
TESTREPORT_RUN_CMD = "./$(@:.testreport=.exe)"

# Run zompsh tests using zompsh instead of the executable
testsuite/zompsh/%.testreport: TESTREPORT_RUN_CMD="(echo '!setSourceLocation $< 1'; cat \"$<\" testsuite/zompsh/append.txt) | $(ZOMPSH) --stats $(@:.testreport=.compile_stats)"

TESTREPORT_LIB_DEPS = source/prelude.zomp source/runtime.ll libs/unittest.zomp libs/libcee.zomp libs/basic_ops.zomp
TESTREPORT_DEPS = $(ZOMPC_FILE) $(ZOMPSH_FILE) $(CHECK_TEST) external_lib_links makefile testsuite/testsuite.mk $(TESTREPORT_LIB_DEPS)

.PRECIOUS: %.test_output
.PRECIOUS: %.compile_output
.PRECIOUS: %.compile_stats
%.testreport %.result %.test_output %.compile_output %.compile_stats: %.zomp $(TESTREPORT_DEPS)
	@$(ECHO) Running test suite case $< ...
	-if [ -e "${@:.testreport=.result}" ]; then mv ${@:.testreport=.result} ${@:.testreport=.last_result}; fi
	rm -f ${@:.testreport=.}{bc,opt-bc,ll,o,s,exe,test_output,compile_output,compile_stats,testreport}
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
testsuite/check_test_verify/test_check_test_error_report.testreport: testsuite/check_test_verify/test_check_test_error_report.zomp testsuite/testsuite.mk $(CHECK_TEST)
	rm -f ${@:.testreport=.}{bc,op-bc,ll,exe,test_output,compile_output}
	($(CHECK_TEST) $@ "$(MAKE) SILENT=1" 2>&1) > $@.tmp
# suppress printing of command to avoid it being recognized as an error pattern
	@(cat $@.tmp | grep 'testsuite/check_test_verify/test_check_test_error_report.zomp:2:' | grep warning | grep invalid) 2>&1 > /dev/null
	mv $@.tmp $@



