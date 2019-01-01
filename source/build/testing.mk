#
# Tests
#

FILES_TO_DELETE_ON_CLEAN += $(call BUILD_PRODUCTS_ML, source/indentlexer_tests)
FILES_TO_DELETE_ON_CLEAN += $(call BUILD_PRODUCTS_ML, source/newparser_tests)
TEST_ML_SRC = source/testing.ml source/indentlexer_tests.ml source/newparser_tests.ml

MLTEST = $(OUT_DIR)/source/mltest
ALL_TARGETS += $(MLTEST)
FILES_TO_DELETE_ON_CLEAN += $(call BUILD_PRODUCTS_ML, source/mltest)
$(MLTEST): $(NEWPARSER_ML_SRC:.ml=.$(CAML_OBJ_EXT)) $(TEST_ML_SRC:.ml=.$(CAML_OBJ_EXT))
$(MLTEST): CAML_OBJS = $(NEWPARSER_ML_SRC:.ml=) $(TEST_ML_SRC:.ml=)
$(MLTEST): CAML_LIBS = str bigarray

MLTEST_SUMMARY_FILE = $(TESTSUITE_OUT_DIR)/mltest_summary.test_output
MLTEST_OUTPUT_FILE = $(TESTSUITE_OUT_DIR)/mltest.test_output

TEST_SUB_TARGETS += runmltests
.PHONY: runmltests
runmltests: $(MLTEST)
	@$(ECHO) Running OCaml test suite ...
	$(MLTEST) $(MLTEST_SUMMARY_FILE) | tee $(MLTEST_OUTPUT_FILE); exit $${PIPESTATUS}

