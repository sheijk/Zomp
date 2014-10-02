#
# Makefile for zomp project
#

################################################################################
# Config
################################################################################

# guard for makefiles in sub directories
ZOMP_MAIN_MAKEFILE=1

ifndef DEBUG
DEBUG=0
endif

ifndef CAML_BYTE_CODE
CAML_BYTE_CODE=0
endif

ifndef PWD
PWD=`pwd`
endif


ZOMP_DIR = $(PWD)
.DEFAULT_GOAL: all
include source/build/help.mk
include source/build/config.mk

ifneq "$(SILENT)" "1"
  $(info Build variant $(BUILD_VARIANT), LLVM variant = $(LLVM_VARIANT))
endif

BUILD_DIR_BASE = build
BUILD_DIR = $(BUILD_DIR_BASE)/$(BUILD_VARIANT)
DEPLOY_DIR = $(BUILD_DIR)/deploy
OUT_DIR = $(BUILD_DIR)/intermediate
CAML_DOC_DIR = $(BUILD_DIR)/doc
TESTSUITE_OUT_DIR = $(BUILD_DIR)/testsuite

ZOMP_DLL_FILE = $(DEPLOY_DIR)/dllzompvm.so
ZOMPC_FILE = $(DEPLOY_DIR)/zompc
ZOMPSH_FILE = $(DEPLOY_DIR)/zompsh

# Extended by included makefiles
CLEAN_SUB_TARGETS =
FILES_TO_DELETE_ON_CLEAN =
TEST_SUB_TARGETS =
ALL_TARGETS=
DOC_TARGETS=

AUTO_DEPENDENCY_FILE = $(OUT_DIR)/auto_depends.mk

# When this is changed, LANG_CMOS and LANG_CMXS will need to be changed, too
CAMLDEP_INPUT = $(foreach file, ast2.ml bindings.ml common.ml serror.ml \
    result.ml expander.ml gen_c_bindings.ml builtins.ml genllvm.ml indentlexer.ml \
    indentlexer_tests.ml lang.ml machine.ml stats.ml newparser_tests.ml parseutils.ml \
    compileutils.ml semantic.ml zompsh.ml testing.ml types.ml \
    zompc.ml zompvm.ml basics.ml mltest.ml, source/$(file) source/$(file:.ml=.mli)) \
    $(foreach file, make_history_report.ml make_report.ml check_test.ml zomp_source_to_html.ml, testsuite/$(file) testsuite/$(file:.ml=.mli))

CAML_COMPILER_LIBS = str.cma bigarray.cma
# When this is changed, CAMLDEP_INPUT and LANG_CMXS will need to be changed, too
LANG_CMOS = source/common.cmo source/basics.cmo source/testing.cmo \
  source/types.cmo source/ast2.cmo source/lang.cmo source/bindings.cmo source/serror.cmo source/result.cmo \
  source/semantic.cmo source/machine.cmo source/stats.cmo source/zompvm.cmo \
  source/builtins.cmo source/genllvm.cmo $(ZOMP_DLL_FILE) source/indentlexer.cmo \
  source/newparser.cmo source/parseutils.cmo source/expander.cmo \
  source/compileutils.cmo

# When this is changed, LANG_CMOS and CAMLDEP_INPUT will need to be changed, too
LANG_CMXS= common.cmx basics.cmx ast2.cmx types.cmx lang.cmx bindings.cmx serror.cmx result.cmx \
    semantic.cmx machine.cmx stats.cmx zompvm.cmx builtins.cmx genllvm.cmx \
     -cclib -lstdc++-static $(LLVM_LIBS_CAML) source/libzompvm.a indentlexer.cmx \
    newparser.cmx parseutils.cmx expander.cmx testing.cmx compileutils.cmx

include source/build/depends.mk
include testsuite/testsuite.mk
include libs/libs.mk
include examples/examples.mk
include examples/smallpt/smallpt.mk
include bindgen/bindgen.mk
include source/build/tools.mk
include source/build/external_libs.mk
include source/build/reports.mk
include source/build/checks.mk
include source/build/stats.mk
include source/build/rules.mk
include source/build/clean.mk
include source/build/testing.mk
include source/build/flymake.mk
include source/build/main_targets.mk

print_extended_path:
	echo "PATH=$(LLVM_BIN_DIR):$(ZOMP_TOOL_PATH_RELATIVE)/bin:$(OCAMLPATH):$(PATH)"

################################################################################
# Main targets
################################################################################

.PHONY: make_build_dir
make_build_dir: $(BUILD_DIR)/.exists

$(BUILD_DIR)/.exists:
	@$(ECHO) "Creating build directory ..."
	mkdir -p $(DEPLOY_DIR)
	mkdir -p $(OUT_DIR)
	mkdir -p $(TESTSUITE_OUT_DIR)
	mkdir -p $(CAML_DOC_DIR)
	touch $@

.PHONY: deps
deps: $(AUTO_DEPENDENCY_FILE)

ALL_TARGETS += doc
.PHONY: doc
doc: $(DOC_TARGETS)

.PHONY: all
all: $(ALL_TARGETS)

.PHONY: test
test: all $(TEST_SUB_TARGETS)

