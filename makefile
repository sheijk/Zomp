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

help:
	@$(ECHO) "Please use 'make all' and/or 'make test'"
	exit 1

debug:
	@$(ECHO) "BUILD_VARIANT = " $(BUILD_VARIANT)
	@$(ECHO) "PATH = "
	@$(ECHO) "$(PATH)" | $(TR) : \\n
	@$(ECHO) "SHELL = " $(SHELL)
	@$(ECHO) "LLVM_BIN_DIR = $(LLVM_BIN_DIR)"
	@$(ECHO) "LLVM_CONFIG = $(LLVM_CONFIG)"
	@$(ECHO) "CC = $(CC)"
	@$(ECHO) "CXX = $(CXX)"
	@$(ECHO) "CCFLAGS = " $(CCFLAGS)
	@$(ECHO) "CXXFLAGS = '"$(CXXFLAGS)"'"
	@$(ECHO) "LDFLAGS = " $(LDFLAGS)
	@$(ECHO) "BUILD_PLATFORM = $(BUILD_PLATFORM)"
	@$(ECHO) "LLVM_EXTRA_OPTIONS = $(LLVM_EXTRA_OPTIONS)"
	@$(ECHO) "ZOMP_MAIN_MAKEFILE = $(ZOMP_MAIN_MAKEFILE)"
ifneq "$(PRINT_VAR)" ""
	@$(ECHO) "$(PRINT_VAR) = '$($(PRINT_VAR))'"
else
	@$(ECHO) "Use PRINT_VAR=foo to print foo"
endif

ZOMP_DIR = $(PWD)
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

.PHONY: make_build_dir

make_build_dir: $(BUILD_DIR)/.exists

$(BUILD_DIR)/.exists:
	@$(ECHO) "Creating build directory ..."
	mkdir -p $(DEPLOY_DIR)
	mkdir -p $(OUT_DIR)
	mkdir -p $(TESTSUITE_OUT_DIR)
	mkdir -p $(CAML_DOC_DIR)
	touch $@

include source/build/flymake.mk

# Extended by included makefiles
CLEAN_SUB_TARGETS =
FILES_TO_DELETE_ON_CLEAN =
TEST_SUB_TARGETS =
ALL_TARGETS=
DOC_TARGETS=

AUTO_DEPENDENCY_FILE = $(OUT_DIR)/auto_depends.mk

.PHONY: deps
deps: $(AUTO_DEPENDENCY_FILE)

# When this is changed, LANG_CMOS and LANG_CMXS will need to be changed, too
CAMLDEP_INPUT = $(foreach file, ast2.ml bindings.ml common.ml serror.ml \
    result.ml expander.ml gen_c_bindings.ml builtins.ml genllvm.ml indentlexer.ml \
    indentlexer_tests.ml lang.ml machine.ml stats.ml newparser_tests.ml parseutils.ml \
    compileutils.ml semantic.ml zompsh.ml testing.ml types.ml \
    zompc.ml zompvm.ml basics.ml mltest.ml, source/$(file) source/$(file:.ml=.mli)) \
    $(foreach file, make_history_report.ml make_report.ml check_test.ml zomp_source_to_html.ml, testsuite/$(file) testsuite/$(file:.ml=.mli))

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

print_extended_path:
	echo "PATH=$(LLVM_BIN_DIR):$(ZOMP_TOOL_PATH_RELATIVE)/bin:$(OCAMLPATH):$(PATH)"

################################################################################
# Compiler flags
################################################################################

CXXFLAGS += -I $(CLANG_INCLUDE_DIR) -I $(LLVM_INCLUDE_DIR) -L$(LLVM_LIB_DIR) $(ARCHFLAG)
CCFLAGS += -std=c89 -I /usr/local/lib/ocaml/ $(ARCHFLAG)
LDFLAGS += $(ARCHFLAG) -L $(LLVM_LIB_DIR)

OCAMLDOC_FLAGS = -I source/ -I testsuite/
OCAMLDEP_FLAGS = $(OCAMLDOC_FLAGS) -dot-include-all -dot-reduce

ifeq "$(DEBUG)" "1"
  OCAMLC += -g
  CAML_FLAGS += -g
  CAML_NATIVE_FLAGS += -g -ccopt -g
  CXXFLAGS += -pg -g -DDEBUG
  CCFLAGS += -pg -g -DDEBUG
  LDFLAGS += -g
else
  ifeq "$(DEBUG)" "0"
    CXXFLAGS += -O3
  else
    $(error DEBUG flag has to either 0 or 1)
  endif
endif

################################################################################
# Combined/main targets
################################################################################

.PHONY: libbindings
ALL_TARGETS += libbindings
libbindings: source/gen_c_bindings $(GENERATED_LIBRARY_SOURCES) \
  libs/libglut.dylib libs/libquicktext.dylib libs/libutils.dylib libs/stb_image.dylib

ifeq "$(CAML_BYTE_CODE)" "1"

.PHONY: byte
ALL_TARGETS += byte
byte: $(ZOMP_DLL_FILE) $(ZOMPC_FILE) $(ZOMPSH_FILE)
.PHONY: compiler
compiler: byte

else

.PHONY: native
ALL_TARGETS += native
native: $(ZOMP_DLL_FILE) $(ZOMPC_FILE) $(ZOMPSH_FILE)
.PHONY: compiler
compiler: native

endif

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

.PRECIOUS: source/machine_stubs.c source/machine.mli source/stats_stubs.c source/stats.mli

################################################################################
# Zomp tools
################################################################################

ALL_TARGETS += source/zompvm_dummy.o

ZOMP_DLL_OBJS = source/zompvm_impl.o source/zompvm_caml.o source/stats_impl.o source/runtime.o source/machine_stubs.o source/stats_stubs.o

$(ZOMP_DLL_FILE): $(ZOMP_DLL_OBJS) source/runtime.ll $(OUT_DIR)/has_clang
	@$(ECHO) Building $@ ...
ifeq "$(BUILD_PLATFORM)" "Linux"
	$(CXX) $(DLL_FLAG) $(LDFLAGS) -o source/zompvm -DPIC -fPIC $(ZOMP_DLL_OBJS) -L$(LLVM_LIB_DIR) $(LLVM_LIBS)
else # OS X
	ocamlmklib -o source/zompvm -lcurl $(ZOMP_DLL_OBJS) -lstdc++-static -L$(LLVM_LIB_DIR) $(LLVM_LIBS)
	cp source/dllzompvm.so $(ZOMP_DLL_FILE)
endif

FILES_TO_DELETE_ON_CLEAN += source/zompc{.cmi,.cmo,.cmx,.o} $(ZOMPC_FILE)
FILES_TO_DELETE_ON_CLEAN += source/zompsh{.cmi,.cmo,.cmx,.o} $(ZOMPSH_FILE)
ifeq "$(CAML_BYTE_CODE)" "0"

$(ZOMPC_FILE): $(LANG_CMOS:.cmo=.cmx) source/zompc.cmx $(ZOMP_DLL_FILE)
	@$(ECHO) Building $@ ...
	$(OCAMLOPT) $(CAML_NATIVE_FLAGS)  -o $@ -I $(LLVM_LIB_DIR) $(CAML_COMPILER_LIBS:.cma=.cmxa) $(LANG_CMXS) source/zompc.cmx -cclib -lcurl

$(ZOMPSH_FILE): source/zompsh.cmx $(LANG_CMOS:.cmo=.cmx) $(ZOMP_DLL_FILE)
	@$(ECHO) Building $@ ...
	$(OCAMLOPT) -o $@ $(CAML_NATIVE_FLAGS) -I $(LLVM_LIB_DIR) str.cmxa bigarray.cmxa $(LANG_CMXS) source/zompsh.cmx -cclib -lcurl

else

$(ZOMPC_FILE): $(LANG_CMOS) source/zompc.cmo $(ZOMP_DLL_FILE)
	@$(ECHO) Building $@ ...
	$(OCAMLC) $(CAML_FLAGS) -o $@ $(CAML_COMPILER_LIBS) $(LANG_CMOS) $(ZOMP_DLL_FILE)

$(ZOMPSH_FILE): source/zompsh.cmo $(LANG_CMOS:.cmo=.cmx)
	@$(ECHO) Building $@ ...
	$(OCAMLC) $(CAML_FLAGS) -o $@ $(CAML_COMPILER_LIBS) $(LANG_CMOS) source/zompsh.cmo

endif

FILES_TO_DELETE_ON_CLEAN += source/gen_c_bindings{.cmi,.cmo,.cmx,.o,}
source/gen_c_bindings: CAML_LIBS += str

ALL_TARGETS += source/runtime.bc source/runtime.ll
source/runtim%.bc source/runtim%.ll: source/runtim%.c $(OUT_DIR)/has_llvm $(OUT_DIR)/has_clang
	@$(ECHO) Building bytecode standard library $@ ...
	$(CLANG) -std=c89 -emit-llvm -c $< -o source/runtime.bc
	$(LLVM_DIS) < source/runtime.bc > source/runtime.orig.ll
	($(SED) 's/nounwind//' | $(SED) 's/readonly//' | $(SED) 's/ssp//') < source/runtime.orig.ll > source/runtime.ll
	$(RM) -f source/runtime.bc source/runtime.orig.ll
	$(LLVM_AS) < source/runtime.ll > source/runtime.bc

NEWPARSER_ML_SRC = $(foreach file, common basics ast2 newparser indentlexer, source/$(file).ml)

################################################################################
# ZompVM server
################################################################################

VM_HTTP_SERVER_OBJS = source/mongoose.o source/runtime.o source/zompvm_impl.o \
  source/zompvm_caml_dummy.o source/stats_impl.o source/vm_http_server.o

source/vm_http_server.o: CXXFLAGS += `$(LLVM_CONFIG) --cxxflags`
source/zompvm_impl.o: CXXFLAGS += `$(LLVM_CONFIG) --cxxflags`

ALL_TARGETS += $(DEPLOY_DIR)/vm_http_server
$(DEPLOY_DIR)/vm_http_server: $(VM_HTTP_SERVER_OBJS) source/mongoose.h
	@$(ECHO) Building $@ ...
	$(CXX) $(LDFLAGS) -o $@ -lstdc++-static -lcurl $(LLVM_LIBS) $(VM_HTTP_SERVER_OBJS)

vm_server: source/vm_server.o source/vm_protocol.o
	@$(ECHO) Building $@ ...
	$(CXX) $(LDFLAGS) -o $@ $< source/vm_protocol.o

vm_client: source/vm_client.o source/vm_protocol.o
	@$(ECHO) Building $@ ...
	$(CXX) $(LDFLAGS) -o $@ $< source/vm_protocol.o

run_remote_zompsh_test: $(DEPLOY_DIR)/vm_http_server $(ZOMPSH_FILE)
	$(ZOMPSH) < tests/vmserver.zomp

source/vm_client.o: source/vm_protocol.h
source/vm_server.o: source/vm_protocol.h
source/vm_protocol.o: source/vm_protocol.h

################################################################################
# Tests
################################################################################

FILES_TO_DELETE_ON_CLEAN += source/indentlexer_tests{.cmi,.cmo,.cmx,.o}
FILES_TO_DELETE_ON_CLEAN += source/newparser_tests{.cmi,.cmo,.cmx,.o}
TEST_ML_SRC = source/testing.ml source/indentlexer_tests.ml source/newparser_tests.ml

MLTEST = $(OUT_DIR)/source/mltest
ALL_TARGETS += $(MLTEST)
FILES_TO_DELETE_ON_CLEAN += source/mltest{.cmi,.cmo,.cmx,.o,}
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

.PHONY: test
test: all $(TEST_SUB_TARGETS)

################################################################################
# Documentation
################################################################################

.PHONY: doc
doc: $(CAML_DOC_DIR)/index.html

$(CAML_DOC_DIR)/index.html: $(CAMLDEP_INPUT)
	mkdir -p $(CAML_DOC_DIR)
	$(OCAMLDOC) $(OCAMLDOC_FLAGS) -html -d $(CAML_DOC_DIR) $(CAMLDEP_INPUT)

################################################################################
# Cleaning
################################################################################

.PHONY: clean clean_tags clean_all

CLEAN_SUB_TARGETS += source/clean
source/clean:
	$(DELETE_FILE) source/zomp_shell.o $(ZOMPSH_FILE)
	$(DELETE_FILE) source/runtime.bc source/runtime.ll source/runtime.o
	$(DELETE_FILE) source/machine.{cmi,cmo,cmx,ml,mli} source/machine_stubs.{c,o}
	$(DELETE_FILE) source/stats.{cmi,cmo,cmx,ml,mli} source/stats_stubs.{c,o} source/stats_impl.o
	$(DELETE_FILE) $(ZOMP_DLL_FILE) source/libzompvm.a
	$(DELETE_FILE) source/zompvm_impl.o source/zompvm_dummy.o
	$(DELETE_FILE) source/zompvm_caml.o source/zompvm_caml_dummy.o
	$(DELETE_FILE) source/*_flymake.*
	$(DELETE_FILE) source/indentlexer.{cmi,cmo,cma,cmx,o}
	$(DELETE_FILE) source/newparser.{cmi,cmo,o,ml,mli,conflicts}
	$(DELETE_FILE) source/newparser_tests.{cmi,cmo,o} source/newparser_tests
	$(DELETE_FILE) source/expandertests.cm?
	$(DELETE_FILE) source/vm_http_server.o source/mongoose.o source/vm_server.o source/vm_protocol.o
	$(DELETE_FILE) source/dllzompvm.so

clean: $(CLEAN_SUB_TARGETS)
	@$(ECHO) "Cleaning ..."
	cd tests && make clean_tests
	@$(DELETE_FILE) $(FILES_TO_DELETE_ON_CLEAN)
	@$(DELETE_FILE) $(foreach f,$(LANG_CMOS),${f:.cmo=.cm?})
	@$(DELETE_FILE) $(foreach f,$(LANG_CMOS),${f:.cmo=.o})
	$(DELETE_FILE) expander_tests.cm?
	$(DELETE_FILE) *_flymake.*
	$(DELETE_FILE) $(AUTO_DEPENDENCY_FILE)
	$(DELETE_FILE) libs/glQuickText.o libs/libquicktext.dylib libs/libglut.dylib
	$(DELETE_FILE) perflog.txt
	$(DELETE_FILE) libs/libutils.dylib
	$(DELETE_FILE) gmon.out
	$(DELETE_FILE) $(DEPLOY_DIR)/vm_http_server
	$(DELETE_FILE) $(MLTEST_SUMMARY_FILE) $(MLTEST_OUTPUT_FILE)
	$(DELETE_FILE) $(BUILD_DIR)/.exists
	$(DELETE_DIR) -rdf $(OUT_DIR) $(DEPLOY_DIR) $(TESTSUITE_OUT_DIR)

clean_tags:
	$(DELETE_FILE) source/*.annot source/*.cmt source/*.cmti
	$(DELETE_FILE) source/*.conflicts
	$(DELETE_FILE) testsuite/*.annot testsuite/*.cmt testsuite/*.cmti
	$(DELETE_FILE) $(FLYMAKE_LOG)

clean_doc:
	rm -rf $(CAML_DOC_DIR)

clean_all: clean clean_tags

################################################################################
# More targets
################################################################################

# This will create a script to setup the environment as it is used for this build.
# Use this for debugging or if you want to make sure you're using the same tools
# as the make process when looking up help, etc.
setenv: $(OUT_DIR)/env.sh

ALL_TARGETS += $(OUT_DIR)/env.sh
$(OUT_DIR)/env.sh: makefile
	@$(ECHO) "Creating $@ ..."
	echo '#!/usr/bin/env sh' > $@
	echo "# source this script to setup the environment for this build"
	echo "PATH=$(PATH)" >> $@

ALL_TARGETS += doc
.PHONY: doc
doc: $(DOC_TARGETS)

.PHONY: all
all: $(ALL_TARGETS)

