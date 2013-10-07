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
	touch $@

include source/build/flymake.mk

# Extended by included makefiles
CLEAN_SUB_TARGETS =
TEST_SUB_TARGETS =
FILES_TO_DELETE_ON_CLEAN =
ALL_TARGETS=

AUTO_DEPENDENCY_FILE = $(OUT_DIR)/auto_depends.mk

$(AUTO_DEPENDENCY_FILE): $(BUILD_DIR)/.exists $(CAMLDEP_INPUT) makefile
	@$(ECHO) "Calculating dependencies ..."
	$(OCAMLDEP) -I source $(CAML_PP) $(CAMLDEP_INPUT) > $(AUTO_DEPENDENCY_FILE)

# When this is changed, LANG_CMOS and LANG_CMXS will need to be changed, too
CAMLDEP_INPUT = $(foreach file, ast2.ml bindings.ml common.ml serror.ml \
    expander.ml gen_c_bindings.ml genllvm.ml indentlexer.ml \
    indentlexer_tests.ml lang.ml machine.ml stats.ml newparser_tests.ml parseutils.ml \
    compileutils.ml semantic.ml zompsh.ml testing.ml typesystems.ml \
    zompc.ml zompvm.ml basics.ml, source/$(file) source/$(file:.ml=.mli)) \
    $(foreach file, make_history_report.ml make_report.ml check_test.ml, testsuite/$(file) testsuite/$(file:.ml=.mli))

-include $(AUTO_DEPENDENCY_FILE)
include testsuite/testsuite.mk
include libs/libs.mk
include examples/examples.mk
include examples/smallpt/smallpt.mk
include bindgen/bindgen.mk

export PATH := $(LLVM_BIN_DIR):./tools/arch-$(ARCH)/bin:$(OCAMLPATH):$(PATH)
# If this line is removed the PATH above won't be in effect.
SHELL = sh

CXXFLAGS += -I $(CLANG_INCLUDE_DIR) -I $(LLVM_INCLUDE_DIR) -L$(LLVM_LIB_DIR) $(ARCHFLAG)
CCFLAGS += -std=c89 -I /usr/local/lib/ocaml/ $(ARCHFLAG)
LDFLAGS += $(ARCHFLAG) -L $(LLVM_LIB_DIR)

ifeq "$(DEBUG)" "1"
  OCAMLC += -g
  CXXFLAGS += -pg -g -DDEBUG
  CCFLAGS += -pg -g -DDEBUG
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

.PHONY: all

.PHONY: libbindings
ALL_TARGETS += libbindings
libbindings: source/gen_c_bindings $(GENERATED_LIBRARY_SOURCES) \
  libs/libglut.dylib libs/libquicktext.dylib libs/libutils.dylib libs/stb_image.dylib

ifeq "$(CAML_BYTE_CODE)" "1"

.PHONY: byte
ALL_TARGETS += byte
byte: $(ZOMP_DLL_FILE) $(ZOMPC_FILE) $(ZOMPSH_FILE)
compiler: byte

else

.PHONY: native
ALL_TARGETS += native
native: $(ZOMP_DLL_FILE) $(ZOMPC_FILE) $(ZOMPSH_FILE)
compiler: native

endif

CAML_COMPILER_LIBS = str.cma bigarray.cma
# When this is changed, CAMLDEP_INPUT and LANG_CMXS will need to be changed, too
LANG_CMOS = source/common.cmo source/basics.cmo source/testing.cmo \
  source/typesystems.cmo source/bindings.cmo source/ast2.cmo source/serror.cmo \
  source/lang.cmo source/semantic.cmo source/machine.cmo source/stats.cmo source/zompvm.cmo \
  source/genllvm.cmo $(ZOMP_DLL_FILE) source/indentlexer.cmo \
  source/newparser.cmo source/parseutils.cmo source/expander.cmo \
  source/compileutils.cmo

# When this is changed, LANG_CMOS and CAMLDEP_INPUT will need to be changed, too
LANG_CMXS= common.cmx basics.cmx ast2.cmx bindings.cmx serror.cmx \
    typesystems.cmx lang.cmx semantic.cmx machine.cmx stats.cmx zompvm.cmx genllvm.cmx \
     -cclib -lstdc++ $(LLVM_LIBS_CAML) source/libzompvm.a indentlexer.cmx \
    newparser.cmx parseutils.cmx expander.cmx testing.cmx compileutils.cmx

################################################################################
# Zomp tools
################################################################################

ALL_TARGETS += source/zompvm_dummy.o

ZOMP_DLL_OBJS = source/zompvm_impl.o source/zompvm_caml.o source/stats_impl.o source/runtime.o source/machine.o source/stats.o

$(ZOMP_DLL_FILE): $(ZOMP_DLL_OBJS) source/runtime.ll $(OUT_DIR)/has_clang
	@$(ECHO) Building $@ ...
ifeq "$(BUILD_PLATFORM)" "Linux"
	$(CXX) $(DLL_FLAG) $(LDFLAGS) -o source/zompvm -DPIC -fPIC $(ZOMP_DLL_OBJS) -L$(LLVM_LIB_DIR) $(LLVM_LIBS)
else # OS X
	ocamlmklib -o source/zompvm -lcurl $(ZOMP_DLL_OBJS) -lstdc++ -L$(LLVM_LIB_DIR) $(LLVM_LIBS)
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
	$(CXX) $(LDFLAGS) -o $@ -lstdc++ -lcurl $(LLVM_LIBS) $(VM_HTTP_SERVER_OBJS)

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
# Reporting
################################################################################

.PHONY: report
report: $(BUILD_DIR)/report.html $(BUILD_DIR)/testsuite/summary.txt

EXPECTED_BUILD_PRODUCTS = $(DEPLOY_DIR)/zompc $(DEPLOY_DIR)/zompsh $(DEPLOY_DIR)/vm_http_server

MAKE_REPORT = $(OUT_DIR)/testsuite/make_report
ALL_TARGETS += $(MAKE_REPORT)
FILES_TO_DELETE_ON_CLEAN += testsuite/make_report{.cmx,.cmi,.cmo,.o,}
$(MAKE_REPORT): CAML_LIBS += str

.PHONY: $(BUILD_DIR)/report.html
$(BUILD_DIR)/report.html: $(MAKE_REPORT)
	@$(ECHO) Creating test report ...
	cat testsuite/report_head.html > $@
	echo "Report generated at `date \"+%Y-%m-%d %H:%M:%S\"`</br>" >> $@
	echo "Build variant = $(BUILD_VARIANT) </br>" >> $@
	$(MAKE_REPORT) "Unit tests" $(sort $(TESTSUITE_CASES:.testreport=)) >> $@
	./libs/make_libs_result_files.sh $(ZOMP_LIBS_SRC)
	$(MAKE_REPORT) "Libraries" $(sort $(ZOMP_LIBS_SRC:.zomp=)) >> $@
	./examples/make_examples_result_files.sh $(EXAMPLES_SOURCES)
	$(MAKE_REPORT) "Examples" $(sort $(EXAMPLES_SOURCES:.zomp=)) >> $@
	echo "<h2>OCaml unit tests</h2>" >> $@
	echo "<a href=\"../../$(MLTEST_OUTPUT_FILE)\">Output</a>\n" >> $@
	echo "<p><span style=\"font-family:monospace\">\n" >> $@
	(cat $(MLTEST_SUMMARY_FILE) 2>/dev/null || echo "File <span class=\"failed\">$(MLTEST_SUMMARY_FILE)</span> does not exist, mltests have not been run<br />") >> $@
	BEGIN_ERROR="<span class=\"failed\">" END_ERROR="</span>" NEWLINE="<br />" ./source/build/check_build_products.sh $(ARCH) $(EXPECTED_BUILD_PRODUCTS) >> $@
	echo "</span></p>\n" >> $@
	echo "</body>\n</html>" >> $@

MAKE_HISTORY_REPORT = $(OUT_DIR)/testsuite/make_history_report
ALL_TARGETS += $(MAKE_HISTORY_REPORT)
FILES_TO_DELETE_ON_CLEAN += testsuite/make_history_report{.cmx,.cmi,.cmo,.o,}
$(MAKE_HISTORY_REPORT): CAML_OBJS += source/common
$(MAKE_HISTORY_REPORT): CAML_LIBS += str bigarray

print_ci_stats: $(MAKE_HISTORY_REPORT)
	 ./testsuite/for_each_ci_run.sh $(MAKE_HISTORY_REPORT)

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
# Out of date, TODO: fix and add to test suite
################################################################################

# PROF_COMP_TARGET=metaballs
# 
# .PHONY: profile_comp
# profile_comp: $(ZOMPC_FILE) source/runtime.bc libs/opengl20.zomp libs/glfw.zomp
# 	cd examples && $(RM) -f $(PROF_COMP_TARGET).ll $(PROF_COMP_TARGET).bc
# 	cd examples && time make $(PROF_COMP_TARGET).ll $(PROF_COMP_TARGET).bc ZOMPCFLAGS=--print-timings
# 
# .PHONY: runtests
# runtests: $(LANG_CMOS)
# 	@$(ECHO) Running tests ...
# 	cd tests && time make clean_tests check
# 
# # FUNCTION_COUNTS=10 1000
# PERFTEST_GEN=
# # PERFTEST_GEN=_iexpr
# FUNCTION_COUNTS=100 1000 2000 3000 4000 5000 6000 7000 8000
# 
# .PHONY: perftest
# perftest: $(ZOMPC_FILE)
# 	cd tests && make clean_tests compileperftest FUNCTION_COUNTS="$(FUNCTION_COUNTS)" PERFTEST_GEN=genperftest$(PERFTEST_GEN)
# 	gnuplot makeperfgraph.gnuplot || $(ECHO) "Could not execute gnuplot"
# 	mv temp.png perf_results$(PERFTEST_GEN).png
# 
# .PHONY: perftest2
# perftest2: $(ZOMPC_FILE)
# 	cd tests && make clean_tests compileperftest FUNCTION_COUNTS="$(FUNCTION_COUNTS)" PERFTEST_GEN=genperftest
# 	$(CP) tests/timing.txt tests/timing_sexpr.txt
# 	cd tests && make clean_tests compileperftest FUNCTION_COUNTS="$(FUNCTION_COUNTS)" PERFTEST_GEN=genperftest_iexpr
# 	$(CP) tests/timing.txt tests/timing_iexpr.txt
# 	gnuplot makeperfgraph2.gnuplot || $(ECHO) "Could not execute gnuplot"

################################################################################
# Rules
################################################################################

.PRECIOUS: %.mli $(OUT_DIR)/has_menhir
%.ml: %.mly $(OUT_DIR)/has_ocaml $(OUT_DIR)/has_menhir
	@$(ECHO) Generating parser $< ...
	$(MENHIR) $(MENHIR_FLAGS) --ocamlc "$(OCAMLC) $(CAML_FLAGS)" --explain --infer $<

%.mli: %.ml %.mly
	@$(ECHO) "Generating $@ ..."
	$(OCAMLOPT) $(CAML_NATIVE_FLAGS) -i $< > $@

%.ml: %.mll $(OUT_DIR)/has_ocaml $(OUT_DIR)/has_menhir
	@$(ECHO) Generating lexer $< ...
	$(OCAMLLEX) $<

source/%.c source/%.ml: source/%.skel source/gen_c_bindings
	@$(ECHO) Making OCaml bindings for zomp-machine ...
	./source/gen_c_bindings $(<:.skel=)

ifeq "$(CAML_BYTE_CODE)" "0"

%.cmi: %.mli
	@$(ECHO) "Compiling $@ ..."
	$(OCAMLOPT) $(CAML_NATIVE_FLAGS) -c $<

%.cmx: %.ml %.cmi
	@$(ECHO) "Compiling $@ ..."
	$(OCAMLOPT) $(CAML_NATIVE_FLAGS) -c $<

source/%.mli: source/%.ml source/%.skel
	@$(ECHO) "Generating $@ ..."
	$(OCAMLOPT) $(CAML_NATIVE_FLAGS) -i $< > $@

%: %.cmx $(CAML_DEPENDENCIES)
	$(ECHO) "Building native ml program $@ ..."
	$(OCAMLOPT) $(CAML_NATIVE_FLAGS) $(CAML_LINK_FLAGS) -o $@ $<

$(OUT_DIR)/%: %.cmx $(CAML_DEPENDENCIES)
	$(ECHO) "Building native ml program $@ ..."
	mkdir -p `dirname $@`
	$(OCAMLOPT) $(CAML_NATIVE_FLAGS) $(CAML_LINK_FLAGS) -o $@ $<

else

%.cmi: %.mli
	@$(ECHO) "Compiling $@ ..."
	$(OCAMLC) $(CAML_FLAGS) -c $<

%.cmo: %.ml %.cmi
	@$(ECHO) "Compiling $@ ..."
	$(OCAMLC) $(CAML_FLAGS) -c $<

source/%.mli: source/%.ml source/%.skel
	@$(ECHO) "Generating $@ ..."
	$(OCAMLC) $(CAML_FLAGS) -i $< > $@

%: %.cmo $(CAML_DEPENDENCIES)
	$(ECHO) "Building ml program $@ ..."
	$(OCAMLC) $(CAML_FLAGS) $(CAML_LINK_FLAGS) -o $@ $<

$(OUT_DIR)/%: %.cmo
	$(ECHO) "Building ml program $@ ..."
	mkdir -p `dirname $@`
	$(OCAMLC) $(CAML_FLAGS) $(CAML_LINK_FLAGS) -o $@ $<

endif

%.o: %.c
	$(CC) $(CCFLAGS) -c -o $@ $<

%.o: %.cpp
	@$(ECHO) "Compiling $< ..."
	$(CXX) $(CXXFLAGS) -c -o $@ $<

.PRECIOUS: %.ll %.bc %.opt-bc
%.ll: %.zomp $(ZOMPC_FILE) $(OUT_DIR)/has_llvm
	$(ECHO) Compiling $(<) to .ll...
	$(ZOMPC) -c $< $(ZOMPCFLAGS) || (rm -f $@; exit 1)

%.bc: %.ll $(OUT_DIR)/has_llvm
	@echo Compiling $< to $@
	$(LLVM_AS) -f $< -o $@

%.opt-bc: %.bc $(OUT_DIR)/has_llvm
	@$(ECHO) Optimizing $< to $@ ...
	$(LLVM_OPT) $< -o $@ -O3

ifeq "$(OPT)" "1"
%.s: %.opt-bc
else
%.s: %.bc
endif
	@$(ECHO) LLVM code generating $@ ...
	$(LLVM_LLC) -o $@ -march=x86 $<

%.o: %.s
	@$(ECHO) Assembling $@ ...
	$(AS) -o $@ $< -arch i386

%.exe: %.o source/runtime.o $(LIBS)
	@$(ECHO) Making $@ ...
	$(CC) $(LDFLAGS) -o $@ -L. -L./examples -L./testsuite $(LIBS) $< -arch i386

ifeq "$(REGEN_MLI)" "1"
# Use this to re-generate an OCaml module interface (mli file) from the ml file.
# for those modules that simply export everything they define.
# rm -f path/foo.mli
# make path/gen-foo.mli REGEN_MLI=1
gen-%.mli: %.ml
	@$(ECHO) "Auto generating interface $(<)i from $< ..."
	if [ -e "$(<)i" ]; then echo "$<i: 0: error: file exists. delete it first!"; exit 1; fi
	$(OCAMLOPT) $(CAML_NATIVE_FLAGS) -i $< > $(<:.ml=.mli)
endif

################################################################################
# External libraries
################################################################################

libs/libglut.dylib:
	@$(ECHO) Building $@ ...
	$(CC) $(DLL_FLAG) $(LDFLAGS) $(LINK_GLUT) -o $@

libs/libquicktext.dylib: libs/glQuickText.o
	@$(ECHO) Building $@ ...
	$(CXX) $(DLL_FLAG) $(LDFLAGS) -o $@ libs/glQuickText.o $(LINK_GL)

libs/libutils.dylib: libs/libutils.cpp
	@$(ECHO) Building $@ ...
	$(CXX) $(DLL_FLAG) $(LDFLAGS) $< -o $@

libs/stb_image.dylib: libs/stb_image.c libs/stb_image.h
	@$(ECHO) Building $@ ...
	$(CXX) $(DLL_FLAG) $(LDFLAGS) $< -o $@

EXTLIB_DIR = extlibs
ASSIMP_DIR = $(EXTLIB_DIR)/assimp-svn

SCONS = $(PWD)/extlibs/scons/scons.py
ifeq "$(DEBUG)" "1"
SCONSFLAGS += "debug=1"
endif

extlib_assimp:
	@echo Building assimp library ...
	cd $(ASSIMP_DIR)/workspaces/SCons; $(SCONS) $(SCONSFLAGS)

libassimp.a: $(ASSIMP_DIR)/workspaces/SCons/libassimp.a makefile
	- rm -f $@ # prefix '-' means this rule is allowed to fail
	ln -s $< $@

assimp.dylib: libassimp.a makefile libs/forcelinkassimp.c
	@echo Building $@ ...
	$(CXX) $(DLL_FLAG) $(LDFLAGS) -o $@ -I $(ASSIMP_DIR)/include -L. -lassimp libs/forcelinkassimp.c

%.zomp: %.skel source/gen_c_bindings
	@$(ECHO) Generating Zomp bindings for $(<:.skel=) ...
	./source/gen_c_bindings -lang zomp $(<:.skel=)

libs/opengl20print.zomp: libs/opengl20.skel source/gen_c_bindings
	@$(ECHO) Generating OpenGL enum printer ...
	$(CP) libs/opengl20.skel libs/opengl20print.skel
	./source/gen_c_bindings -lang zomp-glprinter $(@:.zomp=)
	$(RM) -f libs/opengl20print.skel

# The install names of external libs are expected to be relative to the
# executable using @executable_path so we need to add links to them into all
# directories that will contain binaries requiring those libs.
EXTERNAL_LIB_LINK_NAMES = glfw GLEW GLEW.1.9 GLEW.1.9.0
EXTERNAL_LIB_TARGET_DIRS = examples testsuite/std libs
EXTERNAL_LIB_LINKS = $(foreach target_dir, $(EXTERNAL_LIB_TARGET_DIRS), \
  $(foreach lib, $(EXTERNAL_LIB_LINK_NAMES), $(target_dir)/lib$(lib).dylib))

libs/lib%.dylib: $(ZOMP_TOOL_PATH)/lib/lib%.dylib
	@$(ECHO) "Creating symlink to library $@ ..."
	rm -f $(@)
	ln -s $(<) $(@)

examples/lib%.dylib: $(ZOMP_TOOL_PATH)/lib/lib%.dylib
	@$(ECHO) "Creating symlink to library $@ ..."
	rm -f $(@)
	ln -s $(<) $(@)

testsuite/std/lib%.dylib: $(ZOMP_TOOL_PATH)/lib/lib%.dylib
	@$(ECHO) "Creating symlink to library $@ ..."
	rm -f $(@)
	ln -s $(<) $(@)

.PHONY: external_lib_links
ALL_TARGETS += external_lib_links
external_lib_links: $(EXTERNAL_LIB_LINKS)

CLEAN_SUB_TARGETS += clean_external_lib_links
.PHONY: clean_external_lib_links
clean_external_lib_links:
	rm -f $(EXTERNAL_LIB_LINKS)

################################################################################
# LLVM download and compilation
################################################################################

LLVM_INSTALL_HELP = "error: You do not have LLVM and clang installed. Please \
run make tools/llvm-$(LLVM_VERSION) to download and build them (requires an \
internet connection). Note that at least on Mac OS X you cannot use the \
prebuilt libraries as they are 64-bit"

tools/clang-$(LLVM_VERSION).tgz:
	@$(ECHO) Downloading $@ ...
	mkdir -p tools
	curl "http://llvm.org/releases/$(LLVM_VERSION)/clang-$(LLVM_VERSION).tgz" -o $@ -s -S

tools/llvm-$(LLVM_VERSION).tgz:
	@$(ECHO) Downloading $@ ...
	mkdir -p tools
	curl "http://llvm.org/releases/$(LLVM_VERSION)/llvm-$(LLVM_VERSION).tgz" -o $@ -s -S

tools/llvm-$(LLVM_VERSION): tools/llvm-$(LLVM_VERSION).tgz tools/clang-$(LLVM_VERSION).tgz
	@$(ECHO) Unpacking LLVM and clang ...
	cd tools && gunzip --stdout llvm-$(LLVM_VERSION).tgz | tar -xf -
	cd tools && gunzip --stdout clang-$(LLVM_VERSION).tgz | tar -xf - -C $(LLVM_BASE_DIR)/tools
	mv $(LLVM_BASE_DIR)/tools/clang-$(LLVM_VERSION) $(LLVM_BASE_DIR)/tools/clang
	$(TOUCH) $@ # tar sets date from archive. avoid downloading the archive twice
	@$(ECHO) Configuring LLVM $(LLVM_VERSION) and clang ...
	cd tools/llvm-$(LLVM_VERSION) && ./configure EXTRA_OPTIONS="$(LLVM_EXTRA_OPTIONS)"
	@$(ECHO) Building LLVM $(LLVM_VERSION) and clang ...
	cd tools/llvm-$(LLVM_VERSION) && (make EXTRA_OPTIONS="$(LLVM_EXTRA_OPTIONS)"; make ENABLE_OPTIMIZED=0 EXTRA_OPTIONS="$(LLVM_EXTRA_OPTIONS)")

tools/llvm-$(LLVM_VERSION)/TAGS:
	@$(ECHO) Building tags for LLVM $(LLVM_VERSION)
	cd tools/llvm-$(LLVM_VERSION)/ && find -E lib include -regex ".*\.(cpp|h)" | xargs etags -o TAGS

FILES_TO_DELETE_ON_CLEAN += $(OUT_DIR)/has_llvm
$(OUT_DIR)/has_llvm:
	@$(ECHO) "Checking LLVM ..."
	($(WHICH) -s $(LLVM_AS)) || (echo $(LLVM_INSTALL_HELP); exit 1)
	$(TOUCH) $@

FILES_TO_DELETE_ON_CLEAN += $(OUT_DIR)/has_clang
ALL_TARGETS += $(OUT_DIR)/has_llvm $(OUT_DIR)/has_clang
$(OUT_DIR)/has_clang:
	@$(ECHO) "Checking clang ..."
	($(WHICH) -s $(CLANG)) || (echo $(LLVM_INSTALL_HELP); exit 1)
	$(TOUCH) $@

LLVM_LIBS=`$(LLVM_CONFIG) --libs all`
LLVM_LIBS_CAML=-cclib "$(LLVM_LIBS)"

.PHONY: check_llvm
check_llvm: $(OUT_DIR)/has_llvm $(OUT_DIR)/has_clang

################################################################################
# Check OCaml and menhir installation
################################################################################

OCAML_INSTALL_HELP = "error: OCaml for $(ARCH) not found in $(PATH), please install"

FILES_TO_DELETE_ON_CLEAN += $(OUT_DIR)/has_ocaml
FILES_TO_DELETE_ON_CLEAN += $(OUT_DIR)/has_ocaml.tmp
$(OUT_DIR)/has_ocaml:
	@$(ECHO) "Checking OCaml ..."
	($(FILE) `$(WHICH) ocamlopt.opt` | grep $(ARCH) > $(OUT_DIR)/has_ocaml.tmp) || ($(ECHO) $(OCAML_INSTALL_HELP); exit 1)
	touch $@

MENHIR_INSTALL_HELP = "error: Menhir for $(ARCH) not found in $(PATH), please install"

FILES_TO_DELETE_ON_CLEAN += $(OUT_DIR)/has_menhir
FILES_TO_DELETE_ON_CLEAN += $(OUT_DIR)/has_menhir.tmp
$(OUT_DIR)/has_menhir:
	@$(ECHO) "Checking Menhir ..."
	($(FILE) `$(WHICH) menhir` | grep $(ARCH)) > $(OUT_DIR)/has_menhir.tmp || ($(ECHO) $(MENHIR_INSTALL_HELP); exit 1)
	touch $@

.PHONY: check_ocaml
check_ocaml: $(OUT_DIR)/has_ocaml $(OUT_DIR)/has_menhir

################################################################################
# Dependencies
#
# Try to detect dependencies automatically. Does not work for everything, yet,
# add additional ones here. Use something like make -j 20 for a quick test
################################################################################

source/newparser_tests.cmi: source/newparser.cmi
source/newparser_tests.cmo: source/newparser.cmo
source/newparser_tests.cmx: source/newparser.cmx
source/indentlexer.cmi: source/newparser.$(CAML_OBJ_EXT)
source/indentlexer.cmo: source/newparser.cmo
source/indentlexer.cmx: source/newparser.cmx

source/newparser.ml: source/ast2.$(CAML_OBJ_EXT)
source/newparser.mli: source/ast2.$(CAML_OBJ_EXT)
source/newparser.cmi: source/ast2.cmi
source/newparser.cmo: source/ast2.cmo
source/newparser.cmx: source/ast2.cmx

source/machine.cmo: $(ZOMP_DLL_FILE)
source/machine.cmx: $(ZOMP_DLL_FILE)
source/stats.cmo: $(ZOMP_DLL_FILE)
source/stats.cmx: $(ZOMP_DLL_FILE)
source/zompvm.cmo: source/machine.cmo
source/zompvm.cmx: source/machine.cmx

source/ast2.cmo: source/basics.cmo source/common.cmo
source/ast2.cmx: source/basics.cmx source/common.cmx

source/mltest.cmo: source/newparser_tests.cmo source/indentlexer_tests.cmo
source/mltest.cmx: source/newparser_tests.cmx source/indentlexer_tests.cmx

# TODO: auto generate C/C++ dependencies
source/runtime.o: source/zomputils.h

source/zompvm_impl.o: source/zomputils.h
source/zompvm_impl.o: source/zompvm_impl.h
source/zompvm_impl.o: source/zompvm_caml.h
source/zompvm_impl.o: source/stats_impl.h
source/zompvm_impl.o: source/zompvm_caml.h
source/zompvm_caml.o: source/zomputils.h
source/stats_impl.o: source/zomputils.h
source/stats_impl.o: source/stats_impl.h
source/vm_http_server.o: source/zomputils.h
source/vm_http_server.o: source/zompvm_impl.h
source/vm_http_server.o: source/mongoose.h
source/vm_http_server.o: source/

source/vm_protocol.o: source/vm_protocol.h
source/vm_client.o: source/vm_protocol.h
source/vm_server.o: source/vm_protocol.h


################################################################################
# Tags
################################################################################

ALL_TARGETS += TAGS
TAGS:
	@$(ECHO) Generating tags ...
	otags 2> /dev/null || $(ECHO) "otags not found, no tags generated"

################################################################################
# Visualize OCaml module dependencies
################################################################################

ALL_TARGETS += $(OUT_DIR)/deps.svg
$(OUT_DIR)/deps.dot $(OUT_DIR)/deps.svg: makefile $(AUTO_DEPENDENCY_FILE) $(CAMLDEP_INPUT:.mli=.cmi) source/newparser.mli
	@$(ECHO) Generating dependency graph for graphviz ...
	$(OCAMLDOC) -I source/ -I testsuite/ -o $(OUT_DIR)/deps.dot -dot -dot-reduce $(CAMLDEP_INPUT) source/newparser.ml source/newparser.mli
	cat $(OUT_DIR)/deps.dot | sed 's/rotate=90;/rotate=0;/' > $(OUT_DIR)/deps.dot.tmp
	mv $(OUT_DIR)/deps.dot.tmp $(OUT_DIR)/deps.dot
	-dot -Tsvg $(OUT_DIR)/deps.dot > $(OUT_DIR)/deps.svg

################################################################################
# Outdated line of code statistics. Does not work anymore, kept for reference
################################################################################

ML_SRC_FILES = $(wildcard source/*.ml) $(wildcard source/*.mli) $(wildcard source/*.mll) $(wildcard source/*.mly)

.PHONY: loc_stats_no_summary
loc_stats_no_summary:
	$(LS) $(wildcard source/*.ml source/*.mli source/*.mly source/*.mll) | grep -v source/newparser.ml | xargs $(LINE_COUNT) | $(SORT) -n
	$(LINE_COUNT) $(wildcard *.mk) makefile | $(SORT) -n
	$(LINE_COUNT) libs/libutils.cpp source/prelude.zomp source/runtime.c zomp.el source/zomputils.h source/zompvm_impl.cpp source/zompvm_impl.h source/zompvm_dummy.cpp | $(SORT) -n
	$(LINE_COUNT) $(wildcard libs/*.skel) | $(SORT) -n
	$(LS) $(wildcard libs/*.zomp) | grep -v libs/opengl20.\*\.zomp | grep -v libs/glfw\.zomp | grep -v libs/quicktext\.zomp | grep -v libs/glut.zomp | xargs $(LINE_COUNT) | $(SORT) -n
	$(LINE_COUNT) $(wildcard examples/*.zomp) | $(SORT) -n
	$(LINE_COUNT) $(wildcard testsuite/*.zomp) | $(SORT) -n
	$(LS) $(wildcard tests/*.zomp) | grep -v sharkperf.zomp | xargs $(LINE_COUNT) | $(SORT) -n

.PHONY: loc_stats
loc_stats: loc_stats_no_summary
	make -ks loc_stats_no_summary | grep total | awk '{ sum = sum + $$1; } END { print sum " total lines of code "; }'

################################################################################
# Git statistics
################################################################################

GITSTATS = $(ZOMP_TOOL_PATH)/../gitstats/gitstats
.PHONY: git_repo_stats
git_repo_stats:
	@$(ECHO) "Creating git repository statistincs ..."
	$(GITSTATS) . build/git-statistics
	@$(ECHO) "Open build/git-statistics/index.html to see git repository statistics"

################################################################################
# Count lines of code
################################################################################

CLOC_LANG_DEF_FILE = $(OUT_DIR)/cloc-lang-defs.txt

$(CLOC_LANG_DEF_FILE): source/build/cloc-zomp-def.txt source/build/cloc-glsl-def.txt makefile
	@$(ECHO) Creating cloc language definition file ...
	cloc --write-lang-def=$@
	cat source/build/cloc-zomp-def.txt >> $@
	cat source/build/cloc-glsl-def.txt >> $@

build/cloc.txt: $(CLOC_LANG_DEF_FILE)
	@$(ECHO) Creating lines of code statistics in $@ ...
	cloc --read-lang-def=$(CLOC_LANG_DEF_FILE) --exclude-dir=tools,build,data --report-file=$@ .

################################################################################
# Cleaning
################################################################################

.PHONY: clean clean_tags clean_all

CLEAN_SUB_TARGETS += source/clean
source/clean:
	$(DELETE_FILE) source/zomp_shell.o $(ZOMPSH_FILE)
	$(DELETE_FILE) source/runtime.bc source/runtime.ll source/runtime.o
	$(DELETE_FILE) source/machine.{cmi,cmo,cmx,o,c,ml}
	$(DELETE_FILE) source/stats.{cmi,cmo,cmx,o,c,ml}
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
	$(DELETE_FILE) $(FILES_TO_DELETE_ON_CLEAN)
	$(DELETE_FILE) $(foreach f,$(LANG_CMOS),${f:.cmo=.cm?})
	$(DELETE_FILE) $(foreach f,$(LANG_CMOS),${f:.cmo=.o})
	$(DELETE_FILE) expander_tests.cm?
	$(DELETE_FILE) *_flymake.*
	$(DELETE_FILE) $(OUT_DIR)/deps.{png,dot}
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
	$(DELETE_FILE) source/*.annot
	$(DELETE_FILE) source/*.conflicts
	$(DELETE_FILE) TAGS
	$(DELETE_FILE) $(FLYMAKE_LOG)

clean_all: clean clean_tags

all: $(ALL_TARGETS)

