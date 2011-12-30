#
# Makefile for zomp project
#

################################################################################
# Config
################################################################################

# guard for makefiles in sub directories
ZOMP_MAIN_MAKEFILE=1

ifndef DEBUG
DEBUG=1
endif

ifndef PWD
PWD=`pwd`
endif

help:
	@$(ECHO) "Please use 'make all' and/or 'make test'"

debug:
	@$(ECHO) "PATH = "
	@$(ECHO) "$(PATH)" | $(TR) : \\n
	@$(ECHO) "CXXFLAGS = '"$(CXXFLAGS)"'"
	@$(ECHO) "CCFLAGS = " $(CCFLAGS)
	@$(ECHO) "LLVM_BIN_DIR = $(LLVM_BIN_DIR)"
	@$(ECHO) "LLVM_CONFIG = $(LLVM_CONFIG)"
	@$(ECHO) "CC = $(CC)"
	@$(ECHO) "CXX = $(CXX)"
	@$(ECHO) "BUILD_PLATFORM = $(BUILD_PLATFORM)"
	@$(ECHO) "LLVM_EXTRA_OPTIONS = $(LLVM_EXTRA_OPTIONS)"
	@$(ECHO) "ZOMP_MAIN_MAKEFILE = $(ZOMP_MAIN_MAKEFILE)"
ifneq "$(PRINT_VAR)" ""
	@$(ECHO) "$(PRINT_VAR) = " $($(PRINT_VAR))
endif

ZOMP_TOOL_PATH = $(PWD)/tools
include config.mk

FLYMAKE_LOG=flymake.log
include flymake.mk

CLEAN_SUB_TARGETS =

-include depends.mk
include testsuite/makefile
include libs/makefile
include examples/makefile
include examples/smallpt/makefile
include bindgen/bindgen.mk

PATH := $(LLVM_BIN_DIR):$(PATH)

CXXFLAGS = -I /usr/local/lib/ocaml/ -I $(CLANG_INCLUDE_DIR) -I $(LLVM_INCLUDE_DIR) -L$(LLVM_LIB_DIR) $(ARCHFLAG)
CCFLAGS = -std=c89 -I /usr/local/lib/ocaml/ $(ARCHFLAG)
LDFLAGS = $(ARCHFLAG)

ifeq ($(DEBUG), 1)
  OCAMLC += -g
  CXXFLAGS += -pg -g -DDEBUG
  CCFLAGS += -pg -g -DDEBUG
else
  ifeq ($(DEBUG),0)
    CXXFLAGS += -O3
  else
    $(error DEBUG flag has to either 0 or 1)
  endif
endif

ifneq ("$(SILENT)", "1")
  ifeq ($(DEBUG), 1)
    $(info Debug build, LLVM variant = $(LLVM_VARIANT))
  else
    ifeq ($(DEBUG),0)
      $(info Release build, LLVM variant = $(LLVM_VARIANT))
    endif
  endif
endif

################################################################################
# Combined/main targets
################################################################################

.PHONY: all libbindings byte native

all: byte native source/runtime.bc source/runtime.ll libbindings TAGS deps.png \
    mltest source/zompvm_dummy.o has_llvm has_clang
libbindings: source/gen_c_bindings libs/opengl20.zomp libs/opengl20print.zomp \
    libs/glfw.zomp libs/glut.zomp libs/libglut.dylib libs/quicktext.zomp \
    libs/libquicktext.dylib libs/libutils.dylib
byte: dllzompvm.so zompc zomp_shell
native: dllzompvm.so $(LANG_CMOS:.cmo=.cmx) zomp_shell.native zompc.native

CAML_LIBS = str.cma bigarray.cma
LANG_CMO_NAMES = common.cmo testing.cmo typesystems.cmo bindings.cmo ast2.cmo \
    lang.cmo semantic.cmo machine.cmo zompvm.cmo \
    genllvm.cmo dllzompvm.so indentlexer.cmo newparser.cmo parseutils.cmo \
    expander.cmo testing.cmo compileutils.cmo
LANG_CMOS = $(foreach file, $(LANG_CMO_NAMES), source/$(file))

################################################################################
# Zomp tools
################################################################################

dllzompvm.so: source/zompvm.h source/zomputils.h source/zompvm.cpp source/machine.c source/runtime.o source/runtime.ll has_clang
	@$(ECHO) Building $@ ...
	$(CLANG) -std=c++98 $(CXXFLAGS) `$(LLVM_CONFIG) --cxxflags` -c source/zompvm.cpp -o source/zompvm.o
	$(CC) $(CCFLAGS) -I /usr/local/lib/ocaml/ -c source/machine.c -o source/machine.o
ifeq "$(BUILD_PLATFORM)" "Linux"
	$(CXX) $(DLL_FLAG) $(LDFLAGS) -o source/zompvm -DPIC -fPIC source/zompvm.o source/runtime.o source/machine.o -L$(LLVM_LIB_DIR) $(LLVM_LIBS)
else # OS X
	ocamlmklib -o source/zompvm source/zompvm.o source/runtime.o source/machine.o -lstdc++ -L$(LLVM_LIB_DIR) $(LLVM_LIBS)
	rm -f dllzompvm.so
	ln -s source/dllzompvm.so dllzompvm.so
endif

zomp_shell: source/zomp_shell.cmo $(LANG_CMOS:.cmo=.cmx)
	@$(ECHO) Building $@ ...
	$(OCAMLC) $(CAML_FLAGS) -o $@ $(CAML_LIBS) $(LANG_CMOS) source/zomp_shell.cmo

zomp_shell.native: source/zomp_shell.cmx $(LANG_CMOS:.cmo=.cmx) source/dllzompvm.so
	@$(ECHO) Building $@ ...
	$(OCAMLOPT) -o $@ $(CAML_NATIVE_FLAGS) -I $(LLVM_LIB_DIR) str.cmxa bigarray.cmxa $(LANG_CMXS) source/zomp_shell.cmx

zompc.native: $(LANG_CMOS:.cmo=.cmx) source/zompc.cmx source/dllzompvm.so
	@$(ECHO) Building $@ ...
	$(OCAMLOPT) $(CAML_NATIVE_FLAGS)  -o $@ -I $(LLVM_LIB_DIR) $(CAML_LIBS:.cma=.cmxa) $(LANG_CMXS) source/zompc.cmx

zompc: $(LANG_CMOS) source/zompc.cmo source/dllzompvm.so
	@$(ECHO) Building $@ ...
	$(OCAMLC) $(CAML_FLAGS)  -o $@ $(CAML_LIBS) $(LANG_CMOS) source/dllzompvm.so source/machine.cmo source/zompvm.cmo source/zompc.cmo

source/gen_c_bindings: source/gen_c_bindings.cmo source/gen_c_bindings.ml
	@$(ECHO) Building $@ ...
	$(OCAMLC) $(CAML_FLAGS)  -o $@ $(CAML_LIBS) source/gen_c_bindings.cmo

source/machine.c source/machine.ml: source/gen_c_bindings source/machine.skel
	@$(ECHO) Making OCaml bindings for zomp-machine ...
	./source/gen_c_bindings source/machine

NEWPARSER_CMOS = $(foreach file, common.cmo testing.cmo ast2.cmo newparser.cmo indentlexer.cmo, source/$(file))

################################################################################
# Tests
################################################################################

TEST_CMOS = source/indentlexer_tests.cmo source/newparser_tests.cmo

.PHONY: runtestsuite perftest2 perftest runtestsuite runtests
.PHONY: profile_comp exampletests runmltests alltests

test: runmltests libs/test examples/test testsuite/test

.PHONY: testsuite/report.html
testsuite/report.html:
	@$(ECHO) Creating test report ...
	cat testsuite/report_head.html > $@
	echo Report generated at `date "+%Y-%m-%d %H:%M:%S"` >> $@
	./testsuite/make_report.sh "Unit tests" $(TESTSUITE_CASES:.testreport=) >> $@
	./testsuite/make_report.sh "Libraries" $(ZOMP_LIBS_SRC:.zomp=) >> $@
	./testsuite/make_report.sh "Examples" $(ZOMP_EXAMPLES:.zomp=) >> $@
	echo "</body>\n</html>" >> $@

mltest: source/testing.cmo $(LANG_CMOS) $(NEWPARSER_CMOS) $(TEST_CMOS)
	@$(ECHO) Building $@ ...
	$(OCAMLC) $(CAML_FLAGS) -o $@ bigarray.cma str.cma $(LANG_CMOS) $(NEWPARSER_CMOS) $(TEST_CMOS)

runmltests: mltest
	@$(ECHO) Running OCaml test suite ...
	$(OCAMLRUN) -b ./mltest

PROF_COMP_TARGET=metaballs

profile_comp: zompc zompc.native source/runtime.bc libs/opengl20.zomp libs/glfw.zomp
	cd examples && $(RM) -f $(PROF_COMP_TARGET).ll $(PROF_COMP_TARGET).bc
	cd examples && time make $(PROF_COMP_TARGET).ll $(PROF_COMP_TARGET).bc ZOMPCFLAGS=--print-timings

runtests: $(LANG_CMOS)
	@$(ECHO) Running tests ...
	cd tests && time make clean_tests check

# FUNCTION_COUNTS=10 1000
PERFTEST_GEN=
# PERFTEST_GEN=_iexpr
FUNCTION_COUNTS=100 1000 2000 3000 4000 5000 6000 7000 8000

perftest: zompc.native zompc
	cd tests && make clean_tests compileperftest FUNCTION_COUNTS="$(FUNCTION_COUNTS)" PERFTEST_GEN=genperftest$(PERFTEST_GEN)
	gnuplot makeperfgraph.gnuplot || $(ECHO) "Could not execute gnuplot"
	mv temp.png perf_results$(PERFTEST_GEN).png

perftest2: zompc.native zompc
	cd tests && make clean_tests compileperftest FUNCTION_COUNTS="$(FUNCTION_COUNTS)" PERFTEST_GEN=genperftest
	$(CP) tests/timing.txt tests/timing_sexpr.txt
	cd tests && make clean_tests compileperftest FUNCTION_COUNTS="$(FUNCTION_COUNTS)" PERFTEST_GEN=genperftest_iexpr
	$(CP) tests/timing.txt tests/timing_iexpr.txt
	gnuplot makeperfgraph2.gnuplot || $(ECHO) "Could not execute gnuplot"

source/runtim%.bc source/runtim%.ll: source/runtim%.c has_llvm has_clang
	@$(ECHO) Building bytecode standard library $@ ...
	$(CLANG) -std=c89 -emit-llvm -c $< -o source/runtime.bc
	$(LLVM_DIS) < source/runtime.bc > source/runtime.orig.ll
	($(SED) 's/nounwind//' | $(SED) 's/readonly//' | $(SED) 's/ssp//') < source/runtime.orig.ll > source/runtime.ll
	$(RM) -f source/runtime.bc source/runtime.orig.ll
	$(LLVM_AS) < source/runtime.ll > source/runtime.bc

LLVM_INSTALL_HELP = "You do not have LLVM and clang installed. Please run make	\
tools/llvm-$(LLVM_VERSION) to download and build them (requires an internet		\
connection of course). Note that at least on Mac OS X you cannot use the		\
prebuilt libraries as they are 64-bit"

has_llvm:
	@$(ECHO) Checking if LLVM exists ...
	($(WHICH) -s $(LLVM_AS)) || (echo $(LLVM_INSTALL_HELP); exit 1)
	$(TOUCH) $@

has_clang:
	@$(ECHO) Checking if clang exists ...
	($(WHICH) -s $(CLANG)) || (echo $(LLVM_INSTALL_HELP); exit 1)
	$(TOUCH) $@

################################################################################
# Rules
################################################################################

%.ml: %.mly
	@$(ECHO) Generating parser $< ...
	$(MENHIR) --ocamlc "$(OCAMLC) $(CAML_FLAGS)" --explain --infer $<
	$(OCAMLC) $(CAML_FLAGS) -c $(<:.mly=.mli)

%.ml: %.mll
	@$(ECHO) Generating lexer $< ...
	$(OCAMLLEX) $<

%.cmi: %.mli
	@$(ECHO) "Compiling $@ ..."
	$(OCAMLC) $(CAML_FLAGS) -c $<

%.cmo %.cmi: %.ml
	@$(ECHO) "Compiling (bytecode) $< ..."
	$(OCAMLC) $(CAML_FLAGS) -c $<

%.cmx: %.ml
	@$(ECHO) "Compiling (native) $< ..."
	$(OCAMLOPT) $(CAML_NATIVE_FLAGS)  -c $<

%.o: %.c
	$(CC) $(CCFLAGS) -c -o $@ $<

.PRECIOUS: %.ll %.bc %.opt-bc
%.ll: %.zomp $(ZOMPC) has_llvm
	$(ECHO) Compiling $(<) to .ll...
	$(ZOMPC) -c $< $(ZOMPCFLAGS) || rm -f $@

%.bc: %.ll
	@echo Compiling $< to $@
	$(LLVM_AS) -f $< -o $@

%.opt-bc: %.bc
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

%.exe: %.o source/runtime.o
	@$(ECHO) Making $@ ...
	$(CC) $(LDFLAGS) -o $@ -L. -L./examples -L./testsuite $(LIBS) $< -arch i386


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

EXTLIB_DIR = extlibs
ASSIMP_DIR = $(EXTLIB_DIR)/assimp-svn

SCONS = $(PWD)/extlibs/scons/scons.py
ifeq ($(DEBUG), 1)
SCONSFLAGS += "debug=1"
endif

extlib_assimp:
	@echo Building assimp library ...
	cd $(ASSIMP_DIR)/workspaces/SCons; $(SCONS) $(SCONSFLAGS)

libassimp.a: $(ASSIMP_DIR)/workspaces/SCons/libassimp.a makefile
	- rm -f $@ # prefix '-' means this rule is allowed to fail
	ln -s $< $@

assimp.dylib: libassimp.a makefile forcelinkassimp.c
	@echo Building $@ ...
	$(CXX) $(DLL_FLAG) $(LDFLAGS) -o $@ -I $(ASSIMP_DIR)/include -L. -lassimp forcelinkassimp.c

%.zomp: %.skel source/gen_c_bindings
	@$(ECHO) Generating Zomp bindings for $(<:.skel=) ...
	./source/gen_c_bindings -lang zomp $(<:.skel=)

libs/%.zomp: libs/%.skel source/gen_c_bindings
	@$(ECHO) Generating Zomp bindings for $(<:.skel=) ...
	./source/gen_c_bindings -lang zomp $(<:.skel=)

libs/opengl20print.zomp: libs/opengl20.skel source/gen_c_bindings
	@$(ECHO) Generating OpenGL enum printer ...
	$(CP) libs/opengl20.skel libs/opengl20print.skel
	./source/gen_c_bindings -lang zomp-glprinter $(@:.zomp=)
	$(RM) -f libs/opengl20print.skel

################################################################################
# LLVM download and compilation
################################################################################

LLVM_VERSION=2.9

tools/clang-$(LLVM_VERSION).tgz:
	@$(ECHO) Downloading $@ ...
	mkdir -p tools
	curl "http://llvm.org/releases/$(LLVM_VERSION)/clang-$(LLVM_VERSION).tgz" -o $@ -s -S

tools/llvm-$(LLVM_VERSION).tgz:
	@$(ECHO) Downloading $@ ...
	mkdir -p tools
	curl http://llvm.org/releases/$(LLVM_VERSION)/llvm-$(LLVM_VERSION).tgz -o $@ -s -S

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

LLVM_LIBS=`$(LLVM_CONFIG) --libs all`
LLVM_LIBS_CAML=-cclib "$(LLVM_LIBS)"
LANG_CMXS=common.cmx ast2.cmx bindings.cmx \
    typesystems.cmx lang.cmx semantic.cmx machine.cmx zompvm.cmx genllvm.cmx \
    -cclib -lstdc++ $(LLVM_LIBS_CAML) source/libzompvm.a indentlexer.cmx newparser.cmx \
    parseutils.cmx expander.cmx testing.cmx compileutils.cmx

################################################################################
# Dependencies
#
# Try to detect dependencies automatically. Does not work for everything, yet,
# add additional ones here. Use something like make -j 20 for a quick test
################################################################################

depends.mk: $(CAMLDEP_INPUT)
	@$(ECHO) Calculating dependencies ...
	$(OCAMLDEP) -I source $(CAML_PP) $(CAMLDEP_INPUT) > depends.mk

CAMLDEP_INPUT = $(foreach file, ast2.ml bindings.ml common.ml expander.ml \
    gen_c_bindings.ml genllvm.ml indentlexer.ml indentlexer.mli \
    indentlexer_tests.ml lang.ml machine.ml newparser_tests.ml parseutils.ml \
    compileutils.ml semantic.ml zomp_shell.ml testing.ml typesystems.ml \
    zompc.ml zompvm.ml, source/$(file))

source/newparser.ml: source/newparser.mly source/ast2.cmo
source/newparser_tests.cmo: source/newparser.cmo
source/newparser_tests.cmx: source/newparser.cmx
source/indentlexer.cmo: source/newparser.ml
source/indentlexer.cmi: source/newparser.ml

source/machine.cmo: source/machine.skel source/dllzompvm.so
source/machine.cmx: source/machine.skel source/dllzompvm.so
source/zompvm.cmo: source/machine.cmo
source/zompvm.cmx: source/machine.cmx

source/lang.cmi: source/common.cmo
source/expander.cmi: source/lang.cmi
source/bindings.cmi: source/common.cmo source/lang.cmo

################################################################################
# Additional utility targets
################################################################################

TAGS:
	@$(ECHO) Generating tags ...
	otags 2> /dev/null || $(ECHO) "otags not found, no tags generated"

# generate a file for graphviz which visualizes the dependencies between modules
deps.dot deps.png: depends.mk $(CAMLDEP_INPUT) $(LANG_CMOS)
	@$(ECHO) Generating dependency graph for graphviz ...
	$(OCAMLDOC) -I source/ -o deps.dot -dot -dot-reduce $(CAMLDEP_INPUT) source/newparser.ml
	dot -Tpng deps.dot > deps.png || $(ECHO) "warning: dot not found, deps.png not generated"

# Warning, ugly. But at least not essential so who cares :)
.PHONY: loc_stats loc_stats_no_summary

ML_SRC_FILE_NAMES = ast2.ml bindings.ml bindings.mli common.ml compileutils.ml    \
    expander.ml gen_c_bindings.ml genllvm.ml indentlexer.ml indentlexer.mli         \
    indentlexer_tests.ml lang.ml machine.ml newparser.mly newparser_tests.ml \
    parseutils.ml semantic.ml zomp_shell.ml\
    testing.ml typesystems.ml zompc.ml zompvm.ml
ML_SRC_FILES = $(foreach file, $(ML_SRC_FILE_NAMES), source/$(file))

loc_stats_no_summary:
	$(LS) $(wildcard source/*.ml source/*.mli source/*.mly source/*.mll) | grep -v source/newparser.ml | xargs $(LINE_COUNT) | $(SORT) -n
	$(LINE_COUNT) $(wildcard *.mk) makefile | $(SORT) -n
	$(LINE_COUNT) libs/libutils.cpp prelude.zomp source/runtime.c zomp.el source/zomputils.h source/zompvm.cpp source/zompvm.h source/zompvm_dummy.cpp | $(SORT) -n
	$(LINE_COUNT) $(wildcard libs/*.skel) | $(SORT) -n
	$(LS) $(wildcard libs/*.zomp) | grep -v libs/opengl20.\*\.zomp | grep -v libs/glfw\.zomp | grep -v libs/quicktext\.zomp | grep -v libs/glut.zomp | xargs $(LINE_COUNT) | $(SORT) -n
	$(LINE_COUNT) $(wildcard examples/*.zomp) | $(SORT) -n
	$(LINE_COUNT) $(wildcard testsuite/*.zomp) | $(SORT) -n
	$(LS) $(wildcard tests/*.zomp) | grep -v sharkperf.zomp | xargs $(LINE_COUNT) | $(SORT) -n

loc_stats: loc_stats_no_summary
	make -ks loc_stats_no_summary | grep total | awk '{ sum = sum + $$1; } END { print sum " total lines of code "; }'

GITSTATS = $(ZOMP_TOOL_PATH)/gitstats/gitstats
.PHONY: git_repo_stats
git_repo_stats:
	@$(ECHO) "Creating git repository statistincs ..."
	$(GITSTATS) . build/stats
	@$(ECHO) "Open build/stats/index.html to see git repository statistics"

################################################################################
# Cleaning
################################################################################

.PHONY: clean clean_tags clean_all

clean: $(CLEAN_SUB_TARGETS)
	@$(ECHO) "Cleaning ..."
	cd tests && make clean_tests
	$(RM) -f $(foreach f,$(LANG_CMOS),${f:.cmo=.cm?})
	$(RM) -f $(foreach f,$(LANG_CMOS),${f:.cmo=.o}) source/zompc.o source/zomp_shell.o
	$(RM) -f expander_tests.cm?
	$(RM) -f source/zompc.cm? zompc
	$(RM) -f source/runtime.bc source/runtime.ll source/runtime.o
	$(RM) -f zomp_shell source/zomp_shell.cmi source/zomp_shell.cmo
	$(RM) -f source/gen_c_bindings.cmi source/gen_c_bindings.cmo source/gen_c_bindings
	$(RM) -f source/machine.c source/machine.ml source/machine.cmi source/machine.cmo source/machine.o
	$(RM) -f forktest forktest.cmi forktest.cmo
	$(RM) -f source/dllzompvm.so dllzompvm.so source/libzompvm.a
	$(RM) -f source/zompvm.o source/zompvm_dummy.o
	$(RM) -f testdll.o dlltest.dylib
	$(RM) -f *_flymake.*
	$(RM) -f source/*_flymake.*
	$(RM) -f source/*.cmx *.native
	$(RM) -f deps.png deps.dot
	$(RM) -f depends.mk
	$(RM) -f libs/opengl20.zomp libs/glfw.zomp libs/opengl20print.zomp libs/quicktext.zomp libs/glut.zomp
	$(RM) -f libs/glQuickText.o libs/libquicktext.dylib libs/libglut.dylib
	$(RM) -f source/indentlexer.cm? source/newparser.cm? source/newparser.o source/indentlexer.o source/newparser.ml source/newparser.mli source/newparser.conflicts
	$(RM) -f source/newparser_tests.cmi source/newparser_tests.cmo source/newparser_tests source/newparser_tests.o
	$(RM) -f source/indentlexer_tests.cmo source/indentlexer_tests.cmi
	$(RM) -f source/expandertests.cm? source/alltests.cm? source/alltests
	$(RM) -f perflog.txt
	$(RM) -f mltest
	$(RM) -f libs/libutils.dylib
	$(RM) -f has_llvm has_clang
	$(RM) -f gmon.out

clean_tags:
	$(RM) -f source/*.annot
	$(RM) -f source/*.conflicts
	$(RM) -f TAGS
	$(RM) -f $(FLYMAKE_LOG)

clean_all: clean clean_tags

