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

ZOMP_TOOL_PATH = $(PWD)/tools
include config.mk

FLYMAKE_LOG=flymake.log
include flymake.mk

-include depends.mk
include testsuite/makefile
include examples/makefile
include examples/smallpt/makefile

PATH := $(LLVM_BIN_DIR):$(LLVM_GCC_BIN_DIR):$(PATH)

CXXFLAGS = -I /usr/local/lib/ocaml/ -I $(LLVM_INCLUDE_DIR) -L$(LLVM_LIB_DIR) $(ARCHFLAG)
CCFLAGS = -I /usr/local/lib/ocaml/ $(ARCHFLAG)
LDFLAGS = $(ARCHFLAG)

ifeq ($(DEBUG), 1)
$(info Debug build, LLVM variant = $(LLVM_VARIANT))
OCAMLC += -g
CXXFLAGS += -pg -g
CCFLAGS += -pg -g
else
$(info Release build, LLVM variant = $(LLVM_VARIANT))
CXXFLAGS += -O5
endif

################################################################################
# Combined/main targets
################################################################################

all: byte native runtime.bc runtime.ll libbindings TAGS deps.png mltest \
    zompvm_dummy.o
libbindings: gen_c_bindings opengl20.zomp opengl20print.zomp glfw.zomp glut.zomp \
    libglut.dylib quicktext.zomp libquicktext.dylib libutils.dylib
byte: dllzompvm.so zompc zomp_shell
native: dllzompvm.so $(LANG_CMOS:.cmo=.cmx) zomp_shell.native zompc.native

CAML_LIBS = str.cma bigarray.cma
LANG_CMOS = common.cmo testing.cmo typesystems.cmo bindings.cmo ast2.cmo \
    lang.cmo semantic.cmo sexprparser.cmo sexprlexer.cmo machine.cmo zompvm.cmo \
    genllvm.cmo dllzompvm.so indentlexer.cmo newparser.cmo parseutils.cmo \
    expander.cmo testing.cmo compileutils.cmo

SEXPR_TL_INPUT = $(LANG_CMOS) zomp_shell.cmo

# SEXPR_TL_INPUT = common.cmo ast2.cmo sexprparser.cmo sexprlexer.cmo \
#     bindings.cmo typesystems.cmo lang.cmo semantic.cmo genllvm.cmo  \
#     common.cmo machine.cmo dllzompvm.so zompvm.cmo newparser.cmo    \
#     indentlexer.cmo parseutils.cmo expander.cmo testing.cmo         \
#     compileutils.cmo testing.cmo zomp_shell.cmo

################################################################################
# Zomp tools
################################################################################

dllzompvm.so: zompvm.h zompvm.cpp machine.c runtime.o runtime.ll
	@$(ECHO) Building $@ ...
	$(LLVM_CXX) $(CXXFLAGS) `$(LLVM_CONFIG) --cxxflags` -c zompvm.cpp -o zompvm.o
	$(CC) $(CCFLAGS) -I /usr/local/lib/ocaml/ -c machine.c -o machine.o
ifeq "$(BUILD_PLATFORM)" "Linux"
	$(CXX) $(DLL_FLAG) $(LDFLAGS) -o zompvm -DPIC -fPIC zompvm.o runtime.o machine.o -L$(LLVM_LIB_DIR) $(LLVM_LIBS)
else # OS X
	ocamlmklib -o zompvm zompvm.o runtime.o machine.o -lstdc++ -L$(LLVM_LIB_DIR) $(LLVM_LIBS)
endif

zomp_shell: $(SEXPR_TL_INPUT) $(LANG_CMOS:.cmo=.cmx)
	@$(ECHO) Building $@ ...
	$(OCAMLC) $(CAML_FLAGS) -o $@ $(CAML_LIBS) $(SEXPR_TL_INPUT)

zomp_shell.native: $(SEXPR_TL_INPUT:.cmo=.cmx) dllzompvm.so
	@$(ECHO) Building $@ ...
	$(OCAMLOPT) -o $@ $(CAML_NATIVE_FLAGS) -I $(LLVM_LIB_DIR) str.cmxa bigarray.cmxa $(LANG_CMXS) zomp_shell.cmx

zompc.native: $(LANG_CMOS:.cmo=.cmx) zompc.cmx dllzompvm.so
	@$(ECHO) Building $@ ...
	$(OCAMLOPT) $(CAML_NATIVE_FLAGS)  -o $@ -I $(LLVM_LIB_DIR) $(CAML_LIBS:.cma=.cmxa) $(LANG_CMXS) zompc.cmx

zompc: $(LANG_CMOS) zompc.cmo dllzompvm.so
	@$(ECHO) Building $@ ...
	$(OCAMLC) $(CAML_FLAGS)  -o $@ $(CAML_LIBS) $(LANG_CMOS) dllzompvm.so machine.cmo zompvm.cmo zompc.cmo

gen_c_bindings: gen_c_bindings.cmo gen_c_bindings.ml
	@$(ECHO) Building $@ ...
	$(OCAMLC) $(CAML_FLAGS)  -o $@ $(CAML_LIBS) gen_c_bindings.cmo

machine.c machine.ml: gen_c_bindings machine.skel
	@$(ECHO) Making OCaml bindings for zomp-machine ...
	./gen_c_bindings machine

NEWPARSER_CMOS = common.cmo testing.cmo ast2.cmo newparser.cmo indentlexer.cmo

################################################################################
# Tests
################################################################################

TEST_CMOS = indentlexer_tests.cmo newparser_tests.cmo

.PHONY: runtestsuite perftest2 perftest runtestsuite runtests \
   profile_comp exampletests runmltests mltest alltests

alltests: runmltests runtestsuite exampletests

mltest: testing.cmo $(LANG_CMOS) $(NEWPARSER_CMOS) $(TEST_CMOS)
	@$(ECHO) Building $@ ...
	$(OCAMLC) $(CAML_FLAGS) -o $@ bigarray.cma str.cma $(LANG_CMOS) $(NEWPARSER_CMOS) $(TEST_CMOS)

runmltests: mltest
	@$(ECHO) Running all tests ...
	$(OCAMLRUN) -b ./mltest

exampletests:
	@$(ECHO) Compiling examples ...
	cd examples && make clean && make test

PROF_COMP_TARGET=metaballs

profile_comp: zompc zompc.native runtime.bc opengl20.zomp glfw.zomp
	cd examples && $(RM) -f $(PROF_COMP_TARGET).ll $(PROF_COMP_TARGET).bc
	cd examples && time make $(PROF_COMP_TARGET).ll $(PROF_COMP_TARGET).bc ZOMPCFLAGS=--print-timings

runtests: $(LANG_CMOS) #expander_tests.cmo
	@$(ECHO) Running tests ...
	cd tests && time make clean_tests check

runtestsuite: all testsuite/all

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

runtime.bc runtime.ll: runtime.c
	@$(ECHO) Building bytecode standard library $@ ...
	$(LLVM_CC) --emit-llvm -c $< -o runtime.bc
	$(LLVM_DIS) < runtime.bc > runtime.orig.ll
	($(SED) 's/nounwind//' | $(SED) 's/readonly//') < runtime.orig.ll > runtime.ll
	$(RM) -f runtime.bc runtime.orig.ll
	$(LLVM_AS) < runtime.ll > runtime.bc

################################################################################
# Rules
################################################################################

%.ml: %.mly
	@$(ECHO) Generating parser $< ...
	$(MENHIR) --explain --infer $<
	$(OCAMLC) $(CAML_FLAGS) -c $(<:.mly=.mli)

%.ml: %.mll
	@$(ECHO) Generating lexer $< ...
	$(OCAMLLEX) $<

%.cmi: %.mli
	@$(ECHO) "Compiling $@ ..."
	$(OCAMLC) $(CAML_FLAGS) -c $<

%.cmo %.cmi: %.ml
	@$(ECHO) "Compiling (bytecode) $< ..."
	$(OCAMLC) $(CAML_FLAGS)  -c $<

%.cmx: %.ml
	@$(ECHO) "Compiling (native) $< ..."
	$(OCAMLOPT) $(CAML_NATIVE_FLAGS)  -c $<

%.o: %.c
	$(CC) $(CCFLAGS) -c -o $@ $<

################################################################################
# External libraries
################################################################################

libglut.dylib:
	@$(ECHO) Building $@ ...
	$(CC) $(DLL_FLAG) $(LDFLAGS) $(LINK_GLUT) -o $@

libquicktext.dylib: glQuickText.o
	@$(ECHO) Building $@ ...
	$(CXX) $(DLL_FLAG) $(LDFLAGS) -o $@ glQuickText.o $(LINK_GL)

libutils.dylib: libutils.cpp
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

%.zomp: %.skel gen_c_bindings
	@$(ECHO) Generating Zomp bindings for $(<:.skel=) ...
	./gen_c_bindings -lang zomp $(<:.skel=)

opengl20print.zomp: opengl20.skel gen_c_bindings
	@$(ECHO) Generating OpenGL enum printer ...
	$(CP) opengl20.skel opengl20print.skel
	./gen_c_bindings -lang zomp-glprinter opengl20print
	$(RM) -f opengl20print.skel

################################################################################
# LLVM download and compilation
################################################################################

LLVM_VERSION=2.5

tools/llvm-$(LLVM_VERSION).tar.gz:
	@$(ECHO) Downloading $@ ...
	mkdir -p tools
	curl http://llvm.org/releases/$(LLVM_VERSION)/llvm-$(LLVM_VERSION).tar.gz -o $@ -s -S

tools/llvm-$(LLVM_VERSION): tools/llvm-$(LLVM_VERSION).tar.gz
	@$(ECHO) Unpacking $@ ...
	cd tools && gunzip --stdout llvm-$(LLVM_VERSION).tar.gz | tar -xvf -
	touch $@ # tar sets date from archive. avoid downloading the archive twice
	@$(ECHO) Configuring LLVM $(LLVM_VERSION)
	cd tools/llvm-$(LLVM_VERSION) && ./configure EXTRA_OPTIONS="$(LLVM_EXTRA_OPTIONS)"

tools/llvm: tools/llvm-$(LLVM_VERSION)
	@$(ECHO) Building LLVM $(LLVM_VERSION)
	cd tools/llvm-$(LLVM_VERSION) && (make EXTRA_OPTIONS="$(LLVM_EXTRA_OPTIONS)"; make ENABLE_OPTIMIZED=0 EXTRA_OPTIONS="$(LLVM_EXTRA_OPTIONS)")
	@$(ECHO) Linking $@ to tools/llvm-$(LLVM_VERSION)
	ln -s llvm-$(LLVM_VERSION) $@

tools/llvm-$(LLVM_VERSION)/TAGS:
	@$(ECHO) Building tags for LLVM $(LLVM_VERSION)
	cd tools/llvm-$(LLVM_VERSION)/ && find -E lib include -regex ".*\.(cpp|h)" | xargs etags -o TAGS

tools/llvm/TAGS: tools/llvm-$(LLVM_VERSION)/TAGS

LLVM_LIBS=`$(LLVM_CONFIG) --libs all | $(SED) 's![^ ]*/Release/lib/LLVMCBase.o!!'`
LLVM_LIBS_CAML=-cclib "$(LLVM_LIBS)"
LANG_CMXS=common.cmx ast2.cmx sexprparser.cmx sexprlexer.cmx bindings.cmx \
    typesystems.cmx lang.cmx semantic.cmx machine.cmx zompvm.cmx genllvm.cmx \
    -cclib -lstdc++ $(LLVM_LIBS_CAML) libzompvm.a indentlexer.cmx newparser.cmx \
    parseutils.cmx expander.cmx testing.cmx compileutils.cmx

################################################################################
# Dependencies
#
# Try to detect dependencies automatically. Does not work for everything, yet,
# add additional ones here. Use something like make -j 20 for a quick test
################################################################################

depends.mk: $(CAMLDEP_INPUT)
	@$(ECHO) Calculating dependencies ...
	$(OCAMLDEP) $(CAML_PP) $(CAMLDEP_INPUT) > depends.mk

CAMLDEP_INPUT= ast2.ml bindings.ml common.ml expander.ml gen_c_bindings.ml genllvm.ml \
    indentlexer.ml indentlexer.mli indentlexer_tests.ml lang.ml machine.ml     \
    newparser_tests.ml parseutils.ml compileutils.ml semantic.ml               \
    zomp_shell.ml testing.ml typesystems.ml zompc.ml zompvm.ml

glfw.zomp: gen_c_bindings
opengl20.zomp: gen_c_bindings

newparser.ml: newparser.mly ast2.cmo
newparser_tests.cmo: newparser.cmo
newparser_tests.cmx: newparser.cmx
indentlexer.cmo: newparser.ml
indentlexer.cmi: newparser.ml

sexprparser.ml: ast2.cmo common.cmo
sexprlexer.ml: ast2.ml common.ml sexprparser.cmo
machine.cmo: machine.skel dllzompvm.so
machine.cmx: machine.skel dllzompvm.so
zompvm.cmo: machine.cmo
zompvm.cmx: machine.cmx
parseutils.cmo: sexprlexer.cmo
parseutils.cmx: sexprlexer.cmx

lang.cmi: common.cmo
bindings.cmi: common.cmo lang.cmo

################################################################################
# Additional utility targets
################################################################################

TAGS:
	@$(ECHO) Generating tags ...
	otags 2> /dev/null || $(ECHO) "otags not found, no tags generated"

# generate a file for graphviz which visualizes the dependencies between modules
deps.dot deps.png: depends.mk $(CAMLDEP_INPUT) $(LANG_CMOS)
	@$(ECHO) Generating dependency graph for graphviz ...
	$(OCAMLDOC) -o deps.dot -dot -dot-reduce $(CAMLDEP_INPUT) newparser.ml sexprlexer.ml sexprparser.ml
	dot -Tpng deps.dot > deps.png || $(ECHO) "warning: dot not found, deps.png not generated"

# Warning, ugly. But at least not essential so who cares :)
.PHONY: loc_stats loc_stats_no_summary

ML_SRC_FILES = ast2.ml bindings.ml bindings.mli common.ml compileutils.ml    \
    expander.ml gen_c_bindings.ml genllvm.ml indentlexer.ml indentlexer.mli         \
    indentlexer_tests.ml lang.ml machine.ml newparser.mly newparser_tests.ml \
    parseutils.ml semantic.ml sexprlexer.mll sexprparser.mly zomp_shell.ml\
    testing.ml typesystems.ml zompc.ml zompvm.ml

loc_stats_no_summary:
	$(LS) $(wildcard *.ml *.mli *.mly *.mll) | grep -v sexprparser.ml | grep -v newparser.ml | xargs $(LINE_COUNT) | $(SORT) -n
	$(LINE_COUNT) $(wildcard *.mk) makefile | $(SORT) -n
	$(LINE_COUNT) libutils.cpp prelude.zomp runtime.c zomp.el zomputils.h zompvm.cpp zompvm.h zompvm_dummy.cpp | $(SORT) -n
	$(LINE_COUNT) $(wildcard *.skel) | $(SORT) -n
	$(LS) $(wildcard libs/*.zomp) | grep -v opengl20.\*\.zomp | grep -v glfw\.zomp | grep -v quicktext\.zomp | grep -v glut.zomp | xargs $(LINE_COUNT) | $(SORT) -n
	$(LINE_COUNT) $(wildcard examples/*.zomp) | $(SORT) -n
	$(LINE_COUNT) $(wildcard testsuite/*.zomp) | $(SORT) -n
	$(LS) $(wildcard tests/*.zomp) | grep -v sharkperf.zomp | xargs $(LINE_COUNT) | $(SORT) -n

loc_stats: loc_stats_no_summary
	make -ks loc_stats_no_summary | grep total | awk '{ sum = sum + $$1; } END { print sum " total lines of code "; }'

################################################################################
# Cleaning
################################################################################

.PHONY: clean clean_tags clean_all

clean: testsuite/clean examples/clean
	@$(ECHO) Cleaning...
	cd tests && make clean_tests
	$(RM) -f $(foreach f,$(LANG_CMOS),${f:.cmo=.cm?})
	$(RM) -f $(foreach f,$(LANG_CMOS),${f:.cmo=.o}) zompc.o zomp_shell.o
	$(RM) -f expander_tests.cm?
	$(RM) -f zompc.cm? zompc
	$(RM) -f runtime.bc runtime.ll runtime.o
	$(RM) -f sexprlexer.cmi sexprlexer.cmo sexprlexer.ml
	$(RM) -f sexprparser.cmi sexprparser.cmo sexprparser.ml sexprparser.mli
	$(RM) -f zomp_shell zomp_shell.cmi zomp_shell.cmo
	$(RM) -f gen_c_bindings.cmi gen_c_bindings.cmo gen_c_bindings
	$(RM) -f machine.c machine.ml machine.cmi machine.cmo machine.o
	$(RM) -f forktest forktest.cmi forktest.cmo
	$(RM) -f dllzompvm.so libzompvm.a zompvm.o
	$(RM) -f testdll.o dlltest.dylib
	$(RM) -f *_flymake.*
	$(RM) -f *.cmx *.native
	$(RM) -f deps.png deps.dot
	$(RM) -f depends.mk
	$(RM) -f opengl20.zomp glfw.zomp opengl20print.zomp
	$(RM) -f indentlexer.cm? newparser.cm? newparser.o indentlexer.o newparser.ml newparser.mli newparser.conflicts
	$(RM) -f newparser_tests.cmi newparser_tests.cmo newparser_tests newparser_tests.o
	$(RM) -f indentlexer_tests.cmo indentlexer_tests.cmi
	$(RM) -f expandertests.cm? alltests.cm? alltests
	$(RM) -f glQuickText.o libquicktext.dylib libglut.dylib
	$(RM) -f perflog.txt
	$(RM) -f mltest

clean_tags:
	$(RM) -f *.annot
	$(RM) -f *.conflicts
	$(RM) -f TAGS
	$(RM) -f $(FLYMAKE_LOG)

clean_all: clean clean_tags

