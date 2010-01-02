#
# Makefile for zomp project
#

# Config #
################################################################################

ifndef DEBUG
DEBUG=1
endif

ifndef PWD
PWD=`pwd`
endif

ZOMP_TOOL_PATH=$(PWD)/tools
include config.mk


PATH:=$(LLVM_BIN_DIR):$(LLVM_GCC_BIN_DIR):$(PATH)

help:
	@$(ECHO) "PATH = $(PATH)" | $(TR) : \\n
	@$(ECHO) "CXX_FLAGS = '"$(CXX_FLAGS)"'"
	@$(ECHO) "LLVM_BIN_DIR = $(LLVM_BIN_DIR)"
	@$(ECHO) "LLVM_CONFIG = $(LLVM_CONFIG)"
	@$(ECHO) "CC = $(CC)"
	@$(ECHO) "CXX = $(CXX)"

FLYMAKE_LOG=flymake.log

CAML_INCLUDE=
CAML_PP=

CAML_FLAGS= $(CAML_INCLUDE) $(CAML_PP)
CAML_NATIVE_FLAGS = $(CAML_INCLUDE) $(CAML_PP) -p

ARCHFLAG = -m32

CXX_FLAGS=-I /usr/local/lib/ocaml/ -I $(LLVM_INCLUDE_DIR) -L$(LLVM_LIB_DIR) $(ARCHFLAG)
C_FLAGS=-I /usr/local/lib/ocaml/ $(ARCHFLAG)

# ifeq ($(DEBUG),1)
# $(echo "Debug build")
# else
# $(echo "Release build")
# endif

CFLAGS = $(C_FLAGS)
CXXFLAGS = $(CXX_FLAGS)

ifeq ($(DEBUG), 1)
OCAMLC += -g
CXX_FLAGS += -pg -g
C_FLAGS += -pg -g
else
CXX_FLAGS += -O5
endif

CAML_LIBS = str.cma bigarray.cma
LANG_CMOS = common.cmo testing.cmo typesystems.cmo bindings.cmo ast2.cmo \
lang.cmo semantic.cmo sexprparser.cmo sexprlexer.cmo genllvm.cmo dllzompvm.so \
machine.cmo zompvm.cmo indentlexer.cmo newparser.cmo parseutils.cmo expander.cmo \
testing.cmo compileutils.cmo

SEXPR_TL_INPUT = common.cmo ast2.cmo sexprparser.cmo sexprlexer.cmo \
bindings.cmo typesystems.cmo lang.cmo semantic.cmo genllvm.cmo common.cmo \
machine.cmo dllzompvm.so zompvm.cmo newparser.cmo indentlexer.cmo \
parseutils.cmo expander.cmo testing.cmo compileutils.cmo testing.cmo \
sexprtoplevel.cmo

LLVM_LIBS=`$(LLVM_CONFIG) --libs all | $(SED) 's![^ ]*/Release/lib/LLVMCBase.o!!'`
LLVM_LIBS_CAML=-cclib "$(LLVM_LIBS)"
LANG_CMXS=common.cmx ast2.cmx sexprparser.cmx sexprlexer.cmx bindings.cmx      \
typesystems.cmx lang.cmx semantic.cmx genllvm.cmx machine.cmx -cclib -lstdc++  \
$(LLVM_LIBS_CAML) libzompvm.a zompvm.cmx indentlexer.cmx newparser.cmx         \
parseutils.cmx expander.cmx testing.cmx compileutils.cmx

# Combined targets
################################################################################

all: byte native stdlib.bc stdlib.ll libbindings tags deps.png mltest zompvm_dummy.o
libbindings: gencode opengl20.zomp opengl20print.zomp glfw.zomp glut.zomp libglut.dylib quicktext.zomp libquicktext.dylib
byte: dllzompvm.so zompc sexprtoplevel
native: dllzompvm.so $(LANG_CMOS:.cmo=.cmx) sexprtoplevel.native zompc.native

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
	cd tools/llvm-$(LLVM_VERSION) && ./configure EXTRA_OPTIONS=$(ARCHFLAG)

tools/llvm: tools/llvm-$(LLVM_VERSION)
	@$(ECHO) Building LLVM $(LLVM_VERSION)
	cd tools/llvm-$(LLVM_VERSION) && make
	@$(ECHO) Linking $@ to tools/llvm-$(LLVM_VERSION)
	ln -s llvm-$(LLVM_VERSION) $@

tools/llvm-$(LLVM_VERSION)/TAGS:
	@$(ECHO) Building tags for LLVM $(LLVM_VERSION)
	cd tools/llvm-$(LLVM_VERSION)/ && find -E lib include -regex ".*\.(cpp|h)" | xargs etags -o TAGS

tools/llvm/TAGS: tools/llvm-$(LLVM_VERSION)/TAGS

################################################################################
# External libraries

libglut.dylib:
	@$(ECHO) Building $@ ...
	$(CC) $(CFLAGS) -dynamiclib -framework GLUT -o $@

libquicktext.dylib: glQuickText.o
	@$(ECHO) Building $@ ...
	$(CXX) $(CXXFLAGS) -dynamiclib -o $@ glQuickText.o -framework OpenGL

EXTLIB_DIR = extlibs
# ASSIMP_DIR = $(EXTLIB_DIR)/assimp--1.0.412-sdk
ASSIMP_DIR = $(EXTLIB_DIR)/assimp-svn

SCONS = $(PWD)/extlibs/scons/scons.py
ifeq ($(DEBUG), 1)
SCONSFLAGS += "debug=1"
endif

extlib_assimp:
	cd $(ASSIMP_DIR)/workspaces/SCons; $(SCONS) $(SCONSFLAGS)

libassimp.a: $(ASSIMP_DIR)/workspaces/SCons/libassimp.a makefile
	- rm -f $@
	ln -s $< $@

assimp.dylib: libassimp.a makefile forcelinkassimp.c
	$(CXX) $(CXXFLAGS) -dynamiclib -o $@ -I $(ASSIMP_DIR)/include -L. -lassimp forcelinkassimp.c

# opengl.dylib: opengl.c
# 	@$(ECHO) Building $@ ...
# 	$(CC) -c opengl.c -o opengl.o
# 	$(CC) -dynamiclib -framework OpenGL opengl.o -o $@

################################################################################

dllzompvm.so: zompvm.h zompvm.cpp machine.c stdlib.o stdlib.ll
	@$(ECHO) Building $@ ...
	$(LLVM_CXX) $(CXX_FLAGS) `$(LLVM_CONFIG) --cxxflags` -c zompvm.cpp -o zompvm.o
	$(CC) $(CFLAGS) -I /usr/local/lib/ocaml/ -c machine.c -o machine.o
	ocamlmklib -o zompvm zompvm.o stdlib.o machine.o -lstdc++ -L$(LLVM_LIB_DIR) $(LLVM_LIBS)

sexprtoplevel: $(SEXPR_TL_INPUT) $(LANG_CMOS:.cmo=.cmx) machine.cmo zompvm.cmo
	@$(ECHO) Building $@ ...
	$(OCAMLC) $(CAML_FLAGS) -o $@ $(CAML_LIBS) $(SEXPR_TL_INPUT)

sexprtoplevel.native: $(SEXPR_TL_INPUT:.cmo=.cmx) $(TL_CMXS) dllzompvm.so machine.cmx zompvm.cmx
	@$(ECHO) Building $@ ...
	$(OCAMLOPT) $(CAML_NATIVE_FLAGS) -o $@ -I $(LLVM_LIB_DIR) str.cmxa bigarray.cmxa $(LANG_CMXS) sexprtoplevel.cmx

zompc.native: $(LANG_CMOS:.cmo=.cmx) zompc.cmx dllzompvm.so
	@$(ECHO) Building $@ ...
	$(OCAMLOPT) $(CAML_NATIVE_FLAGS)  -o $@ -I $(LLVM_LIB_DIR) $(CAML_LIBS:.cma=.cmxa) $(LANG_CMXS) zompc.cmx

zompc: $(LANG_CMOS) zompc.cmo dllzompvm.so
	@$(ECHO) Building $@ ...
	$(OCAMLC) $(CAML_FLAGS)  -o $@ $(CAML_LIBS) $(LANG_CMOS) dllzompvm.so machine.cmo zompvm.cmo zompc.cmo

gencode: gencode.cmo gencode.ml
	@$(ECHO) Building $@ ...
	$(OCAMLC) $(CAML_FLAGS)  -o $@ $(CAML_LIBS) gencode.cmo

machine.c machine.ml: gencode machine.skel
	@$(ECHO) Making OCaml bindings for zomp-machine ...
	./gencode machine

NEWPARSER_CMOS = common.cmo testing.cmo ast2.cmo newparser.cmo indentlexer.cmo

# Tests #
################################################################################

TEST_CMOS = indentlexer_tests.cmo newparser_tests.cmo

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

profile_comp: zompc zompc.native stdlib.bc opengl20.zomp glfw.zomp
	cd examples && $(RM) -f $(PROF_COMP_TARGET).ll $(PROF_COMP_TARGET).bc
	cd examples && time make $(PROF_COMP_TARGET).ll $(PROF_COMP_TARGET).bc ZOMPCFLAGS=--print-timings

runtests: $(LANG_CMOS) #expander_tests.cmo
	@$(ECHO) Running tests ...
	cd tests && time make clean_tests check

runtestsuite: all
	cd testsuite && time make all

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

# perftest_quick: zompc.native zompc
# 	cd tests && make clean_tests compileperftest FUNCTION_COUNTS="10 1000 2000 4000"
# 	gnuplot makeperfgraph.gnuplot || $(ECHO) "Could not execute gnuplot"

stdlib.bc stdlib.ll: stdlib.c
	@$(ECHO) Building bytecode standard library $@ ...
	$(LLVM_CC) --emit-llvm -c $< -o stdlib.bc
	$(LLVM_DIS) < stdlib.bc > stdlib.orig.ll
	($(SED) 's/nounwind//' | $(SED) 's/readonly//') < stdlib.orig.ll > stdlib.ll
	$(RM) -f stdlib.bc stdlib.orig.ll
	$(LLVM_AS) < stdlib.ll > stdlib.bc

# generate a file for graphviz which visualizes the dependencies
# between modules
deps.dot deps.png: depends.mk $(CAMLDEP_INPUT)
	@$(ECHO) Generating dependency graph for graphviz ...
	ocamldot depends.mk > deps.dot || $(ECHO) "ocamldot not found, no graphviz deps graph generated"
	dot -Tpng deps.dot > deps.png || $(ECHO) "dot not found, deps.png not generated"

# Rules #
################################################################################

.PHONY: clean clean_all runtestsuite perftest2 perftest runtestsuite runtests \
   profile_comp exampletests runmltests mltest alltests

%.ml: %.mly
	@$(ECHO) Generating parser $< ...
	$(MENHIR) --explain --infer $<
	$(OCAMLC) $(CAML_FLAGS) -c $(<:.mly=.mli)

%.ml: %.mll
	@$(ECHO) Generating lexer $< ...
	$(OCAMLLEX) $<

# %.cmi: %.ml
# 	@$(ECHO) Compiling $< triggered by $@ ...
# 	$(OCAMLC) $(CAML_FLAGS)  -c $<

%.cmi: %.mli
	@$(ECHO) "Compiling $@ ..."
	$(OCAMLC) $(CAML_FLAGS) -c $<

%.cmo %.cmi: %.ml
	@$(ECHO) "Compiling (bytecode) $< ..."
	$(OCAMLC) $(CAML_FLAGS)  -c $<

%.cmx: %.ml
	@$(ECHO) "Compiling (native) $< ..."
	$(OCAMLOPT) $(CAML_NATIVE_FLAGS)  -c $<

%.zomp: %.skel gencode
	@$(ECHO) Generating Zomp bindings for $(<:.skel=) ...
	./gencode -lang zomp $(<:.skel=)

opengl20print.zomp: opengl20.skel gencode
	@$(ECHO) Generating OpenGL enum printer ...
	$(CP) opengl20.skel opengl20print.skel
	./gencode -lang zomp-glprinter opengl20print
	$(RM) -f opengl20print.skel

CAMLDEP_INPUT= ast2.ml bindings.ml common.ml expander.ml gencode.ml genllvm.ml \
indentlexer.ml indentlexer.mli indentlexer_tests.ml lang.ml machine.ml         \
newparser_tests.ml parseutils.ml compileutils.ml semantic.ml sexprtoplevel.ml  \
testing.ml typesystems.ml zompc.ml zompvm.ml

# Dependencies #
################################################################################

depends.mk: $(CAMLDEP_INPUT)
	@$(ECHO) Calculating dependencies ...
	$(OCAMLDEP) $(CAML_PP) $(CAMLDEP_INPUT) > depends.mk

glfw.zomp: gencode
opengl20.zomp: gencode

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

# genllvm.cmo: genllvm.cmi
# genllvm.cmx: genllvm.cmi

lang.cmi: common.cmo
bindings.cmi: common.cmo lang.cmo

# Generate tags if otags exists #
################################################################################

tags:
	@$(ECHO) Generating tags ...
	otags 2> /dev/null || $(ECHO) "otags not found, no tags generated"

# Cleaning #
################################################################################

cleanml:
	$(RM) -f *.cmo *.cmi

clean:
	@$(ECHO) Cleaning...
	cd tests && make clean_tests
	$(RM) -f $(foreach f,$(LANG_CMOS),${f:.cmo=.cm?})
	$(RM) -f $(foreach f,$(LANG_CMOS),${f:.cmo=.o}) zompc.o sexprtoplevel.o
	$(RM) -f expander_tests.cm?
	$(RM) -f zompc.cm? zompc
	$(RM) -f stdlib.bc stdlib.o
	$(RM) -f sexprlexer.cmi sexprlexer.cmo sexprlexer.ml
	$(RM) -f sexprparser.cmi sexprparser.cmo sexprparser.ml sexprparser.mli
	$(RM) -f sexprtoplevel sexprtoplevel.cmi sexprtoplevel.cmo
	$(RM) -f gencode.cmi gencode.cmo gencode
	$(RM) -f machine.c machine.ml machine.cmi machine.cmo machine.o
	$(RM) -f forktest forktest.cmi forktest.cmo
	$(RM) -f dllzompvm.so libzompvm.a zompvm.o
	$(RM) -f testdll.o dlltest.dylib
	$(RM) -f *_flymake.*
	$(RM) -f *.cmx *.native
	$(RM) -f deps.png deps.dot
	$(RM) -f depends.mk
	$(RM) -f stdlib.ll
	$(RM) -f opengl20.zomp glfw.zomp opengl20print.zomp
	$(RM) -f indentlexer.cm? newparser.cm? newparser.o indentlexer.o newparser.ml newparser.mli newparser.conflicts
	$(RM) -f newparser_tests.cmi newparser_tests.cmo newparser_tests newparser_tests.o
	$(RM) -f indentlexer_tests.cmo indentlexer_tests.cmi
	$(RM) -f expandertests.cm? alltests.cm? alltests
	$(RM) -f glQuickText.o libquicktext.dylib libglut.dylib
	$(RM) -f perflog.txt
	$(RM) -f mltest
	cd examples/ && make clean
	cd testsuite/ && make clean

clean_tags:
	$(RM) -f *.annot
	$(RM) -f *.conflicts
	$(RM) -f TAGS
	$(RM) -f $(FLYMAKE_LOG)

clean_all: clean clean_tags

include flymake.mk
include depends.mk

