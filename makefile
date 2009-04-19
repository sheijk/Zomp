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

# help:
#	@echo "LLVM_LIBS = " $(LLVM_LIBS)
#	echo PATH $(PATH)
#	echo "CXX_FLAGS = '"$(CXX_FLAGS)"'"
#	echo LLVM_BIN_DIR $(LLVM_BIN_DIR)
#	echo LLVM_CONFIG $(LLVM_CONFIG)
#	echo llvm config help `${LLVM_CONFIG} --help`

FLYMAKE_LOG=flymake.log

CAML_INCLUDE=
CAML_PP=

CAML_FLAGS= $(CAML_INCLUDE) $(CAML_PP)
CAML_NATIVE_FLAGS = $(CAML_INCLUDE) $(CAML_PP) -p

CXX_FLAGS=-I /usr/local/lib/ocaml/ -I $(LLVM_INCLUDE_DIR) -L$(LLVM_LIB_DIR)

ifeq ($(DEBUG), 1)
OCAMLC += -g
CXX_FLAGS += -pg -g
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

LLVM_LIBS=`$(LLVM_CONFIG) --libs all | sed 's![^ ]*/Release/lib/LLVMCBase.o!!'`
LLVM_LIBS_CAML=-cclib "$(LLVM_LIBS)"
LANG_CMXS=common.cmx ast2.cmx sexprparser.cmx sexprlexer.cmx bindings.cmx      \
typesystems.cmx lang.cmx semantic.cmx genllvm.cmx machine.cmx -cclib -lstdc++  \
$(LLVM_LIBS_CAML) libzompvm.a zompvm.cmx indentlexer.cmx newparser.cmx         \
parseutils.cmx expander.cmx testing.cmx compileutils.cmx

# Combined targets
################################################################################

all: byte native stdlib.bc stdlib.ll libbindings tags deps.png mltest
libbindings: gencode opengl20.zomp glfw.zomp glut.zomp glut.dylib quicktext.zomp libquicktext.dylib
byte: dllzompvm.so zompc sexprtoplevel
native: dllzompvm.so $(LANG_CMOS:.cmo=.cmx) sexprtoplevel.native zompc.native

# LLVM download and compilation
################################################################################

LLVM_VERSION=2.5

tools/llvm-$(LLVM_VERSION).tar.gz:
	@echo Downloading $@ ...
	mkdir -p tools
	curl http://llvm.org/releases/$(LLVM_VERSION)/llvm-$(LLVM_VERSION).tar.gz -o $@ -s -S

tools/llvm-$(LLVM_VERSION): tools/llvm-$(LLVM_VERSION).tar.gz
	@echo Unpacking $@ ...
	cd tools && gunzip --stdout llvm-$(LLVM_VERSION).tar.gz | tar -xvf -
	touch $@ # tar sets date from archive. avoid downloading the archive twice
	@echo Configuring LLVM $(LLVM_VERSION)
	cd tools/llvm-$(LLVM_VERSION) && ./configure

tools/llvm: tools/llvm-$(LLVM_VERSION)
	@echo Building LLVM $(LLVM_VERSION)
	cd tools/llvm-$(LLVM_VERSION) && make
	@echo Linking $@ to tools/llvm-$(LLVM_VERSION)
	ln -s llvm-$(LLVM_VERSION) $@

tools/llvm-$(LLVM_VERSION)/TAGS:
	@echo Building tags for LLVM $(LLVM_VERSION)
	cd tools/llvm-$(LLVM_VERSION)/ && find -E lib include -regex ".*\.(cpp|h)" | xargs etags -o TAGS

tools/llvm/TAGS: tools/llvm-$(LLVM_VERSION)/TAGS

################################################################################
# External libraries

glut.dylib:
	@echo Building $@ ...
	gcc -dynamiclib -framework GLUT -o $@

libquicktext.dylib: glQuickText.o
	@echo Building $@ ...
	g++ -dynamiclib -o $@ glQuickText.o -framework OpenGL

EXTLIB_DIR = extlibs
ASSIMP_DIR = $(EXTLIB_DIR)/assimp-r281--sdk

libassimp.a: $(ASSIMP_DIR)/workspaces/SCons/libassimp.a makefile
	$(CP) $< $@

assimp.dylib: libassimp.a makefile forcelinkassimp.c
	g++ -dynamiclib -o $@ -I $(ASSIMP_DIR)/include -L. -lassimp forcelinkassimp.c

# opengl.dylib: opengl.c
# 	echo Building $@ ...
# 	gcc -c opengl.c -o opengl.o
# 	gcc -dynamiclib -framework OpenGL opengl.o -o $@

################################################################################

dllzompvm.so: zompvm.h zompvm.cpp machine.c stdlib.o
	echo Building $@ ...
	$(LLVM_CXX) $(CXX_FLAGS) `$(LLVM_CONFIG) --cxxflags` -c zompvm.cpp -o zompvm.o
	gcc $(CXX_FLAGS) -I /usr/local/lib/ocaml/ -c machine.c -o machine.o
	ocamlmklib -o zompvm zompvm.o stdlib.o machine.o -lstdc++ -L$(LLVM_LIB_DIR) $(LLVM_LIBS)

sexprtoplevel: $(SEXPR_TL_INPUT) $(LANG_CMOS:.cmo=.cmx) machine.cmo zompvm.cmo
	echo Building $@ ...
	$(OCAMLC) $(CAML_FLAGS) -o $@ $(CAML_LIBS) $(SEXPR_TL_INPUT)

sexprtoplevel.native: $(SEXPR_TL_INPUT:.cmo=.cmx) $(TL_CMXS) dllzompvm.so machine.cmx zompvm.cmx
	echo Building $@ ...
	$(OCAMLOPT) $(CAML_NATIVE_FLAGS) -o $@ -I $(LLVM_LIB_DIR) str.cmxa bigarray.cmxa $(LANG_CMXS) sexprtoplevel.cmx

zompc.native: $(LANG_CMOS:.cmo=.cmx) zompc.cmx dllzompvm.so
	echo Building $@ ...
	$(OCAMLOPT) $(CAML_NATIVE_FLAGS)  -o $@ -I $(LLVM_LIB_DIR) $(CAML_LIBS:.cma=.cmxa) $(LANG_CMXS) zompc.cmx

zompc: $(LANG_CMOS) zompc.cmo dllzompvm.so
	echo Building $@ ...
	$(OCAMLC) $(CAML_FLAGS)  -o $@ $(CAML_LIBS) $(LANG_CMOS) dllzompvm.so machine.cmo zompvm.cmo zompc.cmo

gencode: gencode.cmo gencode.ml
	echo Building $@ ...
	$(OCAMLC) $(CAML_FLAGS)  -o $@ $(CAML_LIBS) gencode.cmo

machine.c machine.ml: gencode machine.skel
	echo Making OCaml bindings for zomp-machine ...
	./gencode machine

NEWPARSER_CMOS = common.cmo testing.cmo ast2.cmo newparser.cmo indentlexer.cmo

# Tests #
################################################################################

TEST_CMOS = indentlexer_tests.cmo newparser_tests.cmo

alltests: runmltests runtestsuite exampletests

mltest: testing.cmo $(LANG_CMOS) $(NEWPARSER_CMOS) $(TEST_CMOS)
	echo Building $@ ...
	$(OCAMLC) $(CAML_FLAGS) -o $@ bigarray.cma str.cma $(LANG_CMOS) $(NEWPARSER_CMOS) $(TEST_CMOS)

runmltests: mltest
	@echo Running all tests ...
	$(OCAMLRUN) -b ./mltest

exampletests:
	@echo Compiling examples ...
	cd examples && make test

PROF_COMP_TARGET=metaballs

profile_comp: zompc zompc.native stdlib.bc opengl20.zomp glfw.zomp
	cd examples && rm -f $(PROF_COMP_TARGET).ll $(PROF_COMP_TARGET).bc
	cd examples && time make $(PROF_COMP_TARGET).ll $(PROF_COMP_TARGET).bc ZOMPCFLAGS=--print-timings

runtests: $(LANG_CMOS) #expander_tests.cmo
	echo Running tests ...
	cd tests && time make clean_tests check

runtestsuite: all
	cd testsuite && time make all

# FUNCTION_COUNTS=10 1000
PERFTEST_GEN=
# PERFTEST_GEN=_iexpr
FUNCTION_COUNTS=100 1000 2000 3000 4000 5000 6000 7000 8000

perftest: zompc.native zompc
	cd tests && make clean_tests compileperftest FUNCTION_COUNTS="$(FUNCTION_COUNTS)" PERFTEST_GEN=genperftest$(PERFTEST_GEN)
	gnuplot makeperfgraph.gnuplot || echo "Could not execute gnuplot"
	mv temp.png perf_results$(PERFTEST_GEN).png

perftest2: zompc.native zompc
	cd tests && make clean_tests compileperftest FUNCTION_COUNTS="$(FUNCTION_COUNTS)" PERFTEST_GEN=genperftest
	cp tests/timing.txt tests/timing_sexpr.txt
	cd tests && make clean_tests compileperftest FUNCTION_COUNTS="$(FUNCTION_COUNTS)" PERFTEST_GEN=genperftest_iexpr
	cp tests/timing.txt tests/timing_iexpr.txt
	gnuplot makeperfgraph2.gnuplot || echo "Could not execute gnuplot"

# perftest_quick: zompc.native zompc
# 	cd tests && make clean_tests compileperftest FUNCTION_COUNTS="10 1000 2000 4000"
# 	gnuplot makeperfgraph.gnuplot || echo "Could not execute gnuplot"

stdlib.bc stdlib.ll: stdlib.c
	echo Building bytecode standard library $@ ...
	$(LLVM_CC) --emit-llvm -c $< -o stdlib.bc
	$(LLVM_DIS) < stdlib.bc > stdlib.orig.ll
	($(SED) 's/nounwind//' | $(SED) 's/readonly//') < stdlib.orig.ll > stdlib.ll
	$(RM) -f stdlib.bc stdlib.orig.ll
	$(LLVM_AS) < stdlib.ll > stdlib.bc

# generate a file for graphviz which visualizes the dependencies
# between modules
deps.dot deps.png: depends.mk $(CAMLDEP_INPUT)
	echo Generating dependency graph for graphviz ...
	ocamldot depends.mk > deps.dot || echo "ocamldot not found, no graphviz deps graph generated"
	dot -Tpng deps.dot > deps.png || echo "dot not found, deps.png not generated"

# Rules #
################################################################################

.PHONY: clean clean_all

%.ml: %.mly
	echo Generating parser $< ...
	$(MENHIR) --explain --infer $<
	$(OCAMLC) $(CAML_FLAGS) -c $(<:.mly=.mli)

%.ml: %.mll
	echo Generating lexer $< ...
	$(OCAMLLEX) $<

# %.cmi: %.ml
# 	echo Compiling $< triggered by $@ ...
# 	$(OCAMLC) $(CAML_FLAGS)  -c $<

%.cmi: %.mli
	echo "Compiling $@ ..."
	$(OCAMLC) $(CAML_FLAGS) -c $<

%.cmo %.cmi: %.ml
	echo "Compiling (bytecode) $< ..."
	$(OCAMLC) $(CAML_FLAGS)  -c $<

%.cmx: %.ml
	echo "Compiling (native) $< ..."
	$(OCAMLOPT) $(CAML_NATIVE_FLAGS)  -c $<

%.zomp: %.skel gencode
	echo Generating Zomp bindings for $(<:.skel=) ...
	./gencode -lang zomp $(<:.skel=)

CAMLDEP_INPUT= ast2.ml bindings.ml common.ml expander.ml gencode.ml genllvm.ml \
indentlexer.ml indentlexer.mli indentlexer_tests.ml lang.ml machine.ml         \
newparser_tests.ml parseutils.ml compileutils.ml semantic.ml sexprtoplevel.ml  \
testing.ml typesystems.ml zompc.ml zompvm.ml

# Dependencies #
################################################################################

depends.mk: $(CAMLDEP_INPUT)
	echo Calculating dependencies ...
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
	echo Generating tags ...
	otags 2> /dev/null || echo "otags not found, no tags generated"

# Cleaning #
################################################################################

cleanml:
	rm -f *.cmo *.cmi

clean:
	@echo Cleaning...
	cd tests && make clean_tests
	rm -f $(foreach f,$(LANG_CMOS),${f:.cmo=.cm?})
	rm -f $(foreach f,$(LANG_CMOS),${f:.cmo=.o}) zompc.o sexprtoplevel.o
	rm -f expander_tests.cm?
	rm -f zompc.cm? zompc
	rm -f stdlib.bc stdlib.o
	rm -f sexprlexer.cmi sexprlexer.cmo sexprlexer.ml
	rm -f sexprparser.cmi sexprparser.cmo sexprparser.ml sexprparser.mli
	rm -f sexprtoplevel sexprtoplevel.cmi sexprtoplevel.cmo
	rm -f gencode.cmi gencode.cmo gencode
	rm -f machine.c machine.ml machine.cmi machine.cmo machine.o
	rm -f forktest forktest.cmi forktest.cmo
	rm -f dllzompvm.so libzompvm.a zompvm.o
	rm -f testdll.o dlltest.dylib
	rm -f *_flymake.*
	rm -f *.cmx *.native
	rm -f deps.png deps.dot
	rm -f depends.mk
	rm -f stdlib.ll
	rm -f opengl20.zomp glfw.zomp
	rm -f indentlexer.cm? newparser.cm? newparser.o indentlexer.o newparser.ml newparser.mli newparser.conflicts
	rm -f newparser_tests.cmi newparser_tests.cmo newparser_tests newparser_tests.o
	rm -f indentlexer_tests.cmo indentlexer_tests.cmi
	rm -f expandertests.cm? alltests.cm? alltests
	rm -f glQuickText.o libquicktext.dylib glut.dylib
	rm -f perflog.txt
	rm -f mltest
	cd examples/ && make clean
	cd testsuite/ && make clean

clean_tags:
	rm -f *.annot
	rm -f *.conflicts
	rm -f TAGS
	rm -f $(FLYMAKE_LOG)

clean_all: clean clean_tags

include flymake.mk
include depends.mk

