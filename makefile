#
# Makefile for zomp project
#

# Config #
################################################################################

OCAMLPATH=
OCAMLLEX=$(OCAMLPATH)ocamllex.opt
OCAMLYACC=$(OCAMLPATH)ocamlyacc
MENHIR=$(OCAMLPATH)menhir
OCAML=$(OCAMLPATH)ocaml
OCAMLRUN=$(OCAMLPATH)ocamlrun
OCAMLC=$(OCAMLPATH)ocamlc.opt -dtypes -warn-error A -g
OCAMLOPT=$(OCAMLPATH)ocamlopt.opt -dtypes -warn-error A
OCAMLMKLIB=$(OCAMLPATH)ocamlmklib
OCAMLDEP=$(OCAMLPATH)ocamldep.opt
UPDATE=cp

LLVM_BIN_DIR=$(PWD)/tools/llvm/Release/bin
LLVM_INCLUDE_DIR=$(PWD)/tools/llvm/include
LLVM_LIB_DIR=$(PWD)/tools/llvm/Release/lib
LLVM_GCC_BIN_DIR=$(PWD)/tools/llvm-gcc/bin

LLVM_CONFIG=$(LLVM_BIN_DIR)/llvm-config
LLVM_CC=$(LLVM_GCC_BIN_DIR)/llvm-gcc
LLVM_CXX=$(LLVM_GCC_BIN_DIR)/llvm-g++
LLVM_DIS=$(LLVM_BIN_DIR)/llvm-dis

PATH:=$(LLVM_BIN_DIR):$(LLVM_GCC_BIN_DIR):$(PATH)

# help:
# 	echo PATH $(PATH)
# 	echo "CXX_FLAGS = '"$(CXX_FLAGS)"'"
# 	echo LLVM_BIN_DIR $(LLVM_BIN_DIR)
# 	echo LLVM_CONFIG $(LLVM_CONFIG)
# 	echo llvm config help `${LLVM_CONFIG} --help`

FLYMAKE_LOG=flymake-log.temp

CAML_INCLUDE=
CAML_PP=

CAML_FLAGS= $(CAML_INCLUDE) $(CAML_PP)
CAML_NATIVE_FLAGS = $(CAML_INCLUDE) $(CAML_PP) -p

CXX_FLAGS=-pg -g -I /usr/local/lib/ocaml/ -I $(LLVM_INCLUDE_DIR) -L$(LLVM_LIB_DIR)

CAML_LIBS = str.cma bigarray.cma
LANG_CMOS = common.cmo testing.cmo typesystems.cmo bindings.cmo ast2.cmo \
lang.cmo semantic.cmo sexprparser.cmo sexprlexer.cmo genllvm.cmo dllzompvm.so \
machine.cmo zompvm.cmo indentlexer.cmo newparser.cmo parseutils.cmo expander.cmo \
testing.cmo compileutils.cmo

# Combined targets
################################################################################

all: byte native stdlib.bc stdlib.ll libbindings tags deps.png alltests
libbindings: gencode opengl20.zomp glfw.zomp opengl20.izomp glfw.izomp
byte: dllzompvm.so zompc sexprtoplevel
native: dllzompvm.so $(LANG_CMOS:.cmo=.cmx) sexprtoplevel.native zompc.native

# LLVM download and compilation
################################################################################

LLVM_VERSION=2.1

tools/llvm-$(LLVM_VERSION).tar.gz:
	@echo Downloading $@ ...
	mkdir tools
	curl http://llvm.org/releases/$(LLVM_VERSION)/llvm-$(LLVM_VERSION).tar.gz -o $@ -s -S

tools/llvm-$(LLVM_VERSION): tools/llvm-$(LLVM_VERSION).tar.gz
	@echo Unpacking $@ ...
	cd tools && gunzip --stdout llvm-$(LLVM_VERSION).tar.gz | tar -xvf -
	touch $@ # tar sets date from archive. avoid downloading the archive twice

tools/llvm: tools/llvm-$(LLVM_VERSION)
	@echo Linking $@ to tools/llvm-$(LLVM_VERSION)
	rm -f $@
	ln -s llvm-$(LLVM_VERSION) $@
	@echo Configuring and building llvm $(LLVM_VERSION)
	cd tools/llvm && ./configure && make

################################################################################

SEXPR_TL_INPUT = common.cmo ast2.cmo sexprparser.cmo sexprlexer.cmo \
bindings.cmo typesystems.cmo lang.cmo semantic.cmo genllvm.cmo common.cmo \
machine.cmo dllzompvm.so zompvm.cmo newparser.cmo indentlexer.cmo \
parseutils.cmo expander.cmo testing.cmo compileutils.cmo testing.cmo \
sexprtoplevel.cmo

dllzompvm.so: zompvm.h zompvm.cpp machine.c stdlib.o
	echo Building $@ ...
	$(LLVM_CXX) $(CXX_FLAGS) `$(LLVM_CONFIG) --cxxflags` -c zompvm.cpp -o zompvm.o
	gcc $(CXX_FLAGS) -I /usr/local/lib/ocaml/ -c machine.c -o machine.o
	ocamlmklib -o zompvm zompvm.o stdlib.o machine.o -lstdc++ -L$(LLVM_LIB_DIR) `$(LLVM_CONFIG) --libs all`

glut.dylib:
	@echo Building $@ ...
	gcc -dynamiclib -framework GLUT -o $@

# opengl.dylib: opengl.c
# 	echo Building $@ ...
# 	gcc -c opengl.c -o opengl.o
# 	gcc -dynamiclib -framework OpenGL opengl.o -o $@

sexprtoplevel: $(SEXPR_TL_INPUT) $(LANG_CMOS:.cmo=.cmx) machine.cmo zompvm.cmo
	echo Building $@ ...
	$(OCAMLC) $(CAML_FLAGS) -o $@ $(CAML_LIBS) $(SEXPR_TL_INPUT)

LLVM_LIBS=`$(LLVM_CONFIG) --libs all`
LLVM_LIBS_CAML=-cclib "$(LLVM_LIBS)"
LANG_CMXS=common.cmx ast2.cmx sexprparser.cmx sexprlexer.cmx bindings.cmx      \
typesystems.cmx lang.cmx semantic.cmx genllvm.cmx machine.cmx -cclib -lstdc++  \
$(LLVM_LIBS_CAML) libzompvm.a zompvm.cmx indentlexer.cmx newparser.cmx         \
parseutils.cmx expander.cmx testing.cmx compileutils.cmx

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

alltests: testing.cmo $(LANG_CMOS) $(NEWPARSER_CMOS) $(TEST_CMOS)
	echo Building $@ ...
	$(OCAMLC) $(CAML_FLAGS) -o $@ bigarray.cma str.cma $(LANG_CMOS) $(NEWPARSER_CMOS) $(TEST_CMOS)

runmltests: alltests
	@echo Running all tests ...
	$(OCAMLRUN) -b ./alltests

run_metaballs: zompc zompc.native stdlib.bc opengl20.zomp glfw.zomp
	cd examples && rm -f metaballs.ll metaballs.bc && time make metaballs.bc

runtests: $(LANG_CMOS) #expander_tests.cmo
	echo Running tests ...
# 	ocamlrun $(CAML_LIBS) $(LANG_CMOS) expander_tests.cmo
# 	date +"Starting at %d.%m.20%y %H:%M:%S"
	cd tests && time make clean_tests check
# 	date +"Finished at %d.%m.20%y %H:%M:%S"

runtestsuite: all
	cd testsuite && time make all

perftest: zompc.native zompc
	cd tests && make clean_tests compileperftest FUNCTION_COUNTS="100 1000 2000 4000 8000 16000"

perftest_quick: zompc.native zompc
	cd tests && make clean_tests compileperftest FUNCTION_COUNTS="1000 2000"

stdlib.bc stdlib.ll: stdlib.c
	echo Building bytecode standard library $@ ...
	$(LLVM_CC) --emit-llvm -c $< -o $@
	$(LLVM_DIS) < stdlib.bc > stdlib.ll

# generate a file for graphviz which visualizes the dependencies
# between modules
deps.dot deps.png: makefile.depends $(CAMLDEP_INPUT)
	echo Generating dependency graph for graphviz ...
	ocamldot makefile.depends > deps.dot || echo "ocamldot not found, no graphviz deps graph generated"
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

%.cmi: %.ml
	echo Compiling $< triggered by .cmi ...
	$(OCAMLC) $(CAML_FLAGS)  -c $<

%.cmo: %.ml
	echo "Compiling (bytecode) $< ..."
	$(OCAMLC) $(CAML_FLAGS)  -c $<

%.cmx: %.ml
	echo "Compiling (native) $< ..."
	$(OCAMLOPT) $(CAML_NATIVE_FLAGS)  -c $<

%.zomp: %.skel gencode
	echo Generating Zomp bindings for $(<:.skel=) ...
	./gencode -lang zomp $(<:.skel=)

%.izomp: %.skel gencode
	echo Generating Zomp bindings using indent syntax for $(<:.skel=) ...
	./gencode -lang izomp $(<:.skel=)

CAMLDEP_INPUT= ast2.ml bindings.ml common.ml expander.ml gencode.ml	\
genllvm.ml indentlexer.ml indentlexer_tests.ml lang.ml machine.ml	\
newparser_tests.ml parseutils.ml compileutils.ml semantic.ml		\
sexprtoplevel.ml testing.ml typesystems.ml zompc.ml zompvm.ml

makefile.depends: $(CAMLDEP_INPUT)
	echo Calculating dependencies ...
	$(OCAMLDEP) $(CAML_PP) $(CAMLDEP_INPUT) > makefile.depends

# Additional/manual dependencies #
################################################################################

glfw.zomp: gencode
opengl20.zomp: gencode
opengl20.izomp: gencode
glfw.izomp: gencode

newparser.ml: newparser.mly ast2.cmo
newparser_tests.cmo: newparser.cmo
newparser_tests.cmx: newparser.cmx
indentlexer.cmo: newparser.ml

sexprparser.ml: ast2.cmo common.cmo
sexprlexer.ml: ast2.ml common.ml sexprparser.cmo
machine.cmo: machine.skel dllzompvm.so
machine.cmx: machine.skel dllzompvm.so
zompvm.cmo: machine.cmo
zompvm.cmx: machine.cmx


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
	rm -f makefile.depends
	rm -f stdlib.ll
	rm -f opengl20.zomp glfw.zomp
	rm -f opengl20.izomp glfw.izomp
	rm -f indentlexer.cm? newparser.cm? newparser.o indentlexer.o newparser.ml newparser.mli newparser.conflicts
	rm -f newparser_tests.cmi newparser_tests.cmo newparser_tests newparser_tests.o
	rm -f expandertests.cm? alltests.cm? alltests
	cd examples/ && make clean
	cd testsuite/ && make clean

clean_tags:
	rm -f *.annot
	rm -f *.conflicts
	rm -f TAGS
	rm -f $(FLYMAKE_LOG)

clean_all: clean clean_tags

include makefile.flymake
include makefile.depends

