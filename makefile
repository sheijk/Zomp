OCAMLPATH=
OCAMLLEX=$(OCAMLPATH)ocamllex.opt
OCAMLYACC=$(OCAMLPATH)ocamlyacc
MENHIR=$(OCAMLPATH)menhir
OCAML=$(OCAMLPATH)ocaml
OCAMLC=$(OCAMLPATH)ocamlc.opt -dtypes -warn-error A -g
OCAMLOPT=$(OCAMLPATH)ocamlopt.opt -dtypes -warn-error A
OCAMLMKLIB=$(OCAMLPATH)ocamlmklib
OCAMLDEP=$(OCAMLPATH)ocamldep.opt
UPDATE=cp

FLYMAKE_LOG=flymake-log.temp

# CAML_INCLUDE=-I `ocamlfind query type-conv` -I `ocamlfind query sexplib`
# CAML_PP=-pp "camlp4o $(CAML_INCLUDE) pa_type_conv.cmo pa_sexp_conv.cmo"
CAML_INCLUDE=
CAML_PP=

CAML_FLAGS= $(CAML_INCLUDE) $(CAML_PP)
CAML_NATIVE_FLAGS = $(CAML_INCLUDE) $(CAML_PP) -p

CXX_FLAGS=-pg -g
# CXX_FLAGS=

CAML_LIBS = str.cma bigarray.cma
LANG_CMOS = common.cmo testing.cmo typesystems.cmo bindings.cmo ast2.cmo lang.cmo semantic.cmo parser2.cmo lexer2.cmo sexprparser.cmo sexprlexer.cmo genllvm.cmo dllzompvm.so machine.cmo zompvm.cmo expander.cmo parseutils.cmo

# Combined targets
all: byte native stdlib.bc stdlib.ll libbindings tags deps.png
libbindings: gencode opengl20.zomp glfw.zomp opengl20.izomp glfw.izomp
byte: dllzompvm.so toplevel2 zompc sexprtoplevel
native: dllzompvm.so $(LANG_CMOS:.cmo=.cmx) sexprtoplevel.native zompc.native

SEXPR_TL_INPUT = common.cmo ast2.cmo parser2.cmo lexer2.cmo sexprparser.cmo sexprlexer.cmo bindings.cmo typesystems.cmo lang.cmo semantic.cmo genllvm.cmo common.cmo machine.cmo dllzompvm.so zompvm.cmo expander.cmo parseutils.cmo testing.cmo newparser.cmo iexprtest.cmo newparsertest.cmo sexprtoplevel.cmo

dllzompvm.so: zompvm.h zompvm.cpp machine.c
	echo Building $@ ...
	g++ $(CXX_FLAGS) `llvm-config --cxxflags` -c zompvm.cpp -o zompvm.o
	gcc $(CXX_FLAGS) -I /usr/local/lib/ocaml/ -c machine.c -o machine.o
# 	ocamlmklib -o zompvm zompvm.o machine.o -lstdc++ `llvm-config --libs jit interpreter native x86 asmparser analysis transformutils`
	ocamlmklib -o zompvm zompvm.o machine.o -lstdc++ `llvm-config --libs all`

glut.dylib:
	@echo Building $@ ...
	gcc -dynamiclib -framework GLUT -o $@

# opengl.dylib: opengl.c
# 	echo Building $@ ...
# 	gcc -c opengl.c -o opengl.o
# 	gcc -dynamiclib -framework OpenGL opengl.o -o $@

sexprtoplevel: $(SEXPR_TL_INPUT)
	echo Building $@ ...
	$(OCAMLC) $(CAML_FLAGS)  -o $@ $(CAML_LIBS) $(SEXPR_TL_INPUT)

# LLVM_LIBS=`llvm-config --libs jit interpreter native x86 asmparser`
LLVM_LIBS=`llvm-config --libs all`
LLVM_LIBS_CAML=-cclib "$(LLVM_LIBS)"
LANG_CMXS=common.cmx ast2.cmx parser2.cmx lexer2.cmx sexprparser.cmx sexprlexer.cmx bindings.cmx typesystems.cmx lang.cmx semantic.cmx genllvm.cmx machine.cmx -cclib -lstdc++ $(LLVM_LIBS_CAML) libzompvm.a zompvm.cmx expander.cmx parseutils.cmx 

TL_CMXS=testing.cmx newparser.cmx iexprtest.cmx newparsertest.cmx

sexprtoplevel.native: $(SEXPR_TL_INPUT:.cmo=.cmx) $(TL_CMXS)
	echo Building $@ ...
	$(OCAMLOPT) $(CAML_NATIVE_FLAGS)  -o $@ str.cmxa bigarray.cmxa $(LANG_CMXS) $(TL_CMXS) sexprtoplevel.cmx

zompc.native: $(LANG_CMOS:.cmo=.cmx) zompc.cmx
	echo Building $@ ...
	$(OCAMLOPT) $(CAML_NATIVE_FLAGS)  -o $@ $(CAML_LIBS:.cma=.cmxa) $(LANG_CMXS) zompc.cmx

toplevel2: $(LANG_CMOS) toplevel2.cmo
	echo Building $@ ...
	$(OCAMLC) $(CAML_FLAGS)  -o $@ $(CAML_LIBS) $(LANG_CMOS) toplevel2.cmo

zompc: $(LANG_CMOS) zompc.cmo
	echo Building $@ ...
	$(OCAMLC) $(CAML_FLAGS)  -o $@ $(CAML_LIBS) $(LANG_CMOS) dllzompvm.so machine.cmo zompvm.cmo zompc.cmo

gencode: gencode.cmo gencode.ml
	echo Building $@ ...
	$(OCAMLC) $(CAML_FLAGS)  -o $@ $(CAML_LIBS) gencode.cmo

machine.c machine.ml: gencode machine.skel
	echo Making OCaml bindings for zomp-machine ...
	./gencode machine

NEWPARSER_CMOS = common.cmo testing.cmo ast2.cmo newparser.cmo\
 iexprtest.cmo newparsertest.cmo

newparsertest: $(NEWPARSER_CMOS)
	echo Building $@ ...
	$(OCAMLC) $(CAML_FLAGS) -o $@ bigarray.cma str.cma $(NEWPARSER_CMOS)

testnewparser: $(NEWPARSER_CMOS) newparsertest
	echo Running newparser tests ...
	ocamlrun -b ./newparsertest
# 	$(OCAML) str.cma bigarray.cma common.cmo testing.cmo iexprtest.ml

runtests: $(LANG_CMOS) #expander_tests.cmo
	echo Running tests ...
# 	ocamlrun $(CAML_LIBS) $(LANG_CMOS) expander_tests.cmo
# 	date +"Starting at %d.%m.20%y %H:%M:%S"
	cd tests && time make clean_tests check
# 	date +"Finished at %d.%m.20%y %H:%M:%S"

perftest: zompc.native zompc
	cd tests && make clean_tests compileperftest FUNCTION_COUNTS="100 1000 2000 4000 8000 16000"

stdlib.bc stdlib.ll: stdlib.c
	echo Building bytecode standard library $@ ...
	llvm-gcc --emit-llvm -c $< -o $@
	llvm-dis < stdlib.bc > stdlib.ll

# glfw.zomp: glfw.skel gencode
# 	echo Generating Zomp bindings for GLFW ...
# 	./gencode -lang zomp glfw

# opengl20.zomp: opengl20.skel gencode
# 	echo Generating Zomp bindings for OpenGL 2.0 ...
# 	./gencode -lang zomp opengl20

# generate a file for graphviz which visualizes the dependencies
# between modules
deps.dot deps.png: makefile.depends $(CAMLDEP_INPUT)
	echo Generating dependency graph for graphviz ...
	ocamldot makefile.depends > deps.dot || echo "ocamldot not found, no graphviz deps graph generated"
	dot -Tpng deps.dot > deps.png || echo "dot not found, deps.png not generated"

.SUFFIXES: .ml .cmo .mly .mll .cmi .cmx .skel .zomp .izomp

.PHONY: clean clean_all

.mly.ml:
	echo Generating parser $< ...
	$(MENHIR) --explain --infer $<
	$(OCAMLC) $(CAML_FLAGS) -c $(<:.mly=.mli)

.mll.ml:
	echo Generating lexer $< ...
	$(OCAMLLEX) $<

.ml.cmi:
	echo Compiling $< triggered by .cmi ...
	$(OCAMLC) $(CAML_FLAGS)  -c $<

.ml.cmo:
	echo "Compiling (bytecode) $< ..."
	$(OCAMLC) $(CAML_FLAGS)  -c $<

.ml.cmx:
	echo "Compiling (native) $< ..."
	$(OCAMLOPT) $(CAML_NATIVE_FLAGS)  -c $<

.skel.zomp: gencode
	echo Generating Zomp bindings for $(<:.skel=) ...
	./gencode -lang zomp $(<:.skel=)

.skel.izomp: gencode
	echo Generating Zomp bindings using indent syntax for $(<:.skel=) ...
	./gencode -lang izomp $(<:.skel=)

glfw.zomp: gencode
opengl20.zomp: gencode
opengl20.izomp: gencode
glfw.izomp: gencode

CAMLDEP_INPUT=ast2.ml bindings.ml common.ml expander.ml gencode.ml\
genllvm.ml lang.ml parseutils.ml semantic.ml sexprparser.mly sexprlexer.mll\
sexprtoplevel.ml toplevel2.ml typesystems.ml zompc.ml zompvm.ml\
iexpr.ml iexprtest.ml newparsertest.ml testing.ml

# Additional dependencies
newparser.ml: newparser.mly ast2.cmo

makefile.depends: $(CAMLDEP_INPUT)
	echo Calculating dependencies ...
	$(OCAMLDEP) $(CAML_PP) $(CAMLDEP_INPUT) > makefile.depends

parser2.cmo: ast2.ml
lexer2.cmo: common.ml
sexprparser.cmo: ast2.ml common.ml
sexprlexer.cmo: ast2.ml common.ml
machine.cmo: machine.skel
zompvm.cmo: machine.cmo
zompc: dllzompvm.so

# generate tags if otags exists
tags:
	echo Generating tags ...
	otags 2> /dev/null || echo "otags not found, no tags generated"

cleanml:
	rm -f *.cmo *.cmi

clean:
	cd tests && make clean_tests
	rm -f $(foreach f,$(LANG_CMOS),${f:.cmo=.cm?})
	rm -f $(foreach f,$(LANG_CMOS),${f:.cmo=.o}) zompc.o sexprtoplevel.o
	rm -f lexer2.ml
	rm -f parser2.ml parser2.mli
	rm -f expander_tests.cm?
	rm -f toplevel2.cm? toplevel2
	rm -f zompc.cm? zompc
	rm -f stdlib.bc
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
	rm -f iexpr.cm? iexprtest.cm? newparser.cm?
	rm -f newparsertest.cmi newparsertest.cmo

clean_tags:
	rm -f *.annot
	rm -f TAGS
	rm -f $(FLYMAKE_LOG)

clean_all: clean clean_tags

ml_check:
	@echo Checking OCaml files $(CHK_SOURCES)
	@$(OCAMLC) $(CAML_FLAGS) -c $(CHK_SOURCES) -o /tmp/flymake_temp.cmo > $(FLYMAKE_LOG)
	@perl -pe "s/_flymake//g" < $(CHK_SOURCES:.ml=.annot) > $(CHK_SOURCES:_flymake.ml=.annot)

cpp_check:
	@echo Checking C++ files $(CHK_SOURCES)
	@g++ -c $(CHK_SOURCES) -Wall `llvm-config --cxxflags` -fsyntax-only > $(FLYMAKE_LOG)

check-source: $(patsubst %.ml,ml_check, $(patsubst %.cpp,cpp_check,$(CHK_SOURCES)))
	@cat $(FLYMAKE_LOG)

check-syntax: check-source
	@echo `date "+%Y-%m-%d %H:%M:%S"` \" \" $(CHK_SOURCES) >> $(FLYMAKE_LOG)
	@rm -f *_flymake.cpp

# check-syntax:
# 	@echo `date "+%Y-%m-%d %H:%M:%S"` \" \" $(CHK_SOURCES) >> build/flymake-log
# 	@ocamlc -c $(CHK_SOURCES) > build/flymake-output && mv *_flymake.cm? build/

include makefile.depends

