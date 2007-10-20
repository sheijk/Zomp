OCAMLPATH=
OCAMLLEX=$(OCAMLPATH)ocamllex
OCAMLYACC=$(OCAMLPATH)ocamlyacc
MENHIR=$(OCAMLPATH)menhir
OCAMLC=$(OCAMLPATH)ocamlc -dtypes -warn-error A
OCAMLMKLIB=$(OCAMLPATH)ocamlmklib
OCAMLDEP=$(OCAMLPATH)ocamldep
UPDATE=cp

FLYMAKE_LOG=flymake-log.temp

# CAML_INCLUDE=-I `ocamlfind query type-conv` -I `ocamlfind query sexplib`
# CAML_PP=-pp "camlp4o $(CAML_INCLUDE) pa_type_conv.cmo pa_sexp_conv.cmo"
CAML_INCLUDE=
CAML_PP=

CAML_FLAGS= $(CAML_INCLUDE) $(CAML_PP)

CAML_LIBS = str.cma
LANG_CMOS = common.cmo typesystems.cmo bindings.cmo ast2.cmo lang.cmo semantic.cmo parser2.cmo lexer2.cmo sexprparser.cmo sexprlexer.cmo genllvm.cmo expander.cmo parseutils.cmo

all: deps toplevel2 zompc stdlib.bc stdlib.ll sexprtoplevel gencode tags

# ocamlbuild:
# 	ocamlbuild gencode.native libzompvm.a zompc.native sexprtoplevel.native toplevel2.native -lib str -classic-display

SEXPR_TL_INPUT = common.cmo ast2.cmo parser2.cmo lexer2.cmo sexprparser.cmo sexprlexer.cmo bindings.cmo typesystems.cmo lang.cmo semantic.cmo genllvm.cmo common.cmo expander.cmo dllzompvm.so machine.cmo parseutils.cmo zompvm.cmo sexprtoplevel.cmo

dllzompvm.so: zompvm.h zompvm.cpp machine.c
	echo Building $@ ...
	g++ `llvm-config --cxxflags` -c zompvm.cpp -o zompvm.o
	gcc -I /usr/local/lib/ocaml/ -c machine.c -o machine.o
	ocamlmklib -o zompvm zompvm.o machine.o -lstdc++ `llvm-config --libs jit interpreter native x86 asmparser`

sexprtoplevel: $(SEXPR_TL_INPUT)
	echo Building $@ ...
	$(OCAMLC) $(CAML_FLAGS) -g -o $@ $(CAML_LIBS) $(SEXPR_TL_INPUT)

toplevel2: $(LANG_CMOS) toplevel2.cmo
	echo Building $@ ...
	$(OCAMLC) $(CAML_FLAGS) -g -o $@ $(CAML_LIBS) $(LANG_CMOS) toplevel2.cmo

zompc: $(LANG_CMOS) zompc.cmo
	echo Building $@ ...
	$(OCAMLC) $(CAML_FLAGS) -g -o $@ $(CAML_LIBS) $(LANG_CMOS) zompc.cmo

gencode: gencode.cmo gencode.ml
	echo Building $@ ...
	$(OCAMLC) $(CAML_FLAGS) -g -o $@ $(CAML_LIBS) gencode.cmo

machine.cmo: machine.skel

machine.c machine.ml: gencode machine.skel
	echo Making OCaml bindings for zomp-machine ...
	./gencode machine

runtests: $(LANG_CMOS) #expander_tests.cmo
	echo Running tests ...
# 	ocamlrun $(CAML_LIBS) $(LANG_CMOS) expander_tests.cmo
	cd tests && make clean_tests check

stdlib.bc stdlib.ll: stdlib.c
	echo Building bytecode standard library $@ ...
	llvm-gcc --emit-llvm -c $< -o $@
	llvm-dis < stdlib.bc > stdlib.ll


.SUFFIXES: .ml .cmo .mly .mll .cmi

.mly.ml:
	echo Generating parser $< ...
	$(MENHIR) $<
	$(OCAMLC) $(CAML_FLAGS) -c $(<:.mly=.mli)

.mll.ml:
	echo Generating lexer $< ...
	$(OCAMLLEX) $<

.ml.cmi:
	echo Compiling $< triggered by .cmi ...
	$(OCAMLC) $(CAML_FLAGS) -g -c $<

.ml.cmo:
	echo Compiling $< ...
	$(OCAMLC) $(CAML_FLAGS) -g -c $<

deps:
	echo Calculating dependencies ...
	$(OCAMLDEP) $(CAML_PP) *.ml *.mly *.mll > makefile.depends

parser2.cmo: ast2.ml
sexprparser.cmo: ast2.ml
sexprlexer.cmo: ast2.ml

# generate tags if otags exists
tags:
	echo Generating tags ...
	otags 2> /dev/null || echo "otags not found, no tags generated"

cleanml:
	rm -f *.cmo *.cmi

clean:
	cd tests && make clean_tests
	rm -f $(foreach f,$(LANG_CMOS),${f:.cmo=.cm?})
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
	rm -f *_flymake.*

clean_tags:
	rm -f *.annot
	rm -f TAGS

clean_all: clean clean_tags

ml_check:
	@echo Checking OCaml files $(CHK_SOURCES)
	@$(OCAMLC) $(CAML_FLAGS) -c $(CHK_SOURCES) -o /tmp/flymake_temp.cmo > $(FLYMAKE_LOG)

cpp_check:
	@echo Checking C++ files $(CHK_SOURCES)
	@g++ -c $(CHK_SOURCES) `llvm-config --cxxflags` -fsyntax-only > $(FLYMAKE_LOG)

check-source: $(patsubst %.ml,ml_check, $(patsubst %.cpp,cpp_check,$(CHK_SOURCES)))
	@cat $(FLYMAKE_LOG)

check-syntax: check-source all
	@echo `date "+%Y-%m-%d %H:%M:%S"` \" \" $(CHK_SOURCES) >> $(FLYMAKE_LOG)
	@rm -f *_flymake.cpp

# check-syntax:
# 	@echo `date "+%Y-%m-%d %H:%M:%S"` \" \" $(CHK_SOURCES) >> build/flymake-log
# 	@ocamlc -c $(CHK_SOURCES) > build/flymake-output && mv *_flymake.cm? build/

include makefile.depends

