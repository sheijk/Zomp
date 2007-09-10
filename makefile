OCAMLPATH=
OCAMLLEX=$(OCAMLPATH)ocamllex
OCAMLYACC=$(OCAMLPATH)ocamlyacc
MENHIR=$(OCAMLPATH)menhir
OCAMLC=$(OCAMLPATH)ocamlc -dtypes
OCAMLMKLIB=$(OCAMLPATH)ocamlmklib
OCAMLDEP=$(OCAMLPATH)ocamldep
UPDATE=cp

CAML_LIBS = str.cma
LANG_CMOS = common.cmo typesystems.cmo bindings.cmo ast2.cmo lang.cmo parser2.cmo lexer2.cmo sexprparser.cmo sexprlexer.cmo expander.cmo genllvm.cmo

all_notags: deps toplevel2 zompc stdlib.bc sexprtoplevel
all: all_notags tags

SEXPR_TL_INPUT = common.cmo ast2.cmo parser2.cmo lexer2.cmo sexprparser.cmo sexprlexer.cmo bindings.cmo typesystems.cmo lang.cmo genllvm.cmo common.cmo expander.cmo sexprtoplevel.cmo

sexprtoplevel: $(SEXPR_TL_INPUT)
	echo Building $@ ...
	$(OCAMLC) -g -o $@ str.cma $(SEXPR_TL_INPUT)

toplevel2: $(LANG_CMOS) toplevel2.cmo
	echo Building $@ ...
	$(OCAMLC) -g -o $@ $(CAML_LIBS) $(LANG_CMOS) toplevel2.cmo

zompc: $(LANG_CMOS) zompc.cmo
	echo Building $@ ...
	$(OCAMLC) -g -o $@ str.cma $(LANG_CMOS) zompc.cmo

runtests: $(LANG_CMOS) #expander_tests.cmo
	echo Running tests ...
# 	ocamlrun $(CAML_LIBS) $(LANG_CMOS) expander_tests.cmo
	cd tests && make clean_tests check

stdlib.bc: stdlib.c
	echo Building bytecode standard library $@ ...
	llvm-gcc --emit-llvm -c $< -o $@

.SUFFIXES: .ml .cmo .mly .mll .cmi

.mly.ml:
	echo Generating parser $< ...
	$(MENHIR) $<
	$(OCAMLC) -c $(<:.mly=.mli)

.mll.ml:
	echo Generating lexer $< ...
	$(OCAMLLEX) $<

.ml.cmi:
	echo Compiling $< triggered by .cmi ...
	$(OCAMLC) -g -c $<

.ml.cmo:
	echo Compiling $< ...
	$(OCAMLC) -g -c $<

deps:
	echo Calculating dependencies ...
	$(OCAMLDEP) *.ml *.mly *.mll > makefile.depends

tags:
	echo Generating tags ...
	otags *.ml

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

clean_tags:
	rm -f *.annot
	rm -f TAGS

clean_all: clean clean_tags

check-syntax:
	@echo `date "+%Y-%m-%d %H:%M:%S"` \" \" $(CHK_SOURCES) >> build/flymake-log
	@ocamlc -c $(CHK_SOURCES) > build/flymake-output && mv *_flymake.cm? build/

include makefile.depends

