OCAMLPATH=
OCAMLLEX=$(OCAMLPATH)ocamllex
OCAMLYACC=$(OCAMLPATH)ocamlyacc
MENHIR=$(OCAMLPATH)menhir
OCAMLC=$(OCAMLPATH)ocamlc -dtypes
OCAMLMKLIB=$(OCAMLPATH)ocamlmklib
OCAMLDEP=$(OCAMLPATH)ocamldep
UPDATE=cp

CAML_LIBS = str.cma
LANG_CMOS = common.cmo bindings.cmo ast2.cmo lang.cmo parser2.cmo lexer2.cmo expander.cmo genllvm.cmo

all: toplevel2 zompc stdlib.bc tags

toplevel2: $(LANG_CMOS) toplevel2.cmo
	echo Building $@ ...
	$(OCAMLC) -o $@ $(CAML_LIBS) $(LANG_CMOS) toplevel2.cmo

zompc: $(LANG_CMOS) zompc.cmo
	echo Building $@ ...
	$(OCAMLC) -o $@ str.cma $(LANG_CMOS) zompc.cmo

runtests: $(LANG_CMOS) #expander_tests.cmo
	echo Running tests ...
# 	ocamlrun $(CAML_LIBS) $(LANG_CMOS) expander_tests.cmo
	cd tests && make check

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
	$(OCAMLC) -c $<

.ml.cmo:
	echo Compiling $< ...
	$(OCAMLC) -c $<

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

check-syntax:
	@echo `date "+%Y-%m-%d %H:%M:%S"` \" \" $(CHK_SOURCES) >> build/flymake-log
	@ocamlc -c $(CHK_SOURCES) > build/flymake-output && mv *_flymake.cm? build/

include makefile.depends

