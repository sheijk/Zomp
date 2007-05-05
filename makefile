OCAMLPATH=
OCAMLLEX=$(OCAMLPATH)ocamllex
OCAMLYACC=$(OCAMLPATH)ocamlyacc
MENHIR=$(OCAMLPATH)menhir
OCAMLC=$(OCAMLPATH)ocamlc
OCAMLMKLIB=$(OCAMLPATH)ocamlmklib
OCAMLDEP=$(OCAMLPATH)ocamldep
UPDATE=cp

CAML_LIBS = str.cma
LANG_CMOS = common.cmo bindings.cmo ast2.cmo lang.cmo parser2.cmo lexer2.cmo expander.cmo genllvm.cmo

all: toplevel2 zompc

toplevel2: $(LANG_CMOs) toplevel2.cmo
	$(OCAMLC) -o $@ $(CAML_LIBS) $(LANG_CMOS) toplevel2.cmo

zompc: $(LANG_CMOS) zompc.cmo
	$(OCAMLC) -o $@ str.cma $(LANG_CMOS) zompc.cmo

runtests:
	./runtests.sh

.SUFFIXES: .ml .cmo .mly .mll

.ml.cmo:
	$(OCAMLC) -c $<

.mly.cmo:
	$(MENHIR) $<
	$(OCAMLC) -c $(<:.mly=.mli)
	$(OCAMLC) -c $(<:.mly=.ml)

.mll.cmo:
	$(OCAMLLEX) $<
	$(OCAMLC) -c $(<:.mll=.ml)

deps:
	$(OCAMLDEP) *.ml *.mly *.mll > makefile.depends

clean:
	rm -f $(foreach f,$(LANG_CMOS),${f:.cmo=.cm?})
	rm -f lexer2.ml
	rm -f parser2.ml parser2.mli
	rm -f toplevel2.cm? toplevel2
	rm -f zompc.cm? zompc

check-syntax:
	@echo `date "+%Y-%m-%d %H:%M:%S"` \" \" $(CHK_SOURCES) >> build/flymake-log
	@ocamlc -c $(CHK_SOURCES) > build/flymake-output && mv *_flymake.cm? build/

include makefile.depends

