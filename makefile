OCAMLPATH=
OCAMLLEX=$(OCAMLPATH)ocamllex
OCAMLYACC=$(OCAMLPATH)ocamlyacc
MENHIR=$(OCAMLPATH)menhir
OCAMLC=$(OCAMLPATH)ocamlc
OCAMLMKLIB=$(OCAMLPATH)ocamlmklib
OCAMLDEP=$(OCAMLPATH)ocamldep
UPDATE=cp

all: ast2.cmo lang.cmo parser2.cmo lexer2.cmo expander.cmo genllvm.cmo toplevel2.cmo
	$(OCAMLC) -o toplevel2 str.cma ast2.cmo lexer2.cmo parser2.cmo lang.cmo genllvm.cmo expander.cmo toplevel2.cmo

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
	@rm -f ast2.cm?
	@rm -f lang.cm?
	@rm -f expander.cm?
	@rm -f genllvm.cm?
	@rm -f lexer2.ml lexer2.cm?
	@rm -f parser2.ml parser2.mli parser2.cm?
	@rm -f toplevel2.cm?
	@rm -f toplevel2

check-syntax:
	@echo `date "+%Y-%m-%d %H:%M:%S"` \" \" $(CHK_SOURCES) >> build/flymake-log
	@ocamlc -c $(CHK_SOURCES) > build/flymake-output && mv *_flymake.cm? build/

include makefile.depends

