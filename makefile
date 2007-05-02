OCAMLPATH=
OCAMLLEX=$(OCAMLPATH)ocamllex
OCAMLYACC=$(OCAMLPATH)ocamlyacc
MENHIR=$(OCAMLPATH)menhir
OCAMLC=$(OCAMLPATH)ocamlc
OCAMLMKLIB=$(OCAMLPATH)ocamlmklib
UPDATE=cp

all:
	$(OCAMLC) -c ast2.ml
	$(OCAMLC) -c lang.ml
	$(OCAMLC) -c genllvm.ml
	$(OCAMLLEX) lexer2.mll
	$(MENHIR) parser2.mly
	$(OCAMLC) -c parser2.mli
	$(OCAMLC) -c lexer2.ml
	$(OCAMLC) -c parser2.ml
	$(OCAMLC) -c toplevel2.ml
	$(OCAMLC) -o toplevel2 str.cma ast2.cmo lexer2.cmo parser2.cmo lang.cmo genllvm.cmo toplevel2.cmo

clean:
	@rm -f ast2.cm?
	@rm -f lang.cm?
	@rm -f genllvm.cm?
	@rm -f lexer2.ml lexer2.cm?
	@rm -f parser2.ml parser2.mli parser2.cm?
	@rm -f toplevel2.cm?
	@rm -f toplevel2

check-syntax:
	@echo `date "+%Y-%m-%d %H:%M:%S"` \" \" $(CHK_SOURCES) >> build/flymake-log
	@ocamlc -c $(CHK_SOURCES) > build/flymake-output && mv *_flymake.cm? build/

