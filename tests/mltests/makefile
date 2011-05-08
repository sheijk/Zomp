

CAML_INCLUDE=-I `ocamlfind query type-conv` -I `ocamlfind query sexplib`

sexptest.cmo: sexptest.ml 
	ocamlc -c -pp "camlp4o $(CAML_INCLUDE) pa_type_conv.cmo pa_sexp_conv.cmo" $(CAML_INCLUDE) sexplib.cmo sexptest.ml

sexptest.exe: sexptest.cmo
	ocamlc $(CAML_INCLUDE) bigarray.cma sexplib.a sexplib.cma sexptest.cmo -o $@

%.exe: %.ml
	ocamlopt bigarray.cmxa str.cmxa -o $@ $<

clean:
	rm -f sexptest.exe sexptest.cmo sexptest.cmi

FLYMAKE_LOG=flymake.log
include ../flymake.mk

