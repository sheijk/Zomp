

# CAML_INCLUDE=-I `ocamlfind query type-conv` -I `ocamlfind query sexplib`

sexptest.cmo: sexptest.ml 
# 	ocamlc -c -o $@ sexplib.ml
# 	ocamlc -c -I +camlp4 camlp4lib.cma -pp camlp4o -I `ocamlfind query type-conv` -I `ocamlfind query sexplib` pa_type_conv.cmo pa_sexp_conv.cmo sexplib.ml
# 	ocamlc -c -pp camlp4o  -I `ocamlfind query type-conv` -I `ocamlfind query sexplib` sexplib.cmo sexplib.ml
# 	ocamlc -c -I `ocamlfind query type-conv` -I `ocamlfind query sexplib` pa_sexp_conv.cmo sexplib.ml
	ocamlc -c -pp "camlp4o -I `ocamlfind query type-conv` -I `ocamlfind query sexplib` pa_type_conv.cmo pa_sexp_conv.cmo" -I `ocamlfind query sexplib` sexplib.cmo sexptest.ml

sexptest.exe: sexptest.cmo
	ocamlc -I `ocamlfind query type-conv` -I `ocamlfind query sexplib` bigarray.cma sexplib.a sexplib.cma sexptest.cmo -o $@

# ocamlc -I `ocamlfind query type-conv` -I `ocamlfind query sexplib` -pp pa_sexp_conv.cmo -c sexplib.ml
# 	ocamlc -pp /usr/local/lib/ocaml/site-lib/sexplib/pa_sexp_conv.cmo sexplib.ml

# check-syntax:
# 	ocamlc -c +ocamlp4 -pp pa_sexp_conv $(CHK_SOURCES) -o /tmp/flymake_temp.cmo

