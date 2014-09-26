
# Build rule to compute dependencies between OCaml modules
$(AUTO_DEPENDENCY_FILE): $(BUILD_DIR)/.exists $(CAMLDEP_INPUT) makefile
	@$(ECHO) "Calculating dependencies ..."
	$(OCAMLDEP) -I source $(CAML_PP) $(CAMLDEP_INPUT) > $(AUTO_DEPENDENCY_FILE)

include $(AUTO_DEPENDENCY_FILE)

################################################################################
# Visualize OCaml module dependencies
################################################################################

DOC_TARGETS += $(OUT_DIR)/caml-modules.svg

$(OUT_DIR)/caml-modules.dot: makefile $(AUTO_DEPENDENCY_FILE) $(CAMLDEP_INPUT:.mli=.cmi) source/newparser.mli
	@$(ECHO) Generating module graph for graphviz ...
	$(OCAMLDOC) $(OCAMLDEP_FLAGS) -o $@ -dot $(CAMLDEP_INPUT) source/newparser.ml source/newparser.mli
	cat $@ | sed 's/rotate=90;/rotate=0;/' > $@.tmp
	mv $@.tmp $@

DOC_TARGETS += $(OUT_DIR)/caml-types.svg

$(OUT_DIR)/caml-types.dot: makefile $(AUTO_DEPENDENCY_FILE) $(CAMLDEP_INPUT:.mli=.cmi) source/newparser.mli
	@$(ECHO) Generating type graph for graphviz ...
	$(OCAMLDOC) $(OCAMLDOC_FLAGS) -o $@ -dot -dot-types -dot-include-all -dot-reduce $(CAMLDEP_INPUT) source/newparser.ml source/newparser.mli
	cat $@ | sed 's/rotate=90;/rotate=0;/' | sed 's/rankdir = TB ;/rankdir=LR;/' > $@.tmp
	mv $@.tmp $@

################################################################################
# Dependencies
#
# Try to detect dependencies automatically. Does not work for everything, yet,
# add additional ones here. Use something like make -j 20 for a quick test
################################################################################

source/newparser_tests.cmi: source/newparser.cmi
source/newparser_tests.cmo: source/newparser.cmo
source/newparser_tests.cmx: source/newparser.cmx
source/indentlexer.cmi: source/newparser.$(CAML_OBJ_EXT)
source/indentlexer.cmo: source/newparser.cmo
source/indentlexer.cmx: source/newparser.cmx

source/newparser.ml: source/ast2.$(CAML_OBJ_EXT)
source/newparser.mli: source/ast2.$(CAML_OBJ_EXT)
source/newparser.cmi: source/ast2.cmi
source/newparser.cmo: source/ast2.cmo
source/newparser.cmx: source/ast2.cmx

source/machine.cmo: $(ZOMP_DLL_FILE)
source/machine.cmx: $(ZOMP_DLL_FILE)
source/stats.cmo: $(ZOMP_DLL_FILE)
source/stats.cmx: $(ZOMP_DLL_FILE)
source/zompvm.cmi: source/machine.cmi
source/zompvm.cmo: source/machine.cmo
source/zompvm.cmx: source/machine.cmx
source/semantic.cmi: source/machine.cmi
source/semantic.cmo: source/machine.cmo
source/semantic.cmx: source/machine.cmx

source/ast2.cmo: source/basics.cmo source/common.cmo
source/ast2.cmx: source/basics.cmx source/common.cmx

source/mltest.cmo: source/newparser_tests.cmo source/indentlexer_tests.cmo
source/mltest.cmx: source/newparser_tests.cmx source/indentlexer_tests.cmx

# TODO: auto generate C/C++ dependencies
source/runtime.o: source/zomputils.h

source/zompvm_impl.o: source/zomputils.h
source/zompvm_impl.o: source/zompvm_impl.h
source/zompvm_impl.o: source/zompvm_caml.h
source/zompvm_impl.o: source/stats_impl.h
source/zompvm_impl.o: source/zompvm_caml.h
source/zompvm_caml.o: source/zomputils.h
source/stats_impl.o: source/zomputils.h
source/stats_impl.o: source/stats_impl.h
source/vm_http_server.o: source/zomputils.h
source/vm_http_server.o: source/zompvm_impl.h
source/vm_http_server.o: source/mongoose.h
source/vm_http_server.o: source/

source/vm_protocol.o: source/vm_protocol.h
source/vm_client.o: source/vm_protocol.h
source/vm_server.o: source/vm_protocol.h

libs/quicktext.ll: libs/libquicktext.dylib

