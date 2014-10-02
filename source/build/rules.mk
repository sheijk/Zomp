################################################################################
# Rules
################################################################################

.PRECIOUS: %.mli $(OUT_DIR)/has_menhir
%.ml: %.mly $(OUT_DIR)/has_ocaml $(OUT_DIR)/has_menhir
	@$(ECHO) Generating parser $< ...
	$(MENHIR) $(MENHIR_FLAGS) --ocamlc "$(OCAMLC) $(CAMLC_FLAGS)" --explain --infer $<

%.mli: %.ml %.mly
	@$(ECHO) "Generating $@ ..."
	$(OCAMLOPT) $(CAMLOPT_FLAGS) -i $< > $@

%.ml: %.mll $(OUT_DIR)/has_ocaml $(OUT_DIR)/has_menhir
	@$(ECHO) "Generating lexer $@ from $< ..."
	$(OCAMLLEX) $<

source/%_stubs.c source/%.ml: source/%.skel source/gen_c_bindings
	@$(ECHO) "Making OCaml bindings for $(<:.skel=) ..."
	./source/gen_c_bindings $(<:.skel=)

ifeq "$(CAML_BYTE_CODE)" "0"

%.cmi: %.mli
	@$(ECHO) "Compiling $@ ..."
	$(OCAMLOPT) $(CAMLOPT_FLAGS) -c $<

%.cmx: %.ml %.cmi
	@$(ECHO) "Compiling $@ ..."
	$(OCAMLOPT) $(CAMLOPT_FLAGS) -c $<

source/%.mli: source/%.ml source/%.skel
	@$(ECHO) "Generating $@ from $< ..."
	$(OCAMLOPT) $(CAMLOPT_FLAGS) -i $< > $@

%: %.cmx $(CAML_DEPENDENCIES)
	$(ECHO) "Building native ml program $@ ..."
	$(OCAMLOPT) $(CAMLOPT_FLAGS) $(CAML_LINK_FLAGS) -o $@ $<

$(OUT_DIR)/%: %.cmx $(CAML_DEPENDENCIES)
	$(ECHO) "Building native ml program $@ ..."
	mkdir -p `dirname $@`
	$(OCAMLOPT) $(CAMLOPT_FLAGS) $(CAML_LINK_FLAGS) -o $@ $<

else

%.cmi: %.mli
	@$(ECHO) "Compiling $@ ..."
	$(OCAMLC) $(CAMLC_FLAGS) -c $<

%.cmo: %.ml %.cmi
	@$(ECHO) "Compiling $@ ..."
	$(OCAMLC) $(CAMLC_FLAGS) -c $<

source/%.mli: source/%.ml source/%.skel
	@$(ECHO) "Generating $@ from $< ..."
	$(OCAMLC) $(CAMLC_FLAGS) -i $< > $@

%: %.cmo $(CAML_DEPENDENCIES)
	$(ECHO) "Building ml program $@ ..."
	$(OCAMLC) $(CAMLC_FLAGS) $(CAML_LINK_FLAGS) -o $@ $<

$(OUT_DIR)/%: %.cmo
	$(ECHO) "Building ml program $@ ..."
	mkdir -p `dirname $@`
	$(OCAMLC) $(CAMLC_FLAGS) $(CAML_LINK_FLAGS) -o $@ $<

endif

.PRECIOUS: %.o
%.o: %.c
	$(CC) $(CCFLAGS) -c -o $@ $<

%.o: %.cpp
	@$(ECHO) "Compiling $< ..."
	$(CXX) $(CXXFLAGS) -c -o $@ $<

.PRECIOUS: %.ll
%.ll %.compile_output: %.zomp $(ZOMPC_FILE) source/prelude.zomp $(OUT_DIR)/has_llvm
	@$(ECHO) Compiling $(<) to .ll ...
	$(RUN_AND_LOG) $(<:.zomp=.compile_output) $(ZOMPC) -c $< $(ZOMPCFLAGS) --stats $(@:.ll=.compile_stats)

.PRECIOUS: %.bc
%.bc: %.ll $(OUT_DIR)/has_llvm
	@$(ECHO) Compiling $< to $@ ...
	$(LLVM_AS) -f $< -o $@

.PRECIOUS: %.opt-bc
%.opt-bc: %.bc $(OUT_DIR)/has_llvm
	@$(ECHO) Optimizing $< to $@ ...
	$(LLVM_OPT) $< -o $@ -O3

.PRECIOUS: %.s
ifeq "$(OPT)" "1"
%.s: %.opt-bc
else
%.s: %.bc
endif
	@$(ECHO) LLVM code generating $@ ...
	$(LLVM_LLC) -o $@ -march=$(LLVM_ARCH) $<

%.o: %.s
	@$(ECHO) Assembling $@ ...
	$(AS) -o $@ $< -arch $(ARCH)

%.exe: %.o source/runtime.o $(LIBS)
	@$(ECHO) Making $@ ...
	$(CC) $(LDFLAGS) -o $@ -L. -L./examples -L./testsuite $(LIBS) $< -arch $(ARCH)

%.svg: %.dot
	@$(ECHO) "Generating $@ ..."
	-dot -Tsvg $< > $@

ifeq "$(REGEN_MLI)" "1"
# Use this to re-generate an OCaml module interface (mli file) from the ml file.
# for those modules that simply export everything they define.
# rm -f path/foo.mli
# make path/gen-foo.mli REGEN_MLI=1
gen-%.mli: %.ml
	@$(ECHO) "Auto generating interface $(<)i from $< ..."
	if [ -e "$(<)i" ]; then echo "$<i: 0: error: file exists. delete it first!"; exit 1; fi
	$(OCAMLOPT) $(CAMLOPT_FLAGS) -i $< > $(<:.ml=.mli)
endif

