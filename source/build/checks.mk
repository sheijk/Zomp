################################################################################
# Check LLVM installation
################################################################################

FILES_TO_DELETE_ON_CLEAN += $(OUT_DIR)/has_llvm
$(OUT_DIR)/has_llvm:
	@$(ECHO) "Checking LLVM ..."
	($(WHICH) -s $(LLVM_AS)) || (echo $(LLVM_INSTALL_HELP); exit 1)
	$(TOUCH) $@

FILES_TO_DELETE_ON_CLEAN += $(OUT_DIR)/has_clang
ALL_TARGETS += $(OUT_DIR)/has_llvm $(OUT_DIR)/has_clang
$(OUT_DIR)/has_clang:
	@$(ECHO) "Checking clang ..."
	($(WHICH) -s $(CLANG)) || (echo $(LLVM_INSTALL_HELP); exit 1)
	$(TOUCH) $@

LLVM_LIBS=`$(LLVM_CONFIG) --libs all`
LLVM_LIBS_CAML=-cclib "$(LLVM_LIBS)"

.PHONY: check_llvm
check_llvm: $(OUT_DIR)/has_llvm $(OUT_DIR)/has_clang

################################################################################
# Check OCaml and menhir installation
################################################################################

OCAML_INSTALL_HELP = "error: OCaml for $(ARCH) not found in $(PATH), please install"

FILES_TO_DELETE_ON_CLEAN += $(OUT_DIR)/has_ocaml
FILES_TO_DELETE_ON_CLEAN += $(OUT_DIR)/has_ocaml.tmp
$(OUT_DIR)/has_ocaml:
	@$(ECHO) "Checking OCaml ..."
	($(FILE) `$(WHICH) ocamlopt.opt` | grep $(ARCH) > $(OUT_DIR)/has_ocaml.tmp) || ($(ECHO) $(OCAML_INSTALL_HELP); exit 1)
	touch $@

MENHIR_INSTALL_HELP = "error: Menhir for $(ARCH) not found in $(PATH), please install"

FILES_TO_DELETE_ON_CLEAN += $(OUT_DIR)/has_menhir
FILES_TO_DELETE_ON_CLEAN += $(OUT_DIR)/has_menhir.tmp
$(OUT_DIR)/has_menhir:
	@$(ECHO) "Checking Menhir ..."
	($(FILE) `$(WHICH) menhir` | grep $(ARCH)) > $(OUT_DIR)/has_menhir.tmp || ($(ECHO) $(MENHIR_INSTALL_HELP); exit 1)
	touch $@

.PHONY: check_ocaml
check_ocaml: $(OUT_DIR)/has_ocaml $(OUT_DIR)/has_menhir

