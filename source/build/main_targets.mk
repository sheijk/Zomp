#
# Main targets: libraries, executables, etc.
#

################################################################################
# Zomp tools
################################################################################

ALL_TARGETS += source/zompvm_dummy.o
FILES_TO_DELETE_ON_CLEAN += source/zompvm_dummy.o

ZOMP_DLL_OBJS = source/zompvm_impl.o source/zompvm_caml.o source/stats_impl.o source/runtime.o source/machine_stubs.o source/stats_stubs.o
FILES_TO_DELETE_ON_CLEAN += ${ZOMP_DLL_OBJS}

FILES_TO_DELETE_ON_CLEAN += $(ZOMP_DLL_FILE) source/dllzompvm.so source/libzompvm.a
$(ZOMP_DLL_FILE): $(ZOMP_DLL_OBJS) source/runtime.ll $(OUT_DIR)/has_clang
	@$(ECHO) Building $@ ...
ifeq "$(BUILD_PLATFORM)" "Linux"
	$(CXX) $(DLL_FLAG) $(LDFLAGS) -o source/zompvm -DPIC -fPIC $(ZOMP_DLL_OBJS) -L$(LLVM_LIB_DIR) $(LLVM_LIBS)
else # OS X
	ocamlmklib -o source/zompvm -lcurl $(ZOMP_DLL_OBJS) -lstdc++-static -L$(LLVM_LIB_DIR) $(LLVM_LIBS)
	cp source/dllzompvm.so $(ZOMP_DLL_FILE)
endif

$(ZOMPC_FILE): CAML_LIBS = str bigarray
$(ZOMPSH_FILE): CAML_LIBS = str bigarray
FILES_TO_DELETE_ON_CLEAN += $(call BUILD_PRODUCTS_ML, source/zompc) $(ZOMPC_FILE)
FILES_TO_DELETE_ON_CLEAN += $(call BUILD_PRODUCTS_ML, source/zompsh) $(ZOMPSH_FILE)
ifeq "$(CAML_BYTE_CODE)" "0"

$(ZOMPC_FILE): $(LANG_CMOS:.cmo=.cmx) source/zompc.cmx $(ZOMP_DLL_FILE)
	@$(ECHO) Building $@ ...
	$(OCAMLOPT) -linkpkg $(CAMLOPT_FLAGS) $(CAML_LINK_FLAGS) -o $@ -I $(LLVM_LIB_DIR) $(LANG_CMXS) source/zompc.cmx -cclib -lcurl

$(ZOMPSH_FILE): source/zompsh.cmx $(LANG_CMOS:.cmo=.cmx) $(ZOMP_DLL_FILE)
	@$(ECHO) Building $@ ...
	$(OCAMLOPT) -linkpkg -o $@ $(CAMLOPT_FLAGS) $(CAML_LINK_FLAGS) -I $(LLVM_LIB_DIR) $(LANG_CMXS) source/zompsh.cmx -cclib -lcurl

else

$(ZOMPC_FILE): $(LANG_CMOS) source/zompc.cmo $(ZOMP_DLL_FILE)
	@$(ECHO) Building $@ ...
	$(OCAMLC) -linkpkg $(CAMLC_FLAGS) $(CAML_LINK_FLAGS) -o $@ $(LANG_CMOS) $(ZOMP_DLL_FILE)

$(ZOMPSH_FILE): source/zompsh.cmo $(LANG_CMOS:.cmo=.cmx)
	@$(ECHO) Building $@ ...
	$(OCAMLC) -linkpkg $(CAMLC_FLAGS) $(CAML_LINK_FLAGS) -o $@ $(LANG_CMOS) source/zompsh.cmo

endif

FILES_TO_DELETE_ON_CLEAN += $(call BUILD_PRODUCTS_ML, source/gen_c_bindings)
source/gen_c_bindings: CAML_LIBS += str

ALL_TARGETS += source/runtime.bc source/runtime.ll
FILES_TO_DELETE_ON_CLEAN += source/runtime.bc source/runtime.ll source/runtime.o
source/runtim%.bc source/runtim%.ll: source/runtim%.c $(OUT_DIR)/has_llvm $(OUT_DIR)/has_clang
	@$(ECHO) Building bytecode standard library $@ ...
	$(CLANG) $(CCFLAGS) -std=c89 -emit-llvm -c $< -o source/runtime.bc
	$(LLVM_DIS) < source/runtime.bc > source/runtime.orig.ll
	($(SED) 's/nounwind//' | $(SED) 's/readonly//' | $(SED) 's/ssp//' | $(SED) 's/%struct.__sFILE = type .*/%struct.__sFILE = type {}/') < source/runtime.orig.ll > source/runtime.ll
	$(RM) -f source/runtime.bc source/runtime.orig.ll
	$(LLVM_AS) < source/runtime.ll > source/runtime.bc

NEWPARSER_ML_SRC = $(foreach file, common basics ast2 newparser indentlexer, source/$(file).ml)

################################################################################
# ZompVM server
################################################################################

VM_HTTP_SERVER_OBJS = source/mongoose.o source/runtime.o source/zompvm_impl.o \
  source/zompvm_caml_dummy.o source/stats_impl.o source/vm_http_server.o
FILES_TO_DELETE_ON_CLEAN += ${VM_HTTP_SERVER_OBJS}

source/vm_http_server.o: CXXFLAGS += `$(LLVM_CONFIG) --cxxflags`
source/zompvm_impl.o: CXXFLAGS += `$(LLVM_CONFIG) --cxxflags`

ALL_TARGETS += $(DEPLOY_DIR)/vm_http_server
$(DEPLOY_DIR)/vm_http_server: $(VM_HTTP_SERVER_OBJS) source/mongoose.h
	@$(ECHO) Building $@ ...
	$(CXX) $(LDFLAGS) -o $@ -lstdc++-static -lcurl $(LLVM_LIBS) $(VM_HTTP_SERVER_OBJS)

vm_server: source/vm_server.o source/vm_protocol.o
	@$(ECHO) Building $@ ...
	$(CXX) $(LDFLAGS) -o $@ $< source/vm_protocol.o

vm_client: source/vm_client.o source/vm_protocol.o
	@$(ECHO) Building $@ ...
	$(CXX) $(LDFLAGS) -o $@ $< source/vm_protocol.o

run_remote_zompsh_test: $(DEPLOY_DIR)/vm_http_server $(ZOMPSH_FILE)
	$(ZOMPSH) < tests/vmserver.zomp

################################################################################
# Misc targets
################################################################################

.PHONY: libbindings
ALL_TARGETS += libbindings
libbindings: source/gen_c_bindings $(GENERATED_LIBRARY_SOURCES) \
  libs/libglut.dylib libs/libquicktext.dylib libs/libutils.dylib libs/stb_image.dylib

ifeq "$(CAML_BYTE_CODE)" "1"

.PHONY: byte
ALL_TARGETS += byte
byte: $(ZOMP_DLL_FILE) $(ZOMPC_FILE) $(ZOMPSH_FILE)
.PHONY: compiler
compiler: byte

else

.PHONY: native
ALL_TARGETS += native
native: $(ZOMP_DLL_FILE) $(ZOMPC_FILE) $(ZOMPSH_FILE)
.PHONY: compiler
compiler: native

endif

.PRECIOUS: source/machine_stubs.c source/machine.mli source/stats_stubs.c source/stats.mli

################################################################################
# Documentation
################################################################################

.PHONY: doc
doc: $(CAML_DOC_DIR)/index.html

$(CAML_DOC_DIR)/index.html: $(CAMLDEP_INPUT:.mli=.cmi)
	mkdir -p $(CAML_DOC_DIR)
	$(OCAMLDOC) $(CAMLDOC_FLAGS) -html -d $(CAML_DOC_DIR) $(CAMLDEP_INPUT)

