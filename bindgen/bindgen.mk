#
# Makefile to build examples
#

.PRECIOUS: bindgen/%.o
bindgen/%.o: bindgen/%.cpp
	@$(ECHO) Compiling $< ...
	$(CXX) -c $(CXXFLAGS) `$(LLVM_CONFIG) --cxxflags` $< -o $@

CLANG_LIBS = -lclang -lclangLex -lclangAST -lclangParse -lclangAnalysis -lclangRewrite -lclangBasic -lclangSema -lclangCodeGen -lclangSerialization -lclangDriver -lclangStaticAnalyzerCheckers -lclangFrontend -lclangStaticAnalyzerCore -lclangFrontendTool -lclangStaticAnalyzerFrontend -lclangIndex

.PRECIOUS: bindgen/%.dylib
bindgen/%.dylib: bindgen/%_plugin.o
	@$(ECHO) Linking $@ ...
	$(CXX) $(LDFLAGS) -shared `$(LLVM_CONFIG) --ldflags` -lLLVMSupport -lLLVMBitReader -lLLVMBitWriter -lLLVMmc $(CLANG_LIBS) $< -o $@

CLEAN_SUB_TARGETS += bindgen/clean
bindgen/clean:
	@$(ECHO) "Cleaning bindgen ..."
	$(DELETE_FILE) bindgen/zomp_bindgen_plugin.o bindgen/zomp_bindgen.dylib

%.bindings: %.h bindgen/zomp_bindgen.dylib bindgen/bindgen.mk
	@$(ECHO) Generating Zomp bindings for $< ...
	$(CLANG) -cc1 -load bindgen/zomp_bindgen.dylib -plugin gen-zomp-bindings $< > $@

%.bindings: %.c bindgen/zomp_bindgen.dylib bindgen/bindgen.mk
	@$(ECHO) Generating Zomp bindings for $< ...
	$(CLANG) -cc1 -load bindgen/zomp_bindgen.dylib -plugin gen-zomp-bindings $< > $@


