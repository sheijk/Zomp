#
# Makefile to build examples
#

ifneq "$(ZOMP_MAIN_MAKEFILE)" "1"
$(error "Call make from the main zomp dir, only")
endif

bindgen/%.o: bindgen/%.cpp
	$(CXX) -c $(CXXFLAGS) `$(LLVM_CONFIG) --cxxflags`  -I$(CLANG_INCLUDE_DIR) $< -o $@

CLANG_LIBS = -lclang -lclangLex -lclangAST -lclangParse -lclangAnalysis -lclangRewrite -lclangBasic -lclangSema -lclangCodeGen -lclangSerialization -lclangDriver -lclangStaticAnalyzerCheckers -lclangFrontend -lclangStaticAnalyzerCore -lclangFrontendTool -lclangStaticAnalyzerFrontend -lclangIndex

bindgen/bindgen.dylib: bindgen/zomp_bindgen_plugin.o
	$(CXX) $(LDFLAGS) -shared `$(LLVM_CONFIG) --ldflags` -lLLVMSupport -lLLVMBitReader -lLLVMBitWriter -lLLVMmc $(CLANG_LIBS) $< -o $@

CLEAN_SUB_TARGETS += bindgen/clean
bindgen/clean:
	rm -f bindgen/zomp_bindgen_plugin.o bindgen/bindgen.dylib

%.bindings: %.c bindgen/bindgen.dylib
	$(CLANG) -cc1 -load bindgen/bindgen.dylib -plugin gen-zomp-bindings $< > $@

