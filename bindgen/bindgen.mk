#
# Makefile to build examples
#

ifneq "$(ZOMP_MAIN_MAKEFILE)" "1"
$(error "Call make from the main zomp dir, only")
endif

bindgen/%.o: bindgen/%.cpp
	$(CXX) -c $(CXXFLAGS) `$(LLVM_CONFIG) --cxxflags`  -I$(CLANG_INCLUDE_DIR) $< -o $@

# -llibclang.a -llibclangLex.a -llibclangAST.a -llibclangParse.a
# -llibclangAnalysis.a -llibclangRewrite.a -llibclangBasic.a -llibclangSema.a
# -llibclangCodeGen.a -llibclangSerialization.a -llibclangDriver.a
# -llibclangStaticAnalyzerCheckers.a -llibclangFrontend.a
# -llibclangStaticAnalyzerCore.a -llibclangFrontendTool.a
# -llibclangStaticAnalyzerFrontend.a -llibclangIndex.a

bindgen/bindgen.dylib: bindgen/main.o
	$(CXX) $(LDFLAGS) -shared `$(LLVM_CONFIG) --ldflags` -lLLVMSupport -lLLVMBitReader -lLLVMBitWriter -lLLVMmc -lclang -lclangLex -lclangAST -lclangParse -lclangAnalysis -lclangRewrite -lclangBasic -lclangSema -lclangCodeGen -lclangSerialization -lclangDriver -lclangStaticAnalyzerCheckers -lclangFrontend -lclangStaticAnalyzerCore -lclangFrontendTool -lclangStaticAnalyzerFrontend -lclangIndex $< -o $@

%.bindings: %.c bindgen/bindgen.dylib
	$(CLANG) -cc1 -load bindgen/bindgen.dylib -plugin print-fns $< > $@
	-@$(ECHO) "Generated bindings:"
	-$(CAT) $@

