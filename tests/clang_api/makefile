lookup-symbol: bindgen/lookup_symbol.dylib
	$(CLANG) -cc1 -I$(LLVM_INCLUDE_DIR) -I$(CLANG_INCLUDE_DIR) -load bindgen/lookup_symbol.dylib -plugin lookup-symbol $(FILE) -plugin-arg-lookup-symbol $(SYMBOL)

clang-lookup: bindgen/clang_lookup.exe
	LD_LIBRARY_PATH=$(LLVM_BIN_DIR)/../lib ./bindgen/clang_lookup.exe $(FILE) $(LINE) $(COLUMN)

