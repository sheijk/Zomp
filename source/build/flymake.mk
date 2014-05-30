#
# Contains rules to use flymake together with OCaml (.ml) and C++ (.cpp) files
#

FLYMAKE_LOG=$(BUILD_DIR_BASE)/flymake-log.txt
FLYMAKE_BUILD=$(BUILD_DIR_BASE)/flymake-last-build.txt

flymake.ml:
	@echo Checking OCaml files $(CHK_SOURCES)
	@rm -f $(FLYMAKE_BUILD)
	@($(OCAMLC) $(CAML_FLAGS) -c $(CHK_SOURCES) -o /tmp/flymake_temp.cmo 2>&1) | tee $(FLYMAKE_BUILD)
	@perl -pe "s/_flymake//g" < $(CHK_SOURCES:.ml=.annot) > $(CHK_SOURCES:_flymake.ml=.annot)
	@cat $(FLYMAKE_BUILD)

flymake.mli: flymake.ml

LLVM_CXXFLAGS = -I/Users/sheijk/Documents/Development/dword/trunk/Stuff/ocaml/lang/git/tools/llvm/Release/include  -D_DEBUG  -D_GNU_SOURCE -D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS -O3   -Woverloaded-virtual
LLVM_CFLAGS = -I/Users/sheijk/Documents/Development/dword/trunk/Stuff/ocaml/lang/git/tools/llvm/Release/include  -D_DEBUG  -D_GNU_SOURCE -D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS -O3  

flymake.cpp:
	@$(CLANGXX) -c $(CHK_SOURCES) $(CXXFLAGS) -Wall $(LLVM_CXXFLAGS) -fsyntax-only

flymake.c:
	@$(CLANG) -c $(CHK_SOURCES) $(CCFLAGS) -Wall $(LLVM_CFLAGS) -fsyntax-only

flymake.zomp:
	echo "Running '$(ZOMPC) --zomp-include-dir .. -c $(CHK_SOURCES) $(ZOMPCFLAGS)'" > $(FLYMAKE_BUILD)
	$(ZOMPC) -c $(CHK_SOURCES) $(ZOMPCFLAGS) >> $(FLYMAKE_BUILD)
	cat $(FLYMAKE_BUILD)

check-source: flymake$(suffix $(CHK_SOURCES))
	@cat $(FLYMAKE_BUILD) || exit 0

ifndef FLYMAKE_LOG
FLYMAKE_TARGET=flymake_missing_settings
else
FLYMAKE_TARGET=flymake_dispatch
endif

flymake_log:
	@echo "$(shell date '+%Y-%m-%d %H:%M:%S') checking $(CHK_SOURCES)" >> $(FLYMAKE_LOG)

flymake_dispatch: flymake_log check-source
	@rm -f *_flymake.cpp

flymake_missing_settings:
	$(error "Please define FLYMAKE_LOG to point to a log file for flymake")

check-syntax: $(FLYMAKE_TARGET)

