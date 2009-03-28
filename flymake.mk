#
# Contains rules to use flymake together with OCaml (.ml) and C++ (.cpp) files
#

FLYMAKE_BUILD=last_build_$(FLYMAKE_LOG)

ml_check:
	@echo Checking OCaml files $(CHK_SOURCES)
	@$(OCAMLC) $(CAML_FLAGS) -c $(CHK_SOURCES) -o /tmp/flymake_temp.cmo > $(FLYMAKE_BUILD)
	@perl -pe "s/_flymake//g" < $(CHK_SOURCES:.ml=.annot) > $(CHK_SOURCES:_flymake.ml=.annot)
	@cat $(FLYMAKE_BUILD)

LLVM_CXXFLAGS = -I/Users/sheijk/Documents/Development/dword/trunk/Stuff/ocaml/lang/git/tools/llvm/Release/include  -D_DEBUG  -D_GNU_SOURCE -D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS -O3   -Woverloaded-virtual
LLVM_CFLAGS = -I/Users/sheijk/Documents/Development/dword/trunk/Stuff/ocaml/lang/git/tools/llvm/Release/include  -D_DEBUG  -D_GNU_SOURCE -D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS -O3  

cpp_check:
	@$(LLVM_GXX) -c $(CHK_SOURCES) $(CXX_FLAGS) -Wall $(LLVM_CXXFLAGS) -fsyntax-only

c_check:
	@$(LLVM_GCC) -c $(CHK_SOURCES) $(C_FLAGS) -Wall $(LLVM_CFLAGS) -fsyntax-only

check-source: $(patsubst %.c,c_check, $(patsubst %.ml,ml_check, $(patsubst %.cpp,cpp_check,$(CHK_SOURCES))))
	@cat $(FLYMAKE_BUILD) || exit 0

ifndef FLYMAKE_LOG
FLYMAKE_TARGET=flymake_missing_settings
else
FLYMAKE_TARGET=flymake_dispatch
endif

flymake_log:
	@echo `date "+%Y-%m-%d %H:%M:%S"` checking $(CHK_SOURCES) >> $(FLYMAKE_LOG)

flymake_dispatch: flymake_log check-source
	@rm -f *_flymake.cpp

flymake_missing_settings:
	@echo "flymake.mk:1: error: Please define FLYMAKE_LOG to point to a log file for flymake"

check-syntax: $(FLYMAKE_TARGET)

