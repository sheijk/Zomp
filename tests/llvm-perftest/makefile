
# CXXFLAGS += -g
# LDFLAGS += -g

CXXFLAGS += `llvm-config --cxxflags`
LDFLAGS += `llvm-config --ldflags`

LLVM_LIBS = `llvm-config --libs executionengine jit interpreter x86`

llvmperftest: main_perftest.o makefile
	$(CXX) -o $@ $< $(LLVM_LIBS)

TAGS: TAGS.local makefile
	cp TAGS.local $@
	cat TAGS.llvm >> $@

TAGS.local:
	etags -o $@ *.cpp

TAGS.llvm:
	rm -f $@
	find `llvm-config --includedir`/llvm -iname *.h -exec etags -o $@ --append {} \;

BROWSE: makefile
	ebrowse *.cpp `find \`llvm-config --includedir\`/llvm -iname *.h`

# Support for flymake mode in emacs (for .cpp and .ml)
FLYMAKE_LOG=flymake.log
FLYMAKE_OUT=flymake.out

cpp_check:
	@echo Checking C++ files $(CHK_SOURCES)
	@g++ -c $(CHK_SOURCES) $(CXXFLAGS) -Wall `llvm-config --cxxflags` -fsyntax-only > $(FLYMAKE_OUT)

check-source: $(patsubst %.cpp,cpp_check,$(CHK_SOURCES))
	@cat $(FLYMAKE_OUT)

check-syntax: check-source
	@echo `date "+%Y-%m-%d %H:%M:%S"` \" \" $(CHK_SOURCES) >> $(FLYMAKE_LOG)
	@rm -f *_flymake.cpp

