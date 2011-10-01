#
# Makefile to build examples
#

ifneq "$(ZOMP_MAIN_MAKEFILE)" "1"
$(error "Call make from the main zomp dir, only (examples)")
endif

examples/clean:
	cd examples && rm -f *.bc *.opt-bc *.ll *.exe *.s *.o

EXAMPLES = \
  billboardmix.zomp cee.zomp fib.zomp instrument.zomp lighting.zomp \
  metaball2d.zomp metaballs.zomp opengl.zomp shaderfun.zomp tweakbar.zomp \
  using_assimp.zomp smallpt.zomp

NOT_WORKING_EXAMPLES = static.zomp

examples/test: $(foreach FILE, $(EXAMPLES), examples/$(FILE:.zomp=.exe))
#   Printing a short report. What a mess..
	for f in $(EXAMPLES:.zomp=.exe); do \
		if [ -e examples/$$f ]; then \
			echo "Ok     $$f"; \
		else \
			echo "Failed $$f"; \
		fi; \
	done

# default to optimizations enabled
ifeq "$(OPT)" ""
OPT=1
endif

################################################################################
# Additional libraries
################################################################################

ZOMP_STDLIBS = source/zompvm_dummy.o source/runtime.o
LIBS = $(ZOMP_STDLIBS)
GL_LIBS = $(LINK_GLFW) $(LINK_GLEW) $(LINK_GL) $(LINK_GLUT)

examples/opengl.exe examples/billboardmix.exe examples/metaball2d.exe examples/shaderfun.exe: override LIBS = $(ZOMP_STDLIBS) $(GL_LIBS)
examples/metaballs.exe: override LIBS = $(ZOMP_STDLIBS) $(GL_LIBS) $(LINK_QUICKTEXT)
examples/using_assimp.exe: override LIBS = $(ZOMP_STDLIBS) $(GL_LIBS) $(LINK_QUICKTEXT) $(LINK_CPPSTDLIB) $(LINK_ASSIMP)
examples/tweakbar.exe: override LIBS = $(ZOMP_STDLIBS) $(GL_LIBS) $(LINK_ANTTWEAKBAR)
examples/lighting.exe: override LIBS = $(ZOMP_STDLIBS) $(GL_LIBS) $(LINK_ANTTWEAKBAR) $(LINK_CPPSTDLIB) $(LINK_ASSIMP)
examples/pipeline_experiment.exe: override LIBS = $(ZOMP_STDLIBS) $(GL_LIBS) $(LINK_ANTTWEAKBAR) $(LINK_CPPSTDLIB) $(LINK_ASSIMP) $(LINK_UTILS)
examples/utilsdemo.exe: override LIBS = $(ZOMP_STDLIBS) $(LINK_CPPSTDLIB) $(LINK_UTILS)

################################################################################
# Dependencies
################################################################################

examples/cee.ll: libs/libcee.zomp
examples/metaballs.zomp: libs/libcee.zomp opengl20.zomp glfw.zomp glut.zomp quicktext.zomp libs/glutils.zomp
examples/opengl.ll: libs/libcee.zomp opengl20.zomp glfw.zomp libs/glutils.zomp
examples/shaderfun.ll: opengl20.zomp glfw.zomp libs/glutils.zomp glut.zomp quicktext.zomp libs/libcee.zomp
examples/using_assimp.zomp: libs/libcee.zomp opengl20.zomp glfw.zomp libs/glutils.zomp

################################################################################
# Rules
################################################################################

.PRECIOUS: examples/%.ll
.PHONY: examples/%.run

examples/%.ll: examples/%.zomp $(ZOMPC) has_llvm
	$(ECHO) Compiling $(<) to .ll...
	$(ZOMPC) -c $< $(ZOMPCFLAGS) || rm -f $@

examples/%.bc: examples/%.ll prelude.zomp source/runtime.c
	echo Compiling $< to $@
	$(LLVM_AS) -f $< -o $@

examples/%.opt-bc: examples/%.bc
	@$(ECHO) Optimizing $< to $@ ...
	$(LLVM_OPT) $< -o $@ -O3

ifeq "$(OPT)" "1"
examples/%.s: examples/%.opt-bc
else
examples/%.s: examples/%.bc
endif
	@$(ECHO) Assembling $@ ...
	$(LLVM_LLC) -o $@ -march=x86 $<

examples/%.o: examples/%.s
	@$(ECHO) Making $@ ...
	$(AS) -o $@ $< -arch i386

examples/runtime.s: runtime.zomp
	@$(ECHO) Making $@ ...
	$(LLVM_LLC) -o $@ runtime.bc -f -march=x86

examples/%.exe: examples/%.o source/runtime.o
	@$(ECHO) Making $@ ...
	$(CC) $(CFLAGS) -o $@ -L. -L./examples $(LIBS) $< -arch i386

examples/%.run: examples/%.exe
	@$(ECHO) Running $@ ...
	DYLD_LIBRARY_PATH=${DYLD_LIBRARY_PATH}:. ./$<

