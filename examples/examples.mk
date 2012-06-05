#
# Makefile to build examples
#

ifneq "$(ZOMP_MAIN_MAKEFILE)" "1"
$(error "Call make from the main zomp dir, only (examples)")
endif

CLEAN_SUB_TARGETS += examples/clean
examples/clean:
	cd examples && rm -f *.bc *.opt-bc *.ll *.exe *.s *.o

EXAMPLES_SOURCES_X = \
  cee.zomp fib.zomp instrument.zomp lighting.zomp hello.zomp \
  metaball2d.zomp metaballs.zomp opengl.zomp shaderfun.zomp tweakbar.zomp \
  using_assimp.zomp smallpt.zomp pipeline_experiment.zomp

EXAMPLES_SOURCES = $(foreach FILE, $(EXAMPLES_SOURCES_X), examples/$(FILE))

NOT_WORKING_EXAMPLES = static.zomp

examples/test: $(EXAMPLES_SOURCES:.zomp=.exe)
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
examples/image_loading.exe: override LIBS = $(ZOMP_STDLIBS) $(GL_LIBS) $(LINK_ANTTWEAKBAR)

################################################################################
# Dependencies
################################################################################

examples/cee.ll: libs/libcee.zomp
examples/metaballs.zomp: libs/libcee.zomp libs/opengl20.zomp libs/glfw.zomp libs/glut.zomp libs/quicktext.zomp libs/glutils.zomp
examples/opengl.ll: libs/libcee.zomp libs/opengl20.zomp libs/glfw.zomp libs/glutils.zomp
examples/shaderfun.ll: libs/opengl20.zomp libs/glfw.zomp libs/glutils.zomp libs/glut.zomp libs/quicktext.zomp libs/libcee.zomp
examples/using_assimp.zomp: libs/libcee.zomp libs/opengl20.zomp libs/glfw.zomp libs/glutils.zomp

################################################################################
# Rules
################################################################################

.PRECIOUS: examples/%.ll
.PHONY: examples/%.run

examples/runtime.s: runtime.zomp
	@$(ECHO) Making $@ ...
	$(LLVM_LLC) -o $@ runtime.bc -f -march=x86

examples/%.run: examples/%.exe
	@$(ECHO) Running $@ ...
	DYLD_LIBRARY_PATH=${DYLD_LIBRARY_PATH}:. ./$<

