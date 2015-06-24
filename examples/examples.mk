#
# Makefile to build examples
#

EXAMPLES_SOURCES = \
  examples/fib.zomp \
  examples/mini.zomp \
  examples/hello.zomp \
  examples/utilsdemo.zomp \
  examples/fourk/fourk.zomp \
  examples/macros/instrument.zomp \
  examples/macros/introspection.zomp \
  examples/macros/loop.zomp \
  examples/opengl/circles.zomp \
  examples/opengl/lighting.zomp \
  examples/opengl/metaballs.zomp \
  examples/opengl/metaball2d.zomp \
  examples/opengl/simple.zomp \
  examples/opengl/pipeline_experiment.zomp \
  examples/opengl/shaderfun.zomp \
  examples/opengl/tweakbar.zomp \
  examples/opengl/using_assimp.zomp \
  examples/opengl/wobble.zomp \
  examples/smallpt/smallpt.zomp

CLEAN_SUB_TARGETS += examples/clean
examples/clean:
	@$(ECHO) "Cleaning examples ..."
	@$(DELETE_FILE) $(foreach file, $(EXAMPLES_SOURCES:.zomp=), $(file).{bc,opt-bc,ll,exe,s,o,compile_output,compile_stats,result,zomp.expanded})

.PHONY: examples/all
ALL_TARGETS += examples/all
examples/all: $(EXAMPLES_SOURCES:.zomp=.exe)

TEST_SUB_TARGETS += examples/test
.PHONY: examples/test
examples/test: examples/all

# default to optimizations enabled
ifeq "$(OPT)" ""
OPT=1
endif

################################################################################
# Additional libraries
################################################################################

ZOMP_STDLIBS = source/zompvm_dummy.o source/zompvm_caml_dummy.o source/runtime.o
LIBS = $(ZOMP_STDLIBS)
GL_LIBS = $(LINK_GLFW) $(LINK_GLEW) $(LINK_GL) $(LINK_GLUT)

examples/utilsdemo.exe: override LIBS = $(ZOMP_STDLIBS) $(LINK_CPPSTDLIB) $(LINK_UTILS)

examples/fourk/fourk.exe: override LIBS = $(ZOMP_STDLIBS) $(GL_LIBS) $(LINK_ANTTWEAKBAR) $(LINK_QUICKTEXT)

examples/opengl/simple.exe: override LIBS = $(ZOMP_STDLIBS) $(GL_LIBS)
examples/opengl/metaball2d.exe: override LIBS = $(ZOMP_STDLIBS) $(GL_LIBS)
examples/opengl/shaderfun.exe: override LIBS = $(ZOMP_STDLIBS) $(GL_LIBS)
examples/opengl/circles.exe: override LIBS = $(ZOMP_STDLIBS) $(GL_LIBS) $(LINK_ANTTWEAKBAR)
examples/opengl/wobble.exe: override LIBS = $(ZOMP_STDLIBS) $(GL_LIBS) $(LINK_ANTTWEAKBAR)
examples/opengl/metaballs.exe: override LIBS = $(ZOMP_STDLIBS) $(GL_LIBS) $(LINK_QUICKTEXT)
examples/opengl/using_assimp.exe: override LIBS = $(ZOMP_STDLIBS) $(GL_LIBS) $(LINK_QUICKTEXT) $(LINK_CPPSTDLIB) $(LINK_ASSIMP)
examples/opengl/tweakbar.exe: override LIBS = $(ZOMP_STDLIBS) $(GL_LIBS) $(LINK_ANTTWEAKBAR)
examples/opengl/lighting.exe: override LIBS = $(ZOMP_STDLIBS) $(GL_LIBS) $(LINK_ANTTWEAKBAR) $(LINK_CPPSTDLIB) $(LINK_ASSIMP)
examples/opengl/pipeline_experiment.exe: override LIBS = $(ZOMP_STDLIBS) $(GL_LIBS) $(LINK_ANTTWEAKBAR) $(LINK_CPPSTDLIB) $(LINK_ASSIMP) $(LINK_UTILS)

################################################################################
# Dependencies
################################################################################

examples/cee.ll: libs/libcee.zomp
examples/opengl/metaballs.ll: libs/libcee.zomp libs/opengl20.zomp libs/glfw.zomp libs/glut.zomp libs/quicktext.zomp libs/glutils.zomp
examples/opengl/metaball2d.ll: libs/libcee.zomp libs/opengl20.zomp libs/opengl20print.zomp libs/glfw.zomp libs/glut.zomp libs/quicktext.zomp libs/glutils.zomp
examples/opengl/simple.ll: libs/libcee.zomp libs/opengl20.zomp libs/glfw.zomp libs/glutils.zomp
examples/opengl/shaderfun.ll: libs/opengl20.zomp libs/glfw.zomp libs/glutils.zomp libs/glut.zomp libs/quicktext.zomp libs/libcee.zomp
examples/opengl/using_assimp.ll: libs/libcee.zomp libs/opengl20.zomp libs/glfw.zomp libs/glutils.zomp
examples/opengl/tweakbar.ll: libs/libcee.zomp libs/glutils.zomp libs/opengl20.zomp libs/glfw.zomp libs/anttweakbar.zomp

################################################################################
# Rules
################################################################################

.PRECIOUS: examples/%.ll
.PHONY: examples/%.run

examples/runtime.s: runtime.zomp
	@$(ECHO) Making $@ ...
	$(LLVM_LLC) -o $@ runtime.bc -f -march=$(LLVM_ARCH)

examples/%.run: examples/%.exe
	@$(ECHO) Running $@ ...
	DYLD_LIBRARY_PATH=${DYLD_LIBRARY_PATH}:. ./$<

