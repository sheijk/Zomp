#
# Makefile to build examples
#

CLEAN_SUB_TARGETS += examples/clean
examples/clean:
	cd examples && rm -f *.bc *.opt-bc *.ll *.exe *.s *.o

EXAMPLES_SOURCES = $(wildcard examples/*.zomp)

NOT_WORKING_EXAMPLES = static.zomp

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
examples/metaball2d.zomp: libs/libcee.zomp libs/opengl20.zomp libs/opengl20print.zomp libs/glfw.zomp libs/glut.zomp libs/quicktext.zomp libs/glutils.zomp
examples/opengl.ll: libs/libcee.zomp libs/opengl20.zomp libs/glfw.zomp libs/glutils.zomp
examples/shaderfun.ll: libs/opengl20.zomp libs/glfw.zomp libs/glutils.zomp libs/glut.zomp libs/quicktext.zomp libs/libcee.zomp
examples/using_assimp.zomp: libs/libcee.zomp libs/opengl20.zomp libs/glfw.zomp libs/glutils.zomp
examples/tweakbar.ll: libs/libcee.zomp libs/glutils.zomp libs/opengl20.zomp libs/glfw.zomp libs/anttweakbar.zomp

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

