
include ../config.mk

LLVM_GCC_BIN_DIR=$(PWD)/../tools/llvm-gcc/bin
LLVM_BIN_DIR=$(PWD)/../tools/llvm/Release/bin
PATH:=$(LLVM_BIN_DIR):$(LLVM_GCC_BIN_DIR):$(PATH)

ifeq ($(DEBUG), 1)
OCAMLC += -g
CXX_FLAGS += -pg -g
C_FLAGS += -pg -g
else
CXX_FLAGS += -O5
endif

CFLAGS = $(C_FLAGS)
CXXFLAGS = $(CXX_FLAGS)

default:
	@echo There is no default build target here
	@exit 1

clean:
	rm -f *.bc *.opt-bc *.ll *.exe *.s *.o


# SIMPLE_PROGRAMS=cee.zomp fib.zomp instrument.zomp
# GL_PROGRAMS=opengl.zomp metaball2d.zomp billboardmix.zomp metaballs.zomp shaderfun.zomp using_assimp.zomp

EXAMPLES = \
  billboardmix.zomp cee.zomp fib.zomp instrument.zomp lighting.zomp \
  metaball2d.zomp metaballs.zomp opengl.zomp shaderfun.zomp tweakbar.zomp \
  using_assimp.zomp

NOT_WORKING_EXAMPLES = static.zomp

test: $(EXAMPLES:.zomp=.exe)
#   Printing a short report. What a mess..
	for f in $(EXAMPLES:.zomp=.exe); do \
		if [ -e $$f ]; then \
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

ZOMP_STDLIBS = ../zompvm_dummy.o stdlib.o
LIBS = $(ZOMP_STDLIBS)
GL_LIBS = $(ZOMP_STDLIBS) ../libglfw.dylib ../libGLEW.dylib -framework OpenGL -framework GLUT

opengl.exe billboardmix.exe metaball2d.exe shaderfun.exe: override LIBS = $(GL_LIBS)
metaballs.exe: override LIBS = $(GL_LIBS) ../libquicktext.dylib
using_assimp.exe: override LIBS = $(GL_LIBS) ../libquicktext.dylib -lstdc++ ../libassimp.a
tweakbar.exe: override LIBS = $(GL_LIBS) ../libAntTweakBar.dylib
lighting.exe: override LIBS = $(GL_LIBS) ../libAntTweakBar.dylib -lstdc++ ../libassimp.a
pipeline_experiment.exe: override LIBS = $(GL_LIBS) ../libAntTweakBar.dylib -lstdc++ ../libassimp.a

################################################################################
# Dependencies
################################################################################

cee.ll: ../libs/libcee.zomp
metaballs.zomp: ../libs/libcee.zomp ../opengl20.zomp ../glfw.zomp ../glut.zomp ../quicktext.zomp ../libs/glutils.zomp
opengl.ll: ../libs/libcee.zomp ../opengl20.zomp ../glfw.zomp ../libs/glutils.zomp
shaderfun.ll: ../opengl20.zomp ../glfw.zomp ../libs/glutils.zomp ../glut.zomp ../quicktext.zomp ../libs/libcee.zomp
using_assimp.zomp: ../libs/libcee.zomp ../opengl20.zomp ../glfw.zomp ../libs/glutils.zomp

################################################################################
# Rules
################################################################################

.PRECIOUS: %.ll

%.ll: %.zomp ../zompc.native
	$(ECHO) Compiling $(<) to .ll...
	../zomp-check-syntax.sh $< $(ZOMPCFLAGS) || rm -f $@

%.bc: %.ll ../stdlib.zomp ../stdlib.c
	echo Compiling $< to $@
	$(LLVM_AS) -f $< -o $@

%.opt-bc: %.bc
	@$(ECHO) Optimizing $< to $@ ...
	$(LLVM_OPT) $< -o $@ -O3

ifeq "$(OPT)" "1"
%.s: %.opt-bc
else
%.s: %.bc
endif
	@$(ECHO) Assembling $@ ...
	$(LLVM_LLC) -o $@ -march=x86 $<

%.o: %.s
	@$(ECHO) Making $@ ...
	$(AS) -o $@ $< -arch i386

stdlib.s: ../stdlib.zomp
	@$(ECHO) Making $@ ...
	$(LLVM_LLC) -o $@ ../stdlib.bc -f -march=x86

%.exe: %.o stdlib.o
	@$(ECHO) Making $@ ...
	$(CC) $(CFLAGS) -o $@ -L. -L.. $(LIBS) $< -arch i386

