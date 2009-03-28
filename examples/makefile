
include ../config.mk

LLVM_GCC_BIN_DIR=$(PWD)/../tools/llvm-gcc/bin
LLVM_BIN_DIR=$(PWD)/../tools/llvm/Release/bin
PATH:=$(LLVM_BIN_DIR):$(LLVM_GCC_BIN_DIR):$(PATH)

default:
	@echo There is no default build target here
	@exit 1

clean:
	rm -f *.bc *.ll *.exe *.glexe

SIMPLE_PROGRAMS=cee.zomp fib.zomp instrument.zomp
GL_PROGRAMS=opengl.zomp
NOT_WORKING_PROGRAMS=static.zomp metaballs.zomp using_assimp.zomp shaderfun.zomp

test: $(SIMPLE_PROGRAMS:.zomp=.exe) $(GL_PROGRAMS:.zomp=.glexe)

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
	llvm-as -f $< -o $(<:.ll=.o.bc)
	llvm-link -f ../stdlib.bc $(<:.ll=.o.bc) -o $@

%.exe: %.bc
	llvm-ld -native -o $@ $< -L=.. -L=. $(LLVMLDFLAGS)

%.glexe: %.bc
	llvm-ld -native -o $@ $< -L=.. -L-. $(LLVMLDFLAGS) -lglfw -lGLEW -framework=OpenGL -framework=GLUT

