
include ../config.mk

LLVM_GCC_BIN_DIR=$(PWD)/../tools/llvm-gcc/bin
LLVM_BIN_DIR=$(PWD)/../tools/llvm/Release/bin
PATH:=$(LLVM_BIN_DIR):$(LLVM_GCC_BIN_DIR):$(PATH)

default:
	@echo There is no default build target here
	@exit 1

clean:
	rm -f *.bc *.opt-bc *.ll *.exe *.glexe *.s *.o

SIMPLE_PROGRAMS=cee.zomp fib.zomp instrument.zomp
GL_PROGRAMS=opengl.zomp metaball2d.zomp
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
	$(LLVM_AS) -f $< -o $@

%.opt-bc: %.bc
	@$(ECHO) Optimizing $< to $@ ...
	$(LLVM_OPT) $< -o $@ -O3

# default to optimizations enabled
ifeq "$(OPT)" ""
OPT=1
endif

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
	$(LLVM_LLC) -o $@ ../stdlib.bc -march=x86

%.exe: %.o stdlib.o
	@$(ECHO) Making $@ ...
	$(CC) -o $@ ../zompvm_dummy.o stdlib.o $< -arch i386

%.glexe: %.o stdlib.o
	@$(ECHO) Making $@ ...
	$(CC) -o $@ stdlib.o -L. -L.. ../zompvm_dummy.o ../libglfw.dylib ../libGLEW.dylib -framework OpenGL -framework GLUT $< -arch i386

