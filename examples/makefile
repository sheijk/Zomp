
include ../makefile.config

LLVM_GCC_BIN_DIR=$(PWD)/../tools/llvm-gcc/bin
LLVM_BIN_DIR=$(PWD)/../tools/llvm/Release/bin
PATH:=$(LLVM_BIN_DIR):$(LLVM_GCC_BIN_DIR):$(PATH)

clean:
	rm -f *.bc *.ll *.exe

################################################################################
# Rules
################################################################################

.PRECIOUS: %.ll

%.ll: %.zomp ../zompc.native
	$(ECHO) Compiling $(<) to .ll...
	../zomp-check-syntax.sh $< || rm -f $@

%.bc: %.ll ../stdlib.zomp ../stdlib.c
	echo Compiling $< to $@
	llvm-as -f $< -o $(<:.ll=.o.bc)
	llvm-link -f ../stdlib.bc $(<:.ll=.o.bc) -o $@

%.exe: %.bc
	llvm-ld -native -o $@ $< -L=.. -L=. $(LLVMLDFLAGS)

opengl.exe: opengl.bc
	llvm-ld -native -o $@ $< -L=.. -L=. $(LLVMLDFLAGS) -lglfw -lGLEW -framework=OpenGL

