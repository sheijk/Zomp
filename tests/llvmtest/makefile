
LLVM_DIR=../../tools/llvm-$(LLVM_VERSION)

LLVM_VERSION=2.9
LLVM_GCC=../../tools/llvm-gcc/bin/llvm-gcc
LLVM_DIS=$(LLVM_DIR)/Release/bin/llvm-dis
LLVM_AS=$(LLVM_DIR)/Release/bin/llvm-as
LLI=$(LLVM_DIR)/Release/bin/lli
LLC=$(LLVM_DIR)/Release/bin/llc
CLANG=$(LLVM_DIR)/Release/bin/clang

test: simpletest.bc
	$(LLI) simpletest.bc

clean:
	rm -f *.s *.exe *.bc *.ll

# LLVM IR Text zu IR Bytecode
%.bc: %.ll makefile
	rm -f $@
	$(LLVM_AS) $<

.PRECIOUS: %.s %.exe

# %.s: %.bc makefile
# 	$(LLC) $< -o $@

%.exe: %.bc makefile
	$(CLANG) -g $< -o $@

q%.clang.exe: %.c makefile
	$(CLANG) -g $< -o $@

# %.exe: %.s makefile
# 	$(CLANG) 
# gcc $< -o $@

# C File nach Bytecode
%.ll: %.c makefile
	$(LLVM_GCC) -c --emit-llvm -O0 -g $< -o ${@:.ll=.bc}
	$(LLVM_DIS) -f ${@:.ll=.bc}

# misc.ll: misc.bc
# 	$(LLVM_DIS) -f -o $@ < $f

ll2bc:
	$(LLVM_AS) -f -o $(FILE).bc < $(FILE).ll

loopopt.bc: loopopt.c
	$(LLVM_GCC) -c --emit-llvm -O3 $< -o $@
	$(LLVM_DIS) -f $@

# Faketarget, "make foo.run" fuehrt Bytecodedatei foo.bc aus
# (und erzeugt sie bei Bedarf)
%.run: %.bc makefile
	$(LLI) $<

FLYMAKE_LOG=flymake-log.temp

include ../../flymake.mk

