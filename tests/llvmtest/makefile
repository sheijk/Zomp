
LLVM_VERSION=2.9
LLVM_GCC=../../tools/llvm-gcc/bin/llvm-gcc
LLVM_DIS=../../tools/llvm-$(LLVM_VERSION)/Release/bin/llvm-dis
LLVM_AS=../../tools/llvm-$(LLVM_VERSION)/Release/bin/llvm-as
LLI=../../tools/llvm-$(LLVM_VERSION)/Release/bin/lli

test: simpletest.bc
	$(LLI) simpletest.bc

clean:
	rm *.bc

# LLVM IR Text zu IR Bytecode
%.bc: %.ll makefile
	rm -f $@
	$(LLVM_AS) $<

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

