
LLVM_GCC=../tools/llvm-gcc/bin/llvm-gcc
LLVM_DIS=../tools/llvm/Release/bin/llvm-dis
LLVM_AS=../tools/llvm/Release/bin/llvm-as
LLI=../tools/llvm/Release/bin/lli

test: simpletest.bc
	$(LLI) simpletest.bc

clean:
	rm *.bc

# LLVM IR Text zu IR Bytecode
%.bc: %.ll
	rm -f $@
	$(LLVM_AS) $<

# C File nach Bytecode
%.bc: %.c
	$(LLVM_GCC) -c --emit-llvm -O0 $< -o $@
	$(LLVM_DIS) -f $@

misc.ll: misc.bc
	$(LLVM_DIS) -f $< -o $@

# Faketarget, "make foo.run" fuehrt Bytecodedatei foo.bc aus
# (und erzeugt sie bei Bedarf)
%.run: %.bc
	$(LLI) $<

FLYMAKE_LOG=flymake-log.temp

include ../makefile.flymake

