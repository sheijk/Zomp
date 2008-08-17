

test: simpletest.bc
	lli simpletest.bc

clean:
	rm *.bc

# LLVM IR Text zu IR Bytecode
%.bc: %.ll
	rm -f $@
	llvm-as $<

# C File nach Bytecode
%.bc: %.c
	llvm-gcc -c --emit-llvm -O0 $< -o $@
	llvm-dis -f $@

misc.ll: misc.bc
	llvm-dis -f $< -o $@

# Faketarget, "make foo.run" fuehrt Bytecodedatei foo.bc aus
# (und erzeugt sie bei Bedarf)
%.run: %.bc
	lli $<

FLYMAKE_LOG=flymake-log.temp

include ../makefile.flymake

