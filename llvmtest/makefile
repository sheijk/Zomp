

test: simpletest.bc
	lli simpletest.bc

clean:
	rm *.bc

.SUFFIXES: .ll .bc .c

# LLVM IR Text zu IR Bytecode
.ll.bc:
	rm -f $@
	llvm-as $<

# C File nach Bytecode
.c.bc:
	llvm-gcc -c --emit-llvm $< -o $@
	llvm-dis -f $@

# Faketarget, "make foo.run" fuehrt Bytecodedatei foo.bc aus
# (und erzeugt sie bei Bedarf)
.bc.run:
	lli $<

