

test: simpletest.bc
	lli simpletest.bc

.SUFFIXES: .ll .bc .c

.ll.bc:
	rm -f $@
	llvm-as $<

.c.bc:
	llvm-gcc -c --emit-llvm $< -o $@
	llvm-dis -f $@

