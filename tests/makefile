
WORKING = comments.zomp constant.zomp functions.zomp globalvar.zomp stdlibfuncs.zomp var.zomp
FAILING = extfuncs.zomp ifthenelse.zomp loop.zomp simple.zomp

TESTPROGS = $(WORKING) $(FAILING)

check: $(WORKING:.zomp=.check)

checkfail: $(FAILING:.zomp=.check)

checkall: $(TESTPROGS:.zomp=.check)

build: $(WORKING:.zomp=.bc)

clean:
	rm -f *.ll *.bc

.SUFFIXES: .zomp .ll .check .bc

.zomp.check:
	@echo Checking $<
	@zomp-check-syntax.sh $< || cat ${<:.zomp=.ll}

.zomp.ll:
	@echo Compiling $< to LLVM IR
	@zomp-check-syntax.sh $<

.ll.bc:
	@echo Compiling $< to $@
	@llvm-as -f $< -o $@

