
WORKING = comments.zomp constant.zomp globalvar.zomp intrinsics.zomp var.zomp functions.zomp stdlibfuncs.zomp void.zomp 
# WORKING = var.zomp 
FAILING = ifthenelse.zomp loop.zomp simple.zomp

TESTPROGS = $(WORKING) $(FAILING)

check: $(WORKING:.zomp=.check)

checkfail: $(FAILING:.zomp=.check)

checkall: $(TESTPROGS:.zomp=.check)

build: $(WORKING:.zomp=.bc)

stdlibfuncs_exe.bc: stdlibfuncs.bc
	@llvm-ld ../stdlib.bc $< -o ${@:.bc=}

clean_tests:
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

