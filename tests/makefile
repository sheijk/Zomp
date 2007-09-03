
WORKING = comments.zomp constant.zomp globalvar.zomp intrinsics.zomp var.zomp functions.zomp stdlibfuncs.zomp void.zomp ifthenelse.zomp loop.zomp ops.zomp typedefs.zomp sexpr.zomp pointers.zomp
# WORKING = pointers.zomp

FAILING = simple.zomp records.zomp pointers.zomp

TESTPROGS = $(WORKING) $(FAILING)

check: $(WORKING:.zomp=.check)

checkfail: $(FAILING:.zomp=.check)

checkall: $(TESTPROGS:.zomp=.check)

build: $(WORKING:.zomp=.bc)

stdlibfuncs_exe.bc: stdlibfuncs.bc
	@llvm-ld ../stdlib.bc $< -o ${@:.bc=}

clean_tests:
	rm -f *.ll *.bc

.SUFFIXES: .zomp .ll .check .bc .run

.zomp.check:
	@echo Checking $<
	@../zomp-check-syntax.sh $< || cat ${<:.zomp=.ll}

.bc.run:
	@echo Running $(<:.bc=)
	@lli $<
	@echo Done

.zomp.ll:
	@echo Compiling $< to LLVM IR
	@../zomp-check-syntax.sh $<

.ll.bc:
	@echo Compiling $< to $@
	@llvm-as -f $< -o $(<:.ll=.o.bc)
	@llvm-link -f ../stdlib.bc $(<:.ll=.o.bc) -o $@

