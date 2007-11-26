
WORKING = comments.zomp constant.zomp globalvar.zomp intrinsics.zomp var.zomp functions.zomp stdlibfuncs.zomp void.zomp voidvar.zomp ops.zomp typedefs.zomp sexpr.zomp pointers.zomp records.zomp ret.zomp labels.zomp strings.zomp macros.zomp nested.zomp arrays.zomp ast.zomp compiletime.zomp casts.zomp variadicmacro.zomp floats.zomp 

# WORKING = comments.zomp constant.zomp globalvar.zomp intrinsics.zomp functions.zomp voidvar.zomp ops.zomp typedefs.zomp pointers.zomp records.zomp ret.zomp macros.zomp nested.zomp arrays.zomp strings.zomp

# WORKING = stuff.zomp
# WORKING = stuff.zomp comments.zomp

FAILING = simple.zomp labels.zomp sexpr.zomp void.zomp stdlibfuncs.zomp var.zomp var.zomp


TESTPROGS = $(WORKING) $(FAILING)

check: $(WORKING:.zomp=.check)

checkfail: $(FAILING:.zomp=.check)

checkall: $(TESTPROGS:.zomp=.check)

build: $(WORKING:.zomp=.bc)

compileperftest:
	@for funcs in 10 100 1000 1200 1400 1600 1800 2000 2200 2400 2600 2800 3000 4000 5000 7500 10000; \
	do \
	  rm -f compileperf.zomp; \
	  echo; \
	  echo Measuring compile time of simple program: `wc -l compileperf.zomp`; \
	  ocaml genperftest.ml $$funcs compileperf.zomp; \
	  time ../zomp-check-syntax.sh compileperf.zomp; \
	done

stdlibfuncs_exe.bc: stdlibfuncs.bc
	@llvm-ld ../stdlib.bc $< -o ${@:.bc=}

clean_tests:
	@echo Cleaning tests...
	rm -f *.ll *.bc
	rm -f compileperf.*

.SUFFIXES: .zomp .ll .check .bc .run

.zomp.check:
	@echo Checking $<
	@rm -f ${<:.zomp=.ll}
	@../zomp-check-syntax.sh $< || cat ${<:.zomp=.ll}

.zomp.run:
	@rm -f ${<:.zomp=.ll} ${<:.zomp=.bc}
	@make $(<:.zomp=.bc)
	@echo Running $(<:.zomp=)
	@lli $(<:.zomp=.bc)
	@echo Done

.zomp.ll:
	@echo Compiling $< to LLVM IR
	@../zomp-check-syntax.sh $<

.ll.bc:
	@echo Compiling $< to $@
	@llvm-as -f $< -o $(<:.ll=.o.bc)
	@llvm-link -f ../stdlib.bc $(<:.ll=.o.bc) -o $@

