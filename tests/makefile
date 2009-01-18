
WORKING = comments.zomp constant.zomp globalvar.zomp var.zomp stdlibfuncs.zomp void.zomp voidvar.zomp ops.zomp typedefs.zomp sexpr.zomp pointers.zomp records.zomp ret.zomp labels.zomp strings.zomp macros.zomp nested.zomp arrays.zomp ast.zomp compiletime.zomp casts.zomp variadicmacro.zomp floats.zomp ops.zomp

OUTDATED= intrinsics.zomp functions.zomp
# WORKING=sharkperf.zomp

# WORKING = comments.zomp constant.zomp globalvar.zomp intrinsics.zomp functions.zomp voidvar.zomp ops.zomp typedefs.zomp pointers.zomp records.zomp ret.zomp macros.zomp nested.zomp arrays.zomp strings.zomp

# WORKING = stuff.zomp
# WORKING = stuff.zomp comments.zomp

FAILING = simple.zomp labels.zomp sexpr.zomp void.zomp stdlibfuncs.zomp var.zomp var.zomp


TESTPROGS = $(WORKING) $(FAILING)

check: $(WORKING:.zomp=.check)

checkfail: $(FAILING:.zomp=.check)

checkall: $(TESTPROGS:.zomp=.check)

build: $(WORKING:.zomp=.bc)

# FUNCTION_COUNTS=10 100 1000 1200 1400 1600 1800 2000 2200 2400
FUNCTION_COUNTS=10 100 1000 4000

LLVM_GCC_BIN_DIR=$(PWD)/../tools/llvm-gcc/bin
LLVM_BIN_DIR=$(PWD)/../tools/llvm/Release/bin
PATH:=$(LLVM_BIN_DIR):$(LLVM_GCC_BIN_DIR):$(PATH)

compileperftest:
	@rm -f timing.txt
	@for funcs in $(FUNCTION_COUNTS); \
	do \
	  rm -f compileperf.zomp; \
	  ocaml genperftest.ml $$funcs compileperf.zomp; \
	  ./logtiming.sh timing.txt $$funcs '../zomp-check-syntax.sh compileperf.zomp'; \
	done

GLAPP_LIBS = -framework Carbon -framework CoreFoundation -framework OpenGL -framework AGL ../libglfw.a ../libGLEW.a

gltest_llvm.exe: gltest.bc
	@echo Building $@ ...
	llvm-ld -native -o $@ ../stdlib.bc $<

gltest.exe: gltest_converted.c
	@echo Building $@ ...
	g++ -c -g -o gltest_converted.o gltest_converted.c
	g++ -g -o gltest.exe $(GLAPP_LIBS) gltest_converted.o
# 	g++ -o gltest.exe -g $(GLAPP_LIBS) gltest_converted.c


stdlibfuncs_exe.bc: stdlibfuncs.bc
	@llvm-ld ../stdlib.bc $< -o ${@:.bc=}

clean_tests:
	@echo Cleaning tests ...
	rm -f *.ll *.bc *.exe
	rm -f compileperf.*

%.cbe.c: %.bc
	@echo Generating $@ ...
	llc -march=c -f $<

%.check: %.zomp
	@echo Checking $<
	@rm -f ${<:.zomp=.ll}
	@../zomp-check-syntax.sh $<

%.run: %.zomp
	@rm -f ${<:.zomp=.ll} ${<:.zomp=.bc}
	@make $(<:.zomp=.bc)
	@echo Running $(<:.zomp=)
	@lli $(<:.zomp=.bc)
	@echo Done

%.ll: %.zomp
	@echo Compiling $< to LLVM IR
	@../zomp-check-syntax.sh $<

%.bc: %.ll
	@echo Compiling $< to $@
	@llvm-as -f $< -o $(<:.ll=.o.bc)
	@llvm-link -f ../stdlib.bc $(<:.ll=.o.bc) -o $@

%.exe: %.bc
	@echo Building $@ ...
	@llvm-ld $< -native -o $@

%.s: %.bc
	@llc $< -f -o $@

%.o: %.s
	@as $< -o $@

%.glexe: %.o
	@g++ -o $@ -lstdc++ $(GLAPP_LIBS) $<

# glanim.glexe: glanim.o
# 	@g++ -o $@ -lstdc++ $(GLAPP_LIBS) glanim.o

