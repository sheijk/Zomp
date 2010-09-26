
ZOMP_TOOL_PATH=$(PWD)/../tools
include ../config.mk

WORKING = comments.zomp constant.zomp globalvar.zomp var.zomp stdlibfuncs.zomp void.zomp voidvar.zomp ops.zomp typedefs.zomp sexpr.zomp pointers.zomp records.zomp ret.zomp labels.zomp strings.zomp macros.zomp nested.zomp arrays.zomp ast.zomp compiletime.zomp casts.zomp variadicmacro.zomp floats.zomp ops.zomp

OUTDATED= intrinsics.zomp functions.zomp
# WORKING=sharkperf.zomp

# WORKING = comments.zomp constant.zomp globalvar.zomp intrinsics.zomp functions.zomp voidvar.zomp ops.zomp typedefs.zomp pointers.zomp records.zomp ret.zomp macros.zomp nested.zomp arrays.zomp strings.zomp

# WORKING = stuff.zomp
# WORKING = stuff.zomp comments.zomp

FAILING = simple.zomp labels.zomp sexpr.zomp void.zomp stdlibfuncs.zomp var.zomp var.zomp

.PHONY: .run

TESTPROGS = $(WORKING) $(FAILING)

check: $(WORKING:.zomp=.check)

checkfail: $(FAILING:.zomp=.check)

checkall: $(TESTPROGS:.zomp=.check)

build: $(WORKING:.zomp=.bc)

# FUNCTION_COUNTS=10 100 1000 1200 1400 1600 1800 2000 2200 2400
FUNCTION_COUNTS=10 100 1000 4000

# LLVM_GCC_BIN_DIR=$(PWD)/../tools/llvm-gcc/bin
# LLVM_BIN_DIR=$(PWD)/../tools/llvm/Release/bin
# PATH:=$(LLVM_BIN_DIR):$(LLVM_GCC_BIN_DIR):$(PATH)

PERFTEST_GEN=genperftest

compileperftest:
	@$(RM) -f timing.txt
	@for funcs in $(FUNCTION_COUNTS); \
	do \
	  rm -f compileperf.zomp; \
	  $(OCAML) $(PERFTEST_GEN).ml $$funcs compileperf.zomp; \
	  ./logtiming.sh timing.txt $$funcs '../zomp-check-syntax.sh compileperf.zomp'; \
	done

GLAPP_LIBS = -framework Carbon -framework CoreFoundation -framework OpenGL -framework AGL ../libglfw.a ../libGLEW.a

gltest_llvm.exe: gltest.bc
	@$(ECHO) Building $@ ...
	$(LLVM_LD) -native -o $@ ../stdlib.bc $<

gltest.exe: gltest_converted.c
	@$(ECHO) Building $@ ...
	$(CXX) -c -g -o gltest_converted.o gltest_converted.c
	$(CXX) -g -o gltest.exe $(GLAPP_LIBS) gltest_converted.o
# 	$(CXX) -o gltest.exe -g $(GLAPP_LIBS) gltest_converted.c


stdlibfuncs_exe.bc: stdlibfuncs.bc
	@$(LLVM_LD) ../stdlib.bc $< -o ${@:.bc=}

clean_tests:
	@$(ECHO) Cleaning tests ...
	$(RM) -f *.ll *.bc *.exe
	$(RM) -f compileperf.*

%.cbe.c: %.bc
	@$(ECHO) Generating $@ ...
	$(LLC) -march=c -f $<

%.check: %.zomp
	@$(ECHO) Checking $<
	$(RM) -f ${<:.zomp=.ll}
	../zomp-check-syntax.sh $<

# %.run: %.zomp
# 	@$(RM) -f ${<:.zomp=.ll} ${<:.zomp=.bc}
# 	$(MAKE) $(<:.zomp=.bc)
# 	@$(ECHO) Running $(<:.zomp=)
# 	$(LLI) $(<:.zomp=.bc)
# 	@$(ECHO) Done

%.ll: %.zomp ../zompc.native ../prelude.zomp
	@$(ECHO) Compiling $< to LLVM IR
	../zomp-check-syntax.sh $<

%.bc: %.ll ../runtime.c
	@$(ECHO) Compiling $< to $@
	$(LLVM_AS) -f $< -o $(<:.ll=.o.bc)
	$(LLVM_LINK) -f ../runtime.bc $(<:.ll=.o.bc) -o $@

%.exe: %.bc
	@$(ECHO) Building $@ ...
	$(LLVM_LD) $< -native -o $@

%.run: %.bc
	@$(ECHO) Running $@ ...
	$(LLVM_LLI) $<

%.s: %.bc
	$(LLC) $< -f -o $@

%.o: %.s
	$(AS) $< -o $@

%.glexe: %.o
	$(CXX) -o $@ -lstdc++ $(GLAPP_LIBS) $<

# glanim.glexe: glanim.o
# 	@g++ -o $@ -lstdc++ $(GLAPP_LIBS) glanim.o

