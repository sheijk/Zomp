
include ../makefile.config

clean:
	rm -f *.bc *.ll

################################################################################
# Rules
################################################################################

.PRECIOUS: %.ll

%.ll: %.zomp ../zompc.native
	@$(ECHO) Compiling $(<) to .ll...
	@../zomp-check-syntax.sh $<

%.bc: %.ll ../stdlib.zomp ../stdlib.c
	@echo Compiling $< to $@
	@llvm-as -f $< -o $(<:.ll=.o.bc)
	@llvm-link -f ../stdlib.bc $(<:.ll=.o.bc) -o $@

