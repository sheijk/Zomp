#
# Helpers to diagnose build issues.
#

help:
	@$(ECHO) "Please use 'make all' and/or 'make test'"
	exit 1

debug:
	@$(ECHO) "BUILD_VARIANT = " $(BUILD_VARIANT)
	@$(ECHO) "PATH = "
	@$(ECHO) "$(PATH)" | $(TR) : \\n
	@$(ECHO) "SHELL = " $(SHELL)
	@$(ECHO) "LLVM_BIN_DIR = $(LLVM_BIN_DIR)"
	@$(ECHO) "LLVM_CONFIG = $(LLVM_CONFIG)"
	@$(ECHO) "CC = $(CC)"
	@$(ECHO) "CXX = $(CXX)"
	@$(ECHO) "CCFLAGS = " $(CCFLAGS)
	@$(ECHO) "CXXFLAGS = '"$(CXXFLAGS)"'"
	@$(ECHO) "LDFLAGS = " $(LDFLAGS)
	@$(ECHO) "BUILD_PLATFORM = $(BUILD_PLATFORM)"
	@$(ECHO) "LLVM_EXTRA_OPTIONS = $(LLVM_EXTRA_OPTIONS)"
	@$(ECHO) "ZOMP_MAIN_MAKEFILE = $(ZOMP_MAIN_MAKEFILE)"
ifneq "$(PRINT_VAR)" ""
	@$(ECHO) "$(PRINT_VAR) = '$($(PRINT_VAR))'"
else
	@$(ECHO) "Use PRINT_VAR=foo to print foo"
endif


# This will create a script to setup the environment as it is used for this build.
# Use this for debugging or if you want to make sure you're using the same tools
# as the make process when looking up help, etc.
setenv: $(OUT_DIR)/env.sh

ALL_TARGETS += $(OUT_DIR)/env.sh
$(OUT_DIR)/env.sh: makefile
	@$(ECHO) "Creating $@ ..."
	echo '#!/usr/bin/env sh' > $@
	echo "# source this script to setup the environment for this build"
	echo "PATH=$(PATH)" >> $@

