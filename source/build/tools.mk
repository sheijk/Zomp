
################################################################################
# LLVM download and compilation
################################################################################

LLVM_INSTALL_HELP = "error: You do not have LLVM and clang installed. Please \
run make tools/llvm-$(LLVM_VERSION) to download and build them (requires an \
internet connection). Note that at least on Mac OS X you cannot use the \
prebuilt libraries as they are 64-bit"

tools/clang-$(LLVM_VERSION).tgz:
	@$(ECHO) Downloading $@ ...
	mkdir -p tools
	curl "http://llvm.org/releases/$(LLVM_VERSION)/clang-$(LLVM_VERSION).tgz" -o $@ -s -S

tools/llvm-$(LLVM_VERSION).tgz:
	@$(ECHO) Downloading $@ ...
	mkdir -p tools
	curl "http://llvm.org/releases/$(LLVM_VERSION)/llvm-$(LLVM_VERSION).tgz" -o $@ -s -S

tools/llvm-$(LLVM_VERSION): tools/llvm-$(LLVM_VERSION).tgz tools/clang-$(LLVM_VERSION).tgz
	@$(ECHO) Unpacking LLVM and clang ...
	cd tools && gunzip --stdout llvm-$(LLVM_VERSION).tgz | tar -xf - -C $(dir $(LLVM_BASE_DIR))
	cd tools && gunzip --stdout clang-$(LLVM_VERSION).tgz | tar -xf - -C $(LLVM_BASE_DIR)/tools
	mv $(LLVM_BASE_DIR)/tools/clang-$(LLVM_VERSION) $(LLVM_BASE_DIR)/tools/clang
	$(TOUCH) $@ # tar sets date from archive. avoid downloading the archive twice
	@$(ECHO) Configuring LLVM $(LLVM_VERSION) and clang ...
	cd $(LLVM_BASE_DIR) && ./configure EXTRA_OPTIONS="$(LLVM_EXTRA_OPTIONS)"
	@$(ECHO) Building LLVM $(LLVM_VERSION) and clang ...
	cd $(LLVM_BASE_DIR) && (make EXTRA_OPTIONS="$(LLVM_EXTRA_OPTIONS)"; make ENABLE_OPTIMIZED=0 EXTRA_OPTIONS="$(LLVM_EXTRA_OPTIONS)")

tools/llvm-$(LLVM_VERSION)/TAGS:
	@$(ECHO) Building tags for LLVM $(LLVM_VERSION)
	cd tools/llvm-$(LLVM_VERSION)/ && find -E lib include -regex ".*\.(cpp|h)" | xargs etags -o TAGS

################################################################################
# Rules
################################################################################

.PRECIOUS: tools/arch-$(ARCH)/sources/%/.exists

tools/%.tgz: tools/%.tar.gz
	cp $< $@

tools/arch-$(ARCH)/sources/%/.exists: tools/%.tgz
	@$(ECHO) "Extracting $< ..."
	rm -rf $(dir $@)
	mkdir -p "$(dir $@)"
	gunzip --stdout "$(<)" | tar -xf - -C "$(dir $@)"/..
	touch $@

tools/arch-$(ARCH)/sources/%/.exists: tools/%.zip
	@$(ECHO) "Extracting $< ..."
	rm -rf $(dir $@)
	mkdir -p "$(dir $@)"
	unzip -q $< -d "$(dir $@)"

tools/arch-$(ARCH)/sources/%/.built: tools/arch-$(ARCH)/sources/%/.exists
	@$(ECHO) "Building $(dir $@) ..."
	cd tools/arch-$(ARCH)/sources && ARCHFLAG=$(ARCHFLAG) ../../../source/build/build_extlib_$(*).sh
	touch $@

################################################################################
# External libraries
################################################################################

GLEW_VERSION = glew-1.9.0
EXTERNAL_TOOLS += tools/arch-$(ARCH)/sources/$(GLEW_VERSION)/.built

GLFW_VERSION = glfw-legacy-2.7.9
EXTERNAL_TOOLS += tools/arch-$(ARCH)/sources/$(GLFW_VERSION)/.built

ANTTWEAKBAR_VERSION = AntTweakBar_116
EXTERNAL_TOOLS += tools/arch-$(ARCH)/sources/$(ANTTWEAKBAR_VERSION)/.built

external_tools: $(EXTERNAL_TOOLS)

