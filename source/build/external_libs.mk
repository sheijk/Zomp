################################################################################
# External libraries
################################################################################

libs/libglut.dylib:
	@$(ECHO) Building $@ ...
	$(CC) $(DLL_FLAG) $(LDFLAGS) $(LINK_GLUT) -o $@

libs/libquicktext.dylib: libs/glQuickText.o
	@$(ECHO) Building $@ ...
	$(CXX) $(DLL_FLAG) $(LDFLAGS) -o $@ libs/glQuickText.o $(LINK_GL)

libs/libutils.dylib: libs/libutils.cpp
	@$(ECHO) Building $@ ...
	$(CXX) $(DLL_FLAG) $(LDFLAGS) $< -o $@

libs/stb_image.dylib: libs/stb_image.c libs/stb_image.h
	@$(ECHO) Building $@ ...
	$(CXX) $(DLL_FLAG) $(LDFLAGS) $< -o $@

EXTLIB_DIR = extlibs
ASSIMP_DIR = $(EXTLIB_DIR)/assimp-svn

SCONS = $(PWD)/extlibs/scons/scons.py
ifeq "$(DEBUG)" "1"
SCONSFLAGS += "debug=1"
endif

extlib_assimp:
	@$(ECHO) Building assimp library ...
	cd $(ASSIMP_DIR)/workspaces/SCons; $(SCONS) $(SCONSFLAGS)

libassimp.a: $(ASSIMP_DIR)/workspaces/SCons/libassimp.a makefile
	- rm -f $@ # prefix '-' means this rule is allowed to fail
	ln -s $< $@

assimp.dylib: libassimp.a makefile libs/forcelinkassimp.c
	@$(ECHO) Building $@ ...
	$(CXX) $(DLL_FLAG) $(LDFLAGS) -o $@ -I $(ASSIMP_DIR)/include -L. -lassimp libs/forcelinkassimp.c

%.zomp: %.skel source/gen_c_bindings
	@$(ECHO) Generating Zomp bindings for $(<:.skel=) ...
	./source/gen_c_bindings -lang zomp $(<:.skel=)

libs/opengl20print.zomp: libs/opengl20.skel source/gen_c_bindings
	@$(ECHO) Generating OpenGL enum printer ...
	$(CP) libs/opengl20.skel libs/opengl20print.skel
	./source/gen_c_bindings -lang zomp-glprinter $(@:.zomp=)
	$(RM) -f libs/opengl20print.skel

# The install names of external libs are expected to be relative to the
# executable using @executable_path so we need to add links to them into all
# directories that will contain binaries requiring those libs.
EXTERNAL_LIB_LINK_NAMES = glfw GLEW GLEW.1.9 GLEW.1.9.0
EXTERNAL_LIB_TARGET_DIRS = examples testsuite/std libs
EXTERNAL_LIB_LINKS = $(foreach target_dir, $(EXTERNAL_LIB_TARGET_DIRS), \
  $(foreach lib, $(EXTERNAL_LIB_LINK_NAMES), $(target_dir)/lib$(lib).dylib))

.PHONY: external_lib_links
ALL_TARGETS += external_lib_links
external_lib_links:
	./source/build/make_lib_links.sh $(ZOMP_TOOL_PATH) "$(EXTERNAL_LIB_LINK_NAMES)" "$(EXTERNAL_LIB_TARGET_DIRS)"

CLEAN_SUB_TARGETS += clean_external_lib_links
.PHONY: clean_external_lib_links
clean_external_lib_links:
	rm -f $(EXTERNAL_LIB_LINKS)

