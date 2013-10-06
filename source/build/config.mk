#
# Setting up paths and options for most tools
#

ARCH=i386

ifeq "$(ARCH)" "i386"
ARCHFLAG = -m32
else
  ifeq "$(ARCH)" "x86_64"
    ARCHFLAG = -m64
  else
    $(error "Only i386 and x86_64 architectures supported")
  endif
endif

ZOMP_TOOL_PATH_RELATIVE = ./tools/arch-$(ARCH)
ZOMP_TOOL_PATH=$(ZOMP_DIR)/$(ZOMP_TOOL_PATH_RELATIVE)
LLVM_VERSION=2.9

ifeq "$(CAML_BYTE_CODE)" "1"
  BUILD_ARCH_CAML=-caml_bc
else
  BUILD_ARCH_CAML=
endif

ifeq "$(DEBUG)" "1"
  BUILD_VARIANT = debug-$(ARCH)$(BUILD_ARCH_CAML)
else
  ifeq "$(DEBUG)" "0"
  BUILD_VARIANT = release-$(ARCH)$(BUILD_ARCH_CAML)
  else
    $(error Please define DEBUG to be either 0 or 1)
  endif
endif

################################################################################
# OCaml binaries
################################################################################

OCAML_BIN_POSTFIX = .opt

OCAMLPATH =
OCAMLLEX = $(OCAMLPATH)ocamllex$(OCAML_BIN_POSTFIX)
OCAMLYACC = $(OCAMLPATH)ocamlyacc
MENHIR = $(OCAMLPATH)menhir
OCAML = $(OCAMLPATH)ocaml
OCAMLRUN = $(OCAMLPATH)ocamlrun
OCAMLC = $(OCAMLPATH)ocamlc$(OCAML_BIN_POSTFIX) -dtypes -warn-error A
OCAMLOPT = $(OCAMLPATH)ocamlopt$(OCAML_BIN_POSTFIX) -dtypes -warn-error A
OCAMLMKLIB = $(OCAMLPATH)ocamlmklib
OCAMLDEP = $(OCAMLPATH)ocamldep$(OCAML_BIN_POSTFIX)
OCAMLDOC = $(OCAMLPATH)ocamldoc$(OCAML_BIN_POSTFIX)

# OCaml tags program (currently only works with OCaml 3.9 and thus is probably
# not present anymore)
OTAGS = otags

ifeq "$(CAML_BYTE_CODE)" "0"
CAML_OBJ_EXT=cmx
CAML_LIB_EXT=cmxa
else
CAML_OBJ_EXT=cmo
CAML_LIB_EXT=cma
endif

################################################################################
# LLVM binaries
################################################################################

ifeq "$(DEBUG)" "1"
LLVM_VARIANT = Debug
else
LLVM_VARIANT = Release
endif

LLVM_BASE_DIR = $(ZOMP_TOOL_PATH)/sources/llvm-$(LLVM_VERSION)
LLVM_BIN_DIR = $(LLVM_BASE_DIR)/$(LLVM_VARIANT)/bin
LLVM_INCLUDE_DIR = $(LLVM_BASE_DIR)/include
LLVM_LIB_DIR = $(LLVM_BASE_DIR)/$(LLVM_VARIANT)/lib

LLVM_CONFIG = $(LLVM_BIN_DIR)/llvm-config
LLVM_DIS = $(LLVM_BIN_DIR)/llvm-dis
LLVM_AS = $(LLVM_BIN_DIR)/llvm-as
LLVM_LLI = $(LLVM_BIN_DIR)/lli
LLVM_LD = $(LLVM_BIN_DIR)/llvm-ld
LLVM_LLVMC = $(LLVM_BIN_DIR)/llvmc
LLVM_LLC = $(LLVM_BIN_DIR)/llc
LLVM_OPT = $(LLVM_BIN_DIR)/opt

CLANG_BASE_DIR = $(LLVM_BASE_DIR)/tools/clang
CLANG_INCLUDE_DIR = $(CLANG_BASE_DIR)/include

CLANG = $(LLVM_BASE_DIR)/Release/bin/clang
CLANGXX = $(LLVM_BASE_DIR)/Release/bin/clang++

################################################################################
# Shell tool binaries
################################################################################

UPDATE = cp
CP = cp
SED = sed
ECHO = echo
CAT = cat
CP = cp
RM = rm
DELETE_FILE = $(RM) -f
SED = sed
# TR = tc
TR = tr
LINE_COUNT = wc -l
SORT = sort
LS = ls
WHICH = which
FILE = file
TOUCH = touch
PERL = perl

# image magick
CONVERT_IMAGE = convert

################################################################################
# Native tool chain (C/C++/Assembler/Linker/...) binaries
################################################################################

CC = $(CLANG)
CXX = $(CLANGXX)
AS = as
LD = ld

################################################################################
# Zomp binaries
################################################################################

ifeq "$(PROFILE_ZOMPC)" ""
  RUN_W_PROFILE =
else
  RUN_W_PROFILE = /usr/bin/time
endif

ifeq "$(CAML_BYTE_CODE)" "1"
  ZOMPC = $(RUN_W_PROFILE) OCAMLRUNPARAM=b $(ZOMP_DIR)/$(ZOMPC_FILE)
else
  ZOMPC = $(RUN_W_PROFILE) $(ZOMP_DIR)/$(ZOMPC_FILE)
endif

ZOMPSH = $(DEPLOY_DIR)/zompsh

################################################################################
# Build flags
################################################################################

CAML_INCLUDE = -I source/ -I testsuite/
CAML_PP =

CAML_LIBS =
CAML_OBJS =
CAML_DEPENDENCIES = $(foreach obj, $(CAML_OBJS), $(obj).$(CAML_OBJ_EXT))
CAML_LINK_FLAGS = $(foreach lib, $(CAML_LIBS), $(lib).$(CAML_LIB_EXT)) $(CAML_DEPENDENCIES)
CAML_FLAGS = $(CAML_INCLUDE) $(CAML_PP)
CAML_NATIVE_FLAGS = $(CAML_INCLUDE) $(CAML_PP)

LLVM_EXTRA_OPTIONS = "$(ARCHFLAG)"

BUILD_PLATFORM := $(shell 'uname')

ifeq "$(BUILD_PLATFORM)" "Linux"
DLL_FLAG = -shared
LLVM_EXTRA_OPTIONS += -fPIC -DPIC
CAML_NATIVE_FLAGS += -fPIC
else # os x
DLL_FLAG = -dynamiclib
endif

# setting up link flags for libraries
ifeq "$(BUILD_PLATFORM)" "Linux"
LDFLAGS += -L/usr/lib64 -L/usr/local/lib64
LDFLAGS += -fPIC
CCFLAGS += -fPIC
CXXFLAGS += -fPIC
endif

CXXFLAGS += -I /usr/local/lib/ocaml/
# OCaml deploys headers into lib dir
CXXFLAGS += -I $(ZOMP_TOOL_PATH)/lib/ocaml/
CCFLAGS += -I $(ZOMP_TOOL_PATH)/lib/ocaml/
LDFLAGS += -L$(ZOMP_TOOL_PATH)/lib/
ZOMPCFLAGS += --dll-dir $(ZOMP_TOOL_PATH)/lib/

MENHIR_FLAGS += --stdlib $(ZOMP_TOOL_PATH)/share/menhir

################################################################################
# Libraries
################################################################################

ifeq "$(BUILD_PLATFORM)" "Linux"
DLL_EXTENSION = so
LINK_GLUT = -lglut
LINK_GL = -lGL
else
DLL_EXTENSION = dylib
LINK_GLUT = -framework GLUT
LINK_GL = -framework OpenGL
endif

LDFLAGS += -Llibs -Ltools/arch-$(ARCH)/external/libs

LINK_QUICKTEXT = libs/libquicktext.$(DLL_EXTENSION)
LINK_ANTTWEAKBAR = $(ZOMP_TOOL_PATH)/lib/libAntTweakBar.$(DLL_EXTENSION)
LINK_ASSIMP = $(ZOMP_TOOL_PATH)/lib/libassimp.a
LINK_CPPSTDLIB = -lstdc++
LINK_GLEW = $(ZOMP_TOOL_PATH)/lib/libGLEW.$(DLL_EXTENSION)
LINK_GLFW = $(ZOMP_TOOL_PATH)/lib/libglfw.$(DLL_EXTENSION)
LINK_UTILS = libs/libutils.$(DLL_EXTENSION)

