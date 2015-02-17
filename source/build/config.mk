#
# Setting up paths and options for most tools
#

ARCH := $(shell uname -m)

# Set defaults for known architectures
ifeq "$(ARCH)" "i386"
  ARCHFLAG = -m32
  LLVM_ARCH = x86
else ifeq "$(ARCH)" "x86_64"
  ARCHFLAG = -m64
  LLVM_ARCH = x86-64
else
  $(error "Value of ARCH flag '$(ARCH)' is invalid")
endif

ifeq "$(ARCHFLAG)" ""
$(error "ARCHFLAG must be set")
endif

ifeq "$(LLVM_ARCH)" ""
$(error "LLVM_ARCH must be set")
endif

TOOL_SUFFIX=
ZOMP_TOOL_PATH_RELATIVE = ./tools/arch-$(ARCH)$(TOOL_SUFFIX)
ZOMP_TOOL_PATH=$(CURDIR)/$(ZOMP_TOOL_PATH_RELATIVE)
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

RUN_AND_LOG = ./source/build/run_and_log.sh

################################################################################
# OCaml binaries
################################################################################

OCAMLFIND = ocamlfind
MENHIR = menhir
OCAML = ocaml
OCAMLRUN = ocamlrun
OCAMLC = $(OCAMLFIND) ocamlc
OCAMLOPT = $(OCAMLFIND) ocamlopt
OCAMLMKLIB = $(OCAMLFIND) ocamlmklib
OCAMLDEP = $(OCAMLFIND) ocamldep
OCAMLDOC = $(OCAMLFIND) ocamldoc

ifeq "$(CAML_BYTE_CODE)" "0"
CAML_OBJ_EXT=cmx
else
CAML_OBJ_EXT=cmo
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
DELETE_DIR = $(RM) -rdf
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
  ZOMPC = $(RUN_W_PROFILE) OCAMLRUNPARAM=b $(CURDIR)/$(ZOMPC_FILE)
else
  ZOMPC = $(RUN_W_PROFILE) $(CURDIR)/$(ZOMPC_FILE)
endif

ZOMPSH = $(ZOMPSH_FILE)

################################################################################
# Build flags
################################################################################

# Can be overridden for individual targets.
CAML_LIBS =
CAML_OBJS =
CAML_DEPENDENCIES = $(foreach obj, $(CAML_OBJS), $(obj).$(CAML_OBJ_EXT))
CAML_LINK_FLAGS = $(foreach lib, $(CAML_LIBS), -package $(lib)) $(CAML_DEPENDENCIES)

CAML_INCLUDE = -I source/ -I testsuite/
CAML_COMMON = $(CAML_INCLUDE) -warn-error A-3
CAMLC_FLAGS = -annot $(CAML_COMMON)
CAMLOPT_FLAGS = -annot $(CAML_COMMON)
CAMLDOC_FLAGS = $(CAML_INCLUDE)
CAMLDEP_FLAGS = $(CAML_INCLUDE) -dot-include-all -dot-reduce

OCAML_3_DOT_X := $(shell $(OCAMLOPT) -version | grep ^3)
ifeq "$(OCAML_3_DOT_X)" ""
CAMLC_FLAGS += -bin-annot
CAMLOPT_FLAGS += -bin-annot
endif

LLVM_EXTRA_OPTIONS = "$(ARCHFLAG)"

BUILD_PLATFORM := $(shell 'uname')

ifeq "$(BUILD_PLATFORM)" "Linux"
DLL_FLAG = -shared
LLVM_EXTRA_OPTIONS += -fPIC -DPIC
CAMLOPT_FLAGS += -fPIC
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

# Fix to use clang 2.9 on OS X 10.10
CXXFLAGS += -mmacosx-version-min=10.5
CCFLAGS += -mmacosx-version-min=10.5
LDFLAGS += -mmacosx-version-min=10.5

CXXFLAGS += -I /usr/local/lib/ocaml/
# OCaml deploys headers into lib dir
CXXFLAGS += -I $(ZOMP_TOOL_PATH)/lib/ocaml/
CCFLAGS += -I $(ZOMP_TOOL_PATH)/lib/ocaml/
LDFLAGS += -L$(ZOMP_TOOL_PATH)/lib/
ZOMPCFLAGS += --dll-dir $(ZOMP_TOOL_PATH)/lib/

MENHIR_FLAGS += --stdlib $(ZOMP_TOOL_PATH)/share/menhir

ZOMPCFLAGS += --dll-dir /usr/lib
ZOMPCFLAGS += --dll-dir /usr/local/lib

ZOMPCFLAGS += --dll-dir ./libs
LDFLAGS += -Llibs

CXXFLAGS += -I $(CLANG_INCLUDE_DIR) -I $(LLVM_INCLUDE_DIR) -L$(LLVM_LIB_DIR) $(ARCHFLAG)
CCFLAGS += -std=c89 -I /usr/local/lib/ocaml/ $(ARCHFLAG)
LDFLAGS += $(ARCHFLAG) -L $(LLVM_LIB_DIR)

ifeq "$(DEBUG)" "1"
  OCAMLC += -g
  CAMLC_FLAGS += -g
  CAMLOPT_FLAGS += -g -ccopt -g
  CXXFLAGS += -pg -g -DDEBUG
  CCFLAGS += -pg -g -DDEBUG
  LDFLAGS += -g
else
  ifeq "$(DEBUG)" "0"
    CXXFLAGS += -O3
  else
    $(error DEBUG flag has to either 0 or 1)
  endif
endif

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

LINK_QUICKTEXT = -lquicktext
LINK_ANTTWEAKBAR = -lAntTweakBar
LINK_ASSIMP = -lassimp
LINK_CPPSTDLIB = -lstdc++
LINK_GLEW = -lGLEW
LINK_GLFW = -lglfw
LINK_UTILS = -lutils

