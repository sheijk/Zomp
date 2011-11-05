

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

################################################################################
# LLVM binaries
################################################################################

ifeq "$(DEBUG)" "1"
LLVM_VARIANT = Debug
else
LLVM_VARIANT = Release
endif

LLVM_BASE_DIR = $(ZOMP_TOOL_PATH)/llvm-$(LLVM_VERSION)
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
SED = sed
# TR = tc
TR = tr
LINE_COUNT = wc -l
SORT = sort
LS = ls
WHICH = which
TOUCH = touch

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
ZOMPC = ./zompc.native
else
ZOMPC = /usr/bin/time ./zompc.native
endif

################################################################################
# Build flags
################################################################################

CAML_INCLUDE = -I source/
CAML_PP =

CAML_FLAGS = $(CAML_INCLUDE) $(CAML_PP)
CAML_NATIVE_FLAGS = $(CAML_INCLUDE) $(CAML_PP) -p

ARCHFLAG = -m32

LLVM_EXTRA_OPTIONS = "$(ARCHFLAG)"

BUILD_PLATFORM = $(shell 'uname')

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

LINK_QUICKTEXT = libquicktext.$(DLL_EXTENSION)
LINK_ANTTWEAKBAR = libAntTweakBar.$(DLL_EXTENSION)
LINK_ASSIMP = libassimp.a
LINK_CPPSTDLIB = -lstdc++
LINK_GLEW = libGLEW.$(DLL_EXTENSION)
LINK_GLFW = libglfw.$(DLL_EXTENSION)
LINK_UTILS = libutils.$(DLL_EXTENSION)

