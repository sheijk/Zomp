

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

# OCaml tags program (currently only works with OCaml 3.9 and thus is probably
# not present anymore)
OTAGS = otags

################################################################################
# LLVM binaries
################################################################################

LLVM_VARIANT = Debug

LLVM_BIN_DIR = $(ZOMP_TOOL_PATH)/llvm/$(LLVM_VARIANT)/bin
LLVM_INCLUDE_DIR = $(ZOMP_TOOL_PATH)/llvm/include
LLVM_LIB_DIR = $(ZOMP_TOOL_PATH)/llvm/$(LLVM_VARIANT)/lib
LLVM_GCC_BIN_DIR = $(ZOMP_TOOL_PATH)/llvm-gcc/bin
# LLVM_GCC_BIN_DIR = /usr/bin

LLVM_GCC = $(LLVM_GCC_BIN_DIR)/llvm-gcc
LLVM_GXX = $(LLVM_GCC_BIN_DIR)/llvm-g++

LLVM_CONFIG = $(LLVM_BIN_DIR)/llvm-config
LLVM_CC = $(LLVM_GCC_BIN_DIR)/llvm-gcc
LLVM_CXX = $(LLVM_GCC_BIN_DIR)/llvm-g++
LLVM_DIS = $(LLVM_BIN_DIR)/llvm-dis
LLVM_AS = $(LLVM_BIN_DIR)/llvm-as
LLVM_LINK = $(LLVM_BIN_DIR)/llvm-link
LLVM_LLI = $(LLVM_BIN_DIR)/lli
LLVM_LD = $(LLVM_BIN_DIR)/llvm-ld
LLVM_LLVMC = $(LLVM_BIN_DIR)/llvmc
LLVM_LLC = $(LLVM_BIN_DIR)/llc
LLVM_OPT = $(LLVM_BIN_DIR)/opt

################################################################################
# Shell tool binaries
################################################################################

UPDATE = cp
CP = cp
SED = sed
ECHO = echo
CP = cp
RM = rm
SED = sed
# TR = tc
TR = tr

################################################################################
# Native tool chain (C/C++/Assembler/Linker/...) binaries
################################################################################

CC = gcc
CXX = g++
AS = as
LD = ld

################################################################################
# Zomp binaries
################################################################################

ZOMPC = ./zompc.native

################################################################################
# Build flags
################################################################################

CAML_INCLUDE =
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
LINK_GLUT = -lglut
LINK_GL = -lGL
LDFLAGS += -L/usr/lib64 -L/usr/local/lib64
LDFLAGS += -fPIC
CCFLAGS += -fPIC
CXXFLAGS += -fPIC
else
LINK_GLUT = -framework GLUT
LINK_GL = -framework OpenGL
endif

