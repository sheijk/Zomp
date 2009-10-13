
OCAMLPATH=
OCAMLLEX=$(OCAMLPATH)ocamllex.opt
OCAMLYACC=$(OCAMLPATH)ocamlyacc
MENHIR=$(OCAMLPATH)menhir
OCAML=$(OCAMLPATH)ocaml
OCAMLRUN=$(OCAMLPATH)ocamlrun
OCAMLC=$(OCAMLPATH)ocamlc.opt -dtypes -warn-error A
OCAMLOPT=$(OCAMLPATH)ocamlopt.opt -dtypes -warn-error A
OCAMLMKLIB=$(OCAMLPATH)ocamlmklib
OCAMLDEP=$(OCAMLPATH)ocamldep.opt

UPDATE=cp
CP=cp
SED=sed
ECHO=echo

LLVM_BIN_DIR=$(ZOMP_TOOL_PATH)/llvm/Release/bin
LLVM_INCLUDE_DIR=$(ZOMP_TOOL_PATH)/llvm/include
LLVM_LIB_DIR=$(ZOMP_TOOL_PATH)/llvm/Release/lib
LLVM_GCC_BIN_DIR=$(ZOMP_TOOL_PATH)/llvm-gcc/bin

LLVM_GCC=$(LLVM_GCC_BIN_DIR)/llvm-gcc
LLVM_GXX=$(LLVM_GCC_BIN_DIR)/llvm-g++

LLVM_CONFIG=$(LLVM_BIN_DIR)/llvm-config
LLVM_CC=$(LLVM_GCC_BIN_DIR)/llvm-gcc
LLVM_CXX=$(LLVM_GCC_BIN_DIR)/llvm-g++
LLVM_DIS=$(LLVM_BIN_DIR)/llvm-dis
LLVM_AS=$(LLVM_BIN_DIR)/llvm-as
LLVM_LINK=$(LLVM_BIN_DIR)/llvm-link
LLVM_LLI=$(LLVM_BIN_DIR)/lli
LLVM_LD=$(LLVM_BIN_DIR)/llvm-ld

ECHO=echo
CP=cp
RM=rm
SED=sed
TR=tc

CC=gcc
CXX=g++

