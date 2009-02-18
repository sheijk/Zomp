
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

LLVM_BIN_DIR=$(PWD)/tools/llvm/Release/bin
LLVM_INCLUDE_DIR=$(PWD)/tools/llvm/include
LLVM_LIB_DIR=$(PWD)/tools/llvm/Release/lib
LLVM_GCC_BIN_DIR=$(PWD)/tools/llvm-gcc/bin

LLVM_CONFIG=$(LLVM_BIN_DIR)/llvm-config
LLVM_CC=$(LLVM_GCC_BIN_DIR)/llvm-gcc
LLVM_CXX=$(LLVM_GCC_BIN_DIR)/llvm-g++
LLVM_DIS=$(LLVM_BIN_DIR)/llvm-dis

