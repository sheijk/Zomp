##===- examples/HowToUseJIT/Makefile -----------------------*- Makefile -*-===##
# 
#                     The LLVM Compiler Infrastructure
#
# This file was developed by Valery A. Khamenya and is distributed under
# the University of Illinois Open Source License. See LICENSE.TXT for details.
# 
##===----------------------------------------------------------------------===##

LINK_COMPONENTS := jit interpreter native

jittest: HowToUseJit.cpp 
	g++ -o jittest HowToUseJit.cpp `llvm-config --cxxflags` `llvm-config --libs $(LINK_COMPONENTS)` -Wall

