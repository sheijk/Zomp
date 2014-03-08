#!/usr/bin/env sh
#
# Helper script used by makefile to compiler GLFW for Zomp.
#

set -uE

cd glfw-legacy-2.7.9
CC=clang make cocoa CLAGS+=${ARCHFLAG}

cp lib/cocoa/libglfw.dylib ../../lib

