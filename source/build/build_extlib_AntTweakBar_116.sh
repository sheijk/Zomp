#!/usr/bin/env sh
#
# Use this script to compile AntTweakBar for Zomp and install into parent directory
#

set -uE

cd AntTweakBar_116/AntTweakBar/src
make -f Makefile.osx

install_name_tool -id "@executable_path/libAntTweakBar.dylib" AntTweakBar/lib/libAntTweakBar.dylib

cp AntTweakBar/lib/libAntTweakBar.dylib ../lib

