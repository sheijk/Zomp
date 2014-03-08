#!/usr/bin/env sh
#
# Helper script used by makefile to compiler GLEW for Zomp.
#

set -uE

cd glew-1.9.0

PREFIX_PLACE_HOLDER_PATH=at_executable_path
make GLEW_DEST="${PREFIX_PLACE_HOLDER_PATH}" all install
cd lib
for dll in *.dylib; do
    install_name_tool -id "@executable_path/${dll}" ${dll}
done
cp *.dylib ../../../lib

