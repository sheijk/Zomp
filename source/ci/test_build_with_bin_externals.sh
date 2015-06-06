#!/usr/bin/env sh

# This script will tests whether installing binary versions of dependencies
# works

set -e # fail on errors
set -u # abort when using undefined variable

TARGET_DIR=ci_archive/$(date '+%Y/%m/%d/%H_%M_%S')
ORIGIN_DIR=$(pwd)

mkdir -p ${TARGET_DIR}
cd ${TARGET_DIR}
git clone ${ORIGIN_DIR} zomp
cd zomp
./build.sh -j8 -ks external_tools_bin_darwin
./build.sh -j8 -ks all
./build.sh -j8 -ks test

