#!/usr/bin/env sh
#
# Use this to start auto running Zomp CI on every commit for 32-bit x86.  You
# need to clone the Zomp repo into a new dir, make sure the prerequisites are
# available (create a symlink to the tools directory of the main Zomp repo) and
# then run this script from the cloned directory.

if [ ! -d testsuite ]
then
    echo "error: you need to run this from the repository root"
    exit 1
fi

../Zomp/source/ci/watch.sh ../Zomp/source/ci/ci.sh -ks -j8 ARCH=i386

