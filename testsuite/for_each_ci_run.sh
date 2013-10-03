#!/usr/bin/env sh

# $* ci_archive/2013/10/03/17_28_36/build/release-i386

find ci_archive/20* -type d -iname release-i386 -exec $* {} \;

