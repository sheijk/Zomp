#!/usr/bin/env bash
# Checking dependencies for ml files. For each target doing a clean and then
# only building that target with high parallel settings. Will not be able to
# find all issues but works as a kind of sanity check.
#
# Run this from Zomp root dir.

set -e

for mlfile in source/*.ml testsuite/*.ml
do
    TARGET=${mlfile/.ml/.cmx}
    OUT=build/check_deps/${TARGET}
    mkdir -p $(dirname ${OUT})

    echo "Building only ${OUT} ..."
    (make -s clean_all 2>&1) > ${OUT}.clean
    ((make -j32 ${TARGET} 2>&1) > ${OUT}.make) || echo "Failed to build ${OUT}, \$? = $?"
done

