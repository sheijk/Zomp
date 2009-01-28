#!/usr/bin/env sh

ZOMP_PATH=/Users/sheijk/Documents/Development/Stuff/ocaml/lang/git

basename=${1%.zomp}
${ZOMP_PATH}/zompc.native -c $*
RESULT=$?
if [ $RESULT != 0 ]; then
    echo Failed with $RESULT
    exit $RESULT
fi
llvm-as -f ${basename}.ll -o /tmp/zomp_dummy.bc

