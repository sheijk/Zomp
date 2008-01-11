#!/usr/bin/env sh

ZOMP_PATH=/Users/sheijk/Documents/Development/Stuff/ocaml/lang/v3/

basename=${1%.zomp}
ocamlrun -I ${ZOMP_PATH} -b ${ZOMP_PATH}/zompc -c $1
# ${ZOMP_PATH}/zompc.native -c $1
RESULT=$?
if [ $RESULT != 0 ]; then
    echo Failed with $RESULT
    exit $RESULT
fi
llvm-as -f ${basename}.ll -o /tmp/zomp_dummy.bc

