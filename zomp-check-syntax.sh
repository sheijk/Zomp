#!/usr/bin/env sh

ZOMP_PATH=/Users/sheijk/Documents/Development/Stuff/ocaml/lang/v3/

basename=${1%.zomp}
ocamlrun -I ${ZOMP_PATH} -b ${ZOMP_PATH}/zompc -c $1
# ocamlrun -I ${ZOMP_PATH} -b ${ZOMP_PATH}/zompc < $1 > ${basename}.ll
llvm-as -f ${basename}.ll -o /tmp/zomp_dummy.bc

