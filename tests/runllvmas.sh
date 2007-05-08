#!/bin/sh

echo foobar
llvm-as -f $1 -o /tmp/flymake_dummy.bc
#echo lala
# set >> flymake-log

