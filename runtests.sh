#!/bin/bash

cd tests/

for f in *.zomp
do
    echo
    echo Checking ${f}
    echo
    ../zomp-check-syntax.sh ${f}
done

