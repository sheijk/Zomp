#!/usr/bin/env sh

# For each file.A in dir, if file.B exists prints "ok file", print "missing
# file" otherwise

EXT_A=$1
EXT_B=$2
DIR=$3

if [ "${DIR}" != "" ]; then
    cd $DIR
fi

for f in *.${EXT_A}; do
    BASENAME=`basename $f .${EXT_A}`
    OTHER=${BASENAME}.${EXT_B}
    if [ ! -e ${OTHER} ]; then
        echo "missing ${BASENAME}"
    else
        echo "ok ${BASENAME}"
    fi
done

