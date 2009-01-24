#!/usr/bin/env sh

FILENAME=$1
TEXT=$2
COMMAND=$3

BUILD_LOG=compileperf_buildlog.txt

DOLLAR1='$1'

USED_TIME=` (/usr/bin/time $COMMAND > ${BUILD_LOG}) 2>&1 | awk "{ print ${DOLLAR1} }"`

echo "${USED_TIME}s"
echo "${TEXT} ${USED_TIME}" >> $FILENAME

