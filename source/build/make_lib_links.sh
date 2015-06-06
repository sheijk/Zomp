#!/usr/bin/env sh

set -e # fail on errors
set -u # abort when using undefined variable

ZOMP_TOOL_PATH=$1
EXTERNAL_LIB_LINK_NAMES="$2"
EXTERNAL_LIB_TARGET_DIRS="$3"

for DIR in ${EXTERNAL_LIB_TARGET_DIRS}; do
    for NAME in ${EXTERNAL_LIB_LINK_NAMES}; do
        LINK="${DIR}/lib${NAME}.dylib"
        FILE="${ZOMP_TOOL_PATH}/lib/lib${NAME}.dylib"
        if [ -e "${FILE}" ]; then
            rm -f "${LINK}"
            ln -s "${FILE}" "${LINK}"
        fi
    done
done

