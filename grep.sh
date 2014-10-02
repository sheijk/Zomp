#!/usr/bin/env sh

BASE_DIR=`dirname $0`
find ${BASE_DIR} \( -iname .git -or -ipath ${BASE_DIR}/ci_archive -or -ipath ${BASE_DIR}/data -or -ipath ${BASE_DIR}/tools -or -ipath ${BASE_DIR}/build \) -prune -or -type f -exec grep -E -nH "$@" {} +

