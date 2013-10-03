#!/usr/bin/env sh

DIR=$1
cd ${DIR}

SUM=testsuite/summary.txt

function count {
    grep $1 ${SUM} | wc -l | sed 's/ //g'
}

function report {
    PATTERN=$1

    TOTAL=`count "^${PATTERN}/"`
    SUCC=`count "^${PATTERN}/.*=succeeded$"`
    echo "${PATTERN} ${SUCC}/${TOTAL} \c"
}

echo "${DIR}: \c"
report testsuite
report libs
report examples
echo

