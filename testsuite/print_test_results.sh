#!/usr/bin/env sh

TEST_CASE=$1

RESULT="unknown"

if [ -e ${TEST_CASE}.test ];
then
	RESULT="ok"
elif [ -e ${TEST_CASE}.bc ];
then
	RESULT="could not execute"
else
	RESULT="failed to compile"
fi

echo "  ${TEST_CASE} = ${RESULT}"

