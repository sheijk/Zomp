#!/usr/bin/env sh

# This script acts as a shell replacement for make. It will log the command and
# then pass it on to sh. It can be used to run 'make -s' and still get a list of
# all commands which where executed inside a log file. See build.sh

LOGFILE=$1
shift
LOGFILE_DIR=`dirname ${LOGFILE}`
mkdir -p ${LOGFILE_DIR}

if [ "$1" != "-c" ]; then
    echo "error: expected $0 file.txt -c ... instead of $@"
    exit 1
fi
shift

echo "$@" >> ${LOGFILE}
/bin/sh -c "$@"
exit $?

