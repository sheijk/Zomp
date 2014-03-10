#!/usr/bin/env bash
#
# Will run a command and log it's stdout to a file. First argument will be the
# file and all other arguments will form the command.

LOG_FILE=$1
shift

"$@" 2>&1 | tee ${LOG_FILE}

exit ${PIPESTATUS[0]}

