#!/usr/bin/env sh

# A simple build front end. Use this to run make -s and still get a list of all
# commands in buildlog.txt for inspection on errors. Just run it with the same
# arguments as make. Make will always be invoked from the Zomp root dir

BUILDLOG=buildlog.txt

cd `dirname $0`
rm -f ${BUILDLOG}
LOGSHELL=`pwd`/logshell.sh
echo "LOGSHELL = ${LOGSHELL}"
date '+Starting build at %Y-%m-%d %H:%M:%S' >> ${BUILDLOG}
make $@ SHELL="${LOGSHELL} ${BUILDLOG}"
date '+Finishing build at %Y-%m-%d %H:%M:%S' >> ${BUILDLOG}
exit $?

