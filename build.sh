#!/usr/bin/env sh

# A simple build front end. Use this to run make -s and still get a list of all
# commands in buildlog.txt for inspection on errors. Just run it with the same
# arguments as make. Make will always be invoked from the Zomp root dir

BUILDLOG=buildlog.txt

cd `dirname $0`
rm -f ${BUILDLOG}
LOGSHELL=`pwd`/source/build/logshell.sh
echo "LOGSHELL = ${LOGSHELL}"
echo "Starting build at `date '+%Y-%m-%d %H:%M:%S'` with params '$@'" >> ${BUILDLOG}

make $@ SHELL="${LOGSHELL} ${BUILDLOG}"
RETVAL=$?

echo "Auto update test report ..." >> ${BUILDLOG}
make SILENT=1 report.html >> ${BUILDLOG}

date "+Finishing build at %Y-%m-%d %H:%M:%S with ${RETVAL}" >> ${BUILDLOG}
exit ${RETVAL}

