#!/usr/bin/env sh

# A simple build front end. Use this to run make -s and still get a list of all
# commands in buildlog.txt for inspection on errors. Just run it with the same
# arguments as make. Make will always be invoked from the Zomp root dir

BUILDLOG=build/buildlog.txt

OPTIONS=`echo "$@" | tr " " "\n" | grep "^[A-Za-z_][A-Za-z0-9_]*=" | tr "\n" " "`

cd `dirname $0`
rm -f ${BUILDLOG}
LOGSHELL=`pwd`/source/build/logshell.sh
echo "Starting build at `date '+%Y-%m-%d %H:%M:%S'` with params '$@', options = ${OPTIONS}" > ${BUILDLOG}

# Add --print-directory even though we're not using recursive make so Emacs (and
# other tools) shows correct source locations even when compilation command has
# not been triggered from this directory.
make --print-directory $@ SHELL="${LOGSHELL} ${BUILDLOG}"
RETVAL=$?

echo "Auto update test report ..." >> ${BUILDLOG}
make SILENT=1 ${OPTIONS} report >> ${BUILDLOG}

date "+Finishing build at %Y-%m-%d %H:%M:%S with ${RETVAL}" >> ${BUILDLOG}
exit ${RETVAL}

