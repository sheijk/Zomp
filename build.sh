#!/usr/bin/env bash

# A simple build front end. Use this to run make -s and still get a list of all
# commands in buildlog.txt for inspection on errors. Just run it with the same
# arguments as make. Make will always be invoked from the Zomp root dir
#
# Also supports doing multiple make invocations.
# For example ./build.sh DEBUG=1 -k clean,all,test will invoke
# make DEBUG=1 -k clean; make DEBUG=1 -k all; make DEBUG=1 -k test

if [[ -z ${MAKE} ]]; then
    MAKE=make
else
    echo "Using make command '${MAKE}'"
fi

BUILDLOG=build/buildlog.txt
BUILDLOG_REPORT=build/buildlog_report.txt

CLEAN_TARGET=0

case "$1" in
    --clean)
        CLEAN_TARGET="clean"
        shift ;;
    --clean=*)
        CLEAN_TARGET=`echo $1 | sed 's#--clean=\(.*\)#\1/clean#'`
        shift ;;
esac

# Find all make options of the form FOO_BAR=... to keep them for multiple make
# invocations and find multi-targets of the form target_a,target_b
MULTI_TARGETS=()
SWITCHES=()
OPTIONS=()
for arg in "$@"; do
    echo "${arg}" | grep "^[A-Za-z_][A-Za-z0-9_]*=" > /dev/null
    if [ "$?" -eq "0" ]; then
        OPTIONS+=("${arg}")
    elif (echo "${arg}" | grep ".*,.*" > /dev/null); then
        for target in $(echo ${arg} | tr , "\n"); do
            echo "target = ${target}"
            MULTI_TARGETS+=(${target})
        done
    else
        SWITCHES+=(${arg})
    fi
done

cd `dirname $0`
rm -f ${BUILDLOG}
LOGSHELL=`pwd`/source/build/logshell.sh
echo "Starting build at `date '+%Y-%m-%d %H:%M:%S'` with params '$@', options = ${OPTIONS}" > ${BUILDLOG}

# Add --print-directory even though we're not using recursive make so Emacs (and
# other tools) shows correct source locations even when compilation command has
# not been triggered from this directory.

# Get rid of build report file in case clean command fails
${MAKE} --print-directory -s "${OPTIONS[@]}" clean_report

if [ "${CLEAN_TARGET}" != "0" ]
then
    echo "Cleaning ${CLEAN_TARGET} before build ..."
    ${MAKE} --print-directory -s ${OPTIONS} ${CLEAN_TARGET} || exit 1
fi

if [ "${MULTI_TARGETS}" = "" ]; then
    ${MAKE} --print-directory "$@" SHELL="${LOGSHELL} ${BUILDLOG}" BUILDLOG=${BUILDLOG}
    RETVAL=$?
else
    for target in "${MULTI_TARGETS[@]}"; do
        echo "building ${target}"
        ${MAKE} --print-directory "${OPTIONS[@]}" "${SWITCHES[@]}" SHELL="${LOGSHELL} ${BUILDLOG}" BUILDLOG=${BUILDLOG} ${target}
        RETVAL=$?
        if [ "${RETVAL}" -ne "0" ]; then
            echo "Building multi-target ${target} failed, aborting remaining targets"
            break
        fi
    done
fi

echo "Auto update test report ..." >> ${BUILDLOG}
(${MAKE} SILENT=1 "${OPTIONS[@]}" debug report 2>&1) > ${BUILDLOG_REPORT}
REPORT_EXIT_CODE=$?
if [ ${REPORT_EXIT_CODE} -eq 0 ];
then
    cat ${BUILDLOG_REPORT} >> ${BUILDLOG}
else
    echo "Failed to build test report with exit code ${REPORT_EXIT_CODE}"
    cat ${BUILDLOG_REPORT}
    exit ${REPORT_EXIT_CODE}
fi

date "+Finishing build at %Y-%m-%d %H:%M:%S with ${RETVAL}" >> ${BUILDLOG}
exit ${RETVAL}

