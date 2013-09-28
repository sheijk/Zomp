#!/usr/bin/env sh
# A simplified CI script that will just pull updates, run a build and archive
# everything. Run this in a loop using 'source/build/watch.sh ./ci.sh'

ERROR_COMMAND_FAILED=1
ERROR_INVALID_ARGS=2
ERROR_ZOMP_WORKING_DIR_BROKEN=3

TESTED_BRANCH=master

if [ ! -z "$2" ]; then
    echo "error: invalid arguments, expected:"
    echo "`basename $0` make-flags?"
    exit ${ERROR_INVALID_ARGS}
fi

FLAGS=$1
START_TIME=`date '+%Y/%m/%d/%H_%M_%S'`

if [ ! -e .git ]; then
    echo "Couldn't find .git directory in current dir. This needs to be run from within a check out of the Zomp source."
    exit ${ERROR_ZOMP_WORKING_DIR_BROKEN}
fi

if [ ! -e tools ]; then
    echo "Couldn't find tools directory. Please create a link to a Zomp tools dir."
    exit ${ERROR_ZOMP_WORKING_DIR_BROKEN}
fi

# abort_on_error(action, command...)
function run_action {
    ACTION_NAME=$1
    shift
    COMMAND=$@

    mkdir -p build
    LOGFILE=build/ci_log_${ACTION_NAME}.txt
    rm -f ${LOGFILE}
    touch ${LOGFILE}

    MSG="running ${ACTION_NAME} '${COMMAND}' at `date '+%Y-%m-%d %H:%M:%S'`"
    echo "${MSG}"
    echo "${MSG}" >> ${MAIN_LOG}
    echo "${MSG}" >> ${LOGFILE}

    { ${COMMAND} 2>&1 ; } | tee -a ${LOGFILE}
    EXITSTATUS=$PIPESTATUS
    echo "${ACTION_NAME} exited with code ${EXITSTATUS}" >> ${LOGFILE}

    if [ "$EXITSTATUS" -ne "0" ]; then
        echo "  failed with ${EXITSTATUS}" | tee -a ${MAIN_LOG}
    fi
}

function git_fetch_and_reset {
    git fetch
    git reset --hard origin/${TESTED_BRANCH}
}

function copy_to_archive {
    ARCHIVE_DIR=ci_archive/${START_TIME}
    mkdir -p ${ARCHIVE_DIR}
    cp -R build ${ARCHIVE_DIR}
    rm -f ci_archive/last
    ln -s ../${ARCHIVE_DIR} ci_archive/last
    ARCHIVED_SRC_DIRS="testsuite examples libs"
    find ${ARCHIVED_SRC_DIRS} -type d -exec mkdir -p ${ARCHIVE_DIR}/{} \;
    find ${ARCHIVED_SRC_DIRS} \( -iname \*.zomp -or -iname \*.testreport -or -iname \*.test_output \) -exec cp -a {} ${ARCHIVE_DIR}/{} \;
}

MAIN_LOG=build/ci_log.txt
rm -f ${MAIN_LOG}
touch ${MAIN_LOG}
echo "Logging to ${MAIN_LOG}"

LAST_RUN_FILE=ci_archive/last_run.txt

OLD_REV=`git rev-parse --verify HEAD || echo old_invalid`
run_action "git_get_remote_changes" git_fetch_and_reset
run_action "git_checkout" git checkout master
NEW_REV=`git rev-parse --verify HEAD || echo new_invalid`

# Short circuit if no changes where found and ci has been run before
if [ -e ci_archive ]; then
    if [ "${OLD_REV}" == "${NEW_REV}" ]; then
        echo "No changes"

        if [ -e ${LAST_RUN_FILE} ]; then
            cat ${LAST_RUN_FILE}
        fi

        exit 0
    fi
fi

git rev-parse HEAD > build/ci_git_revision.txt
run_action "make" ./build.sh ${FLAGS} all test
run_action "archive" copy_to_archive

rm -f ${LAST_RUN_FILE}
echo "Last run finished at `date '+%Y-%m-%d %H:%M:%S'`" > ${LAST_RUN_FILE}

