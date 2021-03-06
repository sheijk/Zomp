#!/usr/bin/env sh
# A simplified CI script that will just pull updates, run a build and archive
# everything. Run this in a loop using 'source/ci/watch.sh ./ci.sh'
#
# Set ZOMP_CI_BRANCH to control which branch to run on. Will default to the one
# currently checked out.

ERROR_COMMAND_FAILED=1
ERROR_INVALID_ARGS=2
ERROR_ZOMP_WORKING_DIR_BROKEN=3

FLAGS="$@"
START_TIME=`date '+%Y/%m/%d/%H_%M_%S'`

if [ ! -e .git ]; then
    echo "Couldn't find .git directory in current dir. This needs to be run from within a check out of the Zomp source."
    exit ${ERROR_ZOMP_WORKING_DIR_BROKEN}
fi

if [ ! -e tools ]; then
    echo "Couldn't find tools directory. Please create a link to a Zomp tools dir."
    exit ${ERROR_ZOMP_WORKING_DIR_BROKEN}
fi

if [ -z ${ZOMP_CI_BRANCH} ]; then
    ZOMP_CI_BRANCH=$(git rev-parse --abbrev-ref HEAD)
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

    MSG="running ${ACTION_NAME} '${COMMAND}' at `date '+%Y-%m-%d %H:%M:%S'`, logging to ${LOGFILE}"
    echo "${MSG}"
    echo "${MSG}" >> ${MAIN_LOG}
    echo "${MSG}" >> ${LOGFILE}

    { ${COMMAND} 2>&1 ; } | tee -a ${LOGFILE}
    EXITSTATUS=$PIPESTATUS
    echo "${ACTION_NAME} exited with code ${EXITSTATUS}" >> ${LOGFILE}

    if [ "$EXITSTATUS" -ne "0" ]; then
        ACTION_RESULT="failed (${EXITSTATUS})"
        echo "  failed with ${EXITSTATUS}" | tee -a ${MAIN_LOG}
    else
        ACTION_RESULT="ok"
    fi
}

function git_fetch_and_reset {
    git fetch
    git reset --hard origin/${ZOMP_CI_BRANCH}
}

function copy_to_archive {
    ARCHIVE_DIR=ci_archive/${START_TIME}
    mkdir -p ${ARCHIVE_DIR}
    cp -R build ${ARCHIVE_DIR}
    rm -f ci_archive/last
    ln -s ../${ARCHIVE_DIR} ci_archive/last
    ARCHIVED_SRC_DIRS="testsuite examples libs"
    find ${ARCHIVED_SRC_DIRS} -type d -exec mkdir -p ${ARCHIVE_DIR}/{} \;
    find ${ARCHIVED_SRC_DIRS} \( -iname \*.zomp -or -iname \*.testreport -or -iname \*.test_output -or -iname \*.compile_output -or -iname \*.result -iname \*.compile_stats \) -exec cp -a {} ${ARCHIVE_DIR}/{} \;
}

function build {
    ./build.sh -j8 ${FLAGS} all
    if [ "$?" -ne 0 ]; then
        echo "  Build failed, trying clean build" >> ${MAIN_LOG}
        echo "Build failed, trying clean build"
        ./build.sh ${FLAGS} clean_all
        ./build.sh -j8 ${FLAGS} all
    fi
}

MAIN_LOG=build/ci_log.txt
rm -f ${MAIN_LOG}
touch ${MAIN_LOG}
echo "Logging to ${MAIN_LOG}"
echo "Building with FLAGS=${FLAGS}" >> ${MAIN_LOG}
echo "Building with FLAGS=${FLAGS}"

LAST_RUN_FILE=ci_archive/last_run.txt

OLD_REV=`git rev-parse --verify HEAD || echo old_invalid`
run_action "git_get_remote_changes" git_fetch_and_reset
run_action "git_checkout" git checkout ${ZOMP_CI_BRANCH}
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
run_action "initial-clean" ./build.sh ${FLAGS} clean_all
run_action "make" build
BUILD_RESULT=${ACTION_RESULT}
run_action "testsuite" ./build.sh -j8 ${FLAGS} test
TESTSUITE_RESULT=${ACTION_RESULT}
run_action "stats" ./build.sh ${FLAGS} print_ci_stats
run_action "archive" copy_to_archive

rm -f ${LAST_RUN_FILE}
echo "Last run:"
(echo "  finished at\t`date '+%Y-%m-%d %H:%M:%S'`"
 echo "  branch\t${ZOMP_CI_BRANCH}"
 echo "  revision\t$(git rev-parse HEAD)"
 echo "  flags\t${FLAGS}"
 echo "  build\t${BUILD_RESULT}"
 echo "  testsuite\t${TESTSUITE_RESULT}") | column -t -s $'\t' > ${LAST_RUN_FILE}
cat ${LAST_RUN_FILE}

