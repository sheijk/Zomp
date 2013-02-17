#!/usr/bin/env sh
# A simple continuous integration script. Will do a git clone of the specified
# directory, do a full build and run the unit test suite

ERROR_INVALID_ARGS=2
ERROR_COMMAND_FAILED=1

if [ ! -z "$4" -o -z "$1" -o -z "$2" ]; then
    echo "error: invalid arguments, expected:"
    echo "`basename $0` zomp-git-repo external-tools-dir make-flags?"
    exit ${ERROR_INVALID_ARGS}
fi

if [ ! -d "$1" ]; then
    echo "Cannot find vc dir '$1' in `pwd`"
    exit ${ERROR_INVALID_ARGS}
fi

if [ ! -d "$2" ]; then
    echo "Cannot find external tools dir '$2' in `pwd`"
    exit ${ERROR_INVALID_ARGS}
fi

VC_DIR=`cd $1; pwd`
EXTERNAL_TOOLS_DIR=`cd $2; pwd`
FLAGS=$3

ACTION_NUMBER=0

# abort_on_error(action, command...)
function run_action {
    ACTION=$1
    shift
    CAN_FAIL=0
    if [ ${1-NOFAIL} == "CAN-FAIL" ]; then
        CAN_FAIL=1
        shift
    fi

    ACTION_NUMBER=$((${ACTION_NUMBER} + 1))
    LOGFILE=${TARGET_DIR}/log_${ACTION_NUMBER}_${ACTION}.txt

    MSG="running ${ACTION} '$@'"
    echo "${MSG}"
    echo "${MSG}" >> ${MAIN_LOG}
    echo "${MSG}" >> ${LOGFILE}

    { $@ 2>&1 ; } | tee -a ${LOGFILE}
    EXITSTATUS=$PIPESTATUS
    echo "${ACTION} exited with code ${EXITSTATUS}" >> ${LOGFILE}

    if [ "$EXITSTATUS" -ne "0" ]; then
        echo "  failed with ${EXITSTATUS}" | tee -a ${MAIN_LOG}
        if [ "${CAN_FAIL}" == "0" ]; then
            exit ${ERROR_COMMAND_FAILED}
        fi
    fi
}

# find_all_files(output_file_name)
function find_all_files {
    find . \( -iname .git -prune -or -true \) > $1
}

echo "Starting build and test run"

TARGET_DIR_REL=`date '+%Y/%m/%d/%H_%M_%S'`
mkdir -p ${TARGET_DIR_REL}
TARGET_DIR=`cd ${TARGET_DIR_REL}; pwd`
echo "Target dir is ${TARGET_DIR}"
echo "External tools dir is ${EXTERNAL_TOOLS_DIR}"

MAIN_LOG=${TARGET_DIR}/log.txt
rm -f ${MAIN_LOG}
echo "Logging to ${MAIN_LOG}"
touch ${MAIN_LOG}

run_action "git_clone" git clone ${VC_DIR} ${TARGET_DIR}/zomp_vc

cd ${TARGET_DIR}
cd zomp_vc

# Setup links to external tools
pwd
mkdir -p tools
ln -s ${EXTERNAL_TOOLS_DIR}/llvm-2.9 tools/llvm-2.9
ln -s ${EXTERNAL_TOOLS_DIR}/llvm-gcc tools/llvm-gcc
ln -s ${EXTERNAL_TOOLS_DIR}/external tools/external

run_action "find_initial_files" find_all_files ../files_after_clone.txt

run_action "make_all" ./build.sh ${FLAGS} all
run_action "make_test" CAN-FAIL ./build.sh ${FLAGS} test

run_action "make_clean" make ${FLAGS} clean_all
run_action "find_files_after_clean" find_all_files ../files_after_clean.txt

DIFF_FORMAT="--new-line-format +%L --old-line-format=-%L --unchanged-line-format="
run_action "check_make_clean" diff ../files_after_clone.txt ../files_after_clean.txt ${DIFF_FORMAT}

