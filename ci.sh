#!/usr/bin/env sh

# A simple continuous integration script. Will do a git clone of the specified
# directory, do a full build and run the unit test suite

VC_DIR=$1
EXTERNAL_TOOLS_DIR=$2
FLAGS=$3

# abort_on_error(action, command...)
function run_action {
    ACTION=$1
    shift
    echo "running ${ACTION}: $@"
    $@ | tee ${TARGET_DIR}/log_${ACTION}.txt

    if [ "$?" -ne "0" ]; then
        echo "${ACTION} failed with $?"
        exit 1
    fi
}

if [ ! -d "${VC_DIR}" ]; then
    echo "Cannot find vc dir '${VC_DIR}' in `pwd`"
    exit 2
fi

if [ ! -d "${EXTERNAL_TOOLS_DIR}" ]; then
    echo "Cannot find external tools dir '${EXTERNAL_TOOLS_DIR}' in `pwd`"
    exit 3
fi

echo "Starting build and test run"

TARGET_DIR_REL=ci_run_`date '+%Y-%m-%d_%H_%M_%S'`
mkdir -p ${TARGET_DIR_REL}
TARGET_DIR=`cd ${TARGET_DIR_REL}; pwd`
echo "Target dir is ${TARGET_DIR}"

run_action "git_clone" git clone ${VC_DIR} ${TARGET_DIR}/zomp_vc
cd ${TARGET_DIR}/zomp_vc

run_action "make_all" make ${FLAGS} all
run_action "make_test" make ${FLAGS} test

