#!/usr/bin/env sh

# A simple continuous integration script. Will do a git clone of the specified
# directory, do a full build and run the unit test suite

if [ ! -d "$1" ]; then
    echo "Cannot find vc dir '$1' in `pwd`"
    exit 2
fi

if [ ! -d "$2" ]; then
    echo "Cannot find external tools dir '$2' in `pwd`"
    exit 3
fi

VC_DIR=`cd $1; pwd`
EXTERNAL_TOOLS_DIR=`cd $2; pwd`
FLAGS=$3

# abort_on_error(action, command...)
function run_action {
    ACTION=$1
    shift
    echo "running ${ACTION}: $@"
    LOGFILE=${TARGET_DIR}/log_${ACTION}.txt
    $@ | tee ${LOGFILE}
    EXITSTATUS=$PIPESTATUS
    echo "Exited with code ${EXITSTATUS}" >> ${LOGFILE}

    if [ "$EXITSTATUS" -ne "0" ]; then
        echo "${ACTION} failed with ${EXITSTATUS}"
        exit 1
    fi
}

# find_all_files(output_file_name)
function find_all_files {
    find . \( -iname .git -prune -or -true \) > $1
}

echo "Starting build and test run"

TARGET_DIR_REL=ci_run_`date '+%Y-%m-%d_%H_%M_%S'`
mkdir -p ${TARGET_DIR_REL}
TARGET_DIR=`cd ${TARGET_DIR_REL}; pwd`
echo "Target dir is ${TARGET_DIR}"
echo "External tools dir is ${EXTERNAL_TOOLS_DIR}"

run_action "git_clone" git clone ${VC_DIR} ${TARGET_DIR}/zomp_vc

cd ${TARGET_DIR}
cd zomp_vc

# Setup links to external tools
pwd
mkdir -p tools
ln -s ${EXTERNAL_TOOLS_DIR}/llvm-2.9 tools/llvm-2.9
ln -s ${EXTERNAL_TOOLS_DIR}/llvm-gcc tools/llvm-gcc

run_action "find_initial_files" find_all_files ../files_after_clone.txt

run_action "make_all" make ${FLAGS} all
# run_action "make_test" make ${FLAGS} test

run_action "make_clean" make ${FLAGS} clean_all
run_action "find_files_after_clean" find_all_files ../files_after_clean.txt

echo "" > .gitignore
run_action "check_make_clean" diff ../files_after_clone.txt ../files_after_clean.txt > ../file_diff.txt

