#!/usr/bin/env sh
# Will remove all lines that are zompsh commands (starting with !) and then run
# the result through zomc

ZOMPC=$1
if [[ "$2" != "-c" ]] || [ -z ${3} ]; then
    echo "error: expected 'zompc_name -c file_name ...' but found '$*'"
    exit 1
fi
FILE_NAME=$3
shift 3

TEMP_FILE=${FILE_NAME/test_/no_zompsh_test_}
cat ${FILE_NAME} | sed -E 's#^(!.*)#// (removed) \1#' > ${TEMP_FILE}

echo "running ${ZOMPC} -c ${TEMP_FILE} $*"
${ZOMPC} -c ${TEMP_FILE} $*
EXIT_CODE=$?
mv ${TEMP_FILE%.zomp}.ll ${FILE_NAME%.zomp}.ll
# rm -f ${TEMP_FILE}
exit ${EXIT_CODE}

