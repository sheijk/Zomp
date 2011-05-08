#!/usr/bin/env sh

# This script tries to find non-phony targets in the makefile which do not result
# in building a corresponding file

TARGETS=`grep "^[^#$%]*: " makefile |grep -v .PHONY |sed 's/\([^:]*\):.*/\1/'`

MAKE_COMMAND="make -ks ${TARGETS}"
# ${MAKE_COMMAND}
RESULT=$?

if [ ${RESULT} -eq 0 ]; then
    echo "Ok"
else
    echo "Building make targets failed. Command was ${MAKE_COMMAND}"
    exit 1
fi

for target in ${TARGETS};
do
    if [ ! -e ${target} ]
    then
        grep ".PHONY" makefile | grep "${target}" > /dev/null
        if [ $? -eq 1 ];
        then
            echo "warning: Target ${target} is not phony but no file was generated"
        fi
    fi
done

