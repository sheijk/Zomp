#!/usr/bin/env sh
# Will check if the given files exists and have the right architecture
# Use like this:
# check_build_products.sh arch files*
#
# Environment variables BEGIN_ERROR and END_ERROR can be used to add
# highlighting/ansi color codes/etc. around errors.

ARCH=$1
shift
PRODUCTS=$*

HAD_ERRORS="0"

for product in $PRODUCTS; do
    if [ ! -e $product ]; then
        echo "Build product ${BEGIN_ERROR}${product}${END_ERROR} does not exist${NEWLINE}"
        HAD_ERRORS="1"
    else
        file ${product} | grep " ${ARCH}" > /dev/null
        if [ "$?" -ne "0" ]; then
            echo "Build product ${BEGIN_ERROR}${product}${END_ERROR} has the wrong architecture${NEWLINE}"
            HAD_ERRORS="1"
        fi
    fi
done

if [ "${HAD_ERRORS}" -eq "0" ]; then
    echo "All build products ok${NEWLINE}"
fi

