#!/usr/bin/env sh

BASE_DIR=`dirname $0`
find ${BASE_DIR} \( -iname .git -or -iname data -or -iname tools -or -iname build \) -prune -or -type f -exec grep -E -nH $* {} +

