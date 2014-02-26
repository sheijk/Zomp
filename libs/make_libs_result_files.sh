#!/usr/bin/env sh
#
# Creates .report files for the lib files in $1
# 

for src in $@; do
    f="${src%.*}";
	(if [ -e $f.ll ]; then
		echo "ok";
	else
		echo "compilation failed";
	fi > $f.result)
done

