#!/usr/bin/env sh
#
# Creates .report files for the lib files in $1
# 

for src in $@; do
	f=`basename $src .zomp`;
	(if [ -e libs/$f.ll ]; then
		echo "ok";
	else
		echo "compilation failed";
	fi > libs/$f.result)
done

