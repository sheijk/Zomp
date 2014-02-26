#!/usr/bin/env sh
#
# Creates .report files for the example files in $1
#

for src in $@; do
	f="${src%.*}";
	(if [ \! -e $f.ll ]; then
		echo "compilation failed";
	elif [ \! -e $f.bc ]; then
		echo "llvm-as failed";
	elif [ \! -e $f.exe ]; then
		echo "linking failed";
	else
		echo "ok";
	fi > $f.result)
done

