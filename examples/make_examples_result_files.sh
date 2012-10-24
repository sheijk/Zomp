#!/usr/bin/env sh
#
# Creates .report files for the example files in $1
#

for src in $1; do
	f=`basename $$src .zomp`;
	(if [ \! -e examples/$$f.ll ]; then
		echo "compilation failed";
	elif [ \! -e examples/$$f.bc ]; then
		echo "llvm-as failed";
	elif [ \! -e examples/$$f.exe ]; then
		echo "linking failed";
	else
		echo "succeeded";
	fi > examples/$$f.result)
done

