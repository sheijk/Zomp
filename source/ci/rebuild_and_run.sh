#!/usr/bin/env sh
# Run this from main dir. Will cause the ci script to rebuild and start, again.

make -s clean_all
git reset --hard HEAD~1
./run.sh

