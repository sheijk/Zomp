#!/usr/bin/env sh
# A watch like program that doesn't kill make processes which run too long.

while true; do
    clear
    echo "--- $(date '+%Y-%m-%d %H:%M:%S')"
    echo ""
    $@
    sleep 10
done

