#!/usr/bin/env sh
# A watch like program that doesn't kill of make processes which run too long.

while true; do
    echo "--------------------------------------------------"
    $@
    sleep 10
done

