#!/usr/bin/env sh
#
# A simple script to run integration test for zomp remote shell/vm.
# Run this from the Zomp main dir.
#
# TODO: This might be outdated
#

if [ ! -e ./vm_server ]; then
    echo "$0:1: error: missing ./vm_server. Make sure that you run this from the Zomp main dir and that ./vm_server has been built"
    exit 1
fi

PORT=$((20000 + ${RANDOM} % 10000))

echo "Using port ${PORT}"

./vm_server ${PORT} &
sleep 1
./vm_client localhost ${PORT} "$1"
# sleep 1
# killall vm_server

