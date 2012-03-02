#!/usr/bin/env sh

PORT=$((20000 + ${RANDOM} % 10000))

echo "Using port ${PORT}"

./vm_server ${PORT} &
sleep 1
./vm_client localhost ${PORT} "$1"
# sleep 1
# killall vm_server

