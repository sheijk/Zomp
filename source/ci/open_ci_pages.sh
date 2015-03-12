#!/usr/bin/env sh

cd $(dirname $0)

open ../../../zompci/i386/build/release-i386/report.html
open ../../../zompci/x86_64/build/release-x86_64/report.html

# Fails with Chrome if we do not do this
sleep 0.1

open ../../build/release-i386/report.html
open ../../build/release-x86_64/report.html

