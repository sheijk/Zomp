#!/usr/bin/env sh

cd $(dirname $0)

open ../../../zomp_ci/build/release-i386/report.html
open ../../../zomp_ci_64/build/release-x86_64/report.html

open ../../build/release-i386/report.html
open ../../build/release-x86_64/report.html
open ../../build/release-x86_64/intermediate/caml-modules.svg

