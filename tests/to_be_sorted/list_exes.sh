#!/usr/bin/env sh

find . \( \! -path \*extlib\* \) \( \! -path \*tools\* \) -perm +u+x -type f

