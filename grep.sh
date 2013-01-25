#!/usr/bin/env sh

find . \( -iname .git -or -iname data -or -iname tools \) -prune -or -type f -exec grep -nH $* {} +

