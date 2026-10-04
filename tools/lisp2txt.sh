#!/bin/bash
# convert lists in lisp file to txt with line numbers (to be read with puredata)

LISP=$1
OUT=$(basename -s ".lisp" "$LISP")
cat "$LISP" | sed -e 's/(//g' -e 's/).*$/\;/g' |nl > "$OUT.txt"



