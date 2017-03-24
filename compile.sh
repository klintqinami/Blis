#!/usr/bin/env sh
set -e

MICROC="./microc.native"
LLC="llc"
CC="cc"

basename=$(basename "$1" .mc)
llfile="${basename}.ll"
objfile="${basename}.s"
outfile="${basename}"
${MICROC} < $1 > "${llfile}"
${LLC} "$llfile" > "${objfile}"
${CC} -o "${outfile}" "${objfile}"
