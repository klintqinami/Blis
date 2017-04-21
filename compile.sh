#!/usr/bin/env sh
set -e

MICROC="./microc.native"
CC="cc"
if [[ `uname` == "Darwin" ]]; then
  LLC="llc-3.8"
  LIBS="-L/usr/local/lib/ -framework OpenGL -lglfw"
else
  LLC="llc"
  LIBS="-lGLEW -lGL -lglfw"
fi

basename=$(basename "$1" .mc)
llfile="${basename}.ll"
objfile="${basename}.s"
outfile="${basename}"
${MICROC} $1 > "${llfile}"
${LLC} "$llfile" > "${objfile}"
${CC} -o "${outfile}" "${objfile}" runtime.o ${LIBS}
