#!/bin/sh
npt="./release/npt --build"

checkerr()
{
	if [ "$?" -ne 0 ]; then
		echo "$@"
		exit 1
	fi
}

rm -f lisp.core
checkerr "rm error"

${npt} --script save.lisp
checkerr "save.lisp error"

exit 0

