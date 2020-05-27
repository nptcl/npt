#!/bin/sh
npt="./release/npt --core"

checkerr()
{
	if [ "$?" -ne 0 ]; then
		echo "$@"
		exit 1
	fi
}

${npt} --corefile lisp.core --script load.lisp
checkerr "load.lisp error"

exit 0

