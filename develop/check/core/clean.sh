#!/bin/sh

checkerr()
{
	if [ $? -ne 0 ]; then
		echo "$@"
		exit 1
	fi
}

cd $(dirname $0)
checkerr "cd error"

rm -rf ./release
checkerr "rm release error"

rm -f $(/bin/ls npt-*.*.*.tar.gz 2> /dev/null)
checkerr "rm npt error"

rm -f lisp.core
checkerr "rm lisp.core error"

exit 0

