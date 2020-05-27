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

rm -f result.txt
checkerr "rm result.txt error";

./make/clean.sh
checkerr "clean.sh error"

./core/clean.sh
checkerr "clean.sh error"

exit 0

