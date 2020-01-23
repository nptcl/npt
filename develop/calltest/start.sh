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

## amalgamation
./amalgamation.sh $1
checkerr "amalgamation.sh error"
./check.sh amalgamation
checkerr "amalgamation.sh check error"


## script
./script.sh $1
checkerr "script.sh error"
./check.sh
checkerr "script.sh check error"


## autoconf
./configure.sh
checkerr "configure.sh error"
./check.sh
checkerr "configure.sh check error"


## clean
./clean.sh
checkerr "clean.sh error"

echo OK
exit 0

