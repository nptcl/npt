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

{
./test_amalgamation.sh "$1"
checkerr "test_amalgamation error."

./test_script.sh "$1"
checkerr "test_script error."

./test_configure.sh "$1"
checkerr "test_configure error."
} 2>&1 | tee result.txt

exit 0
