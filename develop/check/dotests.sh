#!/bin/sh

checkerr()
{
	if [ $? -ne 0 ]; then
		echo "$@"
		exit 1
	fi
}

rm -f result.txt check/result.txt > /dev/null 2>&1

cd $(dirname $0)
checkerr "cd error"

rm -f result.txt
checkerr "rm result.txt error";

{
./test_amalgamation.sh single "$1"
checkerr "test_amalgamation single error."

./test_amalgamation.sh multiple "$1"
checkerr "test_amalgamation multiple error."

./test_amalgamation.sh header "$1"
checkerr "test_amalgamation multiple error."

./test_script.sh "$1"
checkerr "test_script error."

./test_configure.sh "$1"
checkerr "test_configure error."

./test_core.sh "$1"
checkerr "test_core error."

} 2>&1 | tee result.txt

exit 0

