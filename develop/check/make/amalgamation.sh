#!/bin/sh

## lisp="npt --script"
lisp="sbcl --script"

checkerr()
{
	if [ $? -ne 0 ]; then
		echo "$@"
		exit 1
	fi
}

compile_amalgamation()
{
	echo "Compile amalgamation code."
	if [ $1 = "linux" ]; then
		args="-g -Wall -m64 -fstack-protector"
		args="${args} -DLISP_64BIT -DLISP_LINUX"
		args="${args} -DLISP_PROMPT_READLINE -lm -lreadline"
	else
		args="-g -Wall -std=c99 -m64 -fstack-protector"
		args="${args} -DLISP_64BIT -DLISP_FREEBSD"
		args="${args} -DLISP_PROMPT_EDITLINE -lm -ledit"
	fi
	cc ${args} -o npt lisp*.c shell.c
	checkerr "cc error"
}


type="$1"
name="$2"
[ -n "$type" ]
checkerr "argument type error"
[ -n "$name" ]
checkerr "argument name error"

##  initialize
cd $(dirname $0)
checkerr "cd error"

rm -rf ./release
checkerr "rm error"


##  extract
release="$(/bin/ls npt-*.*.*.tar.gz 2> /dev/null)"
[ -r "${release}" ]
checkerr "release file error: ${release}"

tar zxf ${release}
checkerr "tar error"

path="$(ls -d npt-*/)"
[ -d "${path}" ]
checkerr "path error"

mv "${path}" release
checkerr "mv error"


##  compile
(
cd release/develop/amalgamation/
checkerr "cd error"

${lisp} amalgamation-${type}.lisp
checkerr "amalgamation error"

[ -r lisp.h ]
checkerr "make error"

mv -n lisp*.c lisp.h shell.c ../../.
checkerr "mv error"

cd ../../.
checkerr "cd previous error"

compile_amalgamation "${name}"

[ -x npt ]
checkerr "make error"

./npt --version
checkerr "npt error"

) || exit 1

echo OK
exit 0

