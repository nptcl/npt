#!/bin/sh

checkerr()
{
	if [ $? -ne 0 ]; then
		echo "$@"
		exit 1
	fi
}

npt_pwd=$(cd ./release/; pwd);
npt="${npt_pwd}/npt"

[ -x ${npt} ]
checkerr "npt error"


## mode check
version_script()
{
	${npt} --version-script | awk "{if (\$1 == \"$1\") {print \$2}}"
}

version_script_equal()
{
	check="$(version_script $1)"
	[ "${check}" = "$2" ]
	checkerr "ERROR: $1"
	echo "OK: npt mode $1"
}

version_script_equal "name" "npt"
version_script_equal "execute-mode" "standalone"
version_script_equal "release-mode" "release"
version_script_equal "degrade-mode" "release"
version_script_equal "debug-memory" "false"
version_script_equal "force-gc" "disable"

if [ "$1" = "amalgamation" ]; then
  version_script_equal "amalgamation" "true"
else
  version_script_equal "amalgamation" "false"
fi

## --version
${npt} --version > /dev/null 2>&1
checkerr "ERROR: npt --version"

check="$(${npt} --version | grep npt)"
[ -n "${check}" ]
checkerr "ERROR: npt --version message"
echo "OK: npt --version"


## --help
${npt} --help > /dev/null 2>&1
checkerr "ERROR: npt --help"

check="$(${npt} --help | grep npt)"
[ -n "${check}" ]
checkerr "ERROR: npt --help"
echo "OK: npt --help"


## default
${npt} --quit --eval '(format t "Hello")' > /dev/null 2>&1
checkerr "ERROR: npt default"
check="$(${npt} --quit --eval '(format t "Hello")')"
[ "${check}" = "Hello" ]
checkerr "ERROR: npt default"
echo "OK: npt default"


## --standalone
${npt} --standalone --quit --eval '(format t "Hello")' > /dev/null 2>&1
checkerr "ERROR: npt --standalone"
check="$(${npt} --standalone --quit --eval '(format t "Hello")')"
[ "${check}" = "Hello" ]
checkerr "ERROR: npt --standalone"
echo "OK: npt --standalone"


## --build
${npt} --build --quit --eval '(format t "Hello")' > /dev/null 2>&1
checkerr "ERROR: npt --build"
check="$(${npt} --build --quit --eval '(format t "Hello")')"
[ "${check}" = "Hello" ]
checkerr "ERROR: npt --build"
echo "OK: npt --build"


## eval, eval
${npt} --quit --eval '(princ "ABC")' --eval '(princ "DEF")' > /dev/null 2>&1
checkerr "ERROR: npt --eval, --eval"
check="$(${npt} --quit --eval '(princ "ABC")' --eval '(princ "DEF")')"
[ "${check}" = "ABCDEF" ]
checkerr "ERROR: npt --eval, --eval"
echo "OK: npt --eval, --eval"


## load
${npt} --quit --load loadfile.lisp > /dev/null 2>&1
checkerr "ERROR: npt --load"
check="$(${npt} --quit --load loadfile.lisp)"
[ "${check}" = "HelloTest" ]
checkerr "ERROR: npt --load"
echo "OK: npt --load"


## script
${npt} --script loadfile.lisp > /dev/null 2>&1
checkerr "ERROR: npt --script"
check="$(${npt} --script loadfile.lisp)"
[ "${check}" = "HelloTest" ]
checkerr "ERROR: npt --script"
echo "OK: npt --script"


## rt.lisp
cd ../root/.
checkerr "cd error"
${npt} --script test/rt.lisp
checkerr "npt error"


## result
echo OK
exit 0

