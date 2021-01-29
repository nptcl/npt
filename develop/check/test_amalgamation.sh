#!/bin/sh

checkerr()
{
	if [ $? -ne 0 ]; then
		echo "$@"
		exit 1
	fi
}

type="$1"
[ -n "${type}" ]
checkerr "type error"

case "$2" in
bsd)     name="bsd";;
freebsd) name="bsd";;
linux)   name="linux";;
*) name="";;
esac

cd $(dirname $0)
checkerr "cd error"

release="$(/bin/ls ../npt-*.*.*.tar.gz 2> /dev/null)"
[ -r "${release}" ]
checkerr "release file error: ${release}"

./make/clean.sh
checkerr "clean.sh error"

cp -n ${release} make/.
checkerr "cp error: ${release}"

[ -n "${name}" ]
checkerr "argument error"
./make/amalgamation.sh ${type} ${name} 2>&1
checkerr "amalgamation.sh error"

./make/check.sh amalgamation
checkerr "check.sh error"

./make/clean.sh
checkerr "clean.sh error"

exit 0

