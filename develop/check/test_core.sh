#!/bin/sh

checkerr()
{
	if [ $? -ne 0 ]; then
		echo "$@"
		exit 1
	fi
}

case "$1" in
freebsd) name="freebsd";;
linux)   name="linux";;
*) name="";;
esac

release="$(/bin/ls ../npt-*.*.*.tar.gz 2> /dev/null)"
[ -r "${release}" ]
checkerr "release file error: ${release}"

./core/clean.sh
checkerr "clean.sh error"

cp -n ${release} core/.
checkerr "cp error: ${release}"

[ -n "${name}" ]
checkerr "argument error"

./core/develop.sh ${name} 2>&1
checkerr "core/develop.sh error"

./core/release.sh ${name} 2>&1
checkerr "core/release.sh error"

exit 0

