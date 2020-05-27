#!/bin/sh

checkerr()
{
	if [ $? -ne 0 ]; then
		echo "$@"
		exit 1
	fi
}

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
cd release
checkerr "cd error"

./bootstrap.sh
checkerr "bootstrap.sh error"

./configure --prefix="$(pwd)/work"
checkerr "script error"

make
checkerr "make error"

make install
checkerr "make install error"

[ -x npt ]
checkerr "binary error"

[ -x work/bin/npt ]
checkerr "install error"

./npt --version
checkerr "npt error"

) || exit 1

echo OK
exit 0

