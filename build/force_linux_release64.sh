#!/bin/sh
file="build/Makefile.linux_release64"

checkerr()
{
	if [ $? -ne 0 ]; then
		echo "$@"
		exit 1
	fi
}

cp -f ${file} Makefile
checkerr "cp Makefile error"

make -f ${file} clean
checkerr "make clean error";

make -f ${file} depend
checkerr "make depend error";

make -f ${file} build ${MAKEOPT} "$@"
checkerr "make build error";

echo OK

