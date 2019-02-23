#!/bin/sh

checkerr()
{
	if [ $? -ne 0 ]; then
		echo $*
		exit 1
	fi
}

if [ -f Makefile ]; then
	make clean
	checkerr "make clean error";
fi

rm -f Makefile
checkerr "rm Makefile error"

echo OK

