#!/bin/sh
aclocal
if [ $? -ne 0 ]; then
	echo "aclocal error"
	exit 1
fi

autoheader
if [ $? -ne 0 ]; then
	echo "autoheader error"
	exit 1
fi

automake -a -c
if [ $? -ne 0 ]; then
	echo "automake error"
	exit 1
fi

autoconf
if [ $? -ne 0 ]; then
	echo "autoconf error"
	exit 1
fi

echo OK

