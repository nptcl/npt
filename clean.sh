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

## autoconf
if [ "$1" == "all" ]; then
  rm -f Makefile
  rm -f Makefile.in
  rm -f aclocal.m4
  rm -fr autom4te.cache/
  rm -f compile
  rm -f config.guess
  rm -f config.h.in
  rm -f config.sub
  rm -f configure
  rm -f depcomp
  rm -f install-sh
  rm -f missing
  rm -f config.h config.log config.status
  rm -fr src/.deps/
  rm -f src/.dirstamp
  rm -f stamp-h1
  rm -rf .deps/
  rm -f _debug.txt
else
  rm -f Makefile
  #rm -f Makefile.in
  #rm -f aclocal.m4
  rm -fr autom4te.cache/

  #rm -f configure
  #rm -f install-sh
  #rm -f compile
  #rm -f depcomp
  #rm -f missing
  #rm -f config.guess
  #rm -f config.h.in
  #rm -f config.sub
  rm -f config.h
  rm -f config.log
  rm -f config.status
  rm -fr src/.deps/
  rm -f src/.dirstamp
  rm -f stamp-h1
  rm -rf .deps/
  rm -f _debug.txt
fi

echo OK

