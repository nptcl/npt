#!/bin/sh

files=""
for src in `cd src; /bin/ls *.c`; do
  files="${files} ${src}"
done

cat << __EOF__ > Makefile
VPATH = src:test
CC = cc
CFLAGS = -g -O3 -std=c99 -Wall -fstack-protector -Isrc -Itest
CFLAGS += -DLISP_ANSIC -DLISP_COMPLEX_DOUBLE -DLISP_MODE_STANDARD
a.out = npt
source = ${files}
object = \$(source:.c=.o)

\$(a.out) : \$(object)
	\$(CC) \$(CFLAGS) -o \$(a.out) \$(object) -lm

.PHONY : clean
clean :
	-rm -f \$(a.out) \$(object)
__EOF__

make ${MAKEOPT}

