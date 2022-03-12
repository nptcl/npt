#!/bin/sh

files=""
for src in `cd src; /bin/ls *.c`; do
  files="${files} ${src}"
done

cat << __EOF__ > Makefile
VPATH = src:test
CC = c++
CFLAGS = -g -Wall -Wno-deprecated -fstack-protector -Isrc -Itest
CFLAGS += -DLISP_LINUX -DLISP_DEBUG -DLISP_MODE_STANDARD
CFLAGS += -DLISP_DYNAMIC_LINK
a.out = npt
source = ${files}
object = \$(source:.c=.o)

\$(a.out) : \$(object)
	\$(CC) \$(CFLAGS) -o \$(a.out) \$(object) -lm -ldl -lreadline

.PHONY : clean
clean :
	-rm -f \$(a.out) \$(object)
__EOF__

make ${MAKEOPT}

