#!/bin/sh

file="src/version.h"
output="config.version"
version_a=$(grep LISP_VERSION_A ${file} | awk '{print $3}')
version_b=$(grep LISP_VERSION_B ${file} | awk '{print $3}')
version_c=$(grep LISP_VERSION_C ${file} | awk '{print $3}')
echo -n "${version_a}.${version_b}.${version_c}" > ${output}

## autoconf
aclocal || {
	echo "aclocal error"
	exit 1
}
autoheader || {
	echo "autoheader error"
	exit 1
}
automake -a -c || {
	echo "automake error"
	exit 1
}
autoconf || {
	echo "autoconf error"
	exit 1
}
echo OK

