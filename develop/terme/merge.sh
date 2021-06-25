#!/bin/sh
output="../terme.c"
file_header="
terme_call.h
terme_font.h
terme_input.h
terme_output.h
terme_prompt.h
terme_value.h"
file_source="
terme_interface.c
terme_call.c
terme_font.c
terme_input.c
terme_output.c
terme_prompt.c
terme_value.c"

checkerr()
{
	if [ "$?" -ne 0 ]; then
		echo "$@"
		exit 1
	fi
}

output_file()
{
	echo "/************************************************************"
	echo "  ${1}"
	echo " ************************************************************/"
	grep -v '#include "terme_' ${file}
	checkerr "cat error: ${file}"
	echo >> ${output}
}

cat /dev/null > ${output}
checkerr "cat error"

##
##  define
##
echo '#include "define.h"' >> ${output}
echo >> ${output}
echo '#ifdef LISP_TERME' >> ${output}


##
##  header
##
for file in ${file_header}; do
	echo ${file}
	output_file ${file} >> ${output}
done


##
##  source
##
for file in ${file_source}; do
	echo ${file}
	output_file ${file} >> ${output}
done


##
##  disable
##
echo '#else' >> ${output}
file="terme_disable.c"
echo ${file}
output_file ${file} >> ${output}
echo '#endif' >> ${output}
echo >> ${output}


##
##  end
##
echo "Output file: ${output}"
exit 0

