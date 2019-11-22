/*
 *  files-ansi
 */
#include <dirent.h>
#include "condition.h"
#include "pathname.h"

_g void directory_files(Execute ptr, addr *ret, addr pos)
{
	fmte("DIRECTORY function is not supported in ANSI-C mode.", NULL);
}

_g void probe_file_files(Execute ptr, addr *ret, addr pos)
{
	fmte("PROBE-FILE function is not supported in ANSI-C mode.", NULL);
}

_g void ensure_directories_exist_files(Execute ptr,
		addr *ret1, addr *ret2, addr pos, int verbose)
{
	fmte("ENSUER-DIRECTORIES-EXIST function is not supported in ANSI-C mode.", NULL);
}

_g void file_author_files(Execute ptr, addr *ret, addr pos)
{
	fmte("FILE-AUTHOR function is not supported in ANSI-C mode.", NULL);
}

_g void file_write_date_files(Execute ptr, addr *ret, addr pos)
{
	fmte("FILE-WRITE-DATE function is not supported in ANSI-C mode.", NULL);
}

_g void rename_file_files(Execute ptr,
		addr *ret1, addr *ret2, addr *ret3, addr file, addr to)
{
	fmte("TODO", NULL);
}

_g void delete_file_files(Execute ptr, addr pos)
{
	fmte("DELETE-FILE function is not supported in ANSI-C mode.", NULL);
}

_g void truename_files(Execute ptr, addr file, addr *ret, int errorp)
{
	if (! errorp) {
		*ret = Nil;
		return;
	}
	simple_file_error_stdarg(file, "TRUENAME is not support in ANSI-C mode.", NULL);
}

