/*
 *  files-ansi
 */
#include <dirent.h>
#include "condition.h"
#include "pathname.h"

void directory_files(Execute ptr, addr *ret, addr pos)
{
	fmte("DIRECTORY function is not supported in ANSI-C mode.", NULL);
}

void probe_file_files(Execute ptr, addr *ret, addr pos)
{
	fmte("PROBE-FILE function is not supported in ANSI-C mode.", NULL);
}

void ensure_directories_exist_files(Execute ptr,
		addr *ret1, addr *ret2, addr pos, int verbose)
{
	fmte("ENSUER-DIRECTORIES-EXIST function is not supported in ANSI-C mode.", NULL);
}

void file_author_files(Execute ptr, addr *ret, addr pos)
{
	fmte("FILE-AUTHOR function is not supported in ANSI-C mode.", NULL);
}

void file_write_date_files(Execute ptr, addr *ret, addr pos)
{
	fmte("FILE-WRITE-DATE function is not supported in ANSI-C mode.", NULL);
}

void rename_file_files(Execute ptr,
		addr *ret1, addr *ret2, addr *ret3, addr file, addr to)
{
	fmte("TODO", NULL);
}

void delete_file_files(Execute ptr, addr pos)
{
	fmte("DELETE-FILE function is not supported in ANSI-C mode.", NULL);
}

