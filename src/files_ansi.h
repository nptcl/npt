/*
 *  files-ansi
 */
#include <stdio.h>
#include "condition.h"
#include "encode.h"
#include "pathname.h"
#include "stream.h"
#include "strtype.h"

_g void directory_files(Execute ptr, addr *ret, addr pos)
{
	_fmte("DIRECTORY function is not supported in ANSI-C mode.", NULL);
}

static int probe_file_boolean(const char *file)
{
	FILE *input;

	input = fopen(file, "r");
	if (input)
		fclose(input);

	return input != NULL;
}

static void probe_file_run_files(Execute ptr, addr *ret, addr pos)
{
	addr value;
	const char *str;

	/* filename */
	if (stringp(pos))
		physical_pathname_local(ptr, pos, &pos);
	else
		physical_pathname_heap(ptr, pos, &pos);
	/* wildcard */
	if (wild_pathname_boolean(pos, Nil)) {
		GetConst(COMMON_PATHNAME, &value);
		type_error_stdarg(pos, value,
				"Cannot probe-file the wildcard pathname ~S.", pos, NULL);
	}
	/* check */
	name_pathname_local(ptr, pos, &pos);
	if (UTF8_buffer_clang(ptr->local, &pos, pos))
		_fmte("Cannot decode UTF-8 string ~S.", pos, NULL);
	str = (const char *)posbodyr(pos);
	*ret = probe_file_boolean(str)? T: Nil;
}

_g void probe_file_files(Execute ptr, addr *ret, addr pos)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	probe_file_run_files(ptr, ret, pos);
	rollback_local(local, stack);
}

_g void ensure_directories_exist_files(Execute ptr,
		addr *ret1, addr *ret2, addr pos, int verbose)
{
	_fmte("ENSUER-DIRECTORIES-EXIST function is not supported in ANSI-C mode.", NULL);
}

_g void file_author_files(Execute ptr, addr *ret, addr pos)
{
	_fmte("FILE-AUTHOR function is not supported in ANSI-C mode.", NULL);
}

_g void file_write_date_files(Execute ptr, addr *ret, addr pos)
{
	_fmte("FILE-WRITE-DATE function is not supported in ANSI-C mode.", NULL);
}

static void rename_file_run_files(Execute ptr,
		addr *ret1, addr *ret2, addr *ret3, addr pos, addr to)
{
	LocalRoot local;
	addr file, from, value, true1, true2;
	const char *str1, *str2;

	pathname_designer_heap(ptr, pos, &file);
	physical_pathname_heap(ptr, file, &from);
	physical_pathname_heap(ptr, to, &to);
	truename_files(ptr, from, &true1, 0);
	if (wild_pathname_boolean(from, Nil))
		_fmte("Cannot rename wildcard pathname from ~S", from, NULL);
	if (wild_pathname_boolean(to, Nil))
		_fmte("Cannot rename wildcard pathname to ~S", to, NULL);
	/* filename */
	local = ptr->local;
	name_pathname_local(ptr, from, &value);
	if (UTF8_buffer_clang(local, &value, value))
		_fmte("Cannot decode UTF-8 string ~S.", from, NULL);
	str1 = (const char *)posbodyr(value);
	name_pathname_local(ptr, to, &value);
	if (UTF8_buffer_clang(local, &value, value))
		_fmte("Cannot decode UTF-8 string ~S.", to, NULL);
	str2 = (const char *)posbodyr(value);
	/* check */
	if (probe_file_boolean(str2)) {
		simple_file_error_stdarg(to, "The file ~S is already exist.", to, NULL);
		return;
	}
	/* rename */
	if (rename(str1, str2)) {
		simple_file_error_stdarg(to, "Cannot rename ~S to ~S.", from, to, NULL);
		return;
	}
	/* stream */
	if (streamp(pos))
		SetPathnameStream(pos, to);
	/* result */
	truename_files(ptr, to, &true2, 0);
	*ret1 = to;
	*ret2 = true1;
	*ret3 = true2;
}

_g void rename_file_files(Execute ptr,
		addr *ret1, addr *ret2, addr *ret3, addr file, addr to)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	rename_file_run_files(ptr, ret1, ret2, ret3, file, to);
	rollback_local(local, stack);
}

_g void delete_file_files(Execute ptr, addr pos)
{
	_fmte("DELETE-FILE function is not supported in ANSI-C mode.", NULL);
}

_g void truename_files(Execute ptr, addr file, addr *ret, int errorp)
{
	if (! errorp) {
		*ret = Nil;
		return;
	}
	simple_file_error_stdarg(file, "TRUENAME is not support in ANSI-C mode.", NULL);
}

_g int remove_file_common(Execute ptr, addr pos, int errorp)
{
	_fmte("REMOVE-FILE function is not supported in ANSI-C mode.", NULL);
	return 0;
}

_g int remove_directory_common(Execute ptr, addr pos, int errorp)
{
	_fmte("REMOVE-DIRECTORY function is not supported in ANSI-C mode.", NULL);
	return 0;
}

