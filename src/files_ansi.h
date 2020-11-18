/*
 *  files-ansi
 */
#include <stdio.h>
#include "condition.h"
#include "encode.h"
#include "pathname.h"
#include "pathname_object.h"
#include "pathname_wildcard.h"
#include "stream.h"
#include "strtype.h"

int directory_files_(Execute ptr, addr *ret, addr pos)
{
	return fmte_("DIRECTORY function is not supported in ANSI-C mode.", NULL);
}

static int probe_file_boolean(const char *file)
{
	FILE *input;

	input = fopen(file, "r");
	if (input)
		fclose(input);

	return input != NULL;
}

static int probe_file_run_files(Execute ptr, addr *ret, addr pos)
{
	int check;
	addr value, name;
	const char *str;

	/* wildcard */
	Return(pathname_designer_heap_(ptr, pos, &pos));
	Return(wild_pathname_boolean_(pos, Nil, &check));
	if (check) {
		return call_simple_file_error_va_(ptr, pos,
				"Cannot probe-file the wildcard pathname ~S.", pos, NULL);
	}

	/* filename */
	if (stringp(pos)) {
		Return(physical_pathname_local_(ptr, pos, &pos));
	}
	else {
		Return(physical_pathname_heap_(ptr, pos, &pos));
	}

	/* check */
	Return(name_pathname_local_(ptr, pos, &name));
	Return(UTF8_buffer_clang_(ptr->local, &value, name));
	if (value == Unbound) {
		return call_simple_file_error_va_(ptr, pos,
				"Cannot decode UTF-8 string ~S.", name, NULL);
	}
	str = (const char *)posbodyr(value);
	*ret = probe_file_boolean(str)? pos: Nil;

	return 0;
}

int probe_file_files_(Execute ptr, addr *ret, addr pos)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	Return(probe_file_run_files(ptr, ret, pos));
	rollback_local(local, stack);

	return 0;
}

int ensure_directories_exist_files_(Execute ptr,
		addr *ret1, addr *ret2, addr pos, int verbose)
{
	return fmte_("ENSUER-DIRECTORIES-EXIST function is "
			"not supported in ANSI-C mode.", NULL);
}

int file_author_files_(Execute ptr, addr *ret, addr pos)
{
	return fmte_("FILE-AUTHOR function is not supported in ANSI-C mode.", NULL);
}

int file_write_date_files_(Execute ptr, addr *ret, addr pos)
{
	return fmte_("FILE-WRITE-DATE function is not supported in ANSI-C mode.", NULL);
}

static int rename_file_run_files(Execute ptr,
		addr *ret1, addr *ret2, addr *ret3, addr pos, addr to)
{
	int check;
	LocalRoot local;
	addr file, from, value, true1, true2;
	const char *str1, *str2;

	Return(pathname_designer_heap_(ptr, pos, &file));
	Return(physical_pathname_heap_(ptr, file, &from));
	Return(physical_pathname_heap_(ptr, to, &to));
	Return(truename_files_(ptr, from, &true1, 0));
	Return(wild_pathname_boolean_(from, Nil, &check));
	if (check) {
		return call_simple_file_error_va_(ptr, pos,
				"Cannot rename wildcard pathname from ~S", from, NULL);
	}
	Return(wild_pathname_boolean_(to, Nil, &check));
	if (check) {
		return call_simple_file_error_va_(ptr, to,
				"Cannot rename wildcard pathname to ~S", to, NULL);
	}
	/* filename */
	local = ptr->local;
	Return(name_pathname_local_(ptr, from, &value));
	Return(UTF8_buffer_clang_(local, &value, value));
	if (value == Unbound) {
		return call_simple_file_error_va_(ptr, from,
				"Cannot decode UTF-8 string ~S.", from, NULL);
	}
	str1 = (const char *)posbodyr(value);
	Return(name_pathname_local_(ptr, to, &value));
	Return(UTF8_buffer_clang_(local, &value, value));
	if (value == Unbound) {
		return call_simple_file_error_va_(ptr, to,
				"Cannot decode UTF-8 string ~S.", to, NULL);
	}
	str2 = (const char *)posbodyr(value);
	/* check */
	if (probe_file_boolean(str2)) {
		return call_simple_file_error_va_(ptr, to,
				"The file ~S is already exist.", to, NULL);
	}
	/* rename */
	if (rename(str1, str2)) {
		return call_simple_file_error_va_(ptr, to,
				"Cannot rename ~S to ~S.", from, to, NULL);
	}
	/* stream */
	if (streamp(pos))
		SetPathnameStream(pos, to);
	/* result */
	Return(truename_files_(ptr, to, &true2, 0));
	*ret1 = to;
	*ret2 = true1;
	*ret3 = true2;

	return 0;
}

int rename_file_files_(Execute ptr,
		addr *ret1, addr *ret2, addr *ret3, addr file, addr to)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	Return(rename_file_run_files(ptr, ret1, ret2, ret3, file, to));
	rollback_local(local, stack);

	return 0;
}

int delete_file_files_(Execute ptr, addr pos)
{
	return fmte_("DELETE-FILE function is not supported in ANSI-C mode.", NULL);
}

int remove_file_common_(Execute ptr, addr pos, int errorp, int *ret)
{
	*ret = 0;
	return fmte_("REMOVE-FILE function is not supported in ANSI-C mode.", NULL);
}

int remove_directory_common_(Execute ptr, addr pos, int errorp, int *ret)
{
	*ret = 0;
	return fmte_("REMOVE-DIRECTORY function is not supported in ANSI-C mode.", NULL);
}

int truename_files_(Execute ptr, addr file, addr *ret, int errorp)
{
	if (! errorp)
		return Result(ret, Nil);
	return call_simple_file_error_va_(ptr, file,
			"TRUENAME is not support in ANSI-C mode.", NULL);
}

