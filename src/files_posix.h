/*
 *  files-posix
 */
#include <dirent.h>
#include <pwd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "control_object.h"
#include "copy.h"
#include "encode.h"
#include "format.h"
#include "local.h"
#include "integer.h"
#include "pathname_common.h"
#include "pathname_object.h"
#include "pathname.h"
#include "sequence.h"
#include "stream.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"
#include "unicode.h"

/*
 *  directory
 */
struct directory_struct {
	Execute ptr;
	LocalRoot local;
	addr pos, list, front, root;
};

static int init_directory_struct_(Execute ptr, struct directory_struct *str, addr pos)
{
	clearpoint(str);
	str->ptr = ptr;
	str->local = ptr->local;
	str->list = str->root = str->front = Nil;
	return pathname_designer_heap_(ptr, pos, &str->pos);
}

static int make_list_directory_pathname_(struct directory_struct *str,
		addr *ret, addr list)
{
	LocalRoot local;
	addr pos, one, version;

	local = str->local;
	pos = str->pos;
	make_pathname_alloc(local, &one, RefLogicalPathname(pos));
	copylocal_pathname_array(local, pos, PATHNAME_INDEX_HOST, one);
	copylocal_pathname_array(local, pos, PATHNAME_INDEX_DEVICE, one);
	copylocal_pathname_array(local, pos, PATHNAME_INDEX_NAME, one);
	copylocal_pathname_array(local, pos, PATHNAME_INDEX_TYPE, one);
	/* directory */
	reverse_list_local_unsafe(local, &list, list);
	copylocal_object(local, &list, list);
	SetDirectoryPathname(one, list);
	/* version */
	GetConst(KEYWORD_UNSPECIFIC, &version);
	SetVersionPathname(one, version);
	/* result */
	return physical_pathname_local_(str->ptr, one, ret);
}

static int make_directory_pathname_(struct directory_struct *str, addr *ret)
{
	return make_list_directory_pathname_(str, ret, str->list);
}

static int opendir_files(LocalRoot local, addr pos, DIR **ret)
{
	addr name;
	LocalStack stack;
	const char *clang;
	DIR *dir;

	push_local(local, &stack);
	Return(directory_name_pathname_local_(local, pos, &pos));
	Return(UTF8_buffer_clang_(local, &name, pos));
	if (name == Unbound) {
		*ret = NULL;
		return fmte_("Cannot convert ~S to UTF-8 string.", pos, NULL);
	}
	clang = (const char *)posbodyr(name);
	dir = opendir(clang);
	rollback_local(local, stack);

	return Result(ret, dir);
}

static void merge_directory_files(LocalRoot local, addr path, addr defaults)
{
	copylocal_pathname_array(local, defaults, PATHNAME_INDEX_HOST, path);
	copylocal_pathname_array(local, defaults, PATHNAME_INDEX_DEVICE, path);
	copylocal_pathname_array(local, defaults, PATHNAME_INDEX_DIRECTORY, path);
}

static int directoryp_directory_files(Execute ptr, addr file, int *ret)
{
	int check;
	LocalRoot local;
	LocalStack stack;
	const char *body;
	addr pos;
	struct stat st;

	local = ptr->local;
	push_local(local, &stack);
	Return(name_pathname_local_(ptr, file, &pos));
	Return(UTF8_buffer_clang_(local, &pos, pos));
	if (pos == Unbound) {
		*ret = 0;
		return fmte_("Cannot convert ~S to UTF-8 string.", file, NULL);
	}
	body = (const char *)posbodyr(pos);
	check = (! lstat(body, &st)) && S_ISDIR(st.st_mode);
	rollback_local(local, stack);

	return Result(ret, check);
}

static int files_path_directory_files(struct directory_struct *str,
		addr path, addr name, addr base, addr *ret)
{
	int check;
	addr list;
	LocalRoot local;

	Return(directoryp_directory_files(str->ptr, path, &check));
	if (check) {
		local = str->local;
		copy_list_local_unsafe(local, &list, str->list);
		cons_local(local, &list, name, list);
		Return(make_list_directory_pathname_(str, &path, list));
		SetNamePathname(path, Nil);
		SetTypePathname(path, Nil);
	}

	return Result(ret, path);
}

static int files_name_directory_files(struct directory_struct *str,
		addr base, addr name)
{
	int check;
	Execute ptr;
	addr path;

	ptr = str->ptr;
	Return(pathname_designer_local_(ptr, name, &path));
	merge_directory_files(str->local, path, base);
	Return(wildcard_pathname_(path, str->pos, 1, &check));
	if (check) {
		/* push heap */
		Return(files_path_directory_files(str, path, name, base, &path));
		Return(pathname_designer_heap_(ptr, path, &path));
		cons_heap(&str->root, path, str->root);
	}

	return 0;
}

static int files_push_directory_files(struct directory_struct *str,
		addr base, const char *name)
{
	LocalRoot local;
	LocalStack stack;
	addr path;

	if (strcmp(name, ".") == 0)
		return 0;
	if (strcmp(name, "..") == 0)
		return 0;
	local = str->local;
	push_local(local, &stack);
	Return(string8_null_local_(local, &path, name));
	Return(files_name_directory_files(str, base, path));
	rollback_local(local, stack);

	return 0;
}

static int files_directory_files(struct directory_struct *str, addr base)
{
	DIR *dir;
	const char *name;
	struct dirent *entry;

	Return(opendir_files(str->local, base, &dir));
	if (dir == NULL)
		return 0;
	for (;;) {
		/* The readdir_r() interface is deprecated. */
		entry = readdir(dir);
		if (entry == NULL)
			break;
		name = entry->d_name;
		Return(files_push_directory_files(str, base, name));
	}
	if (closedir(dir))
		return fmte_("closedir() error.", NULL);

	return 0;
}

static int file_directory_files(struct directory_struct *str)
{
	addr base;
	LocalStack stack;

	push_local(str->local, &stack);
	Return(make_directory_pathname_(str, &base));
	Return(files_directory_files(str, base));
	rollback_local(str->local, stack);

	return 0;
}

static int wild_check_directory_files(struct directory_struct *str, addr name, int *ret)
{
	int check;
	LocalRoot local;
	LocalStack stack;
	const char *ptr;
	addr pos, value;
	struct stat st;

	local = str->local;
	push_local(local, &stack);
	copy_list_local_unsafe(local, &pos, str->list);
	cons_local(local, &pos, name, pos);
	Return(make_list_directory_pathname_(str, &pos, pos));
	Return(directory_name_pathname_local_(local, pos, &pos));
	Return(UTF8_buffer_clang_(local, &value, pos));
	if (value == Unbound)
		return fmte_("Cannot convert ~S to UTF-8 string.", pos, NULL);
	ptr = (const char *)posbodyr(value);
	check = (! lstat(ptr, &st)) && S_ISDIR(st.st_mode);
	rollback_local(local, stack);

	return Result(ret, check);
}

static int loop_directory_files(struct directory_struct *str);
static int wild_push_directory_files(struct directory_struct *str, const char *name)
{
	int check;
	LocalRoot local;
	LocalStack stack;
	addr path, root, list, front;

	if (strcmp(name, ".") == 0)
		return 0;
	if (strcmp(name, "..") == 0)
		return 0;
	local = str->local;
	push_local(local, &stack);
	Return(string8_null_local_(local, &path, name));
	/* directory check */
	Return(wild_check_directory_files(str, path, &check));
	if (check) {
		/* backup list */
		list = str->list;
		front = str->front;
		/* new list */
		copy_list_local_unsafe(local, &root, list);
		cons_local(local, &root, path, root);
		str->list = root;
		/* find */
		Return(loop_directory_files(str));
		/* rollback list */
		str->list = list;
		str->front = front;
	}
	/* rollback local */
	rollback_local(local, stack);

	return 0;
}

static int wild_file_directory_files(struct directory_struct *str, addr base)
{
	DIR *dir;
	const char *name;
	struct dirent *entry;

	Return(opendir_files(str->local, base, &dir));
	if (dir == NULL)
		return 0;
	for (;;) {
		/* The readdir_r() interface is deprecated. */
		entry = readdir(dir);
		if (entry == NULL)
			break;
		name = entry->d_name;
		Return(wild_push_directory_files(str, name));
	}
	if (closedir(dir))
		return fmte_("closedir() error.", NULL);

	return 0;
}

static int wild_directory_files(struct directory_struct *str)
{
	addr base;
	LocalStack stack;

	push_local(str->local, &stack);
	Return(make_directory_pathname_(str, &base));
	Return(wild_file_directory_files(str, base));
	rollback_local(str->local, stack);

	return 0;
}

static int inferiors_file_directory_files(struct directory_struct *str, addr name)
{
	addr base;
	LocalStack stack;

	push_local(str->local, &stack);
	Return(make_directory_pathname_(str, &base));
	Return(files_name_directory_files(str, base, name));
	rollback_local(str->local, stack);

	return 0;
}

static int inferiors_directory_files(struct directory_struct *str);
static int inferiors_push_directory_files(struct directory_struct *str,
		const char *name)
{
	int check;
	LocalRoot local;
	LocalStack stack;
	addr path, root, list, front;

	if (strcmp(name, ".") == 0)
		return 0;
	if (strcmp(name, "..") == 0)
		return 0;
	local = str->local;
	push_local(local, &stack);
	Return(string8_null_local_(local, &path, name));
	Return(inferiors_file_directory_files(str, path));
	/* directory check */
	Return(wild_check_directory_files(str, path, &check));
	if (check) {
		/* backup list */
		list = str->list;
		front = str->front;
		/* new list */
		copy_list_local_unsafe(local, &root, list);
		cons_local(local, &root, path, root);
		str->list = root;
		/* find */
		Return(inferiors_directory_files(str));
		/* rollback list */
		str->list = list;
		str->front = front;
	}
	/* rollback local */
	rollback_local(local, stack);

	return 0;
}

static int inferiors_find_directory_files(struct directory_struct *str, addr base)
{
	DIR *dir;
	const char *name;
	struct dirent *entry;

	Return(opendir_files(str->local, base, &dir));
	if (dir == NULL)
		return 0;
	for (;;) {
		/* The readdir_r() interface is deprecated. */
		entry = readdir(dir);
		if (entry == NULL)
			break;
		name = entry->d_name;
		Return(inferiors_push_directory_files(str, name));
	}
	if (closedir(dir))
		return fmte_("closedir() error.", NULL);

	return 0;
}

static int inferiors_directory_files(struct directory_struct *str)
{
	addr base;
	LocalStack stack;

	push_local(str->local, &stack);
	Return(make_directory_pathname_(str, &base));
	Return(inferiors_find_directory_files(str, base));
	rollback_local(str->local, stack);

	return 0;
}

static int loop_directory_files(struct directory_struct *str)
{
	addr name, check;

	/* file */
	if (str->front == Nil)
		return file_directory_files(str);

	/* wild */
	GetCons(str->front, &name, &str->front);
	GetConst(KEYWORD_WILD, &check);
	if (name == check)
		return wild_directory_files(str);

	/* wild-inferiors */
	GetConst(KEYWORD_WILD_INFERIORS, &check);
	if (name == check)
		return inferiors_directory_files(str);

	/* next */
	cons_local(str->local, &str->list, name, str->list);
	return loop_directory_files(str);
}

_g int directory_files_(Execute ptr, addr *ret, addr pos)
{
	struct directory_struct str;

	Return(init_directory_struct_(ptr, &str, pos));
	GetDirectoryPathname(str.pos, &str.front);
	Return(loop_directory_files(&str));
	return Result(ret, str.root);
}


/*
 *  probe-file
 */
static int probe_file_boolean(const char *file)
{
	struct stat st;
	return lstat(file, &st) == 0;
}

static int probe_file_run_files(Execute ptr, addr *ret, addr pos)
{
	int check;
	addr value;
	const char *str;

	/* filename */
	if (stringp(pos)) {
		Return(physical_pathname_local_(ptr, pos, &pos));
	}
	else {
		Return(physical_pathname_heap_(ptr, pos, &pos));
	}
	/* wildcard */
	Return(wild_pathname_boolean_(pos, Nil, &check));
	if (check) {
		GetConst(COMMON_PATHNAME, &value);
		return call_type_error_va_(ptr, pos, value,
				"Cannot probe-file the wildcard pathname ~S.", pos, NULL);
	}
	/* check */
	Return(name_pathname_local_(ptr, pos, &pos));
	Return(UTF8_buffer_clang_(ptr->local, &value, pos))
	if (value == Unbound)
		return fmte_("Cannot decode UTF-8 string ~S.", value, NULL);
	str = (const char *)posbodyr(value);
	*ret = probe_file_boolean(str)? T: Nil;

	return 0;
}

_g int probe_file_files_(Execute ptr, addr *ret, addr pos)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	Return(probe_file_run_files(ptr, ret, pos));
	rollback_local(local, stack);

	return 0;
}


/*
 *  ensure-directories-exist
 */
static int ensure_directires_exist_wild_files_(addr pos, int *ret)
{
	int check;
	addr field;

	/* directory */
	GetConst(KEYWORD_DIRECTORY, &field);
	Return(wild_pathname_boolean_(pos, field, &check));
	if (check)
		return Result(ret, 1);

	/* host */
	GetConst(KEYWORD_HOST, &field);
	Return(wild_pathname_boolean_(pos, field, &check));
	if (check)
		return Result(ret, 1);

	/* device */
	GetConst(KEYWORD_DEVICE, &field);
	Return(wild_pathname_boolean_(pos, field, &check));
	if (check)
		return Result(ret, 1);

	return Result(ret, 0);
}

static void merge_directory_pathname(LocalRoot local, addr *ret, addr pos, addr value)
{
	addr one;

	make_pathname_alloc(local, &one, RefLogicalPathname(pos));
	copylocal_pathname_array(local, pos, PATHNAME_INDEX_HOST, one);
	copylocal_pathname_array(local, pos, PATHNAME_INDEX_DEVICE, one);
	SetDirectoryPathname(one, value);
	*ret = one;
}

static int ensure_directories_exist_run_files(Execute ptr,
		addr *ret, addr pos, int verbose)
{
	LocalRoot local;
	LocalStack stack;
	const char *str;
	addr list, value, root, temp, result;
	mode_t mode;
	struct stat st;

	GetDirectoryPathname(pos, &list);
	if (! consp_getcons(list, &value, &list))
		return fmte_("Invalid pathname directory ~S.", pos, NULL);
	if (! consp(list))
		return fmte_("Invalid pathname directory ~S.", pos, NULL);
	result = Nil;
	local = ptr->local;
	conscar_local(local, &root, value);
	while (list != Nil) {
		GetCons(list, &value, &list);
		cons_local(local, &root, value, root);
		push_local(local, &stack);
		reverse_list_local_unsafe(local, &temp, root);
		merge_directory_pathname(local, &temp, pos, temp);
		/* directory check */
		Return(directory_name_pathname_local_(local, temp, &temp));
		Return(UTF8_buffer_clang_(local, &value, temp));
		if (value == Unbound)
			return fmte_("Cannot decode UTF-8 string ~S.", temp, NULL);
		str = (const char *)posbodyr(value);
		/* already exist */
		if (! lstat(str, &st)) {
			if (! S_ISDIR(st.st_mode))
				return fmte_("Cannot make directory ~S.", pos, NULL);
			rollback_local(local, stack);
			continue;
		}
		/* mkdir 0x755 */
		mode = S_IRUSR | S_IWUSR | S_IXUSR |
			S_IRGRP | S_IXGRP |
			S_IROTH | S_IXOTH;
		if (mkdir(str, mode))
			return fmte_("Cannot make directory ~S.", pos, NULL);
		result = T;
		/* verbose */
		if (verbose) {
			Return(format_stdout(ptr, "~&Creating directory: ~S~%", temp, NULL));
		}
		/* continue */
		rollback_local(local, stack);
	}

	return Result(ret, result);
}

_g int ensure_directories_exist_files_(Execute ptr,
		addr *ret1, addr *ret2, addr pos, int verbose)
{
	int check;
	addr value;
	LocalRoot local;
	LocalStack stack;

	/* filename */
	Return(physical_pathname_heap_(ptr, pos, &pos));
	/* wildcard */
	Return(ensure_directires_exist_wild_files_(pos, &check));
	if (check) {
		GetConst(COMMON_PATHNAME, &value);
		return call_type_error_va_(ptr, pos, value,
				"Cannot ENSURE-DIRECTIRIS-EXIST the wildcard pathname ~S.",
				pos, NULL);
	}
	/* loop */
	local = ptr->local;
	push_local(local, &stack);
	Return(ensure_directories_exist_run_files(ptr, ret2, pos, verbose));
	rollback_local(local, stack);
	/* result */
	return Result(ret1, pos);
}


/*
 *  file-author
 */
static int file_author_run_files(Execute ptr, addr *ret, addr pos)
{
	int check;
	LocalRoot local;
	char *str;
	addr value;
	long size;
	struct stat st;
	struct passwd pw, *ppw;

	/* filename */
	if (stringp(pos)) {
		Return(physical_pathname_local_(ptr, pos, &pos));
	}
	else {
		Return(physical_pathname_heap_(ptr, pos, &pos));
	}

	/* wildcard */
	Return(wild_pathname_boolean_(pos, Nil, &check));
	if (check) {
		GetConst(COMMON_PATHNAME, &value);
		return call_type_error_va_(ptr, pos, value,
				"Cannot file-author the wildcard pathname ~S.", pos, NULL);
	}
	/* file-author */
	local = ptr->local;
	Return(name_pathname_local_(ptr, pos, &value));
	if (UTF8_buffer_clang_(local, &value, value))
		return fmte_("Cannot decode UTF-8 string ~S.", pos, NULL);
	str = (char *)posbodyr(value);
	if (lstat(str, &st))
		return fmte_("The file ~S is not exist.", pos, NULL);
	size = sysconf(_SC_GETPW_R_SIZE_MAX);
	if (size < 0)
		size = 0x010000;
	str = (char *)lowlevel_local(local, (size_t)size);
	if (getpwuid_r(st.st_uid, &pw, str, (size_t)size, &ppw) || ppw == NULL)
		return Result(ret, Nil);
	else
		return string8_null_heap_(ret, ppw->pw_name);
}

_g int file_author_files_(Execute ptr, addr *ret, addr pos)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	Return(file_author_run_files(ptr, ret, pos));
	rollback_local(local, stack);

	return 0;
}


/*
 *  file-write-date
 */
static int file_write_date_run_files(Execute ptr, addr *ret, addr pos)
{
	int check;
	LocalRoot local;
	char *str;
	addr value, symbol;
	struct stat st;

	/* filename */
	if (stringp(pos)) {
		Return(physical_pathname_local_(ptr, pos, &pos));
	}
	else {
		Return(physical_pathname_heap_(ptr, pos, &pos));
	}

	/* wildcard */
	Return(wild_pathname_boolean_(pos, Nil, &check));
	if (check) {
		GetConst(COMMON_PATHNAME, &value);
		return call_type_error_va_(ptr, pos, value,
				"Cannot file-write-date the wildcard pathname ~S.", pos, NULL);
	}

	/* file-author */
	local = ptr->local;
	Return(name_pathname_local_(ptr, pos, &value));
	Return(UTF8_buffer_clang_(local, &value, value));
	if (value == Unbound)
		return fmte_("Cannot decode UTF-8 string ~S.", pos, NULL);
	str = (char *)posbodyr(value);
	if (lstat(str, &st))
		return fmte_("The file ~S is not exist.", pos, NULL);
	value = intsizeh((size_t)st.st_mtime);
	GetConst(SYSTEM_TIME1970, &symbol);
	GetValueSymbol(symbol, &symbol);
	Check(symbol == Unbound, "Unbound error, (must run build_pathnames).");
	plus_ii_real_common(local, symbol, value, ret);

	return 0;
}

_g int file_write_date_files_(Execute ptr, addr *ret, addr pos)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	Return(file_write_date_run_files(ptr, ret, pos));
	rollback_local(local, stack);

	return 0;
}


/*
 *  rename-file
 */
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
	if (check)
		return fmte_("Cannot rename wildcard pathname from ~S", from, NULL);
	Return(wild_pathname_boolean_(to, Nil, &check));
	if (check)
		return fmte_("Cannot rename wildcard pathname to ~S", to, NULL);
	/* filename */
	local = ptr->local;
	Return(name_pathname_local_(ptr, from, &value));
	Return(UTF8_buffer_clang_(local, &value, value));
	if (value == Unbound)
		return fmte_("Cannot decode UTF-8 string ~S.", from, NULL);
	str1 = (const char *)posbodyr(value);
	Return(name_pathname_local_(ptr, to, &value));
	Return(UTF8_buffer_clang_(local, &value, value));
	if (value == Unbound)
		return fmte_("Cannot decode UTF-8 string ~S.", to, NULL);
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

_g int rename_file_files_(Execute ptr,
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


/*
 *  delete-file
 */
static int delete_file_run_files(Execute ptr, addr pos, int errorp, int *ret)
{
	int check;
	LocalRoot local;
	addr file, value;
	const char *str;

	Return(physical_pathname_heap_(ptr, pos, &file));
	Return(wild_pathname_boolean_(file, Nil, &check));
	if (check)
		return fmte_("Cannot delete wildcard pathname ~S", pos, NULL);
	if (! pathname_file_p(file)) {
		if (errorp)
			return fmte_("The argument ~S is not a file.", pos, NULL);
		return Result(ret, 0);
	}
	/* filename */
	local = ptr->local;
	Return(name_pathname_local_(ptr, file, &value));
	Return(UTF8_buffer_clang_(local, &value, value));
	if (value == Unbound)
		return fmte_("Cannot decode UTF-8 string ~S.", file, NULL);
	str = (const char *)posbodyr(value);
	/* delete */
	if (unlink(str)) {
		if (errorp) {
			return call_simple_file_error_va_(ptr, pos,
					"Cannot delete ~S.", file, NULL);
		}
		return Result(ret, 0);
	}
	/* stream */
	if (streamp(pos)) {
		Return(close_stream_(pos));
	}

	return Result(ret, 1);
}

static int delete_file_errorp(Execute ptr, addr pos, int errorp, int *ret)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	Return(delete_file_run_files(ptr, pos, errorp, ret));
	rollback_local(local, stack);

	return 0;
}

_g int delete_file_files_(Execute ptr, addr pos)
{
	int check;
	return delete_file_errorp(ptr, pos, 1, &check);
}

_g int remove_file_common_(Execute ptr, addr pos, int errorp, int *ret)
{
	return delete_file_errorp(ptr, pos, errorp, ret);
}


/*
 *  remove-directory
 */
_g int remove_directory_common_(Execute ptr, addr pos, int errorp, int *ret)
{
	int check;
	LocalRoot local;
	addr file, value;
	const char *str;

	Return(physical_pathname_heap_(ptr, pos, &file));
	Return(wild_pathname_boolean_(file, Nil, &check));
	if (check)
		return fmte_("Cannot delete wildcard pathname ~S", pos, NULL);
	if (! pathname_directory_p(file)) {
		if (errorp)
			return fmte_("The argument ~S is not a directory.", pos, NULL);
		return Result(ret, 0);
	}
	/* filename */
	local = ptr->local;
	Return(name_pathname_local_(ptr, file, &value));
	Return(UTF8_buffer_clang_(local, &value, value));
	if (value == Unbound)
		return fmte_("Cannot decode UTF-8 string ~S.", file, NULL);
	str = (const char *)posbodyr(value);
	/* delete */
	if (rmdir(str)) {
		if (errorp) {
			return call_simple_file_error_va_(ptr, pos,
					"Cannot delete ~S.", file, NULL);
		}
		return Result(ret, 0);
	}
	/* stream */
	if (streamp(pos)) {
		Return(close_stream_(pos));
	}

	return Result(ret, 1);
}


/*
 *  truename
 */
_g int truename_files_(Execute ptr, addr file, addr *ret, int errorp)
{
	int check;
	addr pos;
	const unicode *u;
	char *str, *temp;
	size_t s32, s8;
	LocalRoot local;
	LocalStack stack;

	/* wild-check */
	Return(physical_pathname_heap_(ptr, file, &pos));
	Return(wild_pathname_boolean_(pos, Nil, &check));
	if (check) {
		if (! errorp)
			goto error_nil;
		return call_simple_file_error_va_(ptr, file,
				"TRUENAME don't allow the wildcard filename ~S.", file, NULL);
	}
	Return(name_pathname_heap_(ptr, pos, &pos));

	/* realpath */
	strvect_posbodylen(pos, &u, &s32);
	if (UTF32_length_utf8(u, s32, &s8)) {
		if (! errorp)
			goto error_nil;
		return call_simple_file_error_va_(ptr, file,
				"Invalid unicode string ~S.", pos, NULL);
	}
	local = ptr->local;
	push_local(local, &stack);
	str = (char *)lowlevel_local(local, s8 + 1UL);
	if (UTF32_make_utf8((byte *)str, u, s32)) {
		if (! errorp)
			goto error_nil_rollback;
		return call_simple_file_error_va_(ptr, file,
				"Invalid unicode string ~S.", pos, NULL);
	}
	str[s8] = 0;

	/* API */
	str = realpath(str, NULL); /* malloc */
	if (str == NULL) {
		if (! errorp)
			goto error_nil_rollback;
		return call_simple_file_error_va_(ptr, file,
				"Cannot find the TRUENAME ~S file.", file, NULL);
	}
	s8 = strlen(str) + 1UL;
	temp = (char *)lowlevel_local(local, s8);
	memcpy(temp, str, s8);
	free(str);

	/* make-pathname */
	Return(string8_null_heap_(&pos, str));
	Return(pathname_designer_heap_(ptr, pos, ret));
	rollback_local(local, stack);
	return 0;

error_nil_rollback:
	rollback_local(local, stack);
error_nil:
	return Result(ret, Nil);
}

