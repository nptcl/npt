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
#include "copy.h"
#include "encode.h"
#include "format.h"
#include "local.h"
#include "integer.h"
#include "pathname.h"
#include "sequence.h"
#include "stream.h"
#include "strtype.h"
#include "symbol.h"


/*
 *  directory
 */
struct directory_struct {
	Execute ptr;
	LocalRoot local;
	addr pos, list, front, root;
};

static void init_directory_struct(Execute ptr, struct directory_struct *str, addr pos)
{
	clearpoint(str);
	str->ptr = ptr;
	str->local = ptr->local;
	pathname_designer_heap(ptr, pos, &str->pos);
	str->list = str->root = str->front = Nil;
}

static void make_list_directory_pathname(struct directory_struct *str,
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
	SetPathname(one, PATHNAME_INDEX_DIRECTORY, list);
	/* version */
	GetConst(KEYWORD_UNSPECIFIC, &version);
	SetPathname(one, PATHNAME_INDEX_VERSION, version);
	/* result */
	physical_pathname_local(str->ptr, one, ret);
}

static void make_directory_pathname(struct directory_struct *str, addr *ret)
{
	make_list_directory_pathname(str, ret, str->list);
}

static DIR *opendir_files(LocalRoot local, addr pos)
{
	addr name;
	LocalStack stack;
	const char *clang;
	DIR *dir;

	push_local(local, &stack);
	directory_name_pathname_local(local, pos, &pos);
	if (UTF8_buffer_clang(local, &name, pos))
		fmte("Cannot convert ~S to UTF-8 string.", pos, NULL);
	clang = (const char *)posbodyr(name);
	dir = opendir(clang);
	rollback_local(local, stack);

	return dir;
}

static void merge_directory_files(LocalRoot local, addr path, addr defaults)
{
	copylocal_pathname_array(local, defaults, PATHNAME_INDEX_HOST, path);
	copylocal_pathname_array(local, defaults, PATHNAME_INDEX_DEVICE, path);
	copylocal_pathname_array(local, defaults, PATHNAME_INDEX_DIRECTORY, path);
}

static int directoryp_directory_files(Execute ptr, addr file)
{
	int check;
	LocalRoot local;
	LocalStack stack;
	const char *body;
	addr pos;
	struct stat st;

	local = ptr->local;
	push_local(local, &stack);
	name_pathname_local(ptr, file, &pos);
	if (UTF8_buffer_clang(local, &pos, pos))
		fmte("Cannot convert ~S to UTF-8 string.", file, NULL);
	body = (const char *)posbodyr(pos);
	check = (! lstat(body, &st)) && S_ISDIR(st.st_mode);
	rollback_local(local, stack);

	return check;
}

static void files_path_directory_files(struct directory_struct *str,
		addr path, addr name, addr base, addr *ret)
{
	addr list;
	LocalRoot local;

	if (directoryp_directory_files(str->ptr, path)) {
		local = str->local;
		copy_list_local_unsafe(local, &list, str->list);
		cons_local(local, &list, name, list);
		make_list_directory_pathname(str, &path, list);
		SetPathname(path, PATHNAME_INDEX_NAME, Nil);
		SetPathname(path, PATHNAME_INDEX_TYPE, Nil);
	}
	*ret = path;
}

static void files_name_directory_files(struct directory_struct *str,
		addr base, addr name)
{
	Execute ptr;
	addr path;

	ptr = str->ptr;
	pathname_designer_local(ptr, name, &path);
	merge_directory_files(str->local, path, base);
	if (wildcard_pathname(path, str->pos, 1)) {
		/* push heap */
		files_path_directory_files(str, path, name, base, &path);
		pathname_designer_heap(ptr, path, &path);
		cons_heap(&str->root, path, str->root);
	}
}

static void files_push_directory_files(struct directory_struct *str,
		addr base, const char *name)
{
	LocalRoot local;
	LocalStack stack;
	addr path;

	if (strcmp(name, ".") == 0) return;
	if (strcmp(name, "..") == 0) return;
	local = str->local;
	push_local(local, &stack);
	string8_null_local(local, &path, name);
	files_name_directory_files(str, base, path);
	rollback_local(local, stack);
}

static void files_directory_files(struct directory_struct *str, addr base)
{
	DIR *dir;
	const char *name;
	struct dirent *entry;

	dir = opendir_files(str->local, base);
	if (dir == NULL)
		return;
	for (;;) {
		/* The readdir_r() interface is deprecated. */
		entry = readdir(dir);
		if (entry == NULL)
			break;
		name = entry->d_name;
		files_push_directory_files(str, base, name);
	}
	if (closedir(dir))
		fmte("closedir() error.", NULL);
}

static void file_directory_files(struct directory_struct *str)
{
	addr base;
	LocalStack stack;

	push_local(str->local, &stack);
	make_directory_pathname(str, &base);
	files_directory_files(str, base);
	rollback_local(str->local, stack);
}

static int wild_check_directory_files(struct directory_struct *str, addr name)
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
	make_list_directory_pathname(str, &pos, pos);
	directory_name_pathname_local(local, pos, &pos);
	if (UTF8_buffer_clang(local, &value, pos))
		fmte("Cannot convert ~S to UTF-8 string.", pos, NULL);
	ptr = (const char *)posbodyr(value);
	check = (! lstat(ptr, &st)) && S_ISDIR(st.st_mode);
	rollback_local(local, stack);

	return check;
}

static void loop_directory_files(struct directory_struct *str);
static void wild_push_directory_files(struct directory_struct *str, const char *name)
{
	LocalRoot local;
	LocalStack stack;
	addr path, root, list, front;

	if (strcmp(name, ".") == 0) return;
	if (strcmp(name, "..") == 0) return;
	local = str->local;
	push_local(local, &stack);
	string8_null_local(local, &path, name);
	/* directory check */
	if (wild_check_directory_files(str, path)) {
		/* backup list */
		list = str->list;
		front = str->front;
		/* new list */
		copy_list_local_unsafe(local, &root, list);
		cons_local(local, &root, path, root);
		str->list = root;
		/* find */
		loop_directory_files(str);
		/* rollback list */
		str->list = list;
		str->front = front;
	}
	/* rollback local */
	rollback_local(local, stack);
}

static void wild_file_directory_files(struct directory_struct *str, addr base)
{
	DIR *dir;
	const char *name;
	struct dirent *entry;

	dir = opendir_files(str->local, base);
	if (dir == NULL)
		return;
	for (;;) {
		/* The readdir_r() interface is deprecated. */
		entry = readdir(dir);
		if (entry == NULL)
			break;
		name = entry->d_name;
		wild_push_directory_files(str, name);
	}
	if (closedir(dir))
		fmte("closedir() error.", NULL);
}

static void wild_directory_files(struct directory_struct *str)
{
	addr base;
	LocalStack stack;

	push_local(str->local, &stack);
	make_directory_pathname(str, &base);
	wild_file_directory_files(str, base);
	rollback_local(str->local, stack);
}

static void inferiors_file_directory_files(struct directory_struct *str, addr name)
{
	addr base;
	LocalStack stack;

	push_local(str->local, &stack);
	make_directory_pathname(str, &base);
	files_name_directory_files(str, base, name);
	rollback_local(str->local, stack);
}

static void inferiors_directory_files(struct directory_struct *str);
static void inferiors_push_directory_files(struct directory_struct *str,
		const char *name)
{
	LocalRoot local;
	LocalStack stack;
	addr path, root, list, front;

	if (strcmp(name, ".") == 0) return;
	if (strcmp(name, "..") == 0) return;
	local = str->local;
	push_local(local, &stack);
	string8_null_local(local, &path, name);
	inferiors_file_directory_files(str, path);
	/* directory check */
	if (wild_check_directory_files(str, path)) {
		/* backup list */
		list = str->list;
		front = str->front;
		/* new list */
		copy_list_local_unsafe(local, &root, list);
		cons_local(local, &root, path, root);
		str->list = root;
		/* find */
		inferiors_directory_files(str);
		/* rollback list */
		str->list = list;
		str->front = front;
	}
	/* rollback local */
	rollback_local(local, stack);
}

static void inferiors_find_directory_files(struct directory_struct *str, addr base)
{
	DIR *dir;
	const char *name;
	struct dirent *entry;

	dir = opendir_files(str->local, base);
	if (dir == NULL)
		return;
	for (;;) {
		/* The readdir_r() interface is deprecated. */
		entry = readdir(dir);
		if (entry == NULL)
			break;
		name = entry->d_name;
		inferiors_push_directory_files(str, name);
	}
	if (closedir(dir))
		fmte("closedir() error.", NULL);
}

static void inferiors_directory_files(struct directory_struct *str)
{
	addr base;
	LocalStack stack;

	push_local(str->local, &stack);
	make_directory_pathname(str, &base);
	inferiors_find_directory_files(str, base);
	rollback_local(str->local, stack);
}

static void loop_directory_files(struct directory_struct *str)
{
	addr name, check;

	/* file */
	if (str->front == Nil) {
		file_directory_files(str);
		return;
	}

	/* wild */
	GetCons(str->front, &name, &str->front);
	GetConst(KEYWORD_WILD, &check);
	if (name == check) {
		wild_directory_files(str);
		return;
	}

	/* wild-inferiors */
	GetConst(KEYWORD_WILD_INFERIORS, &check);
	if (name == check) {
		inferiors_directory_files(str);
		return;
	}

	/* next */
	cons_local(str->local, &str->list, name, str->list);
	loop_directory_files(str);
}

_g void directory_files(Execute ptr, addr *ret, addr pos)
{
	struct directory_struct str;

	init_directory_struct(ptr, &str, pos);
	GetPathname(str.pos, PATHNAME_INDEX_DIRECTORY, &str.front);
	loop_directory_files(&str);
	*ret = str.root;
}


/*
 *  probe-file
 */
static int probe_file_boolean(const char *file)
{
	struct stat st;
	return lstat(file, &st) == 0;
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
		fmte("Cannot decode UTF-8 string ~S.", pos, NULL);
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


/*
 *  ensure-directories-exist
 */
static int ensure_directires_exist_wild_files(addr pos)
{
	addr field;

	GetConst(KEYWORD_DIRECTORY, &field);
	if (wild_pathname_boolean(pos, field)) return 1;
	GetConst(KEYWORD_HOST, &field);
	if (wild_pathname_boolean(pos, field)) return 1;
	GetConst(KEYWORD_DEVICE, &field);
	if (wild_pathname_boolean(pos, field)) return 1;

	return 0;
}

static void merge_directory_pathname(LocalRoot local, addr *ret, addr pos, addr value)
{
	addr one;

	make_pathname_alloc(local, &one, RefLogicalPathname(pos));
	copylocal_pathname_array(local, pos, PATHNAME_INDEX_HOST, one);
	copylocal_pathname_array(local, pos, PATHNAME_INDEX_DEVICE, one);
	SetPathname(one, PATHNAME_INDEX_DIRECTORY, value);
	*ret = one;
}

static void ensure_directories_exist_run_files(Execute ptr,
		addr *ret, addr pos, int verbose)
{
	LocalRoot local;
	LocalStack stack;
	const char *str;
	addr list, value, root, temp, result;
	mode_t mode;
	struct stat st;

	GetPathname(pos, PATHNAME_INDEX_DIRECTORY, &list);
	if (! consp(list))
		fmte("Invalid pathname directory ~S.", pos, NULL);
	GetCons(list, &value, &list);
	if (! consp(list))
		fmte("Invalid pathname directory ~S.", pos, NULL);
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
		directory_name_pathname_local(local, temp, &temp);
		if (UTF8_buffer_clang(local, &value, temp))
			fmte("Cannot decode UTF-8 string ~S.", value, NULL);
		str = (const char *)posbodyr(value);
		/* already exist */
		if (! lstat(str, &st)) {
			if (! S_ISDIR(st.st_mode))
				fmte("Cannot make directory ~S.", pos, NULL);
			rollback_local(local, stack);
			continue;
		}
		/* mkdir 0x755 */
		mode = S_IRUSR | S_IWUSR | S_IXUSR |
			S_IRGRP | S_IXGRP |
			S_IROTH | S_IXOTH;
		if (mkdir(str, mode))
			fmte("Cannot make directory ~S.", pos, NULL);
		result = T;
		/* verbose */
		if (verbose)
			format_stdout(ptr, "~&Creating directory: ~S~%", temp, NULL);
		/* continue */
		rollback_local(local, stack);
	}
	*ret = result;
}

_g void ensure_directories_exist_files(Execute ptr,
		addr *ret1, addr *ret2, addr pos, int verbose)
{
	addr value;
	LocalRoot local;
	LocalStack stack;

	/* filename */
	physical_pathname_heap(ptr, pos, &pos);
	/* wildcard */
	if (ensure_directires_exist_wild_files(pos)) {
		GetConst(COMMON_PATHNAME, &value);
		type_error_stdarg(pos, value,
				"Cannot ENSURE-DIRECTIRIS-EXIST the wildcard pathname ~S.",
				pos, NULL);
	}
	/* loop */
	local = ptr->local;
	push_local(local, &stack);
	ensure_directories_exist_run_files(ptr, ret2, pos, verbose);
	rollback_local(local, stack);
	/* result */
	*ret1 = pos;
}


/*
 *  file-author
 */
static void file_author_run_files(Execute ptr, addr *ret, addr pos)
{
	LocalRoot local;
	char *str;
	addr value;
	long size;
	struct stat st;
	struct passwd pw, *ppw;

	/* filename */
	if (stringp(pos))
		physical_pathname_local(ptr, pos, &pos);
	else
		physical_pathname_heap(ptr, pos, &pos);
	/* wildcard */
	if (wild_pathname_boolean(pos, Nil)) {
		GetConst(COMMON_PATHNAME, &value);
		type_error_stdarg(pos, value,
				"Cannot file-authro the wildcard pathname ~S.", pos, NULL);
	}
	/* file-author */
	local = ptr->local;
	name_pathname_local(ptr, pos, &value);
	if (UTF8_buffer_clang(local, &value, value))
		fmte("Cannot decode UTF-8 string ~S.", pos, NULL);
	str = (char *)posbodyr(value);
	if (lstat(str, &st))
		fmte("The file ~S is not exist.", pos, NULL);
	size = sysconf(_SC_GETPW_R_SIZE_MAX);
	if (size < 0)
		size = 0x010000;
	str = (char *)lowlevel_local(local, (size_t)size);
	if (getpwuid_r(st.st_uid, &pw, str, (size_t)size, &ppw) || ppw == NULL)
		*ret = Nil;
	else
		string8_null_heap(ret, ppw->pw_name);
}

_g void file_author_files(Execute ptr, addr *ret, addr pos)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	file_author_run_files(ptr, ret, pos);
	rollback_local(local, stack);
}


/*
 *  file-write-date
 */
static void file_write_date_run_files(Execute ptr, addr *ret, addr pos)
{
	LocalRoot local;
	char *str;
	addr value, symbol;
	struct stat st;

	/* filename */
	if (stringp(pos))
		physical_pathname_local(ptr, pos, &pos);
	else
		physical_pathname_heap(ptr, pos, &pos);
	/* wildcard */
	if (wild_pathname_boolean(pos, Nil)) {
		GetConst(COMMON_PATHNAME, &value);
		type_error_stdarg(pos, value,
				"Cannot write-date the wildcard pathname ~S.", pos, NULL);
	}
	/* file-author */
	local = ptr->local;
	name_pathname_local(ptr, pos, &value);
	if (UTF8_buffer_clang(local, &value, value))
		fmte("Cannot decode UTF-8 string ~S.", pos, NULL);
	str = (char *)posbodyr(value);
	if (lstat(str, &st))
		fmte("The file ~S is not exist.", pos, NULL);
	value = intsizeh((size_t)st.st_mtime);
	GetConst(SYSTEM_TIME1970, &symbol);
	GetValueSymbol(symbol, &symbol);
	Check(symbol == Unbound, "Unbound error, (must run build_pathnames).");
	plus_ii_real_common(local, symbol, value, ret);
}

_g void file_write_date_files(Execute ptr, addr *ret, addr pos)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	file_write_date_run_files(ptr, ret, pos);
	rollback_local(local, stack);
}


/*
 *  rename-file
 */
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
		fmte("Cannot rename wildcard pathname from ~S", from, NULL);
	if (wild_pathname_boolean(to, Nil))
		fmte("Cannot rename wildcard pathname to ~S", to, NULL);
	/* filename */
	local = ptr->local;
	name_pathname_local(ptr, from, &value);
	if (UTF8_buffer_clang(local, &value, value))
		fmte("Cannot decode UTF-8 string ~S.", from, NULL);
	str1 = (const char *)posbodyr(value);
	name_pathname_local(ptr, to, &value);
	if (UTF8_buffer_clang(local, &value, value))
		fmte("Cannot decode UTF-8 string ~S.", to, NULL);
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


/*
 *  delete-file
 */
static int delete_file_run_files(Execute ptr, addr pos, int errorp)
{
	LocalRoot local;
	addr file, value;
	const char *str;

	physical_pathname_heap(ptr, pos, &file);
	if (wild_pathname_boolean(file, Nil))
		fmte("Cannot delete wildcard pathname ~S", pos, NULL);
	if (! pathname_file_p(file)) {
		if (errorp)
			fmte("The argument ~S is not a file.", pos, NULL);
		return 0;
	}
	/* filename */
	local = ptr->local;
	name_pathname_local(ptr, file, &value);
	if (UTF8_buffer_clang(local, &value, value))
		fmte("Cannot decode UTF-8 string ~S.", file, NULL);
	str = (const char *)posbodyr(value);
	/* delete */
	if (unlink(str)) {
		if (errorp)
			simple_file_error_stdarg(pos, "Cannot delete ~S.", file, NULL);
		return 0;
	}
	/* stream */
	if (streamp(pos))
		close_stream(pos);

	return 1;
}

static int delete_file_errorp(Execute ptr, addr pos, int errorp)
{
	int check;
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	check = delete_file_run_files(ptr, pos, errorp);
	rollback_local(local, stack);

	return check;
}

_g void delete_file_files(Execute ptr, addr pos)
{
	delete_file_errorp(ptr, pos, 1);
}

_g int remove_file_common(Execute ptr, addr pos, int errorp)
{
	return delete_file_errorp(ptr, pos, errorp);
}


/*
 *  truename
 */
_g void truename_files(Execute ptr, addr file, addr *ret, int errorp)
{
	addr pos;
	const unicode *u;
	char *str, *temp;
	size_t s32, s8;
	LocalRoot local;
	LocalStack stack;

	/* wild-check */
	physical_pathname_heap(ptr, file, &pos);
	if (wild_pathname_boolean(pos, Nil)) {
		if (! errorp)
			goto error_nil;
		simple_file_error_stdarg(file,
				"TRUENAME don't allow the wildcard filename ~S.", file, NULL);
		return;
	}
	name_pathname_heap(ptr, pos, &pos);

	/* realpath */
	strvect_posbodylen(pos, &u, &s32);
	if (UTF32_length_utf8(u, s32, &s8)) {
		if (! errorp)
			goto error_nil;
		simple_file_error_stdarg(file, "Invalid unicode string ~S.", pos, NULL);
		return;
	}
	local = ptr->local;
	push_local(local, &stack);
	str = (char *)lowlevel_local(local, s8 + 1UL);
	if (UTF32_make_utf8((byte *)str, u, s32)) {
		if (! errorp)
			goto error_nil_rollback;
		simple_file_error_stdarg(file, "Invalid unicode string ~S.", pos, NULL);
		goto error;
	}
	str[s8] = 0;

	/* API */
	str = realpath(str, NULL); /* malloc */
	if (str == NULL) {
		if (! errorp)
			goto error_nil_rollback;
		simple_file_error_stdarg(file, "Cannot find the TRUENAME ~S file.", file, NULL);
		goto error;
	}
	s8 = strlen(str) + 1UL;
	temp = (char *)lowlevel_local(local, s8);
	memcpy(temp, str, s8);
	free(str);

	/* make-pathname */
	string8_null_heap(&pos, str);
	pathname_designer_heap(ptr, pos, ret);

error:
	rollback_local(local, stack);
	return;

error_nil_rollback:
	rollback_local(local, stack);
error_nil:
	*ret = Nil;
}


/*
 *  remove-directory
 */
_g int remove_directory_common(Execute ptr, addr pos, int errorp)
{
	LocalRoot local;
	addr file, value;
	const char *str;

	physical_pathname_heap(ptr, pos, &file);
	if (wild_pathname_boolean(file, Nil))
		fmte("Cannot delete wildcard pathname ~S", pos, NULL);
	if (! pathname_directory_p(file)) {
		if (errorp)
			fmte("The argument ~S is not a directory.", pos, NULL);
		return 1;
	}
	/* filename */
	local = ptr->local;
	name_pathname_local(ptr, file, &value);
	if (UTF8_buffer_clang(local, &value, value))
		fmte("Cannot decode UTF-8 string ~S.", file, NULL);
	str = (const char *)posbodyr(value);
	/* delete */
	if (rmdir(str)) {
		if (errorp)
			simple_file_error_stdarg(pos, "Cannot delete ~S.", file, NULL);
		return 0;
	}
	/* stream */
	if (streamp(pos))
		close_stream(pos);

	return 1;
}

