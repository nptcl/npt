/*
 *  files-posix
 */
#include <dirent.h>
#include <pwd.h>
#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include "condition.h"
#include "cons.h"
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

static void files_name_directory_files(struct directory_struct *str,
		addr base, addr name)
{
	Execute ptr;

	ptr = str->ptr;
	pathname_designer_local(ptr, name, &name);
	merge_directory_files(str->local, name, base);
	if (wildcard_pathname(name, str->pos, 0)) {
		/* push heap */
		pathname_designer_heap(ptr, name, &name);
		cons_heap(&str->root, name, str->root);
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
	else {
		inferiors_file_directory_files(str, path);
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

void directory_files(Execute ptr, addr *ret, addr pos)
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
static void probe_file_run_files(Execute ptr, addr *ret, addr pos)
{
	addr value;
	const char *str;
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
				"Cannot probe-file the wildcard pathname ~S.", pos, NULL);
	}
	/* check */
	name_pathname_local(ptr, pos, &pos);
	if (UTF8_buffer_clang(ptr->local, &pos, pos))
		fmte("Cannot decode UTF-8 string ~S.", pos, NULL);
	str = (const char *)posbodyr(pos);
	*ret = lstat(str, &st)? Nil: T;
}

void probe_file_files(Execute ptr, addr *ret, addr pos)
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
			format("~&Creating directory: ~S~%", temp, NULL);
		/* continue */
		rollback_local(local, stack);
	}
	*ret = result;
}

void ensure_directories_exist_files(Execute ptr,
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

void file_author_files(Execute ptr, addr *ret, addr pos)
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

void file_write_date_files(Execute ptr, addr *ret, addr pos)
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
	addr file, from, value;
	const char *str1, *str2;

	pathname_designer_heap(ptr, pos, &file);
	physical_pathname_heap(ptr, file, &from);
	physical_pathname_heap(ptr, to, &to);
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
	/* rename */
	if (rename(str1, str2))
		file_error(file);
	/* stream */
	if (streamp(pos))
		SetPathnameStream(pos, to);
	/* result */
	*ret1 = file;
	*ret2 = from;
	*ret3 = to;
}

void rename_file_files(Execute ptr,
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
static void delete_file_run_files(Execute ptr, addr pos)
{
	LocalRoot local;
	addr file, value;
	const char *str;

	physical_pathname_heap(ptr, pos, &file);
	if (wild_pathname_boolean(file, Nil))
		fmte("Cannot delete wildcard pathname ~S", pos, NULL);
	/* filename */
	local = ptr->local;
	name_pathname_local(ptr, file, &value);
	if (UTF8_buffer_clang(local, &value, value))
		fmte("Cannot decode UTF-8 string ~S.", file, NULL);
	str = (const char *)posbodyr(value);
	/* delete */
	if (unlink(str))
		file_error(file);
	/* stream */
	if (streamp(pos))
		close_stream(pos);
}

void delete_file_files(Execute ptr, addr pos)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	delete_file_run_files(ptr, pos);
	rollback_local(local, stack);
}

