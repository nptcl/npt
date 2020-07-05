/*
 *  files-windows
 */
#include <aclapi.h>
#include <pathcch.h>
#include <shlwapi.h>
#include <windows.h>
#include "bignum.h"
#include "bignum_object.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "copy.h"
#include "encode.h"
#include "format.h"
#include "integer.h"
#include "pathname_common.h"
#include "pathname_object.h"
#include "pathname.h"
#include "sequence.h"
#include "stream.h"
#include "strtype.h"
#include "strvect.h"
#include "unicode.h"

#pragma comment(lib, "advapi32.lib")
#pragma comment(lib, "pathcch.lib")
#pragma comment(lib, "shlwapi.lib")

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
	SetDirectoryPathname(one, list);
	/* version */
	GetConst(KEYWORD_UNSPECIFIC, &version);
	SetVersionPathname(one, version);
	/* result */
	physical_pathname_local(str->ptr, one, ret);
}

static void make_directory_pathname(struct directory_struct *str, addr *ret)
{
	make_list_directory_pathname(str, ret, str->list);
}

static void find_pathname_files(LocalRoot local, addr pos, addr *ret)
{
	unicode *data1, *data2;
	addr one;
	size_t size, i;

	strvect_length(pos, &size);
	strvect_local(local, &one, size + 3ULL);
	data1 = (unicode *)PtrStringUnicode(pos);
	data2 = (unicode *)PtrStringUnicode(one);
	for (i = 0; i < size; i++)
		data2[i] = data1[i];
	data2[size + 0] = '*';
	data2[size + 1] = '.';
	data2[size + 2] = '*';
	*ret = one;
}

static HANDLE opendir_files(LocalRoot local, addr pos, LPWIN32_FIND_DATAW data)
{
	addr name;
	LocalStack stack;
	const WCHAR *clang;
	HANDLE hFind;

	push_local(local, &stack);
	directory_name_pathname_local(local, pos, &pos);
	find_pathname_files(local, pos, &pos);
	if (UTF16_buffer_clang(local, &name, pos))
		fmte("Cannot convert ~S to UTF-16 string.", pos, NULL);
	clang = (const WCHAR *)posbodyr(name);
	hFind = FindFirstFileW(clang, data);
	rollback_local(local, stack);

	return hFind;
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
	const WCHAR *body;
	addr pos;

	local = ptr->local;
	push_local(local, &stack);
	name_pathname_local(ptr, file, &pos);
	if (UTF16_buffer_clang(local, &pos, pos))
		fmte("Cannot convert ~S to UTF-16 string.", file, NULL);
	body = (const WCHAR *)posbodyr(pos);
	check = PathIsDirectory(body);
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
		SetNamePathname(path, Nil);
		SetTypePathname(path, Nil);
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
		addr base, const WCHAR *name)
{
	LocalRoot local;
	LocalStack stack;
	addr path;

	if (wcscmp(name, L".") == 0) return;
	if (wcscmp(name, L"..") == 0) return;
	local = str->local;
	push_local(local, &stack);
	string16_null_local(local, &path, (const byte16 *)name);
	files_name_directory_files(str, base, path);
	rollback_local(local, stack);
}

static void files_directory_files(struct directory_struct *str, addr base)
{
	HANDLE hFind;
	WIN32_FIND_DATAW data;

	hFind = opendir_files(str->local, base, &data);
	if (hFind == INVALID_HANDLE_VALUE)
		return;
	files_push_directory_files(str, base, data.cFileName);
	while (FindNextFileW(hFind, &data))
		files_push_directory_files(str, base, data.cFileName);
	if (FindClose(hFind) == 0)
		fmte("FindClose error.", NULL);
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
	const WCHAR *ptr;
	addr pos, value;

	local = str->local;
	push_local(local, &stack);
	copy_list_local_unsafe(local, &pos, str->list);
	cons_local(local, &pos, name, pos);
	make_list_directory_pathname(str, &pos, pos);
	directory_name_pathname_local(local, pos, &pos);
	if (UTF16_buffer_clang(local, &value, pos))
		fmte("Cannot convert ~S to UTF-16 string.", pos, NULL);
	ptr = (const WCHAR *)posbodyr(value);
	check = PathIsDirectory(ptr);
	rollback_local(local, stack);

	return check;
}

static void loop_directory_files(struct directory_struct *str);
static void wild_push_directory_files(struct directory_struct *str, const WCHAR *name)
{
	LocalRoot local;
	LocalStack stack;
	addr path, root, list, front;

	if (wcscmp(name, L".") == 0) return;
	if (wcscmp(name, L"..") == 0) return;
	local = str->local;
	push_local(local, &stack);
	string16_null_local(local, &path, (const byte16 *)name);
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
	HANDLE hFind;
	WIN32_FIND_DATAW data;

	hFind = opendir_files(str->local, base, &data);
	if (hFind == INVALID_HANDLE_VALUE)
		return;
	wild_push_directory_files(str, data.cFileName);
	while (FindNextFileW(hFind, &data))
		wild_push_directory_files(str, data.cFileName);
	if (FindClose(hFind) == 0)
		fmte("FindClose error.", NULL);
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
		const WCHAR *name)
{
	LocalRoot local;
	LocalStack stack;
	addr path, root, list, front;

	if (wcscmp(name, L".") == 0) return;
	if (wcscmp(name, L"..") == 0) return;
	local = str->local;
	push_local(local, &stack);
	string16_null_local(local, &path, (const byte16 *)name);
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
	HANDLE hFind;
	WIN32_FIND_DATAW data;

	hFind = opendir_files(str->local, base, &data);
	if (hFind == INVALID_HANDLE_VALUE)
		return;
	inferiors_push_directory_files(str, data.cFileName);
	while (FindNextFileW(hFind, &data))
		inferiors_push_directory_files(str, data.cFileName);
	if (FindClose(hFind) == 0)
		fmte("FindClose error.", NULL);
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
	GetDirectoryPathname(str.pos, &str.front);
	loop_directory_files(&str);
	*ret = str.root;
}


/*
 *  probe-file
 */
static int probe_file_boolean(const WCHAR *file)
{
	return PathFileExistsW(file) == TRUE;
}

static void probe_file_run_files(Execute ptr, addr *ret, addr pos)
{
	addr value;
	const WCHAR *str;

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
	if (UTF16_buffer_clang(ptr->local, &pos, pos))
		fmte("Cannot decode UTF-16 string ~S.", pos, NULL);
	str = (const WCHAR *)posbodyr(pos);
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
	SetDirectoryPathname(one, value);
	*ret = one;
}

static void ensure_directories_exist_run_files(Execute ptr,
		addr *ret, addr pos, int verbose)
{
	LocalRoot local;
	LocalStack stack;
	const WCHAR *str;
	addr list, value, root, temp, result;

	GetDirectoryPathname(pos, &list);
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
		if (UTF16_buffer_clang(local, &value, temp))
			fmte("Cannot decode UTF-16 string ~S.", value, NULL);
		str = (const WCHAR *)posbodyr(value);
		/* already exist */
		if (PathFileExists(str)) {
			if (! PathIsDirectory(str))
				fmte("Cannot make directory ~S.", pos, NULL);
			rollback_local(local, stack);
			continue;
		}
		/* CreateDirectory */
		if (CreateDirectoryW(str, NULL) == 0)
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
_g void file_author_files(Execute ptr, addr *ret, addr pos)
{
	BOOL result;
	DWORD size1, size2;
	LocalRoot local;
	WCHAR *str, *name, *domain;
	addr value;
	PSID owner;
	SID_NAME_USE use;
	PSECURITY_DESCRIPTOR psd;

	/* filename */
	if (stringp(pos))
		physical_pathname_local(ptr, pos, &pos);
	else
		physical_pathname_heap(ptr, pos, &pos);
	/* wildcard */
	if (wild_pathname_boolean(pos, Nil)) {
		GetConst(COMMON_PATHNAME, &value);
		type_error_stdarg(pos, value,
				"Cannot file-authorthe wildcard pathname ~S.", pos, NULL);
	}

	/* UTF-16 */
	local = ptr->local;
	name_pathname_local(ptr, pos, &value);
	if (UTF16_buffer_clang(local, &value, value))
		fmte("Cannot decode UTF-16 string ~S.", pos, NULL);
	str = (WCHAR *)posbodyr(value);

	/* GetFileSecurity */
	size1 = 0;
	psd = NULL;
	result = GetFileSecurityW(str, OWNER_SECURITY_INFORMATION, psd, size1, &size1);
	psd = (PSECURITY_DESCRIPTOR)lowlevel_local(local, size1);
	result = GetFileSecurityW(str, OWNER_SECURITY_INFORMATION, psd, size1, &size1);
	if (result == 0)
		goto finish_nil;

	/* GetSecurityDescriptorOwner */
	result = 0;
	result = GetSecurityDescriptorOwner(psd, &owner, &result);
	if (result == 0)
		fmte("GetSecurityDescriptorOwner error.", NULL);

	/* LookupAccountSid */
	use = SidTypeUnknown;
	size1 = size2 = 1;
	name = domain = NULL;
	result = LookupAccountSidW(NULL, owner, name, &size1, domain, &size2, &use);
	name = (WCHAR *)lowlevel_local(local, size1 * sizeof(DWORD));
	domain = (WCHAR *)lowlevel_local(local, size2 * sizeof(DWORD));
	result = LookupAccountSidW(NULL, owner, name, &size1, domain, &size2, &use);
	if (result == FALSE) {
		if (GetLastError() == ERROR_NONE_MAPPED)
			goto finish_nil;
		fmte("LookupAccountSid error.", NULL);
	}

	/* result */
	string16_null_heap(ret, (const byte16 *)name);
	return;

finish_nil:
	*ret = Nil;
}


/*
 *  file-write-date
 */
static int file_write_date_base_files(addr *ret, const FILETIME *file)
{
	ULONGLONG a, b;

	/* The FILETIME structure is a 64-bit value representing
	 * the number of 100-nanosecond intervals since January 1, 1601.
	 */
	a = (((1900ULL - 1601ULL) * 365ULL) + 74ULL - 2ULL) *
		24ULL * 60ULL * 60ULL * 10000000ULL;
	b = (((ULONGLONG)file->dwHighDateTime) << 32ULL) | file->dwLowDateTime;
	if (b < a)
		return 1;
	b = (b - a) / 10000000ULL;
#ifdef LISP_64BIT
	*ret = intsizeh((size_t)b);
#else
	bignum_value2_heap(ret, signplus_bignum,
			(fixed)(b >> 32ULL),
			(fixed)(b & 0xFFFFFFFFULL));
	bignum_result_heap(*ret, ret);
#endif

	return 0;
}

static void file_write_date_run_files(Execute ptr, addr *ret, addr pos)
{
	LocalRoot local;
	HANDLE hFind;
	const WCHAR *str;
	addr value;
	WIN32_FIND_DATAW data;

	/* filename */
	if (stringp(pos))
		physical_pathname_local(ptr, pos, &pos);
	else
		physical_pathname_heap(ptr, pos, &pos);
	/* wildcard */
	if (wild_pathname_boolean(pos, Nil)) {
		GetConst(COMMON_PATHNAME, &value);
		type_error_stdarg(pos, value,
				"Cannot file-write-date the wildcard pathname ~S.", pos, NULL);
	}

	/* UTF-16 */
	local = ptr->local;
	name_pathname_local(ptr, pos, &value);
	if (UTF16_buffer_clang(local, &value, value))
		fmte("Cannot decode UTF-16 string ~S.", pos, NULL);
	str = (const WCHAR *)posbodyr(value);

	/* FindFirstFile */
	hFind = FindFirstFileW(str, &data);
	if (hFind == INVALID_HANDLE_VALUE)
		fmte("Cannot find file ~S.", pos, NULL);
	FindClose(hFind);

	/* result */
	if (file_write_date_base_files(ret, &data.ftLastWriteTime))
		fmte("The file ~S timestamp must be after 1900 year.", pos, NULL);
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
	const WCHAR *str1, *str2;

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
	if (UTF16_buffer_clang(local, &value, value))
		fmte("Cannot decode UTF-16 string ~S.", from, NULL);
	str1 = (const WCHAR *)posbodyr(value);
	name_pathname_local(ptr, to, &value);
	if (UTF16_buffer_clang(local, &value, value))
		fmte("Cannot decode UTF-16 string ~S.", to, NULL);
	str2 = (const WCHAR *)posbodyr(value);
	/* check */
	if (probe_file_boolean(str2)) {
		simple_file_error_stdarg(to, "The file ~S is already exist.", to, NULL);
		return;
	}
	/* rename */
	if (MoveFileW(str1, str2) == 0) {
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
static BOOL DeleteFileAsyncW(LPCWSTR name)
{
	WCHAR path[MAX_PATH];
	WCHAR temp[MAX_PATH];
	size_t size;

	size = wcslen(name);
	if (MAX_PATH <= size) {
		Debug("length error.");
		return 0;
	}
	wcsncpy(path, name, MAX_PATH);

	if (PathCchRemoveFileSpec(path, MAX_PATH) != S_OK) {
		Debug("PathCchRemoveFileSpec error.");
		return 0;
	}
	if (path[0] == 0) {
		wcscpy(path, L".");
	}

	if (! GetTempFileNameW(path, L".rm", 0, temp)) {
		Debug("GetTempFileNameW error.");
		return 0;
	}

	if (! MoveFileExW(name, temp, MOVEFILE_REPLACE_EXISTING)) {
		/* Debug("MoveFileExW error."); */
		return 0;
	}

	if (! DeleteFileW(temp)) {
		Debug("DeleteFileW error.");
		return 0;
	}

	return 1;
}

static int delete_file_run_files(Execute ptr, addr pos, int errorp)
{
	LocalRoot local;
	addr file, value;
	const WCHAR *str;

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
	if (UTF16_buffer_clang(local, &value, value))
		fmte("Cannot decode UTF-16 string ~S.", file, NULL);
	str = (const WCHAR *)posbodyr(value);
	/* delete */
	if (DeleteFileAsyncW(str) == 0) {
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
	wchar_t *str, *dst;
	size_t s32, s16;
	LocalRoot local;
	LocalStack stack;
	DWORD check;

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
	if (UTF32_length_utf16(u, s32, &s16)) {
		if (! errorp)
			goto error_nil;
		simple_file_error_stdarg(file, "Invalid unicode string ~S.", pos, NULL);
		return;
	}
	local = ptr->local;
	push_local(local, &stack);
	str = (wchar_t *)lowlevel_local(local, (s16 + 1UL) * sizeoft(wchar_t));
	if (UTF32_make_utf16((byte16 *)str, u, s32)) {
		if (! errorp)
			goto error_nil_rollback;
		simple_file_error_stdarg(file, "Invalid unicode string ~S.", pos, NULL);
		goto error;
	}
	str[s16] = 0;

	/* API */
	dst = (wchar_t *)lowlevel_local(local, MAX_PATH * sizeoft(wchar_t));
	check = GetFullPathNameW(str, MAX_PATH, dst, NULL);
	if (check == 0) {
		if (! errorp)
			goto error_nil_rollback;
		simple_file_error_stdarg(file, "Cannot find the TRUENAME ~S file.", file, NULL);
		goto error;
	}

	/* make-pathname */
	string16_size_heap(&pos, dst, (size_t)check);
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
	const WCHAR *str;

	physical_pathname_heap(ptr, pos, &file);
	if (wild_pathname_boolean(file, Nil))
		fmte("Cannot delete wildcard pathname ~S", pos, NULL);
	if (! pathname_directory_p(file)) {
		if (errorp)
			fmte("The argument ~S is not a file.", pos, NULL);
		return 0;
	}
	/* filename */
	local = ptr->local;
	name_pathname_local(ptr, file, &value);
	if (UTF16_buffer_clang(local, &value, value))
		fmte("Cannot decode UTF-16 string ~S.", file, NULL);
	str = (const WCHAR *)posbodyr(value);
	/* delete */
	if (RemoveDirectoryW(str) == 0) {
		if (errorp)
			simple_file_error_stdarg(pos, "Cannot delete ~S.", file, NULL);
		return 0;
	}
	/* stream */
	if (streamp(pos))
		close_stream(pos);

	return 1;
}

