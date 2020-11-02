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
#include "control_object.h"
#include "copy.h"
#include "encode.h"
#include "format.h"
#include "integer.h"
#include "pathname.h"
#include "pathname_object.h"
#include "pathname_wildcard.h"
#include "sequence.h"
#include "stream.h"
#include "stream_function.h"
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

static int opendir_files(LocalRoot local,
		addr pos, LPWIN32_FIND_DATAW data, HANDLE *ret)
{
	addr name;
	LocalStack stack;
	const WCHAR *clang;
	HANDLE hFind;

	push_local(local, &stack);
	Return(directory_name_pathname_local_(local, pos, &pos));
	find_pathname_files(local, pos, &pos);
	Return(UTF16_buffer_clang_(local, &name, pos));
	if (name == Unbound) {
		*ret = NULL;
		return fmte_("Cannot convert ~S to UTF-16 string.", pos, NULL);
	}
	clang = (const WCHAR *)posbodyr(name);
	hFind = FindFirstFileW(clang, data);
	rollback_local(local, stack);

	return Result(ret, hFind);
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
	const WCHAR *body;
	addr pos;

	local = ptr->local;
	push_local(local, &stack);
	Return(name_pathname_local_(ptr, file, &pos));
	Return(UTF16_buffer_clang_(local, &pos, pos));
	if (pos == Unbound) {
		*ret = 0;
		return fmte_("Cannot convert ~S to UTF-16 string.", file, NULL);
	}
	body = (const WCHAR *)posbodyr(pos);
	check = PathIsDirectoryW(body);
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
		addr base, const WCHAR *name)
{
	LocalRoot local;
	LocalStack stack;
	addr path;

	if (wcscmp(name, L".") == 0)
		return 0;
	if (wcscmp(name, L"..") == 0)
		return 0;
	local = str->local;
	push_local(local, &stack);
	Return(string16_null_local_(local, &path, (const byte16 *)name));
	Return(files_name_directory_files(str, base, path));
	rollback_local(local, stack);

	return 0;
}

static int files_directory_files(struct directory_struct *str, addr base)
{
	HANDLE hFind;
	WIN32_FIND_DATAW data;

	Return(opendir_files(str->local, base, &data, &hFind));
	if (hFind == INVALID_HANDLE_VALUE)
		return 0;
	Return(files_push_directory_files(str, base, data.cFileName));
	while (FindNextFileW(hFind, &data)) {
		Return(files_push_directory_files(str, base, data.cFileName));
	}
	if (FindClose(hFind) == 0)
		return fmte_("FindClose error.", NULL);

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
	const WCHAR *ptr;
	addr pos, value;

	local = str->local;
	push_local(local, &stack);
	copy_list_local_unsafe(local, &pos, str->list);
	cons_local(local, &pos, name, pos);
	Return(make_list_directory_pathname_(str, &pos, pos));
	Return(directory_name_pathname_local_(local, pos, &pos));
	Return(UTF16_buffer_clang_(local, &value, pos));
	if (value == Unbound)
		return fmte_("Cannot convert ~S to UTF-16 string.", pos, NULL);
	ptr = (const WCHAR *)posbodyr(value);
	check = PathIsDirectoryW(ptr);
	rollback_local(local, stack);

	return Result(ret, check);
}

static int loop_directory_files(struct directory_struct *str);
static int wild_push_directory_files(struct directory_struct *str, const WCHAR *name)
{
	int check;
	LocalRoot local;
	LocalStack stack;
	addr path, root, list, front;

	if (wcscmp(name, L".") == 0)
		return 0;
	if (wcscmp(name, L"..") == 0)
		return 0;
	local = str->local;
	push_local(local, &stack);
	Return(string16_null_local_(local, &path, (const byte16 *)name));
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
	HANDLE hFind;
	WIN32_FIND_DATAW data;

	Return(opendir_files(str->local, base, &data, &hFind));
	if (hFind == INVALID_HANDLE_VALUE)
		return 0;
	Return(wild_push_directory_files(str, data.cFileName));
	while (FindNextFileW(hFind, &data)) {
		Return(wild_push_directory_files(str, data.cFileName));
	}
	if (FindClose(hFind) == 0)
		return fmte_("FindClose error.", NULL);

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
		const WCHAR *name)
{
	int check;
	LocalRoot local;
	LocalStack stack;
	addr path, root, list, front;

	if (wcscmp(name, L".") == 0)
		return 0;
	if (wcscmp(name, L"..") == 0)
		return 0;
	local = str->local;
	push_local(local, &stack);
	Return(string16_null_local_(local, &path, (const byte16 *)name));
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
	HANDLE hFind;
	WIN32_FIND_DATAW data;

	Return(opendir_files(str->local, base, &data, &hFind));
	if (hFind == INVALID_HANDLE_VALUE)
		return 0;
	Return(inferiors_push_directory_files(str, data.cFileName));
	while (FindNextFileW(hFind, &data)) {
		Return(inferiors_push_directory_files(str, data.cFileName));
	}
	if (FindClose(hFind) == 0)
		return fmte_("FindClose error.", NULL);

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
static int probe_file_boolean(const WCHAR *file)
{
	return PathFileExistsW(file) == TRUE;
}

static int probe_file_run_files(Execute ptr, addr *ret, addr pos)
{
	int check;
	addr value;
	const WCHAR *str;

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
	Return(UTF16_buffer_clang_(ptr->local, &value, pos));
	if (value == Unbound)
		return fmte_("Cannot decode UTF-16 string ~S.", pos, NULL);
	str = (const WCHAR *)posbodyr(value);
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
	const WCHAR *str;
	addr list, value, root, temp, result;

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
		Return(UTF16_buffer_clang_(local, &value, temp));
		if (value == Unbound)
			return fmte_("Cannot decode UTF-16 string ~S.", temp, NULL);
		str = (const WCHAR *)posbodyr(value);
		/* already exist */
		if (PathFileExistsW(str)) {
			if (! PathIsDirectoryW(str))
				return fmte_("Cannot make directory ~S.", pos, NULL);
			rollback_local(local, stack);
			continue;
		}
		/* CreateDirectory */
		if (CreateDirectoryW(str, NULL) == 0)
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
_g int file_author_files_(Execute ptr, addr *ret, addr pos)
{
	int check;
	BOOL result;
	DWORD size1, size2;
	LocalRoot local;
	WCHAR *str, *name, *domain;
	addr value;
	PSID owner;
	SID_NAME_USE use;
	PSECURITY_DESCRIPTOR psd;

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

	/* UTF-16 */
	local = ptr->local;
	Return(name_pathname_local_(ptr, pos, &value));
	Return(UTF16_buffer_clang_(local, &value, value));
	if (value == Unbound)
		return fmte_("Cannot decode UTF-16 string ~S.", pos, NULL);
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
		return fmte_("GetSecurityDescriptorOwner error.", NULL);

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
		return fmte_("LookupAccountSid error.", NULL);
	}

	/* result */
	return string16_null_heap_(ret, (const byte16 *)name);

finish_nil:
	return Result(ret, Nil);
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

static int file_write_date_run_files(Execute ptr, addr *ret, addr pos)
{
	int check;
	LocalRoot local;
	HANDLE hFind;
	const WCHAR *str;
	addr value;
	WIN32_FIND_DATAW data;

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

	/* UTF-16 */
	local = ptr->local;
	Return(name_pathname_local_(ptr, pos, &value));
	Return(UTF16_buffer_clang_(local, &value, value));
	if (value == Unbound)
		return fmte_("Cannot decode UTF-16 string ~S.", pos, NULL);
	str = (const WCHAR *)posbodyr(value);

	/* FindFirstFile */
	hFind = FindFirstFileW(str, &data);
	if (hFind == INVALID_HANDLE_VALUE)
		return fmte_("Cannot find file ~S.", pos, NULL);
	FindClose(hFind);

	/* result */
	if (file_write_date_base_files(ret, &data.ftLastWriteTime))
		return fmte_("The file ~S timestamp must be after 1900 year.", pos, NULL);

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
	const WCHAR *str1, *str2;

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
	Return(UTF16_buffer_clang_(local, &value, value));
	if (value == Unbound)
		return fmte_("Cannot decode UTF-16 string ~S.", from, NULL);
	str1 = (const WCHAR *)posbodyr(value);
	Return(name_pathname_local_(ptr, to, &value));
	Return(UTF16_buffer_clang_(local, &value, value));
	if (value == Unbound)
		return fmte_("Cannot decode UTF-16 string ~S.", to, NULL);
	str2 = (const WCHAR *)posbodyr(value);
	/* check */
	if (probe_file_boolean(str2)) {
		return call_simple_file_error_va_(ptr, to,
				"The file ~S is already exist.", to, NULL);
	}
	/* rename */
	if (MoveFileW(str1, str2) == 0) {
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

static int delete_file_run_files(Execute ptr, addr pos, int errorp, int *ret)
{
	int check;
	LocalRoot local;
	addr file, value;
	const WCHAR *str;

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
	Return(UTF16_buffer_clang_(local, &value, value));
	if (value == Unbound)
		return fmte_("Cannot decode UTF-16 string ~S.", file, NULL);
	str = (const WCHAR *)posbodyr(value);
	/* delete */
	if (DeleteFileAsyncW(str) == 0) {
		if (errorp) {
			return call_simple_file_error_va_(ptr, pos,
					"Cannot delete ~S.", file, NULL);
		}
		return Result(ret, 0);
	}
	/* stream */
	if (streamp(pos)) {
		Return(close_stream_(pos, NULL));
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
	const WCHAR *str;

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
	Return(UTF16_buffer_clang_(local, &value, value));
	if (value == Unbound)
		return fmte_("Cannot decode UTF-16 string ~S.", file, NULL);
	str = (const WCHAR *)posbodyr(value);
	/* delete */
	if (RemoveDirectoryW(str) == 0) {
		if (errorp) {
			return call_simple_file_error_va_(ptr, pos,
					"Cannot delete ~S.", file, NULL);
		}
		return Result(ret, 0);
	}
	/* stream */
	if (streamp(pos)) {
		Return(close_stream_(pos, NULL));
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
	wchar_t *str, *dst;
	size_t s32, s16;
	LocalRoot local;
	LocalStack stack;
	DWORD dcheck;

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
	if (UTF32_length_utf16(u, s32, &s16)) {
		if (! errorp)
			goto error_nil;
		return call_simple_file_error_va_(ptr, file,
				"Invalid unicode string ~S.", pos, NULL);
	}
	local = ptr->local;
	push_local(local, &stack);
	str = (wchar_t *)lowlevel_local(local, (s16 + 1UL) * sizeoft(wchar_t));
	if (UTF32_make_utf16((byte16 *)str, u, s32)) {
		if (! errorp)
			goto error_nil_rollback;
		return call_simple_file_error_va_(ptr, file,
				"Invalid unicode string ~S.", pos, NULL);
	}
	str[s16] = 0;

	/* API */
	dst = (wchar_t *)lowlevel_local(local, MAX_PATH * sizeoft(wchar_t));
	dcheck = GetFullPathNameW(str, MAX_PATH, dst, NULL);
	if (dcheck == 0) {
		if (! errorp)
			goto error_nil_rollback;
		return call_simple_file_error_va_(ptr, file,
				"Cannot find the TRUENAME ~S file.", file, NULL);
	}

	/* make-pathname */
	Return(string16_size_heap_(&pos, dst, (size_t)dcheck));
	Return(pathname_designer_heap_(ptr, pos, ret));
	rollback_local(local, stack);
	return 0;

error_nil_rollback:
	rollback_local(local, stack);
error_nil:
	return Result(ret, Nil);
}

