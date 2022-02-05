#include "constant.h"
#include "env_version.h"
#include "hashtable.h"
#include "pathname.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"
#include "unicode.h"

/*
 *  lisp-implementation-type
 */
void implementation_type_common(addr *ret)
{
	strvect_char_heap(ret, LISPNAME);
}


/*
 *  lisp-implementation-version
 */
void implementation_version_common(addr *ret)
{
	char buffer[256];

	snprintf(buffer, 256, "%d.%d.%d [%s]",
			LISP_VERSION_A, LISP_VERSION_B, LISP_VERSION_C, LISP_REVISION);
	strvect_char_heap(ret, buffer);
}


/*
 *  short-site-name
 */
#if defined(LISP_UNIX)
#include <sys/utsname.h>
int short_site_name_common_(addr *ret)
{
	struct utsname data;

	if (uname(&data))
		return Result(ret, Nil);
	else
		return string8_null_heap_(ret, data.nodename);
}
#elif defined(LISP_WINDOWS)
static int windows_site_name_(addr *ret, COMPUTER_NAME_FORMAT format)
{
	BOOL check;
	WCHAR *data;
	DWORD size;
	LocalRoot local;
	LocalStack stack;

	size = 0;
	(void)GetComputerNameExW(format, NULL, &size);
	local = Local_Thread;
	push_local(local, &stack);
	data = (WCHAR *)lowlevel_local(local, size);
	check = GetComputerNameExW(format, NULL, &size);
	if (check == 0) {
		*ret = Nil;
	}
	else {
		Return(string16_size_heap_(ret, (const byte16 *)data, (size_t)size));
	}
	rollback_local(local, stack);

	return 0;
}
int short_site_name_common_(addr *ret)
{
	return windows_site_name_(ret, ComputerNameNetBIOS);
}
#else
int short_site_name_common_(addr *ret)
{
	return Result(ret, Nil);
}
#endif


/*
 *  long-site-name
 */
#if defined(LISP_UNIX)
#include <sys/utsname.h>
int long_site_name_common_(addr *ret)
{
	return short_site_name_common_(ret);
}
#elif defined(LISP_WINDOWS)
int long_site_name_common_(addr *ret)
{
	return windows_site_name_(ret, ComputerNamePhysicalDnsFullyQualified);
}
#else
int long_site_name_common_(addr *ret)
{
	return Result(ret, Nil);
}
#endif


/*
 *  machine-instance
 */
int machine_instance_common_(addr *ret)
{
	return short_site_name_common_(ret);
}


/*
 *  machine-type
 */
#if defined(LISP_UNIX)
int machine_type_common_(addr *ret)
{
	struct utsname data;

	if (uname(&data))
		return Result(ret, Nil);
	else
		return string8_null_heap_(ret, data.machine);
}
#elif defined(LISP_WINDOWS)
int machine_type_common_(addr *ret)
{
	const char *str;
	SYSTEM_INFO info;

	GetNativeSystemInfo(&info);
	/* winnt.h */
	switch (info.wProcessorArchitecture) {
		case 0: str = "INTEL"; break;
		case 1: str = "MIPS"; break;
		case 2: str = "ALPHA"; break;
		case 3: str = "PPC"; break;
		case 4: str = "SHX"; break;
		case 5: str = "ARM"; break;
		case 6: str = "IA64"; break;
		case 7: str = "ALPHA64"; break;
		case 8: str = "MSIL"; break;
		case 9: str = "AMD64"; break;
		case 10: str = "IA32-ON-WIN64"; break;
		case 11: str = "NEUTRAL"; break;
		case 12: str = "ARM64"; break;
		case 13: str = "ARM32-ON-WIN64"; break;
		case 14: str = "IA32-ON-ARM64"; break;
		default: str = NULL; break;
	}
	if (str == NULL)
		return Result(ret, Nil);
	strvect_char_heap(ret, str);
	return 0;
}
#else
int machine_type_common_(addr *ret)
{
	return Result(ret, Nil);
}
#endif


/*
 *  machine-version
 */
#if defined(LISP_UNIX)
int machine_version_common_(addr *ret)
{
	return machine_type_common_(ret);
}
#elif defined(LISP_WINDOWS)
int machine_version_common_(addr *ret)
{
	const char *str;
	SYSTEM_INFO info;

	GetNativeSystemInfo(&info);
	/* winnt.h */
	switch (info.dwProcessorType) {
		case 386: str = "INTEL-386"; break;
		case 486: str = "INTEL-486"; break;
		case 586: str = "INTEL-PENTIUM"; break;
		case 2200: str = "INTEL-IA64"; break;
		case 8664: str = "AMD-X8664"; break;
		case 4000: str = "MIPS-R4000"; break;
		case 21064: str = "ALPHA-21064"; break;
		case 601: str = "PPC-601"; break;
		case 603: str = "PPC-603"; break;
		case 604: str = "PPC-604"; break;
		case 620: str = "PPC-620"; break;
		case 10003: str = "HITACHI-SH3"; break;
		case 10004: str = "HITACHI-SH3E"; break;
		case 10005: str = "HITACHI-SH4"; break;
		case 821: str = "MOTOROLA-821"; break;
		case 103: str = "SHx-SH3"; break;
		case 104: str = "SHx-SH4"; break;
		case 2577: str = "STRONGARM"; break;
		case 1824: str = "ARM720"; break;
		case 2080: str = "ARM820"; break;
		case 2336: str = "ARM920"; break;
		case 70001: str = "ARM-7TDMI"; break;
		case 0x494f: str = "OPTIL"; break;
		default: str = NULL; break;
	}
	if (str == NULL)
		return Result(ret, Nil);
	strvect_char_heap(ret, str);
	return 0;
}
#else
int machine_version_common_(addr *ret)
{
	return Result(ret, Nil);
}
#endif


/*
 *  software-type
 */
#if defined(LISP_UNIX)
int software_type_common_(addr *ret)
{
	struct utsname data;

	if (uname(&data))
		return Result(ret, Nil);
	else
		return string8_null_heap_(ret, data.sysname);
}
#elif defined(LISP_WINDOWS)
int software_type_common_(addr *ret)
{
	strvect_char_heap(ret, "Windows");
	return 0;
}
#else
int software_type_common_(addr *ret)
{
	return Result(ret, Nil);
}
#endif


/*
 *  software-version
 */
#if defined(LISP_UNIX)
int software_version_common_(addr *ret)
{
	struct utsname data;

	if (uname(&data))
		return Result(ret, Nil);
	else
		return string8_null_heap_(ret, data.release);
}
#elif defined(LISP_WINDOWS)
int software_version_common_(addr *ret)
{
	BOOL (*local_GetFileVersionInfo)(LPCSTR, DWORD, DWORD, LPVOID);
	DWORD (*local_GetFileVersionInfoSize)(LPCSTR, LPDWORD);
	BOOL (*local_VerQueryValue)(LPCVOID, LPCSTR, LPVOID *, PUINT);
	BOOL check;
	HMODULE version_dll;
	FARPROC call;
	DWORD size, handle;
	struct LANGANDCODEPAGE {
		WORD wLanguage;
		WORD wCodePage;
	} *translate;
	void *ptr;
	char buffer[256];
	const char *pversion;
	UINT usize;

#ifdef LISP_32BIT
	return Result(ret, Nil);
#endif

	ptr = NULL;
	/* VERSION.DLL */
	version_dll = LoadLibraryA("VERSION.DLL");
	if (version_dll == NULL) {
		goto error_dll;
	}

	/* GetFileVersionInfo */
	call = GetProcAddress(version_dll, "GetFileVersionInfoA");
	if (call == NULL) {
		goto error_dll;
	}
	local_GetFileVersionInfo = (BOOL (*)(LPCSTR, DWORD, DWORD, LPVOID))call;

	/* GetFileVersionInfoSize */
	call = GetProcAddress(version_dll, "GetFileVersionInfoSizeA");
	if (call == NULL) {
		goto error_dll;
	}
	local_GetFileVersionInfoSize = (DWORD (*)(LPCSTR, LPDWORD))call;

	/* VerQueryValue */
	call = GetProcAddress(version_dll, "VerQueryValueA");
	if (call == NULL) {
		goto error_dll;
	}
	local_VerQueryValue = (BOOL (*)(LPCVOID, LPCSTR, LPVOID *, PUINT))call;

	/* size */
	handle = 0;
	size = local_GetFileVersionInfoSize("KERNEL32.DLL", &handle);
	if (size == 0) {
		goto error_dll;
	}

	/* version */
	ptr = malloc(size);
	if (ptr == NULL) {
		goto error_dll;
	}
	check = local_GetFileVersionInfo("KERNEL32.DLL", 0, size, ptr);
	if (check == 0) {
		goto error_dll;
	}

	/* query */
	check = local_VerQueryValue(ptr,
			"\\VarFileInfo\\Translation",
			(LPVOID *)&translate, &usize);
	if (check == 0) {
		goto error_dll;
	}
	snprintf(buffer, 256, "\\StringFileInfo\\%04x%04x\\ProductVersion",
			translate->wLanguage, translate->wCodePage);
	check = local_VerQueryValue(ptr, buffer, (LPVOID *)&pversion, &usize);
	if (check == 0) {
		goto error_dll;
	}

	/* result */
	Return(string8_null_heap_(ret, pversion));
	free(ptr);
	FreeLibrary(version_dll);
	return 0;

error_dll:
	free(ptr);
	if (version_dll)
		FreeLibrary(version_dll);
	return Result(ret, Nil);
}

#else
int software_version_common_(addr *ret)
{
	return Result(ret, Nil);
}
#endif


/*
 *  user-homedir-pathname
 */
static int default_user_homedir_pathname_common_(Execute ptr, addr *ret)
{
	return parse_pathname_char_heap_(ptr, ".", ret);
}

static int user_homedir_pathname_string_(LocalRoot local, addr pos, addr *ret)
{
	unicode c;
	addr one;
	size_t size, i;

	string_length(pos, &size);
	if (size == 0)
		goto no_update;
	Return(string_getc_(pos, size - 1, &c));
	if (c == '/' || c == '\\')
		goto no_update;
	strvect_local(local, &one, size + 1);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		Return(strvect_setc_(one, i, c));
	}
#ifdef LISP_WINDOWS
	Return(strvect_setc_(one, i, '\\'));
#else
	Return(strvect_setc_(one, i, '/'));
#endif
	return Result(ret, one);

no_update:
	return Result(ret, pos);
}

int user_homedir_pathname_common_(Execute ptr, addr *ret)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	GetConst(SYSTEM_SPECIAL_ENVIRONMENT, &pos);
	getspecial_local(ptr, pos, &pos);
	if (pos == Unbound || pos == Nil)
		goto error;
	Return(find_char_hashtable_(pos, "HOME", &pos));
	if (pos == Unbound)
		goto error;
	/* /home/name -> /home/name/ */
	local = ptr->local;
	push_local(local, &stack);
	Return(user_homedir_pathname_string_(local, pos, &pos));
	Return(physical_pathname_heap_(ptr, pos, ret));
	rollback_local(local, stack);
	return 0;

error:
	return default_user_homedir_pathname_common_(ptr, ret);
}

