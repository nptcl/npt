/*
 *  file-windows
 *    Windows file function
 */
#ifndef __FILE_WINDOWS_HEADER__
#define __FILE_WINDOWS_HEADER__

#include <windows.h>
#include "condition.h"
#include "cons.h"
#include "condition.h"
#include "encode.h"
#include "file.h"
#include "object.h"
#include "pathname.h"
#include "strtype.h"
#include "typedef.h"
#ifdef LISP_TERME_WINDOWS
#include "windows_arch.h"
#endif

static int fileio_input_console_p;
static int fileio_output_console_p;
static int fileio_error_console_p;
static file_type fileio_input;
static file_type fileio_output;
static file_type fileio_error;

static int init_fileio(file_type *file, DWORD type)
{
	HANDLE hFile;

	hFile = GetStdHandle(type);
	if (hFile == INVALID_HANDLE_VALUE) {
		Debug("GetStdHandle error");
		return 1;
	}
	*file = hFile;

	return 0;
}

static inline int standard_input_arch(file_type *ret)
{
	HANDLE file;
	DWORD ignore;

	if (init_fileio(&file, STD_INPUT_HANDLE))
		return 1;
	fileio_input_console_p = GetConsoleMode(file, &ignore);
	fileio_input = file;
	*ret = file;

	return 0;
}

static inline int standard_output_arch(file_type *ret)
{
	HANDLE file;
	DWORD ignore;

	if (init_fileio(&file, STD_OUTPUT_HANDLE))
		return 1;
	fileio_output_console_p = GetConsoleMode(file, &ignore);
	fileio_output = file;
	*ret = file;

	return 0;
}

static inline int standard_error_arch(file_type *ret)
{
	HANDLE file;
	DWORD ignore;

	if (init_fileio(&file, STD_ERROR_HANDLE))
		return 1;
	fileio_error_console_p = GetConsoleMode(file, &ignore);
	fileio_error = file;
	*ret = file;

	return 0;
}

int init_file(void)
{
	if (standard_input_arch(&fileio_input))
		return 1;
	if (standard_output_arch(&fileio_output))
		return 1;
	if (standard_error_arch(&fileio_error))
		return 1;

	return 0;
}

void free_file(void)
{
}

int consolep_file(void)
{
#ifdef LISP_TERME_WINDOWS
	return consolep_windows();
#else
	return fileio_input_console_p
		|| fileio_output_console_p
		|| fileio_error_console_p;
#endif
}

static inline int filename_encode_(LocalRoot local, addr name, LPCWSTR *ret)
{
	addr data;

	Check(! stringp(name), "name error");
	Return(UTF16_buffer_clang_(local, &data, name));
	if (data == Unbound) {
		*ret = NULL;
		return fmte_("Invalid UTF-16 encoding ~S.", name, NULL);
	}
	posbody(data, (addr *)ret);
	return 0;
}

static inline int open_input_chartype(file_type *ret, LPCWSTR name)
{
	file_type file;

	file = CreateFileW(
			name,
			GENERIC_READ,
			FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
			NULL,
			OPEN_EXISTING,
			FILE_ATTRIBUTE_NORMAL,
			NULL);
	if (file == INVALID_HANDLE_VALUE)
		return 1;
	*ret = file;

	return 0;
}

static inline int open_input_unicode(file_type *ret, const unicode *name, size_t size)
{
	byte16 *ptr;
	size_t value;

	ptr = NULL;
	if (UTF32_length_utf16(name, size, &value))
		goto error;
	ptr = (byte16 *)malloc(sizeoft(byte16) * value);
	if (ptr == NULL)
		goto error;
	if (UTF32_make_utf16(ptr, name, value))
		goto error;
	if (open_input_chartype(ret, (LPCWSTR)ptr))
		goto error;
	free(ptr);
	return 0;

error:
	free(ptr);
	return 1;
}

static inline int open_input_arch_(LocalRoot local,
		addr name, file_type *value, int *ret)
{
	LocalStack stack;
	LPCWSTR utf16;

	push_local(local, &stack);
	Return(filename_encode_(local, name, &utf16));
	if (utf16 == NULL) {
		*ret = 1;
		goto finish;
	}
	if (open_input_chartype(value, utf16)) {
		*ret = 1;
		goto finish;
	}
	*ret = 0;

finish:
	rollback_local(local, stack);
	return 0;
}

static inline int open_output_chartype(file_type *ret,
		LPCWSTR name, enum FileOutput mode)
{
	file_type file;

	switch (mode) {
		case FileOutput_supersede:
			file = CreateFileW(
					name,
					GENERIC_WRITE,
					FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
					NULL,
					CREATE_ALWAYS,
					FILE_ATTRIBUTE_NORMAL,
					NULL);
			break;

		case FileOutput_append:
			file = CreateFileW(
					name,
					FILE_APPEND_DATA,
					FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
					NULL,
					OPEN_ALWAYS,
					FILE_ATTRIBUTE_NORMAL,
					NULL);
			break;

		case FileOutput_overwrite:
			file = CreateFileW(
					name,
					GENERIC_WRITE,
					FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
					NULL,
					OPEN_ALWAYS,
					FILE_ATTRIBUTE_NORMAL,
					NULL);
			break;

		default:
			Debug("Invalid mode.");
			return 1;
	}
	if (file == INVALID_HANDLE_VALUE)
		return 1;
	*ret = file;

	return 0;
}

static inline int open_output_arch_(LocalRoot local,
		addr name, enum FileOutput mode, file_type *value, int *ret)
{
	LocalStack stack;
	LPCWSTR utf16;

	push_local(local, &stack);
	Return(filename_encode_(local, name, &utf16));
	if (utf16 == NULL) {
		*ret = 1;
		goto finish;
	}
	if (open_output_chartype(value, utf16, mode)) {
		*ret = 1;
		goto finish;
	}
	*ret = 0;

finish:
	rollback_local(local, stack);
	return 0;
}

static inline file_type open_io_chartype_append(LPCWSTR name)
{
	file_type file;
	LARGE_INTEGER zero, pos;

	file = CreateFileW(
			name,
			GENERIC_READ | GENERIC_WRITE,
			FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
			NULL,
			OPEN_ALWAYS,
			FILE_ATTRIBUTE_NORMAL,
			NULL);
	if (file == INVALID_HANDLE_VALUE)
		return file;

	/* file-position-end */
	zero.QuadPart = 0ULL;
	if (SetFilePointerEx(file, zero, &pos, FILE_END) == 0) {
		(void)CloseHandle(file);
		return INVALID_HANDLE_VALUE;
	}

	/* success */
	return file;
}

static inline int open_io_chartype(file_type *ret,
		LPCWSTR name, enum FileOutput mode)
{
	file_type file;

	switch (mode) {
		case FileOutput_supersede:
			file = CreateFileW(
					name,
					GENERIC_READ | GENERIC_WRITE,
					FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
					NULL,
					CREATE_ALWAYS,
					FILE_ATTRIBUTE_NORMAL,
					NULL);
			break;

		case FileOutput_append:
			file = open_io_chartype_append(name);
			break;

		case FileOutput_overwrite:
			file = CreateFileW(
					name,
					GENERIC_READ | GENERIC_WRITE,
					FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
					NULL,
					OPEN_ALWAYS,
					FILE_ATTRIBUTE_NORMAL,
					NULL);
			break;

		default:
			Debug("Invalid mode.");
			return 1;
	}
	if (file == INVALID_HANDLE_VALUE)
		return 1;
	*ret = file;

	return 0;
}

static inline int open_io_arch_(LocalRoot local,
		addr name, enum FileOutput mode, file_type *value, int *ret)
{
	LocalStack stack;
	LPCWSTR utf16;

	push_local(local, &stack);
	Return(filename_encode_(local, name, &utf16));
	if (utf16 == NULL) {
		*ret = 1;
		goto finish;
	}
	if (open_io_chartype(value, utf16, mode)) {
		*ret = 1;
		goto finish;
	}
	*ret = 0;

finish:
	rollback_local(local, stack);
	return 0;
}

static inline int read_arch(file_type file, void *pos, size_t size, size_t *ret)
{
	BOOL check;
	DWORD dsize;

	Check(0xFFFFFFFFULL < size, "size error");
	check = ReadFile(file, (LPVOID)pos, (DWORD)size, &dsize, NULL);
	if (check == 0) {
		Debug("ReadFile error");
		*ret = 0;
		return -1;
	}
	if (dsize == 0) {
		*ret = 0;
		return 1;
	}
	else {
		*ret = (size_t)dsize;
		return 0;
	}
}

static inline int write_arch(file_type file, const void *pos, size_t size, size_t *ret)
{
	BOOL check;
	DWORD dsize;

	check = WriteFile(file, (LPCVOID)pos, (DWORD)size, &dsize, NULL);
	if (check == 0) {
		Debug("WriteFile error");
		*ret = 0;
		return -1;
	}
	if (dsize == 0) {
		*ret = 0;
		return 1;
	}
	else {
		*ret = (size_t)dsize;
		return 0;
	}
}

static inline int close_arch(file_type file)
{
	if (file == fileio_input)
		return 0;
	if (file == fileio_output)
		return 0;
	if (file == fileio_error)
		return 0;

	if (CloseHandle(file) == 0) {
		Debug("CloseHandle error");
		return 1;
	}

	return 0;
}

static inline int flush_arch(file_type file)
{
	if (FlushFileBuffers(file) == 0) {
		if (file == fileio_output || file == fileio_error) {
			/* fails if hFile is a handle to the console output. */
			return 0;
		}
		Debug("FlushFileBuffers error");
		return 1;
	}

	return 0;
}

static inline int read_ready_arch(file_type file)
{
	return (file != fileio_input);
}

static int large_integer_value(PLARGE_INTEGER ptr, size_t *ret)
{
#ifdef LISP_64BIT
	*ret = (size_t)ptr->QuadPart;
	return 0;
#else
	if (ptr->HighPart != 0)
		return 1;
	*ret = (fixed)ptr->LowPart;
	return 0;
#endif
}

static inline int file_length_arch(file_type file, size_t *ret)
{
	LARGE_INTEGER size;

	if (! GetFileSizeEx(file, &size)) {
		*ret = 0;
		return 1;
	}
	if (large_integer_value(&size, ret)) {
		*ret = 0;
		return 1;
	}

	return 0;
}

static inline int file_position_arch(file_type file, size_t *ret)
{
	LARGE_INTEGER zero, pos;

	zero.QuadPart = 0ULL;
	if (! SetFilePointerEx(file, zero, &pos, FILE_CURRENT)) {
		*ret = 0;
		return 1;
	}
	if (large_integer_value(&pos, ret)) {
		*ret = 0;
		return 1;
	}

	return 0;
}

static inline int file_position_start_arch(file_type file)
{
	LARGE_INTEGER zero, pos;
	zero.QuadPart = 0ULL;
	return SetFilePointerEx(file, zero, &pos, FILE_BEGIN) == 0;
}

static inline int file_position_end_arch(file_type file)
{
	LARGE_INTEGER zero, pos;
	zero.QuadPart = 0ULL;
	return SetFilePointerEx(file, zero, &pos, FILE_END) == 0;
}

static inline int file_position_set_arch(file_type file, size_t value)
{
	LARGE_INTEGER zero, pos;
	zero.QuadPart = (LONGLONG)value;
	return SetFilePointerEx(file, zero, &pos, FILE_BEGIN) == 0;
}

#endif

