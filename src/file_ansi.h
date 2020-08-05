/*
 *  file-ansi
 *    ANCI-C file function
 */
#ifndef __FILE_ANSI_HEADER__
#define __FILE_ANSI_HEADER__

#include <stdio.h>
#include "condition.h"
#include "cons.h"
#include "encode.h"
#include "object.h"
#include "pathname.h"
#include "strtype.h"
#include "typedef.h"

#ifdef LISP_ANSI_WINDOWS
#define UTF_buffer_clang UTF16LE_buffer_clang
#define fopen_input(x) _wfopen(x, L"rb")
#define fopen_output(x) _wfopen(x, L"wb")
#define fopen_append(x) _wfopen(x, L"ab")
#else
#define UTF_buffer_clang UTF8_buffer_clang
#define fopen_input(x) fopen(x, "rb")
#define fopen_output(x) fopen(x, "wb")
#define fopen_append(x) fopen(x, "ab")
#endif

_g int init_file(void)
{
	return 0;
}

_g void free_file(void)
{
}

_g int consolep_file(void)
{
	return 0;
}

static inline void standard_input_arch(file_type *file)
{
	*file = stdin;
}

static inline void standard_output_arch(file_type *file)
{
	*file = stdout;
}

static inline void standard_error_arch(file_type *file)
{
	*file = stderr;
}

static inline int filename_encode_(LocalRoot local, addr name, const char **const ret)
{
	addr data;

	Check(! stringp(name), "name error");
	Return(UTF8_buffer_clang_(local, &data, name));
	if (data == Unbound) {
		*ret = NULL;
		return fmte_("Invalid UTF-8 encoding ~S.", name, NULL);
	}
	posbody(data, (addr *)ret);
	return 0;
}

static inline int open_input_chartype(file_type *ret, const char *name)
{
	file_type file;

	file = fopen_input(name);
	if (file == NULL)
		return 1;
	*ret = file;

	return 0;
}

static inline int open_input_unicode(file_type *ret, const unicode *name, size_t size)
{
	byte *ptr;
	size_t value;

	ptr = NULL;
	if (UTF32_length_utf8(name, size, &value))
		goto error;
	ptr = (byte *)malloc(value);
	if (ptr == NULL)
		goto error;
	if (UTF32_make_utf8(ptr, name, value))
		goto error;
	if (open_input_chartype(ret, (const char *)ptr))
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
	const char *utf8;

	push_local(local, &stack);
	Return(filename_encode_(local, name, &utf8));
	if (utf8 == NULL) {
		*ret = 1;
		goto finish;
	}
	if (open_input_chartype(value, utf8)) {
		*ret = 1;
		goto finish;
	}
	*ret = 0;

finish:
	rollback_local(local, stack);
	return 0;
}

static inline int open_output_chartype(file_type *ret,
		const char *name, enum FileOutput mode)
{
	file_type file;

	switch (mode) {
		case FileOutput_supersede:
			file = fopen_output(name);
			break;

		case FileOutput_append:
			file = fopen_append(name);
			break;

		case FileOutput_overwrite:
			file = fopen_append(name);
			if (file == NULL)
				return 1;
			if (fseek(file, 0L, SEEK_SET))
				return 1;
			*ret = file;
			return 0;

		default:
			Debug("Invalid mode.");
			return 1;
	}
	if (file == NULL)
		return 1;
	*ret = file;

	return 0;
}

static inline int open_output_arch_(LocalRoot local,
		addr name, enum FileOutput mode, file_type *value, int *ret)
{
	LocalStack stack;
	const char *utf8;

	push_local(local, &stack);
	Return(filename_encode_(local, name, &utf8));
	if (utf8 == NULL) {
		*ret = 1;
		goto finish;
	}
	if (open_output_chartype(value, utf8, mode)) {
		*ret = 1;
		goto finish;
	}
	*ret = 0;

finish:
	rollback_local(local, stack);
	return 0;
}

static inline int open_io_chartype(file_type *ret,
		const char *name, enum FileOutput mode)
{
	file_type file;

	switch (mode) {
		case FileOutput_supersede:
		case FileOutput_append:
		case FileOutput_overwrite:
			file = fopen_append(name);
			break;

		default:
			Debug("Invalid mode.");
			return -1;
	}
	if (file == NULL)
		return 1;
	*ret = file;

	return 0;
}

static inline int open_io_arch_(LocalRoot local,
		addr name, enum FileOutput mode, file_type *value, int *ret)
{
	LocalStack stack;
	const char *utf8;

	push_local(local, &stack);
	Return(filename_encode_(local, name, &utf8));
	if (utf8 == NULL) {
		*ret = 1;
		goto finish;
	}
	if (open_io_chartype(value, utf8, mode)) {
		*ret = 1;
		goto finish;
	}
	*ret = 0;

finish:
	rollback_local(local, stack);
	return 0;
}

static inline int readcall_arch(file_type file, void *pos, size_t size, size_t *ret)
{
	size_t check;

	check = fread(pos, 1, size, file);
	if (ferror(file)) {
		Debug("fread error");
		*ret = 0;
		return -1;
	}
	if (check == 0 && feof(file)) {
		*ret = 0;
		return 1;
	}
	else {
		*ret = check;
		return 0;
	}
}

static inline int writecall_arch(file_type file,
		const void *pos, size_t size, size_t *ret)
{
	size_t check;

	check = fwrite(pos, 1, size, file);
	if (ferror(file)) {
		Debug("fwrite error");
		*ret = 0;
		return -1;
	}
	if (check == 0 && feof(file)) {
		*ret = 0;
		return 1;
	}
	else {
		*ret = check;
		return 0;
	}
}

static inline int close_arch(file_type file)
{
	if (file == stdin)
		return 0;
	if (file == stdout)
		return 0;
	if (file == stderr)
		return 0;

	if (fclose(file)) {
		Debug("close error");
		return 1;
	}

	return 0;
}

static inline int flush_arch(file_type file)
{
	if (fflush(file)) {
		Debug("fsync error");
		return 1;
	}

	return 0;
}

static inline int read_ready_arch(file_type file)
{
	Debug("read_ready_arch does not implement.");
	return 1;
}

static inline int file_length_arch(file_type file, size_t *ret)
{
	*ret = 0;
	return 1;
}

static inline int file_position_arch(file_type file, size_t *ret)
{
	fpos_t pos;

	if (fgetpos(file, &pos)) {
		*ret = 0;
		return 1;
	}
#if defined(__GNUC__) && (! defined(__clang__))
	*ret = (size_t)pos.__pos;
#else
	*ret = (size_t)pos;
#endif

	return 0;
}

static inline int file_position_start_arch(file_type file)
{
	return fseek(file, 0, SEEK_SET) != 0;
}

static inline int file_position_end_arch(file_type file)
{
	return fseek(file, 0, SEEK_END) != 0;
}

static inline int file_position_set_arch(file_type file, size_t pos)
{
	return fseek(file, (long)pos, SEEK_SET) != 0;
}

#endif

