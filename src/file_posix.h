/*
 *  file-posix
 *    UNIX file function
 */
#ifndef __FILE_POSIX_HEADER__
#define __FILE_POSIX_HEADER__

#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "encode.h"
#include "file.h"
#include "local.h"
#include "object.h"
#include "pathname.h"
#include "strtype.h"
#include "typedef.h"
#include "unicode.h"

#define create_chmod_644 (S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH)

int init_file(void)
{
	return 0;
}

void free_file(void)
{
}

int consolep_file(void)
{
	return isatty(STDIN_FILENO) &&
		isatty(STDOUT_FILENO) &&
		isatty(STDERR_FILENO);
}

static inline int standard_input_arch(file_type *file)
{
	*file = STDIN_FILENO;
	return 0;
}

static inline int standard_output_arch(file_type *file)
{
	*file = STDOUT_FILENO;
	return 0;
}

static inline int standard_error_arch(file_type *file)
{
	*file = STDERR_FILENO;
	return 0;
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

	file = open(name, O_RDONLY);
	if (file < 0)
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

static inline file_type open_output_call(const char *name, enum FileOutput mode)
{
	switch (mode) {
		case FileOutput_supersede:
			return open(name, O_WRONLY | O_CREAT | O_TRUNC, create_chmod_644);

		case FileOutput_append:
			return open(name, O_WRONLY | O_CREAT | O_APPEND, create_chmod_644);

		case FileOutput_overwrite:
			return open(name, O_WRONLY | O_CREAT, create_chmod_644);

		default:
			Debug("Invalid mode.");
			return -1;
	}
}

static inline int open_output_arch_(LocalRoot local,
		addr name, enum FileOutput mode, file_type *value, int *ret)
{
	LocalStack stack;
	file_type file;
	const char *utf8;

	push_local(local, &stack);
	Return(filename_encode_(local, name, &utf8));
	if (utf8 == NULL) {
		*ret = 1;
		goto finish;
	}
	file = open_output_call(utf8, mode);
	if (file < 0) {
		*ret = 1;
		goto finish;
	}
	*value = file;
	*ret = 0;

finish:
	rollback_local(local, stack);
	return 0;
}

static inline file_type open_io_call(const char *name, enum FileOutput mode)
{
	switch (mode) {
		case FileOutput_supersede:
			return open(name, O_RDWR | O_CREAT | O_TRUNC, create_chmod_644);

		case FileOutput_append:
			return open(name, O_RDWR | O_CREAT | O_APPEND, create_chmod_644);

		case FileOutput_overwrite:
			return open(name, O_RDWR | O_CREAT, create_chmod_644);

		default:
			Debug("Invalid mode.");
			return -1;
	}
}

static inline int open_io_arch_(LocalRoot local,
		addr name, enum FileOutput mode, file_type *value, int *ret)
{
	LocalStack stack;
	file_type file;
	const char *utf8;

	push_local(local, &stack);
	Return(filename_encode_(local, name, &utf8));
	if (utf8 == NULL) {
		*ret = 1;
		goto finish;
	}
	file = open_io_call(utf8, mode);
	if (file < 0) {
		*ret = 1;
		goto finish;
	}
	*value = file;
	*ret = 0;

finish:
	rollback_local(local, stack);
	return 0;
}

static inline int read_arch(file_type file, void *pos, size_t size, size_t *ret)
{
	ssize_t check;

	check = read(file, pos, size);
	if (check < 0) {
		Debug("read error");
		*ret = 0;
		return check;
	}
	if (check == 0) {
		*ret = 0;
		return 1;
	}
	else {
		*ret = (size_t)check;
		return 0;
	}
}

static inline int write_arch(file_type file, const void *pos, size_t size, size_t *ret)
{
	ssize_t check;

	check = write(file, pos, size);
	if (check < 0) {
		Debug("write error");
		*ret = 0;
		return check;
	}
	if (check == 0) {
		*ret = 0;
		return 1;
	}
	else {
		*ret = (size_t)check;
		return 0;
	}
}

static inline int close_arch(file_type file)
{
	if (file == STDIN_FILENO)
		return 0;
	if (file == STDOUT_FILENO)
		return 0;
	if (file == STDERR_FILENO)
		return 0;

	if (close(file)) {
		Debug("close error");
		return 1;
	}

	return 0;
}

static inline int flush_arch(file_type file)
{
	if (fsync(file) && errno != EINVAL) {
		Debug("fsync error");
		return 1;
	}

	return 0;
}

static inline int read_ready_arch(file_type file)
{
	int result;
	fd_set fds;
	struct timeval timeout;

	FD_ZERO(&fds);
	FD_SET(file, &fds);
	memset(&timeout, 0, sizeof(timeout));

	result = select(file + 1, &fds, NULL, NULL, &timeout);
	if (result < 0) {
		Debug("select error");
		return 1;
	}

	return result != 0;
}

static inline int file_length_arch(file_type file, size_t *ret)
{
	struct stat st;

	if (fstat(file, &st)) {
		*ret = 0;
		return 1;
	}
	else {
		*ret = (size_t)st.st_size;
		return 0;
	}
}

static inline int file_position_arch(file_type file, size_t *ret)
{
	off_t pos;

	pos = lseek(file, 0, SEEK_CUR);
	if (pos < 0) {
		*ret = 0;
		return 1;
	}
	*ret = (size_t)pos;

	return 0;
}

static inline int file_position_start_arch(file_type file)
{
	return lseek(file, 0, SEEK_SET) < 0;
}

static inline int file_position_end_arch(file_type file)
{
	return lseek(file, 0, SEEK_END) < 0;
}

static inline int file_position_set_arch(file_type file, size_t pos)
{
	return lseek(file, (off_t)pos, SEEK_SET) < 0;
}

#endif

