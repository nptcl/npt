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
#include "cons.h"
#include "cons_list.h"
#include "encode.h"
#include "local.h"
#include "object.h"
#include "pathname.h"
#include "typedef.h"

#define create_chmod_644 (S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH)

_g int init_file(void)
{
	return 0;
}

_g void free_file(void)
{
}

_g int consolep_file(void)
{
	return isatty(STDIN_FILENO) &&
		isatty(STDOUT_FILENO) &&
		isatty(STDERR_FILENO);
}

static inline void standard_input_arch(file_type *file)
{
	*file = STDIN_FILENO;
}

static inline void standard_output_arch(file_type *file)
{
	*file = STDOUT_FILENO;
}

static inline void standard_error_arch(file_type *file)
{
	*file = STDERR_FILENO;
}

static inline int filename_encode(Execute ptr, addr name, const byte **const ret)
{
	name_pathname_local(ptr, name, &name);
	if (UTF8_buffer_clang(ptr->local, &name, name)) {
		Debug("UTF8_buffer_clang error");
		return 1;
	}
	posbody(name, (addr *)ret);

	return 0;
}

static inline int open_input_chartype(file_type *ret, const char *name)
{
	file_type file;

	file = open(name, O_RDONLY);
	if (file < 0) return 1;
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

static inline int open_input_arch(Execute ptr, file_type *ret, addr name)
{
	int result;
	LocalRoot local;
	LocalStack stack;
	const byte *utf8;

	result = 0;
	local = ptr->local;
	push_local(local, &stack);
	if (filename_encode(ptr, name, &utf8)) {
		result = 2;
		goto finish;
	}
	if (open_input_chartype(ret, (const char *)utf8)) {
		result = 1;
		goto finish;
	}

finish:
	rollback_local(local, stack);
	return result;
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

static inline int open_output_arch(Execute ptr, file_type *ret,
		addr name, enum FileOutput mode)
{
	int result;
	LocalRoot local;
	LocalStack stack;
	file_type file;
	const byte *utf8;

	result = 0;
	local = ptr->local;
	push_local(local, &stack);
	if (filename_encode(ptr, name, &utf8)) {
		result = 2;
		goto finish;
	}

	file = open_output_call((const char *)utf8, mode);
	if (file < 0) {
		result = 1;
		goto finish;
	}
	*ret = file;

finish:
	rollback_local(local, stack);
	return result;
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

static inline int open_io_arch(Execute ptr, file_type *ret,
		addr name, enum FileOutput mode)
{
	int result;
	LocalRoot local;
	LocalStack stack;
	file_type file;
	const byte *utf8;

	result = 0;
	local = ptr->local;
	push_local(local, &stack);
	if (filename_encode(ptr, name, &utf8)) {
		result = 2;
		goto finish;
	}

	file = open_io_call((const char *)utf8, mode);
	if (file < 0) {
		result = 1;
		goto finish;
	}
	*ret = file;

finish:
	rollback_local(local, stack);
	return result;
}

static inline int readcall_arch(file_type file, void *pos, size_t size, size_t *ret)
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

static inline int writecall_arch(file_type file,
		const void *pos, size_t size, size_t *ret)
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
	if (file == STDIN_FILENO) return 0;
	if (file == STDOUT_FILENO) return 0;
	if (file == STDERR_FILENO) return 0;

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

