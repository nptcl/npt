#include "define.h"
#include "file_buffering.h"
#include "file_memory.h"
#include "heap_memory.h"

#ifdef LISP_ANSI
#undef FILEMEMORY_RESTRICT
#include "file_ansi.h"
#endif

#ifdef LISP_POSIX
#undef FILEMEMORY_RESTRICT
#include "file_posix.h"
#endif

#ifdef LISP_WINDOWS
#define FILEMEMORY_RESTRICT 0xFFFFFFFF
#include "file_windows.h"
#endif

#include "file_arch.h"


/*****************************************************************************
 *  filememory source
 *****************************************************************************/
#ifdef FILEMEMORY_RESTRICT
#define RESTRICTSIZE(x) (FILEMEMORY_RESTRICT <= (x)? FILEMEMORY_RESTRICT: (x))
#else
#define RESTRICTSIZE(x) (x)
#endif

static inline void init_filememory(filestream fm)
{
	fm->index = fm->size = 0;
	fm->cache = 1;
	fm->readio = 0;
	fm->redirect = 0;
	fm->ungetc = 0;
	fm->system = filememory_stream;
	fm->encode.type = EncodeType_utf8;
	fm->encode.bom = EncodeBom_auto;
	fm->encode.error = 1;
	fm->encode.create = 0;
	fm->encode.code = 0;
	fm->pos = NULL;
}

static inline void init_input_filememory(filestream fm, file_type file)
{
	init_filememory(fm);
	fm->file = file;
	fm->mode = filememory_normal;
	fm->direct = filememory_input;
}

static inline void init_output_filememory(filestream fm, file_type file)
{
	init_filememory(fm);
	fm->file = file;
	fm->mode = filememory_normal;
	fm->direct = filememory_output;
	fm->size = FILEMEMORY_SIZE;
}

static inline void init_io_filememory(filestream fm, file_type file)
{
	init_filememory(fm);
	fm->file = file;
	fm->mode = filememory_normal;
	fm->direct = filememory_io;
	fm->size = FILEMEMORY_SIZE;
}

/* low level open */
_g int input_unicode_filememory(filestream fm, const unicode *name, size_t size)
{
	file_type file;

	if (open_input_unicode(&file, name, size))
		return 1;
	init_input_filememory(fm, file);

	return 0;
}

_g void update_standard_input_filememory(filestream fm)
{
	standard_input_arch(&(fm->file));
}

_g void update_standard_output_filememory(filestream fm)
{
	standard_output_arch(&(fm->file));
}

_g void update_standard_error_filememory(filestream fm)
{
	standard_error_arch(&(fm->file));
}

/* normal function */
_g void standard_input_filememory(filestream fm)
{
	file_type file;

	standard_input_arch(&file);
	init_input_filememory(fm, file);
	fm->system = filememory_stdin;
	fm->cache = 0;
}

_g void standard_output_filememory(filestream fm)
{
	file_type file;

	standard_output_arch(&file);
	init_output_filememory(fm, file);
	fm->system = filememory_stdout;
	fm->cache = 0;
}

_g void standard_error_filememory(filestream fm)
{
	file_type file;

	standard_error_arch(&file);
	init_output_filememory(fm, file);
	fm->system = filememory_stderr;
	fm->cache = 0;
}

_g int open_input_filememory_(LocalRoot local,
		filestream fm, addr name, int *ret)
{
	int check;
	file_type file;

	Check(! stringp(name), "name error");
	Return(open_input_arch_(local, name, &file, &check));
	if (check)
		return Result(ret, 1);
	init_input_filememory(fm, file);

	return Result(ret, 0);
}

_g int open_output_filememory_(LocalRoot local,
		filestream fm, addr name, enum FileOutput mode, int *ret)
{
	int check;
	file_type file;

	Check(! stringp(name), "name error");
	Return(open_output_arch_(local, name, mode, &file, &check));
	if (check)
		return Result(ret, 1);
	init_output_filememory(fm, file);

	return Result(ret, 0);
}

_g int open_io_filememory_(LocalRoot local,
		filestream fm, addr name, enum FileOutput mode, int *ret)
{
	int check;
	file_type file;

	Check(! stringp(name), "name error");
	Return(open_io_arch_(local, name, mode, &file, &check));
	if (check)
		return Result(ret, 1);
	init_io_filememory(fm, file);

	return Result(ret, 0);
}

_g void open_input_redirect_filememory_(filestream fm, addr pos)
{
	file_type file;

	Check(! streamp(pos), "stream error");
	cleartype(file);
	init_input_filememory(fm, file);
	fm->redirect = 1;
	fm->cache = 0;
}

_g void open_output_redirect_filememory_(filestream fm, addr pos)
{
	file_type file;

	Check(! streamp(pos), "stream error");
	cleartype(file);
	init_output_filememory(fm, file);
	fm->redirect = 1;
	fm->cache = 0;
}

_g void open_io_redirect_filememory_(filestream fm, addr pos)
{
	file_type file;

	Check(! streamp(pos), "stream error");
	cleartype(file);
	init_io_filememory(fm, file);
	fm->redirect = 1;
	fm->cache = 0;
}

_g int close_filememory(filestream fm)
{
	if (fm->mode == filememory_close) {
		Debug("file already closed.");
		return 1;
	}
	if (flush_filememory(fm)) {
		Debug("flush_filememory error");
		return 1;
	}
	if (close_low(fm)) {
		Debug("close_low error");
		fm->mode = filememory_error;
		return 1;
	}
	fm->mode = filememory_close;

	return 0;
}


/*
 *  read/write call
 */
#define stream_errorcheck(fm, check, name) { \
	if (check < 0) { \
		Debug(name "error"); \
		fm->mode = filememory_error; \
		return check; \
	} \
}

#define fmend(fm) (fm->size <= fm->index)
#define fmreadforce(fm, s, r) (readforce(fm, fm->buffer, s, r))
#define fmwriteforce(fm, s, r) (writeforce(fm, fm->buffer, s, r))

static inline int readforce(filestream fm, byte *pos, size_t size, size_t *ret)
{
	int check;
	size_t count, rsize, diff;

	for (count = 0; count < size; count += rsize) {
		diff = size - count;
		check = read_low(fm, (void *)pos, RESTRICTSIZE(diff), &rsize);
		/* Error */
		if (check < 0) {
			Debug("read_low error");
			return check;
		}
		/* EOF */
		if (check) {
			if (count == 0)
				return check;
			break;
		}
		/* Next */
		pos += rsize;
	}
	*ret = count;

	return 0;
}

static inline int writeforce(filestream fm, const byte *pos, size_t size, size_t *ret)
{
	int check;
	size_t count, rsize, diff;

	for (count = 0; count < size; count += rsize) {
		diff = size - count;
		check = write_low(fm, (const void *)pos, RESTRICTSIZE(diff), &rsize);
		/* Error */
		if (check < 0) {
			Debug("write_low error");
			return check;
		}
		/* EOF */
		if (check) {
			if (count == 0)
				return check;
			break;
		}
		/* Next */
		pos += rsize;
	}
	*ret = count;

	return 0;
}

static inline int readforce_nonblocking(filestream fm,
		byte *pos, size_t size, size_t *ret)
{
	int check;
	size_t rsize, count;

	count = 0;
	for (;;) {
		/* ready check */
		check = read_ready_low(fm);
		if (check < 0) {
			Debug("read_ready_low error");
			return check;
		}
		if (check == 0) /* no ready */
			break;

		/* read_low */
		check = read_low(fm, pos, size, &rsize);
		if (check < 0)  {
			Debug("read_low error");
			return check;
		}
		if (check) {
			if (count == 0)
				return 1;
			break;
		}
		if (size <= rsize) {
			count += rsize;
			break;
		}
		pos += rsize;
		size -= rsize;
		count += rsize;
	}
	*ret = count;

	return 0;
}


/*
 *  read
 */
static inline int readnext_large(filestream fm,
		byte *pos, size_t size, size_t *ret)
{
	int check;
	size_t rsize;

	check = readforce(fm, pos, size, &rsize);
	stream_errorcheck(fm, check, "readforce");
	if (check) {
		fm->mode = filememory_end;
		return check;
	}
	*ret = rsize;

	return 0;
}

static inline int readnext_small(filestream fm,
		byte *pos, size_t size, size_t *ret)
{
	int check;
	size_t rsize;

	check = fmreadforce(fm, FILEMEMORY_SIZE, &rsize);
	stream_errorcheck(fm, check, "fmreadforce");
	if (check) {
		fm->mode = filememory_end;
		return check;
	}
	if (rsize <= size) {
		memcpy(pos, fm->buffer, rsize);
		fm->index = 0;
		*ret = rsize;
	}
	else {
		memcpy(pos, fm->buffer, size);
		fm->index = size;
		fm->size = rsize;
		*ret = size;
	}

	return 0;
}

static inline int readnext(filestream fm, byte *pos, size_t size, size_t *ret)
{
	if ((FILEMEMORY_SIZE / 2) < size)
		return readnext_large(fm, pos, size, ret);
	else
		return readnext_small(fm, pos, size, ret);
}

static inline int readbuffer_call(filestream fm,
		byte *pos, size_t size, size_t *ret,
		int (*next)(filestream , byte *, size_t, size_t *))
{
	int check;
	size_t diff, result;

	/* Request size is smaller than fm.buffer. -> buffer */
	diff = fm->size - fm->index;
	if (size < diff) {
		memcpy(pos, fm->buffer + fm->index, size);
		fm->index += size;
		*ret = size;
		return 0;
	}

	/* Request size is equal to fm.buffer. -> next */
	memcpy(pos, fm->buffer + fm->index, diff);
	fm->index = 0;
	if (size == diff) {
		*ret = diff;
		return 0;
	}

	/* Request size is greater than fm.buffer. -> next */
	check = next(fm, pos + diff, size - diff, &result);
	if (check < 0) {
		Debug("call next error");
		return check;
	}
	if (check) {
		*ret = diff;
		return 0; /* not EOF */
	}
	*ret = result + diff;

	return 0;
}

static inline int readbuffer(filestream fm,
		byte *pos, size_t size, size_t *ret)
{
	return readbuffer_call(fm, pos, size, ret, readnext);
}

static inline int readungetc_call(filestream fm,
		byte *pos, size_t size, size_t *ret,
		int (*next)(filestream , byte *, size_t, size_t *),
		int (*buffer)(filestream , byte *, size_t, size_t *))
{
	int check;
	size_t count, result;

	/* ungetc stack */
	for (count = 0; fm->ungetc && size; count++) {
		fm->ungetc--;
		*(pos++) = fm->ungetc_value[fm->ungetc];
		size--;
	}

	/* Success */
	if (size == 0) {
		*ret = count;
		return 0;
	}

	/* Read tail */
	if (fm->index == 0)
		check = next(fm, pos, size, &result);
	else
		check = buffer(fm, pos, size, &result);
	if (check < 0) {
		Debug("call function error");
		return check;
	}
	if (check) {
		*ret = 1;
		return 0;  /* not EOF. */
	}
	*ret = result + count;

	return 0;
}

static inline int readungetc(filestream fm,
		byte *pos, size_t size, size_t *ret)
{
	return readungetc_call(fm, pos, size, ret, readnext, readbuffer);
}

static int readnocache(filestream fm, byte *pos, size_t size, size_t *ret)
{
	int check;
	size_t count, result;

	/* ungetc stack */
	for (count = 0; fm->ungetc && size; count++) {
		fm->ungetc--;
		*(pos++) = fm->ungetc_value[fm->ungetc];
		size--;
	}

	/* Success */
	if (size == 0) {
		*ret = count;
		return 0;
	}

	/* Read tail */
	Check(fm->index, "index error");
	check = read_low(fm, (void *)pos, size, &result);
	if (check < 0) {
		Debug("call function error");
		return check;
	}
	if (check) {
		*ret = 1;
		return count == 0;
	}
	*ret = result + count;

	return 0;
}

static inline int read_normal(filestream fm, byte *pos, size_t size, size_t *ret)
{
	if (! fm->cache)
		return readnocache(fm, pos, size, ret);
	if (fm->ungetc)
		return readungetc(fm, pos, size, ret);
	if (fm->index == 0)
		return readnext(fm, pos, size, ret);
	else
		return readbuffer(fm, pos, size, ret);
}

static int flush_io_filememory(filestream fm, int readp)
{
	if (readp && fm->readio)
		return 0;
	if ((! readp) && (! fm->readio))
		return 0;
	fm->readio = readp? 1: 0;
	return flush_filememory(fm);
}

_g int read_filememory(filestream fm, void *dst, size_t size, size_t *ret)
{
	if (fm->direct == filememory_output) {
		Debug("direction error");
		return -1;
	}
	if (fm->direct == filememory_io) {
		if (flush_io_filememory(fm, 1)) {
			Debug("flush_io error");
			return -1;
		}
	}
	if (size == 0) {
		*ret = 0;
		return 0;
	}

	switch (fm->mode) {
		case filememory_normal:
			return read_normal(fm, (byte *)dst, size, ret);

		case filememory_end:
			return 1;

		case filememory_error:
		default:
			Debug("type error");
			return -1;
	}

	return 0;
}

static inline int read_normal_force(filestream fm,
		byte *pos, size_t size, size_t *ret)
{
	int check;
	size_t count, result;

	for (count = 0; count < size; count += result) {
		check = read_normal(fm, pos + count, size - count, &result);
		stream_errorcheck(fm, check, "read_normal");
		if (check) {
			if (count)
				break;
			fm->mode = filememory_end;
			return check;
		}
	}
	*ret = count;

	return 0;
}

_g int readf_filememory(filestream fm, void *dst, size_t size, size_t *ret)
{
	if (fm->direct == filememory_output) {
		Debug("direction error");
		return -1;
	}
	if (fm->direct == filememory_io) {
		if (flush_io_filememory(fm, 1)) {
			Debug("flush_io error");
			return -1;
		}
	}
	if (size == 0) {
		*ret = 0;
		return 0;
	}

	switch (fm->mode) {
		case filememory_normal:
			return read_normal_force(fm, (byte *)dst, size, ret);

		case filememory_end:
			return 1;

		case filememory_error:
		default:
			Debug("type error");
			return -1;
	}

	return 0;
}


/*
 *  read-nonblocking
 */
static inline int readnext_nonblocking_large(filestream fm,
		byte *pos, size_t size, size_t *ret)
{
	int check;
	size_t rsize;

	check = readforce_nonblocking(fm, pos, size, &rsize);
	stream_errorcheck(fm, check, "readforce_nonblocking");
	if (check) {
		fm->mode = filememory_end;
		return check;
	}
	*ret = rsize;

	return 0;
}

static inline int readnext_nonblocking_small(filestream fm,
		byte *pos, size_t size, size_t *ret)
{
	int check;
	size_t rsize;

	check = readforce_nonblocking(fm, fm->buffer, FILEMEMORY_SIZE, &rsize);
	stream_errorcheck(fm, check, "readforce_nonblocking");
	if (check) {
		fm->mode = filememory_end;
		return check;
	}
	if (rsize <= size) {
		memcpy(pos, fm->buffer, rsize);
		fm->index = 0;
		*ret = rsize;
	}
	else {
		memcpy(pos, fm->buffer, size);
		fm->index = size;
		fm->size = rsize;
		*ret = size;
	}

	return 0;
}

static inline int readnext_nonblocking(filestream fm,
		byte *pos, size_t size, size_t *ret)
{
	if ((FILEMEMORY_SIZE / 2) < size)
		return readnext_nonblocking_large(fm, pos, size, ret);
	else
		return readnext_nonblocking_small(fm, pos, size, ret);
}

static inline int readbuffer_nonblocking(filestream fm,
		byte *pos, size_t size, size_t *ret)
{
	return readbuffer_call(fm, pos, size, ret, readnext_nonblocking);
}

static inline int readungetc_nonblocking(filestream fm,
		byte *pos, size_t size, size_t *ret)
{
	return readungetc_call(fm, pos, size, ret,
			readnext_nonblocking, readbuffer_nonblocking);
}

static int readnocache_nonblocking(filestream fm,
		byte *pos, size_t size, size_t *ret)
{
	int check;
	size_t count, result;

	/* ungetc stack */
	for (count = 0; fm->ungetc && size; count++) {
		fm->ungetc--;
		*(pos++) = fm->ungetc_value[fm->ungetc];
		size--;
	}

	/* Success */
	if (size == 0) {
		*ret = count;
		return 0;
	}

	/* Read tail */
	Check(fm->index, "index error");
	check = read_ready_low(fm);
	if (check < 0) {
		Debug("read_ready_low error");
		return check;
	}
	if (check == 0) {
		/* no ready */
		result = 0;
	}
	else {
		/* ready */
		check = read_low(fm, (void *)pos, size, &result);
		if (check < 0) {
			Debug("call function error");
			return check;
		}
		if (check) {
			*ret = 1;
			return count == 0;
		}
	}
	*ret = result + count;

	return 0;
}

static inline int read_nonblocking(filestream fm,
		byte *pos, size_t size, size_t *ret)
{
	if (! fm->cache)
		return readnocache_nonblocking(fm, pos, size, ret);
	if (fm->ungetc)
		return readungetc_nonblocking(fm, pos, size, ret);
	if (fm->index == 0)
		return readnext_nonblocking(fm, pos, size, ret);
	else
		return readbuffer_nonblocking(fm, pos, size, ret);
}

_g int read_nonblocking_filememory(filestream fm,
		void *dst, size_t size, size_t *ret)
{
	if (fm->direct == filememory_output) {
		Debug("direction error");
		return -1;
	}
	if (fm->direct == filememory_io) {
		if (flush_io_filememory(fm, 1)) {
			Debug("flush_io error");
			return -1;
		}
	}
	if (size == 0) {
		*ret = 0;
		return 0;
	}

	switch (fm->mode) {
		case filememory_normal:
			return read_nonblocking(fm, (byte *)dst, size, ret);

		case filememory_end:
			return 1;

		case filememory_error:
		default:
			Debug("type error");
			return -1;
	}

	return 0;
}


/*
 *  getc
 */
static inline int readbuffer_small(filestream fm,
		byte *pos, size_t size, size_t *ret)
{
	return readbuffer_call(fm, pos, size, ret, readnext_small);
}

static inline int readungetc_small(filestream fm,
		byte *pos, size_t size, size_t *ret)
{
	return readungetc_call(fm, pos, size, ret, readnext_small, readbuffer_small);
}

static inline int getc_normal(filestream fm, byte *pos)
{
	size_t dummy;

	if (! fm->cache)
		return readnocache(fm, pos, 1, &dummy);
	if (fm->ungetc)
		return readungetc_small(fm, pos, 1, &dummy);
	if (fm->index == 0)
		return readnext_small(fm, pos, 1, &dummy);
	else
		return readbuffer_small(fm, pos, 1, &dummy);
}

_g int getc_filememory(filestream fm, byte *pos)
{
	if (fm->direct == filememory_output) {
		Debug("direction error");
		return -1;
	}
	if (fm->direct == filememory_io) {
		if (flush_io_filememory(fm, 1)) {
			Debug("flush_io error");
			return -1;
		}
	}

	switch (fm->mode) {
		case filememory_normal:
			return getc_normal(fm, pos);

		case filememory_end:
			return 1;

		case filememory_error:
		default:
			Debug("type error");
			return -1;
	}

	return 0;
}

static inline int readbuffer_nonblocking_small(filestream fm,
		byte *pos, size_t size, size_t *ret)
{
	return readbuffer_call(fm, pos, size, ret, readnext_nonblocking_small);
}

static inline int readungetc_nonblocking_small(filestream fm,
		byte *pos, size_t size, size_t *ret)
{
	return readungetc_call(fm, pos, size, ret,
			readnext_nonblocking_small,
			readbuffer_nonblocking_small);
}

static int getc_nonblocking(filestream fm, byte *pos, size_t *ret)
{
	if (! fm->cache)
		return readnocache_nonblocking(fm, pos, 1, ret);
	if (fm->ungetc)
		return readungetc_nonblocking_small(fm, pos, 1, ret);
	if (fm->index == 0)
		return readnext_nonblocking_small(fm, pos, 1, ret);
	else
		return readbuffer_nonblocking_small(fm, pos, 1, ret);
}

_g int getc_nonblocking_filememory(filestream fm, byte *pos, size_t *ret)
{
	if (fm->direct == filememory_output) {
		Debug("direction error");
		return -1;
	}
	if (fm->direct == filememory_io) {
		if (flush_io_filememory(fm, 1)) {
			Debug("flush_io error");
			return -1;
		}
	}

	switch (fm->mode) {
		case filememory_normal:
			return getc_nonblocking(fm, pos, ret);

		case filememory_end:
			return 1;

		case filememory_error:
		default:
			Debug("type error");
			return -1;
	}

	return 0;
}

_g int ungetc_filememory(filestream fm, byte c)
{
	if (fm->direct == filememory_output) {
		Debug("direction error");
		return -1;
	}
	if (FILEMEMORY_UNGETC_SIZE <= fm->ungetc) {
		Debug("ungetc stack overflow.");
		return -1;
	}

	switch (fm->mode) {
		case filememory_normal:
			fm->ungetc_value[fm->ungetc++] = c;
			return 0;

		case filememory_end:
			return 1;

		case filememory_error:
		default:
			Debug("type error");
			return -1;
	}

	return 0;
}


/*
 *  write
 */
static inline int flush_write(filestream fm)
{
	int check;
	size_t rsize;

	if (fm->index) {
		check = fmwriteforce(fm, fm->index, &rsize);
		stream_errorcheck(fm, check, "fmwriteforce");
		if (check) {
			fm->mode = filememory_end;
			return check;
		}
		fm->index = 0;
	}

	return 0;
}

static int write_normal(filestream fm,
		const byte *pos, size_t size, size_t *ret)
{
	int check;
	size_t index, rsize, diff;

	index = fm->index;
	/* large copy */
	if (FILEMEMORY_SIZE <= size) {
		check = flush_write(fm);
		stream_errorcheck(fm, check, "flush_write");
		if (check) { /* EOF */
			return check;
		}

		/* write from memory */
		check = writeforce(fm, pos, size, &rsize);
		stream_errorcheck(fm, check, "writeforce");
		if (check) {
			fm->mode = filememory_end;
			return check;
		}
		*ret = rsize;
		return 0;
	}

	/* small copy */
	if (FILEMEMORY_SIZE < (size + index)) {
		diff = FILEMEMORY_SIZE - index;
		memcpy(fm->buffer + index, pos, diff);

		check = fmwriteforce(fm, FILEMEMORY_SIZE, &rsize);
		stream_errorcheck(fm, check, "fmwriteforce");
		if (check) {
			fm->mode = filememory_end;
			return check;
		}

		rsize = size - diff;
		memcpy(fm->buffer, pos + diff, rsize);
		fm->index = rsize;
		*ret = size;
		return 0;
	}

	/* memory only */
	memcpy(fm->buffer + fm->index, pos, size);
	fm->index += size;
	*ret = size;

	return 0;
}

_g int write_filememory(filestream fm,
		const void *dst, size_t size, size_t *ret)
{
	if (fm->direct == filememory_input) {
		Debug("direction error");
		return -1;
	}
	if (fm->direct == filememory_io) {
		if (flush_io_filememory(fm, 0)) {
			Debug("flush_io error");
			return -1;
		}
	}
	if (size == 0) {
		*ret = 0;
		return 0;
	}

	switch (fm->mode) {
		case filememory_normal:
			return write_normal(fm, (byte *)dst, size, ret);

		case filememory_end:
			return 1;

		case filememory_error:
		default:
			Debug("type error");
			return -1;
	}

	return 0;
}

/* read type */
_g int read_s16_filememory(filestream fm, int16_t *ret)
{
	int check;
	union read_s16_union {
		byte a[2];
		int16_t v;
	} u;
	size_t size;

	check = readf_filememory(fm, (void *)u.a, 2, &size);
	if (check)
		return check;
	if (size != 2)
		return 1;

	*ret = u.v;
	return 0;
}

_g int read_s32_filememory(filestream fm, int32_t *ret)
{
	int check;
	union read_s32_union {
		byte a[4];
		int32_t v;
	} u;
	size_t size;

	check = readf_filememory(fm, (void *)u.a, 4, &size);
	if (check)
		return check;
	if (size != 4)
		return 1;

	*ret = u.v;
	return 0;
}

#ifdef LISP_ARCH_64BIT
_g int read_s64_filememory(filestream fm, int64_t *ret)
{
	int check;
	union read_s64_union {
		byte a[8];
		int64_t v;
	} u;
	size_t size;

	check = readf_filememory(fm, (void *)u.a, 8, &size);
	if (check)
		return check;
	if (size != 8)
		return 1;

	*ret = u.v;
	return 0;
}
#endif

_g int read_u16_filememory(filestream fm, uint16_t *ret)
{
	int check;
	union read_u16_union {
		byte a[2];
		uint16_t v;
	} u;
	size_t size;

	check = readf_filememory(fm, (void *)u.a, 2, &size);
	if (check)
		return check;
	if (size != 2)
		return 1;

	*ret = u.v;
	return 0;
}

_g int read_u32_filememory(filestream fm, uint32_t *ret)
{
	int check;
	union read_u32_union {
		byte a[4];
		uint32_t v;
	} u;
	size_t size;

	check = readf_filememory(fm, (void *)u.a, 4, &size);
	if (check)
		return check;
	if (size != 4)
		return 1;

	*ret = u.v;
	return 0;
}

#ifdef LISP_ARCH_64BIT
_g int read_u64_filememory(filestream fm, uint64_t *ret)
{
	int check;
	union read_u64_union {
		byte a[8];
		uint64_t v;
	} u;
	size_t size;

	check = readf_filememory(fm, (void *)u.a, 8, &size);
	if (check)
		return check;
	if (size != 8)
		return 1;

	*ret = u.v;
	return 0;
}
#endif

/* write type */
static int write_type_filememory(filestream fm, const void *p, size_t n)
{
	int check;
	size_t size;

	check = write_filememory(fm, p, n, &size);
	if (check)
		return check;
	if (size != n)
		return 1;

	return 0;
}

_g int write_s16_filememory(filestream fm, int16_t c)
{
	return write_type_filememory(fm, (const void *)&c, 2);
}

_g int write_s32_filememory(filestream fm, int32_t c)
{
	return write_type_filememory(fm, (const void *)&c, 4);
}

#ifdef LISP_ARCH_64BIT
_g int write_s64_filememory(filestream fm, int64_t c)
{
	return write_type_filememory(fm, (const void *)&c, 8);
}
#endif

_g int write_u16_filememory(filestream fm, uint16_t c)
{
	return write_type_filememory(fm, (const void *)&c, 2);
}

_g int write_u32_filememory(filestream fm, uint32_t c)
{
	return write_type_filememory(fm, (const void *)&c, 4);
}

#ifdef LISP_ARCH_64BIT
_g int write_u64_filememory(filestream fm, uint64_t c)
{
	return write_type_filememory(fm, (const void *)&c, 8);
}
#endif


/*
 *  putc
 */
static int putcnormal(filestream fm, byte c)
{
	int check;
	size_t rsize;

	if (FILEMEMORY_SIZE <= fm->index) {
		check = fmwriteforce(fm, FILEMEMORY_SIZE, &rsize);
		stream_errorcheck(fm, check, "fmwriteforce");
		if (check) {
			fm->mode = filememory_end;
			return check;
		}
		fm->index = 0;
	}
	fm->buffer[fm->index++] = c;

	return 0;
}

_g int putc_filememory(filestream fm, byte c)
{
	if (fm->direct == filememory_input) {
		Debug("direction error");
		return -1;
	}
	if (fm->direct == filememory_io) {
		if (flush_io_filememory(fm, 0)) {
			Debug("flush_io error");
			return -1;
		}
	}

	switch (fm->mode) {
		case filememory_normal:
			return putcnormal(fm, c);

		case filememory_end:
			return 1;

		case filememory_error:
		default:
			Debug("type error");
			return -1;
	}

	return 0;
}


/*
 *  flush
 */
_g int flush_filememory(filestream fm)
{
	int check;

	if (fm->mode == filememory_end)
		goto sync;
	switch (fm->direct) {
		case filememory_input:
			break;

		case filememory_output:
			check = flush_write(fm);
			stream_errorcheck(fm, check, "flush_write");
			if (check)
				fm->mode = filememory_end;
			break;

		case filememory_io:
			if (! fm->readio) {
				check = flush_write(fm);
				stream_errorcheck(fm, check, "flush_write");
				if (check)
					fm->mode = filememory_end;
			}
			break;

		default:
			Debug("direction error");
			return -1;
	}

sync:
	if (fm->direct == filememory_output
			|| (fm->direct == filememory_io && (! fm->readio)))
		flush_low(fm);

	return 0;
}

_g void exitpoint_filememory(filestream fm)
{
	if (fm->cache == 0)
		(void)flush_filememory(fm);
}

_g int end_filememory(filestream fm)
{
	if (fm->mode == filememory_end)
		return 1;
	if (fm->mode == filememory_error) {
		Debug("mode error");
		return -1;
	}

	return 0;
}

_g int error_filememory(filestream fm)
{
	return fm->mode == filememory_error;
}

_g int listen_filememory(filestream fm)
{
	int check;

	if (fm->direct != filememory_input) {
		Debug("direction error");
		return -1;
	}
	if (fm->mode == filememory_end)
		return 0;
	if (fm->mode != filememory_normal)
		return 1;
	if (fm->index == 0) {
		check = read_ready_low(fm);
#ifdef LISP_DEBUG
		if (check < 0)
			Debug("read_ready_low error");
#endif
		return check;
	}

	return 1;  /* ready */
}

_g int clear_input_filememory(filestream fm)
{
	switch (fm->direct) {
		case filememory_io:
			if (! fm->readio)
				return 0;
			/* FALLTHROUGH */
		case filememory_input:
			if (fm->mode == filememory_end)
				return 0;
			if (fm->mode != filememory_normal)
				return 1;
			/* clear buffer */
			fm->ungetc = 0;
			fm->index = fm->size = 0;
			return 0;

		default:
			Debug("direction error");
			return -1;
	}
}

_g int clear_output_filememory(filestream fm)
{
	switch (fm->direct) {
		case filememory_io:
			if (fm->readio)
				return 0;
			/* FALLTHROUGH */
		case filememory_output:
			if (fm->mode == filememory_end)
				return 0;
			if (fm->mode != filememory_normal)
				return 1;
			/* clear buffer */
			fm->index = 0;
			fm->size = FILEMEMORY_SIZE;
			return 0;

		default:
			Debug("direction error");
			return -1;
	}
}

_g int file_length_filememory(filestream fm, size_t *ret)
{
	if (fm->mode == filememory_normal || fm->mode == filememory_end)
		return file_length_low(fm, ret);
	else
		return 1;
}

static int file_position_input(filestream fm)
{
	return fm->direct == filememory_input ||
		(fm->direct == filememory_io && fm->readio);
}

_g int file_position_filememory(filestream fm, size_t *ret)
{
	size_t size, unread;

	if (fm->mode != filememory_normal && fm->mode != filememory_end)
		return 1;
	file_position_low(fm, &size);
	if (file_position_input(fm)) {
		/* input */
		size += fm->index;
		size -= fm->size;
		unread = (size_t)fm->ungetc;
		if (size < unread)
			return -1;
		*ret = size - unread;
	}
	else {
		/* output */
		*ret = size + fm->index;
	}

	return 0;
}

_g int file_position_start_filememory(filestream fm)
{
	int check;

	if (fm->mode == filememory_normal || fm->mode == filememory_end) {
		check = file_position_start_low(fm);
		if (check)
			return check;
		fm->index = 0;
		fm->ungetc = 0;
		return 0;
	}

	return 1;
}

_g int file_position_end_filememory(filestream fm)
{
	int check;

	if (fm->mode == filememory_normal || fm->mode == filememory_end) {
		check = file_position_end_low(fm);
		if (check)
			return check;
		fm->index = 0;
		fm->ungetc = 0;
		return 0;
	}

	return 1;
}

_g int file_position_set_filememory(filestream fm, size_t pos)
{
	int check;

	if (fm->mode == filememory_normal || fm->mode == filememory_end) {
		check = file_position_set_low(fm, pos);
		if (check)
			return check;
		fm->index = 0;
		fm->ungetc = 0;
		return 0;
	}

	return 1;
}


/*
 *  core
 */
_g int readcheck_filememory(filestream fm, void *dst, size_t size)
{
	size_t check;
	return readf_filememory(fm, dst, size, &check) || size != check;
}
_g int writecheck_filememory(filestream fm, const void *dst, size_t size)
{
	size_t check;
	return write_filememory(fm, dst, size, &check) || size != check;
}

_g int readptr_filememory(filestream fm, void **ret)
{
	uintptr_t ptr;

	if (readcheck_filememory(fm, &ptr, sizeoft(uintptr_t))) {
		Debug("readaddr error: filememory");
		return 1;
	}
	if (ptr == (uintptr_t)Unbound)
		*ret = (void *)Unbound;
	else
		*ret = (void *)(heap_root + ptr);

	return 0;
}
_g int writeptr_filememory(filestream fm, const void *pos)
{
	uintptr_t ptr;

	if (pos == Unbound) {
		return writecheck_filememory(fm, &pos, sizeoft(addr));
	}
	ptr = (uintptr_t)pos;
	if (ptr < (uintptr_t)heap_root || (uintptr_t)heap_front < ptr) {
		Debug("writeaddr error: out of range.");
		return 1;
	}
	ptr -= (uintptr_t)heap_root;

	return writecheck_filememory(fm, &ptr, sizeoft(uintptr_t));
}

_g int readaddr_filememory(filestream fm, addr *ret)
{
	return readptr_filememory(fm, (void **)ret);
}
_g int writeaddr_filememory(filestream fm, addr pos)
{
	uintptr_t ptr;

	if (pos == Unbound) {
		return writecheck_filememory(fm, &pos, sizeoft(addr));
	}
	if (GetStatusDynamic(pos)) {
		Debug("writeaddr error: dynamic object.");
		return 1;
	}
	ptr = (uintptr_t)pos;
	if (ptr < (uintptr_t)heap_root || (uintptr_t)heap_front < ptr) {
		Debug("writeaddr error: out of range.");
		return 1;
	}
	ptr -= (uintptr_t)heap_root;

	return writecheck_filememory(fm, &ptr, sizeoft(uintptr_t));
}

_g int readsize_filememory(filestream fm, size_t *ret)
{
	return readcheck_filememory(fm, ret, sizeoft(size_t));
}

_g int writesize_filememory(filestream fm, size_t pos)
{
	return writecheck_filememory(fm, &pos, sizeoft(size_t));
}
