#include "define.h"
#include "file_memory.h"

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


/*****************************************************************************
 *  filememory source
 *****************************************************************************/
#ifdef FILEMEMORY_RESTRICT
#define RESTRICTSIZE(x) (FILEMEMORY_RESTRICT <= (x)? FILEMEMORY_RESTRICT: (x))
#else
#define RESTRICTSIZE(x) (x)
#endif

static inline void init_filememory(struct filememory *fm)
{
	fm->index = fm->size = 0;
	fm->cache = 1;
	fm->readio = 0;
	fm->ungetc = 0;
	fm->system = filememory_stream;
	fm->encode.type = EncodeType_utf8;
	fm->encode.bom = EncodeBom_auto;
	fm->encode.error = 1;
	fm->encode.create = 0;
	fm->encode.code = 0;
}

static inline void init_input(struct filememory *fm, file_type file)
{
	init_filememory(fm);
	fm->file = file;
	fm->mode = filememory_normal;
	fm->direct = filememory_input;
}

static inline void init_output(struct filememory *fm, file_type file)
{
	init_filememory(fm);
	fm->file = file;
	fm->mode = filememory_normal;
	fm->direct = filememory_output;
	fm->size = FILEMEMORY_SIZE;
}

/* low level open */
int input_unicode_filememory(struct filememory *fm, const void *name, size_t size)
{
	file_type file;

	if (open_input_unicode(&file, name, size)) return 1;
	init_input(fm, file);

	return 0;
}

void update_standard_input_filememory(struct filememory *fm)
{
	standard_input_arch(&(fm->file));
}

void update_standard_output_filememory(struct filememory *fm)
{
	standard_output_arch(&(fm->file));
}

void update_standard_error_filememory(struct filememory *fm)
{
	standard_error_arch(&(fm->file));
}

/* normal function */
void standard_input_filememory(struct filememory *fm)
{
	file_type file;

	standard_input_arch(&file);
	init_input(fm, file);
	fm->system = filememory_stdin;
	fm->cache = 0;
}

void standard_output_filememory(struct filememory *fm)
{
	file_type file;

	standard_output_arch(&file);
	init_output(fm, file);
	fm->system = filememory_stdout;
	fm->cache = 0;
}

void standard_error_filememory(struct filememory *fm)
{
	file_type file;

	standard_error_arch(&file);
	init_output(fm, file);
	fm->system = filememory_stderr;
	fm->cache = 0;
}

int open_input_filememory(Execute ptr, struct filememory *fm, addr name)
{
	file_type file;

	if (open_input_arch(ptr, &file, name)) {
		return 1;
	}
	init_input(fm, file);

	return 0;
}

int open_output_filememory(Execute ptr, struct filememory *fm,
		addr name, enum FileOutput mode)
{
	file_type file;

	if (open_output_arch(ptr, &file, name, mode)) return 1;
	init_output(fm, file);

	return 0;
}

int open_io_filememory(Execute ptr, struct filememory *fm,
		addr name, enum FileOutput mode)
{
	file_type file;

	if (open_io_arch(ptr, &file, name, mode)) return 1;
	init_filememory(fm);
	fm->file = file;
	fm->mode = filememory_normal;
	fm->direct = filememory_io;
	fm->size = FILEMEMORY_SIZE;

	return 0;
}

int close_filememory(struct filememory *fm)
{
	if (fm->mode == filememory_close) {
		Debug("file already closed.");
		return 1;
	}
	if (flush_filememory(fm)) {
		Debug("flush_filememory error");
		return 1;
	}
	if (close_arch(fm->file)) {
		Debug("close_arch error");
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
#define fmreadforce(fm, s, r) (readforce(fm->file, fm->buffer, s, r))
#define fmwriteforce(fm, s, r) (writeforce(fm->file, fm->buffer, s, r))

static inline int readforce(file_type file, byte *pos, size_t size, size_t *ret)
{
	int check;
	size_t count, rsize, diff;

	for (count = 0; count < size; count += rsize) {
		diff = size - count;
		check = readcall_arch(file, (void *)pos, RESTRICTSIZE(diff), &rsize);
		/* Error */
		if (check < 0) {
			Debug("readcall_arch error");
			return check;
		}
		/* EOF */
		if (check) {
			if (count == 0) return check;
			break;
		}
		/* Next */
		pos += rsize;
	}
	*ret = count;

	return 0;
}

static inline int writeforce(file_type file, const byte *pos, size_t size, size_t *ret)
{
	int check;
	size_t count, rsize, diff;

	for (count = 0; count < size; count += rsize) {
		diff = size - count;
		check = writecall_arch(file, (const void *)pos, RESTRICTSIZE(diff), &rsize);
		/* Error */
		if (check < 0) {
			Debug("writecall_arch error");
			return check;
		}
		/* EOF */
		if (check) {
			if (count == 0) return check;
			break;
		}
		/* Next */
		pos += rsize;
	}
	*ret = count;

	return 0;
}

static inline int readforce_nonblocking(file_type file,
		byte *pos, size_t size, size_t *ret)
{
	int check;
	size_t rsize, count;

	count = 0;
	for (;;) {
		/* ready check */
		check = read_ready_arch(file);
		if (check < 0) {
			Debug("read_ready_arch error");
			return check;
		}
		if (check == 0) /* no ready */
			break;

		/* readcall_arch */
		check = readcall_arch(file, pos, size, &rsize);
		if (check < 0)  {
			Debug("readcall_arch error");
			return check;
		}
		if (check) {
			if (count == 0) return 1;
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
static inline int readnext_large(struct filememory *fm,
		byte *pos, size_t size, size_t *ret)
{
	int check;
	size_t rsize;

	check = readforce(fm->file, pos, size, &rsize);
	stream_errorcheck(fm, check, "readforce");
	if (check) {
		fm->mode = filememory_end;
		return check;
	}
	*ret = rsize;

	return 0;
}

static inline int readnext_small(struct filememory *fm,
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

static inline int readnext(struct filememory *fm, byte *pos, size_t size, size_t *ret)
{
	if ((FILEMEMORY_SIZE / 2) < size)
		return readnext_large(fm, pos, size, ret);
	else
		return readnext_small(fm, pos, size, ret);
}

static inline int readbuffer_call(struct filememory *fm,
		byte *pos, size_t size, size_t *ret,
		int (*next)(struct filememory *, byte *, size_t, size_t *))
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

static inline int readbuffer(struct filememory *fm,
		byte *pos, size_t size, size_t *ret)
{
	return readbuffer_call(fm, pos, size, ret, readnext);
}

static inline int readungetc_call(struct filememory *fm,
		byte *pos, size_t size, size_t *ret,
		int (*next)(struct filememory *, byte *, size_t, size_t *),
		int (*buffer)(struct filememory *, byte *, size_t, size_t *))
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

static inline int readungetc(struct filememory *fm,
		byte *pos, size_t size, size_t *ret)
{
	return readungetc_call(fm, pos, size, ret, readnext, readbuffer);
}

static int readnocache(struct filememory *fm, byte *pos, size_t size, size_t *ret)
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
	check = readcall_arch(fm->file, (void *)pos, size, &result);
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

static inline int read_normal(struct filememory *fm,
		byte *pos, size_t size, size_t *ret)
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

static int flush_io_filememory(struct filememory *fm, int readp)
{
	if (readp && fm->readio) return 0;
	if ((! readp) && (! fm->readio)) return 0;
	fm->readio = readp? 1: 0;
	return flush_filememory(fm);
}

int read_filememory(struct filememory *fm, void *dst, size_t size, size_t *ret)
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

static inline int read_normal_force(struct filememory *fm,
		byte *pos, size_t size, size_t *ret)
{
	int check;
	size_t count, result;

	for (count = 0; count < size; count += result) {
		check = read_normal(fm, pos + count, size - count, &result);
		stream_errorcheck(fm, check, "read_normal");
		if (check) {
			if (count) break;
			fm->mode = filememory_end;
			return check;
		}
	}
	*ret = count;

	return 0;
}

int readforce_filememory(struct filememory *fm, void *dst, size_t size, size_t *ret)
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
static inline int readnext_nonblocking_large(struct filememory *fm,
		byte *pos, size_t size, size_t *ret)
{
	int check;
	size_t rsize;

	check = readforce_nonblocking(fm->file, pos, size, &rsize);
	stream_errorcheck(fm, check, "readforce_nonblocking");
	if (check) {
		fm->mode = filememory_end;
		return check;
	}
	*ret = rsize;

	return 0;
}

static inline int readnext_nonblocking_small(struct filememory *fm,
		byte *pos, size_t size, size_t *ret)
{
	int check;
	size_t rsize;

	check = readforce_nonblocking(fm->file, fm->buffer, FILEMEMORY_SIZE, &rsize);
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

static inline int readnext_nonblocking(struct filememory *fm,
		byte *pos, size_t size, size_t *ret)
{
	if ((FILEMEMORY_SIZE / 2) < size)
		return readnext_nonblocking_large(fm, pos, size, ret);
	else
		return readnext_nonblocking_small(fm, pos, size, ret);
}

static inline int readbuffer_nonblocking(struct filememory *fm,
		byte *pos, size_t size, size_t *ret)
{
	return readbuffer_call(fm, pos, size, ret, readnext_nonblocking);
}

static inline int readungetc_nonblocking(struct filememory *fm,
		byte *pos, size_t size, size_t *ret)
{
	return readungetc_call(fm, pos, size, ret,
			readnext_nonblocking, readbuffer_nonblocking);
}

static int readnocache_nonblocking(struct filememory *fm,
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
	check = read_ready_arch(fm->file);
	if (check < 0) {
		Debug("read_ready_arch error");
		return check;
	}
	if (check == 0) {
		/* no ready */
		result = 0;
	}
	else {
		/* ready */
		check = readcall_arch(fm->file, (void *)pos, size, &result);
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

static inline int read_nonblocking(struct filememory *fm,
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

int read_nonblocking_filememory(struct filememory *fm,
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
static inline int readbuffer_small(struct filememory *fm,
		byte *pos, size_t size, size_t *ret)
{
	return readbuffer_call(fm, pos, size, ret, readnext_small);
}

static inline int readungetc_small(struct filememory *fm,
		byte *pos, size_t size, size_t *ret)
{
	return readungetc_call(fm, pos, size, ret, readnext_small, readbuffer_small);
}

static inline int getc_normal(struct filememory *fm, byte *pos)
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

int getc_filememory(struct filememory *fm, byte *pos)
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

static inline int readbuffer_nonblocking_small(struct filememory *fm,
		byte *pos, size_t size, size_t *ret)
{
	return readbuffer_call(fm, pos, size, ret, readnext_nonblocking_small);
}

static inline int readungetc_nonblocking_small(struct filememory *fm,
		byte *pos, size_t size, size_t *ret)
{
	return readungetc_call(fm, pos, size, ret,
			readnext_nonblocking_small,
			readbuffer_nonblocking_small);
}

static int getc_nonblocking(struct filememory *fm, byte *pos, size_t *ret)
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

int getc_nonblocking_filememory(struct filememory *fm, byte *pos, size_t *ret)
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

int ungetc_filememory(struct filememory *fm, byte c)
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
static inline int flush_write(struct filememory *fm)
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

static int writenormal(struct filememory *fm,
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
		check = writeforce(fm->file, pos, size, &rsize);
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

static int writenocache(struct filememory *fm,
		const byte *pos, size_t size, size_t *ret)
{
	int check;

	Check(fm->index, "index error");
	check = writeforce(fm->file, pos, size, ret);
	stream_errorcheck(fm, check, "writeforce");
	flush_arch(fm->file);
	if (check) {
		fm->mode = filememory_end;
		return check;
	}

	return 0;
}

int write_filememory(struct filememory *fm,
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
			if (fm->cache)
				return writenormal(fm, (byte *)dst, size, ret);
			else
				return writenocache(fm, (byte *)dst, size, ret);

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
 *  putc
 */
static int putcnormal(struct filememory *fm, byte c)
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

static int putcnocache(struct filememory *fm, byte c)
{
	size_t dummy;
	return writenocache(fm, &c, 1, &dummy);
}

int putc_filememory(struct filememory *fm, byte c)
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
			if (fm->cache)
				return putcnormal(fm, c);
			else
				return putcnocache(fm, c);

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
int flush_filememory(struct filememory *fm)
{
	int check;

	if (fm->mode == filememory_end) goto sync;
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
		flush_arch(fm->file);

	return 0;
}

int end_filememory(struct filememory *fm)
{
	if (fm->mode == filememory_end) return 1;
	if (fm->mode == filememory_error) {
		Debug("mode error");
		return -1;
	}

	return 0;
}

int error_filememory(struct filememory *fm)
{
	return fm->mode == filememory_error;
}

int listen_filememory(struct filememory *fm)
{
	int check;

	if (fm->direct != filememory_input) {
		Debug("direction error");
		return -1;
	}
	if (fm->mode == filememory_end) return 0;
	if (fm->mode != filememory_normal) return 1;
	if (fm->index == 0) {
		check = read_ready_arch(fm->file);
		if (check < 0)
			Debug("read_ready_arch error");
		return check;
	}

	return 1;  /* ready */
}

int clear_input_filememory(struct filememory *fm)
{
	switch (fm->direct) {
		case filememory_io:
			if (! fm->readio) return 0;
			/* FALLTHROUGH */
		case filememory_input:
			if (fm->mode == filememory_end) return 0;
			if (fm->mode != filememory_normal) return 1;
			/* clear buffer */
			fm->ungetc = 0;
			fm->index = fm->size = 0;
			return 0;

		default:
			Debug("direction error");
			return -1;
	}
}

int clear_output_filememory(struct filememory *fm)
{
	switch (fm->direct) {
		case filememory_io:
			if (fm->readio) return 0;
			/* FALLTHROUGH */
		case filememory_output:
			if (fm->mode == filememory_end) return 0;
			if (fm->mode != filememory_normal) return 1;
			/* clear buffer */
			fm->index = 0;
			fm->size = FILEMEMORY_SIZE;
			return 0;

		default:
			Debug("direction error");
			return -1;
	}
}

int file_length_filememory(struct filememory *fm, size_t *ret)
{
	if (fm->mode == filememory_normal || fm->mode == filememory_end)
		return file_length_arch(fm->file, ret);
	else
		return 1;
}

static int file_position_input(struct filememory *fm)
{
	return fm->direct == filememory_input ||
		(fm->direct == filememory_io && fm->readio);
}

int file_position_filememory(struct filememory *fm, size_t *ret)
{
	size_t size, unread;

	if (fm->mode != filememory_normal && fm->mode != filememory_end)
		return 1;
	file_position_arch(fm->file, &size);
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

int file_position_start_filememory(struct filememory *fm)
{
	int check;

	if (fm->mode == filememory_normal || fm->mode == filememory_end) {
		check = file_position_start_arch(fm->file);
		if (check)
			return check;
		fm->index = 0;
		fm->ungetc = 0;
		return 0;
	}

	return 1;
}

int file_position_end_filememory(struct filememory *fm)
{
	int check;

	if (fm->mode == filememory_normal || fm->mode == filememory_end) {
		check = file_position_end_arch(fm->file);
		if (check)
			return check;
		fm->index = 0;
		fm->ungetc = 0;
		return 0;
	}

	return 1;
}

int file_position_set_filememory(struct filememory *fm, size_t pos)
{
	int check;

	if (fm->mode == filememory_normal || fm->mode == filememory_end) {
		check = file_position_set_arch(fm->file, pos);
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
extern addr heap_root;
extern addr heap_front;

int readcheck_filememory(struct filememory *fm, void *dst, size_t size)
{
	size_t check;
	return readforce_filememory(fm, dst, size, &check) || size != check;
}
int writecheck_filememory(struct filememory *fm, const void *dst, size_t size)
{
	size_t check;
	return write_filememory(fm, dst, size, &check) || size != check;
}

int readptr_filememory(struct filememory *fm, void **ret)
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
int writeptr_filememory(struct filememory *fm, const void *pos)
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

int readaddr_filememory(struct filememory *fm, addr *ret)
{
	return readptr_filememory(fm, (void **)ret);
}
int writeaddr_filememory(struct filememory *fm, addr pos)
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

