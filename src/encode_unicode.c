#include "alloc.h"
#include "build.h"
#include "character.h"
#include "encode.h"
#include "execute.h"
#include "file_memory.h"
#include "thread.h"

enum read_unicode_enum {
	read_unicode_enum_normal,
	read_unicode_enum_nonblocking,
	read_unicode_enum_buffer
};

enum read_unicode_result {
	read_unicode_result_normal,
	read_unicode_result_error,
	read_unicode_result_end,
	read_unicode_result_rollback
};

#define READ_UNICODE_DATA 8
struct read_unicode_struct {
	enum read_unicode_enum type;
	byte data[READ_UNICODE_DATA];
	struct filememory *fm;
	int hang;
	unsigned rollback;
	const byte *src;
	size_t size, index;
};

static int read_unicode_rollback(struct read_unicode_struct *ptr)
{
	while (ptr->rollback) {
		ptr->rollback--;
		if (ungetc_filememory(ptr->fm, ptr->data[ptr->rollback]))
			return 1;
	}

	return 0;
}

static void read_unicode_push(struct read_unicode_struct *ptr, byte c)
{
	Check(READ_UNICODE_DATA <= ptr->rollback, "rollback error");
	ptr->data[ptr->rollback++] = c;
}


/*
 *  read-utf8
 */
#define read_utf8_macro() { \
	result = getcall(ptr, &c); \
	if (result == read_unicode_result_normal) { \
		read_unicode_push(ptr, c); \
	} \
	else if (result == read_unicode_result_end) { \
		goto file_error; \
	} \
	else if (result == read_unicode_result_rollback) { \
		goto rollback; \
	} \
	else { \
		goto unicode_error; \
	} \
}
static int read_utf8_call(struct read_unicode_struct *ptr,
		unicode *ret,
		enum read_unicode_result (*getcall)(struct read_unicode_struct *, byte *))
{
	byte c;
	enum read_unicode_result result;
	unicode value;

	result = getcall(ptr, &c);
	if (result == read_unicode_result_normal)
		read_unicode_push(ptr, c);
	else if (result == read_unicode_result_end)
		goto end_of_file;
	else if (result == read_unicode_result_rollback)
		goto rollback;
	else
		goto file_error;

	if (0x00 <= c && c <= 0x7F) goto sequence1;
	if (0xC2 <= c && c <= 0xDF) goto sequence2;
	if (0xE0 <= c && c <= 0xEF) goto sequence3;
	if (0xF0 <= c && c <= 0xF7) goto sequence4;
	if (0xF8 <= c && c <= 0xFB) goto sequence5;
	if (0xFC <= c && c <= 0xFD) goto sequence6;
	goto unicode_error;

sequence1:
	value = (unicode)c;
	goto normal;

sequence2:
	value = (0x1F & c) << 6;
	read_utf8_macro();
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	value |= 0x3F & c;
	if (value < 0x80) goto range_error;
	goto normal;

sequence3:
	value = (0x0F & c) << 12;
	read_utf8_macro();
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	value |= (0x3F & c) << 6;
	read_utf8_macro();
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	value |= 0x3F & c;
	if (value < 0x0800) goto range_error;
	if (UTF16range(value)) goto surrogate_error;
	goto normal;

sequence4:
	value = (0x07 & c) << 18;
	read_utf8_macro();
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	value |= (0x3F & c) << 12;
	read_utf8_macro();
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	value |= (0x3F & c) << 6;
	read_utf8_macro();
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	value |= 0x3F & c;
	if (value < 0x010000) goto range_error;
#ifdef LISP_UTF8_SEQ5CHECK
	if (UnicodeCount <= value) goto range_error;
#endif
	goto normal;

sequence5:
#ifdef LISP_UTF8_SEQ5CHECK
	goto sequence_error;
#else
	value = (0x03 & c) << 24;
	read_utf8_macro();
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	value |= (0x3F & c) << 18;
	read_utf8_macro();
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	value |= (0x3F & c) << 12;
	read_utf8_macro();
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	value |= (0x3F & c) << 6;
	read_utf8_macro();
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	value |= 0x3F & c;
	if (value < 0x00200000 || 0x03FFFFFF < value) goto range_error;
	goto normal;
#endif

sequence6:
#ifdef LISP_UTF8_SEQ5CHECK
	goto sequence_error;
#else
	value = (0x01 & c) << 30;
	read_utf8_macro();
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	value |= (0x3F & c) << 24;
	read_utf8_macro();
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	value |= (0x3F & c) << 18;
	read_utf8_macro();
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	value |= (0x3F & c) << 12;
	read_utf8_macro();
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	value |= (0x3F & c) << 6;
	read_utf8_macro();
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	value |= 0x3F & c;
	if (value < 0x04000000 || 0x7FFFFFFF < value) goto range_error;
	goto normal;
#endif

normal:
	*ret = value;
	return 0;

file_error:
	return -1;
unicode_error:
	return -2;
range_error:
	return -3;
#ifdef LISP_UTF8_SEQ5CHECK
sequence_error:
	return -4;
#endif
surrogate_error:
	return -5;
end_of_file:
	return 1;

rollback:
	if (read_unicode_rollback(ptr))
		goto file_error;
	ptr->hang = 1;
	return 0;
}


/* read_utf8_normal */
static enum read_unicode_result getc_utf8_normal(
		struct read_unicode_struct *ptr, byte *ret)
{
	int check;

	check = getc_filememory(ptr->fm, ret);
	if (check == 0)
		return read_unicode_result_normal;
	else if (0 < check)
		return read_unicode_result_end;
	else
		return read_unicode_result_error;
}

int read_utf8_normal(struct filememory *fm, unicode *ret)
{
	struct read_unicode_struct str;

#ifdef LISP_DEBUG
	aatype(str);
#endif
	str.type = read_unicode_enum_normal;
	str.fm = fm;
	str.rollback = 0;

	return read_utf8_call(&str, ret, getc_utf8_normal);
}


/* read_utf8_nonblocking */
static enum read_unicode_result getc_utf8_nonblocking(
		struct read_unicode_struct *ptr, byte *ret)
{
	int check;
	size_t size;

	check = getc_nonblocking_filememory(ptr->fm, ret, &size);
	if (check == 0)
		return size?
			read_unicode_result_normal:
			read_unicode_result_rollback;
	else if (0 < check)
		return read_unicode_result_end;
	else
		return read_unicode_result_error;
}

int read_utf8_nonblocking(struct filememory *fm, unicode *ret, int *hang)
{
	int check;
	struct read_unicode_struct str;

#ifdef LISP_DEBUG
	aatype(str);
#endif
	str.type = read_unicode_enum_nonblocking;
	str.fm = fm;
	str.rollback = 0;
	str.hang = 0;

	check = read_utf8_call(&str, ret, getc_utf8_nonblocking);
	*hang = str.hang;

	return check;
}


/* read_utf8_buffer */
static enum read_unicode_result getc_utf8_buffer(
		struct read_unicode_struct *ptr, byte *ret)
{
	if (ptr->size == 0) {
		*ret = ptr->src[ptr->index++];
		return *ret?
			read_unicode_result_normal:
			read_unicode_result_end;
	}
	else if (ptr->index < ptr->size) {
		*ret = ptr->src[ptr->index++];
		return read_unicode_result_normal;
	}
	else {
		return read_unicode_result_end;
	}
}

int read_utf8_buffer(unicode *dst, const byte *src, size_t size, size_t *ret)
{
	int check;
	struct read_unicode_struct str;
	unicode u;
	size_t count;

#ifdef LISP_DEBUG
	aatype(str);
#endif
	str.index = 0;
	for (count = 0; ; count++) {
		str.type = read_unicode_enum_buffer;
		str.rollback = 0;
		str.src = src;
		str.size = size;

		check = read_utf8_call(&str, &u, getc_utf8_buffer);
		if (check < 0)
			return check;
		if (check)
			break;
		if (dst)
			dst[count] = u;
	}
	if (ret)
		*ret = count;

	return 0;
}


/*
 *  read-utf16
 */
static int read_utf16_call(struct read_unicode_struct *ptr,
		unicode *ret,
		int big_endian_p,
		enum read_unicode_result (*getcall)(struct read_unicode_struct *, byte *))
{
	byte a, b;
	enum read_unicode_result result;
	uint16_t c, d;

	/* 1 byte */
	result = getcall(ptr, &a);
	if (result == read_unicode_result_normal)
		read_unicode_push(ptr, a);
	else if (result == read_unicode_result_end)
		goto end_of_file;
	else if (result == read_unicode_result_rollback)
		goto rollback;
	else
		goto file_error;

	/* 2 byte */
	result = getcall(ptr, &b);
	if (result == read_unicode_result_normal)
		read_unicode_push(ptr, b);
	else if (result == read_unicode_result_rollback)
		goto rollback;
	else
		goto file_error;
	if (big_endian_p)
		c = (a << 8) | b;
	else
		c = a | (b << 8);

	/* 1 character */
	if (UTF16low(c))
		goto unicode_error;
	if (! UTF16high(c)) {
		*ret = c;
		goto normal;
	}

	/* 3 byte */
	result = getcall(ptr, &a);
	if (result == read_unicode_result_normal)
		read_unicode_push(ptr, a);
	else if (result == read_unicode_result_rollback)
		goto rollback;
	else
		goto file_error;

	/* 4 byte */
	result = getcall(ptr, &b);
	if (result == read_unicode_result_normal)
		read_unicode_push(ptr, b);
	else if (result == read_unicode_result_rollback)
		goto rollback;
	else
		goto file_error;

	/* surrogate pair */
	if (big_endian_p)
		d = (a << 8) | b;
	else
		d = a | (b << 8);
	if (! UTF16low(d))
		goto unicode_error;
	*ret = UTF16unicode(c, d);
	goto normal;

normal:
	return 0;
file_error:
	return -1;
unicode_error:
	return -2;
end_of_file:
	return 1;

rollback:
	if (read_unicode_rollback(ptr))
		goto file_error;
	ptr->hang = 1;
	return 0;
}


/* read_utf16_normal */
int read_utf16_normal(struct filememory *fm, unicode *ret, int be)
{
	struct read_unicode_struct str;

#ifdef LISP_DEBUG
	aatype(str);
#endif
	str.type = read_unicode_enum_normal;
	str.fm = fm;
	str.rollback = 0;

	return read_utf16_call(&str, ret, be, getc_utf8_normal);
}


/* read_utf16_nonblocking */
int read_utf16_nonblocking(struct filememory *fm, unicode *ret, int *hang, int be)
{
	int check;
	struct read_unicode_struct str;

#ifdef LISP_DEBUG
	aatype(str);
#endif
	str.type = read_unicode_enum_nonblocking;
	str.fm = fm;
	str.rollback = 0;
	str.hang = 0;

	check = read_utf16_call(&str, ret, be, getc_utf8_nonblocking);
	*hang = str.hang;

	return check;
}


/*
 *  read-utf32
 */
static int read_utf32_call(struct read_unicode_struct *ptr,
		unicode *ret,
		int big_endian_p,
		enum read_unicode_result (*getcall)(struct read_unicode_struct *, byte *))
{
	byte a, b, c, d;
	enum read_unicode_result result;
	unicode value;

	/* 1 byte */
	result = getcall(ptr, &a);
	if (result == read_unicode_result_normal)
		read_unicode_push(ptr, a);
	else if (result == read_unicode_result_end)
		goto end_of_file;
	else if (result == read_unicode_result_rollback)
		goto rollback;
	else
		goto file_error;

	/* 2 byte */
	result = getcall(ptr, &b);
	if (result == read_unicode_result_normal)
		read_unicode_push(ptr, b);
	else if (result == read_unicode_result_rollback)
		goto rollback;
	else
		goto file_error;

	/* 3 byte */
	result = getcall(ptr, &c);
	if (result == read_unicode_result_normal)
		read_unicode_push(ptr, c);
	else if (result == read_unicode_result_rollback)
		goto rollback;
	else
		goto file_error;

	/* 4 byte */
	result = getcall(ptr, &d);
	if (result == read_unicode_result_normal)
		read_unicode_push(ptr, d);
	else if (result == read_unicode_result_rollback)
		goto rollback;
	else
		goto file_error;

	/* unicode */
	if (big_endian_p)
		value = (a << 24) | (b << 16) | (c << 8) | d;
	else
		value = (d << 24) | (c << 16) | (b << 8) | a;
	if (UTF16range(value))
		goto range_error;
#ifdef LISP_UTF8_SEQ5CHECK
	if (UnicodeCount <= value)
		goto range_error;
#endif
	*ret = value;
	return 0;

file_error:
	return -1;
range_error:
	return -2;
end_of_file:
	return 1;

rollback:
	if (read_unicode_rollback(ptr))
		goto file_error;
	ptr->hang = 1;
	return 0;
}


/* read_utf32_normal */
int read_utf32_normal(struct filememory *fm, unicode *ret, int be)
{
	struct read_unicode_struct str;

#ifdef LISP_DEBUG
	aatype(str);
#endif
	str.type = read_unicode_enum_normal;
	str.fm = fm;
	str.rollback = 0;

	return read_utf32_call(&str, ret, be, getc_utf8_normal);
}


/* read_utf32_nonblocking */
int read_utf32_nonblocking(struct filememory *fm, unicode *ret, int *hang, int be)
{
	int check;
	struct read_unicode_struct str;

#ifdef LISP_DEBUG
	aatype(str);
#endif
	str.type = read_unicode_enum_nonblocking;
	str.fm = fm;
	str.rollback = 0;
	str.hang = 0;

	check = read_utf32_call(&str, ret, be, getc_utf8_nonblocking);
	*hang = str.hang;

	return check;
}


/*
 *  encode-utf8
 */
int encode_utf8(unicode u, byte *dst, size_t *ret)
{
	size_t w;

	w = 0;
	/* 1 byte */
	if (u < 0x80) {
		dst[w++] = u;
		goto normal;
	}
	/* 2 byte */
	if (u < 0x0800) {
		dst[w++] = 0xC2 | (u >> 6);
		dst[w++] = 0x80 | (0x3F & u);
		goto normal;
	}
	/* 3 byte */
	if (u < 0xD800) {
		goto sequence3;
	}
	/* surrogate pair */
	if (u < 0xE000) {
		goto error;
	}
	/* 3 byte */
	if (u < 0x010000) {
sequence3:
		dst[w++] = 0xE0 | (u >> 12);
		dst[w++] = 0x80 | (0x3F & (u >> 6));
		dst[w++] = 0x80 | (0x3F & u);
		goto normal;
	}
	/* 4 byte */
#ifdef LISP_UTF8_SEQ5CHECK
	if (u < UnicodeCount) {
		dst[w++] = 0xF0 | (u >> 18);
		dst[w++] = 0x80 | (0x3F & (u >> 12));
		dst[w++] = 0x80 | (0x3F & (u >> 6));
		dst[w++] = 0x80 | (0x3F & u);
		goto normal;
	}
#else
	if (u < 0x200000) {
		dst[w++] = 0xF0 | (u >> 18);
		dst[w++] = 0x80 | (0x3F & (u >> 12));
		dst[w++] = 0x80 | (0x3F & (u >> 6));
		dst[w++] = 0x80 | (0x3F & u);
		goto normal;
	}
	if (u < 0x04000000) {
		dst[w++] = 0xF8 | (u >> 24);
		dst[w++] = 0x80 | (0x3F & (u >> 18));
		dst[w++] = 0x80 | (0x3F & (u >> 12));
		dst[w++] = 0x80 | (0x3F & (u >> 6));
		dst[w++] = 0x80 | (0x3F & u);
		goto normal;
	}
	if (u < 0x80000000) {
		dst[w++] = 0xFC | (u >> 30);
		dst[w++] = 0x80 | (0x3F & (u >> 24));
		dst[w++] = 0x80 | (0x3F & (u >> 18));
		dst[w++] = 0x80 | (0x3F & (u >> 12));
		dst[w++] = 0x80 | (0x3F & (u >> 6));
		dst[w++] = 0x80 | (0x3F & u);
		goto normal;
	}
#endif
	/* error */

error:
	return 1;

normal:
	*ret = w;
	return 0;
}


/*
 *  encode-utf16
 */
int encode_utf16a(unicode u, byte16 *surrogate, byte16 *code)
{
	/* 1 byte, 2 byte */
	if (u < 0xD800) {
		*surrogate = 0;
		*code = (byte16)u;
		return 0;
	}
	/* surrogate pair */
	if (u < 0xE000) {
		return 1; /* error */
	}
	/* 2 byte */
	if (u < 0x010000) {
		*surrogate = 0;
		*code = (byte16)u;
		return 0;
	}
	/* 4 byte */
	if (u < UnicodeCount) {
		*code = 0xDC00 | (0x03FF & u);
		u = ((((u >> 16) & 0x1F) - 1) << 6) | (0x3F & (u >> 10));
		*surrogate = 0xD800 | u;
		return 0;
	}

	/* error */
	return 1;
}

int encode_utf16b(unicode u, byte16 *dst, size_t *ret)
{
	byte16 surrogate, code;

	if (encode_utf16a(u, &surrogate, &code))
		return 1;
	if (surrogate == 0) {
		dst[0] = code;
		*ret = 1;
	}
	else {
		dst[0] = surrogate;
		dst[1] = code;
		*ret = 2;
	}

	return 0;
}

int encode_utf16(unicode u, int big_endian_p, byte *dst, size_t *ret)
{
	byte16 high, code;
	unsigned i;

	if (encode_utf16a(u, &high, &code))
		return 1;
	i = 0;
	if (big_endian_p) {
		if (high) {
			dst[i++] = 0xFF & (high >> 8);
			dst[i++] = 0xFF & high;
		}
		dst[i++] = 0xFF & (code >> 8);
		dst[i++] = 0xFF & code;
	}
	else {
		if (high) {
			dst[i++] = 0xFF & high;
			dst[i++] = 0xFF & (high >> 8);
		}
		dst[i++] = 0xFF & code;
		dst[i++] = 0xFF & (code >> 8);
	}
	*ret = (size_t)i;

	return 0;
}


/*
 *  encode-utf32
 */
int encode_utf32check(unicode u)
{
	if (UTF16range(u))
		return 1;
#ifdef LISP_UTF8_SEQ5CHECK
	if (UnicodeCount <= u)
		return 1;
#endif
	return 0;
}

int encode_utf32(unicode u, int big_endian_p, byte *dst, size_t *ret)
{
	unsigned i;

	if (encode_utf32check(u))
		return 1;
	i = 0;
	if (big_endian_p) {
		dst[i++] = 0xFF & (u >> 24);
		dst[i++] = 0xFF & (u >> 16);
		dst[i++] = 0xFF & (u >> 8);
		dst[i++] = 0xFF & u;
	}
	else {
		dst[i++] = 0xFF & u;
		dst[i++] = 0xFF & (u >> 8);
		dst[i++] = 0xFF & (u >> 16);
		dst[i++] = 0xFF & (u >> 24);
	}
	*ret = (size_t)i;

	return 0;
}

