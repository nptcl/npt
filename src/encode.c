#include "character.h"
#include "condition.h"
#include "file_memory.h"
#include "object.h"
#include "stream.h"
#include "strtype.h"
#include "unicode.h"

#define UTF8_SEQ5CHECK
#define UTF16range(x) (0xD800 <= (x) && (x) < 0xE000)
#define UTF16high(x) (0xD800 <= (x) && (x) < 0xDC00)
#define UTF16low(x) (0xDC00 <= (x) && (x) < 0xE000)
#define UTF16unicode(a, b) \
	((((((a)>>6UL)&0x0FUL)+1UL)<<16UL) | (((a)&0x3FUL)<<10UL) | ((b)&0x03FFUL))

typedef int (*read_char_calltype)(struct filememory *, unicode *);
typedef int (*read_hang_calltype)(struct filememory *, unicode *, int *);
typedef int (*write_char_calltype)(struct filememory *, unicode);
typedef int (*length_char_calltype)(struct filememory *, unicode);
static read_char_calltype read_char_call[EncodeType_size];
static read_hang_calltype read_hang_call[EncodeType_size];
static write_char_calltype write_char_call[EncodeType_size];
static length_char_calltype length_char_call[EncodeType_size];

/*
 *  Byte Order Mark
 */
int readbom8_encode(struct filememory *fm)
{
	byte c;
	int check;

	check = getc_filememory(fm, &c);
	if (check < 0)
		goto invalid;
	if (check)
		goto empty;
	if (c != 0xEF) {
		if (ungetc_filememory(fm, c))
			goto invalid; /* ungetc error */
		else
			goto empty; /* ok */
	}
	if (getc_filememory(fm, &c))
		goto invalid; /* invalid bom sequence */
	if (c != 0xBB)
		goto invalid;
	if (getc_filememory(fm, &c))
		goto invalid;
	if (c != 0xBF)
		goto invalid;
	goto exist;

invalid:
	return -1;
empty:
	return 0;
exist:
	return 1;
}

int readbom16_encode(struct filememory *fm)
{
	byte a, b;
	int check;

	/* 1:le, 2:be */
	check = getc_filememory(fm, &a);
	if (check < 0)
		goto invalid;
	if (check)
		goto empty;
	if (getc_filememory(fm, &b))
		goto invalid;

	if (a == 0xFF && b == 0xFE)
		goto little_endian;
	if (a == 0xFE && b == 0xFF)
		goto big_endian;

	if (ungetc_filememory(fm, b))
		goto invalid;
	if (ungetc_filememory(fm, a))
		goto invalid;
	goto empty;

invalid:
	return -1;
empty:
	return 0;
little_endian:
	return 1;
big_endian:
	return 2;
}

int readbom32_encode(struct filememory *fm)
{
	byte a, b, c, d;
	int check;

	/* 1:le, 2:be */
	check = getc_filememory(fm, &a);
	if (check < 0)
		goto invalid;
	if (check)
		goto empty;
	if (getc_filememory(fm, &b))
		goto invalid;
	if (getc_filememory(fm, &c))
		goto invalid;
	if (getc_filememory(fm, &d))
		goto invalid;

	if (a == 0xFF && b == 0xFE && c == 0x00 && d == 0x00)
		goto little_endian;
	if (a == 0x00 && b == 0x00 && c == 0xFE && d == 0xFF)
		goto big_endian;

	if (ungetc_filememory(fm, d))
		goto invalid;
	if (ungetc_filememory(fm, c))
		goto invalid;
	if (ungetc_filememory(fm, b))
		goto invalid;
	if (ungetc_filememory(fm, a))
		goto invalid;
	goto empty;

invalid:
	return -1;
empty:
	return 0;
little_endian:
	return 1;
big_endian:
	return 2;
}

int writebom8_encode(struct filememory *fm)
{
	if (putc_filememory(fm, 0xEF)) return 1;
	if (putc_filememory(fm, 0xBB)) return 1;
	if (putc_filememory(fm, 0xBF)) return 1;
	return 0;
}

int writebom16_encode(struct filememory *fm, int big_endian_p)
{
	if (big_endian_p) {
		if (putc_filememory(fm, 0xFE)) return 1;
		if (putc_filememory(fm, 0xFF)) return 1;
	}
	else {
		if (putc_filememory(fm, 0xFF)) return 1;
		if (putc_filememory(fm, 0xFE)) return 1;
	}
	return 0;
}

int writebom32_encode(struct filememory *fm, int big_endian_p)
{
	if (big_endian_p) {
		if (putc_filememory(fm, 0x00)) return 1;
		if (putc_filememory(fm, 0x00)) return 1;
		if (putc_filememory(fm, 0xFE)) return 1;
		if (putc_filememory(fm, 0xFF)) return 1;
	}
	else {
		if (putc_filememory(fm, 0xFF)) return 1;
		if (putc_filememory(fm, 0xFE)) return 1;
		if (putc_filememory(fm, 0x00)) return 1;
		if (putc_filememory(fm, 0x00)) return 1;
	}
	return 0;
}


/*
 *  read_char
 */
static int read_char_binary(struct filememory *fm, unicode *u)
{
	fmte("Cannot execute read-char in binary stream.", NULL);
	return -1;
}

static int read_char_ascii(struct filememory *fm, unicode *u)
{
	byte c;
	int check;

	check = getc_filememory(fm, &c);
	if (check < 0)
		fmte("getc error", NULL);
	if (check)
		return -1;
	if (0x80 <= c) {
		if (fm->encode.error)
			return -1;
		*u = fm->encode.code;
		return 0;
	}
	*u = (unicode)c;

	return 0;
}

#define readbyteunicode(check, fm, c) { \
	(check) = getc_filememory((fm), &(c)); \
	if (check < 0) goto file_error; \
	if (check) goto unicode_error; \
}
static int getutf8(struct filememory *fm, unicode *ret)
{
	byte c;
	int check;
	unicode result;

	check = getc_filememory(fm, &c);
	if (check < 0) goto file_error;
	if (check) goto end_of_file;
	if (0x00 <= c && c <= 0x7F) goto sequence1;
	if (0xC2 <= c && c <= 0xDF) goto sequence2;
	if (0xE0 <= c && c <= 0xEF) goto sequence3;
	if (0xF0 <= c && c <= 0xF7) goto sequence4;
	if (0xF8 <= c && c <= 0xFB) goto sequence5;
	if (0xFC <= c && c <= 0xFD) goto sequence6;
	goto unicode_error;

sequence1:
	result = (unicode)c;
	goto normal;

sequence2:
	result = (0x1F & c) << 6;
	readbyteunicode(check, fm, c);
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= 0x3F & c;
	goto normal;

sequence3:
	result = (0x0F & c) << 12;
	readbyteunicode(check, fm, c);
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= (0x3F & c) << 6;
	readbyteunicode(check, fm, c);
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= 0x3F & c;
	if (result < 0x0800) goto range_error;
	if (UTF16range(result)) goto surrogate_error;
	goto normal;

sequence4:
	result = (0x07 & c) << 18;
	readbyteunicode(check, fm, c);
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= (0x3F & c) << 12;
	readbyteunicode(check, fm, c);
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= (0x3F & c) << 6;
	readbyteunicode(check, fm, c);
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= 0x3F & c;
	if (result < 0x010000) goto range_error;
#ifdef UTF8_SEQ5CHECK
	if (UnicodeCount <= result) goto range_error;
#endif
	goto normal;

sequence5:
#ifdef UTF8_SEQ5CHECK
	goto sequence_error;
#else
	result = (0x03 & c) << 24;
	readbyteunicode(check, fm, c);
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= (0x3F & c) << 18;
	readbyteunicode(check, fm, c);
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= (0x3F & c) << 12;
	readbyteunicode(check, fm, c);
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= (0x3F & c) << 6;
	readbyteunicode(check, fm, c);
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= 0x3F & c;
	if (result < 0x00200000 || 0x03FFFFFF < result) goto range_error;
	goto normal;
#endif

sequence6:
#ifdef UTF8_SEQ5CHECK
	goto sequence_error;
#else
	result = (0x01 & c) << 30;
	readbyteunicode(check, fm, c);
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= (0x3F & c) << 24;
	readbyteunicode(check, fm, c);
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= (0x3F & c) << 18;
	readbyteunicode(check, fm, c);
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= (0x3F & c) << 12;
	readbyteunicode(check, fm, c);
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= (0x3F & c) << 6;
	readbyteunicode(check, fm, c);
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= 0x3F & c;
	if (result < 0x04000000 || 0x7FFFFFFF < result) goto range_error;
	goto normal;
#endif

normal:
	*ret = result;
	return 0;

file_error:
	return -1;
unicode_error:
	return -2;
range_error:
	return -3;
sequence_error:
	return -4;
surrogate_error:
	return -5;
end_of_file:
	return 1;
}

static int read_char_utf8(struct filememory *fm, unicode *u)
{
	int check;

	check = getutf8(fm, u);
	/* normal */
	if (check == 0)
		return 0;
	/* EOF */
	if (0 < check)
		return 1;
	/* file error */
	if (check == -1)
		return -1;
	/* encode error */
	if (fm->encode.error)
		return -1;
	/* recovery */
	*u = fm->encode.code;

	return 0;
}

static int getutf16(struct filememory *fm, unicode *ret, int big_endian_p)
{
	int check;
	byte a, b;
	uint16_t c, d;

	/* read character */
	check = getc_filememory(fm, &a);
	if (check < 0)
		goto file_error;
	if (check)
		goto end_of_file;
	if (getc_filememory(fm, &b))
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

	/* surrogate pair */
	if (getc_filememory(fm, &a))
		goto file_error;
	if (getc_filememory(fm, &b))
		goto file_error;
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
}

static int read_char_utf16(struct filememory *fm, unicode *u, int big_endian_p)
{
	int check;

	check = getutf16(fm, u, big_endian_p);
	/* normal */
	if (check == 0)
		return 0;
	/* EOF */
	if (0 < check)
		return 1;
	/* file error */
	if (check == -1)
		return -1;
	/* encode error */
	if (fm->encode.error)
		return -1;
	/* recovery */
	*u = fm->encode.code;

	return 0;
}

static int read_char_utf16le(struct filememory *fm, unicode *u)
{
	return read_char_utf16(fm, u, 0);
}

static int read_char_utf16be(struct filememory *fm, unicode *u)
{
	return read_char_utf16(fm, u, 1);
}

static int getutf32(struct filememory *fm, unicode *ret, int big_endian_p)
{
	int check;
	byte a, b, c, d;
	unicode result;

	check = getc_filememory(fm, &a);
	if (check < 0)
		goto file_error;
	if (check)
		goto end_of_file;
	if (getc_filememory(fm, &b))
		goto file_error;
	if (getc_filememory(fm, &c))
		goto file_error;
	if (getc_filememory(fm, &d))
		goto file_error;
	if (big_endian_p)
		result = (a << 24) | (b << 16) | (c << 8) | d;
	else
		result = (d << 24) | (c << 16) | (b << 8) | a;
	if (UTF16range(result))
		goto range_error;
#ifdef UTF8_SEQ5CHECK
	if (UnicodeCount <= result)
		goto range_error;
#endif
	*ret = result;
	return 0;

file_error:
	return -1;
range_error:
	return -2;
end_of_file:
	return 1;
}

static int read_char_utf32(struct filememory *fm, unicode *u, int big_endian_p)
{
	int check;

	check = getutf32(fm, u, big_endian_p);
	/* normal */
	if (check == 0)
		return 0;
	/* EOF */
	if (0 < check)
		return 1;
	/* file error */
	if (check == -1)
		return -1;
	/* encode error */
	if (fm->encode.error)
		return -1;
	/* recovery */
	*u = fm->encode.code;

	return 0;
}

static int read_char_utf32le(struct filememory *fm, unicode *u)
{
	return read_char_utf32(fm, u, 0);
}

static int read_char_utf32be(struct filememory *fm, unicode *u)
{
	return read_char_utf32(fm, u, 1);
}

static int read_char_windows(struct filememory *fm, unicode *u)
{
	fmte("Invalid external-format :windows.", NULL);
	return -1;
}

static void init_encode_read_char(void)
{
	read_char_call[EncodeType_binary] = read_char_binary;
	read_char_call[EncodeType_ascii] = read_char_ascii;
	read_char_call[EncodeType_utf8] = read_char_utf8;
	read_char_call[EncodeType_utf16le] = read_char_utf16le;
	read_char_call[EncodeType_utf16be] = read_char_utf16be;
	read_char_call[EncodeType_utf32le] = read_char_utf32le;
	read_char_call[EncodeType_utf32be] = read_char_utf32be;
	read_char_call[EncodeType_windows] = read_char_windows;
}

int read_char_encode(struct filememory *fm, unicode *c)
{
	return (read_char_call[(int)fm->encode.type])(fm, c);
}


/*
 *  read_hang
 */
static int read_hang_binary(struct filememory *fm, unicode *u, int *hang)
{
	fmte("Cannot execute read-char-no-hang in binary stream.", NULL);
	return -1;
}

static int read_hang_ascii(struct filememory *fm, unicode *u, int *hang)
{
	byte c;
	int check;
	size_t size;

	check = getc_nonblocking_filememory(fm, &c, &size);
	if (check < 0)
		fmte("getc_nonblocking error", NULL);
	if (check)
		return -1;
	if (size == 0) {
		*hang = 1;
		return 0;
	}
	if (0x80 <= c) {
		if (fm->encode.error)
			return -1;
		*hang = 0;
		*u = fm->encode.code;
		return 0;
	}
	*hang = 0;
	*u = (unicode)c;

	return 0;
}

#define getutf8_rollback(fm, data, index) { \
	while (index) { \
		index--; \
		if (ungetc_filememory(fm, data[index])) \
		goto file_error; \
	} \
}
#define readbyteunicode_nonblocking() { \
	check = getc_nonblocking_filememory(fm, &c, &size); \
	if (check < 0) goto file_error; \
	if (check) goto unicode_error; \
	if (size == 0) goto rollback; \
	data[index++] = c; \
}
static int getutf8_nonblocking(struct filememory *fm, unicode *ret, int *hang)
{
	byte data[8], c;
	int check;
	unicode result;
	size_t size, index;

	index = 0;
	check = getc_nonblocking_filememory(fm, &c, &size);
	if (check < 0) goto file_error;
	if (check) goto end_of_file;
	if (size == 0) goto rollback;
	data[index++] = c;
	if (0x00 <= c && c <= 0x7F) goto sequence1;
	if (0xC2 <= c && c <= 0xDF) goto sequence2;
	if (0xE0 <= c && c <= 0xEF) goto sequence3;
	if (0xF0 <= c && c <= 0xF7) goto sequence4;
	if (0xF8 <= c && c <= 0xFB) goto sequence5;
	if (0xFC <= c && c <= 0xFD) goto sequence6;
	goto unicode_error;

sequence1:
	result = (unicode)c;
	goto normal;

sequence2:
	result = (0x1F & c) << 6;
	readbyteunicode_nonblocking();
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= 0x3F & c;
	Check(result < 0x0080 || 0x07FF < result, "range error");
	goto normal;

sequence3:
	result = (0x0F & c) << 12;
	readbyteunicode_nonblocking();
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= (0x3F & c) << 6;
	readbyteunicode_nonblocking();
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= 0x3F & c;
	if (result < 0x0800) goto range_error;
	if (UTF16range(result)) goto surrogate_error;
	goto normal;

sequence4:
	result = (0x07 & c) << 18;
	readbyteunicode_nonblocking();
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= (0x3F & c) << 12;
	readbyteunicode_nonblocking();
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= (0x3F & c) << 6;
	readbyteunicode_nonblocking();
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= 0x3F & c;
	if (result < 0x010000) goto range_error;
#ifdef UTF8_SEQ5CHECK
	if (UnicodeCount <= result) goto range_error;
#endif
	goto normal;

sequence5:
#ifdef UTF8_SEQ5CHECK
	goto sequence_error;
#else
	result = (0x03 & c) << 24;
	readbyteunicode_nonblocking();
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= (0x3F & c) << 18;
	readbyteunicode_nonblocking();
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= (0x3F & c) << 12;
	readbyteunicode_nonblocking();
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= (0x3F & c) << 6;
	readbyteunicode_nonblocking();
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= 0x3F & c;
	if (result < 0x00200000 || 0x03FFFFFF < result) goto range_error;
	goto normal;
#endif

sequence6:
#ifdef UTF8_SEQ5CHECK
	goto sequence_error;
#else
	result = (0x01 & c) << 30;
	readbyteunicode_nonblocking();
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= (0x3F & c) << 24;
	readbyteunicode_nonblocking();
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= (0x3F & c) << 18;
	readbyteunicode_nonblocking();
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= (0x3F & c) << 12;
	readbyteunicode_nonblocking();
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= (0x3F & c) << 6;
	readbyteunicode_nonblocking();
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= 0x3F & c;
	if (result < 0x04000000 || 0x7FFFFFFF < result) goto range_error;
	goto normal;
#endif

normal:
	*ret = result;
	*hang = 0;
	return 0;

file_error:
	return -1;
unicode_error:
	return -2;
range_error:
	return -3;
sequence_error:
	return -4;
surrogate_error:
	return -5;
end_of_file:
	return 1;
rollback:
	getutf8_rollback(fm, data, index);
	*hang = 1;
	return 0;
}

static int read_hang_utf8(struct filememory *fm, unicode *u, int *hang)
{
	int check;

	check = getutf8_nonblocking(fm, u, hang);
	/* normal */
	if (check == 0)
		return 0;
	/* EOF */
	if (0 < check)
		return 1;
	/* file error */
	if (check == -1)
		return -1;
	/* encode error */
	if (fm->encode.error)
		return -1;
	/* recovery */
	*hang = 0;
	*u = fm->encode.code;

	return 0;
}

static int getutf16_nonblocking(struct filememory *fm,
		unicode *ret, int *hang, int big_endian_p)
{
	int check;
	byte data[8], a, b;
	uint16_t c, d;
	size_t index, size;

	/* 1 byte */
	index = 0;
	check = getc_nonblocking_filememory(fm, &a, &size);
	if (check < 0)
		goto file_error;
	if (check)
		goto end_of_file;
	if (size == 0)
		goto rollback;
	data[index++] = a;

	/* 2 byte */
	if (getc_nonblocking_filememory(fm, &b, &size))
		goto file_error;
	if (size == 0)
		goto rollback;
	data[index++] = b;
	if (big_endian_p)
		c = (a << 8) | b;
	else
		c = a | (b << 8);

	/* 1 character */
	if (UTF16low(c))
		goto unicode_error;
	if (! UTF16high(c)) {
		*hang = 0;
		*ret = c;
		goto normal;
	}

	/* 3 byte */
	if (getc_nonblocking_filememory(fm, &a, &size))
		goto file_error;
	if (size == 0)
		goto rollback;
	data[index++] = a;

	/* 4 byte */
	if (getc_nonblocking_filememory(fm, &b, &size))
		goto file_error;
	if (size == 0)
		goto rollback;

	/* surrogate pair */
	if (big_endian_p)
		d = (a << 8) | b;
	else
		d = a | (b << 8);
	if (! UTF16low(d))
		goto unicode_error;
	*hang = 0;
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
	getutf8_rollback(fm, data, index);
	*hang = 1;
	return 0;
}

static int read_hang_utf16(struct filememory *fm,
		unicode *u, int *hang, int big_endian_p)
{
	int check;

	check = getutf16_nonblocking(fm, u, hang, big_endian_p);
	/* normal */
	if (check == 0)
		return 0;
	/* EOF */
	if (0 < check)
		return 1;
	/* file error */
	if (check == -1)
		return -1;
	/* encode error */
	if (fm->encode.error)
		return -1;
	/* recovery */
	*hang = 0;
	*u = fm->encode.code;

	return 0;
}

static int read_hang_utf16le(struct filememory *fm, unicode *u, int *hang)
{
	return read_hang_utf16(fm, u, hang, 0);
}

static int read_hang_utf16be(struct filememory *fm, unicode *u, int *hang)
{
	return read_hang_utf16(fm, u, hang, 1);
}

static int getutf32_nonblocking(struct filememory *fm,
		unicode *ret, int *hang, int big_endian_p)
{
	int check;
	byte data[8], a, b, c, d;
	unicode result;
	size_t index, size;

	/* 1 byte */
	index = 0;
	check = getc_nonblocking_filememory(fm, &a, &size);
	if (check < 0)
		goto file_error;
	if (check)
		goto end_of_file;
	if (size == 0)
		goto rollback;
	data[index++] = a;

	/* 2 byte */
	if (getc_nonblocking_filememory(fm, &b, &size))
		goto file_error;
	if (size == 0)
		goto rollback;
	data[index++] = b;

	/* 3 byte */
	if (getc_nonblocking_filememory(fm, &c, &size))
		goto file_error;
	if (size == 0)
		goto rollback;
	data[index++] = c;

	/* 4 byte */
	if (getc_nonblocking_filememory(fm, &d, &size))
		goto file_error;
	if (size == 0)
		goto rollback;

	/* unicode */
	if (big_endian_p)
		result = (a << 24) | (b << 16) | (c << 8) | d;
	else
		result = (d << 24) | (c << 16) | (b << 8) | a;
	if (UTF16range(result))
		goto range_error;
#ifdef UTF8_SEQ5CHECK
	if (UnicodeCount <= result)
		goto range_error;
#endif
	*hang = 0;
	*ret = result;
	return 0;

file_error:
	return -1;
range_error:
	return -2;
end_of_file:
	return 1;
rollback:
	getutf8_rollback(fm, data, index);
	*hang = 1;
	return 0;
}

static int read_hang_utf32(struct filememory *fm,
		unicode *u, int *hang, int big_endian_p)
{
	int check;

	check = getutf32_nonblocking(fm, u, hang, big_endian_p);
	/* normal */
	if (check == 0)
		return 0;
	/* EOF */
	if (0 < check)
		return 1;
	/* file error */
	if (check == -1)
		return -1;
	/* encode error */
	if (fm->encode.error)
		return -1;
	/* recovery */
	*hang = 0;
	*u = fm->encode.code;

	return 0;
}

static int read_hang_utf32le(struct filememory *fm, unicode *u, int *hang)
{
	return read_hang_utf32(fm, u, hang, 0);
}

static int read_hang_utf32be(struct filememory *fm, unicode *u, int *hang)
{
	return read_hang_utf32(fm, u, hang, 1);
}

static int read_hang_windows(struct filememory *fm, unicode *u, int *hang)
{
	fmte("Invalid external-format :windows.", NULL);
	return -1;
}

static void init_encode_read_hang(void)
{
	read_hang_call[EncodeType_binary] = read_hang_binary;
	read_hang_call[EncodeType_ascii] = read_hang_ascii;
	read_hang_call[EncodeType_utf8] = read_hang_utf8;
	read_hang_call[EncodeType_utf16le] = read_hang_utf16le;
	read_hang_call[EncodeType_utf16be] = read_hang_utf16be;
	read_hang_call[EncodeType_utf32le] = read_hang_utf32le;
	read_hang_call[EncodeType_utf32be] = read_hang_utf32be;
	read_hang_call[EncodeType_windows] = read_hang_windows;
}

int read_hang_encode(struct filememory *fm, unicode *c, int *hang)
{
	return (read_hang_call[(int)fm->encode.type])(fm, c, hang);
}


/*
 *  write_char
 */
static int write_char_binary(struct filememory *fm, unicode u)
{
	fmte("Cannot execute write-char in binary stream.", NULL);
	return -1;
}

static int write_char_ascii(struct filememory *fm, unicode u)
{
	if (u < 0x80)
		return putc_filememory(fm, (byte)u);
	if (fm->encode.error)
		return -1;
	u = fm->encode.code;
	if (u < 0x80)
		return putc_filememory(fm, (byte)u);

	return -1;
}

static int UTF8_encode(unicode u, byte *dst, size_t *ret)
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
#ifdef UTF8_SEQ5CHECK
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

static int write_char_utf8(struct filememory *fm, unicode u)
{
	byte data[8];
	size_t size;

	if (! UTF8_encode(u, data, &size))
		return write_filememory(fm, data, size, &size);
	if (fm->encode.error)
		return -1;
	if (! UTF8_encode(fm->encode.code, data, &size))
		return write_filememory(fm, data, size, &size);

	return -1;
}

static int UTF16_encode(unicode u, byte16 *surrogate, byte16 *code)
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

static int UTF16_encode_ptr(unicode u, byte16 *dst, size_t *ret)
{
	byte16 surrogate, code;

	if (UTF16_encode(u, &surrogate, &code))
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

static int write_char_utf16(struct filememory *fm, unicode u, int big_endian_p)
{
	byte a, b;
	byte16 high, code;

	if (! UTF16_encode(u, &high, &code))
		goto normal;
	if (fm->encode.error)
		return -1;
	if (! UTF16_encode(fm->encode.code, &high, &code))
		goto normal;
	return -1;

normal:
	if (big_endian_p) {
		if (high) {
			a = 0xFF & (high >> 8);
			b = 0xFF & high;
			if (putc_filememory(fm, a)) return 1;
			if (putc_filememory(fm, b)) return 1;
		}
		a = 0xFF & (code >> 8);
		b = 0xFF & code;
		if (putc_filememory(fm, a)) return 1;
		if (putc_filememory(fm, b)) return 1;
	}
	else {
		if (high) {
			a = 0xFF & high;
			b = 0xFF & (high >> 8);
			if (putc_filememory(fm, a)) return 1;
			if (putc_filememory(fm, b)) return 1;
		}
		a = 0xFF & code;
		b = 0xFF & (code >> 8);
		if (putc_filememory(fm, a)) return 1;
		if (putc_filememory(fm, b)) return 1;
	}
	return 0;
}

static int write_char_utf16le(struct filememory *fm, unicode u)
{
	return write_char_utf16(fm, u, 0);
}

static int write_char_utf16be(struct filememory *fm, unicode u)
{
	return write_char_utf16(fm, u, 1);
}

static int UTF32_encode(unicode u)
{
	if (UTF16range(u))
		return 1;
#ifdef UTF8_SEQ5CHECK
	if (UnicodeCount <= u)
		return 1;
#endif
	return 0;
}

static int write_char_utf32(struct filememory *fm, unicode u, int big_endian_p)
{
	byte a, b, c, d;

	if (! UTF32_encode(u))
		goto normal;
	if (fm->encode.error)
		return -1;
	u = fm->encode.code;
	if (! UTF32_encode(u))
		goto normal;
	return -1;

normal:
	if (big_endian_p) {
		a = 0xFF & (u >> 24);
		b = 0xFF & (u >> 16);
		c = 0xFF & (u >> 8);
		d = 0xFF & u;
	}
	else {
		a = 0xFF & u;
		b = 0xFF & (u >> 8);
		c = 0xFF & (u >> 16);
		d = 0xFF & (u >> 24);
	}
	if (putc_filememory(fm, a)) return 1;
	if (putc_filememory(fm, b)) return 1;
	if (putc_filememory(fm, c)) return 1;
	return putc_filememory(fm, d);
}

static int write_char_utf32le(struct filememory *fm, unicode u)
{
	return write_char_utf32(fm, u, 0);
}

static int write_char_utf32be(struct filememory *fm, unicode u)
{
	return write_char_utf32(fm, u, 1);
}

#if defined(LISP_ANSI_WINDOWS)
static int write_char_windows(struct filememory *fm, unicode u)
{
	char output[8];
	wchar_t input[4];
	byte *ptr, c;
	size_t size;

	/* unicode -> UTF16 */
	if (! UTF16_encode_ptr(u, (byte16 *)input, &size))
		goto normal;
	if (fm->encode.error)
		return -1;
	if (! UTF16_encode_ptr(fm->encode.code, (byte16 *)input, &size))
		goto normal;
	return -1;

	/* UTF16 -> ANSI-code-page  */
normal:
	input[size] = 0;
	snprintf(output, 8, "%S", input);
	if (output[0] == 0)
		return -1;
	for (ptr = (byte *)output; ; ptr++) {
		c = *ptr;
		if (c == 0)
			break;
		if (putc_filememory(fm, c))
			return -1;
	}

	return 0;
}

#elif (defined LISP_WINDOWS)
static int write_char_windows(struct filememory *fm, unicode u)
{
	char output[8];
	wchar_t input[4];
	byte *ptr;
	int result, i;
	size_t size;

	/* unicode -> UTF16 */
	if (! UTF16_encode_ptr(u, (byte16 *)input, &size))
		goto normal;
	if (fm->encode.error)
		return -1;
	if (! UTF16_encode_ptr(fm->encode.code, (byte16 *)input, &size))
		goto normal;
	return -1;

	/* UTF16 -> ANSI-code-page  */
normal:
	input[size] = 0;
	result = WideCharToMultiByte(CP_THREAD_ACP,
			0,
			(LPCWSTR)input,
			(int)size,
			(LPSTR)output,
			8,
			NULL,
			NULL);
	if (result == 0)
		return -1;
	ptr = (byte *)output;
	for (i = 0; i < result; i++) {
		if (putc_filememory(fm, ptr[i]))
			return -1;
	}

	return 0;
}

#else
static int write_char_windows(struct filememory *fm, unicode u)
{
	fmte("This implementation cannot write a windows encode.", NULL);
	return -1;
}
#endif

static void init_encode_write_char(void)
{
	write_char_call[EncodeType_binary] = write_char_binary;
	write_char_call[EncodeType_ascii] = write_char_ascii;
	write_char_call[EncodeType_utf8] = write_char_utf8;
	write_char_call[EncodeType_utf16le] = write_char_utf16le;
	write_char_call[EncodeType_utf16be] = write_char_utf16be;
	write_char_call[EncodeType_utf32le] = write_char_utf32le;
	write_char_call[EncodeType_utf32be] = write_char_utf32be;
	write_char_call[EncodeType_windows] = write_char_windows;
}

int write_char_encode(struct filememory *fm, unicode u)
{
	return (write_char_call[(int)fm->encode.type])(fm, u);
}


/*
 *  length-char
 */
static int length_char_binary(struct filememory *fm, unicode c)
{
	return -1;
}

static int length_char_ascii(struct filememory *fm, unicode c)
{
	return (c < 0x80)? 1: -1;
}

static int length_char_utf8(struct filememory *fm, unicode c)
{
	if (c < 0x80) return 1;
	if (c < 0x0800) return 2;
	if (c < 0x010000) return 3;
	if (c < 0x200000) return 4;
	if (c < 0x04000000) return 5;
	if (c < 0x80000000) return 6;
	return -1;
}

static int length_char_utf16(struct filememory *fm, unicode c)
{
	return (c < 0x010000)? 2: 4;
}

static int length_char_utf32(struct filememory *fm, unicode c)
{
	return 4;
}

#if defined(LISP_ANSI_WINDOWS)
static int length_char_windows(struct filememory *fm, unicode c)
{
	char output[8];
	wchar_t input[4];
	size_t size;

	/* unicode -> UTF16 */
	if (! UTF16_encode_ptr(u, (byte16 *)input, &size))
		goto normal;
	if (fm->encode.error)
		return -1;
	if (! UTF16_encode_ptr(fm->encode.code, (byte16 *)input, &size))
		goto normal;
	return -1;

	/* UTF16 -> ANSI-code-page  */
normal:
	input[size] = 0;
	snprintf(output, 8, "%S", input);
	if (output[0] == 0)
		return -1;
	return (int)strlen(output);
}
#elif defined(LISP_WINDOWS)
static int length_char_windows(struct filememory *fm, unicode c)
{
	char output[8];
	wchar_t input[4];
	int result;
	size_t size;

	/* unicode -> UTF16 */
	if (! UTF16_encode_ptr(c, (byte16 *)input, &size))
		goto normal;
	if (fm->encode.error)
		return -1;
	if (! UTF16_encode_ptr(fm->encode.code, (byte16 *)input, &size))
		goto normal;
	return -1;

	/* UTF16 -> ANSI-code-page  */
normal:
	input[size] = 0;
	result = WideCharToMultiByte(CP_THREAD_ACP,
			0,
			(LPCWSTR)input,
			(int)size,
			(LPSTR)output,
			8,
			NULL,
			NULL);
	if (result == 0)
		return -1;
	return (int)strlen(output);
}
#else
static int length_char_windows(struct filememory *fm, unicode c)
{
	fmte("This implementation cannot use a windows encode.", NULL);
	return -1;
}
#endif

static void init_encode_length_char(void)
{
	length_char_call[EncodeType_binary] = length_char_binary;
	length_char_call[EncodeType_ascii] = length_char_ascii;
	length_char_call[EncodeType_utf8] = length_char_utf8;
	length_char_call[EncodeType_utf16le] = length_char_utf16;
	length_char_call[EncodeType_utf16be] = length_char_utf16;
	length_char_call[EncodeType_utf32le] = length_char_utf32;
	length_char_call[EncodeType_utf32be] = length_char_utf32;
	length_char_call[EncodeType_windows] = length_char_windows;
}

static int length_char_operator(struct filememory *fm, unicode c)
{
	/* Surrogate pair */
	if (UTF16range(c))
		return -1;
	/* Invalid unicode */
#ifdef UTF8_SEQ5CHECK
	if (UnicodeCount <= c)
		return -1;
#endif
	/* Length */
	return (length_char_call[(int)fm->encode.type])(fm, c);
}

int length_char_encode(struct filememory *fm, unicode c)
{
	int check;

	check = length_char_operator(fm, c);
	if (check < 0 && fm->encode.error == 0)
		return length_char_operator(fm, fm->encode.code);
	else
		return check;
}

int length_string_encode(struct filememory *fm, addr pos, size_t *ret)
{
	int check;
	unicode c;
	size_t i, size, count;

	string_length(pos, &size);
	for (count = i = 0; i < size; i++) {
		string_getc(pos, i, &c);
		check = length_char_encode(fm, c);
		if (check < 0)
			return 1;
		count += (size_t)check;
	}
	*ret = count;

	return 0;
}


/*
 *  unicode buffer
 */
static int UTF8_length(addr pos, size_t *ret)
{
	size_t i, w, size;
	unicode c;

	w = 0;
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &c);
		if (c < 0x80) w += 1;
		else if (c < 0x0800) w += 2;
		else if (c < 0xD800) w += 3;
		else if (c < 0xE000) return 1; /* surrogate pair */
		else if (c < 0x010000) w += 3;
#ifdef UTF8_SEQ5CHECK
		else if (c < UnicodeCount) w += 4;
#else
		else if (c < 0x200000) w += 4;
		else if (c < 0x04000000) w += 5;
		else if (c < 0x80000000) w += 6;
#endif
		else return 1;
	}
	*ret = w;

	return 0;
}

static int UTF16_length(addr pos, size_t *ret)
{
	size_t i, w, size;
	unicode c;

	w = 0;
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &c);
		if (c < 0xD800) w += 1;
		else if (c < 0xE000) return 1; /* surrogate pair */
		else if (c < 0x010000) w += 1;
		else if (c < UnicodeCount) w += 2;
		else return 1;
	}
	*ret = w;

	return 0;
}

static int UTF8_make(byte *dst, addr pos)
{
	size_t size, i, w, check;
	unicode u;

	string_length(pos, &size);
	w = 0;
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &u);
		if (UTF8_encode(u, dst + w, &check))
			return 1;
		w += check;
	}
	dst[w] = 0;

	return 0;
}

static int UTF16_make(byte16 *dst, addr pos)
{
	size_t size, i, w, check;
	unicode u;

	string_length(pos, &size);
	w = 0;
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &u);
		if (UTF16_encode_ptr(u, dst + w, &check))
			return 1;
		w += check;
	}
	dst[w++] = 0;
	dst[w] = 0;

	return 0;
}

int UTF8_buffer_clang(LocalRoot local, addr *ret, addr string)
{
	addr pos, body;
	size_t size;

	if (UTF8_length(string, &size))
		return 1;
	local_body(local, &pos, LISPSYSTEM_UNICODE, size + 1);
	posbody(pos, &body);
	if (UTF8_make((byte *)body, string))
		return 1;
	*ret = pos;

	return 0;
}

int UTF16_buffer_clang(LocalRoot local, addr *ret, addr string)
{
	addr pos, body;
	size_t size;

	if (UTF16_length(string, &size))
		return 1;
	local_body(local, &pos, LISPSYSTEM_UNICODE, (size + 1) * 2);
	posbody(pos, &body);
	if (UTF16_make((byte16 *)body, string))
		return 1;
	*ret = pos;

	return 0;
}


/*
 *  unicode string
 */
#define readtable_finish(size, i, c) ((size)? ((size) < (i)): ((c) == 0))

#define readtableunicode(src, size, i, c) { \
	(c) = (src)[(i)++]; \
	if (readtable_finish((size), (i), (c))) goto end_of_file; \
}
static int UTF8_table(unicode *dst, const byte *src, size_t size, size_t *ret)
{
	unicode result;
	size_t i, count;
	byte c;

	i = 0;
	count = 0;

loop:
	c = src[i++];
	if (readtable_finish(size, i, c)) goto end_of_file;
	if (0x00 <= c && c <= 0x7F) goto sequence1;
	if (0xC2 <= c && c <= 0xDF) goto sequence2;
	if (0xE0 <= c && c <= 0xEF) goto sequence3;
	if (0xF0 <= c && c <= 0xF7) goto sequence4;
	if (0xF8 <= c && c <= 0xFB) goto sequence5;
	if (0xFC <= c && c <= 0xFD) goto sequence6;
	goto unicode_error;

sequence1:
	result = (unicode)c;
	goto normal;

sequence2:
	result = (0x1F & c) << 6;
	readtableunicode(src, size, i, c);
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= 0x3F & c;
	Check(result < 0x0080 || 0x07FF < result, "range error");
	goto normal;

sequence3:
	result = (0x0F & c) << 12;
	readtableunicode(src, size, i, c);
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= (0x3F & c) << 6;
	readtableunicode(src, size, i, c);
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= 0x3F & c;
	if (result < 0x0800) goto range_error;
	if (UTF16range(result)) goto surrogate_error;
	goto normal;

sequence4:
	result = (0x07 & c) << 18;
	readtableunicode(src, size, i, c);
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= (0x3F & c) << 12;
	readtableunicode(src, size, i, c);
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= (0x3F & c) << 6;
	readtableunicode(src, size, i, c);
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= 0x3F & c;
	if (result < 0x010000) goto range_error;
#ifdef UTF8_SEQ5CHECK
	if (UnicodeCount <= result) goto range_error;
#endif
	goto normal;

sequence5:
#ifdef UTF8_SEQ5CHECK
	goto sequence_error;
#else
	result = (0x03 & c) << 24;
	readtableunicode(src, size, i, c);
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= (0x3F & c) << 18;
	readtableunicode(src, size, i, c);
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= (0x3F & c) << 12;
	readtableunicode(src, size, i, c);
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= (0x3F & c) << 6;
	readtableunicode(src, size, i, c);
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= 0x3F & c;
	if (result < 0x00200000 || 0x03FFFFFF < result) goto range_error;
	goto normal;
#endif

sequence6:
#ifdef UTF8_SEQ5CHECK
	goto sequence_error;
#else
	result = (0x01 & c) << 30;
	readtableunicode(src, size, i, c);
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= (0x3F & c) << 24;
	readtableunicode(src, size, i, c);
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= (0x3F & c) << 18;
	readtableunicode(src, size, i, c);
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= (0x3F & c) << 12;
	readtableunicode(src, size, i, c);
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= (0x3F & c) << 6;
	readtableunicode(src, size, i, c);
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= 0x3F & c;
	if (result < 0x04000000 || 0x7FFFFFFF < result) goto range_error;
	goto normal;
#endif

normal:
	if (dst) dst[count] = result;
	count++;
	goto loop;

unicode_error:
	return -2;
range_error:
	return -3;
sequence_error:
	return -4;
surrogate_error:
	return -5;
end_of_file:
	if (ret) *ret = count;
	return 0;
}

int UTF8_null_strlen(const byte *src, size_t *ret)
{
	return UTF8_table(NULL, src, 0, ret);
}

int UTF8_size_strlen(const byte *src, size_t size, size_t *ret)
{
	if (size == 0) {
		*ret = 0;
		return 0;
	}

	return UTF8_table(NULL, src, size, ret);
}

int UTF8_null_makeunicode(unicode *dst, const byte *src)
{
	return UTF8_table(dst, src, 0, NULL);
}

int UTF8_size_makeunicode(unicode *dst, const byte *src, size_t size)
{
	if (size == 0)
		return 0;

	return UTF8_table(dst, src, size, NULL);
}

int UTF16_null_strlen(const byte16 *src, size_t *ret)
{
	size_t count;
	byte16 c;

	for (count = 0; ; count++) {
		c = *(src++);
		if (c == 0)
			break;
		if (UTF16low(c))
			return 1;
		if (UTF16high(c)) {
			c = *(src++);
			if (! UTF16low(c))
				return 1;
		}
	}
	*ret = count;

	return 0;
}

int UTF16_size_strlen(const byte16 *src, size_t size, size_t *ret)
{
	size_t count, i;
	byte16 c;

	i = 0;
	count = 0;
	while (i < size) {
		c = src[i];
		if (UTF16low(c))
			return 1;
		if (UTF16high(c)) {
			i++;
			if (size <= i)
				return 1;
			c = src[i];
			if (! UTF16low(c))
				return 1;
		}
		i++;
		count++;
	}
	*ret = count;

	return 0;
}

int UTF16_null_makeunicode(unicode *dst, const byte16 *src)
{
	byte16 a, b;

	for (;;) {
		a = *(src++);
		if (a == 0)
			break;
		if (UTF16low(a))
			return 1;
		if (UTF16high(a)) {
			b = *(src++);
			if (! UTF16low(b))
				return 1;
			*(dst++) = UTF16unicode(a, b);
		}
		else {
			*(dst++) = (unicode)a;
		}
	}

	return 0;
}

int UTF16_size_makeunicode(unicode *dst, const byte16 *src, size_t size)
{
	size_t i;
	byte16 a, b;

	for (i = 0; i < size; i++) {
		a = src[i];
		if (UTF16low(a))
			return 1;
		if (UTF16high(a)) {
			i++;
			if (size <= i)
				return 1;
			b = src[i];
			if (! UTF16low(b))
				return 1;
			*(dst++) = UTF16unicode(a, b);
		}
		else {
			*(dst++) = (unicode)a;
		}
	}

	return 0;
}


/*
 *  initialize
 */
void init_encode(void)
{
	init_encode_read_char();
	init_encode_read_hang();
	init_encode_write_char();
	init_encode_length_char();
}

