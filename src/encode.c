#include "character.h"
#include "condition.h"
#include "file_memory.h"
#include "object.h"
#include "stream.h"
#include "strtype.h"
#include "unicode.h"

#define UTF8_SEQ5CHECK

/*
 *  BOM
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
	check = getc_filememory(fm, &c);
	if (check)
		goto invalid; /* invalid bom sequence */
	if (c != 0xBB)
		goto invalid;
	check = getc_filememory(fm, &c);
	if (check)
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
	check = getc_filememory(fm, &b);
	if (check)
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
	check = getc_filememory(fm, &b);
	if (check < 0)
		goto invalid;
	check = getc_filememory(fm, &c);
	if (check < 0)
		goto invalid;
	check = getc_filememory(fm, &d);
	if (check < 0)
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

int writebom16_encode(struct filememory *fm, int bigp)
{
	if (bigp) {
		if (putc_filememory(fm, 0xFE)) return 1;
		if (putc_filememory(fm, 0xFF)) return 1;
	}
	else {
		if (putc_filememory(fm, 0xFF)) return 1;
		if (putc_filememory(fm, 0xFE)) return 1;
	}
	return 0;
}

int writebom32_encode(struct filememory *fm, int bigp)
{
	if (bigp) {
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
 *  file
 */
/* read_char */
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
		if (fm->encode.error) {
			Debug2("read-char cannot read a %d character in ascii stream.", (int)c);
			return -1;
		}
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
	result = 0x7F & c;
	goto normal;

sequence2:
	result = (0x1F & c) << 6;

	readbyteunicode(check, fm, c);
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= 0x3F & c;

	if (result < 0x0080 || 0x07FF < result) goto range_error;
	goto normal;

sequence3:
	result = (0x0F & c) << 12;

	readbyteunicode(check, fm, c);
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= (0x3F & c) << 6;
	readbyteunicode(check, fm, c);
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= 0x3F & c;

	if (result < 0x0800 || 0xFFFF < result) goto range_error;
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

	if (result < 0x010000 || 0x1FFFFF < result) goto range_error;
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
end_of_file:
	return 1;
}

#define readtable_finish(size, i, c) ((size)? ((size) <= (i)): ((c) == 0))

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
	result = 0x7F & c;
	goto normal;

sequence2:
	result = (0x1F & c) << 6;

	readtableunicode(src, size, i, c);
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= 0x3F & c;

	if (result < 0x0080 || 0x07FF < result) goto range_error;
	goto normal;

sequence3:
	result = (0x0F & c) << 12;

	readtableunicode(src, size, i, c);
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= (0x3F & c) << 6;
	readtableunicode(src, size, i, c);
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= 0x3F & c;

	if (result < 0x0800 || 0xFFFF < result) goto range_error;
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

	if (result < 0x010000 || 0x1FFFFF < result) goto range_error;
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
end_of_file:
	if (ret) *ret = count;
	return 0;
}

static int read_char_utf8(struct filememory *fm, unicode *u)
{
	int check;

	check = getutf8(fm, u);
	if (check == -1) return check; /* file error */
	if (check == 1) return check; /* eof */
	if (check < 0) {
		if (fm->encode.error) {
			Debug("Invalid character in UTF8 stream.");
			return -1;
		}
		*u = fm->encode.code;
	}

	return 0;
}

#define UTF16range(x) (0xD800 <= (x) && (x) < 0xE000)
#define UTF16unicode(a, b) \
	((((((a)>>6UL)&0x0FUL)+1UL)<<16UL) | (((a)&0x3FUL)<<10UL) | ((b)&0x03FFUL))

static int getutf16(struct filememory *fm, unicode *ret, int bigp)
{
	int check;
	byte a, b;
	uint16_t c, d;

	/* read character */
	check = getc_filememory(fm, &a);
	if (check < 0) goto file_error;
	if (check) goto end_of_file;
	check = getc_filememory(fm, &b);
	if (check) goto file_error;
	if (bigp)
		c = (a << 8) | b;
	else
		c = a | (b << 8);

	/* 1 character */
	if (! UTF16range(c)) {
		*ret = a;
		goto normal;
	}

	/* Surrogate pair */
	check = getc_filememory(fm, &a);
	if (check) goto file_error;
	check = getc_filememory(fm, &b);
	if (check) goto file_error;
	if (bigp)
		d = (a << 8) | b;
	else
		d = a | (b << 8);
	if (! UTF16range(d))
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

static int read_char_utf16le(struct filememory *fm, unicode *u)
{
	int check;

	check = getutf16(fm, u, 0);
	if (check == -1) return check; /* file error */
	if (check == 1) return check; /* eof */
	if (check < 0) {
		if (fm->encode.error) {
			Debug("Invalid character in UTF16 stream.");
			return -1;
		}
		*u = fm->encode.code;
	}

	return 0;
}

static int read_char_utf16be(struct filememory *fm, unicode *u)
{
	int check;

	check = getutf16(fm, u, 1);
	if (check == -1) return check; /* file error */
	if (check == 1) return check; /* eof */
	if (check < 0) {
		if (fm->encode.error) {
			Debug("Invalid character in UTF16 stream.");
			return -1;
		}
		*u = fm->encode.code;
	}

	return 0;
}

static int getutf32(struct filememory *fm, unicode *ret, int bigp)
{
	int check;
	byte a, b, c, d;

	check = getc_filememory(fm, &a);
	if (check < 0) goto file_error;
	if (check) goto end_of_file;
	check = getc_filememory(fm, &b);
	if (check) goto file_error;
	check = getc_filememory(fm, &c);
	if (check) goto file_error;
	check = getc_filememory(fm, &d);
	if (check) goto file_error;

	if (bigp)
		*ret = (a << 24) | (b << 16) | (c << 8) | d;
	else
		*ret = (d << 24) | (c << 16) | (b << 8) | a;
	return 0;

file_error:
	return -1;
end_of_file:
	return 1;
}

static int read_char_utf32le(struct filememory *fm, unicode *u)
{
	int check;

	check = getutf32(fm, u, 0);
	if (check == -1) return check; /* file error */
	if (check == 1) return check; /* eof */
	if (check < 0) {
		if (fm->encode.error) {
			Debug("Invalid character in UTF32 stream.");
			return -1;
		}
		*u = fm->encode.code;
	}

	return 0;
}

static int read_char_utf32be(struct filememory *fm, unicode *u)
{
	int check;

	check = getutf32(fm, u, 1);
	if (check == -1) return check; /* file error */
	if (check == 1) return check; /* eof */
	if (check < 0) {
		if (fm->encode.error) {
			Debug("Invalid character in UTF32 stream.");
			return -1;
		}
		*u = fm->encode.code;
	}

	return 0;
}

static int read_char_windows(struct filememory *fm, unicode *u)
{
	fmte("Invalid external-format :windows.", NULL);
	return -1;
}

typedef int (*read_char_calltype)(struct filememory *, unicode *);
static read_char_calltype read_char_call[] = {
	read_char_binary,
	read_char_ascii,
	read_char_utf8,
	read_char_utf16le,
	read_char_utf16be,
	read_char_utf32le,
	read_char_utf32be,
	read_char_windows
};
int read_char_encode(struct filememory *fm, unicode *c)
{
	return (read_char_call[(int)fm->encode.type])(fm, c);
}

/* read_hang */
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
		if (fm->encode.error) {
			Debug2("read-hang cannot read a %d character in ascii stream.", (int)c);
			return -1;
		}
		*u = fm->encode.code;
		return 0;
	}
	*u = (unicode)c;

	return 0;
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
	result = 0x7F & c;
	goto normal;

sequence2:
	result = (0x1F & c) << 6;

	readbyteunicode_nonblocking();
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= 0x3F & c;

	if (result < 0x0080 || 0x07FF < result) goto range_error;
	goto normal;

sequence3:
	result = (0x0F & c) << 12;

	readbyteunicode_nonblocking();
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= (0x3F & c) << 6;
	readbyteunicode_nonblocking();
	if (c < 0x80 || 0xBF < c) goto unicode_error;
	result |= 0x3F & c;

	if (result < 0x0800 || 0xFFFF < result) goto range_error;
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

	if (result < 0x010000 || 0x1FFFFF < result) goto range_error;
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
end_of_file:
	return 1;
rollback:
	while (index) {
		index--;
		if (ungetc_filememory(fm, data[index]))
			goto file_error;
	}
	*hang = 1;
	return 0;
}

static int read_hang_utf8(struct filememory *fm, unicode *u, int *hang)
{
	int check;

	check = getutf8_nonblocking(fm, u, hang);
	if (check == -1) return check; /* file error */
	if (check == 1) return check; /* eof */
	if (check < 0) {
		if (fm->encode.error) {
			Debug("Invalid character in UTF8 stream.");
			return -1;
		}
		*u = fm->encode.code;
	}

	return 0;
}

static int read_hang_utf16le(struct filememory *fm, unicode *u, int *hang)
{
	fmte("Invalid external-format :utf16le.", NULL);
	return -1;
}

static int read_hang_utf16be(struct filememory *fm, unicode *u, int *hang)
{
	fmte("Invalid external-format :utf16be.", NULL);
	return -1;
}

static int read_hang_utf32le(struct filememory *fm, unicode *u, int *hang)
{
	fmte("Invalid external-format :utf32le.", NULL);
	return -1;
}

static int read_hang_utf32be(struct filememory *fm, unicode *u, int *hang)
{
	fmte("Invalid external-format :utf32be.", NULL);
	return -1;
}

static int read_hang_windows(struct filememory *fm, unicode *u, int *hang)
{
	fmte("Invalid external-format :windows.", NULL);
	return -1;
}

typedef int (*read_hang_calltype)(struct filememory *, unicode *, int *);
static read_hang_calltype read_hang_call[] = {
	read_hang_binary,
	read_hang_ascii,
	read_hang_utf8,
	read_hang_utf16le,
	read_hang_utf16be,
	read_hang_utf32le,
	read_hang_utf32be,
	read_hang_windows
};
int read_hang_encode(struct filememory *fm, unicode *c, int *hang)
{
	return (read_hang_call[(int)fm->encode.type])(fm, c, hang);
}

/* write_char */
static int write_char_binary(struct filememory *fm, unicode u)
{
	Debug("Cannot execute write-char in binary stream.");
	return -1;
}

static int write_char_ascii(struct filememory *fm, unicode u)
{
	if (0x80 <= u) {
		if (fm->encode.error) {
			Debug2("write-char cannot write a %lu character "
					"in ascii stream.", (unsigned long)u);
			return -1;
		}
		u = fm->encode.code;
		Check(0x80 <= u, "character error");
	}

	return putc_filememory(fm, (byte)u);
}

static int UTF8_encode(unicode u, byte *dst, size_t *ret)
{
	size_t w;

	w = 0;
	if (u < 0x80) {
		dst[w++] = u;
	}
	else if (u < 0x0800) {
		dst[w++] = 0xC2 | (u >> 6);
		dst[w++] = 0x80 | (0x3F & u);
	}
	else if (u < 0x010000) {
		dst[w++] = 0xE0 | (u >> 12);
		dst[w++] = 0x80 | (0x3F & (u >> 6));
		dst[w++] = 0x80 | (0x3F & u);
	}
	else if (u < 0x200000) {
		dst[w++] = 0xF0 | (u >> 18);
		dst[w++] = 0x80 | (0x3F & (u >> 12));
		dst[w++] = 0x80 | (0x3F & (u >> 6));
		dst[w++] = 0x80 | (0x3F & u);
	}
	else if (u < 0x04000000) {
		dst[w++] = 0xF8 | (u >> 24);
		dst[w++] = 0x80 | (0x3F & (u >> 18));
		dst[w++] = 0x80 | (0x3F & (u >> 12));
		dst[w++] = 0x80 | (0x3F & (u >> 6));
		dst[w++] = 0x80 | (0x3F & u);
	}
	else if (u < 0x80000000) {
		dst[w++] = 0xFC | (u >> 30);
		dst[w++] = 0x80 | (0x3F & (u >> 24));
		dst[w++] = 0x80 | (0x3F & (u >> 18));
		dst[w++] = 0x80 | (0x3F & (u >> 12));
		dst[w++] = 0x80 | (0x3F & (u >> 6));
		dst[w++] = 0x80 | (0x3F & u);
	}
	else {
		return 1;  /* encoding error */
	}
	*ret = w;

	return 0;
}

static int write_char_utf8(struct filememory *fm, unicode u)
{
	byte buffer[8];
	int check;
	size_t size;

	check = UTF8_encode(u, buffer, &size);
	if (check) {
		if (fm->encode.error) {
			Debug2("write-char cannot write a %lu character "
					"in utf8 stream.", (unsigned long)u);
			return -1;
		}
		check = UTF8_encode(fm->encode.code, buffer, &size);
		Check(check, "character error");
	}

	return write_filememory(fm, buffer, size, &size);
}

static int UTF16_encode(unicode u, byte16 *dst, size_t *ret)
{
	unicode a;
	size_t w;

	w = 0;
	if (u < 0xD800) {
		dst[w++] = 0xFFFF & u;
	}
	else if (u < 0xE000) {
		return 1;
	}
	else if (u < 0x010000) {
		dst[w++] = 0xFFFF & u;
	}
	else if (u < 0x110000) {
		a = 0xDC00 | (0x03FF & u);
		dst[w++] = 0xFFFF & a;
		a = (((u >> 16) & 0x1F) - 1) | (0x3F & u);
		dst[w++] = 0xFFFF & a;
	}
	else {
		return 1;
	}
	*ret = w;

	return 0;
}

#if defined(LISP_ANSI_WINDOWS)
static int write_char_windows(struct filememory *fm, unicode u)
{
	char output[8];
	wchar_t input[4];
	byte *ptr, c;
	int check;
	size_t size;

	/* ascii code */
	if (u < 0x80)
		return putc_filememory(fm, u);

	/* unicode -> UTF16 */
	check = UTF16_encode(u, (byte16 *)input, &size);
	if (check) {
		if (fm->encode.error) {
			Debug2("write-char cannot write a %lu character "
					"in windows stream.", (unsigned long)u);
			return -1;
		}
		check = UTF16_encode(fm->encode.code, (byte16 *)input, &size);
		Check(check, "character error");
	}
	input[size] = 0;

	/* UTF16 -> ANSI-code-page  */
	snprintf(output, 8, "%S", input);
	if (output[0] == 0) return -1;
	ptr = (byte *)output;
	for (;;) {
		c = *ptr;
		if (c == 0) break;
		check = putc_filememory(fm, c);
		if (check != 0) return -1;
		ptr++;
	}

	return 0;
}

#elif (defined LISP_WINDOWS)
static int write_char_windows(struct filememory *fm, unicode u)
{
	char output[8];
	wchar_t input[4];
	byte *ptr;
	int check, result, i;
	size_t size;

	/* ascii code */
	if (u < 0x80)
		return putc_filememory(fm, u);

	/* unicode -> UTF16 */
	check = UTF16_encode(u, (byte16 *)input, &size);
	if (check) {
		if (fm->encode.error) {
			Debug2("write-char cannot write a %lu character "
					"in windows stream.", (unsigned long)u);
			return -1;
		}
		check = UTF16_encode(fm->encode.code, (byte16 *)input, &size);
		Check(check, "character error");
	}
	input[size] = 0;

	/* UTF16 -> ANSI-code-page  */
	result = WideCharToMultiByte(CP_THREAD_ACP,
			0,
			(LPCWSTR)input,
			(int)size,
			(LPSTR)output,
			8,
			NULL,
			NULL);
	if (result == 0) return -1;
	ptr = (byte *)output;
	for (i = 0; i < result; i++) {
		check = putc_filememory(fm, ptr[i]);
		if (check != 0) return -1;
	}

	return 0;
}

#else
static int write_char_windows(struct filememory *fm, unicode u)
{
	Debug("This implementation cannot write a windows stream.");
	return -1;
}
#endif

typedef int (*write_char_calltype)(struct filememory *, unicode);
static write_char_calltype write_char_call[] = {
	write_char_binary,
	write_char_ascii,
	write_char_utf8,
	NULL, /* EncodeType_utf16le */
	NULL, /* EncodeType_utf16be */
	NULL, /* EncodeType_utf32be */
	NULL, /* EncodeType_utf32le */
	write_char_windows,
};
int write_char_encode(struct filememory *fm, unicode u)
{
	return (write_char_call[(int)fm->encode.type])(fm, u);
}

/* length-char */
static int length_char_binary(struct filememory *fm, unicode c)
{
	return 1;
}

static int length_char_utf8(struct filememory *fm, unicode c)
{
	if (c < 0x80) return 1;
	if (c < 0x0800) return 2;
	if (c < 0x010000) return 3;
	if (c < 0x200000) return 4;
#ifdef UTF8_SEQ5CHECK
	if (c < 0x04000000) return 5;
	if (c < 0x80000000) return 6;
#endif
	return -1;
}

static int length_char_utf16(struct filememory *fm, unicode c)
{
	if (c < 0x010000) return 2;
	if (c < 0x110000) return 4;
	return -1;
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

	/* ascii code */
	if (c < 0x80)
		return 1;

	/* unicode -> UTF16 */
	if (UTF16_encode(u, (byte16 *)input, &size))
		return -1;
	input[size] = 0;

	/* UTF16 -> ANSI-code-page  */
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

	/* ascii code */
	if (c < 0x80)
		return 1;

	/* unicode -> UTF16 */
	if (UTF16_encode(c, (byte16 *)input, &size))
		return -1;
	input[size] = 0;

	/* UTF16 -> ANSI-code-page  */
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
	Debug("This implementation cannot use a windows stream.");
	return -1;
}
#endif

typedef int (*length_char_calltype)(struct filememory *, unicode);
static length_char_calltype length_char_call[] = {
	length_char_binary,
	length_char_binary,
	length_char_utf8,
	length_char_utf16,
	length_char_utf16,
	length_char_utf32,
	length_char_utf32,
	length_char_windows,
};
static int length_char_operator(struct filememory *fm, unicode c)
{
	/* Surrogate pair */
	if (UTF16range(c)) return -1;
	/* Invalid unicode */
	if (UnicodeCount <= c) return -1;
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
 *  UTF8, UTF16LE
 */
static int UTF8_length(addr pos, size_t *ret)
{
	size_t i, size, result;
	unicode c;

	result = 0;
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &c);
		if (c < 0x80) result += 1;
		else if (c < 0x0800) result += 2;
		else if (c < 0x010000) result += 3;
		else if (c < 0x200000) result += 4;
		else if (c < 0x04000000) result += 5;
		else if (c < 0x80000000) result += 6;
		else return 1;
	}
	*ret = result;

	return 0;
}

static int UTF8_make(byte *dst, addr pos, size_t size)
{
	size_t i, w, check;
	unicode u;

	w = 0;
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &u);
		if (UTF8_encode(u, dst + w, &check)) return 1;
		w += check;
	}
	dst[w] = 0;

	return 0;
}

static int UTF16_length(addr pos, size_t *ret)
{
	size_t i, size, result;
	unicode c;

	result = 0;
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &c);
		if (c < 0xD800) result += 1;
		else if (c < 0xE000) return 1;
		else if (c < 0x010000) result += 1;
		else if (c < 0x110000) result += 2;
		else return 1;
	}
	*ret = result;

	return 0;
}

static int UTF16LE_encode(unicode u, byte *dst, size_t *ret)
{
	byte16 buffer[2], a;
	size_t w, size;

	if (UTF16_encode(u, buffer, &size)) return 1;
	w = 0;
	if (size == 1) {
		a = buffer[0];
		dst[w++] = (byte)(0xFF & a); a >>= 8;
		dst[w++] = (byte)(0xFF & a);
	}
	else if (size == 2) {
		a = buffer[0];
		dst[w++] = (byte)(0xFF & a); a >>= 8;
		dst[w++] = (byte)(0xFF & a);
		a = buffer[1];
		dst[w++] = (byte)(0xFF & a); a >>= 8;
		dst[w++] = (byte)(0xFF & a);
	}
	else {
		return 1;
	}
	*ret = w;

	return 0;
}

static int UTF16LE_make(byte *dst, addr pos, size_t size)
{
	size_t i, w, check;
	unicode u;

	w = 0;
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &u);
		if (UTF16LE_encode(u, dst + w, &check)) return 1;
		w += check;
	}
	dst[w++] = 0;
	dst[w] = 0;

	return 0;
}

static int UTF16_make(byte16 *dst, addr pos, size_t size)
{
	size_t i, w, check;
	unicode u;

	w = 0;
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &u);
		if (UTF16_encode(u, dst + w, &check)) return 1;
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

	if (UTF8_length(string, &size)) {
		Debug("UTF8 parse error");
		return 1;
	}
	local_body(local, &pos, LISPTYPE_SYSTEM, size + 1);
	posbody(pos, &body);
	UTF8_make((byte *)body, string, size);
	*ret = pos;

	return 0;
}

int UTF16_buffer_clang(LocalRoot local, addr *ret, addr string)
{
	addr pos, body;
	size_t size;

	if (UTF16_length(string, &size)) {
		Debug("UTF16 parse error");
		return 1;
	}
	local_body(local, &pos, LISPTYPE_SYSTEM, (size + 1) * 2);
	posbody(pos, &body);
	UTF16_make((byte16 *)body, string, size);
	*ret = pos;

	return 0;
}

int UTF16LE_buffer_clang(LocalRoot local, addr *ret, addr string)
{
	addr pos, body;
	size_t size;

	if (UTF16_length(string, &size)) {
		Debug("UTF16 parse error");
		return 1;
	}
	local_body(local, &pos, LISPTYPE_SYSTEM, (size + 1) * 2);
	posbody(pos, &body);
	UTF16LE_make((byte *)body, string, size);
	*ret = pos;

	return 0;
}

int UTF8_putc(addr stream, unicode c)
{
	byte dst[8];
	size_t i, size;

	if (UTF8_encode(c, dst, &size)) return 1;
	for (i = 0; i < size; i++)
		write_byte_stream(stream, dst[i]);

	return 0;
}


/*
 *  unicode string
 */
int UTF8_null_strlen(const byte *src, size_t *ret)
{
	if (src[0] == 0) {
		*ret = 0;
		return 0;
	}

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
	if (src[0] == 0) {
		return 0;
	}

	return UTF8_table(dst, src, 0, NULL);
}

int UTF8_size_makeunicode(unicode *dst, const byte *src, size_t size)
{
	if (size == 0) {
		return 0;
	}

	return UTF8_table(dst, src, size, NULL);
}

int UTF16_null_strlen(const byte16 *src, size_t *ret)
{
	size_t count;
	byte16 c;

	for (count = 0; ; count++) {
		c = *src;
		if (c == 0) break;
		if (UTF16range(c)) {
			c = *(++src);
			if (! UTF16range(c)) return 1;
		}
		src++;
	}
	*ret = count;

	return 0;
}

int UTF16_size_strlen(const byte16 *src, size_t size, size_t *ret)
{
	size_t count, i;
	byte16 c;

	i = 0;
	for (count = 0; i < size; count++, i++) {
		c = *src;
		if (UTF16range(c)) {
			if (size <= (i++) + 1) return 1;
			c = *(++src);
			if (! UTF16range(c)) return 1;
		}
	}
	*ret = count;

	return 0;
}

int UTF16_null_makeunicode(unicode *dst, const byte16 *src)
{
	byte16 a, b;

	for (;;) {
		a = *src;
		if (a == 0) break;
		if (UTF16range(a)) {
			b = *(++src);
			if (! UTF16range(b)) return 1;
			*(dst++) = UTF16unicode(a, b);
		}
		else {
			*(dst++) = (unicode)a;
		}
		src++;
	}

	return 0;
}

int UTF16_size_makeunicode(unicode *dst, const byte16 *src, size_t size)
{
	size_t i;
	byte16 a, b;

	for (i = 0; i < size; i++) {
		a = *src;
		if (UTF16range(a)) {
			if (size <= (i++) + 1) return 1;
			b = *(++src);
			if (! UTF16range(b)) return 1;
			*(dst++) = UTF16unicode(a, b);
		}
		else {
			*(dst++) = (unicode)a;
		}
	}

	return 0;
}

