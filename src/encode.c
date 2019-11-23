#include "character.h"
#include "condition.h"
#include "encode.h"
#include "encode_unicode.h"
#include "file_memory.h"
#include "object.h"
#include "stream.h"
#include "strtype.h"
#include "unicode.h"

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
_g int readbom8_encode(struct filememory *fm)
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

_g int readbom16_encode(struct filememory *fm)
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

_g int readbom32_encode(struct filememory *fm)
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

_g int writebom_encode(struct filememory *fm)
{
	return write_char_encode(fm, 0xFEFF);
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
		return 1;
	if (0x80 <= c) {
		if (fm->encode.error)
			return -1;
		*u = fm->encode.code;
		return 0;
	}
	*u = (unicode)c;

	return 0;
}

static int read_char_utf8(struct filememory *fm, unicode *u)
{
	int check;

	check = read_utf8_normal(fm, u);
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

static int read_char_utf16(struct filememory *fm, unicode *u, int big_endian_p)
{
	int check;

	check = read_utf16_normal(fm, u, big_endian_p);
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

static int read_char_utf32(struct filememory *fm, unicode *u, int big_endian_p)
{
	int check;

	check = read_utf32_normal(fm, u, big_endian_p);
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

_g int read_char_encode(struct filememory *fm, unicode *c)
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
		return 1;
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

static int read_hang_utf8(struct filememory *fm, unicode *u, int *hang)
{
	int check;

	check = read_utf8_nonblocking(fm, u, hang);
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

static int read_hang_utf16(struct filememory *fm,
		unicode *u, int *hang, int big_endian_p)
{
	int check;

	check = read_utf16_nonblocking(fm, u, hang, big_endian_p);
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

static int read_hang_utf32(struct filememory *fm,
		unicode *u, int *hang, int big_endian_p)
{
	int check;

	check = read_utf32_nonblocking(fm, u, hang, big_endian_p);
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

_g int read_hang_encode(struct filememory *fm, unicode *c, int *hang)
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

static int write_char_utf8(struct filememory *fm, unicode u)
{
	byte data[8];
	size_t size;

	if (! encode_utf8(u, data, &size))
		return write_filememory(fm, data, size, &size);
	if (fm->encode.error)
		return -1;
	if (! encode_utf8(fm->encode.code, data, &size))
		return write_filememory(fm, data, size, &size);

	return -1;
}

static int write_char_utf16(struct filememory *fm, unicode u, int big_endian_p)
{
	byte data[8];
	size_t size;

	if (! encode_utf16(u, big_endian_p, data, &size))
		return write_filememory(fm, data, size, &size);
	if (fm->encode.error)
		return -1;
	if (! encode_utf16(fm->encode.code, big_endian_p, data, &size))
		return write_filememory(fm, data, size, &size);
	else
		return -1;
}

static int write_char_utf16le(struct filememory *fm, unicode u)
{
	return write_char_utf16(fm, u, 0);
}

static int write_char_utf16be(struct filememory *fm, unicode u)
{
	return write_char_utf16(fm, u, 1);
}

static int write_char_utf32(struct filememory *fm, unicode u, int big_endian_p)
{
	byte data[8];
	size_t size;

	if (! encode_utf32(u, big_endian_p, data, &size))
		return write_filememory(fm, data, size, &size);
	if (fm->encode.error)
		return -1;
	if (! encode_utf32(fm->encode.code, big_endian_p, data, &size))
		return write_filememory(fm, data, size, &size);
	else
		return -1;
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
	if (! encode_utf16b(u, (byte16 *)input, &size))
		goto normal;
	if (fm->encode.error)
		return -1;
	if (! encode_utf16b(fm->encode.code, (byte16 *)input, &size))
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
	if (! encode_utf16b(u, (byte16 *)input, &size))
		goto normal;
	if (fm->encode.error)
		return -1;
	if (! encode_utf16b(fm->encode.code, (byte16 *)input, &size))
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

_g int write_char_encode(struct filememory *fm, unicode u)
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
	if (! encode_utf16b(u, (byte16 *)input, &size))
		goto normal;
	if (fm->encode.error)
		return -1;
	if (! encode_utf16b(fm->encode.code, (byte16 *)input, &size))
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
	if (! encode_utf16b(c, (byte16 *)input, &size))
		goto normal;
	if (fm->encode.error)
		return -1;
	if (! encode_utf16b(fm->encode.code, (byte16 *)input, &size))
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
#ifdef LISP_UTF8_SEQ5CHECK
	if (UnicodeCount <= c)
		return -1;
#endif
	/* Length */
	return (length_char_call[(int)fm->encode.type])(fm, c);
}

_g int length_char_encode(struct filememory *fm, unicode c)
{
	int check;

	check = length_char_operator(fm, c);
	if (check < 0 && fm->encode.error == 0)
		return length_char_operator(fm, fm->encode.code);
	else
		return check;
}

_g int length_string_encode(struct filememory *fm, addr pos, size_t *ret)
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
_g int UTF32_length_utf8(const unicode *ptr, size_t size, size_t *ret)
{
	size_t i, w;
	unicode c;

	w = 0;
	for (i = 0; i < size; i++) {
		c = ptr[i];
		if (c < 0x80) w += 1;
		else if (c < 0x0800) w += 2;
		else if (c < 0xD800) w += 3;
		else if (c < 0xE000) return 1; /* surrogate pair */
		else if (c < 0x010000) w += 3;
#ifdef LISP_UTF8_SEQ5CHECK
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

static int UTF8_length(addr pos, size_t *ret)
{
	const unicode *body;
	size_t size;
	string_posbodylen(pos, &body, &size);
	return UTF32_length_utf8(body, size, ret);
}

_g int UTF32_length_utf16(const unicode *ptr, size_t size, size_t *ret)
{
	size_t i, w;
	unicode c;

	w = 0;
	for (i = 0; i < size; i++) {
		c = ptr[i];
		if (c < 0xD800) w += 1;
		else if (c < 0xE000) return 1; /* surrogate pair */
		else if (c < 0x010000) w += 1;
		else if (c < UnicodeCount) w += 2;
		else return 1;
	}
	*ret = w;

	return 0;
}

static int UTF16_length(addr pos, size_t *ret)
{
	const unicode *body;
	size_t size;
	string_posbodylen(pos, &body, &size);
	return UTF32_length_utf16(body, size, ret);
}

static int UTF8_make(byte *dst, addr pos)
{
	size_t size, i, w, check;
	unicode u;

	string_length(pos, &size);
	w = 0;
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &u);
		if (encode_utf8(u, dst + w, &check))
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
		if (encode_utf16b(u, dst + w, &check))
			return 1;
		w += check;
	}
	dst[w++] = 0;
	dst[w] = 0;

	return 0;
}

_g int UTF8_buffer_clang(LocalRoot local, addr *ret, addr string)
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

_g int UTF16_buffer_clang(LocalRoot local, addr *ret, addr string)
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
_g int UTF8_null_strlen(const byte *src, size_t *ret)
{
	return read_utf8_buffer(NULL, src, 0, ret);
}

_g int UTF8_size_strlen(const byte *src, size_t size, size_t *ret)
{
	if (size == 0) {
		*ret = 0;
		return 0;
	}

	return read_utf8_buffer(NULL, src, size, ret);
}

_g int UTF8_null_makeunicode(unicode *dst, const byte *src)
{
	return read_utf8_buffer(dst, src, 0, NULL);
}

_g int UTF8_size_makeunicode(unicode *dst, const byte *src, size_t size)
{
	if (size == 0)
		return 0;

	return read_utf8_buffer(dst, src, size, NULL);
}

_g int UTF16_null_strlen(const byte16 *src, size_t *ret)
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

_g int UTF16_size_strlen(const byte16 *src, size_t size, size_t *ret)
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

_g int UTF16_null_makeunicode(unicode *dst, const byte16 *src)
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

_g int UTF16_size_makeunicode(unicode *dst, const byte16 *src, size_t size)
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

_g int UTF32_make_utf8(byte *dst, const unicode *src, size_t size)
{
	byte data[8];
	size_t i, ret;

	for (i = 0; i < size; i += ret) {
		if (encode_utf8(src[i], data, &ret))
			return 1;
		if (size < i + ret)
			return 1;
		memcpy(dst + i, data, ret);
	}

	return 0;
}

_g int UTF32_make_utf16(byte16 *dst, const unicode *src, size_t size)
{
	byte16 data[4];
	size_t i, ret;

	for (i = 0; i < size; i += ret) {
		if (encode_utf16b(src[i], data, &ret))
			return 1;
		if (size < i + ret)
			return 1;
		memcpy(dst + i, data, ret * 2UL);
	}

	return 0;
}


/*
 *  initialize
 */
_g void init_encode(void)
{
	init_encode_read_char();
	init_encode_read_hang();
	init_encode_write_char();
	init_encode_length_char();
}

