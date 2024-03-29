#include "character.h"
#include "condition.h"
#include "encode.h"
#include "encode_unicode.h"
#include "file.h"
#include "file_memory.h"
#include "object.h"
#include "stream.h"
#include "stream_function.h"
#include "strtype.h"

typedef int (*read_char_calltype)(filestream , unicode *, int *);
typedef int (*read_hang_calltype)(filestream , unicode *, int *, int *);
typedef int (*write_char_calltype)(filestream , unicode);
typedef int (*length_char_calltype)(filestream , unicode);
static read_char_calltype read_char_call[EncodeType_size];
static read_hang_calltype read_hang_call[EncodeType_size];
static write_char_calltype write_char_call[EncodeType_size];
static length_char_calltype length_char_call[EncodeType_size];

/*
 *  Byte Order Mark
 */
static int readbom8_encode_filememory(filestream fm)
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

static int readbom16_encode_filememory(filestream fm)
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

static int readbom32_encode_filememory(filestream fm)
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

static filestream begin_readbom_buffering(addr stream, addr *ret)
{
	filestream fm;
	addr mem;

	Check(! file_stream_p(stream), "type error");
	fm = PtrFileMemory(stream);
	if (fm->redirect) {
		GetPathnameStream(stream, &mem);
		Check(! memory_stream_p(mem), "type error");
		*ret = fm->pos;
		fm->pos = mem;
	}
	else {
		*ret = Nil;
	}

	return fm;
}

static void end_readbom_buffering(addr stream, addr prev)
{
	filestream fm;

	Check(! file_stream_p(stream), "type error");
	fm = PtrFileMemory(stream);
	if (fm->redirect) {
		fm->pos = prev;
	}
}

int readbom8_encode(addr stream)
{
	int check;
	filestream fm;
	addr prev;

	fm = begin_readbom_buffering(stream, &prev);
	check = readbom8_encode_filememory(fm);
	end_readbom_buffering(stream, prev);

	return check;
}

int readbom16_encode(addr stream)
{
	int check;
	filestream fm;
	addr prev;

	fm = begin_readbom_buffering(stream, &prev);
	check = readbom16_encode_filememory(fm);
	end_readbom_buffering(stream, prev);

	return check;
}

int readbom32_encode(addr stream)
{
	int check;
	filestream fm;
	addr prev;

	fm = begin_readbom_buffering(stream, &prev);
	check = readbom32_encode_filememory(fm);
	end_readbom_buffering(stream, prev);

	return check;
}

int writebom_encode_(addr stream)
{
	return write_char_stream_(stream, 0xFEFF);
}


/*
 *  read_char
 */
static int read_char_binary_(filestream fm, unicode *u, int *ret)
{
	return fmte_("Cannot execute read-char in binary stream.", NULL);
}

static int read_char_ascii_(filestream fm, unicode *u, int *ret)
{
	byte c;
	int check;

	check = getc_filememory(fm, &c);
	if (check < 0)
		return fmte_("getc error", NULL);
	if (check)
		return Result(ret, 1);
	if (0x80 <= c) {
		if (fm->encode.error)
			return fmte_("encode error", NULL);
		*u = fm->encode.code;
		return Result(ret, 0);
	}
	*u = (unicode)c;

	return Result(ret, 0);
}

static int read_char_utf8_(filestream fm, unicode *u, int *ret)
{
	int check;

	check = read_utf8_normal(fm, u);
	/* normal */
	if (check == 0)
		return Result(ret, 0);
	/* EOF */
	if (0 < check)
		return Result(ret, 1);
	/* file error */
	if (check == -1)
		return fmte_("read error.", NULL);
	/* encode error */
	if (fm->encode.error)
		return fmte_("encode error.", NULL);
	/* recovery */
	*u = fm->encode.code;

	return Result(ret, 0);
}

static int read_char_utf16_(filestream fm, unicode *u, int be, int *ret)
{
	int check;

	check = read_utf16_normal(fm, u, be);
	/* normal */
	if (check == 0)
		return Result(ret, 0);
	/* EOF */
	if (0 < check)
		return Result(ret, 1);
	/* file error */
	if (check == -1)
		return fmte_("read error.", NULL);
	/* encode error */
	if (fm->encode.error)
		return fmte_("encode error.", NULL);
	/* recovery */
	*u = fm->encode.code;

	return Result(ret, 0);
}

static int read_char_utf16le_(filestream fm, unicode *u, int *ret)
{
	return read_char_utf16_(fm, u, 0, ret);
}

static int read_char_utf16be_(filestream fm, unicode *u, int *ret)
{
	return read_char_utf16_(fm, u, 1, ret);
}

static int read_char_utf32_(filestream fm, unicode *u, int be, int *ret)
{
	int check;

	check = read_utf32_normal(fm, u, be);
	/* normal */
	if (check == 0)
		return Result(ret, 0);
	/* EOF */
	if (0 < check)
		return Result(ret, 1);
	/* file error */
	if (check == -1)
		return fmte_("read error.", NULL);
	/* encode error */
	if (fm->encode.error)
		return fmte_("encode error.", NULL);
	/* recovery */
	*u = fm->encode.code;

	return Result(ret, 0);
}

static int read_char_utf32le_(filestream fm, unicode *u, int *ret)
{
	return read_char_utf32_(fm, u, 0, ret);
}

static int read_char_utf32be_(filestream fm, unicode *u, int *ret)
{
	return read_char_utf32_(fm, u, 1, ret);
}

static int read_char_windows_(filestream fm, unicode *u, int *ret)
{
	return fmte_("Invalid external-format :windows.", NULL);
}

static void init_encode_read_char(void)
{
	read_char_call[EncodeType_binary] = read_char_binary_;
	read_char_call[EncodeType_ascii] = read_char_ascii_;
	read_char_call[EncodeType_utf8] = read_char_utf8_;
	read_char_call[EncodeType_utf16le] = read_char_utf16le_;
	read_char_call[EncodeType_utf16be] = read_char_utf16be_;
	read_char_call[EncodeType_utf32le] = read_char_utf32le_;
	read_char_call[EncodeType_utf32be] = read_char_utf32be_;
	read_char_call[EncodeType_windows] = read_char_windows_;
}

int read_char_encode_(filestream fm, unicode *c, int *ret)
{
	return (read_char_call[(int)fm->encode.type])(fm, c, ret);
}


/*
 *  read_hang
 */
static int read_hang_binary_(filestream fm, unicode *u, int *hang, int *ret)
{
	*u = 0;
	*hang = 0;
	*ret = 0;
	return fmte_("Cannot execute read-char-no-hang in binary stream.", NULL);
}

static int read_hang_ascii_(filestream fm, unicode *u, int *hang, int *ret)
{
	byte c;
	int check;
	size_t size;

	check = getc_nonblock_filememory(fm, &c, &size);
	if (check < 0) {
		*u = 0;
		*hang = 0;
		*ret = 0;
		return fmte_("getc_nonblock error", NULL);
	}
	if (check) {
		*u = 0;
		*hang = 0;
		return Result(ret, 1);
	}
	if (size == 0) {
		*u = 0;
		*hang = 1;
		return Result(ret, 0);
	}
	if (0x80 <= c) {
		if (fm->encode.error) {
			*u = 0;
			*hang = 0;
			*ret = 0;
			return fmte_("read error.", NULL);
		}
		*u = fm->encode.code;
		*hang = 0;
		return Result(ret, 0);
	}
	*u = (unicode)c;
	*hang = 0;
	return Result(ret, 0);
}

static int read_hang_utf8_(filestream fm, unicode *u, int *hang, int *ret)
{
	int check;

	check = read_utf8_nonblock(fm, u, hang);
	/* normal */
	if (check == 0)
		return Result(ret, 0);
	/* EOF */
	if (0 < check) {
		*u = 0;
		*hang = 0;
		return Result(ret, 1);
	}
	/* file error */
	if (check == -1) {
		*u = 0;
		*hang = 0;
		*ret = 0;
		return fmte_("read error.", NULL);
	}
	/* encode error */
	if (fm->encode.error) {
		*u = 0;
		*hang = 0;
		*ret = 0;
		return fmte_("encode error.", NULL);
	}
	/* recovery */
	*u = fm->encode.code;
	*hang = 0;
	return Result(ret, 0);
}

static int read_hang_utf16_(filestream fm,
		unicode *u, int *hang, int *ret, int be)
{
	int check;

	check = read_utf16_nonblock(fm, u, hang, be);
	/* normal */
	if (check == 0)
		return Result(ret, 0);
	/* EOF */
	if (0 < check) {
		*u = 0;
		*hang = 0;
		return Result(ret, 1);
	}
	/* file error */
	if (check == -1) {
		*u = 0;
		*hang = 0;
		*ret = 0;
		return fmte_("read error.", NULL);
	}
	/* encode error */
	if (fm->encode.error) {
		*u = 0;
		*hang = 0;
		*ret = 0;
		return fmte_("encode error.", NULL);
	}
	/* recovery */
	*u = fm->encode.code;
	*hang = 0;
	return Result(ret, 0);
}

static int read_hang_utf16le_(filestream fm, unicode *u, int *hang, int *ret)
{
	return read_hang_utf16_(fm, u, hang, ret, 0);
}

static int read_hang_utf16be_(filestream fm, unicode *u, int *hang, int *ret)
{
	return read_hang_utf16_(fm, u, hang, ret, 1);
}

static int read_hang_utf32_(filestream fm,
		unicode *u, int *hang, int *ret, int be)
{
	int check;

	check = read_utf32_nonblock(fm, u, hang, be);
	/* normal */
	if (check == 0)
		return Result(ret, 0);
	/* EOF */
	if (0 < check) {
		*u = 0;
		*hang = 0;
		return Result(ret, 1);
	}
	/* file error */
	if (check == -1) {
		*u = 0;
		*hang = 0;
		*ret = 0;
		return fmte_("read error.", NULL);
	}
	/* encode error */
	if (fm->encode.error) {
		*u = 0;
		*hang = 0;
		*ret = 0;
		return fmte_("encode error.", NULL);
	}
	/* recovery */
	*u = fm->encode.code;
	*hang = 0;
	return Result(ret, 0);
}

static int read_hang_utf32le_(filestream fm, unicode *u, int *hang, int *ret)
{
	return read_hang_utf32_(fm, u, hang, ret, 0);
}

static int read_hang_utf32be_(filestream fm, unicode *u, int *hang, int *ret)
{
	return read_hang_utf32_(fm, u, hang, ret, 1);
}

static int read_hang_windows_(filestream fm, unicode *u, int *hang, int *ret)
{
	*u = 0;
	*hang = 0;
	*ret = 0;
	return fmte_("Invalid external-format :windows.", NULL);
}

static void init_encode_read_hang(void)
{
	read_hang_call[EncodeType_binary] = read_hang_binary_;
	read_hang_call[EncodeType_ascii] = read_hang_ascii_;
	read_hang_call[EncodeType_utf8] = read_hang_utf8_;
	read_hang_call[EncodeType_utf16le] = read_hang_utf16le_;
	read_hang_call[EncodeType_utf16be] = read_hang_utf16be_;
	read_hang_call[EncodeType_utf32le] = read_hang_utf32le_;
	read_hang_call[EncodeType_utf32be] = read_hang_utf32be_;
	read_hang_call[EncodeType_windows] = read_hang_windows_;
}

int read_hang_encode_(filestream fm, unicode *c, int *hang, int *ret)
{
	return (read_hang_call[(int)fm->encode.type])(fm, c, hang, ret);
}


/*
 *  write_char
 */
static int write_char_binary_(filestream fm, unicode u)
{
	return fmte_("Cannot execute write-char in binary stream.", NULL);
}

static int write_char_ascii_(filestream fm, unicode u)
{
	if (u < 0x80) {
		if (putc_filememory(fm, (byte)u))
			return fmte_("putc error.", NULL);
		return 0;
	}
	if (fm->encode.error)
		return fmte_("encode error.", NULL);
	u = fm->encode.code;
	if (u < 0x80) {
		if (putc_filememory(fm, (byte)u))
			return fmte_("putc error.", NULL);
		return 0;
	}

	return fmte_("encode error.", NULL);
}

static int write_char_utf8_(filestream fm, unicode u)
{
	byte data[8];
	size_t size;

	if (! encode_utf8(u, data, &size)) {
		if (write_filememory(fm, data, size, &size))
			return fmte_("write error.", NULL);
		return 0;
	}
	if (fm->encode.error)
		return fmte_("encode error.", NULL);
	if (! encode_utf8(fm->encode.code, data, &size)) {
		if (write_filememory(fm, data, size, &size))
			return fmte_("write error.", NULL);
		return 0;
	}

	return fmte_("encode error.", NULL);
}

static int write_char_utf16_(filestream fm, unicode u, int be)
{
	byte data[8];
	size_t size;

	if (! encode_utf16(u, be, data, &size)) {
		if (write_filememory(fm, data, size, &size))
			return fmte_("write error.", NULL);
		return 0;
	}
	if (fm->encode.error)
		return fmte_("encode error.", NULL);
	if (! encode_utf16(fm->encode.code, be, data, &size)) {
		if (write_filememory(fm, data, size, &size))
			return fmte_("write error.", NULL);
		return 0;
	}

	return fmte_("encode error.", NULL);
}

static int write_char_utf16le_(filestream fm, unicode u)
{
	return write_char_utf16_(fm, u, 0);
}

static int write_char_utf16be_(filestream fm, unicode u)
{
	return write_char_utf16_(fm, u, 1);
}

static int write_char_utf32_(filestream fm, unicode u, int be)
{
	byte data[8];
	size_t size;

	if (! encode_utf32(u, be, data, &size)) {
		if (write_filememory(fm, data, size, &size))
			return fmte_("write error.", NULL);
		return 0;
	}
	if (fm->encode.error)
		return fmte_("encode error.", NULL);
	if (! encode_utf32(fm->encode.code, be, data, &size)) {
		if (write_filememory(fm, data, size, &size))
			return fmte_("write error.", NULL);
		return 0;
	}

	return fmte_("encode error.", NULL);
}

static int write_char_utf32le_(filestream fm, unicode u)
{
	return write_char_utf32_(fm, u, 0);
}

static int write_char_utf32be_(filestream fm, unicode u)
{
	return write_char_utf32_(fm, u, 1);
}

#if defined(LISP_ANSIC_WINDOWS)
static int write_char_windows_(filestream fm, unicode u)
{
	char output[8];
	wchar_t input[4];
	byte *ptr, c;
	size_t size;

	/* unicode -> UTF16 */
	if (! encode_utf16b(u, (byte16 *)input, &size))
		goto normal;
	if (fm->encode.error)
		return fmte_("encode error.", NULL);
	if (! encode_utf16b(fm->encode.code, (byte16 *)input, &size))
		goto normal;
	return fmte_("encode error.", NULL);

	/* UTF16 -> ANSI-code-page  */
normal:
	input[size] = 0;
	snprintf(output, 8, "%S", input);
	if (output[0] == 0)
		return fmte_("encode error.", NULL);
	for (ptr = (byte *)output; ; ptr++) {
		c = *ptr;
		if (c == 0)
			break;
		if (putc_filememory(fm, c))
			return fmte_("putc error.", NULL);
	}

	return 0;
}

#elif (defined LISP_WINDOWS)
static int write_char_windows_(filestream fm, unicode u)
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
		return fmte_("encode error.", NULL);
	if (! encode_utf16b(fm->encode.code, (byte16 *)input, &size))
		goto normal;
	return fmte_("encode error.", NULL);

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
		return fmte_("encode error.", NULL);
	ptr = (byte *)output;
	for (i = 0; i < result; i++) {
		if (putc_filememory(fm, ptr[i]))
			return fmte_("putc error.", NULL);
	}

	return 0;
}

#else
static int write_char_windows_(filestream fm, unicode u)
{
	return fmte_("This implementation cannot write a windows encode.", NULL);
}
#endif

static void init_encode_write_char(void)
{
	write_char_call[EncodeType_binary] = write_char_binary_;
	write_char_call[EncodeType_ascii] = write_char_ascii_;
	write_char_call[EncodeType_utf8] = write_char_utf8_;
	write_char_call[EncodeType_utf16le] = write_char_utf16le_;
	write_char_call[EncodeType_utf16be] = write_char_utf16be_;
	write_char_call[EncodeType_utf32le] = write_char_utf32le_;
	write_char_call[EncodeType_utf32be] = write_char_utf32be_;
	write_char_call[EncodeType_windows] = write_char_windows_;
}

int write_char_encode_(filestream fm, unicode u)
{
	return (write_char_call[(int)fm->encode.type])(fm, u);
}


/*
 *  length-char
 */
static int length_char_binary(filestream fm, unicode c)
{
	return -1;
}

static int length_char_ascii(filestream fm, unicode c)
{
	return (c < 0x80)? 1: -1;
}

static int length_unicode_utf8(unicode c, int *ret)
{
	if (c < 0x80)       { *ret = 1; return 0; }
	if (c < 0x0800)     { *ret = 2; return 0; }
	if (c < 0xD800)     { *ret = 3; return 0; }
	if (c < 0xE000)     { *ret = 0; return 1; } /* error */
	if (c < 0x010000)   { *ret = 3; return 0; }
#ifdef LISP_UTF8_SEQ5CHECK
	if (c < UnicodeCount) { *ret = 4; return 0; }
#else
	if (c < 0x200000)   { *ret = 4; return 0; }
	if (c < 0x04000000) { *ret = 5; return 0; }
	if (c < 0x80000000) { *ret = 6; return 0; }
#endif
	*ret = 0;
	return 1;
}

static int length_char_utf8(filestream fm, unicode c)
{
	int size;
	return length_unicode_utf8(c, &size)? -1: size;
}

static int length_unicode_utf16(unicode c, int *ret)
{
	if (c < 0xD800) { *ret = 1; return 0; }
	if (c < 0xE000) { *ret = 0; return 1; } /* surrogate pair */
	if (c < 0x010000) { *ret = 1; return 0; }
	if (c < UnicodeCount) { *ret = 2; return 0; }
	return 1;
}

static int length_char_utf16(filestream fm, unicode c)
{
	int size;
	return length_unicode_utf16(c, &size)? -1: (size * 2);
}

static int length_char_utf32(filestream fm, unicode c)
{
	return 4;
}

#if defined(LISP_ANSIC_WINDOWS)
static int length_char_windows(filestream fm, unicode c)
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
static int length_char_windows(filestream fm, unicode c)
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
static int length_char_windows(filestream fm, unicode c)
{
	/* return fmte_("This implementation cannot use a windows encode.", NULL); */
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

static int length_char_operator(filestream fm, unicode c)
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

int length_char_encode(filestream fm, unicode c)
{
	int check;

	check = length_char_operator(fm, c);
	if (check < 0 && fm->encode.error == 0)
		return length_char_operator(fm, fm->encode.code);
	else
		return check;
}

int length_string_encode_(filestream fm, addr pos, size_t *rsize, int *ret)
{
	int check;
	unicode c;
	size_t i, size, count;

	string_length(pos, &size);
	for (count = i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		check = length_char_encode(fm, c);
		if (check < 0)
			return Result(ret, 1);
		count += (size_t)check;
	}
	*rsize = count;

	return Result(ret, 0);
}


/*
 *  unicode buffer
 */
int UTF32_length_utf8(const unicode *ptr, size_t size, size_t *ret)
{
	int check;
	unicode c;
	size_t i, w;

	w = 0;
	for (i = 0; i < size; i++) {
		c = ptr[i];
		if (length_unicode_utf8(c, &check))
			return 1;
		w += (size_t)check;
	}
	*ret = w;

	return 0;
}

int UTF8_length_clang_(addr pos, size_t *rsize, int *ret)
{
	int check;
	unicode c;
	size_t size, i, w;

	string_length(pos, &size);
	w = 0;
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		if (length_unicode_utf8(c, &check)) {
			*rsize = 0;
			return Result(ret, 1);
		}
		w += (size_t)check;
	}
	*rsize = w;
	return Result(ret, 0);
}

int UTF32_length_utf16(const unicode *ptr, size_t size, size_t *ret)
{
	int check;
	unicode c;
	size_t i, w;

	w = 0;
	for (i = 0; i < size; i++) {
		c = ptr[i];
		if (length_unicode_utf16(c, &check))
			return 1;
		w += (size_t)check;
	}
	*ret = w;

	return 0;
}

int UTF16_length_clang_(addr pos, size_t *rsize, int *ret)
{
	int check;
	unicode c;
	size_t size, i, w;

	string_length(pos, &size);
	w = 0;
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		if (length_unicode_utf16(c, &check)) {
			*rsize = 0;
			return Result(ret, 1);
		}
		w += (size_t)check;
	}
	*rsize = w;
	return Result(ret, 0);
}

static int UTF8_make_(byte *dst, addr pos, int *ret)
{
	size_t size, i, w, check;
	unicode u;

	string_length(pos, &size);
	w = 0;
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &u));
		if (encode_utf8(u, dst + w, &check))
			return Result(ret, 1);
		w += check;
	}
	dst[w] = 0;

	return Result(ret, 0);
}

static int UTF16_make_(byte16 *dst, addr pos, int *ret)
{
	size_t size, i, w, check;
	unicode u;

	string_length(pos, &size);
	w = 0;
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &u));
		if (encode_utf16b(u, dst + w, &check))
			return Result(ret, 1);
		w += check;
	}
	dst[w++] = 0;
	dst[w] = 0;

	return Result(ret, 0);
}

int UTF8_buffer_clang_(LocalRoot local, addr *ret, addr string)
{
	int check;
	addr pos, body;
	size_t size;

	Return(UTF8_length_clang_(string, &size, &check));
	if (check)
		return Result(ret, Unbound);
	local_body(local, &pos, LISPSYSTEM_UNICODE, size + 1);
	posbody(pos, &body);
	Return(UTF8_make_((byte *)body, string, &check));
	if (check)
		return Result(ret, Unbound);

	return Result(ret, pos);
}

int UTF16_buffer_clang_(LocalRoot local, addr *ret, addr string)
{
	int check;
	addr pos, body;
	size_t size;

	Return(UTF16_length_clang_(string, &size, &check));
	if (check)
		return Result(ret, Unbound);
	local_body(local, &pos, LISPSYSTEM_UNICODE, (size + 1) * 2);
	posbody(pos, &body);
	Return(UTF16_make_((byte16 *)body, string, &check));
	if (check)
		return Result(ret, Unbound);

	return Result(ret, pos);
}


/*
 *  unicode string
 */
int UTF8_null_strlen(const byte *src, size_t *ret)
{
	return read_utf8_buffer(NULL, src, 0, ret);
}

int UTF8_size_strlen(const byte *src, size_t size, size_t *ret)
{
	if (size == 0) {
		*ret = 0;
		return 0;
	}

	return read_utf8_buffer(NULL, src, size, ret);
}

int UTF8_null_makeunicode(unicode *dst, const byte *src)
{
	return read_utf8_buffer(dst, src, 0, NULL);
}

int UTF8_size_makeunicode(unicode *dst, const byte *src, size_t size)
{
	if (size == 0)
		return 0;

	return read_utf8_buffer(dst, src, size, NULL);
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

int UTF32_null_strlen(const unicode *src, size_t *ret)
{
	unicode c;
	size_t i;

	for (i = 0; ; i++) {
		c = src[i];
		if (c == 0)
			break;
		if (UnicodeCount <= c)
			return 1;
		if (isSurrogatePair(c))
			return 1;
	}

	*ret = i;
	return 0;
}

int UTF32_size_strlen(const unicode *src, size_t size, size_t *ret)
{
	unicode c;
	size_t i;

	for (i = 0; i < size; i++) {
		c = src[i];
		if (UnicodeCount <= c)
			return 1;
		if (isSurrogatePair(c))
			return 1;
	}

	*ret = size;
	return 0;
}

int UTF32_null_makeunicode(unicode *dst, const unicode *src)
{
	unicode c;
	size_t i;

	for (i = 0; ; i++) {
		c = src[i];
		if (c == 0)
			break;
		if (UnicodeCount <= c)
			return 1;
		if (isSurrogatePair(c))
			return 1;
		dst[i] = c;
	}

	return 0;
}

int UTF32_size_makeunicode(unicode *dst, const unicode *src, size_t size)
{
	unicode c;
	size_t i;

	for (i = 0; ; i++) {
		c = src[i];
		if (c == 0)
			break;
		if (UnicodeCount <= c)
			return 1;
		if (isSurrogatePair(c))
			return 1;
		dst[i] = c;
	}

	return 0;
}

int UTF32_make_utf8(byte *dst, const unicode *src, size_t size)
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

int UTF32_make_utf16(byte16 *dst, const unicode *src, size_t size)
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
void init_encode(void)
{
	init_encode_read_char();
	init_encode_read_hang();
	init_encode_write_char();
	init_encode_length_char();
}

