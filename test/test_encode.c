#include "encode.c"
#include "character.h"
#include "clos.h"
#include "common.h"
#include "constant.h"
#include "degrade.h"
#include "file.h"
#include "file_memory.h"
#include "reader.h"
#include "package.h"
#include "pathname.h"
#include "stream.h"
#include "strvect.h"
#include "symbol.h"
#include "syscall.h"
#include "type.h"
#include "type_table.h"

#define TESTFILE "_debug.txt"

/*
 *  Byte Order Mark
 */
static FILE *test_fopen_output(void)
{
	FILE *file;

	file = fopen(TESTFILE, "wb");
	if (file == NULL)
		fmte("File open error.", NULL);
	return file;
}

static void test_fopen_output_binary(const void *ptr, size_t size)
{
	FILE *file;
	size_t i;
	const unsigned char *byte;

	file = test_fopen_output();
	byte = (const unsigned char *)ptr;
	for (i = 0; i < size; i++) {
		if (fputc(byte[i], file) == EOF)
			fmte("Invalid file stream.", NULL);
	}
	fclose(file);
}

static void test_open_input_filememory(struct filememory *fm)
{
	int result;
	addr name;

	strvect_char_heap(&name, TESTFILE);
	result = open_input_filememory(Local_Thread, fm, name);
	if (result)
		fmte("File open error.", NULL);
}

static int test_readbom8_encode(void)
{
	/* EF BB BF */
	byte c;
	int check;
	struct filememory fm;

	/* Invalid (-1) */
	test_fopen_output_binary("", 0);
	test_open_input_filememory(&fm);
	close_filememory(&fm);
	check = readbom8_encode(&fm);
	test(check < 0, "readbom8_encode1");

	/* EOF (0) */
	test_fopen_output_binary("", 0);
	test_open_input_filememory(&fm);
	check = readbom8_encode(&fm);
	test(check == 0, "readbom8_encode2");
	check = getc_filememory(&fm, &c);
	test(check, "readbom8_encode3");
	close_filememory(&fm);

	/* No BOM (0) */
	test_fopen_output_binary("ABC", 3);
	test_open_input_filememory(&fm);
	check = readbom8_encode(&fm);
	test(check == 0, "readbom8_encode4");
	check = getc_filememory(&fm, &c);
	test(check == 0, "readbom8_encode5");
	test(c == 'A', "readbom8_encode6");
	close_filememory(&fm);

	/* Invalid (-1): EF */
	test_fopen_output_binary("\xEF", 1);
	test_open_input_filememory(&fm);
	check = readbom8_encode(&fm);
	test(check < 0, "readbom8_encode7");
	close_filememory(&fm);

	/* Invalid (-1): EF ?? */
	test_fopen_output_binary("\xEFxyz", 4);
	test_open_input_filememory(&fm);
	check = readbom8_encode(&fm);
	test(check < 0, "readbom8_encode8");
	close_filememory(&fm);

	/* Invalid (-1): EF BB */
	test_fopen_output_binary("\xEF\xBB", 2);
	test_open_input_filememory(&fm);
	check = readbom8_encode(&fm);
	test(check < 0, "readbom8_encode9");
	close_filememory(&fm);

	/* Invalid (-1): EF BB ?? */
	test_fopen_output_binary("\xEF\xBBz", 3);
	test_open_input_filememory(&fm);
	check = readbom8_encode(&fm);
	test(check < 0, "readbom8_encode10");
	close_filememory(&fm);

	/* Invalid (-1): EF BB BF */
	test_fopen_output_binary("\xEF\xBB\xBF", 3);
	test_open_input_filememory(&fm);
	check = readbom8_encode(&fm);
	test(check, "readbom8_encode11");
	check = getc_filememory(&fm, &c);
	test(check, "readbom8_encode12");
	close_filememory(&fm);

	/* Invalid (-1): EF BB BF abc */
	test_fopen_output_binary("\xEF\xBB\xBFxyz", 6);
	test_open_input_filememory(&fm);
	check = readbom8_encode(&fm);
	test(check, "readbom8_encode13");
	check = getc_filememory(&fm, &c);
	test(check == 0, "readbom8_encode14");
	test(c == 'x', "readbom8_encode15");
	close_filememory(&fm);

	RETURN;
}

static int test_readbom16_encode(void)
{
	/*    big-endian(0): default */
	/*    big-endian(2): FE FF   */
	/* little-endian(1): FF FE   */
	byte c;
	int check;
	struct filememory fm;

	/* Invalid(-1) */
	test_fopen_output_binary("", 0);
	test_open_input_filememory(&fm);
	close_filememory(&fm);
	check = readbom16_encode(&fm);
	test(check < 0, "readbom16_encode1");

	/* empty(0) */
	test_fopen_output_binary("", 0);
	test_open_input_filememory(&fm);
	check = readbom16_encode(&fm);
	test(check == 0, "readbom16_encode2");
	close_filememory(&fm);

	/*    big-endian(0): default */
	test_fopen_output_binary("abcd", 3);
	test_open_input_filememory(&fm);
	check = readbom16_encode(&fm);
	test(check == 0, "readbom16_encode3");
	check = getc_filememory(&fm, &c);
	test(check == 0, "readbom16_encode4");
	test(c == 'a', "readbom16_encode5");
	close_filememory(&fm);

	/* Invalid(-1): a */
	test_fopen_output_binary("a", 1);
	test_open_input_filememory(&fm);
	check = readbom16_encode(&fm);
	test(check < 0, "readbom16_encode6");
	close_filememory(&fm);

	/*    big-endian(0): default */
	test_fopen_output_binary("ab", 2);
	test_open_input_filememory(&fm);
	check = readbom16_encode(&fm);
	test(check == 0, "readbom16_encode7");
	check = getc_filememory(&fm, &c);
	test(check == 0, "readbom16_encode8");
	test(c == 'a', "readbom16_encode9");
	check = getc_filememory(&fm, &c);
	test(check == 0, "readbom16_encode10");
	test(c == 'b', "readbom16_encode11");
	check = getc_filememory(&fm, &c);
	test(check, "readbom16_encode12");

	/*   big-endian(2): FE FF   */
	test_fopen_output_binary("\xFE\xFFxy", 4);
	test_open_input_filememory(&fm);
	check = readbom16_encode(&fm);
	test(check == 2, "readbom16_encode13");
	check = getc_filememory(&fm, &c);
	test(check == 0, "readbom16_encode14");
	test(c == 'x', "readbom16_encode15");
	check = getc_filememory(&fm, &c);
	test(check == 0, "readbom16_encode16");
	test(c == 'y', "readbom16_encode17");
	check = getc_filememory(&fm, &c);
	test(check, "readbom16_encode18");
	close_filememory(&fm);

	/* little-endian(1): FF FE   */
	test_fopen_output_binary("\xFF\xFExy", 4);
	test_open_input_filememory(&fm);
	check = readbom16_encode(&fm);
	test(check == 1, "readbom16_encode19");
	check = getc_filememory(&fm, &c);
	test(check == 0, "readbom16_encode20");
	test(c == 'x', "readbom16_encode21");
	check = getc_filememory(&fm, &c);
	test(check == 0, "readbom16_encode22");
	test(c == 'y', "readbom16_encode23");
	check = getc_filememory(&fm, &c);
	test(check, "readbom16_encode24");
	close_filememory(&fm);

	RETURN;
}

static int test_readbom32_encode(void)
{
	/*    big-endian(0): default */
	/*    big-endian(2): 00 00 FE FF */
	/* little-endian(1): FF FE 00 00 */
	byte c;
	int check;
	struct filememory fm;

	/* Invalid(-1) */
	test_fopen_output_binary("", 0);
	test_open_input_filememory(&fm);
	close_filememory(&fm);
	check = readbom32_encode(&fm);
	test(check < 0, "readbom32_encode1");

	/* empty(0) */
	test_fopen_output_binary("", 0);
	test_open_input_filememory(&fm);
	check = readbom32_encode(&fm);
	test(check == 0, "readbom32_encode2");
	close_filememory(&fm);

	/*   big-endian(0): default */
	test_fopen_output_binary("abcd", 4);
	test_open_input_filememory(&fm);
	check = readbom32_encode(&fm);
	test(check == 0, "readbom32_encode3");
	check = getc_filememory(&fm, &c);
	test(check == 0, "readbom32_encode4");
	test(c == 'a', "readbom32_encode5");
	close_filememory(&fm);

	/*   Invalid(-1): a */
	test_fopen_output_binary("a", 1);
	test_open_input_filememory(&fm);
	check = readbom32_encode(&fm);
	test(check < 0, "readbom32_encode6");
	close_filememory(&fm);

	/*   Invalid(-1): ab */
	test_fopen_output_binary("ab", 2);
	test_open_input_filememory(&fm);
	check = readbom32_encode(&fm);
	test(check < 0, "readbom32_encode7");
	close_filememory(&fm);

	/*   Invalid(-1): abc */
	test_fopen_output_binary("abc", 3);
	test_open_input_filememory(&fm);
	check = readbom32_encode(&fm);
	test(check < 0, "readbom32_encode8");
	close_filememory(&fm);

	/*   big-endian(0): default */
	test_fopen_output_binary("abcd", 4);
	test_open_input_filememory(&fm);
	check = readbom32_encode(&fm);
	test(check == 0, "readbom32_encode9");
	check = getc_filememory(&fm, &c);
	test(check == 0, "readbom32_encode10");
	test(c == 'a', "readbom32_encode11");
	check = getc_filememory(&fm, &c);
	test(check == 0, "readbom32_encode12");
	test(c == 'b', "readbom32_encode13");
	check = getc_filememory(&fm, &c);
	test(check == 0, "readbom32_encode14");
	test(c == 'c', "readbom32_encode15");
	check = getc_filememory(&fm, &c);
	test(check == 0, "readbom32_encode16");
	test(c == 'd', "readbom32_encode17");
	check = getc_filememory(&fm, &c);
	test(check, "readbom32_encode18");

	/*    big-endian(2): 00 00 FE FF */
	test_fopen_output_binary("\x00\x00\xFE\xFFxyzw", 8);
	test_open_input_filememory(&fm);
	check = readbom32_encode(&fm);
	test(check == 2, "readbom32_encode19");
	check = getc_filememory(&fm, &c);
	test(check == 0, "readbom32_encode20");
	test(c == 'x', "readbom32_encode21");
	check = getc_filememory(&fm, &c);
	test(check == 0, "readbom32_encode22");
	test(c == 'y', "readbom32_encode23");
	close_filememory(&fm);

	/* little-endian(1): FF FE 00 00 */
	test_fopen_output_binary("\xFF\xFE\x00\x00xy", 8);
	test_open_input_filememory(&fm);
	check = readbom32_encode(&fm);
	test(check == 1, "readbom32_encode24");
	check = getc_filememory(&fm, &c);
	test(check == 0, "readbom32_encode25");
	test(c == 'x', "readbom32_encode26");
	check = getc_filememory(&fm, &c);
	test(check == 0, "readbom32_encode27");
	test(c == 'y', "readbom32_encode28");
	close_filememory(&fm);

	RETURN;
}

static void test_open_output_filememory(struct filememory *fm)
{
	int result;
	addr name;

	strvect_char_heap(&name, TESTFILE);
	result = open_output_filememory(Local_Thread, fm, name, FileOutput_supersede);
	if (result)
		fmte("File open error.", NULL);
}

static void test_fopen_input_binary(void *ptr, size_t size, size_t *ret)
{
	FILE *file;

	file = fopen(TESTFILE, "rb");
	if (file == NULL)
		fmte("File open error.", NULL);
	*ret = fread(ptr, 1, size, file);
	fclose(file);
}

static int test_writebom8_encode(void)
{
	byte data[256];
	int check;
	size_t size;
	struct filememory fm;

	test_open_output_filememory(&fm);
	fm.encode.type = EncodeType_utf8;
	check = writebom_encode(&fm);
	test(check == 0, "writebom8_encode1");
	check = putc_filememory(&fm, 'A');
	test(check == 0, "writebom8_encode2");
	close_filememory(&fm);
	test_fopen_input_binary(data, 256, &size);
	test(size == 4, "writebom8_encode3");
	test(memcmp(data, "\xEF\xBB\xBF" "A", 4) == 0, "writebom8_encode4");

	RETURN;
}

static int test_writebom16_encode(void)
{
	byte data[256];
	int check;
	size_t size;
	struct filememory fm;

	/* big-endian */
	test_open_output_filememory(&fm);
	fm.encode.type = EncodeType_utf16be;
	check = writebom_encode(&fm);
	test(check == 0, "writebom16_encode1");
	check = putc_filememory(&fm, 'A');
	test(check == 0, "writebom16_encode2");
	close_filememory(&fm);
	test_fopen_input_binary(data, 256, &size);
	test(size == 3, "writebom16_encode3");
	test(memcmp(data, "\xFE\xFF" "A", 3) == 0, "writebom16_encode4");

	/* little-endian */
	test_open_output_filememory(&fm);
	fm.encode.type = EncodeType_utf16le;
	check = writebom_encode(&fm);
	test(check == 0, "writebom16_encode5");
	check = putc_filememory(&fm, 'Z');
	test(check == 0, "writebom16_encode6");
	close_filememory(&fm);
	test_fopen_input_binary(data, 256, &size);
	test(size == 3, "writebom16_encode7");
	test(memcmp(data, "\xFF\xFE" "Z", 3) == 0, "writebom16_encode8");

	RETURN;
}

static int test_writebom32_encode(void)
{
	byte data[256];
	int check;
	size_t size;
	struct filememory fm;

	/* big-endian */
	test_open_output_filememory(&fm);
	fm.encode.type = EncodeType_utf32be;
	check = writebom_encode(&fm);
	test(check == 0, "writebom32_encode1");
	check = putc_filememory(&fm, 'A');
	test(check == 0, "writebom32_encode2");
	close_filememory(&fm);
	test_fopen_input_binary(data, 256, &size);
	test(size == 5, "writebom32_encode3");
	test(memcmp(data, "\x00\x00\xFE\xFF" "A", 5) == 0, "writebom32_encode4");

	/* little-endian */
	test_open_output_filememory(&fm);
	fm.encode.type = EncodeType_utf32le;
	check = writebom_encode(&fm);
	test(check == 0, "writebom32_encode5");
	check = putc_filememory(&fm, 'Z');
	test(check == 0, "writebom32_encode6");
	close_filememory(&fm);
	test_fopen_input_binary(data, 256, &size);
	test(size == 5, "writebom32_encode7");
	test(memcmp(data, "\xFF\xFE\x00\x00" "Z", 5) == 0, "writebom32_encode8");

	RETURN;
}


/*
 *  read-char
 */
static int test_read_char_ascii(void)
{
	unicode c;
	int check;
	struct filememory fm;

	test_fopen_output_binary("x\x7F\x80\x81", 4);
	test_open_input_filememory(&fm);
	fm.encode.type = EncodeType_ascii;
	fm.encode.code = 999;
	fm.encode.error = 0;
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_ascii1");
	test(c == 'x', "read_char_ascii2");

	fm.encode.error = 1;
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_ascii3");
	test(c == 0x7F, "read_char_ascii4");

	fm.encode.error = 0;
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_ascii5");
	test(c == 999, "read_char_ascii6");

	fm.encode.error = 1;
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_ascii7");

	close_filememory(&fm);

	RETURN;
}

static void test_read_char_utf8_file(struct filememory *fm,
		const char *str, size_t size)
{
	test_fopen_output_binary(str, size);
	test_open_input_filememory(fm);
	fm->encode.type = EncodeType_utf8;
	fm->encode.code = 999;
	fm->encode.error = 1;
}

static int test_read_char_utf8(void)
{
	unicode c;
	int check;
	struct filememory fm;

	/* 1 byte */
	test_read_char_utf8_file(&fm, "abc", 1);
	close_filememory(&fm);
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_utf8-1");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "", 0);
	check = read_char_encode(&fm, &c);
	test(check == 1, "read_char_utf8-2");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\x00", 1);
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_utf8-3");
	test(c == 0, "read_char_utf8-4");
	check = read_char_encode(&fm, &c);
	test(0 < check, "read_char_utf8-5");
	close_filememory(&fm);

	/* 2 byte */
	test_read_char_utf8_file(&fm, "\x01\x7F\xC2\x80\xC2\xBF\xDF\xBF", 8);
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_utf8-6");
	test(c == 0x01, "read_char_utf8-7");
	check = read_char_encode(&fm, &c);
	test(c == 0x7F, "read_char_utf8-8");
	check = read_char_encode(&fm, &c);
	test(c == 0x80, "read_char_utf8-9");
	check = read_char_encode(&fm, &c);
	test(c == 0xBF, "read_char_utf8-10");
	check = read_char_encode(&fm, &c);
	test(c == 0x07FF, "read_char_utf8-11");
	check = read_char_encode(&fm, &c);
	test(0 < check, "read_char_utf8-12");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xC2\xC0", 2);
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_utf8-13");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xC2\xC0", 2);
	fm.encode.error = 0;
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_utf8-14");
	test(c == 999, "read_char_utf8-15");
	close_filememory(&fm);

	/* 3 byte */
	test_read_char_utf8_file(&fm, "\xE0\xA0\x80\xEF\xBF\xBF", 6);
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_utf8-16");
	test(c == 0x0800, "read_char_utf8-17");
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_utf8-18");
	test(c == 0xFFFF, "read_char_utf8-19");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xE0\x9F\x80\x40", 4);
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_utf8-20");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xE1\x80\x80\x40", 4);
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_utf8-21");
	test(c == 0x1000, "read_char_utf8-22");
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_utf8-23");
	test(c == 0x40, "read_char_utf8-24");
	check = read_char_encode(&fm, &c);
	test(0 < check, "read_char_utf8-25");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xE1\x7F\x80\x40", 4);
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_utf8-26");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xE1\xC0\x80\x40", 4);
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_utf8-27");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xE1\x80\x7F\x40", 4);
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_utf8-28");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xE1\x80\xC0\x40", 4);
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_utf8-29");
	close_filememory(&fm);

	/* 4 byte */
	test_read_char_utf8_file(&fm, "\xF0\x90\x80\x80\x40", 5);
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_utf8-30");
	test(c == 0x010000, "read_char_utf8-31");
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_utf8-32");
	test(c == 0x40, "read_char_utf8-33");
	check = read_char_encode(&fm, &c);
	test(0 < check, "read_char_utf8-34");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xF4\x8F\xBF\xBF\x7F", 5);
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_utf8-35");
	test(c == 0x10FFFF, "read_char_utf8-36");
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_utf8-37");
	test(c == 0x7F, "read_char_utf8-38");
	check = read_char_encode(&fm, &c);
	test(0 < check, "read_char_utf8-39");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xF0\x8F\x80\x80\x40", 5);
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_utf8-40");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xF1\x8F\x80\x80\x40", 5);
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_utf8-41");
	test(c == 0x04F000, "read_char_utf8-42");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xF1\x7F\x80\x80\x40", 5);
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_utf8-43");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xF1\xC0\x80\x80\x40", 5);
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_utf8-44");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xF1\x80\x7F\x80\x40", 5);
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_utf8-45");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xF1\x80\xC0\x80\x40", 5);
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_utf8-46");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xF1\x80\x80\x7F\x40", 5);
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_utf8-47");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xF1\x80\x80\xC0\x40", 5);
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_utf8-48");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xF8\xBF\x80\x80\x80\x40", 6);
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_utf8-49");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xFC\xBF\x80\x80\x80\x80\x40", 7);
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_utf8-50");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xFE\xBF\x80\x80\x80\x80\x40", 7);
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_utf8-51");
	close_filememory(&fm);

	/* surrogate pair 0xD800 - 0xDFFF */
	test_read_char_utf8_file(&fm, "\xED\x9F\xBF\xED\xA0\x80\x40", 7);
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_utf8-52");
	test(c == 0xD7FF, "read_char_utf8-53");
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_utf8-54");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xEE\x80\x80\xED\xBF\xBF\x40", 7);
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_utf8-55");
	test(c == 0xE000, "read_char_utf8-56");
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_utf8-57");
	close_filememory(&fm);

	/* Unicode range 0x000000 - 0x10FFFF */
	test_read_char_utf8_file(&fm, "\xF4\x8F\xBF\xBF\x7F", 5);
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_utf8-58");
	test(c == 0x10FFFF, "read_char_utf8-59");
	close_filememory(&fm);
	test_read_char_utf8_file(&fm, "\xF4\x90\x80\x80\x7F", 5);
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_utf8-60");
	close_filememory(&fm);

	RETURN;
}

static void test_read_char_utf16le_file(struct filememory *fm,
		const char *str, size_t size)
{
	test_fopen_output_binary(str, size);
	test_open_input_filememory(fm);
	fm->encode.type = EncodeType_utf16le;
	fm->encode.code = 999;
	fm->encode.error = 1;
}

static int test_read_char_utf16le(void)
{
	unicode c;
	int check;
	struct filememory fm;

	/* 1 byte */
	test_read_char_utf16le_file(&fm, "abc", 1);
	close_filememory(&fm);
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_utf16le1");
	close_filememory(&fm);

	test_read_char_utf16le_file(&fm, "", 0);
	check = read_char_encode(&fm, &c);
	test(0 < check, "read_char_utf16le2");
	close_filememory(&fm);

	test_read_char_utf16le_file(&fm, "\x40\x00\x30\x00", 4);
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_utf16le3");
	test(c == 0x40, "read_char_utf16le4");
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_utf16le5");
	test(c == 0x30, "read_char_utf16le6");
	check = read_char_encode(&fm, &c);
	test(0 < check, "read_char_utf16le7");
	close_filememory(&fm);

	test_read_char_utf16le_file(&fm, "\x00\x00\xFF\xD7\x00\xE0\xFF\xFF", 8);
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_utf16le8");
	test(c == 0x0000, "read_char_utf16le9");
	check = read_char_encode(&fm, &c);
	test(c == 0xD7FF, "read_char_utf16le10");
	check = read_char_encode(&fm, &c);
	test(c == 0xE000, "read_char_utf16le11");
	check = read_char_encode(&fm, &c);
	test(c == 0xFFFF, "read_char_utf16le12");
	check = read_char_encode(&fm, &c);
	test(0 < check, "read_char_utf16le13");
	close_filememory(&fm);

	test_read_char_utf16le_file(&fm, "\x00", 1);
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_utf16le14");
	close_filememory(&fm);

	test_read_char_utf16le_file(&fm, "\x00\xD8", 2);
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_utf16le15");
	close_filememory(&fm);

	test_read_char_utf16le_file(&fm, "\x00\xD8\x00\xDC\xFF\xDB\xFF\xDF", 8);
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_utf16le15");
	test(c == 0x010000, "read_char_utf16le16");
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_utf16le17");
	test(c == 0x10FFFF, "read_char_utf16le18");
	close_filememory(&fm);

	test_read_char_utf16le_file(&fm, "\xFF\xD7\x00\xE0", 4);
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_utf16le19");
	test(c == 0xD7FF, "read_char_utf16le20");
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_utf16le21");
	test(c == 0xE000, "read_char_utf16le22");
	close_filememory(&fm);

	test_read_char_utf16le_file(&fm, "\xFF\xDB\x00\xDC", 4);
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_utf16le23");
	test(c == 0x10FC00, "read_char_utf16le24");
	close_filememory(&fm);

	test_read_char_utf16le_file(&fm, "\x00\xDC\x00\xDC", 4);
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_utf16le25");
	close_filememory(&fm);

	test_read_char_utf16le_file(&fm, "\x00\xD8\xFF\xDB\x40\x00", 6);
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_utf16le26");
	close_filememory(&fm);

	test_read_char_utf16le_file(&fm, "\x00\xD8\x00\xDC\x40\x00", 6);
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_utf16le27");
	test(c == 0x010000, "read_char_utf16le28");
	check = read_char_encode(&fm, &c);
	test(c == 0x40, "read_char_utf16le29");
	close_filememory(&fm);

	test_read_char_utf16le_file(&fm, "\x00\xD8\xFF\xDF\x40\x00", 6);
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_utf16le30");
	test(c == 0x0103FF, "read_char_utf16le31");
	check = read_char_encode(&fm, &c);
	test(c == 0x40, "read_char_utf16le32");
	close_filememory(&fm);

	test_read_char_utf16le_file(&fm, "\x00\xD8\x00\xE0\x40\x00", 6);
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_utf16le33");
	close_filememory(&fm);

	RETURN;
}

static void test_read_char_utf16be_file(struct filememory *fm,
		const char *str, size_t size)
{
	test_fopen_output_binary(str, size);
	test_open_input_filememory(fm);
	fm->encode.type = EncodeType_utf16be;
	fm->encode.code = 999;
	fm->encode.error = 1;
}

static int test_read_char_utf16be(void)
{
	unicode c;
	int check;
	struct filememory fm;

	/* 1 byte */
	test_read_char_utf16be_file(&fm, "abc", 1);
	close_filememory(&fm);
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_utf16be1");
	close_filememory(&fm);

	test_read_char_utf16be_file(&fm, "", 0);
	check = read_char_encode(&fm, &c);
	test(0 < check, "read_char_utf16be2");
	close_filememory(&fm);

	test_read_char_utf16be_file(&fm, "\x00\x40\x00\x30", 4);
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_utf16be3");
	test(c == 0x40, "read_char_utf16be4");
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_utf16be5");
	test(c == 0x30, "read_char_utf16be6");
	check = read_char_encode(&fm, &c);
	test(0 < check, "read_char_utf16be7");
	close_filememory(&fm);

	test_read_char_utf16be_file(&fm, "\x00\x00\xD7\xFF\xE0\x00\xFF\xFF", 8);
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_utf16be8");
	test(c == 0x0000, "read_char_utf16be9");
	check = read_char_encode(&fm, &c);
	test(c == 0xD7FF, "read_char_utf16be10");
	check = read_char_encode(&fm, &c);
	test(c == 0xE000, "read_char_utf16be11");
	check = read_char_encode(&fm, &c);
	test(c == 0xFFFF, "read_char_utf16be12");
	check = read_char_encode(&fm, &c);
	test(0 < check, "read_char_utf16be13");
	close_filememory(&fm);

	test_read_char_utf16be_file(&fm, "\x00", 1);
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_utf16be14");
	close_filememory(&fm);

	test_read_char_utf16be_file(&fm, "\xD8\x00", 2);
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_utf16be15");
	close_filememory(&fm);

	test_read_char_utf16be_file(&fm, "\xD8\x00\xDC\x00\xDB\xFF\xDF\xFF", 8);
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_utf16be15");
	test(c == 0x010000, "read_char_utf16be16");
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_utf16be17");
	test(c == 0x10FFFF, "read_char_utf16be18");
	close_filememory(&fm);

	test_read_char_utf16be_file(&fm, "\xD7\xFF\xE0\x00", 4);
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_utf16be19");
	test(c == 0xD7FF, "read_char_utf16be20");
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_utf16be21");
	test(c == 0xE000, "read_char_utf16be22");
	close_filememory(&fm);

	test_read_char_utf16be_file(&fm, "\xDB\xFF\xDC\x00", 4);
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_utf16be23");
	test(c == 0x10FC00, "read_char_utf16be24");
	close_filememory(&fm);

	test_read_char_utf16be_file(&fm, "\xDC\x00\xDC\x00", 4);
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_utf16be25");
	close_filememory(&fm);

	test_read_char_utf16be_file(&fm, "\xD8\x00\xDB\xFF\x00\x40", 6);
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_utf16be26");
	close_filememory(&fm);

	test_read_char_utf16be_file(&fm, "\xD8\x00\xDC\x00\x00\x40", 6);
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_utf16be27");
	test(c == 0x010000, "read_char_utf16be28");
	check = read_char_encode(&fm, &c);
	test(c == 0x40, "read_char_utf16be29");
	close_filememory(&fm);

	test_read_char_utf16be_file(&fm, "\xD8\x00\xDF\xFF\x00\x40", 6);
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_utf16be30");
	test(c == 0x0103FF, "read_char_utf16be31");
	check = read_char_encode(&fm, &c);
	test(c == 0x40, "read_char_utf16be32");
	close_filememory(&fm);

	test_read_char_utf16be_file(&fm, "\xD8\x00\xE0\x00\x00\x40", 6);
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_utf16be33");
	close_filememory(&fm);

	RETURN;
}

static void test_read_char_utf32le_file(struct filememory *fm,
		const char *str, size_t size)
{
	test_fopen_output_binary(str, size);
	test_open_input_filememory(fm);
	fm->encode.type = EncodeType_utf32le;
	fm->encode.code = 999;
	fm->encode.error = 1;
}

static int test_read_char_utf32le(void)
{
	unicode c;
	int check;
	struct filememory fm;

	test_read_char_utf32le_file(&fm, "", 0);
	check = read_char_encode(&fm, &c);
	test(0 < check, "read_char_utf32le1");
	close_filememory(&fm);

	test_read_char_utf32le_file(&fm, "abcdefg", 1);
	close_filememory(&fm);
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_utf32le2");
	close_filememory(&fm);

	test_read_char_utf32le_file(&fm, "abcdefg", 2);
	close_filememory(&fm);
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_utf32le3");
	close_filememory(&fm);

	test_read_char_utf32le_file(&fm, "abcdefg", 3);
	close_filememory(&fm);
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_utf32le4");
	close_filememory(&fm);

	test_read_char_utf32le_file(&fm, "\x00\x00\x00\x00\x80\x00\x00\x00", 8);
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_utf32le5");
	test(c == 0x00, "read_char_utf32le6");
	check = read_char_encode(&fm, &c);
	test(c == 0x80, "read_char_utf32le7");
	close_filememory(&fm);

	test_read_char_utf32le_file(&fm, "\xDE\xBC\x0A\x00\xFF\xFF\x10\x00", 8);
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_utf32le8");
	test(c == 0x0ABCDE, "read_char_utf32le9");
	check = read_char_encode(&fm, &c);
	test(c == 0x10FFFF, "read_char_utf32le10");
	check = read_char_encode(&fm, &c);
	test(0 < check, "read_char_utf32le11");
	close_filememory(&fm);

	test_read_char_utf32le_file(&fm, "\x00\x00\x11\x00", 4);
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_utf32le12");
	close_filememory(&fm);

	RETURN;
}

static void test_read_char_utf32be_file(struct filememory *fm,
		const char *str, size_t size)
{
	test_fopen_output_binary(str, size);
	test_open_input_filememory(fm);
	fm->encode.type = EncodeType_utf32be;
	fm->encode.code = 999;
	fm->encode.error = 1;
}

static int test_read_char_utf32be(void)
{
	unicode c;
	int check;
	struct filememory fm;

	test_read_char_utf32be_file(&fm, "", 0);
	check = read_char_encode(&fm, &c);
	test(0 < check, "read_char_utf32be1");
	close_filememory(&fm);

	test_read_char_utf32be_file(&fm, "abcdefg", 1);
	close_filememory(&fm);
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_utf32be2");
	close_filememory(&fm);

	test_read_char_utf32be_file(&fm, "abcdefg", 2);
	close_filememory(&fm);
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_utf32be3");
	close_filememory(&fm);

	test_read_char_utf32be_file(&fm, "abcdefg", 3);
	close_filememory(&fm);
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_utf32be4");
	close_filememory(&fm);

	test_read_char_utf32be_file(&fm, "\x00\x00\x00\x00\x00\x00\x00\x80", 8);
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_utf32be5");
	test(c == 0x00, "read_char_utf32be6");
	check = read_char_encode(&fm, &c);
	test(c == 0x80, "read_char_utf32be7");
	close_filememory(&fm);

	test_read_char_utf32be_file(&fm, "\x00\x0A\xBC\xDE\x00\x10\xFF\xFF", 8);
	check = read_char_encode(&fm, &c);
	test(check == 0, "read_char_utf32be8");
	test(c == 0x0ABCDE, "read_char_utf32be9");
	check = read_char_encode(&fm, &c);
	test(c == 0x10FFFF, "read_char_utf32be10");
	check = read_char_encode(&fm, &c);
	test(0 < check, "read_char_utf32be11");
	close_filememory(&fm);

	test_read_char_utf32be_file(&fm, "\x00\x11\x00\x00", 4);
	check = read_char_encode(&fm, &c);
	test(check < 0, "read_char_utf32be12");
	close_filememory(&fm);

	RETURN;
}


/*
 *  read-hang
 */
static int test_read_hang_ascii(void)
{
	unicode c;
	int check, hang;
	struct filememory fm;

	test_fopen_output_binary("x\x7F\x80\x81", 4);
	test_open_input_filememory(&fm);
	fm.encode.type = EncodeType_ascii;
	fm.encode.code = 999;
	fm.encode.error = 0;
	hang = 999;
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_ascii1");
	test(c == 'x', "read_hang_ascii2");

	fm.encode.error = 1;
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_ascii1");
	test(c == 0x7F, "read_hang_ascii4");

	fm.encode.error = 0;
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_ascii1");
	test(c == 999, "read_hang_ascii6");

	fm.encode.error = 1;
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_ascii7");

	close_filememory(&fm);

	RETURN;
}

static int test_read_hang_utf8(void)
{
	unicode c;
	int check, hang;
	struct filememory fm;

	/* 1 byte */
	test_read_char_utf8_file(&fm, "abc", 1);
	close_filememory(&fm);
	hang = 999;
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_utf8-1");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "", 0);
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 1, "read_hang_utf8-2");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\x00", 1);
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_utf8-3");
	test(c == 0, "read_hang_utf8-4");
	check = read_hang_encode(&fm, &c, &hang);
	test(0 < check, "read_hang_utf8-5");
	close_filememory(&fm);

	/* 2 byte */
	test_read_char_utf8_file(&fm, "\x01\x7F\xC2\x80\xC2\xBF\xDF\xBF", 8);
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_utf8-6");
	test(c == 0x01, "read_hang_utf8-7");
	check = read_hang_encode(&fm, &c, &hang);
	test(c == 0x7F, "read_hang_utf8-8");
	check = read_hang_encode(&fm, &c, &hang);
	test(c == 0x80, "read_hang_utf8-9");
	check = read_hang_encode(&fm, &c, &hang);
	test(c == 0xBF, "read_hang_utf8-10");
	check = read_hang_encode(&fm, &c, &hang);
	test(c == 0x07FF, "read_hang_utf8-11");
	check = read_hang_encode(&fm, &c, &hang);
	test(0 < check, "read_hang_utf8-12");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xC2\xC0", 2);
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_utf8-13");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xC2\xC0", 2);
	fm.encode.error = 0;
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_utf8-14");
	test(c == 999, "read_hang_utf8-15");
	close_filememory(&fm);

	/* 3 byte */
	test_read_char_utf8_file(&fm, "\xE0\xA0\x80\xEF\xBF\xBF", 6);
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_utf8-16");
	test(c == 0x0800, "read_hang_utf8-17");
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_utf8-18");
	test(c == 0xFFFF, "read_hang_utf8-19");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xE0\x9F\x80\x40", 4);
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_utf8-20");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xE1\x80\x80\x40", 4);
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_utf8-21");
	test(c == 0x1000, "read_hang_utf8-22");
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_utf8-23");
	test(c == 0x40, "read_hang_utf8-24");
	check = read_hang_encode(&fm, &c, &hang);
	test(0 < check, "read_hang_utf8-25");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xE1\x7F\x80\x40", 4);
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_utf8-26");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xE1\xC0\x80\x40", 4);
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_utf8-27");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xE1\x80\x7F\x40", 4);
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_utf8-28");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xE1\x80\xC0\x40", 4);
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_utf8-29");
	close_filememory(&fm);

	/* 4 byte */
	test_read_char_utf8_file(&fm, "\xF0\x90\x80\x80\x40", 5);
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_utf8-30");
	test(c == 0x010000, "read_hang_utf8-31");
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_utf8-32");
	test(c == 0x40, "read_hang_utf8-33");
	check = read_hang_encode(&fm, &c, &hang);
	test(0 < check, "read_hang_utf8-34");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xF4\x8F\xBF\xBF\x7F", 5);
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_utf8-35");
	test(c == 0x10FFFF, "read_hang_utf8-36");
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_utf8-37");
	test(c == 0x7F, "read_hang_utf8-38");
	check = read_hang_encode(&fm, &c, &hang);
	test(0 < check, "read_hang_utf8-39");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xF0\x8F\x80\x80\x40", 5);
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_utf8-40");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xF1\x8F\x80\x80\x40", 5);
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_utf8-41");
	test(c == 0x04F000, "read_hang_utf8-42");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xF1\x7F\x80\x80\x40", 5);
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_utf8-43");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xF1\xC0\x80\x80\x40", 5);
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_utf8-44");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xF1\x80\x7F\x80\x40", 5);
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_utf8-45");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xF1\x80\xC0\x80\x40", 5);
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_utf8-46");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xF1\x80\x80\x7F\x40", 5);
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_utf8-47");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xF1\x80\x80\xC0\x40", 5);
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_utf8-48");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xF8\xBF\x80\x80\x80\x40", 6);
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_utf8-49");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xFC\xBF\x80\x80\x80\x80\x40", 7);
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_utf8-50");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xFE\xBF\x80\x80\x80\x80\x40", 7);
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_utf8-51");
	close_filememory(&fm);

	/* surrogate pair 0xD800 - 0xDFFF */
	test_read_char_utf8_file(&fm, "\xED\x9F\xBF\xED\xA0\x80\x40", 7);
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_utf8-52");
	test(c == 0xD7FF, "read_hang_utf8-53");
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_utf8-54");
	close_filememory(&fm);

	test_read_char_utf8_file(&fm, "\xEE\x80\x80\xED\xBF\xBF\x40", 7);
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_utf8-55");
	test(c == 0xE000, "read_hang_utf8-56");
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_utf8-57");
	close_filememory(&fm);

	/* Unicode range 0x000000 - 0x10FFFF */
	test_read_char_utf8_file(&fm, "\xF4\x8F\xBF\xBF\x7F", 5);
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_utf8-58");
	test(c == 0x10FFFF, "read_hang_utf8-59");
	close_filememory(&fm);
	test_read_char_utf8_file(&fm, "\xF4\x90\x80\x80\x7F", 5);
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_utf8-60");
	close_filememory(&fm);

	RETURN;
}

static int test_read_hang_utf16le(void)
{
	unicode c;
	int check, hang;
	struct filememory fm;

	/* 1 byte */
	test_read_char_utf16le_file(&fm, "abc", 1);
	close_filememory(&fm);
	hang = 999;
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_utf16le1");
	close_filememory(&fm);

	test_read_char_utf16le_file(&fm, "", 0);
	check = read_hang_encode(&fm, &c, &hang);
	test(0 < check, "read_hang_utf16le2");
	close_filememory(&fm);

	test_read_char_utf16le_file(&fm, "\x40\x00\x30\x00", 4);
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_utf16le3");
	test(c == 0x40, "read_hang_utf16le4");
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_utf16le5");
	test(c == 0x30, "read_hang_utf16le6");
	check = read_hang_encode(&fm, &c, &hang);
	test(0 < check, "read_hang_utf16le7");
	close_filememory(&fm);

	test_read_char_utf16le_file(&fm, "\x00\x00\xFF\xD7\x00\xE0\xFF\xFF", 8);
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_utf16le8");
	test(c == 0x0000, "read_hang_utf16le9");
	check = read_hang_encode(&fm, &c, &hang);
	test(c == 0xD7FF, "read_hang_utf16le10");
	check = read_hang_encode(&fm, &c, &hang);
	test(c == 0xE000, "read_hang_utf16le11");
	check = read_hang_encode(&fm, &c, &hang);
	test(c == 0xFFFF, "read_hang_utf16le12");
	check = read_hang_encode(&fm, &c, &hang);
	test(0 < check, "read_hang_utf16le13");
	close_filememory(&fm);

	test_read_char_utf16le_file(&fm, "\x00", 1);
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_utf16le14");
	close_filememory(&fm);

	test_read_char_utf16le_file(&fm, "\x00\xD8", 2);
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_utf16le15");
	close_filememory(&fm);

	test_read_char_utf16le_file(&fm, "\x00\xD8\x00\xDC\xFF\xDB\xFF\xDF", 8);
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_utf16le15");
	test(c == 0x010000, "read_hang_utf16le16");
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_utf16le17");
	test(c == 0x10FFFF, "read_hang_utf16le18");
	close_filememory(&fm);

	test_read_char_utf16le_file(&fm, "\xFF\xD7\x00\xE0", 4);
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_utf16le19");
	test(c == 0xD7FF, "read_hang_utf16le20");
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_utf16le21");
	test(c == 0xE000, "read_hang_utf16le22");
	close_filememory(&fm);

	test_read_char_utf16le_file(&fm, "\xFF\xDB\x00\xDC", 4);
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_utf16le23");
	test(c == 0x10FC00, "read_hang_utf16le24");
	close_filememory(&fm);

	test_read_char_utf16le_file(&fm, "\x00\xDC\x00\xDC", 4);
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_utf16le25");
	close_filememory(&fm);

	test_read_char_utf16le_file(&fm, "\x00\xD8\xFF\xDB\x40\x00", 6);
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_utf16le26");
	close_filememory(&fm);

	test_read_char_utf16le_file(&fm, "\x00\xD8\x00\xDC\x40\x00", 6);
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_utf16le27");
	test(c == 0x010000, "read_hang_utf16le28");
	check = read_hang_encode(&fm, &c, &hang);
	test(c == 0x40, "read_hang_utf16le29");
	close_filememory(&fm);

	test_read_char_utf16le_file(&fm, "\x00\xD8\xFF\xDF\x40\x00", 6);
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_utf16le30");
	test(c == 0x0103FF, "read_hang_utf16le31");
	check = read_hang_encode(&fm, &c, &hang);
	test(c == 0x40, "read_hang_utf16le32");
	close_filememory(&fm);

	test_read_char_utf16le_file(&fm, "\x00\xD8\x00\xE0\x40\x00", 6);
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_utf16le33");
	close_filememory(&fm);

	RETURN;
}

static int test_read_hang_utf16be(void)
{
	unicode c;
	int check, hang;
	struct filememory fm;

	/* 1 byte */
	test_read_char_utf16be_file(&fm, "abc", 1);
	close_filememory(&fm);
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_utf16be1");
	close_filememory(&fm);

	test_read_char_utf16be_file(&fm, "", 0);
	check = read_hang_encode(&fm, &c, &hang);
	test(0 < check, "read_hang_utf16be2");
	close_filememory(&fm);

	test_read_char_utf16be_file(&fm, "\x00\x40\x00\x30", 4);
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_utf16be3");
	test(c == 0x40, "read_hang_utf16be4");
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_utf16be5");
	test(c == 0x30, "read_hang_utf16be6");
	check = read_hang_encode(&fm, &c, &hang);
	test(0 < check, "read_hang_utf16be7");
	close_filememory(&fm);

	test_read_char_utf16be_file(&fm, "\x00\x00\xD7\xFF\xE0\x00\xFF\xFF", 8);
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_utf16be8");
	test(c == 0x0000, "read_hang_utf16be9");
	check = read_hang_encode(&fm, &c, &hang);
	test(c == 0xD7FF, "read_hang_utf16be10");
	check = read_hang_encode(&fm, &c, &hang);
	test(c == 0xE000, "read_hang_utf16be11");
	check = read_hang_encode(&fm, &c, &hang);
	test(c == 0xFFFF, "read_hang_utf16be12");
	check = read_hang_encode(&fm, &c, &hang);
	test(0 < check, "read_hang_utf16be13");
	close_filememory(&fm);

	test_read_char_utf16be_file(&fm, "\x00", 1);
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_utf16be14");
	close_filememory(&fm);

	test_read_char_utf16be_file(&fm, "\xD8\x00", 2);
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_utf16be15");
	close_filememory(&fm);

	test_read_char_utf16be_file(&fm, "\xD8\x00\xDC\x00\xDB\xFF\xDF\xFF", 8);
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_utf16be15");
	test(c == 0x010000, "read_hang_utf16be16");
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_utf16be17");
	test(c == 0x10FFFF, "read_hang_utf16be18");
	close_filememory(&fm);

	test_read_char_utf16be_file(&fm, "\xD7\xFF\xE0\x00", 4);
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_utf16be19");
	test(c == 0xD7FF, "read_hang_utf16be20");
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_utf16be21");
	test(c == 0xE000, "read_hang_utf16be22");
	close_filememory(&fm);

	test_read_char_utf16be_file(&fm, "\xDB\xFF\xDC\x00", 4);
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_utf16be23");
	test(c == 0x10FC00, "read_hang_utf16be24");
	close_filememory(&fm);

	test_read_char_utf16be_file(&fm, "\xDC\x00\xDC\x00", 4);
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_utf16be25");
	close_filememory(&fm);

	test_read_char_utf16be_file(&fm, "\xD8\x00\xDB\xFF\x00\x40", 6);
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_utf16be26");
	close_filememory(&fm);

	test_read_char_utf16be_file(&fm, "\xD8\x00\xDC\x00\x00\x40", 6);
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_utf16be27");
	test(c == 0x010000, "read_hang_utf16be28");
	check = read_hang_encode(&fm, &c, &hang);
	test(c == 0x40, "read_hang_utf16be29");
	close_filememory(&fm);

	test_read_char_utf16be_file(&fm, "\xD8\x00\xDF\xFF\x00\x40", 6);
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_utf16be30");
	test(c == 0x0103FF, "read_hang_utf16be31");
	check = read_hang_encode(&fm, &c, &hang);
	test(c == 0x40, "read_hang_utf16be32");
	close_filememory(&fm);

	test_read_char_utf16be_file(&fm, "\xD8\x00\xE0\x00\x00\x40", 6);
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_utf16be33");
	close_filememory(&fm);

	RETURN;
}

static int test_read_hang_utf32le(void)
{
	unicode c;
	int check, hang;
	struct filememory fm;

	test_read_char_utf32le_file(&fm, "", 0);
	check = read_hang_encode(&fm, &c, &hang);
	test(0 < check, "read_hang_utf32le1");
	close_filememory(&fm);

	test_read_char_utf32le_file(&fm, "abcdefg", 1);
	close_filememory(&fm);
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_utf32le2");
	close_filememory(&fm);

	test_read_char_utf32le_file(&fm, "abcdefg", 2);
	close_filememory(&fm);
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_utf32le3");
	close_filememory(&fm);

	test_read_char_utf32le_file(&fm, "abcdefg", 3);
	close_filememory(&fm);
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_utf32le4");
	close_filememory(&fm);

	test_read_char_utf32le_file(&fm, "\x00\x00\x00\x00\x80\x00\x00\x00", 8);
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_utf32le5");
	test(c == 0x00, "read_hang_utf32le6");
	check = read_hang_encode(&fm, &c, &hang);
	test(c == 0x80, "read_hang_utf32le7");
	close_filememory(&fm);

	test_read_char_utf32le_file(&fm, "\xDE\xBC\x0A\x00\xFF\xFF\x10\x00", 8);
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_utf32le8");
	test(c == 0x0ABCDE, "read_hang_utf32le9");
	check = read_hang_encode(&fm, &c, &hang);
	test(c == 0x10FFFF, "read_hang_utf32le10");
	check = read_hang_encode(&fm, &c, &hang);
	test(0 < check, "read_hang_utf32le11");
	close_filememory(&fm);

	test_read_char_utf32le_file(&fm, "\x00\x00\x11\x00", 4);
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_utf32le12");
	close_filememory(&fm);

	RETURN;
}

static int test_read_hang_utf32be(void)
{
	unicode c;
	int check, hang;
	struct filememory fm;

	test_read_char_utf32be_file(&fm, "", 0);
	check = read_hang_encode(&fm, &c, &hang);
	test(0 < check, "read_hang_utf32be1");
	close_filememory(&fm);

	test_read_char_utf32be_file(&fm, "abcdefg", 1);
	close_filememory(&fm);
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_utf32be2");
	close_filememory(&fm);

	test_read_char_utf32be_file(&fm, "abcdefg", 2);
	close_filememory(&fm);
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_utf32be3");
	close_filememory(&fm);

	test_read_char_utf32be_file(&fm, "abcdefg", 3);
	close_filememory(&fm);
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_utf32be4");
	close_filememory(&fm);

	test_read_char_utf32be_file(&fm, "\x00\x00\x00\x00\x00\x00\x00\x80", 8);
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_utf32be5");
	test(c == 0x00, "read_hang_utf32be6");
	check = read_hang_encode(&fm, &c, &hang);
	test(c == 0x80, "read_hang_utf32be7");
	close_filememory(&fm);

	test_read_char_utf32be_file(&fm, "\x00\x0A\xBC\xDE\x00\x10\xFF\xFF", 8);
	check = read_hang_encode(&fm, &c, &hang);
	test(check == 0 && hang == 0, "read_hang_utf32be8");
	test(c == 0x0ABCDE, "read_hang_utf32be9");
	check = read_hang_encode(&fm, &c, &hang);
	test(c == 0x10FFFF, "read_hang_utf32be10");
	check = read_hang_encode(&fm, &c, &hang);
	test(0 < check, "read_hang_utf32be11");
	close_filememory(&fm);

	test_read_char_utf32be_file(&fm, "\x00\x11\x00\x00", 4);
	check = read_hang_encode(&fm, &c, &hang);
	test(check < 0, "read_hang_utf32be12");
	close_filememory(&fm);

	RETURN;
}


/*
 *  write-char
 */
static void test_write_char_ascii_file(struct filememory *fm)
{
	test_open_output_filememory(fm);
	fm->encode.type = EncodeType_ascii;
	fm->encode.code = 999;
	fm->encode.error = 1;
}

static int test_write_char_ascii(void)
{
	int check;
	byte data[100];
	struct filememory fm;
	size_t size;

	test_write_char_ascii_file(&fm);
	check = write_char_encode(&fm, 0);
	test(check == 0, "write_char_ascii1");
	close_filememory(&fm);
	test_fopen_input_binary(data, 100, &size);
	test(size == 1, "write_char_ascii2");
	test(data[0] == 0, "write_char_ascii3");

	test_write_char_ascii_file(&fm);
	check = write_char_encode(&fm, 0x7F);
	test(check == 0, "write_char_ascii4");
	close_filememory(&fm);
	test_fopen_input_binary(data, 100, &size);
	test(size == 1, "write_char_ascii5");
	test(data[0] == 0x7F, "write_char_ascii6");

	test_write_char_ascii_file(&fm);
	check = write_char_encode(&fm, 0x80);
	test(check < 0, "write_char_ascii7");
	close_filememory(&fm);

	RETURN;
}

static void test_write_char_utf8_file(struct filememory *fm)
{
	test_open_output_filememory(fm);
	fm->encode.type = EncodeType_utf8;
	fm->encode.code = 999;
	fm->encode.error = 1;
}

static int test_write_char_utf8(void)
{
	int check;
	byte data[100];
	struct filememory fm;
	size_t size;

	test_write_char_utf8_file(&fm);
	check = write_char_encode(&fm, 0);
	test(check == 0, "write_char_utf8-1");
	close_filememory(&fm);
	test_fopen_input_binary(data, 100, &size);
	test(size == 1, "write_char_utf8-2");
	test(data[0] == 0, "write_char_utf8-3");

	test_write_char_utf8_file(&fm);
	check = write_char_encode(&fm, 0x7F);
	test(check == 0, "write_char_utf8-4");
	close_filememory(&fm);
	test_fopen_input_binary(data, 100, &size);
	test(size == 1, "write_char_utf8-5");
	test(data[0] == 0x7F, "write_char_utf8-6");

	test_write_char_utf8_file(&fm);
	check = write_char_encode(&fm, 0x80);
	test(check == 0, "write_char_utf8-7");
	close_filememory(&fm);
	test_fopen_input_binary(data, 100, &size);
	test(size == 2, "write_char_utf8-8");
	test(data[0] == 0xC2, "write_char_utf8-9");
	test(data[1] == 0x80, "write_char_utf8-10");

	test_write_char_utf8_file(&fm);
	check = write_char_encode(&fm, 0x81);
	test(check == 0, "write_char_utf8-11");
	check = write_char_encode(&fm, 0x07FE);
	test(check == 0, "write_char_utf8-12");
	check = write_char_encode(&fm, 0x07FF);
	test(check == 0, "write_char_utf8-13");
	close_filememory(&fm);
	test_fopen_input_binary(data, 100, &size);
	test(size == 6, "write_char_utf8-14");
	test(data[0] == 0xC2, "write_char_utf8-15");
	test(data[1] == 0x81, "write_char_utf8-16");
	test(data[2] == 0xDF, "write_char_utf8-17");
	test(data[3] == 0xBE, "write_char_utf8-18");
	test(data[4] == 0xDF, "write_char_utf8-19");
	test(data[5] == 0xBF, "write_char_utf8-20");

	test_write_char_utf8_file(&fm);
	check = write_char_encode(&fm, 0x0800);
	test(check == 0, "write_char_utf8-21");
	check = write_char_encode(&fm, 0xD7FF);
	test(check == 0, "write_char_utf8-22");
	check = write_char_encode(&fm, 0xD800);
	test(check < 0, "write_char_utf8-23");
	check = write_char_encode(&fm, 0xDE00);
	test(check < 0, "write_char_utf8-24");
	check = write_char_encode(&fm, 0xDFFF);
	test(check < 0, "write_char_utf8-25");
	close_filememory(&fm);
	test_fopen_input_binary(data, 100, &size);
	test(size == 6, "write_char_utf8-26");
	test(data[0] == 0xE0, "write_char_utf8-27");
	test(data[1] == 0xA0, "write_char_utf8-28");
	test(data[2] == 0x80, "write_char_utf8-29");
	test(data[3] == 0xED, "write_char_utf8-30");
	test(data[4] == 0x9F, "write_char_utf8-31");
	test(data[5] == 0xBF, "write_char_utf8-32");

	test_write_char_utf8_file(&fm);
	check = write_char_encode(&fm, 0xE000);
	test(check == 0, "write_char_utf8-33");
	check = write_char_encode(&fm, 0xE001);
	test(check == 0, "write_char_utf8-34");
	check = write_char_encode(&fm, 0xE17D);
	test(check == 0, "write_char_utf8-35");
	check = write_char_encode(&fm, 0xFFFE);
	test(check == 0, "write_char_utf8-36");
	check = write_char_encode(&fm, 0xFFFF);
	test(check == 0, "write_char_utf8-37");
	close_filememory(&fm);
	test_fopen_input_binary(data, 100, &size);
	test(size == 15, "write_char_utf8-38");
	test(data[0] == 0xEE, "write_char_utf8-27");
	test(data[1] == 0x80, "write_char_utf8-28");
	test(data[2] == 0x80, "write_char_utf8-29");
	test(data[3] == 0xEE, "write_char_utf8-30");
	test(data[4] == 0x80, "write_char_utf8-31");
	test(data[5] == 0x81, "write_char_utf8-32");
	test(data[6] == 0xEE, "write_char_utf8-33");
	test(data[7] == 0x85, "write_char_utf8-34");
	test(data[8] == 0xBD, "write_char_utf8-35");
	test(data[9] == 0xEF, "write_char_utf8-36");
	test(data[10] == 0xBF, "write_char_utf8-37");
	test(data[11] == 0xBE, "write_char_utf8-38");
	test(data[12] == 0xEF, "write_char_utf8-39");
	test(data[13] == 0xBF, "write_char_utf8-40");
	test(data[14] == 0xBF, "write_char_utf8-41");

	test_write_char_utf8_file(&fm);
	check = write_char_encode(&fm, 0x010000);
	test(check == 0, "write_char_utf8-42");
	check = write_char_encode(&fm, 0x010001);
	test(check == 0, "write_char_utf8-43");
	check = write_char_encode(&fm, 0x0ABCDE);
	test(check == 0, "write_char_utf8-44");
	check = write_char_encode(&fm, 0x10FFFF);
	test(check == 0, "write_char_utf8-45");
	check = write_char_encode(&fm, 0x110000);
	test(check < 0, "write_char_utf8-46");
	close_filememory(&fm);
	test_fopen_input_binary(data, 100, &size);
	test(size == 16, "write_char_utf8-47");
	test(data[0] == 0xF0, "write_char_utf8-48");
	test(data[1] == 0x90, "write_char_utf8-49");
	test(data[2] == 0x80, "write_char_utf8-50");
	test(data[3] == 0x80, "write_char_utf8-51");
	test(data[4] == 0xF0, "write_char_utf8-52");
	test(data[5] == 0x90, "write_char_utf8-53");
	test(data[6] == 0x80, "write_char_utf8-54");
	test(data[7] == 0x81, "write_char_utf8-55");
	test(data[8] == 0xF2, "write_char_utf8-56");
	test(data[9] == 0xAB, "write_char_utf8-57");
	test(data[10] == 0xB3, "write_char_utf8-58");
	test(data[11] == 0x9E, "write_char_utf8-59");
	test(data[12] == 0xF4, "write_char_utf8-60");
	test(data[13] == 0x8F, "write_char_utf8-61");
	test(data[14] == 0xBF, "write_char_utf8-62");
	test(data[15] == 0xBF, "write_char_utf8-63");

	RETURN;
}

static void test_write_char_utf16le_file(struct filememory *fm)
{
	test_open_output_filememory(fm);
	fm->encode.type = EncodeType_utf16le;
	fm->encode.code = 999;
	fm->encode.error = 1;
}

static int test_write_char_utf16le(void)
{
	int check;
	byte data[100];
	struct filememory fm;
	size_t size;

	test_write_char_utf16le_file(&fm);
	check = write_char_encode(&fm, 0);
	test(check == 0, "write_char_utf16le1");
	close_filememory(&fm);
	test_fopen_input_binary(data, 100, &size);
	test(size == 2, "write_char_utf16le2");
	test(data[0] == 0, "write_char_utf16le3");
	test(data[1] == 0, "write_char_utf16le4");

	test_write_char_utf16le_file(&fm);
	check = write_char_encode(&fm, 0x80);
	test(check == 0, "write_char_utf16le5");
	check = write_char_encode(&fm, 0x1234);
	test(check == 0, "write_char_utf16le6");
	check = write_char_encode(&fm, 0xD7FF);
	test(check == 0, "write_char_utf16le7");
	check = write_char_encode(&fm, 0xD800);
	test(check < 0, "write_char_utf16le8");
	check = write_char_encode(&fm, 0xDFFF);
	test(check < 0, "write_char_utf16le9");
	check = write_char_encode(&fm, 0xE000);
	test(check == 0, "write_char_utf16le10");
	check = write_char_encode(&fm, 0xE001);
	test(check == 0, "write_char_utf16le11");
	check = write_char_encode(&fm, 0xABCD);
	test(check == 0, "write_char_utf16le12");
	check = write_char_encode(&fm, 0xFFFE);
	test(check == 0, "write_char_utf16le13");
	check = write_char_encode(&fm, 0xFFFF);
	test(check == 0, "write_char_utf16le14");
	close_filememory(&fm);
	test_fopen_input_binary(data, 100, &size);
	test(size == 16, "write_char_utf16le15");
	test(data[0] == 0x80, "write_char_utf16le16");
	test(data[1] == 0x00, "write_char_utf16le17");
	test(data[2] == 0x34, "write_char_utf16le18");
	test(data[3] == 0x12, "write_char_utf16le19");
	test(data[4] == 0xFF, "write_char_utf16le20");
	test(data[5] == 0xD7, "write_char_utf16le21");
	test(data[6] == 0x00, "write_char_utf16le22");
	test(data[7] == 0xE0, "write_char_utf16le23");
	test(data[8] == 0x01, "write_char_utf16le24");
	test(data[9] == 0xE0, "write_char_utf16le25");
	test(data[10] == 0xCD, "write_char_utf16le26");
	test(data[11] == 0xAB, "write_char_utf16le27");
	test(data[12] == 0xFE, "write_char_utf16le28");
	test(data[13] == 0xFF, "write_char_utf16le29");
	test(data[14] == 0xFF, "write_char_utf16le30");
	test(data[15] == 0xFF, "write_char_utf16le31");

	test_write_char_utf16le_file(&fm);
	check = write_char_encode(&fm, 0x010000);
	test(check == 0, "write_char_utf16le32");
	check = write_char_encode(&fm, 0x010001);
	test(check == 0, "write_char_utf16le33");
	check = write_char_encode(&fm, 0x0ABCDE);
	test(check == 0, "write_char_utf16le34");
	check = write_char_encode(&fm, 0x10FFFE);
	test(check == 0, "write_char_utf16le35");
	check = write_char_encode(&fm, 0x10FFFF);
	test(check == 0, "write_char_utf16le36");
	check = write_char_encode(&fm, 0x110000);
	test(check < 0, "write_char_utf16le37");
	close_filememory(&fm);
	test_fopen_input_binary(data, 100, &size);
	test(size == 20, "write_char_utf16le38");
	test(data[0] == 0x00, "write_char_utf16le39");
	test(data[1] == 0xD8, "write_char_utf16le40");
	test(data[2] == 0x00, "write_char_utf16le41");
	test(data[3] == 0xDC, "write_char_utf16le42");

	test(data[4] == 0x00, "write_char_utf16le43");
	test(data[5] == 0xD8, "write_char_utf16le44");
	test(data[6] == 0x01, "write_char_utf16le45");
	test(data[7] == 0xDC, "write_char_utf16le46");

	test(data[8] == 0x6F, "write_char_utf16le47");
	test(data[9] == 0xDA, "write_char_utf16le48");
	test(data[10] == 0xDE, "write_char_utf16le49");
	test(data[11] == 0xDC, "write_char_utf16le50");

	test(data[12] == 0xFF, "write_char_utf16le51");
	test(data[13] == 0xDB, "write_char_utf16le52");
	test(data[14] == 0xFE, "write_char_utf16le53");
	test(data[15] == 0xDF, "write_char_utf16le54");

	test(data[16] == 0xFF, "write_char_utf16le55");
	test(data[17] == 0xDB, "write_char_utf16le56");
	test(data[18] == 0xFF, "write_char_utf16le57");
	test(data[19] == 0xDF, "write_char_utf16le58");

	RETURN;
}

static void test_write_char_utf16be_file(struct filememory *fm)
{
	test_open_output_filememory(fm);
	fm->encode.type = EncodeType_utf16be;
	fm->encode.code = 999;
	fm->encode.error = 1;
}

static int test_write_char_utf16be(void)
{
	int check;
	byte data[100];
	struct filememory fm;
	size_t size;

	test_write_char_utf16be_file(&fm);
	check = write_char_encode(&fm, 0);
	test(check == 0, "write_char_utf16be1");
	close_filememory(&fm);
	test_fopen_input_binary(data, 100, &size);
	test(size == 2, "write_char_utf16be2");
	test(data[0] == 0, "write_char_utf16be3");
	test(data[1] == 0, "write_char_utf16be4");

	test_write_char_utf16be_file(&fm);
	check = write_char_encode(&fm, 0x80);
	test(check == 0, "write_char_utf16be5");
	check = write_char_encode(&fm, 0x1234);
	test(check == 0, "write_char_utf16be6");
	check = write_char_encode(&fm, 0xD7FF);
	test(check == 0, "write_char_utf16be7");
	check = write_char_encode(&fm, 0xD800);
	test(check < 0, "write_char_utf16be8");
	check = write_char_encode(&fm, 0xDFFF);
	test(check < 0, "write_char_utf16be9");
	check = write_char_encode(&fm, 0xE000);
	test(check == 0, "write_char_utf16be10");
	check = write_char_encode(&fm, 0xE001);
	test(check == 0, "write_char_utf16be11");
	check = write_char_encode(&fm, 0xABCD);
	test(check == 0, "write_char_utf16be12");
	check = write_char_encode(&fm, 0xFFFE);
	test(check == 0, "write_char_utf16be13");
	check = write_char_encode(&fm, 0xFFFF);
	test(check == 0, "write_char_utf16be14");
	close_filememory(&fm);
	test_fopen_input_binary(data, 100, &size);
	test(size == 16, "write_char_utf16be15");
	test(data[0] == 0x00, "write_char_utf16be16");
	test(data[1] == 0x80, "write_char_utf16be17");
	test(data[2] == 0x12, "write_char_utf16be18");
	test(data[3] == 0x34, "write_char_utf16be19");
	test(data[4] == 0xD7, "write_char_utf16be20");
	test(data[5] == 0xFF, "write_char_utf16be21");
	test(data[6] == 0xE0, "write_char_utf16be22");
	test(data[7] == 0x00, "write_char_utf16be23");
	test(data[8] == 0xE0, "write_char_utf16be24");
	test(data[9] == 0x01, "write_char_utf16be25");
	test(data[10] == 0xAB, "write_char_utf16be26");
	test(data[11] == 0xCD, "write_char_utf16be27");
	test(data[12] == 0xFF, "write_char_utf16be28");
	test(data[13] == 0xFE, "write_char_utf16be29");
	test(data[14] == 0xFF, "write_char_utf16be30");
	test(data[15] == 0xFF, "write_char_utf16be31");

	test_write_char_utf16be_file(&fm);
	check = write_char_encode(&fm, 0x010000);
	test(check == 0, "write_char_utf16be32");
	check = write_char_encode(&fm, 0x010001);
	test(check == 0, "write_char_utf16be33");
	check = write_char_encode(&fm, 0x0ABCDE);
	test(check == 0, "write_char_utf16be34");
	check = write_char_encode(&fm, 0x10FFFE);
	test(check == 0, "write_char_utf16be35");
	check = write_char_encode(&fm, 0x10FFFF);
	test(check == 0, "write_char_utf16be36");
	check = write_char_encode(&fm, 0x110000);
	test(check < 0, "write_char_utf16be37");
	close_filememory(&fm);
	test_fopen_input_binary(data, 100, &size);
	test(size == 20, "write_char_utf16be38");
	test(data[0] == 0xD8, "write_char_utf16be39");
	test(data[1] == 0x00, "write_char_utf16be40");
	test(data[2] == 0xDC, "write_char_utf16be41");
	test(data[3] == 0x00, "write_char_utf16be42");

	test(data[4] == 0xD8, "write_char_utf16be43");
	test(data[5] == 0x00, "write_char_utf16be44");
	test(data[6] == 0xDC, "write_char_utf16be45");
	test(data[7] == 0x01, "write_char_utf16be46");

	test(data[8] == 0xDA, "write_char_utf16be47");
	test(data[9] == 0x6F, "write_char_utf16be48");
	test(data[10] == 0xDC, "write_char_utf16be49");
	test(data[11] == 0xDE, "write_char_utf16be50");

	test(data[12] == 0xDB, "write_char_utf16be51");
	test(data[13] == 0xFF, "write_char_utf16be52");
	test(data[14] == 0xDF, "write_char_utf16be53");
	test(data[15] == 0xFE, "write_char_utf16be54");

	test(data[16] == 0xDB, "write_char_utf16be55");
	test(data[17] == 0xFF, "write_char_utf16be56");
	test(data[18] == 0xDF, "write_char_utf16be57");
	test(data[19] == 0xFF, "write_char_utf16be58");

	RETURN;
}

static void test_write_char_utf32le_file(struct filememory *fm)
{
	test_open_output_filememory(fm);
	fm->encode.type = EncodeType_utf32le;
	fm->encode.code = 999;
	fm->encode.error = 1;
}

static int test_write_char_utf32le(void)
{
	int check;
	byte data[100];
	struct filememory fm;
	size_t size;

	test_write_char_utf32le_file(&fm);
	check = write_char_encode(&fm, 0);
	test(check == 0, "write_char_utf32le1");
	close_filememory(&fm);
	test_fopen_input_binary(data, 100, &size);
	test(size == 4, "write_char_utf32le2");
	test(data[0] == 0, "write_char_utf32le3");
	test(data[1] == 0, "write_char_utf32le4");
	test(data[2] == 0, "write_char_utf32le5");
	test(data[3] == 0, "write_char_utf32le6");

	test_write_char_utf32le_file(&fm);
	check = write_char_encode(&fm, 0x40);
	test(check == 0, "write_char_utf32le7");
	check = write_char_encode(&fm, 0x1234);
	test(check == 0, "write_char_utf32le8");
	check = write_char_encode(&fm, 0xD7FF);
	test(check == 0, "write_char_utf32le9");
	check = write_char_encode(&fm, 0xD800);
	test(check < 0, "write_char_utf32le10");
	check = write_char_encode(&fm, 0xDFFF);
	test(check < 0, "write_char_utf32le11");
	check = write_char_encode(&fm, 0xE000);
	test(check == 0, "write_char_utf32le12");
	check = write_char_encode(&fm, 0xE001);
	test(check == 0, "write_char_utf32le13");
	check = write_char_encode(&fm, 0xABCD);
	test(check == 0, "write_char_utf32le14");
	check = write_char_encode(&fm, 0xFFFE);
	test(check == 0, "write_char_utf32le15");
	check = write_char_encode(&fm, 0xFFFF);
	test(check == 0, "write_char_utf32le16");
	check = write_char_encode(&fm, 0x010000);
	test(check == 0, "write_char_utf32le17");
	check = write_char_encode(&fm, 0x010001);
	test(check == 0, "write_char_utf32le18");
	check = write_char_encode(&fm, 0x01ABCD);
	test(check == 0, "write_char_utf32le19");
	check = write_char_encode(&fm, 0x0ABCDE);
	test(check == 0, "write_char_utf32le20");
	check = write_char_encode(&fm, 0x10FFFE);
	test(check == 0, "write_char_utf32le21");
	check = write_char_encode(&fm, 0x10FFFF);
	test(check == 0, "write_char_utf32le22");
	check = write_char_encode(&fm, 0x110000);
	test(check < 0, "write_char_utf32le23");
	close_filememory(&fm);

	test_fopen_input_binary(data, 100, &size);
	test(size == 14*4, "write_char_utf32le24");

	test(data[0] == 0x40, "write_char_utf32le25");
	test(data[1] == 0x00, "write_char_utf32le26");
	test(data[2] == 0x00, "write_char_utf32le27");
	test(data[3] == 0x00, "write_char_utf32le28");

	test(data[4] == 0x34, "write_char_utf32le29");
	test(data[5] == 0x12, "write_char_utf32le30");
	test(data[6] == 0x00, "write_char_utf32le31");
	test(data[7] == 0x00, "write_char_utf32le32");

	test(data[8] == 0xFF, "write_char_utf32le33");
	test(data[9] == 0xD7, "write_char_utf32le34");
	test(data[10] == 0x00, "write_char_utf32le35");
	test(data[11] == 0x00, "write_char_utf32le36");

	test(data[12] == 0x00, "write_char_utf32le37");
	test(data[13] == 0xE0, "write_char_utf32le38");
	test(data[14] == 0x00, "write_char_utf32le39");
	test(data[15] == 0x00, "write_char_utf32le40");

	test(data[16] == 0x01, "write_char_utf32le41");
	test(data[17] == 0xE0, "write_char_utf32le42");
	test(data[18] == 0x00, "write_char_utf32le43");
	test(data[19] == 0x00, "write_char_utf32le44");

	test(data[20] == 0xCD, "write_char_utf32le45");
	test(data[21] == 0xAB, "write_char_utf32le46");
	test(data[22] == 0x00, "write_char_utf32le47");
	test(data[23] == 0x00, "write_char_utf32le48");

	test(data[24] == 0xFE, "write_char_utf32le49");
	test(data[25] == 0xFF, "write_char_utf32le50");
	test(data[26] == 0x00, "write_char_utf32le51");
	test(data[27] == 0x00, "write_char_utf32le52");

	test(data[28] == 0xFF, "write_char_utf32le53");
	test(data[29] == 0xFF, "write_char_utf32le54");
	test(data[30] == 0x00, "write_char_utf32le55");
	test(data[31] == 0x00, "write_char_utf32le56");

	test(data[32] == 0x00, "write_char_utf32le57");
	test(data[33] == 0x00, "write_char_utf32le58");
	test(data[34] == 0x01, "write_char_utf32le59");
	test(data[35] == 0x00, "write_char_utf32le60");

	test(data[36] == 0x01, "write_char_utf32le61");
	test(data[37] == 0x00, "write_char_utf32le62");
	test(data[38] == 0x01, "write_char_utf32le63");
	test(data[39] == 0x00, "write_char_utf32le64");

	test(data[40] == 0xCD, "write_char_utf32le65");
	test(data[41] == 0xAB, "write_char_utf32le66");
	test(data[42] == 0x01, "write_char_utf32le67");
	test(data[43] == 0x00, "write_char_utf32le68");

	test(data[44] == 0xDE, "write_char_utf32le69");
	test(data[45] == 0xBC, "write_char_utf32le70");
	test(data[46] == 0x0A, "write_char_utf32le71");
	test(data[47] == 0x00, "write_char_utf32le72");

	test(data[48] == 0xFE, "write_char_utf32le73");
	test(data[49] == 0xFF, "write_char_utf32le74");
	test(data[50] == 0x10, "write_char_utf32le75");
	test(data[51] == 0x00, "write_char_utf32le76");

	test(data[52] == 0xFF, "write_char_utf32le77");
	test(data[53] == 0xFF, "write_char_utf32le78");
	test(data[54] == 0x10, "write_char_utf32le79");
	test(data[55] == 0x00, "write_char_utf32le80");

	RETURN;
}

static void test_write_char_utf32be_file(struct filememory *fm)
{
	test_open_output_filememory(fm);
	fm->encode.type = EncodeType_utf32be;
	fm->encode.code = 999;
	fm->encode.error = 1;
}

static int test_write_char_utf32be(void)
{
	int check;
	byte data[100];
	struct filememory fm;
	size_t size;

	test_write_char_utf32be_file(&fm);
	check = write_char_encode(&fm, 0);
	test(check == 0, "write_char_utf32be1");
	close_filememory(&fm);
	test_fopen_input_binary(data, 100, &size);
	test(size == 4, "write_char_utf32be2");
	test(data[0] == 0, "write_char_utf32be3");
	test(data[1] == 0, "write_char_utf32be4");
	test(data[2] == 0, "write_char_utf32be5");
	test(data[3] == 0, "write_char_utf32be6");

	test_write_char_utf32be_file(&fm);
	check = write_char_encode(&fm, 0x40);
	test(check == 0, "write_char_utf32be7");
	check = write_char_encode(&fm, 0x1234);
	test(check == 0, "write_char_utf32be8");
	check = write_char_encode(&fm, 0xD7FF);
	test(check == 0, "write_char_utf32be9");
	check = write_char_encode(&fm, 0xD800);
	test(check < 0, "write_char_utf32be10");
	check = write_char_encode(&fm, 0xDFFF);
	test(check < 0, "write_char_utf32be11");
	check = write_char_encode(&fm, 0xE000);
	test(check == 0, "write_char_utf32be12");
	check = write_char_encode(&fm, 0xE001);
	test(check == 0, "write_char_utf32be13");
	check = write_char_encode(&fm, 0xABCD);
	test(check == 0, "write_char_utf32be14");
	check = write_char_encode(&fm, 0xFFFE);
	test(check == 0, "write_char_utf32be15");
	check = write_char_encode(&fm, 0xFFFF);
	test(check == 0, "write_char_utf32be16");
	check = write_char_encode(&fm, 0x010000);
	test(check == 0, "write_char_utf32be17");
	check = write_char_encode(&fm, 0x010001);
	test(check == 0, "write_char_utf32be18");
	check = write_char_encode(&fm, 0x01ABCD);
	test(check == 0, "write_char_utf32be19");
	check = write_char_encode(&fm, 0x0ABCDE);
	test(check == 0, "write_char_utf32be20");
	check = write_char_encode(&fm, 0x10FFFE);
	test(check == 0, "write_char_utf32be21");
	check = write_char_encode(&fm, 0x10FFFF);
	test(check == 0, "write_char_utf32be22");
	check = write_char_encode(&fm, 0x110000);
	test(check < 0, "write_char_utf32be23");
	close_filememory(&fm);

	test_fopen_input_binary(data, 100, &size);
	test(size == 14*4, "write_char_utf32be24");

	test(data[0] == 0x00, "write_char_utf32be25");
	test(data[1] == 0x00, "write_char_utf32be26");
	test(data[2] == 0x00, "write_char_utf32be27");
	test(data[3] == 0x40, "write_char_utf32be28");

	test(data[4] == 0x00, "write_char_utf32be29");
	test(data[5] == 0x00, "write_char_utf32be30");
	test(data[6] == 0x12, "write_char_utf32be31");
	test(data[7] == 0x34, "write_char_utf32be32");

	test(data[8] == 0x00, "write_char_utf32be33");
	test(data[9] == 0x00, "write_char_utf32be34");
	test(data[10] == 0xD7, "write_char_utf32be35");
	test(data[11] == 0xFF, "write_char_utf32be36");

	test(data[12] == 0x00, "write_char_utf32be37");
	test(data[13] == 0x00, "write_char_utf32be38");
	test(data[14] == 0xE0, "write_char_utf32be39");
	test(data[15] == 0x00, "write_char_utf32be40");

	test(data[16] == 0x00, "write_char_utf32be41");
	test(data[17] == 0x00, "write_char_utf32be42");
	test(data[18] == 0xE0, "write_char_utf32be43");
	test(data[19] == 0x01, "write_char_utf32be44");

	test(data[20] == 0x00, "write_char_utf32be45");
	test(data[21] == 0x00, "write_char_utf32be46");
	test(data[22] == 0xAB, "write_char_utf32be47");
	test(data[23] == 0xCD, "write_char_utf32be48");

	test(data[24] == 0x00, "write_char_utf32be49");
	test(data[25] == 0x00, "write_char_utf32be50");
	test(data[26] == 0xFF, "write_char_utf32be51");
	test(data[27] == 0xFE, "write_char_utf32be52");

	test(data[28] == 0x00, "write_char_utf32be53");
	test(data[29] == 0x00, "write_char_utf32be54");
	test(data[30] == 0xFF, "write_char_utf32be55");
	test(data[31] == 0xFF, "write_char_utf32be56");

	test(data[32] == 0x00, "write_char_utf32be57");
	test(data[33] == 0x01, "write_char_utf32be58");
	test(data[34] == 0x00, "write_char_utf32be59");
	test(data[35] == 0x00, "write_char_utf32be60");

	test(data[36] == 0x00, "write_char_utf32be61");
	test(data[37] == 0x01, "write_char_utf32be62");
	test(data[38] == 0x00, "write_char_utf32be63");
	test(data[39] == 0x01, "write_char_utf32be64");

	test(data[40] == 0x00, "write_char_utf32be65");
	test(data[41] == 0x01, "write_char_utf32be66");
	test(data[42] == 0xAB, "write_char_utf32be67");
	test(data[43] == 0xCD, "write_char_utf32be68");

	test(data[44] == 0x00, "write_char_utf32be69");
	test(data[45] == 0x0A, "write_char_utf32be70");
	test(data[46] == 0xBC, "write_char_utf32be71");
	test(data[47] == 0xDE, "write_char_utf32be72");

	test(data[48] == 0x00, "write_char_utf32be73");
	test(data[49] == 0x10, "write_char_utf32be74");
	test(data[50] == 0xFF, "write_char_utf32be75");
	test(data[51] == 0xFE, "write_char_utf32be76");

	test(data[52] == 0x00, "write_char_utf32be77");
	test(data[53] == 0x10, "write_char_utf32be78");
	test(data[54] == 0xFF, "write_char_utf32be79");
	test(data[55] == 0xFF, "write_char_utf32be80");

	RETURN;
}

/*
 *  length-char
 */
static int test_length_char_ascii(void)
{
	struct filememory fm;

	fm.encode.type = EncodeType_ascii;
	fm.encode.code = 999;
	fm.encode.error = 1;
	test(length_char_encode(&fm, 0) == 1, "length_char_ascii1");
	test(length_char_encode(&fm, 1) == 1, "length_char_ascii2");
	test(length_char_encode(&fm, 0x7F) == 1, "length_char_ascii3");
	test(length_char_encode(&fm, 0x80) < 0, "length_char_ascii4");

	RETURN;
}

static int test_length_char_utf8(void)
{
	struct filememory fm;

	fm.encode.type = EncodeType_utf8;
	fm.encode.code = 999;
	fm.encode.error = 1;
	test(length_char_encode(&fm, 0) == 1, "length_char_utf8-1");
	test(length_char_encode(&fm, 1) == 1, "length_char_utf8-2");
	test(length_char_encode(&fm, 0x7F) == 1, "length_char_utf8-3");
	test(length_char_encode(&fm, 0x80) == 2, "length_char_utf8-4");
	test(length_char_encode(&fm, 0x07FF) == 2, "length_char_utf8-5");
	test(length_char_encode(&fm, 0xD7FF) == 3, "length_char_utf8-6");
	test(length_char_encode(&fm, 0xD800) < 0, "length_char_utf8-7");
	test(length_char_encode(&fm, 0xDFFF) < 0, "length_char_utf8-8");
	test(length_char_encode(&fm, 0xE000) == 3, "length_char_utf8-9");
	test(length_char_encode(&fm, 0xE001) == 3, "length_char_utf8-10");
	test(length_char_encode(&fm, 0xFFFF) == 3, "length_char_utf8-11");
	test(length_char_encode(&fm, 0x010000) == 4, "length_char_utf8-12");
	test(length_char_encode(&fm, 0x01ABCD) == 4, "length_char_utf8-13");
	test(length_char_encode(&fm, 0x0ABCDE) == 4, "length_char_utf8-14");
	test(length_char_encode(&fm, 0x10FFFE) == 4, "length_char_utf8-15");
	test(length_char_encode(&fm, 0x10FFFF) == 4, "length_char_utf8-16");
	test(length_char_encode(&fm, 0x110000) < 0, "length_char_utf8-17");

	RETURN;
}

static int test_length_char_utf16(void)
{
	struct filememory fm;

	fm.encode.type = EncodeType_utf16be;
	fm.encode.code = 999;
	fm.encode.error = 1;
	test(length_char_encode(&fm, 0) == 2, "length_char_utf16-1");
	test(length_char_encode(&fm, 1) == 2, "length_char_utf16-2");
	test(length_char_encode(&fm, 0x7F) == 2, "length_char_utf16-3");
	test(length_char_encode(&fm, 0x80) == 2, "length_char_utf16-4");
	test(length_char_encode(&fm, 0x07FF) == 2, "length_char_utf16-5");
	test(length_char_encode(&fm, 0xD7FF) == 2, "length_char_utf16-6");
	test(length_char_encode(&fm, 0xD800) < 0, "length_char_utf16-7");
	test(length_char_encode(&fm, 0xDFFF) < 0, "length_char_utf16-8");
	test(length_char_encode(&fm, 0xE000) == 2, "length_char_utf16-9");
	test(length_char_encode(&fm, 0xE001) == 2, "length_char_utf16-10");
	test(length_char_encode(&fm, 0xFFFF) == 2, "length_char_utf16-11");
	test(length_char_encode(&fm, 0x010000) == 4, "length_char_utf16-12");
	test(length_char_encode(&fm, 0x01ABCD) == 4, "length_char_utf16-13");
	test(length_char_encode(&fm, 0x0ABCDE) == 4, "length_char_utf16-14");
	test(length_char_encode(&fm, 0x10FFFE) == 4, "length_char_utf16-15");
	test(length_char_encode(&fm, 0x10FFFF) == 4, "length_char_utf16-16");
	test(length_char_encode(&fm, 0x110000) < 0, "length_char_utf16-17");

	RETURN;
}

static int test_length_char_utf32(void)
{
	struct filememory fm;

	fm.encode.type = EncodeType_utf32le;
	fm.encode.code = 999;
	fm.encode.error = 1;
	test(length_char_encode(&fm, 0) == 4, "length_char_utf32-1");
	test(length_char_encode(&fm, 1) == 4, "length_char_utf32-2");
	test(length_char_encode(&fm, 0x7F) == 4, "length_char_utf32-3");
	test(length_char_encode(&fm, 0x80) == 4, "length_char_utf32-4");
	test(length_char_encode(&fm, 0x07FF) == 4, "length_char_utf32-5");
	test(length_char_encode(&fm, 0xD7FF) == 4, "length_char_utf32-6");
	test(length_char_encode(&fm, 0xD800) < 0, "length_char_utf32-7");
	test(length_char_encode(&fm, 0xDFFF) < 0, "length_char_utf32-8");
	test(length_char_encode(&fm, 0xE000) == 4, "length_char_utf32-9");
	test(length_char_encode(&fm, 0xE001) == 4, "length_char_utf32-10");
	test(length_char_encode(&fm, 0xFFFF) == 4, "length_char_utf32-11");
	test(length_char_encode(&fm, 0x010000) == 4, "length_char_utf32-12");
	test(length_char_encode(&fm, 0x01ABCD) == 4, "length_char_utf32-13");
	test(length_char_encode(&fm, 0x0ABCDE) == 4, "length_char_utf32-14");
	test(length_char_encode(&fm, 0x10FFFE) == 4, "length_char_utf32-15");
	test(length_char_encode(&fm, 0x10FFFF) == 4, "length_char_utf32-16");
	test(length_char_encode(&fm, 0x110000) < 0, "length_char_utf32-17");

	RETURN;
}

static int test_length_string_encode(void)
{
	addr pos;
	size_t size;
	struct filememory fm;

	fm.encode.code = 999;
	fm.encode.error = 1;

	strvect_heap(&pos, 4);
	strvect_setc(pos, 0, 0x00);
	strvect_setc(pos, 1, 0x01);
	strvect_setc(pos, 2, 0x40);
	strvect_setc(pos, 3, 0x7F);
	fm.encode.type = EncodeType_ascii;
	test(! length_string_encode(&fm, pos, &size), "length_string_encode1");
	test(size == 4, "length_string_encode2");
	fm.encode.type = EncodeType_utf8;
	test(! length_string_encode(&fm, pos, &size), "length_string_encode3");
	test(size == 4, "length_string_encode4");
	fm.encode.type = EncodeType_utf16le;
	test(! length_string_encode(&fm, pos, &size), "length_string_encode5");
	test(size == 8, "length_string_encode6");
	fm.encode.type = EncodeType_utf32be;
	test(! length_string_encode(&fm, pos, &size), "length_string_encode7");
	test(size == 16, "length_string_encode8");

	strvect_heap(&pos, 4);
	strvect_setc(pos, 0, 0x40);
	strvect_setc(pos, 1, 0x0300);
	strvect_setc(pos, 2, 0xABCD);
	strvect_setc(pos, 3, 0x040000);
	fm.encode.type = EncodeType_utf8;
	test(! length_string_encode(&fm, pos, &size), "length_string_encode9");
	test(size == 1+2+3+4, "length_string_encode10");
	fm.encode.type = EncodeType_utf16le;
	test(! length_string_encode(&fm, pos, &size), "length_string_encode11");
	test(size == 2+2+2+4, "length_string_encode12");
	fm.encode.type = EncodeType_utf32be;
	test(! length_string_encode(&fm, pos, &size), "length_string_encode13");
	test(size == 4+4+4+4, "length_string_encode14");

	RETURN;
}


/*
 *  unicode buffer
 */
static int test_UTF8_length(void)
{
	addr pos;
	unicode *ptr;
	size_t size;

	strvect_heap(&pos, 15);
	GetStringUnicode(pos, (const unicode **)&ptr);
	ptr[0] = 0; /* 1 */
	ptr[1] = 1; /* 1 */
	ptr[2] = 0x7F; /* 1 */
	ptr[3] = 0x80; /* 2 */
	ptr[4] = 0x07FF; /* 2 */
	ptr[5] = 0x8000; /* 3 */
	ptr[6] = 0x8001; /* 3 */
	ptr[7] = 0xD7FF; /* 3 */
	ptr[8] = 0xE000; /* 3 */
	ptr[9] = 0xE001; /* 3 */
	ptr[10] = 0x010000; /* 4 */
	ptr[11] = 0x010001; /* 4 */
	ptr[12] = 0x0ABCDE; /* 4 */
	ptr[13] = 0x10FFFE; /* 4 */
	ptr[14] = 0x10FFFF; /* 4 */
	test(! UTF8_length(pos, &size), "UTF8_length1");
	test(size == 1*3+2*2+3*5+4*5, "UTF8_length2");

	ptr[12] = 0xD800; /* error */
	test(UTF8_length(pos, &size), "UTF8_length3");
	ptr[12] = 0xDFFF; /* error */
	test(UTF8_length(pos, &size), "UTF8_length4");
	ptr[12] = 0x110000; /* error */
	test(UTF8_length(pos, &size), "UTF8_length5");

	RETURN;
}

static int test_UTF16_length(void)
{
	addr pos;
	unicode *ptr;
	size_t size;

	strvect_heap(&pos, 15);
	GetStringUnicode(pos, (const unicode **)&ptr);
	ptr[0] = 0; /* 1 */
	ptr[1] = 1; /* 1 */
	ptr[2] = 0x7F; /* 1 */
	ptr[3] = 0x80; /* 1 */
	ptr[4] = 0x07FF; /* 1 */
	ptr[5] = 0x8000; /* 1 */
	ptr[6] = 0x8001; /* 1 */
	ptr[7] = 0xD7FF; /* 1 */
	ptr[8] = 0xE000; /* 1 */
	ptr[9] = 0xE001; /* 1 */
	ptr[10] = 0x010000; /* 2 */
	ptr[11] = 0x010001; /* 2 */
	ptr[12] = 0x0ABCDE; /* 2 */
	ptr[13] = 0x10FFFE; /* 2 */
	ptr[14] = 0x10FFFF; /* 2 */
	test(! UTF16_length(pos, &size), "UTF16_length1");
	test(size == 1*10+2*5, "UTF16_length2");

	ptr[12] = 0xD800; /* error */
	test(UTF16_length(pos, &size), "UTF16_length3");
	ptr[12] = 0xDFFF; /* error */
	test(UTF16_length(pos, &size), "UTF16_length4");
	ptr[12] = 0x110000; /* error */
	test(UTF16_length(pos, &size), "UTF16_length5");

	RETURN;
}

static int test_UTF8_make(void)
{
	byte data[100];
	addr pos;
	unicode *ptr;

	strvect_heap(&pos, 6);
	GetStringUnicode(pos, (const unicode **)&ptr);
	ptr[0] = 0; /* 1 */
	ptr[1] = 0x7F; /* 1 */
	ptr[2] = 0x80; /* 2 */
	ptr[3] = 0x0801; /* 3 */
	ptr[4] = 0x010000; /* 4 */
	ptr[5] = 0x10FFFF; /* 4 */
	test(! UTF8_make(data, pos), "UTF8_make1");
	test(data[0] == 0x00, "UTF8_make2");
	test(data[1] == 0x7F, "UTF8_make3");
	test(data[2] == 0xC2, "UTF8_make4");
	test(data[3] == 0x80, "UTF8_make5");
	test(data[4] == 0xE0, "UTF8_make6");
	test(data[5] == 0xA0, "UTF8_make7");
	test(data[6] == 0x81, "UTF8_make8");
	test(data[7] == 0xF0, "UTF8_make9");
	test(data[8] == 0x90, "UTF8_make10");
	test(data[9] == 0x80, "UTF8_make11");
	test(data[10] == 0x80, "UTF8_make12");
	test(data[11] == 0xF4, "UTF8_make13");
	test(data[12] == 0x8F, "UTF8_make14");
	test(data[13] == 0xBF, "UTF8_make15");
	test(data[14] == 0xBF, "UTF8_make16");
	test(data[15] == 0x00, "UTF8_make17");

	ptr[2] = 0xD800; /* error */
	test(UTF8_make(data, pos), "UTF8_make18");
	ptr[2] = 0xDFFF; /* error */
	test(UTF8_make(data, pos), "UTF8_make19");
	ptr[2] = 0x110000; /* error */
	test(UTF8_make(data, pos), "UTF8_make20");

	RETURN;
}

static int test_UTF16_make(void)
{
	byte16 data[100];
	addr pos;
	unicode *ptr;

	strvect_heap(&pos, 6);
	GetStringUnicode(pos, (const unicode **)&ptr);
	ptr[0] = 0; /* 2 */
	ptr[1] = 0x80; /* 2 */
	ptr[2] = 0xD7FF; /* 2 */
	ptr[3] = 0xE000; /* 2 */
	ptr[4] = 0x010000; /* 4 */
	ptr[5] = 0x10FFFF; /* 4 */
	test(! UTF16_make(data, pos), "UTF16_make1");
	test(data[0] == 0x0000, "UTF16_make2");
	test(data[1] == 0x0080, "UTF16_make3");
	test(data[2] == 0xD7FF, "UTF16_make4");
	test(data[3] == 0xE000, "UTF16_make5");
	test(data[4] == 0xD800, "UTF16_make6");
	test(data[5] == 0xDC00, "UTF16_make7");
	test(data[6] == 0xDBFF, "UTF16_make8");
	test(data[7] == 0xDFFF, "UTF16_make9");
	test(data[8] == 0x0000, "UTF16_make10");

	ptr[2] = 0xD800; /* error */
	test(UTF16_make(data, pos), "UTF16_make11");
	ptr[2] = 0xDFFF; /* error */
	test(UTF16_make(data, pos), "UTF16_make12");
	ptr[2] = 0x110000; /* error */
	test(UTF16_make(data, pos), "UTF16_make13");

	RETURN;
}

static int test_UTF8_buffer_clang(void)
{
	byte *data;
	addr pos, string;
	unicode *ptr;
	LocalRoot local;

	local = Local_Thread;
	strvect_heap(&string, 6);
	GetStringUnicode(string, (const unicode **)&ptr);
	ptr[0] = 0; /* 1 */
	ptr[1] = 0x7F; /* 1 */
	ptr[2] = 0x80; /* 2 */
	ptr[3] = 0x0801; /* 3 */
	ptr[4] = 0x010000; /* 4 */
	ptr[5] = 0x10FFFF; /* 4 */
	test(! UTF8_buffer_clang(local, &pos, string), "UTF8_buffer_clang1");
	posbody(pos, (addr *)&data);
	test(data[0] == 0x00, "UTF8_buffer_clang2");
	test(data[1] == 0x7F, "UTF8_buffer_clang3");
	test(data[2] == 0xC2, "UTF8_buffer_clang4");
	test(data[3] == 0x80, "UTF8_buffer_clang5");
	test(data[4] == 0xE0, "UTF8_buffer_clang6");
	test(data[5] == 0xA0, "UTF8_buffer_clang7");
	test(data[6] == 0x81, "UTF8_buffer_clang8");
	test(data[7] == 0xF0, "UTF8_buffer_clang9");
	test(data[8] == 0x90, "UTF8_buffer_clang10");
	test(data[9] == 0x80, "UTF8_buffer_clang11");
	test(data[10] == 0x80, "UTF8_buffer_clang12");
	test(data[11] == 0xF4, "UTF8_buffer_clang13");
	test(data[12] == 0x8F, "UTF8_buffer_clang14");
	test(data[13] == 0xBF, "UTF8_buffer_clang15");
	test(data[14] == 0xBF, "UTF8_buffer_clang16");
	test(data[15] == 0x00, "UTF8_buffer_clang17");

	ptr[2] = 0xD800; /* error */
	test(UTF8_buffer_clang(local, &pos, string), "UTF8_buffer_clang18");
	ptr[2] = 0xDFFF; /* error */
	test(UTF8_buffer_clang(local, &pos, string), "UTF8_buffer_clang19");
	ptr[2] = 0x110000; /* error */
	test(UTF8_buffer_clang(local, &pos, string), "UTF8_buffer_clang20");

	RETURN;
}

static int test_UTF16_buffer_clang(void)
{
	byte16 *data;
	addr pos, string;
	unicode *ptr;
	LocalRoot local;

	local = Local_Thread;
	strvect_heap(&string, 6);
	GetStringUnicode(string, (const unicode **)&ptr);
	ptr[0] = 0; /* 2 */
	ptr[1] = 0x80; /* 2 */
	ptr[2] = 0xD7FF; /* 2 */
	ptr[3] = 0xE000; /* 2 */
	ptr[4] = 0x010000; /* 4 */
	ptr[5] = 0x10FFFF; /* 4 */
	test(! UTF16_buffer_clang(local, &pos, string), "UTF16_buffer_clang1");
	posbody(pos, (addr *)&data);
	test(data[0] == 0x0000, "UTF16_buffer_clang2");
	test(data[1] == 0x0080, "UTF16_buffer_clang3");
	test(data[2] == 0xD7FF, "UTF16_buffer_clang4");
	test(data[3] == 0xE000, "UTF16_buffer_clang5");
	test(data[4] == 0xD800, "UTF16_buffer_clang6");
	test(data[5] == 0xDC00, "UTF16_buffer_clang7");
	test(data[6] == 0xDBFF, "UTF16_buffer_clang8");
	test(data[7] == 0xDFFF, "UTF16_buffer_clang9");
	test(data[8] == 0x0000, "UTF16_buffer_clang10");

	ptr[2] = 0xD800; /* error */
	test(UTF16_buffer_clang(local, &pos, string), "UTF16_buffer_clang11");
	ptr[2] = 0xDFFF; /* error */
	test(UTF16_buffer_clang(local, &pos, string), "UTF16_buffer_clang12");
	ptr[2] = 0x110000; /* error */
	test(UTF16_buffer_clang(local, &pos, string), "UTF16_buffer_clang13");

	RETURN;
}


/*
 *  unicode string
 */
static int test_UTF8_null_strlen(void)
{
	byte data[100];
	size_t size;

	data[0] = 0;
	test(! UTF8_null_strlen(data, &size), "UTF8_null_strlen1");
	test(size == 0, "UTF8_null_strlen2");

	data[0] = 0x10;
	data[1] = 0x40;
	data[2] = 0x7F;
	data[3] = 0x00;
	test(! UTF8_null_strlen(data, &size), "UTF8_null_strlen3");
	test(size == 3, "UTF8_null_strlen4");

	data[0] = 0x10;
	data[1] = 0xE0;
	data[2] = 0xA0;
	data[3] = 0x80;
	data[4] = 0xF0;
	data[5] = 0x90;
	data[6] = 0x80;
	data[7] = 0x81;
	data[8] = 0x00;
	test(! UTF8_null_strlen(data, &size), "UTF8_null_strlen5");
	test(size == 3, "UTF8_null_strlen6");

	RETURN;
}

static int test_UTF8_size_strlen(void)
{
	byte data[100];
	size_t size;

	data[0] = 0;
	test(! UTF8_size_strlen(data, 0, &size), "UTF8_size_strlen1");
	test(size == 0, "UTF8_size_strlen2");

	data[0] = 0x10;
	data[1] = 0x00;
	data[2] = 0x41;
	data[3] = 0x43;
	data[4] = 0x7F;
	data[5] = 0x00;
	test(! UTF8_size_strlen(data, 1, &size), "UTF8_size_strlen3");
	test(size == 1, "UTF8_size_strlen4");
	test(! UTF8_size_strlen(data, 2, &size), "UTF8_size_strlen5");
	test(size == 2, "UTF8_size_strlen6");
	test(! UTF8_size_strlen(data, 4, &size), "UTF8_size_strlen7");
	test(size == 4, "UTF8_size_strlen8");
	test(! UTF8_size_strlen(data, 5, &size), "UTF8_size_strlen9");
	test(size == 5, "UTF8_size_strlen10");

	data[0] = 0x10;
	data[1] = 0xE0;
	data[2] = 0xA0;
	data[3] = 0x80;
	data[4] = 0xF0;
	data[5] = 0x90;
	data[6] = 0x80;
	data[7] = 0x81;
	data[8] = 0x00;
	test(! UTF8_size_strlen(data, 4, &size), "UTF8_size_strlen11");
	test(size == 2, "UTF8_size_strlen12");
	test(! UTF8_size_strlen(data, 8, &size), "UTF8_size_strlen13");
	test(size == 3, "UTF8_size_strlen14");
	test(! UTF8_size_strlen(data, 9, &size), "UTF8_size_strlen15");
	test(size == 4, "UTF8_size_strlen16");

	RETURN;
}

static int test_UTF8_null_makeunicode(void)
{
	byte data[100];
	unicode check[100];

	check[0] = 999;
	data[0] = 0;
	test(! UTF8_null_makeunicode(check, data), "UTF8_null_makeunicode1");
	test(check[0] == 999, "UTF8_null_makeunicode2");

	data[0] = 0x10;
	data[1] = 0x40;
	data[2] = 0x7F;
	data[3] = 0x00;
	test(! UTF8_null_makeunicode(check, data), "UTF8_null_makeunicode3");
	test(check[0] == 0x10, "UTF8_null_makeunicode4");
	test(check[1] == 0x40, "UTF8_null_makeunicode5");
	test(check[2] == 0x7F, "UTF8_null_makeunicode6");

	data[0] = 0x10;
	data[1] = 0xE0;
	data[2] = 0xA0;
	data[3] = 0x80;
	data[4] = 0xF0;
	data[5] = 0x90;
	data[6] = 0x80;
	data[7] = 0x81;
	data[8] = 0x00;
	test(! UTF8_null_makeunicode(check, data), "UTF8_null_makeunicode7");
	test(check[0] == 0x10, "UTF8_null_makeunicode8");
	test(check[1] == 0x0800, "UTF8_null_makeunicode9");
	test(check[2] == 0x010001, "UTF8_null_makeunicode10");

	RETURN;
}

static int test_UTF8_size_makeunicode(void)
{
	byte data[100];
	unicode check[100];

	data[0] = 0;
	test(! UTF8_size_makeunicode(check, data, 0), "UTF8_size_makeunicode1");

	data[0] = 0x10;
	data[1] = 0x00;
	data[2] = 0x41;
	data[3] = 0x43;
	data[4] = 0x7F;
	data[5] = 0x00;
	cleartype(check);
	check[1] = 999;
	test(! UTF8_size_makeunicode(check, data, 1), "UTF8_size_makeunicode2");
	test(check[0] == 0x10, "UTF8_size_makeunicode3");
	test(check[1] == 999, "UTF8_size_makeunicode4");
	test(! UTF8_size_makeunicode(check, data, 2), "UTF8_size_makeunicode5");
	test(! UTF8_size_makeunicode(check, data, 4), "UTF8_size_makeunicode6");
	cleartype(check);
	test(! UTF8_size_makeunicode(check, data, 5), "UTF8_size_makeunicode7");
	test(check[0] == 0x10, "UTF8_size_makeunicode8");
	test(check[1] == 0x00, "UTF8_size_makeunicode9");
	test(check[2] == 0x41, "UTF8_size_makeunicode10");
	test(check[3] == 0x43, "UTF8_size_makeunicode11");
	test(check[4] == 0x7F, "UTF8_size_makeunicode12");

	data[0] = 0x10;
	data[1] = 0xE0;
	data[2] = 0xA0;
	data[3] = 0x80;
	data[4] = 0xF0;
	data[5] = 0x90;
	data[6] = 0x80;
	data[7] = 0x81;
	data[8] = 0x00;
	test(! UTF8_size_makeunicode(check, data, 4), "UTF8_size_makeunicode13");
	test(! UTF8_size_makeunicode(check, data, 8), "UTF8_size_makeunicode14");
	test(! UTF8_size_makeunicode(check, data, 9), "UTF8_size_makeunicode15");
	test(check[0] == 0x10, "UTF8_size_makeunicode16");
	test(check[1] == 0x0800, "UTF8_size_makeunicode17");
	test(check[2] == 0x010001, "UTF8_size_makeunicode18");

	RETURN;
}

static int test_UTF16_null_strlen(void)
{
	byte16 data[100];
	size_t size;

	data[0] = 0;
	test(! UTF16_null_strlen(data, &size), "UTF16_null_strlen1");
	test(size == 0, "UTF16_null_strlen2");

	data[0] = 0x10;
	data[1] = 0x4000;
	data[2] = 0x7F00;
	data[3] = 0x0000;
	test(! UTF16_null_strlen(data, &size), "UTF16_null_strlen3");
	test(size == 3, "UTF16_null_strlen4");

	data[0] = 0x10;
	data[1] = 0xD800;
	data[2] = 0xDC00;
	data[3] = 0x0040;
	data[4] = 0xD9AB;
	data[5] = 0xDECD;
	data[6] = 0x10;
	data[7] = 0x20;
	data[8] = 0x00;
	test(! UTF16_null_strlen(data, &size), "UTF16_null_strlen5");
	test(size == 6, "UTF16_null_strlen6");

	RETURN;
}

static int test_UTF16_size_strlen(void)
{
	byte16 data[100];
	size_t size;

	data[0] = 0;
	test(! UTF16_size_strlen(data, 0, &size), "UTF16_size_strlen1");
	test(size == 0, "UTF16_size_strlen2");

	data[0] = 0x10;
	data[1] = 0x00;
	data[2] = 0x41;
	data[3] = 0x4311;
	data[4] = 0x7FAB;
	data[5] = 0x00;
	test(! UTF16_size_strlen(data, 1, &size), "UTF16_size_strlen3");
	test(size == 1, "UTF16_size_strlen4");
	test(! UTF16_size_strlen(data, 2, &size), "UTF16_size_strlen5");
	test(size == 2, "UTF16_size_strlen6");
	test(! UTF16_size_strlen(data, 4, &size), "UTF16_size_strlen7");
	test(size == 4, "UTF16_size_strlen8");
	test(! UTF16_size_strlen(data, 5, &size), "UTF16_size_strlen9");
	test(size == 5, "UTF16_size_strlen10");

	data[0] = 0x10;
	data[1] = 0xD800;
	data[2] = 0xDC00;
	data[3] = 0x0040;
	data[4] = 0xD9AB;
	data[5] = 0xDECD;
	data[6] = 0x10;
	data[7] = 0x20;
	data[8] = 0x00;
	test(! UTF16_size_strlen(data, 3, &size), "UTF16_size_strlen11");
	test(size == 2, "UTF16_size_strlen12");
	test(! UTF16_size_strlen(data, 4, &size), "UTF16_size_strlen13");
	test(size == 3, "UTF16_size_strlen14");
	test(! UTF16_size_strlen(data, 8, &size), "UTF16_size_strlen15");
	test(size == 6, "UTF16_size_strlen16");

	RETURN;
}

static int test_UTF16_null_makeunicode(void)
{
	byte16 data[100];
	unicode check[100];

	check[0] = 999;
	data[0] = 0;
	test(! UTF16_null_makeunicode(check, data), "UTF16_null_makeunicode1");
	test(check[0] == 999, "UTF16_null_makeunicode2");

	data[0] = 0x10;
	data[1] = 0x4000;
	data[2] = 0x7F;
	data[3] = 0x00;
	test(! UTF16_null_makeunicode(check, data), "UTF16_null_makeunicode3");
	test(check[0] == 0x10, "UTF16_null_makeunicode4");
	test(check[1] == 0x4000, "UTF16_null_makeunicode5");
	test(check[2] == 0x7F, "UTF16_null_makeunicode6");

	data[0] = 0x10;
	data[1] = 0xD800;
	data[2] = 0xDC00;
	data[3] = 0x0040;
	data[4] = 0xDBFF;
	data[5] = 0xDFFF;
	data[6] = 0x10;
	data[7] = 0x20;
	data[8] = 0x00;
	test(! UTF16_null_makeunicode(check, data), "UTF16_null_makeunicode7");
	test(check[0] == 0x10, "UTF16_null_makeunicode8");
	test(check[1] == 0x010000, "UTF16_null_makeunicode9");
	test(check[2] == 0x40, "UTF16_null_makeunicode10");
	test(check[3] == 0x10FFFF, "UTF16_null_makeunicode11");
	test(check[4] == 0x10, "UTF16_null_makeunicode12");
	test(check[5] == 0x20, "UTF16_null_makeunicode13");

	RETURN;
}

static int test_UTF16_size_makeunicode(void)
{
	byte16 data[100];
	unicode check[100];

	data[0] = 0;
	test(! UTF16_size_makeunicode(check, data, 0), "UTF16_size_makeunicode1");

	data[0] = 0x10;
	data[1] = 0x00;
	data[2] = 0x41AB;
	data[3] = 0x43CD;
	data[4] = 0x7F;
	data[5] = 0x00;
	cleartype(check);
	check[1] = 999;
	test(! UTF16_size_makeunicode(check, data, 1), "UTF16_size_makeunicode2");
	test(check[0] == 0x10, "UTF16_size_makeunicode3");
	test(check[1] == 999, "UTF16_size_makeunicode4");
	test(! UTF16_size_makeunicode(check, data, 2), "UTF16_size_makeunicode5");
	test(! UTF16_size_makeunicode(check, data, 4), "UTF16_size_makeunicode6");
	cleartype(check);
	test(! UTF16_size_makeunicode(check, data, 5), "UTF16_size_makeunicode7");
	test(check[0] == 0x10, "UTF16_size_makeunicode8");
	test(check[1] == 0x00, "UTF16_size_makeunicode9");
	test(check[2] == 0x41AB, "UTF16_size_makeunicode10");
	test(check[3] == 0x43CD, "UTF16_size_makeunicode11");
	test(check[4] == 0x7F, "UTF16_size_makeunicode12");

	data[0] = 0x10;
	data[1] = 0xD800;
	data[2] = 0xDC00;
	data[3] = 0x40;
	data[4] = 0xDBFF;
	data[5] = 0xDFFF;
	data[6] = 0x10;
	data[7] = 0x20;
	data[8] = 0x00;
	test(! UTF16_size_makeunicode(check, data, 4), "UTF16_size_makeunicode13");
	test(! UTF16_size_makeunicode(check, data, 8), "UTF16_size_makeunicode14");
	test(! UTF16_size_makeunicode(check, data, 9), "UTF16_size_makeunicode15");
	test(check[0] == 0x10, "UTF16_null_makeunicode16");
	test(check[1] == 0x010000, "UTF16_null_makeunicode17");
	test(check[2] == 0x40, "UTF16_null_makeunicode18");
	test(check[3] == 0x10FFFF, "UTF16_null_makeunicode19");
	test(check[4] == 0x10, "UTF16_null_makeunicode20");
	test(check[5] == 0x20, "UTF16_null_makeunicode21");
	test(check[6] == 0x00, "UTF16_null_makeunicode22");

	RETURN;
}


/*
 *  main
 */
static int testbreak_encode(void)
{
	/* Byte Order Mark */
	TestBreak(test_readbom8_encode);
	TestBreak(test_readbom16_encode);
	TestBreak(test_readbom32_encode);
	TestBreak(test_writebom8_encode);
	TestBreak(test_writebom16_encode);
	TestBreak(test_writebom32_encode);
	/* read-char */
	TestBreak(test_read_char_ascii);
	TestBreak(test_read_char_utf8);
	TestBreak(test_read_char_utf16le);
	TestBreak(test_read_char_utf16be);
	TestBreak(test_read_char_utf32le);
	TestBreak(test_read_char_utf32be);
	/* read-hang */
	TestBreak(test_read_hang_ascii);
	TestBreak(test_read_hang_utf8);
	TestBreak(test_read_hang_utf16le);
	TestBreak(test_read_hang_utf16be);
	TestBreak(test_read_hang_utf32le);
	TestBreak(test_read_hang_utf32be);
	/* write-char */
	TestBreak(test_write_char_ascii);
	TestBreak(test_write_char_utf8);
	TestBreak(test_write_char_utf16le);
	TestBreak(test_write_char_utf16be);
	TestBreak(test_write_char_utf32le);
	TestBreak(test_write_char_utf32be);
	/* length-char */
	TestBreak(test_length_char_ascii);
	TestBreak(test_length_char_utf8);
	TestBreak(test_length_char_utf16);
	TestBreak(test_length_char_utf32);
	TestBreak(test_length_string_encode);
	/* unicode buffer */
	TestBreak(test_UTF8_length);
	TestBreak(test_UTF16_length);
	TestBreak(test_UTF8_make);
	TestBreak(test_UTF16_make);
	TestBreak(test_UTF8_buffer_clang);
	TestBreak(test_UTF16_buffer_clang);
	/* unicode string */
	TestBreak(test_UTF8_null_strlen);
	TestBreak(test_UTF8_size_strlen);
	TestBreak(test_UTF8_null_makeunicode);
	TestBreak(test_UTF8_size_makeunicode);
	TestBreak(test_UTF16_null_strlen);
	TestBreak(test_UTF16_size_strlen);
	TestBreak(test_UTF16_null_makeunicode);
	TestBreak(test_UTF16_size_makeunicode);

	return 0;
}

int test_encode(void)
{
	int result;
	lispcode code;
	Execute ptr;

	TITLE;

	freelisp();
	alloclisp(0, 0);
	lisp_info_enable = 0;
	ptr = Execute_Thread;
	begin_setjmp(ptr, &code);
	if (code_run_p(code)) {
		build_lisproot(ptr);
		build_constant();
		build_object();
		build_character();
		build_package();
		build_stream();
		build_symbol();
		build_clos(ptr);
		build_condition(ptr);
		build_type();
		build_syscall();
		build_common();
		build_reader();
		build_pathname();
		lisp_initialize = 1;
		result = testbreak_encode();
	}
	end_setjmp(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

