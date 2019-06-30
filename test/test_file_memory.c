#include "file_memory.c"
#include "character.h"
#include "clos.h"
#include "common.h"
#include "condition.h"
#include "constant.h"
#include "degrade.h"
#include "package.h"
#include "pathname.h"
#include "readtable.h"
#include "stream.h"
#include "symbol.h"
#include "syscall.h"
#include "type.h"
#include "type_table.h"

#define TESTFILE "_debug.txt"

static void pressfile(void)
{
	FILE *file;

	file = fopen(TESTFILE, "w");
	Check(file == 0, "fopen error");
	fclose(file);
}


static int test_init_filememory(void)
{
	struct filememory fm;

	aatype(fm);
	init_filememory(&fm);
	test(fm.index == 0, "init_filememory1");
	test(fm.size == 0, "init_filememory2");
	test(fm.ungetc == 0, "init_filememory3");

	RETURN;
}

static int test_init_input(void)
{
	struct filememory fm;

	aatype(fm);
	init_input(&fm, 0);
	test(fm.file == 0, "init_input1");
	test(fm.mode == filememory_normal, "init_input2");
	test(fm.direct == filememory_input, "init_input3");

	RETURN;
}

static int test_init_output(void)
{
	struct filememory fm;

	aatype(fm);
	init_output(&fm, 0);
	test(fm.file == 0, "init_output1");
	test(fm.mode == filememory_normal, "init_output2");
	test(fm.direct == filememory_output, "init_output3");

	RETURN;
}

static int test_standard_input_filememory(void)
{
	struct filememory fm;

	aatype(fm);
	standard_input_filememory(&fm);
	test(fm.direct == filememory_input, "standard_input_filememory1");

	RETURN;
}

static int test_standard_output_filememory(void)
{
	struct filememory fm;

	aatype(fm);
	standard_output_filememory(&fm);
	test(fm.direct == filememory_output, "standard_output_filememory1");

	RETURN;
}

static int test_standard_error_filememory(void)
{
	struct filememory fm;

	aatype(fm);
	standard_error_filememory(&fm);
	test(fm.direct == filememory_output, "standard_error_filememory1");

	RETURN;
}

static int test_open_input_filememory(void)
{
	int result;
	struct filememory fm;
	Execute ptr;
	addr name;

	strvect_char_heap(&name, TESTFILE);
	ptr = Execute_Thread;
	pressfile();
	result = open_input_filememory(ptr, &fm, name);
	test(result == 0, "open_input_filememory1");
	test(fm.direct == filememory_input, "open_input_filememory2");

	RETURN;
}

static int test_open_output_filememory(void)
{
	int result;
	struct filememory fm;
	Execute ptr;
	addr name;

	strvect_char_heap(&name, TESTFILE);
	ptr = Execute_Thread;
	pressfile();
	result = open_output_filememory(ptr, &fm, name, FileOutput_supersede);
	test(result == 0, "open_output_filememory1");
	test(fm.direct == filememory_output, "open_output_filememory2");

	RETURN;
}

static int test_close_filememory(void)
{
	int result;
	struct filememory fm;
	Execute ptr;
	addr name;

	strvect_char_heap(&name, TESTFILE);
	ptr = Execute_Thread;

	pressfile();
	result = open_input_filememory(ptr, &fm, name);
	test(result == 0, "close_filememory1");
	result = close_filememory(&fm);
	test(result == 0, "close_filememory2");
	test(fm.mode == filememory_close, "close_filememory3");

	pressfile();
	result = open_output_filememory(ptr, &fm, name, FileOutput_supersede);
	test(result == 0, "close_filememory4");
	result = close_filememory(&fm);
	test(result == 0, "close_filememory5");
	test(fm.mode == filememory_close, "close_filememory6");

	RETURN;
}

static int openinput(struct filememory *fm)
{
	addr name;
	strvect_char_heap(&name, TESTFILE);
	return open_input_filememory(Execute_Thread, fm, name);
}

static int writetest(const void *buffer, size_t size)
{
	size_t result;
	FILE *file;

	file = fopen(TESTFILE, "wb");
	if (file == NULL) return 1;
	result = fwrite(buffer, size, 1, file);
	fclose(file);

	return result != 1;
}

static int test_readforce(void)
{
	byte buffer[100];
	int result;
	size_t size;
	struct filememory fm;

	if (writetest("0123456789", 10)) return 1;
	if (openinput(&fm)) return 1;
	size = 0;
	aamemory(buffer, 10);
	result = readforce(fm.file, buffer, 5, &size);
	test(result == 0, "readforce1");
	test(size == 5, "readforce2");
	test(memcmp(buffer, "01234", 5) == 0, "readforce3");

	aamemory(buffer, 10);
	result = readforce(fm.file, buffer, 3, &size);
	test(result == 0, "readforce4");
	test(size == 3, "readforce5");
	test(memcmp(buffer, "567", 3) == 0, "readforce6");

	aamemory(buffer, 10);
	result = readforce(fm.file, buffer, 10, &size);
	test(result == 0, "readforce7");
	test(size == 2, "readforce8");
	test(memcmp(buffer, "89", 2) == 0, "readforce9");

	result = readforce(fm.file, buffer, 10, &size);
	test(result == 1, "readforce10");
	close_filememory(&fm);

	RETURN;
}

static int openoutput(struct filememory *fm)
{
	addr name;
	strvect_char_heap(&name, TESTFILE);
	return open_output_filememory(Execute_Thread, fm, name, FileOutput_supersede);
}

static int test_writeforce(void)
{
	byte buffer[100];
	int result;
	size_t size;
	struct filememory fm;

	if (writetest("aabbccddeeffgghh", 10)) return 1;
	if (openoutput(&fm)) return 1;
	size = 0;
	result = writeforce(fm.file, (const byte *)"Hello", 5, &size);
	test(result == 0, "writeforce1");
	test(size == 5, "writeforce2");
	result = writeforce(fm.file, (const byte *)"abc", 3, &size);
	test(result == 0, "writeforce3");
	test(size == 3, "writeforce4");
	close_filememory(&fm);

	openinput(&fm);
	aamemory(buffer, 100);
	result = readforce(fm.file, buffer, 100, &size);
	test(result == 0 && memcmp(buffer, "Helloabc", 8) == 0, "writeforce5");
	close_filememory(&fm);

	RETURN;
}

static int test_readforce_nonblocking(void)
{
	byte buffer[100];
	int result;
	size_t size;
	struct filememory fm;

	if (writetest("0123456789", 10)) return 1;
	if (openinput(&fm)) return 1;
	size = 0;
	aamemory(buffer, 10);
	result = readforce_nonblocking(fm.file, buffer, 5, &size);
	test(result == 0, "readforce_nonblocking1");
	test(size == 5, "readforce_nonblocking2");
	test(memcmp(buffer, "01234", 5) == 0, "readforce_nonblocking3");

	aamemory(buffer, 10);
	result = readforce_nonblocking(fm.file, buffer, 3, &size);
	test(result == 0, "readforce_nonblocking4");
	test(size == 3, "readforce_nonblocking5");
	test(memcmp(buffer, "567", 3) == 0, "readforce_nonblocking6");

	aamemory(buffer, 10);
	result = readforce_nonblocking(fm.file, buffer, 10, &size);
	test(result == 0, "readforce_nonblocking7");
	test(size == 2, "readforce_nonblocking8");
	test(memcmp(buffer, "89", 2) == 0, "readforce_nonblocking9");

	result = readforce_nonblocking(fm.file, buffer, 10, &size);
	test(result == 1, "readforce_nonblocking10");
	close_filememory(&fm);

	RETURN;
}

static int test_readnext_large(void)
{
	byte buffer[100];
	int result;
	size_t size;
	struct filememory fm;

	if (writetest("0123456789", 10)) return 1;
	if (openinput(&fm)) return 1;
	size = 0;
	result = readnext_large(&fm, buffer, 100, &size);
	test(result == 0, "readnext_large1");
	test(size == 10, "readnext_large2");
	result = readnext_large(&fm, buffer, 100, &size);
	test(result == 1, "readnext_large3");
	test(fm.mode == filememory_end, "readnext_large4");
	close_filememory(&fm);

	RETURN;
}

static int test_readnext_small(void)
{
	byte buffer[100];
	int result;
	size_t size;
	struct filememory fm;

	if (writetest("0123", 4)) return 1;
	if (openinput(&fm)) return 1;
	size = 0;
	aamemory(buffer, 100);
	result = readnext_small(&fm, buffer, 3, &size);
	test(result == 0, "readnext_small1");
	test(size == 3, "readnext_small2");
	test(fm.index == 3, "readnext_small3");
	test(fm.size == 4, "readnext_small4");
	test(fm.mode == filememory_normal, "readnext_small5");
	test(memcmp(buffer, "012", 3) == 0, "readnext_small6");
	close_filememory(&fm);

	if (openinput(&fm)) return 1;
	size = 0;
	aamemory(buffer, 100);
	result = readnext_small(&fm, buffer, 100, &size);
	test(result == 0, "readnext_small7");
	test(size == 4, "readnext_small8");
	test(fm.index == 0, "readnext_small9");
	test(fm.mode == filememory_normal, "readnext_small10");
	test(memcmp(buffer, "0123", 4) == 0, "readnext_small11");
	close_filememory(&fm);

	RETURN;
}

static int test_readnext(void)
{
	byte buffer[100];
	int result;
	size_t size;
	struct filememory fm;

	if (writetest("0123456789", 10)) return 1;
	if (openinput(&fm)) return 1;
	size = 0;
	result = readnext(&fm, buffer, 100, &size);
	test(result == 0, "readnext1");
	test(size == 10, "readnext2");
	result = readnext(&fm, buffer, 100, &size);
	test(result == 1, "readnext3");
	test(fm.mode == filememory_end, "readnext4");
	close_filememory(&fm);

	RETURN;
}

static int test_readbuffer(void)
{
	byte buffer[100];
	int result;
	size_t size;
	struct filememory fm;

	if (writetest("0123456789", 10)) return 1;
	if (openinput(&fm)) return 1;
	memcpy(fm.buffer, "abcdefghijkl", 8);
	fm.index = 2;
	fm.size = 8;
	size = 0;
	aamemory(buffer, 100);
	result = readbuffer(&fm, buffer, 5, &size);
	test(result == 0, "readbuffer1");
	test(size == 5, "readbuffer2");
	test(fm.index == 7, "readbuffer3");
	test(memcmp(buffer, "cdefg", 5) == 0, "readbuffer4");
	result = readbuffer(&fm, buffer, 1, &size);
	test(result == 0, "readbuffer5");
	test(size == 1, "readbuffer6");
	test(fm.index == 0, "readbuffer7");
	test(memcmp(buffer, "h", 1) == 0, "readbuffer8");

	memcpy(fm.buffer, "abcdefghijkl", 8);
	fm.index = 2;
	fm.size = 8;
	result = readbuffer(&fm, buffer, 10, &size);
	test(result == 0, "readbuffer9");
	test(size == 10, "readbuffer10");
	test(memcmp(buffer, "cdefgh0123", 10) == 0, "readbuffer11");
	close_filememory(&fm);

	RETURN;
}

static int test_readungetc(void)
{
	byte buffer[100];
	size_t size;
	int result;
	struct filememory fm;

	if (writetest("0123456789", 10)) return 1;
	if (openinput(&fm)) return 1;
	fm.ungetc = 3;
	fm.ungetc_value[2] = 'A';
	fm.ungetc_value[1] = 'B';
	fm.ungetc_value[0] = 'C';
	fm.index = 2;
	fm.size = 8;
	memcpy(fm.buffer, "abcdefghijkl", 8);
	aamemory(buffer, 100);
	result = readungetc(&fm, buffer, 1, &size);
	test(result == 0, "readungetc1");
	test(size == 1, "readungetc2");
	test(buffer[0] == 'A', "readungetc3");
	test(fm.ungetc == 2, "readungetc4");

	aamemory(buffer, 100);
	result = readungetc(&fm, buffer, 2, &size);
	test(result == 0, "readungetc5");
	test(size == 2, "readungetc6");
	test(memcmp(buffer, "BC", 2) == 0, "readungetc7");

	fm.ungetc = 3;
	fm.ungetc_value[2] = 'A';
	fm.ungetc_value[1] = 'B';
	fm.ungetc_value[0] = 'C';
	fm.index = 2;
	fm.size = 8;
	memcpy(fm.buffer, "abcdefghijkl", 8);
	aamemory(buffer, 100);
	result = readungetc(&fm, buffer, 3+6+5, &size);
	test(result == 0, "readungetc8");
	test(size == 3+6+5, "readungetc9");
	test(memcmp(buffer, "ABCcdefgh01234", 3+6+5) == 0, "readungetc10");
	close_filememory(&fm);

	RETURN;
}

static int test_read_normal(void)
{
	byte buffer[100];
	int result;
	size_t size;
	struct filememory fm;

	if (writetest("0123456789", 10)) return 1;
	if (openinput(&fm)) return 1;
	result = read_normal(&fm, buffer, 2, &size);
	test(result == 0, "read_normal1");
	test(size == 2, "read_normal2");
	test(memcmp(buffer, "01", 2) == 0, "read_normal3");
	fm.ungetc_value[1] = 'A';
	fm.ungetc_value[0] = 'B';
	fm.ungetc = 2;
	result = read_normal(&fm, buffer, 4, &size);
	test(result == 0, "read_normal4");
	test(size == 4, "read_normal5");
	test(memcmp(buffer, "AB23", 4) == 0, "read_normal6");
	close_filememory(&fm);

	RETURN;
}

static int test_read_filememory(void)
{
	byte buffer[100];
	int result;
	size_t size;
	struct filememory fm;

	if (writetest("0123456789", 10)) return 1;
	if (openinput(&fm)) return 1;
	result = read_filememory(&fm, buffer, 2, &size);
	test(result == 0, "read_filememory1");
	test(size == 2, "read_filememory2");
	test(memcmp(buffer, "01", 2) == 0, "read_filememory3");
	fm.ungetc_value[1] = 'A';
	fm.ungetc_value[0] = 'B';
	fm.ungetc = 2;
	result = read_filememory(&fm, buffer, 4, &size);
	test(result == 0, "read_filememory4");
	test(size == 4, "read_filememory5");
	test(memcmp(buffer, "AB23", 4) == 0, "read_filememory6");
	close_filememory(&fm);

	fm.direct = filememory_output;
	test(read_filememory(&fm, buffer, 10, &size) < 0, "read_filememory7");
	fm.direct = filememory_input;
	fm.mode = filememory_end;
	test(read_filememory(&fm, buffer, 10, &size) == 1, "read_filememory8");
	fm.mode = filememory_error;
	test(read_filememory(&fm, buffer, 10, &size) < 0, "read_filememory9");

	RETURN;
}

static int test_read_normal_force(void)
{
	byte buffer[100];
	int result;
	size_t size;
	struct filememory fm;

	if (writetest("0123456789", 10)) return 1;
	if (openinput(&fm)) return 1;
	size = 999;
	result = read_normal_force(&fm, buffer, 2, &size);
	test(result == 0, "read_normal_force1");
	test(size == 2, "read_normal_force2");
	test(memcmp(buffer, "01", 2) == 0, "read_normal_force3");
	fm.ungetc_value[1] = 'A';
	fm.ungetc_value[0] = 'B';
	fm.ungetc = 2;
	result = read_normal_force(&fm, buffer, 4, &size);
	test(result == 0, "read_normal_force4");
	test(size == 4, "read_normal_force5");
	test(memcmp(buffer, "AB23", 4) == 0, "read_normal_force6");
	close_filememory(&fm);

	RETURN;
}

static int test_readforce_filememory(void)
{
	byte buffer[100];
	int result;
	size_t size;
	struct filememory fm;

	if (writetest("0123456789", 10)) return 1;
	if (openinput(&fm)) return 1;
	result = readforce_filememory(&fm, buffer, 2, &size);
	test(result == 0, "readforce_filememory1");
	test(size == 2, "readforce_filememory2");
	test(memcmp(buffer, "01", 2) == 0, "readforce_filememory3");
	fm.ungetc_value[1] = 'A';
	fm.ungetc_value[0] = 'B';
	fm.ungetc = 2;
	result = readforce_filememory(&fm, buffer, 4, &size);
	test(result == 0, "readforce_filememory4");
	test(size == 4, "readforce_filememory5");
	test(memcmp(buffer, "AB23", 4) == 0, "readforce_filememory6");
	close_filememory(&fm);

	fm.direct = filememory_output;
	test(readforce_filememory(&fm, buffer, 10, &size) < 0, "readforce_filememory7");
	fm.direct = filememory_input;
	fm.mode = filememory_end;
	test(readforce_filememory(&fm, buffer, 10, &size) == 1, "readforce_filememory8");
	fm.mode = filememory_error;
	test(readforce_filememory(&fm, buffer, 10, &size) < 0, "readforce_filememory9");

	RETURN;
}

static int test_readnext_nonblocking_large(void)
{
	byte buffer[100];
	int result;
	size_t size;
	struct filememory fm;

	if (writetest("0123456789", 10)) return 1;
	if (openinput(&fm)) return 1;
	size = 0;
	result = readnext_nonblocking_large(&fm, buffer, 100, &size);
	test(result == 0, "readnext_nonblocking_large1");
	test(size == 10, "readnext_nonblocking_large2");
	result = readnext_nonblocking_large(&fm, buffer, 100, &size);
	test(result == 1, "readnext_nonblocking_large3");
	test(fm.mode == filememory_end, "readnext_nonblocking_large4");
	close_filememory(&fm);

	RETURN;
}

static int test_readnext_nonblocking_small(void)
{
	byte buffer[100];
	int result;
	size_t size;
	struct filememory fm;

	if (writetest("0123", 4)) return 1;
	if (openinput(&fm)) return 1;
	size = 0;
	aamemory(buffer, 100);
	result = readnext_nonblocking_small(&fm, buffer, 3, &size);
	test(result == 0, "readnext_nonblocking_small1");
	test(size == 3, "readnext_nonblocking_small2");
	test(fm.index == 3, "readnext_nonblocking_small3");
	test(fm.size == 4, "readnext_nonblocking_small4");
	test(fm.mode == filememory_normal, "readnext_nonblocking_small5");
	test(memcmp(buffer, "012", 3) == 0, "readnext_nonblocking_small6");
	close_filememory(&fm);

	if (openinput(&fm)) return 1;
	size = 0;
	aamemory(buffer, 100);
	result = readnext_nonblocking_small(&fm, buffer, 100, &size);
	test(result == 0, "readnext_nonblocking_small7");
	test(size == 4, "readnext_nonblocking_small8");
	test(fm.index == 0, "readnext_nonblocking_small9");
	test(fm.mode == filememory_normal, "readnext_nonblocking_small10");
	test(memcmp(buffer, "0123", 4) == 0, "readnext_nonblocking_small11");
	close_filememory(&fm);

	RETURN;
}

static int test_readnext_nonblocking(void)
{
	byte buffer[100];
	int result;
	size_t size;
	struct filememory fm;

	if (writetest("0123456789", 10)) return 1;
	if (openinput(&fm)) return 1;
	size = 0;
	result = readnext_nonblocking(&fm, buffer, 100, &size);
	test(result == 0, "readnext_nonblocking1");
	test(size == 10, "readnext_nonblocking2");
	result = readnext_nonblocking(&fm, buffer, 100, &size);
	test(result == 1, "readnext_nonblocking3");
	test(fm.mode == filememory_end, "readnext_nonblocking4");
	close_filememory(&fm);

	RETURN;
}

static int test_readbuffer_nonblocking(void)
{
	byte buffer[100];
	int result;
	size_t size;
	struct filememory fm;

	if (writetest("0123456789", 10)) return 1;
	if (openinput(&fm)) return 1;
	memcpy(fm.buffer, "abcdefghijkl", 8);
	fm.index = 2;
	fm.size = 8;
	size = 0;
	aamemory(buffer, 100);
	result = readbuffer_nonblocking(&fm, buffer, 5, &size);
	test(result == 0, "readbuffer1");
	test(size == 5, "readbuffer_nonblocking2");
	test(fm.index == 7, "readbuffer_nonblocking3");
	test(memcmp(buffer, "cdefg", 5) == 0, "readbuffer_nonblocking4");
	result = readbuffer_nonblocking(&fm, buffer, 1, &size);
	test(result == 0, "readbuffer_nonblocking5");
	test(size == 1, "readbuffer_nonblocking6");
	test(fm.index == 0, "readbuffer_nonblocking7");
	test(memcmp(buffer, "h", 1) == 0, "readbuffer_nonblocking8");

	memcpy(fm.buffer, "abcdefghijkl", 8);
	fm.index = 2;
	fm.size = 8;
	result = readbuffer_nonblocking(&fm, buffer, 10, &size);
	test(result == 0, "readbuffer_nonblocking9");
	test(size == 10, "readbuffer_nonblocking10");
	test(memcmp(buffer, "cdefgh0123", 10) == 0, "readbuffer_nonblocking11");
	close_filememory(&fm);

	RETURN;
}

static int test_readungetc_nonblocking(void)
{
	byte buffer[100];
	size_t size;
	int result;
	struct filememory fm;

	if (writetest("0123456789", 10)) return 1;
	if (openinput(&fm)) return 1;
	fm.ungetc = 3;
	fm.ungetc_value[2] = 'A';
	fm.ungetc_value[1] = 'B';
	fm.ungetc_value[0] = 'C';
	fm.index = 2;
	fm.size = 8;
	memcpy(fm.buffer, "abcdefghijkl", 8);
	aamemory(buffer, 100);
	result = readungetc_nonblocking(&fm, buffer, 1, &size);
	test(result == 0, "readungetc_nonblocking1");
	test(size == 1, "readungetc_nonblocking2");
	test(buffer[0] == 'A', "readungetc_nonblocking3");
	test(fm.ungetc == 2, "readungetc_nonblocking4");

	aamemory(buffer, 100);
	result = readungetc_nonblocking(&fm, buffer, 2, &size);
	test(result == 0, "readungetc_nonblocking5");
	test(size == 2, "readungetc_nonblocking6");
	test(memcmp(buffer, "BC", 2) == 0, "readungetc_nonblocking7");

	fm.ungetc = 3;
	fm.ungetc_value[2] = 'A';
	fm.ungetc_value[1] = 'B';
	fm.ungetc_value[0] = 'C';
	fm.index = 2;
	fm.size = 8;
	memcpy(fm.buffer, "abcdefghijkl", 8);
	aamemory(buffer, 100);
	result = readungetc_nonblocking(&fm, buffer, 3+6+5, &size);
	test(result == 0, "readungetc_nonblocking8");
	test(size == 3+6+5, "readungetc_nonblocking9");
	test(memcmp(buffer, "ABCcdefgh01234", 3+6+5) == 0, "readungetc_nonblocking10");
	close_filememory(&fm);

	RETURN;
}

static int test_read_nonblocking(void)
{
	byte buffer[100];
	int result;
	size_t size;
	struct filememory fm;

	if (writetest("0123456789", 10)) return 1;
	if (openinput(&fm)) return 1;
	result = read_nonblocking(&fm, buffer, 2, &size);
	test(result == 0, "read_nonblocking1");
	test(size == 2, "read_nonblocking2");
	test(memcmp(buffer, "01", 2) == 0, "read_nonblocking3");
	fm.ungetc_value[1] = 'A';
	fm.ungetc_value[0] = 'B';
	fm.ungetc = 2;
	result = read_nonblocking(&fm, buffer, 4, &size);
	test(result == 0, "read_nonblocking4");
	test(size == 4, "read_nonblocking5");
	test(memcmp(buffer, "AB23", 4) == 0, "read_nonblocking6");
	close_filememory(&fm);

	RETURN;
}

static int test_read_nonblocking_filememory(void)
{
	byte buffer[100];
	int result;
	size_t size;
	struct filememory fm;

	if (writetest("0123456789", 10)) return 1;
	if (openinput(&fm)) return 1;
	result = read_nonblocking_filememory(&fm, buffer, 2, &size);
	test(result == 0, "read_nonblocking_filememory1");
	test(size == 2, "read_nonblocking_filememory2");
	test(memcmp(buffer, "01", 2) == 0, "read_nonblocking_filememory3");
	fm.ungetc_value[1] = 'A';
	fm.ungetc_value[0] = 'B';
	fm.ungetc = 2;
	result = read_nonblocking_filememory(&fm, buffer, 4, &size);
	test(result == 0, "read_nonblocking_filememory4");
	test(size == 4, "read_nonblocking_filememory5");
	test(memcmp(buffer, "AB23", 4) == 0, "read_nonblocking_filememory6");
	close_filememory(&fm);

	fm.direct = filememory_output;
	test(read_nonblocking_filememory(&fm, buffer, 10, &size) < 0,
			"read_nonblocking_filememory7");
	fm.direct = filememory_input;
	fm.mode = filememory_end;
	test(read_nonblocking_filememory(&fm, buffer, 10, &size) == 1,
			"read_nonblocking_filememory8");
	fm.mode = filememory_error;
	test(read_nonblocking_filememory(&fm, buffer, 10, &size) < 0,
			"read_nonblocking_filememory8");

	RETURN;
}

static int test_readbuffer_small(void)
{
	byte buffer[100];
	int result;
	size_t size;
	struct filememory fm;

	if (writetest("0123456789", 10)) return 1;
	if (openinput(&fm)) return 1;
	memcpy(fm.buffer, "abcdefghijkl", 8);
	fm.index = 2;
	fm.size = 8;
	size = 0;
	aamemory(buffer, 100);
	result = readbuffer_small(&fm, buffer, 5, &size);
	test(result == 0, "readbuffer_small1");
	test(size == 5, "readbuffer_small2");
	test(fm.index == 7, "readbuffer_small3");
	test(memcmp(buffer, "cdefg", 5) == 0, "readbuffer_small4");
	result = readbuffer_small(&fm, buffer, 1, &size);
	test(result == 0, "readbuffer_small5");
	test(size == 1, "readbuffer_small6");
	test(fm.index == 0, "readbuffer_small7");
	test(memcmp(buffer, "h", 1) == 0, "readbuffer_small8");

	memcpy(fm.buffer, "abcdefghijkl", 8);
	fm.index = 2;
	fm.size = 8;
	result = readbuffer_small(&fm, buffer, 10, &size);
	test(result == 0, "readbuffer_small9");
	test(size == 10, "readbuffer_small10");
	test(memcmp(buffer, "cdefgh0123", 10) == 0, "readbuffer_small11");
	close_filememory(&fm);

	RETURN;
}

static int test_readungetc_small(void)
{
	byte buffer[100];
	size_t size;
	int result;
	struct filememory fm;

	if (writetest("0123456789", 10)) return 1;
	if (openinput(&fm)) return 1;
	fm.ungetc = 3;
	fm.ungetc_value[2] = 'A';
	fm.ungetc_value[1] = 'B';
	fm.ungetc_value[0] = 'C';
	fm.index = 2;
	fm.size = 8;
	memcpy(fm.buffer, "abcdefghijkl", 8);
	aamemory(buffer, 100);
	result = readungetc_small(&fm, buffer, 1, &size);
	test(result == 0, "readungetc_small1");
	test(size == 1, "readungetc_small2");
	test(buffer[0] == 'A', "readungetc_small3");
	test(fm.ungetc == 2, "readungetc_small4");

	aamemory(buffer, 100);
	result = readungetc_small(&fm, buffer, 2, &size);
	test(result == 0, "readungetc_small5");
	test(size == 2, "readungetc_small6");
	test(memcmp(buffer, "BC", 2) == 0, "readungetc_small7");

	fm.ungetc = 3;
	fm.ungetc_value[2] = 'A';
	fm.ungetc_value[1] = 'B';
	fm.ungetc_value[0] = 'C';
	fm.index = 2;
	fm.size = 8;
	memcpy(fm.buffer, "abcdefghijkl", 8);
	aamemory(buffer, 100);
	result = readungetc_small(&fm, buffer, 3+6+5, &size);
	test(result == 0, "readungetc_small8");
	test(size == 3+6+5, "readungetc_small9");
	test(memcmp(buffer, "ABCcdefgh01234", 3+6+5) == 0, "readungetc_small10");
	close_filememory(&fm);

	RETURN;
}

static int test_getc_normal(void)
{
	byte c;
	int result;
	struct filememory fm;

	if (writetest("0123456789", 10)) return 1;
	if (openinput(&fm)) return 1;
	fm.ungetc = 1;
	fm.ungetc_value[0] = 'C';
	memcpy(fm.buffer, "abcdefgh", 8);
	fm.index = 7;
	fm.size = 8;
	fm.buffer[0] = 'D';
	c = 0;
	result = getc_normal(&fm, &c);
	test(result == 0, "getc_normal1");
	test(c == 'C', "getc_normal2");
	result = getc_normal(&fm, &c);
	test(result == 0, "getc_normal3");
	test(c == 'h', "getc_normal4");
	result = getc_normal(&fm, &c);
	test(result == 0, "getc_normal5");
	test(c == '0', "getc_normal6");
	close_filememory(&fm);

	RETURN;
}

static int test_getc_filememory(void)
{
	byte c;
	int result;
	struct filememory fm;

	if (writetest("0123456789", 10)) return 1;
	if (openinput(&fm)) return 1;
	fm.ungetc = 1;
	fm.ungetc_value[0] = 'C';
	memcpy(fm.buffer, "abcdefgh", 8);
	fm.index = 7;
	fm.size = 8;
	fm.buffer[0] = 'D';
	c = 0;
	result = getc_filememory(&fm, &c);
	test(result == 0, "getc_filememory1");
	test(c == 'C', "getc_filememory2");
	result = getc_filememory(&fm, &c);
	test(result == 0, "getc_filememory3");
	test(c == 'h', "getc_filememory4");
	result = getc_filememory(&fm, &c);
	test(result == 0, "getc_filememory5");
	test(c == '0', "getc_filememory6");
	close_filememory(&fm);

	fm.direct = filememory_output;
	test(getc_filememory(&fm, &c) < 0, "getc_filememory7");
	fm.direct = filememory_input;
	fm.mode = filememory_end;
	test(getc_filememory(&fm, &c) == 1, "getc_filememory8");
	fm.mode = filememory_error;
	test(getc_filememory(&fm, &c) < 0, "getc_filememory9");

	RETURN;
}

static int test_readbuffer_nonblocking_small(void)
{
	byte buffer[100];
	int result;
	size_t size;
	struct filememory fm;

	if (writetest("0123456789", 10)) return 1;
	if (openinput(&fm)) return 1;
	memcpy(fm.buffer, "abcdefghijkl", 8);
	fm.index = 2;
	fm.size = 8;
	size = 0;
	aamemory(buffer, 100);
	result = readbuffer_nonblocking_small(&fm, buffer, 5, &size);
	test(result == 0, "readbuffer_nonblocking_small1");
	test(size == 5, "readbuffer_nonblocking_small2");
	test(fm.index == 7, "readbuffer_nonblocking_small3");
	test(memcmp(buffer, "cdefg", 5) == 0, "readbuffer_nonblocking_small4");
	result = readbuffer_nonblocking_small(&fm, buffer, 1, &size);
	test(result == 0, "readbuffer_nonblocking_small5");
	test(size == 1, "readbuffer_nonblocking_small6");
	test(fm.index == 0, "readbuffer_nonblocking_small7");
	test(memcmp(buffer, "h", 1) == 0, "readbuffer_nonblocking_small8");

	memcpy(fm.buffer, "abcdefghijkl", 8);
	fm.index = 2;
	fm.size = 8;
	result = readbuffer_nonblocking_small(&fm, buffer, 10, &size);
	test(result == 0, "readbuffer_nonblocking_small9");
	test(size == 10, "readbuffer_nonblocking_small10");
	test(memcmp(buffer, "cdefgh0123", 10) == 0, "readbuffer_nonblocking_small11");
	close_filememory(&fm);

	RETURN;
}

static int test_readungetc_nonblocking_small(void)
{
	byte buffer[100];
	size_t size;
	int result;
	struct filememory fm;

	if (writetest("0123456789", 10)) return 1;
	if (openinput(&fm)) return 1;
	fm.ungetc = 3;
	fm.ungetc_value[2] = 'A';
	fm.ungetc_value[1] = 'B';
	fm.ungetc_value[0] = 'C';
	fm.index = 2;
	fm.size = 8;
	memcpy(fm.buffer, "abcdefghijkl", 8);
	aamemory(buffer, 100);
	result = readungetc_nonblocking_small(&fm, buffer, 1, &size);
	test(result == 0, "readungetc_nonblocking_small1");
	test(size == 1, "readungetc_nonblocking_small2");
	test(buffer[0] == 'A', "readungetc_nonblocking_small3");
	test(fm.ungetc == 2, "readungetc_nonblocking_small4");

	aamemory(buffer, 100);
	result = readungetc_nonblocking_small(&fm, buffer, 2, &size);
	test(result == 0, "readungetc_nonblocking_small5");
	test(size == 2, "readungetc_nonblocking_small6");
	test(memcmp(buffer, "BC", 2) == 0, "readungetc_nonblocking_small7");

	fm.ungetc = 3;
	fm.ungetc_value[2] = 'A';
	fm.ungetc_value[1] = 'B';
	fm.ungetc_value[0] = 'C';
	fm.index = 2;
	fm.size = 8;
	memcpy(fm.buffer, "abcdefghijkl", 8);
	aamemory(buffer, 100);
	result = readungetc_nonblocking_small(&fm, buffer, 3+6+5, &size);
	test(result == 0, "readungetc_nonblocking_small8");
	test(size == 3+6+5, "readungetc_nonblocking_small9");
	test(memcmp(buffer, "ABCcdefgh01234", 3+6+5) == 0, "readungetc_nonblocking_small10");
	close_filememory(&fm);

	RETURN;
}

static int test_getc_nonblocking(void)
{
	byte c;
	int result;
	size_t size;
	struct filememory fm;

	if (writetest("0123456789", 10)) return 1;
	if (openinput(&fm)) return 1;
	fm.ungetc = 1;
	fm.ungetc_value[0] = 'C';
	memcpy(fm.buffer, "abcdefgh", 8);
	fm.index = 7;
	fm.size = 8;
	fm.buffer[0] = 'D';
	c = 0;
	size = 0;
	result = getc_nonblocking(&fm, &c, &size);
	test(result == 0, "getc_nonblocking1");
	test(c == 'C', "getc_nonblocking2");
	test(size == 1, "getc_nonblocking3");
	result = getc_nonblocking(&fm, &c, &size);
	test(result == 0, "getc_nonblocking4");
	test(c == 'h', "getc_nonblocking5");
	test(size == 1, "getc_nonblocking6");
	result = getc_nonblocking(&fm, &c, &size);
	test(result == 0, "getc_nonblocking7");
	test(c == '0', "getc_nonblocking8");
	test(size == 1, "getc_nonblocking9");
	close_filememory(&fm);

	RETURN;
}

static int test_getc_nonblocking_filememory(void)
{
	byte c;
	int result;
	size_t size;
	struct filememory fm;

	if (writetest("0123456789", 10)) return 1;
	if (openinput(&fm)) return 1;
	fm.ungetc = 1;
	fm.ungetc_value[0] = 'C';
	memcpy(fm.buffer, "abcdefgh", 8);
	fm.index = 7;
	fm.size = 8;
	fm.buffer[0] = 'D';
	c = 0;
	size = 0;
	result = getc_nonblocking_filememory(&fm, &c, &size);
	test(result == 0, "getc_nonblocking_filememory1");
	test(c == 'C', "getc_nonblocking_filememory2");
	test(size == 1, "getc_nonblocking_filememory3");
	result = getc_nonblocking_filememory(&fm, &c, &size);
	test(result == 0, "getc_nonblocking_filememory4");
	test(c == 'h', "getc_nonblocking_filememory5");
	test(size == 1, "getc_nonblocking_filememory6");
	result = getc_nonblocking_filememory(&fm, &c, &size);
	test(result == 0, "getc_nonblocking_filememory7");
	test(c == '0', "getc_nonblocking_filememory8");
	test(size == 1, "getc_nonblocking_filememory9");
	close_filememory(&fm);

	fm.direct = filememory_output;
	test(getc_nonblocking_filememory(&fm, &c, &size) < 0,
			"getc_nonblocking_filememory10");
	fm.direct = filememory_input;
	fm.mode = filememory_end;
	test(getc_nonblocking_filememory(&fm, &c, &size) == 1,
			"getc_nonblocking_filememory11");
	fm.mode = filememory_error;
	test(getc_nonblocking_filememory(&fm, &c, &size) < 0,
			"getc_nonblocking_filememory12");

	RETURN;
}

static int test_ungetc_filememory(void)
{
	byte c;
	int result;
	struct filememory fm;

	if (writetest("0123456789", 10)) return 1;
	if (openinput(&fm)) return 1;
	c = 0;
	getc_filememory(&fm, &c);
	test(c == '0', "ungetc_filememory1");
	result = ungetc_filememory(&fm, 'Z');
	test(result == 0, "ungetc_filememory2");
	getc_filememory(&fm, &c);
	test(c == 'Z', "ungetc_filememory3");
	getc_filememory(&fm, &c);
	test(c == '1', "ungetc_filememory4");
	close_filememory(&fm);

	RETURN;
}

static int readfile(void *ptr, size_t size, size_t *ret)
{
	FILE *file;
	size_t temp;

	file = fopen(TESTFILE, "rb");
	if (file == NULL) return 1;
	temp = fread(ptr, 1, size, file);
	if (ret) *ret = temp;
	fclose(file);

	return 0;
}

static int test_flush_write(void)
{
	byte buffer[100];
	int result;
	size_t size;
	struct filememory fm;

	if (openoutput(&fm)) return 1;
	memcpy(fm.buffer, "XYZ", 3);
	fm.index = 3;
	result = flush_write(&fm);
	test(result == 0, "flush_write1");
	test(fm.index == 0, "flush_write2");

	flush_arch(fm.file);
	if (readfile(buffer, 100, &size)) return 1;
	test(size == 3, "flush_write3");
	test(memcmp(buffer, "XYZ", 3) == 0, "flush_write4");
	close_filememory(&fm);

	RETURN;
}

static int test_writenormal(void)
{
#ifdef LISP_DEBUG
	byte buffer[100];
	int result;
	size_t size;
	struct filememory fm;

	if (openoutput(&fm)) return 1;
	size = 0;
	memcpy(fm.buffer, "abcdefg", 7);
	fm.index = 7;
	result = writenormal(&fm, (const byte *)"12345678901234567890", 20, &size);
	test(result == 0, "writenormal1");
	test(size == 20, "writenormal2");
	test(fm.index == 0, "writenormal3");
	flush_arch(fm.file);
	if (readfile(buffer, 100, &size)) return 1;
	test(size == 27, "writenormal4");
	test(memcmp(buffer, "abcdefg12345678901234567890", 27) == 0, "writenormal5");

	result = writenormal(&fm, (const byte *)"123", 3, &size);
	test(result == 0, "writenormal6");
	test(size == 3, "writenormal6");
	test(memcmp(fm.buffer, "123", 3) == 0, "writenormal7");
	test(fm.index == 3, "writenormal8");
	result = writenormal(&fm, (const byte *)"1234567", 7, &size);
	test(result == 0, "writenormal8");
	test(size == 7, "writenormal9");
	test(memcmp(fm.buffer, "67", 2) == 0, "writenormal10");
	test(fm.index == 2, "writenormal11");

	flush_arch(fm.file);
	if (readfile(buffer, 100, &size)) return 1;
	test(size == 35, "writenormal12");
	test(memcmp(buffer, "abcdefg1234567890123456789012312345", 35) == 0,
			"writenormal13");
	close_filememory(&fm);

	RETURN;
#else
	return 0;
#endif
}

static int test_write_filememory(void)
{
	byte buffer[100];
	int result;
	size_t size;
	struct filememory fm;

	if (openoutput(&fm)) return 1;
	size = 0;
	result = write_filememory(&fm, "Hello", 5, &size);
	test(result == 0, "write_filememor1");
	test(size == 5, "write_filememory2");
	close_filememory(&fm);
	if (readfile(buffer, 100, &size)) return 1;
	test(size == 5, "write_filememory3");
	test(memcmp(buffer, "Hello", 5) == 0, "write_filememory4");

	if (openoutput(&fm)) return 1;
	fm.direct = filememory_input;
	result = write_filememory(&fm, "Hello", 5, &size);
	test(result == -1, "write_filememory5");
	close_filememory(&fm);

	RETURN;
}

static int test_putcnormal(void)
{
	struct filememory fm;

	if (openoutput(&fm)) return 1;
	close_filememory(&fm);

	return 0;
}


/*
 *  main
 */
static int testbreak_file_memory(void)
{
	TestBreak(test_init_filememory);
	TestBreak(test_init_input);
	TestBreak(test_init_output);
	TestBreak(test_standard_input_filememory);
	TestBreak(test_standard_output_filememory);
	TestBreak(test_standard_error_filememory);
	TestBreak(test_open_input_filememory);
	TestBreak(test_open_output_filememory);
	TestBreak(test_close_filememory);
	TestBreak(test_readforce);
	TestBreak(test_writeforce);
	TestBreak(test_readforce_nonblocking);
	TestBreak(test_readnext_large);
	TestBreak(test_readnext_small);
	TestBreak(test_readnext);
	TestBreak(test_readbuffer);
	TestBreak(test_readungetc);
	TestBreak(test_read_normal);
	TestBreak(test_read_filememory);
	TestBreak(test_read_normal_force);
	TestBreak(test_readforce_filememory);
	TestBreak(test_readnext_nonblocking_large);
	TestBreak(test_readnext_nonblocking_small);
	TestBreak(test_readnext_nonblocking);
	TestBreak(test_readbuffer_nonblocking);
	TestBreak(test_readungetc_nonblocking);
	TestBreak(test_read_nonblocking);
	TestBreak(test_read_nonblocking_filememory);
	TestBreak(test_readbuffer_small);
	TestBreak(test_readungetc_small);
	TestBreak(test_getc_normal);
	TestBreak(test_getc_filememory);
	TestBreak(test_readbuffer_nonblocking_small);
	TestBreak(test_readungetc_nonblocking_small);
	TestBreak(test_getc_nonblocking);
	TestBreak(test_getc_nonblocking_filememory);
	TestBreak(test_ungetc_filememory);
	TestBreak(test_flush_write);
	TestBreak(test_writenormal);
	TestBreak(test_write_filememory);
	TestBreak(test_putcnormal);

	return 0;
}

int test_file_memory(void)
{
	int result;
	lispcode code;
	Execute ptr;

	TITLE;

	freelisp();
	alloclisp(0, 0);
	lisp_info_enable = 0;
	ptr = Execute_Thread;
	begin_code(ptr, &code);
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
		build_readtable();
		build_pathname();
		lisp_initialize = 1;
		result = testbreak_file_memory();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

