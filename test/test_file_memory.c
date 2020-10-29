#include "file_memory.c"
#include "character.h"
#include "clos.h"
#include "common.h"
#include "condition.h"
#include "constant.h"
#include "degrade.h"
#include "package.h"
#include "pathname.h"
#include "reader.h"
#include "stream.h"
#include "strvect.h"
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

static int test_init_input_filememory(void)
{
	struct filememory fm;

	aatype(fm);
	init_input_filememory(&fm, 0);
	test(fm.file == 0, "init_input_filememory1");
	test(fm.mode == filememory_normal, "init_input_filememory2");
	test(fm.direct == filememory_input, "init_input_filememory3");

	RETURN;
}

static int test_init_output_filememory(void)
{
	struct filememory fm;

	aatype(fm);
	init_output_filememory(&fm, 0);
	test(fm.file == 0, "init_output_filememory1");
	test(fm.mode == filememory_normal, "init_output_filememory2");
	test(fm.direct == filememory_output, "init_output_filememory3");

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
	int result, check;
	struct filememory fm;
	LocalRoot local;
	addr name;

	strvect_char_heap(&name, TESTFILE);
	local = Local_Thread;
	pressfile();
	result = open_input_filememory_(local, &fm, name, &check);
	test(result == 0, "open_input_filememory1");
	test(check == 0, "open_input_filememory2");
	test(fm.direct == filememory_input, "open_input_filememory3");

	RETURN;
}

static int test_open_output_filememory(void)
{
	int result, check;
	struct filememory fm;
	LocalRoot local;
	addr name;

	strvect_char_heap(&name, TESTFILE);
	local = Local_Thread;
	pressfile();
	result = open_output_filememory_(local, &fm, name, FileOutput_supersede, &check);
	test(result == 0, "open_output_filememory1");
	test(check == 0, "open_output_filememory2");
	test(fm.direct == filememory_output, "open_output_filememory3");

	RETURN;
}

static int test_close_filememory(void)
{
	int result, check;
	struct filememory fm;
	LocalRoot local;
	addr name;

	strvect_char_heap(&name, TESTFILE);
	local = Local_Thread;

	pressfile();
	result = open_input_filememory_(local, &fm, name, &check);
	test(result == 0, "close_filememory1");
	test(check == 0, "close_filememory2");
	result = close_filememory(&fm);
	test(result == 0, "close_filememory3");
	test(fm.mode == filememory_close, "close_filememory4");

	pressfile();
	result = open_output_filememory_(local, &fm, name, FileOutput_supersede, &check);
	test(result == 0, "close_filememory5");
	test(check == 0, "close_filememory6");
	result = close_filememory(&fm);
	test(result == 0, "close_filememory7");
	test(fm.mode == filememory_close, "close_filememory8");

	RETURN;
}

static int openinput(filestream fm)
{
	int check;
	addr name;

	check = 0;
	strvect_char_heap(&name, TESTFILE);
	Error(open_input_filememory_(Local_Thread, fm, name, &check));

	return check;
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
	result = fm_readforce(&fm, buffer, 5, &size);
	test(result == 0, "readforce1");
	test(size == 5, "readforce2");
	test(memcmp(buffer, "01234", 5) == 0, "readforce3");

	aamemory(buffer, 10);
	result = fm_readforce(&fm, buffer, 3, &size);
	test(result == 0, "readforce4");
	test(size == 3, "readforce5");
	test(memcmp(buffer, "567", 3) == 0, "readforce6");

	aamemory(buffer, 10);
	result = fm_readforce(&fm, buffer, 10, &size);
	test(result == 0, "readforce7");
	test(size == 2, "readforce8");
	test(memcmp(buffer, "89", 2) == 0, "readforce9");

	result = fm_readforce(&fm, buffer, 10, &size);
	test(result == 1, "readforce10");
	close_filememory(&fm);

	RETURN;
}

static int openoutput(filestream fm)
{
	int check;
	addr name;

	check = 0;
	strvect_char_heap(&name, TESTFILE);
	Error(open_output_filememory_(Local_Thread,
				fm, name, FileOutput_supersede, &check));

	return check;
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
	result = fm_writeforce(&fm, (const byte *)"Hello", 5, &size);
	test(result == 0, "writeforce1");
	test(size == 5, "writeforce2");
	result = fm_writeforce(&fm, (const byte *)"abc", 3, &size);
	test(result == 0, "writeforce3");
	test(size == 3, "writeforce4");
	close_filememory(&fm);

	openinput(&fm);
	aamemory(buffer, 100);
	result = fm_readforce(&fm, buffer, 100, &size);
	test(result == 0 && memcmp(buffer, "Helloabc", 8) == 0, "writeforce5");
	close_filememory(&fm);

	RETURN;
}

static int test_readforce_nonblock(void)
{
	byte buffer[100];
	int result;
	size_t size;
	struct filememory fm;

	if (writetest("0123456789", 10)) return 1;
	if (openinput(&fm)) return 1;
	size = 0;
	aamemory(buffer, 10);
	result = fm_readforce_nonblock(&fm, buffer, 5, &size);
	test(result == 0, "readforce_nonblock1");
	test(size == 5, "readforce_nonblock2");
	test(memcmp(buffer, "01234", 5) == 0, "readforce_nonblock3");

	aamemory(buffer, 10);
	result = fm_readforce_nonblock(&fm, buffer, 3, &size);
	test(result == 0, "readforce_nonblock4");
	test(size == 3, "readforce_nonblock5");
	test(memcmp(buffer, "567", 3) == 0, "readforce_nonblock6");

	aamemory(buffer, 10);
	result = fm_readforce_nonblock(&fm, buffer, 10, &size);
	test(result == 0, "readforce_nonblock7");
	test(size == 2, "readforce_nonblock8");
	test(memcmp(buffer, "89", 2) == 0, "readforce_nonblock9");

	result = fm_readforce_nonblock(&fm, buffer, 10, &size);
	test(result == 1, "readforce_nonblock10");
	close_filememory(&fm);

	RETURN;
}

static int test_fm_readnext_large(void)
{
	byte buffer[100];
	int result;
	size_t size;
	struct filememory fm;

	if (writetest("0123456789", 10)) return 1;
	if (openinput(&fm)) return 1;
	size = 0;
	result = fm_readnext_large(&fm, buffer, 100, &size);
	test(result == 0, "fm_readnext_large1");
	test(size == 10, "fm_readnext_large2");
	result = fm_readnext_large(&fm, buffer, 100, &size);
	test(result == 1, "fm_readnext_large3");
	test(fm.mode == filememory_end, "fm_readnext_large4");
	close_filememory(&fm);

	RETURN;
}

static int test_fm_readnext_small(void)
{
	byte buffer[100];
	int result;
	size_t size;
	struct filememory fm;

	if (writetest("0123", 4)) return 1;
	if (openinput(&fm)) return 1;
	size = 0;
	aamemory(buffer, 100);
	result = fm_readnext_small(&fm, buffer, 3, &size);
	test(result == 0, "fm_readnext_small1");
	test(size == 3, "fm_readnext_small2");
	test(fm.index == 3, "fm_readnext_small3");
	test(fm.size == 4, "fm_readnext_small4");
	test(fm.mode == filememory_normal, "fm_readnext_small5");
	test(memcmp(buffer, "012", 3) == 0, "fm_readnext_small6");
	close_filememory(&fm);

	if (openinput(&fm)) return 1;
	size = 0;
	aamemory(buffer, 100);
	result = fm_readnext_small(&fm, buffer, 100, &size);
	test(result == 0, "fm_readnext_small7");
	test(size == 4, "fm_readnext_small8");
	test(fm.index == 0, "fm_readnext_small9");
	test(fm.mode == filememory_normal, "fm_readnext_small10");
	test(memcmp(buffer, "0123", 4) == 0, "fm_readnext_small11");
	close_filememory(&fm);

	RETURN;
}

static int test_fm_readnext(void)
{
	byte buffer[100];
	int result;
	size_t size;
	struct filememory fm;

	if (writetest("0123456789", 10)) return 1;
	if (openinput(&fm)) return 1;
	size = 0;
	result = fm_readnext(&fm, buffer, 100, &size);
	test(result == 0, "fm_readnext1");
	test(size == 10, "fm_readnext2");
	result = fm_readnext(&fm, buffer, 100, &size);
	test(result == 1, "fm_readnext3");
	test(fm.mode == filememory_end, "fm_readnext4");
	close_filememory(&fm);

	RETURN;
}

static int test_fm_readbuffer(void)
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
	result = fm_readbuffer(&fm, buffer, 5, &size);
	test(result == 0, "fm_readbuffer1");
	test(size == 5, "fm_readbuffer2");
	test(fm.index == 7, "fm_readbuffer3");
	test(memcmp(buffer, "cdefg", 5) == 0, "fm_readbuffer4");
	result = fm_readbuffer(&fm, buffer, 1, &size);
	test(result == 0, "fm_readbuffer5");
	test(size == 1, "fm_readbuffer6");
	test(fm.index == 0, "fm_readbuffer7");
	test(memcmp(buffer, "h", 1) == 0, "fm_readbuffer8");

	memcpy(fm.buffer, "abcdefghijkl", 8);
	fm.index = 2;
	fm.size = 8;
	result = fm_readbuffer(&fm, buffer, 10, &size);
	test(result == 0, "fm_readbuffer9");
	test(size == 10, "fm_readbuffer10");
	test(memcmp(buffer, "cdefgh0123", 10) == 0, "fm_readbuffer11");
	close_filememory(&fm);

	RETURN;
}

static int test_fm_readungetc(void)
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
	result = fm_readungetc(&fm, buffer, 1, &size);
	test(result == 0, "fm_readungetc1");
	test(size == 1, "fm_readungetc2");
	test(buffer[0] == 'A', "fm_readungetc3");
	test(fm.ungetc == 2, "fm_readungetc4");

	aamemory(buffer, 100);
	result = fm_readungetc(&fm, buffer, 2, &size);
	test(result == 0, "fm_readungetc5");
	test(size == 2, "fm_readungetc6");
	test(memcmp(buffer, "BC", 2) == 0, "fm_readungetc7");

	fm.ungetc = 3;
	fm.ungetc_value[2] = 'A';
	fm.ungetc_value[1] = 'B';
	fm.ungetc_value[0] = 'C';
	fm.index = 2;
	fm.size = 8;
	memcpy(fm.buffer, "abcdefghijkl", 8);
	aamemory(buffer, 100);
	result = fm_readungetc(&fm, buffer, 3+6+5, &size);
	test(result == 0, "fm_readungetc8");
	test(size == 3+6+5, "fm_readungetc9");
	test(memcmp(buffer, "ABCcdefgh01234", 3+6+5) == 0, "fm_readungetc10");
	close_filememory(&fm);

	RETURN;
}

static int test_fm_read_normal(void)
{
	byte buffer[100];
	int result;
	size_t size;
	struct filememory fm;

	if (writetest("0123456789", 10)) return 1;
	if (openinput(&fm)) return 1;
	result = fm_read_normal(&fm, buffer, 2, &size);
	test(result == 0, "fm_read_normal1");
	test(size == 2, "fm_read_normal2");
	test(memcmp(buffer, "01", 2) == 0, "fm_read_normal3");
	fm.ungetc_value[1] = 'A';
	fm.ungetc_value[0] = 'B';
	fm.ungetc = 2;
	result = fm_read_normal(&fm, buffer, 4, &size);
	test(result == 0, "fm_read_normal4");
	test(size == 4, "fm_read_normal5");
	test(memcmp(buffer, "AB23", 4) == 0, "fm_read_normal6");
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

static int test_fm_read_normal_force(void)
{
	byte buffer[100];
	int result;
	size_t size;
	struct filememory fm;

	if (writetest("0123456789", 10)) return 1;
	if (openinput(&fm)) return 1;
	size = 999;
	result = fm_read_normal_force(&fm, buffer, 2, &size);
	test(result == 0, "fm_read_normal_force1");
	test(size == 2, "fm_read_normal_force2");
	test(memcmp(buffer, "01", 2) == 0, "fm_read_normal_force3");
	fm.ungetc_value[1] = 'A';
	fm.ungetc_value[0] = 'B';
	fm.ungetc = 2;
	result = fm_read_normal_force(&fm, buffer, 4, &size);
	test(result == 0, "fm_read_normal_force4");
	test(size == 4, "fm_read_normal_force5");
	test(memcmp(buffer, "AB23", 4) == 0, "fm_read_normal_force6");
	close_filememory(&fm);

	RETURN;
}

static int test_readf_filememory(void)
{
	byte buffer[100];
	int result;
	size_t size;
	struct filememory fm;

	if (writetest("0123456789", 10)) return 1;
	if (openinput(&fm)) return 1;
	result = readf_filememory(&fm, buffer, 2, &size);
	test(result == 0, "readf_filememory1");
	test(size == 2, "readf_filememory2");
	test(memcmp(buffer, "01", 2) == 0, "readf_filememory3");
	fm.ungetc_value[1] = 'A';
	fm.ungetc_value[0] = 'B';
	fm.ungetc = 2;
	result = readf_filememory(&fm, buffer, 4, &size);
	test(result == 0, "readf_filememory4");
	test(size == 4, "readf_filememory5");
	test(memcmp(buffer, "AB23", 4) == 0, "readf_filememory6");
	close_filememory(&fm);

	fm.direct = filememory_output;
	test(readf_filememory(&fm, buffer, 10, &size) < 0, "readf_filememory7");
	fm.direct = filememory_input;
	fm.mode = filememory_end;
	test(readf_filememory(&fm, buffer, 10, &size) == 1, "readf_filememory8");
	fm.mode = filememory_error;
	test(readf_filememory(&fm, buffer, 10, &size) < 0, "readf_filememory9");

	RETURN;
}

static int test_fm_readnext_nonblock_large(void)
{
	byte buffer[100];
	int result;
	size_t size;
	struct filememory fm;

	if (writetest("0123456789", 10)) return 1;
	if (openinput(&fm)) return 1;
	size = 0;
	result = fm_readnext_nonblock_large(&fm, buffer, 100, &size);
	test(result == 0, "fm_readnext_nonblock_large1");
	test(size == 10, "fm_readnext_nonblock_large2");
	result = fm_readnext_nonblock_large(&fm, buffer, 100, &size);
	test(result == 1, "fm_readnext_nonblock_large3");
	test(fm.mode == filememory_end, "fm_readnext_nonblock_large4");
	close_filememory(&fm);

	RETURN;
}

static int test_fm_readnext_nonblock_small(void)
{
	byte buffer[100];
	int result;
	size_t size;
	struct filememory fm;

	if (writetest("0123", 4)) return 1;
	if (openinput(&fm)) return 1;
	size = 0;
	aamemory(buffer, 100);
	result = fm_readnext_nonblock_small(&fm, buffer, 3, &size);
	test(result == 0, "fm_readnext_nonblock_small1");
	test(size == 3, "fm_readnext_nonblock_small2");
	test(fm.index == 3, "fm_readnext_nonblock_small3");
	test(fm.size == 4, "fm_readnext_nonblock_small4");
	test(fm.mode == filememory_normal, "fm_readnext_nonblock_small5");
	test(memcmp(buffer, "012", 3) == 0, "fm_readnext_nonblock_small6");
	close_filememory(&fm);

	if (openinput(&fm)) return 1;
	size = 0;
	aamemory(buffer, 100);
	result = fm_readnext_nonblock_small(&fm, buffer, 100, &size);
	test(result == 0, "fm_readnext_nonblock_small7");
	test(size == 4, "fm_readnext_nonblock_small8");
	test(fm.index == 0, "fm_readnext_nonblock_small9");
	test(fm.mode == filememory_normal, "fm_readnext_nonblock_small10");
	test(memcmp(buffer, "0123", 4) == 0, "fm_readnext_nonblock_small11");
	close_filememory(&fm);

	RETURN;
}

static int test_fm_readnext_nonblock(void)
{
	byte buffer[100];
	int result;
	size_t size;
	struct filememory fm;

	if (writetest("0123456789", 10)) return 1;
	if (openinput(&fm)) return 1;
	size = 0;
	result = fm_readnext_nonblock(&fm, buffer, 100, &size);
	test(result == 0, "fm_readnext_nonblock1");
	test(size == 10, "fm_readnext_nonblock2");
	result = fm_readnext_nonblock(&fm, buffer, 100, &size);
	test(result == 1, "fm_readnext_nonblock3");
	test(fm.mode == filememory_end, "fm_readnext_nonblock4");
	close_filememory(&fm);

	RETURN;
}

static int test_fm_readbuffer_nonblock(void)
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
	result = fm_readbuffer_nonblock(&fm, buffer, 5, &size);
	test(result == 0, "fm_readbuffer1");
	test(size == 5, "fm_readbuffer_nonblock2");
	test(fm.index == 7, "fm_readbuffer_nonblock3");
	test(memcmp(buffer, "cdefg", 5) == 0, "fm_readbuffer_nonblock4");
	result = fm_readbuffer_nonblock(&fm, buffer, 1, &size);
	test(result == 0, "fm_readbuffer_nonblock5");
	test(size == 1, "fm_readbuffer_nonblock6");
	test(fm.index == 0, "fm_readbuffer_nonblock7");
	test(memcmp(buffer, "h", 1) == 0, "fm_readbuffer_nonblock8");

	memcpy(fm.buffer, "abcdefghijkl", 8);
	fm.index = 2;
	fm.size = 8;
	result = fm_readbuffer_nonblock(&fm, buffer, 10, &size);
	test(result == 0, "fm_readbuffer_nonblock9");
	test(size == 10, "fm_readbuffer_nonblock10");
	test(memcmp(buffer, "cdefgh0123", 10) == 0, "fm_readbuffer_nonblock11");
	close_filememory(&fm);

	RETURN;
}

static int test_fm_readungetc_nonblock(void)
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
	result = fm_readungetc_nonblock(&fm, buffer, 1, &size);
	test(result == 0, "fm_readungetc_nonblock1");
	test(size == 1, "fm_readungetc_nonblock2");
	test(buffer[0] == 'A', "fm_readungetc_nonblock3");
	test(fm.ungetc == 2, "fm_readungetc_nonblock4");

	aamemory(buffer, 100);
	result = fm_readungetc_nonblock(&fm, buffer, 2, &size);
	test(result == 0, "fm_readungetc_nonblock5");
	test(size == 2, "fm_readungetc_nonblock6");
	test(memcmp(buffer, "BC", 2) == 0, "fm_readungetc_nonblock7");

	fm.ungetc = 3;
	fm.ungetc_value[2] = 'A';
	fm.ungetc_value[1] = 'B';
	fm.ungetc_value[0] = 'C';
	fm.index = 2;
	fm.size = 8;
	memcpy(fm.buffer, "abcdefghijkl", 8);
	aamemory(buffer, 100);
	result = fm_readungetc_nonblock(&fm, buffer, 3+6+5, &size);
	test(result == 0, "fm_readungetc_nonblock8");
	test(size == 3+6+5, "fm_readungetc_nonblock9");
	test(memcmp(buffer, "ABCcdefgh01234", 3+6+5) == 0, "fm_readungetc_nonblock10");
	close_filememory(&fm);

	RETURN;
}

static int test_read_nonblock(void)
{
	byte buffer[100];
	int result;
	size_t size;
	struct filememory fm;

	if (writetest("0123456789", 10)) return 1;
	if (openinput(&fm)) return 1;
	result = read_nonblock(&fm, buffer, 2, &size);
	test(result == 0, "read_nonblock1");
	test(size == 2, "read_nonblock2");
	test(memcmp(buffer, "01", 2) == 0, "read_nonblock3");
	fm.ungetc_value[1] = 'A';
	fm.ungetc_value[0] = 'B';
	fm.ungetc = 2;
	result = read_nonblock(&fm, buffer, 4, &size);
	test(result == 0, "read_nonblock4");
	test(size == 4, "read_nonblock5");
	test(memcmp(buffer, "AB23", 4) == 0, "read_nonblock6");
	close_filememory(&fm);

	RETURN;
}

static int test_read_nonblock_filememory(void)
{
	byte buffer[100];
	int result;
	size_t size;
	struct filememory fm;

	if (writetest("0123456789", 10)) return 1;
	if (openinput(&fm)) return 1;
	result = read_nonblock_filememory(&fm, buffer, 2, &size);
	test(result == 0, "read_nonblock_filememory1");
	test(size == 2, "read_nonblock_filememory2");
	test(memcmp(buffer, "01", 2) == 0, "read_nonblock_filememory3");
	fm.ungetc_value[1] = 'A';
	fm.ungetc_value[0] = 'B';
	fm.ungetc = 2;
	result = read_nonblock_filememory(&fm, buffer, 4, &size);
	test(result == 0, "read_nonblock_filememory4");
	test(size == 4, "read_nonblock_filememory5");
	test(memcmp(buffer, "AB23", 4) == 0, "read_nonblock_filememory6");
	close_filememory(&fm);

	fm.direct = filememory_output;
	test(read_nonblock_filememory(&fm, buffer, 10, &size) < 0,
			"read_nonblock_filememory7");
	fm.direct = filememory_input;
	fm.mode = filememory_end;
	test(read_nonblock_filememory(&fm, buffer, 10, &size) == 1,
			"read_nonblock_filememory8");
	fm.mode = filememory_error;
	test(read_nonblock_filememory(&fm, buffer, 10, &size) < 0,
			"read_nonblock_filememory8");

	RETURN;
}

static int test_fm_readbuffer_small(void)
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
	result = fm_readbuffer_small(&fm, buffer, 5, &size);
	test(result == 0, "fm_readbuffer_small1");
	test(size == 5, "fm_readbuffer_small2");
	test(fm.index == 7, "fm_readbuffer_small3");
	test(memcmp(buffer, "cdefg", 5) == 0, "fm_readbuffer_small4");
	result = fm_readbuffer_small(&fm, buffer, 1, &size);
	test(result == 0, "fm_readbuffer_small5");
	test(size == 1, "fm_readbuffer_small6");
	test(fm.index == 0, "fm_readbuffer_small7");
	test(memcmp(buffer, "h", 1) == 0, "fm_readbuffer_small8");

	memcpy(fm.buffer, "abcdefghijkl", 8);
	fm.index = 2;
	fm.size = 8;
	result = fm_readbuffer_small(&fm, buffer, 10, &size);
	test(result == 0, "fm_readbuffer_small9");
	test(size == 10, "fm_readbuffer_small10");
	test(memcmp(buffer, "cdefgh0123", 10) == 0, "fm_readbuffer_small11");
	close_filememory(&fm);

	RETURN;
}

static int test_fm_readungetc_small(void)
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
	result = fm_readungetc_small(&fm, buffer, 1, &size);
	test(result == 0, "fm_readungetc_small1");
	test(size == 1, "fm_readungetc_small2");
	test(buffer[0] == 'A', "fm_readungetc_small3");
	test(fm.ungetc == 2, "fm_readungetc_small4");

	aamemory(buffer, 100);
	result = fm_readungetc_small(&fm, buffer, 2, &size);
	test(result == 0, "fm_readungetc_small5");
	test(size == 2, "fm_readungetc_small6");
	test(memcmp(buffer, "BC", 2) == 0, "fm_readungetc_small7");

	fm.ungetc = 3;
	fm.ungetc_value[2] = 'A';
	fm.ungetc_value[1] = 'B';
	fm.ungetc_value[0] = 'C';
	fm.index = 2;
	fm.size = 8;
	memcpy(fm.buffer, "abcdefghijkl", 8);
	aamemory(buffer, 100);
	result = fm_readungetc_small(&fm, buffer, 3+6+5, &size);
	test(result == 0, "fm_readungetc_small8");
	test(size == 3+6+5, "fm_readungetc_small9");
	test(memcmp(buffer, "ABCcdefgh01234", 3+6+5) == 0, "fm_readungetc_small10");
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

static int test_fm_readbuffer_nonblock_small(void)
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
	result = fm_readbuffer_nonblock_small(&fm, buffer, 5, &size);
	test(result == 0, "fm_readbuffer_nonblock_small1");
	test(size == 5, "fm_readbuffer_nonblock_small2");
	test(fm.index == 7, "fm_readbuffer_nonblock_small3");
	test(memcmp(buffer, "cdefg", 5) == 0, "fm_readbuffer_nonblock_small4");
	result = fm_readbuffer_nonblock_small(&fm, buffer, 1, &size);
	test(result == 0, "fm_readbuffer_nonblock_small5");
	test(size == 1, "fm_readbuffer_nonblock_small6");
	test(fm.index == 0, "fm_readbuffer_nonblock_small7");
	test(memcmp(buffer, "h", 1) == 0, "fm_readbuffer_nonblock_small8");

	memcpy(fm.buffer, "abcdefghijkl", 8);
	fm.index = 2;
	fm.size = 8;
	result = fm_readbuffer_nonblock_small(&fm, buffer, 10, &size);
	test(result == 0, "fm_readbuffer_nonblock_small9");
	test(size == 10, "fm_readbuffer_nonblock_small10");
	test(memcmp(buffer, "cdefgh0123", 10) == 0, "fm_readbuffer_nonblock_small11");
	close_filememory(&fm);

	RETURN;
}

static int test_fm_readungetc_nonblock_small(void)
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
	result = fm_readungetc_nonblock_small(&fm, buffer, 1, &size);
	test(result == 0, "fm_readungetc_nonblock_small1");
	test(size == 1, "fm_readungetc_nonblock_small2");
	test(buffer[0] == 'A', "fm_readungetc_nonblock_small3");
	test(fm.ungetc == 2, "fm_readungetc_nonblock_small4");

	aamemory(buffer, 100);
	result = fm_readungetc_nonblock_small(&fm, buffer, 2, &size);
	test(result == 0, "fm_readungetc_nonblock_small5");
	test(size == 2, "fm_readungetc_nonblock_small6");
	test(memcmp(buffer, "BC", 2) == 0, "fm_readungetc_nonblock_small7");

	fm.ungetc = 3;
	fm.ungetc_value[2] = 'A';
	fm.ungetc_value[1] = 'B';
	fm.ungetc_value[0] = 'C';
	fm.index = 2;
	fm.size = 8;
	memcpy(fm.buffer, "abcdefghijkl", 8);
	aamemory(buffer, 100);
	result = fm_readungetc_nonblock_small(&fm, buffer, 3+6+5, &size);
	test(result == 0, "fm_readungetc_nonblock_small8");
	test(size == 3+6+5, "fm_readungetc_nonblock_small9");
	test(memcmp(buffer, "ABCcdefgh01234", 3+6+5) == 0, "fm_readungetc_nonblock_small10");
	close_filememory(&fm);

	RETURN;
}

static int test_getc_nonblock(void)
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
	result = getc_nonblock(&fm, &c, &size);
	test(result == 0, "getc_nonblock1");
	test(c == 'C', "getc_nonblock2");
	test(size == 1, "getc_nonblock3");
	result = getc_nonblock(&fm, &c, &size);
	test(result == 0, "getc_nonblock4");
	test(c == 'h', "getc_nonblock5");
	test(size == 1, "getc_nonblock6");
	result = getc_nonblock(&fm, &c, &size);
	test(result == 0, "getc_nonblock7");
	test(c == '0', "getc_nonblock8");
	test(size == 1, "getc_nonblock9");
	close_filememory(&fm);

	RETURN;
}

static int test_getc_nonblock_filememory(void)
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
	result = getc_nonblock_filememory(&fm, &c, &size);
	test(result == 0, "getc_nonblock_filememory1");
	test(c == 'C', "getc_nonblock_filememory2");
	test(size == 1, "getc_nonblock_filememory3");
	result = getc_nonblock_filememory(&fm, &c, &size);
	test(result == 0, "getc_nonblock_filememory4");
	test(c == 'h', "getc_nonblock_filememory5");
	test(size == 1, "getc_nonblock_filememory6");
	result = getc_nonblock_filememory(&fm, &c, &size);
	test(result == 0, "getc_nonblock_filememory7");
	test(c == '0', "getc_nonblock_filememory8");
	test(size == 1, "getc_nonblock_filememory9");
	close_filememory(&fm);

	fm.direct = filememory_output;
	test(getc_nonblock_filememory(&fm, &c, &size) < 0,
			"getc_nonblock_filememory10");
	fm.direct = filememory_input;
	fm.mode = filememory_end;
	test(getc_nonblock_filememory(&fm, &c, &size) == 1,
			"getc_nonblock_filememory11");
	fm.mode = filememory_error;
	test(getc_nonblock_filememory(&fm, &c, &size) < 0,
			"getc_nonblock_filememory12");

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

static int test_flush_write_filememory(void)
{
	byte buffer[100];
	int result;
	size_t size;
	struct filememory fm;

	if (openoutput(&fm)) return 1;
	memcpy(fm.buffer, "XYZ", 3);
	fm.index = 3;
	result = flush_write_filememory(&fm);
	test(result == 0, "flush_write_filememory1");
	test(fm.index == 0, "flush_write_filememory2");

	flush_arch(fm.file);
	if (readfile(buffer, 100, &size)) return 1;
	test(size == 3, "flush_write_filememory3");
	test(memcmp(buffer, "XYZ", 3) == 0, "flush_write_filememory4");
	close_filememory(&fm);

	RETURN;
}

static int test_write_normal(void)
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
	result = write_normal(&fm, (const byte *)"12345678901234567890", 20, &size);
	test(result == 0, "write_normal1");
	test(size == 20, "write_normal2");
	test(fm.index == 0, "write_normal3");
	flush_arch(fm.file);
	if (readfile(buffer, 100, &size)) return 1;
	test(size == 27, "write_normal4");
	test(memcmp(buffer, "abcdefg12345678901234567890", 27) == 0, "write_normal5");

	result = write_normal(&fm, (const byte *)"123", 3, &size);
	test(result == 0, "write_normal6");
	test(size == 3, "write_normal6");
	test(memcmp(fm.buffer, "123", 3) == 0, "write_normal7");
	test(fm.index == 3, "write_normal8");
	result = write_normal(&fm, (const byte *)"1234567", 7, &size);
	test(result == 0, "write_normal8");
	test(size == 7, "write_normal9");
	test(memcmp(fm.buffer, "67", 2) == 0, "write_normal10");
	test(fm.index == 2, "write_normal11");

	flush_arch(fm.file);
	if (readfile(buffer, 100, &size)) return 1;
	test(size == 35, "write_normal12");
	test(memcmp(buffer, "abcdefg1234567890123456789012312345", 35) == 0,
			"write_normal13");
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

static int test_fm_putcnormal(void)
{
	struct filememory fm;

	if (openoutput(&fm)) return 1;
	close_filememory(&fm);

	return 0;
}


/*
 *  main
 */
static int testcase_file_memory(void)
{
	lisp_info_enable = 0;
	TestBreak(test_init_filememory);
	TestBreak(test_init_input_filememory);
	TestBreak(test_init_output_filememory);
	TestBreak(test_standard_input_filememory);
	TestBreak(test_standard_output_filememory);
	TestBreak(test_standard_error_filememory);
	TestBreak(test_open_input_filememory);
	TestBreak(test_open_output_filememory);
	TestBreak(test_close_filememory);
	TestBreak(test_readforce);
	TestBreak(test_writeforce);
	TestBreak(test_readforce_nonblock);
	TestBreak(test_fm_readnext_large);
	TestBreak(test_fm_readnext_small);
	TestBreak(test_fm_readnext);
	TestBreak(test_fm_readbuffer);
	TestBreak(test_fm_readungetc);
	TestBreak(test_fm_read_normal);
	TestBreak(test_read_filememory);
	TestBreak(test_fm_read_normal_force);
	TestBreak(test_readf_filememory);
	TestBreak(test_fm_readnext_nonblock_large);
	TestBreak(test_fm_readnext_nonblock_small);
	TestBreak(test_fm_readnext_nonblock);
	TestBreak(test_fm_readbuffer_nonblock);
	TestBreak(test_fm_readungetc_nonblock);
	TestBreak(test_read_nonblock);
	TestBreak(test_read_nonblock_filememory);
	TestBreak(test_fm_readbuffer_small);
	TestBreak(test_fm_readungetc_small);
	TestBreak(test_getc_normal);
	TestBreak(test_getc_filememory);
	TestBreak(test_fm_readbuffer_nonblock_small);
	TestBreak(test_fm_readungetc_nonblock_small);
	TestBreak(test_getc_nonblock);
	TestBreak(test_getc_nonblock_filememory);
	TestBreak(test_ungetc_filememory);
	TestBreak(test_flush_write_filememory);
	TestBreak(test_write_normal);
	TestBreak(test_write_filememory);
	TestBreak(test_fm_putcnormal);
	lisp_info_enable = 1;

	return 0;
}

static void testinit_file_memory(Execute ptr)
{
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
}

int test_file_memory(void)
{
	DegradeTitle;
	return DegradeCode(file_memory);
}

