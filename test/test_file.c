#include "file.c"

#if 0
#include "character.h"
#include "clos.h"
#include "common.h"
#include "constant.h"
#include "degrade.h"
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

static int test_open_input_binary_stream(void)
{
	addr name, stream;
	filestream fm;

	strvect_char_heap(&name, TESTFILE);
	stream = 0;
	open_input_binary_stream_(Local_Thread, &stream, name);
	test(stream, "open_input_binary_stream1");
	fm = (filestream)PtrDataStream(stream);
	test(fm->direct == filememory_input, "open_input_binary_stream2");
	close_stream_file_(stream, &stream);

	RETURN;
}

static int test_open_output_binary_stream(void)
{
	addr name, stream;
	filestream fm;

	strvect_char_heap(&name, TESTFILE);
	stream = 0;
	open_output_binary_stream(Execute_Thread, &stream, name, FileOutput_supersede);
	test(stream, "open_output_binary_stream1");
	fm = (filestream)PtrDataStream(stream);
	test(fm->direct == filememory_output, "open_output_binary_stream2");
	close_stream_file_(stream, &stream);

	RETURN;
}

static int test_open_input_stream(void)
{
	addr name, stream;
	filestream fm;

	strvect_char_heap(&name, TESTFILE);
	stream = 0;
	open_input_stream(Execute_Thread, &stream, name);
	test(stream, "open_input_stream1");
	fm = (filestream)PtrDataStream(stream);
	test(fm->direct == filememory_input, "open_input_stream2");
	close_stream_file_(stream, &stream);

	RETURN;
}

static int test_open_output_stream(void)
{
	addr name, stream;
	filestream fm;

	strvect_char_heap(&name, TESTFILE);
	stream = 0;
	open_output_stream(Execute_Thread, &stream, name, FileOutput_supersede);
	test(stream, "open_output_stream1");
	fm = (filestream)PtrDataStream(stream);
	test(fm->direct == filememory_output, "open_output_stream2");
	close_stream_file_(stream, &stream);

	RETURN;
}

static int test_make_standard_input(void)
{
	addr stream;
	filestream fm;

	make_standard_input(&stream);
	test(stream, "make_standard_input1");
	fm = (filestream)PtrDataStream(stream);
	test(fm->direct == filememory_input, "make_standard_input2");

	RETURN;
}

static int test_make_standard_output(void)
{
	addr stream;
	filestream fm;

	make_standard_output(&stream);
	test(stream, "make_standard_output1");
	fm = (filestream)PtrDataStream(stream);
	test(fm->direct == filememory_output, "make_standard_output2");

	RETURN;
}

static int test_make_standard_error(void)
{
	addr stream;
	filestream fm;

	make_standard_error(&stream);
	test(stream, "make_standard_error1");
	fm = (filestream)PtrDataStream(stream);
	test(fm->direct == filememory_output, "make_standard_error2");

	RETURN;
}

static int test_close_stream_file(void)
{
	addr name, stream;

	strvect_char_heap(&name, TESTFILE);
	open_input_binary_stream(Execute_Thread, &stream, name);
	close_stream_file_(stream, &stream);

	return 0;
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

static int test_read_binary_file(void)
{
	byte buffer[100];
	size_t size;
	addr name, stream;

	if (writetest("hello", 5)) return 1;
	strvect_char_heap(&name, TESTFILE);
	open_input_binary_stream(Execute_Thread, &stream, name);
	size = 0;
	read_binary_file(stream, buffer, 100, &size);
	test(size == 5, "read_binary_file1");
	test(memcmp(buffer, "hello", 5) == 0, "read_binary_file2");
	close_stream_file_(stream, &stream);

	RETURN;
}

static int test_readf_binary_file(void)
{
	byte buffer[100];
	size_t size;
	addr name, stream;

	if (writetest("hello", 5)) return 1;
	strvect_char_heap(&name, TESTFILE);
	open_input_binary_stream(Execute_Thread, &stream, name);
	size = 0;
	readf_binary_file(stream, buffer, 100, &size);
	test(size == 5, "readf_binary_file1");
	test(memcmp(buffer, "hello", 5) == 0, "readf_binary_file2");
	close_stream_file_(stream, &stream);

	RETURN;
}

static int test_read_byte_file(void)
{
	byte c;
	int check;
	addr name, stream;

	if (writetest("abc", 3)) return 1;
	strvect_char_heap(&name, TESTFILE);
	open_input_binary_stream(Execute_Thread, &stream, name);
	c = 0;
	check = read_byte_file(stream, &c);
	test(check == 0, "read_byte_file1");
	test(c == 'a', "read_byte_file2");
	check = read_byte_file(stream, &c);
	test(check == 0, "read_byte_file3");
	test(c == 'b', "read_byte_file4");
	check = read_byte_file(stream, &c);
	test(check == 0, "read_byte_file5");
	test(c == 'c', "read_byte_file6");
	check = read_byte_file(stream, &c);
	test(check, "read_byte_file7");

	RETURN;
}

static int test_unread_byte_file(void)
{
	byte c;
	int check;
	addr name, stream;

	if (writetest("abc", 3)) return 1;
	strvect_char_heap(&name, TESTFILE);
	open_input_binary_stream(Execute_Thread, &stream, name);
	c = 0;
	check = read_byte_file(stream, &c);
	test(check == 0, "read_byte_file1");
	test(c == 'a', "read_byte_file2");
	check = unread_byte_file(stream, 'A');
	test(check == 0, "read_byte_file3");
	check = read_byte_file(stream, &c);
	test(check == 0, "read_byte_file5");
	test(c == 'A', "read_byte_file6");

	RETURN;
}


/*
 *  main
 */
static int testcase_file(void)
{
	TestBreak(test_open_output_binary_stream);
	TestBreak(test_open_input_binary_stream);
	TestBreak(test_open_output_stream);
	TestBreak(test_open_input_stream);
	TestBreak(test_make_standard_input);
	TestBreak(test_make_standard_output);
	TestBreak(test_make_standard_error);
	TestBreak(test_close_stream_file);
	TestBreak(test_read_binary_file);
	TestBreak(test_readf_binary_file);
	TestBreak(test_read_byte_file);
	TestBreak(test_unread_byte_file);

	return 0;
}

static void testinit_file(Execute ptr)
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
#endif

int test_file(void)
{
#if 0
	DegradeTitle;
	return DegradeCode(file);
#endif
	return 0;
}

