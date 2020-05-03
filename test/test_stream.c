#include "stream.c"
#include "character.h"
#include "clos.h"
#include "common.h"
#include "constant.h"
#include "control.h"
#include "degrade.h"
#include "object.h"
#include "package.h"
#include "reader.h"
#include "symbol.h"
#include "syscall.h"
#include "type.h"
#include "type_table.h"

static int test_stream_heap(void)
{
	addr pos, info;
	struct StructStream *ptr;
	enum StreamType type;

	stream_heap(&pos, StreamType_BinaryInput, 16);
	test(GetType(pos) == LISPTYPE_STREAM, "stream_heap1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAYBODY, "stream_heap2");
	posbody(pos, (addr *)&ptr);
	test(ptr->type == StreamType_BinaryInput, "stream_heap4");
	test(PtrStructStream(pos)->type == StreamType_BinaryInput, "stream_heap5");
	test(PtrBodyStream(pos) == PtrBodyAB(pos), "stream_heap7");
	GetTypeStream(pos, &type);
	test(type == StreamType_BinaryInput, "stream_heap8");
	memset(PtrDataStream(pos), 0xBB, 16);

	SetArrayAB(pos, STREAM_INDEX_PATHNAME, T);
	info = 0;
	GetArrayAB(pos, STREAM_INDEX_PATHNAME, &info);
	test(info == T, "stream_heap9");
	SetPathnameStream(pos, Nil);
	GetPathnameStream(pos, &info);
	test(info == Nil, "stream_heap10");
	SetInfoStream(pos, T);
	GetInfoStream(pos, &info);
	test(info == T, "stream_heap11");

	RETURN;
}

#if 0
/*
 *  StringOutput
 */
static int test_open_output_string_stream(void)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	test(PtrStructStream(stream)->type == StreamType_StringOutput,
			"open_output_string_stream1");
	close_stream(stream);

	RETURN;
}

static int test_write_char_StringOutput(void)
{
	addr stream, check;

	open_output_string_stream(&stream, 0);
	write_char_stream(stream, 'A');
	write_char_stream(stream, 'b');
	write_char_stream(stream, 'c');
	string_stream_heap(stream, &check);
	test(string_equal_char(check, "Abc"), "write_char_stream1");

	RETURN;
}


/*
 *  StrnigInput
 */
static int test_open_input_string_stream(void)
{
	addr stream, str;

	strvect_char_heap(&str, "Hello");
	open_input_string_stream(&stream, str);
	test(PtrStructStream(stream)->type == StreamType_StringInput,
			"open_input_string_stream1");
	test(PtrStructStream(stream)->closed == 0, "open_input_string_stream2");
	close_stream(stream);
	test(PtrStructStream(stream)->closed == 1, "open_input_string_stream3");

	RETURN;
}

static int test_end_StringInput(void)
{
	addr stream, str;

	strvect_char_heap(&str, "Hello");
	open_input_string_stream(&stream, str);
	test(end_stream(stream) == 0, "end_StringInput1");
	PtrStringInputStream(stream)->index = 5;
	test(end_stream(stream), "end_StringInput2");
	close_stream(stream);

	RETURN;
}

static int test_error_StringInput(void)
{
	addr stream, str;

	strvect_char_heap(&str, "Hello");
	open_input_string_stream(&stream, str);
	test(error_stream(stream) == 0, "error_StringInput1");
	close_stream(stream);

	RETURN;
}

static int test_read_char_StringInput(void)
{
	int result;
	addr stream, str;
	unicode c;

	strvect_char_heap(&str, "Abc");
	open_input_string_stream(&stream, str);
	result = read_char_stream(stream, &c);
	test(result == 0, "read_char_StringInput1");
	test(c == 'A', "read_char_StringInput2");
	result = read_char_stream(stream, &c);
	test(result == 0, "read_char_StringInput3");
	test(c == 'b', "read_char_StringInput4");
	result = read_char_stream(stream, &c);
	test(result == 0, "read_char_StringInput5");
	test(c == 'c', "read_char_StringInput6");
	result = read_char_stream(stream, &c);
	test(result != 0, "read_char_StringInput7");
	close_stream(stream);

	RETURN;
}

static int test_unread_char_StringInput(void)
{
	int result;
	addr stream, str;
	unicode c;

	strvect_char_heap(&str, "Abc");
	open_input_string_stream(&stream, str);
	result = read_char_stream(stream, &c);
	test(result == 0, "unread_char_StringInput1");
	test(c == 'A', "unread_char_StringInput2");
	unread_char_stream(stream, 'Z');
	result = read_char_stream(stream, &c);
	test(result == 0, "unread_char_StringInput3");
	test(c == 'Z', "unread_char_StringInput4");
	result = read_char_stream(stream, &c);
	test(result == 0, "unread_char_StringInput5");
	test(c == 'b', "unread_char_StringInput6");
	unread_char_stream(stream, 'Q');
	result = read_char_stream(stream, &c);
	test(result == 0, "unread_char_StringInput7");
	test(c == 'Q', "unread_char_StringInput8");
	result = read_char_stream(stream, &c);
	test(result == 0, "unread_char_StringInput9");
	test(c == 'c', "unread_char_StringInput10");
	result = read_char_stream(stream, &c);
	test(result != 0, "unread_char_StringInput11");
	close_stream(stream);

	RETURN;
}
#endif


/*
 *  Main
 */
static int testbreak_stream(void)
{
	TestBreak(test_stream_heap);
#if 0
	TestBreak(test_open_output_string_stream);
	TestBreak(test_write_char_StringOutput);

	TestBreak(test_open_input_string_stream);
	TestBreak(test_end_StringInput);
	TestBreak(test_error_StringInput);
	TestBreak(test_read_char_StringInput);
	TestBreak(test_unread_char_StringInput);
#endif

	return 0;
}

int test_stream(void)
{
	int result;
	lispcode code;
	Execute ptr;

	TITLE;

	freelisp();
	alloclisp(0, 0);
	lisp_info_enable = 1;
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
		build_reader();
		build_pathname();
		lisp_initialize = 1;
		result = testbreak_stream();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

