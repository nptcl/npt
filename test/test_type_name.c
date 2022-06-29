#include "type_name.c"
#include "character.h"
#include "clos.h"
#include "common.h"
#include "declare.h"
#include "file_open.h"
#include "degrade.h"
#include "hashtable.h"
#include "package.h"
#include "pathname.h"
#include "random_state.h"
#include "reader.h"
#include "stream.h"
#include "stream_broadcast.h"
#include "stream_concat.h"
#include "stream_echo.h"
#include "stream_file.h"
#include "stream_function.h"
#include "stream_string.h"
#include "stream_synonym.h"
#include "stream_twoway.h"
#include "strvect.h"
#include "syscall.h"
#include "type.h"

static int test_type_name_clos(void)
{
	addr name, pos;
	Execute ptr;

	ptr = Execute_Thread;
	GetConst(COMMON_STANDARD_CLASS, &name);
	clos_find_class_(name, &pos);
	type_name_(ptr, pos, &pos);
	test(pos == name, "type_name_clos1");

	RETURN;
}

static int test_type_name_symbol(void)
{
	addr x, y;
	Execute ptr;

	ptr = Execute_Thread;
	x = readr_debug("hello");
	type_name_(ptr, x, &x);
	GetConst(COMMON_SYMBOL, &y);
	test(x == y, "type_name_symbol1");

	x = readr_debug(":hello");
	type_name_(ptr, x, &x);
	GetConst(COMMON_KEYWORD, &y);
	test(x == y, "type_name_symbol2");

	RETURN;
}

static int test_type_name_function(void)
{
	addr x, y;
	Execute ptr;

	ptr = Execute_Thread;
	GetConst(COMMON_CAR, &x);
	getfunction_global_(x, &x);
	type_name_(ptr, x, &x);
	GetConst(COMMON_COMPILED_FUNCTION, &y);
	test(x == y, "type_name_function1");

	GetConst(COMMON_DOLIST, &x);
	getmacro_symbol(x, &x);
	type_name_(ptr, x, &x);
	GetConst(SYSTEM_COMPILED_MACRO_FUNCTION, &y);
	test(x == y, "type_name_function2");

	RETURN;
}

static int test_type_name_stream(void)
{
	addr x, y;
	Execute ptr;

	ptr = Execute_Thread;
	/* broadcast */
	open_broadcast_stream_(&x, Nil);
	type_name_(ptr, x, &x);
	GetConst(COMMON_BROADCAST_STREAM, &y);
	test(x == y, "type_name_stream1");

	/* concatenated */
	open_concatenated_stream_(&x, Nil);
	type_name_(ptr, x, &x);
	GetConst(COMMON_CONCATENATED_STREAM, &y);
	test(x == y, "type_name_stream2");

	/* echo */
	open_concatenated_stream_(&y, Nil);
	open_echo_stream(&x, y, y);
	type_name_(ptr, x, &x);
	GetConst(COMMON_ECHO_STREAM, &y);
	test(x == y, "type_name_stream3");

	/* string */
	strvect_char_heap(&x, "Hello");
	open_input_string_stream_(&x, x);
	type_name_(ptr, x, &x);
	GetConst(COMMON_STRING_STREAM, &y);
	test(x == y, "type_name_stream4");

	/* synonym */
	GetConst(SYSTEM_STANDARD_INPUT, &x);
	open_synonym_stream_(&x, x);
	type_name_(ptr, x, &x);
	GetConst(COMMON_SYNONYM_STREAM, &y);
	test(x == y, "type_name_stream5");

	/* two-way */
	GetConst(SYSTEM_STANDARD_INPUT, &x);
	getspecialcheck_local_(Execute_Thread, x, &x);
	GetConst(SYSTEM_STANDARD_OUTPUT, &y);
	getspecialcheck_local_(Execute_Thread, y, &y);
	open_twoway_stream(&x, x, y);
	type_name_(ptr, x, &x);
	GetConst(COMMON_TWO_WAY_STREAM, &y);
	test(x == y, "type_name_stream6");

	/* file */
	strvect_char_heap(&y, "test/empty.file");
	open_input_stream_error_(Execute_Thread, &y, y, Unbound);
	type_name_(ptr, y, &x);
	close_stream_(y, NULL);
	GetConst(COMMON_FILE_STREAM, &y);
	test(x == y, "type_name_stream7");

	RETURN;
}

static int test_type_name_call(void)
{
	addr x, y;
	Execute ptr;

	ptr = Execute_Thread;
	fixnum_heap(&x, 10);
	type_name_(ptr, x, &x);
	GetConst(COMMON_INTEGER, &y);
	test(x == y, "type_name1");

	RETURN;
}


/*
 *  main
 */
static int testcase_type_name(void)
{
	TestBreak(test_type_name_clos);
	TestBreak(test_type_name_symbol);
	TestBreak(test_type_name_function);
	TestBreak(test_type_name_stream);
	TestBreak(test_type_name_call);

	return 0;
}

static void testinit_type_name(Execute ptr)
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
	build_declare();
}

int test_type_name(void)
{
	DegradeTitle;
	return DegradeCode(type_name);
}

