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
#include "stream_string.h"
#include "stream_synonym.h"
#include "stream_twoway.h"
#include "strvect.h"
#include "syscall.h"
#include "type.h"

static int test_type_name_clos(void)
{
	addr name, pos;

	GetConst(COMMON_STANDARD_CLASS, &name);
	clos_find_class(name, &pos);
	type_name_(&pos, pos);
	test(pos == name, "type_name_clos1");

	RETURN;
}

static int test_type_name_symbol(void)
{
	addr x, y;

	x = readr("hello");
	type_name_(&x, x);
	GetConst(COMMON_SYMBOL, &y);
	test(x == y, "type_name_symbol1");

	x = readr(":hello");
	type_name_(&x, x);
	GetConst(COMMON_KEYWORD, &y);
	test(x == y, "type_name_symbol2");

	RETURN;
}

static int test_type_name_function(void)
{
	addr x, y;

	GetConst(COMMON_CAR, &x);
	getfunction_global(x, &x);
	type_name_(&x, x);
	GetConst(COMMON_COMPILED_FUNCTION, &y);
	test(x == y, "type_name_function1");

	GetConst(COMMON_DOLIST, &x);
	getmacro_symbol(x, &x);
	type_name_(&x, x);
	GetConst(SYSTEM_COMPILED_MACRO_FUNCTION, &y);
	test(x == y, "type_name_function2");

	RETURN;
}

static int test_type_name_stream(void)
{
	addr x, y;

	/* broadcast */
	open_broadcast_stream(&x, Nil);
	type_name_(&x, x);
	GetConst(COMMON_BROADCAST_STREAM, &y);
	test(x == y, "type_name_stream1");

	/* concatenated */
	open_concatenated_stream(&x, Nil);
	type_name_(&x, x);
	GetConst(COMMON_CONCATENATED_STREAM, &y);
	test(x == y, "type_name_stream2");

	/* echo */
	open_concatenated_stream(&y, Nil);
	open_echo_stream(&x, y, y);
	type_name_(&x, x);
	GetConst(COMMON_ECHO_STREAM, &y);
	test(x == y, "type_name_stream3");

	/* string */
	strvect_char_heap(&x, "Hello");
	open_input_string_stream(&x, x);
	type_name_(&x, x);
	GetConst(COMMON_STRING_STREAM, &y);
	test(x == y, "type_name_stream4");

	/* synonym */
	GetConst(SYSTEM_STANDARD_INPUT, &x);
	open_synonym_stream(&x, x);
	type_name_(&x, x);
	GetConst(COMMON_SYNONYM_STREAM, &y);
	test(x == y, "type_name_stream5");

	/* two-way */
	GetConst(SYSTEM_STANDARD_INPUT, &x);
	getspecialcheck_local(Execute_Thread, x, &x);
	GetConst(SYSTEM_STANDARD_OUTPUT, &y);
	getspecialcheck_local(Execute_Thread, y, &y);
	open_twoway_stream(&x, x, y);
	type_name_(&x, x);
	GetConst(COMMON_TWO_WAY_STREAM, &y);
	test(x == y, "type_name_stream6");

	/* file */
	strvect_char_heap(&y, "test/empty.file");
	open_input_stream_error_(Execute_Thread, &y, y);
	type_name_(&x, y);
	close_stream_(y);
	GetConst(COMMON_FILE_STREAM, &y);
	test(x == y, "type_name_stream7");

	RETURN;
}

static int test_type_name_call(void)
{
	addr x, y;

	fixnum_heap(&x, 10);
	type_name_(&x, x);
	GetConst(COMMON_INTEGER, &y);
	test(x == y, "type_name1");

	RETURN;
}


/*
 *  main
 */
static int testbreak_type_name(void)
{
	TestBreak(test_type_name_clos);
	TestBreak(test_type_name_symbol);
	TestBreak(test_type_name_function);
	TestBreak(test_type_name_stream);
	TestBreak(test_type_name_call);

	return 0;
}

int test_type_name(void)
{
	int result;
	lispcode code;
	Execute ptr;

	TITLE;
	freelisp();
	alloclisp(0, 0);
	lisp_info_enable = 1;
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
		build_declare();
		lisp_initialize = 1;
		result = testbreak_type_name();
	}
	end_setjmp(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

