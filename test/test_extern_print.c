#include "extern_print.c"
#include "clos.h"
#include "code.h"
#include "common.h"
#include "condition.h"
#include "constant.h"
#include "character.h"
#include "declare.h"
#include "degrade.h"
#include "extern_control.h"
#include "extern_object.h"
#include "extern_type.h"
#include "main_init.h"
#include "object.h"
#include "package.h"
#include "pathname.h"
#include "reader.h"
#include "stream.h"
#include "stream_string.h"
#include "strvect.h"
#include "symbol.h"
#include "syscall.h"
#include "type.h"

/*
 *  format
 */
static int test_lisp_format8(void)
{
	addr control, x, stream, pos;
	const char str[] = "Hello~A";

	lisp_push_control(&control);
	x = Lisp_hold();

	open_output_string_stream(&stream, 0);
	lisp_push_special8_("*STANDARD-OUTPUT*", stream);
	lisp_fixnum(x, 11);
	lisp_format8_(NULL, str, x, NULL);
	string_stream_heap_(stream, &pos);
	test(strvect_equal_char(pos, "Hello11"), "lisp_format8.1");

	clear_output_string_stream(stream);
	lisp_fixnum(x, 22);
	lisp_format8_(stream, str, x, NULL);
	string_stream_heap_(stream, &pos);
	test(strvect_equal_char(pos, "Hello22"), "lisp_format8.2");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_format16(void)
{
	addr control, x, stream, pos;
	const byte16 str[] = { 'H','e','l','l','o','~','A',0 };

	lisp_push_control(&control);
	x = Lisp_hold();

	open_output_string_stream(&stream, 0);
	lisp_push_special8_("*STANDARD-OUTPUT*", stream);
	lisp_fixnum(x, 11);
	lisp_format16_(NULL, str, x, NULL);
	string_stream_heap_(stream, &pos);
	test(strvect_equal_char(pos, "Hello11"), "lisp_format16.1");

	clear_output_string_stream(stream);
	lisp_fixnum(x, 22);
	lisp_format16_(stream, str, x, NULL);
	string_stream_heap_(stream, &pos);
	test(strvect_equal_char(pos, "Hello22"), "lisp_format16.2");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_format32(void)
{
	addr control, x, stream, pos;
	const unicode str[] = { 'H','e','l','l','o','~','A',0 };

	lisp_push_control(&control);
	x = Lisp_hold();

	open_output_string_stream(&stream, 0);
	lisp_push_special8_("*STANDARD-OUTPUT*", stream);
	lisp_fixnum(x, 11);
	lisp_format32_(NULL, str, x, NULL);
	string_stream_heap_(stream, &pos);
	test(strvect_equal_char(pos, "Hello11"), "lisp_format32.1");

	clear_output_string_stream(stream);
	lisp_fixnum(x, 22);
	lisp_format32_(stream, str, x, NULL);
	string_stream_heap_(stream, &pos);
	test(strvect_equal_char(pos, "Hello22"), "lisp_format32.2");

	lisp_pop_control_(control);

	RETURN;
}


/*
 *  stdout
 */
static int test_lisp_stdout8(void)
{
	addr control, x, stream, pos;
	const char str[] = "Hello~A";

	lisp_push_control(&control);
	x = Lisp_hold();

	open_output_string_stream(&stream, 0);
	lisp_push_special8_("*STANDARD-OUTPUT*", stream);
	lisp_fixnum(x, 11);
	lisp_stdout8_(str, x, NULL);
	string_stream_heap_(stream, &pos);
	test(strvect_equal_char(pos, "Hello11"), "lisp_stdout8.1");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_stdout16(void)
{
	addr control, x, stream, pos;
	const byte16 str[] = { 'H','e','l','l','o','~','A',0 };

	lisp_push_control(&control);
	x = Lisp_hold();

	open_output_string_stream(&stream, 0);
	lisp_push_special8_("*STANDARD-OUTPUT*", stream);
	lisp_fixnum(x, 11);
	lisp_stdout16_(str, x, NULL);
	string_stream_heap_(stream, &pos);
	test(strvect_equal_char(pos, "Hello11"), "lisp_stdout16.1");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_stdout32(void)
{
	addr control, x, stream, pos;
	const unicode str[] = { 'H','e','l','l','o','~','A',0 };

	lisp_push_control(&control);
	x = Lisp_hold();

	open_output_string_stream(&stream, 0);
	lisp_push_special8_("*STANDARD-OUTPUT*", stream);
	lisp_fixnum(x, 11);
	lisp_stdout32_(str, x, NULL);
	string_stream_heap_(stream, &pos);
	test(strvect_equal_char(pos, "Hello11"), "lisp_stdout32.1");

	lisp_pop_control_(control);

	RETURN;
}


/*
 *  stringf
 */
static int test_lisp_stringf8(void)
{
	addr control, x, pos;
	const char str[] = "Hello~A";

	lisp_push_control(&control);
	x = Lisp_hold();

	lisp_fixnum(x, 11);
	lisp0_stringf8_(&pos, str, x, NULL);
	test(strvect_equal_char(pos, "Hello11"), "lisp_stringf8.1");

	lisp_fixnum(x, 22);
	lisp_stringf8_(x, str, x, NULL);
	hold_value(x, &pos);
	test(strvect_equal_char(pos, "Hello22"), "lisp_stringf8.2");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_stringf16(void)
{
	addr control, x, pos;
	const byte16 str[] = { 'H','e','l','l','o','~','A',0 };

	lisp_push_control(&control);
	x = Lisp_hold();

	lisp_fixnum(x, 11);
	lisp0_stringf16_(&pos, str, x, NULL);
	test(strvect_equal_char(pos, "Hello11"), "lisp_stringf16.1");

	lisp_fixnum(x, 22);
	lisp_stringf16_(x, str, x, NULL);
	hold_value(x, &pos);
	test(strvect_equal_char(pos, "Hello22"), "lisp_stringf16.2");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_stringf32(void)
{
	addr control, x, pos;
	const unicode str[] = { 'H','e','l','l','o','~','A',0 };

	lisp_push_control(&control);
	x = Lisp_hold();

	lisp_fixnum(x, 11);
	lisp0_stringf32_(&pos, str, x, NULL);
	test(strvect_equal_char(pos, "Hello11"), "lisp_stringf32.1");

	lisp_fixnum(x, 22);
	lisp_stringf32_(x, str, x, NULL);
	hold_value(x, &pos);
	test(strvect_equal_char(pos, "Hello22"), "lisp_stringf32.2");

	lisp_pop_control_(control);

	RETURN;
}


/*
 *  Main
 */
static int testcase_extern_print(void)
{
	/* format */
	TestBreak(test_lisp_format8);
	TestBreak(test_lisp_format16);
	TestBreak(test_lisp_format32);
	/* stdout */
	TestBreak(test_lisp_stdout8);
	TestBreak(test_lisp_stdout16);
	TestBreak(test_lisp_stdout32);
	/* stringf */
	TestBreak(test_lisp_stringf8);
	TestBreak(test_lisp_stringf16);
	TestBreak(test_lisp_stringf32);

	return 0;
}

static void testinit_extern_print(Execute ptr)
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
	build_code();
}

int test_extern_print(void)
{
	DegradeTitle;
	return DegradeCode(extern_print);
}

