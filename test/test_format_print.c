#include "format_print.c"
#include "character.h"
#include "clos.h"
#include "code.h"
#include "common.h"
#include "constant.h"
#include "degrade.h"
#include "eval.h"
#include "function.h"
#include "object.h"
#include "package.h"
#include "pathname.h"
#include "readtable.h"
#include "stream_string.h"
#include "strtype.h"
#include "symbol.h"
#include "syscall.h"
#include "type.h"
#include "type_table.h"
#include "unicode.h"


/*
 *  format execute
 */
static void fmtprint_print(fmtprint print, const char *str)
{
	while (*str)
		fmtprint_putc(print, *(str++));
}

static int test_fmtprint_putc(void)
{
	addr stream, pos;
	struct fmtprint_struct print;

	aatype(print);
	print.pretty = 0;
	open_output_string_stream(&stream, 0);
	print.stream = stream;
	/* upcase */
	print.conversion = fmtcase_upcase;
	print.first = 1;
	print.word = 0;
	fmtprint_print(&print, "aBc");
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "ABC"), "fmtprint_putc1");
	clear_output_string_stream(stream);

	print.first = 1;
	print.word = 0;
	fmtprint_print(&print, "-aBc-");
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "-ABC-"), "fmtprint_putc2");
	clear_output_string_stream(stream);
	/* downcase */
	print.conversion = fmtcase_downcase;
	print.first = 1;
	print.word = 0;
	fmtprint_print(&print, "AbCdE01234");
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "abcde01234"), "fmtprint_putc3");
	clear_output_string_stream(stream);
	/* capitalize */
	print.conversion = fmtcase_capitalize_all;
	print.first = 1;
	print.word = 0;
	fmtprint_print(&print, "abcd efgh-ije-0ab cde0 abcd");
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "Abcd Efgh-Ije-0ab Cde0 Abcd"), "fmtprint_putc4");
	clear_output_string_stream(stream);
	/* capitalize-first */
	print.conversion = fmtcase_capitalize_first;
	print.first = 1;
	print.word = 0;
	fmtprint_print(&print, "abcd efgh-ije-0ab cde0 abcd");
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "Abcd efgh-ije-0ab cde0 abcd"), "fmtprint_putc5");
	clear_output_string_stream(stream);

	print.first = 1;
	print.word = 0;
	fmtprint_print(&print, "00abcd efgh");
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "00abcd efgh"), "fmtprint_putc6");
	clear_output_string_stream(stream);

	/* normal */
	print.conversion = fmtcase_normal;
	print.first = 1;
	print.word = 0;
	fmtprint_print(&print, "aBcD-eFgH1234");
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "aBcD-eFgH1234"), "fmtprint_putc7");
	clear_output_string_stream(stream);

	close_stream(stream);

	RETURN;
}

static int test_fmtprint_pop(void)
{
	addr rest, pos;
	struct fmtprint_struct print;
	struct fmtstack args;

	aatype(print);
	aatype(args);
	list_heap(&rest,
			fixnum_heapr(10),
			fixnum_heapr(20),
			fixnum_heapr(30),
			NULL);
	args.root = rest;
	args.front = rest;
	args.index = 0;
	cleartype(print);
	print.rest = &args;
	fmtprint_pop(&print, NULL, &pos);
	test(RefFixnum(pos) == 10, "fmtprint_pop1");
	test(args.index == 1, "fmtprint_pop2");
	fmtprint_pop(&print, NULL, &pos);
	test(RefFixnum(pos) == 20, "fmtprint_pop3");
	test(args.index == 2, "fmtprint_pop4");
	fmtprint_pop(&print, NULL, &pos);
	test(RefFixnum(pos) == 30, "fmtprint_pop5");
	test(args.index == 3, "fmtprint_pop6");

	RETURN;
}

static int test_fmtprint_peek(void)
{
	addr rest, pos;
	struct fmtprint_struct print;
	struct fmtstack args;

	aatype(print);
	aatype(args);
	list_heap(&rest,
			fixnum_heapr(10),
			fixnum_heapr(20),
			fixnum_heapr(30),
			NULL);
	args.root = rest;
	args.front = rest;
	args.index = 0;
	cleartype(print);
	print.rest = &args;
	fmtprint_peek(&print, NULL, &pos);
	test(RefFixnum(pos) == 10, "fmtprint_peek1");
	test(args.index == 0, "fmtprint_peek2");
	fmtprint_peek(&print, NULL, &pos);
	test(RefFixnum(pos) == 10, "fmtprint_peek3");
	test(args.index == 0, "fmtprint_peek4");

	RETURN;
}

static int test_fmtprint_forward(void)
{
	addr rest, pos;
	struct fmtprint_struct print;
	struct fmtstack args;

	aatype(print);
	aatype(args);
	list_heap(&rest,
			fixnum_heapr(10),
			fixnum_heapr(20),
			fixnum_heapr(30),
			fixnum_heapr(40),
			fixnum_heapr(50),
			NULL);
	args.root = rest;
	args.front = rest;
	args.index = 0;
	cleartype(print);
	print.rest = &args;
	fmtprint_pop(&print, NULL, &pos);
	fmtprint_forward(&print, NULL, 2);
	test(args.index == 3, "fmtprint_forward1");
	fmtprint_peek(&print, NULL, &pos);
	test(RefFixnum(pos) == 40, "fmtprint_forward2");

	RETURN;
}

static int test_fmtprint_rollback(void)
{
	addr rest, pos;
	struct fmtprint_struct print;
	struct fmtstack args;

	aatype(print);
	aatype(args);
	list_heap(&rest,
			fixnum_heapr(10),
			fixnum_heapr(20),
			fixnum_heapr(30),
			fixnum_heapr(40),
			fixnum_heapr(50),
			NULL);
	args.root = rest;
	args.front = rest;
	args.index = 0;
	cleartype(print);
	print.rest = &args;
	fmtprint_pop(&print, NULL, &pos);
	fmtprint_pop(&print, NULL, &pos);
	fmtprint_pop(&print, NULL, &pos);
	fmtprint_pop(&print, NULL, &pos);
	fmtprint_rollback(&print, NULL, 3);
	test(args.index == 1, "fmtprint_rollback1");
	fmtprint_peek(&print, NULL, &pos);
	test(RefFixnum(pos) == 20, "fmtprint_rollback2");
	fmtprint_rollback(&print, NULL, 1);
	fmtprint_peek(&print, NULL, &pos);
	test(RefFixnum(pos) == 10, "fmtprint_rollback3");

	RETURN;
}

static int test_fmtprint_absolute(void)
{
	addr rest, pos;
	struct fmtprint_struct print;
	struct fmtstack args;

	aatype(print);
	aatype(args);
	list_heap(&rest,
			fixnum_heapr(10),
			fixnum_heapr(20),
			fixnum_heapr(30),
			fixnum_heapr(40),
			fixnum_heapr(50),
			NULL);
	args.root = rest;
	args.front = rest;
	args.index = 0;
	cleartype(print);
	print.rest = &args;
	fmtprint_pop(&print, NULL, &pos);
	fmtprint_pop(&print, NULL, &pos);
	fmtprint_pop(&print, NULL, &pos);
	fmtprint_pop(&print, NULL, &pos);
	fmtprint_absolute(&print, NULL, 3);
	test(args.index == 3, "fmtprint_absolute1");
	fmtprint_peek(&print, NULL, &pos);
	test(RefFixnum(pos) == 40, "fmtprint_absolute2");
	fmtprint_absolute(&print, NULL, 1);
	fmtprint_peek(&print, NULL, &pos);
	test(RefFixnum(pos) == 20, "fmtprint_absolute3");

	RETURN;
}


/*
 *  main
 */
static int testbreak_format_print(void)
{
	TestBreak(test_fmtprint_putc);
	TestBreak(test_fmtprint_pop);
	TestBreak(test_fmtprint_peek);
	TestBreak(test_fmtprint_forward);
	TestBreak(test_fmtprint_rollback);
	TestBreak(test_fmtprint_absolute);

	return 0;
}

int test_format_print(void)
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
		build_readtable();
		build_pathname();
		build_eval_declare();
		build_code();
		lisp_initialize = 1;
		result = testbreak_format_print();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

