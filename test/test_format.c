#include "format.c"
#include "character.h"
#include "clos.h"
#include "code.h"
#include "common.h"
#include "constant.h"
#include "degrade.h"
#include "eval.h"
#include "eval_declare.h"
#include "function.h"
#include "object.h"
#include "package.h"
#include "pathname.h"
#include "readtable.h"
#include "strtype.h"
#include "symbol.h"
#include "syscall.h"
#include "type.h"
#include "type_table.h"
#include "unicode.h"

#if 0
static int test_format_stream_lisp(void)
{
	addr stream, format, list, tail, pos;
	Execute ptr;
	LocalRoot local;
	LocalStack stack;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);
	open_output_string_stream(&stream, 0);
	strvect_char_local(local, &format, "Hello~A~A~%");
	list_local(local, &list,
			fixnum_heapr(10), fixnum_heapr(20), fixnum_heapr(30), NULL);
	format_stream_list(ptr, stream, format, list, &tail);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "Hello1020\n"), "format_stream_list1");
	test(length_list_unsafe(tail) == 1, "format_stream_list2");
	GetCar(tail, &pos);
	test(RefFixnum(pos) == 30, "format_stream_list3");

	clear_output_string_stream(stream);
	format_stream_lisp(ptr, stream, format, list);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "Hello1020\n"), "format_stream_lisp1");

	rollback_local(local, stack);

	RETURN;
}

static int test_format_string_lisp(void)
{
	addr format, list, pos;
	Execute ptr;
	LocalRoot local;
	LocalStack stack;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);
	strvect_char_local(local, &format, "Hello~A~A~%");
	list_local(local, &list,
			fixnum_heapr(10), fixnum_heapr(20), fixnum_heapr(30), NULL);

	format_string_lisp(ptr, format, list, &pos);
	test(string_equal_char(pos, "Hello1020\n"), "format_string_lisp1");

	rollback_local(local, stack);

	RETURN;
}

static int test_format_lisp(void)
{
	addr stream, format, list, pos;
	Execute ptr;
	LocalRoot local;
	LocalStack stack;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);
	open_output_string_stream(&stream, 0);
	strvect_char_local(local, &format, "Hello~A~A~%");
	list_local(local, &list,
			fixnum_heapr(10), fixnum_heapr(20), fixnum_heapr(30), NULL);

	format_lisp(ptr, Nil, format, list, &pos);
	test(string_equal_char(pos, "Hello1020\n"), "format_lisp1");

	rollback_local(local, stack);

	RETURN;
}
#endif


/*
 *  main
 */
static int testbreak_format(void)
{
#if 0
	TestBreak(test_format_stream_lisp);
	TestBreak(test_format_string_lisp);
	TestBreak(test_format_lisp);
#endif

	return 0;
}

int test_format(void)
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
		result = testbreak_format();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

