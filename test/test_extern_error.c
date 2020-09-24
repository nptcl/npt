#include "extern_error.c"
#include "clos.h"
#include "code.h"
#include "common.h"
#include "condition.h"
#include "constant.h"
#include "character.h"
#include "declare.h"
#include "degrade.h"
#include "extern_control.h"
#include "extern_execute.h"
#include "extern_instance.h"
#include "extern_type.h"
#include "main_init.h"
#include "object.h"
#include "package.h"
#include "pathname.h"
#include "reader.h"
#include "stream.h"
#include "stream_string.h"
#include "symbol.h"
#include "syscall.h"
#include "type.h"

static int test_lisp_abort(void)
{
	int finish;
	lisp_abort_calltype handler;

	handler = lisp_set_abort_setjmp_handler();
	finish = 0;
	Lisp_abort_Begin {
		lisp_abort();
		finish = 1;
	}
	Lisp_abort_End;
	lisp_set_abort_handler(handler);
	test(finish == 0, "lisp_abort.1");

	RETURN;
}

static int test_lisp_abortf(void)
{
	char buffer[64];
	FILE *file;
	int finish;
	lisp_abort_calltype handler;
	const char *name = "_debug.txt";

	/* write */
	file = fopen(name, "wb");
	if (file == NULL) {
		degrade_printf("fopen error.\n");
		return 1;
	}
	lisp_stderr = file;
	handler = lisp_set_abort_setjmp_handler();
	finish = 0;
	Lisp_abort_Begin {
		lisp_abortf("Hello: %d", 100);
		finish = 1;
	}
	Lisp_abort_End;
	lisp_set_abort_handler(handler);
	lisp_stderr = NULL;
	fclose(file);
	test(finish == 0, "lisp_abortf.1");

	/* check */
	file = fopen(name, "rb");
	if (file == NULL) {
		degrade_printf("fopen error.\n");
		return 1;
	}
	memset(buffer, 0, sizeof(buffer));
	fread(buffer, 1, 64, file);
	fclose(file);
	test(memcmp(buffer, "Hello: 100", 10) == 0, "lisp_abortf.2");

	RETURN;
}

static int test_lisp_abort8(void)
{
	int finish;
	addr control, pos;
	lisp_abort_calltype handler;
	const char str[] = "Hello";

	lisp_push_control(&control);
	open_output_string_stream(&pos, 0);
	lisp_push_special8_("*ERROR-OUTPUT*", pos);

	handler = lisp_set_abort_setjmp_handler();
	finish = 0;
	Lisp_abort_Begin {
		lisp_abort8(str, NULL);
		finish = 1;
	}
	Lisp_abort_End;
	lisp_set_abort_handler(handler);
	test(finish == 0, "lisp_abort8.1");

	string_stream_heap_(pos, &pos);
	test(strvect_equal_char(pos, "Hello"), "lisp_abort8.2");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_abort16(void)
{
	int finish;
	addr control, pos;
	lisp_abort_calltype handler;
	const byte16 str[] = { 'H','e','l','l','o',0 };

	lisp_push_control(&control);
	open_output_string_stream(&pos, 0);
	lisp_push_special8_("*ERROR-OUTPUT*", pos);

	handler = lisp_set_abort_setjmp_handler();
	finish = 0;
	Lisp_abort_Begin {
		lisp_abort16(str, NULL);
		finish = 1;
	}
	Lisp_abort_End;
	lisp_set_abort_handler(handler);
	test(finish == 0, "lisp_abort16.1");

	string_stream_heap_(pos, &pos);
	test(strvect_equal_char(pos, "Hello"), "lisp_abort16.2");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_abort32(void)
{
	int finish;
	addr control, pos;
	lisp_abort_calltype handler;
	const unicode str[] = { 'H','e','l','l','o',0 };

	lisp_push_control(&control);
	open_output_string_stream(&pos, 0);
	lisp_push_special8_("*ERROR-OUTPUT*", pos);

	handler = lisp_set_abort_setjmp_handler();
	finish = 0;
	Lisp_abort_Begin {
		lisp_abort32(str, NULL);
		finish = 1;
	}
	Lisp_abort_End;
	lisp_set_abort_handler(handler);
	test(finish == 0, "lisp_abort32.1");

	string_stream_heap_(pos, &pos);
	test(strvect_equal_char(pos, "Hello"), "lisp_abort32.2");

	lisp_pop_control_(control);

	RETURN;
}


/*
 *  signal
 */
static int test_lisp_signal(void)
{
	addr control, x, y, pos;

	lisp_push_control(&control);
	x = Lisp_hold();
	y = Lisp_hold();

	lisp_eval8_(NULL, "(define-condition test-lisp-signal () ())");
	lisp_eval8_(NULL, "(defvar *test-lisp-signal* 100)");
	lisp_find_class8_(x, "TEST-LISP-SIGNAL");
	lisp_eval8_(y, "(lambda (c) (declare (ignore c)) (setq *test-lisp-signal* 200))");
	lisp_handler_case_(x, y);
	lisp_instance_(x, x, NULL);
	lisp_signal_(x);
	test(lisp_break_control(), "lisp_signal.1");
	lisp_reset_control();
	lisp_get_special8_(x, "*TEST-LISP-SIGNAL*");
	hold_value(x, &pos);
	test(RefFixnum(pos) == 200, "lisp_signal.2");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_error(void)
{
	addr control, x, y, pos;

	lisp_push_control(&control);
	x = Lisp_hold();
	y = Lisp_hold();

	lisp_eval8_(NULL, "(defvar *test-lisp-error* 100)");
	lisp_find_class8_(x, "SIMPLE-ERROR");
	lisp_eval8_(y, "(lambda (c) (declare (ignore c)) (setq *test-lisp-error* 200))");
	lisp_handler_case_(x, y);
	lisp_instance_(x, x, NULL);
	lisp_error_(x);
	test(lisp_break_control(), "lisp_error.1");
	lisp_reset_control();
	lisp_get_special8_(x, "*TEST-LISP-ERROR*");
	hold_value(x, &pos);
	test(RefFixnum(pos) == 200, "lisp_error.2");

	lisp_pop_control_(control);

	RETURN;
}


/*
 *  error
 */
static int test_lisp_error8(void)
{
	addr control, x, y, pos;
	const char str[] = "Hello";

	lisp_push_control(&control);
	x = Lisp_hold();
	y = Lisp_hold();

	lisp_eval8_(NULL, "(defvar *test-lisp-error8* nil)");
	lisp_find_class8_(x, "SIMPLE-ERROR");
	lisp_eval8_(y, "(lambda (c)"
			"  (setq *test-lisp-error8* (simple-condition-format-control c)))");
	lisp_handler_case_(x, y);
	lisp_error8_(str, NULL);
	test(lisp_break_control(), "lisp_error8.1");
	lisp_reset_control();
	lisp_get_special8_(x, "*TEST-LISP-ERROR8*");
	hold_value(x, &pos);
	test(strvect_equal_char(pos, "Hello"), "lisp_error8.2");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_error16(void)
{
	addr control, x, y, pos;
	const byte16 str[] = { 'H','e','l','l','o',0 };

	lisp_push_control(&control);
	x = Lisp_hold();
	y = Lisp_hold();

	lisp_eval8_(NULL, "(defvar *test-lisp-error16* nil)");
	lisp_find_class8_(x, "SIMPLE-ERROR");
	lisp_eval8_(y, "(lambda (c)"
			"  (setq *test-lisp-error16* (simple-condition-format-control c)))");
	lisp_handler_case_(x, y);
	lisp_error16_(str, NULL);
	test(lisp_break_control(), "lisp_error16.1");
	lisp_reset_control();
	lisp_get_special8_(x, "*TEST-LISP-ERROR16*");
	hold_value(x, &pos);
	test(strvect_equal_char(pos, "Hello"), "lisp_error16.2");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_error32(void)
{
	addr control, x, y, pos;
	const unicode str[] = { 'H','e','l','l','o',0 };

	lisp_push_control(&control);
	x = Lisp_hold();
	y = Lisp_hold();

	lisp_eval8_(NULL, "(defvar *test-lisp-error32* nil)");
	lisp_find_class8_(x, "SIMPLE-ERROR");
	lisp_eval8_(y, "(lambda (c)"
			"  (setq *test-lisp-error32* (simple-condition-format-control c)))");
	lisp_handler_case_(x, y);
	lisp_error32_(str, NULL);
	test(lisp_break_control(), "lisp_error32.1");
	lisp_reset_control();
	lisp_get_special8_(x, "*TEST-LISP-ERROR32*");
	hold_value(x, &pos);
	test(strvect_equal_char(pos, "Hello"), "lisp_error32.2");

	lisp_pop_control_(control);

	RETURN;
}


/*
 *  warn
 */
static int test_lisp_warn8(void)
{
	addr control, x, y, pos;
	const char str[] = "Hello";

	lisp_push_control(&control);
	x = Lisp_hold();
	y = Lisp_hold();

	lisp_eval8_(NULL, "(defvar *test-lisp-warn8* nil)");
	lisp_find_class8_(x, "SIMPLE-WARNING");
	lisp_eval8_(y, "(lambda (c)"
			"  (setq *test-lisp-warn8* (simple-condition-format-control c)))");
	lisp_handler_case_(x, y);
	lisp_warn8_(str, NULL);
	test(lisp_break_control(), "lisp_warn8.1");
	lisp_reset_control();
	lisp_get_special8_(x, "*TEST-LISP-WARN8*");
	hold_value(x, &pos);
	test(strvect_equal_char(pos, "Hello"), "lisp_warn8.2");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_warn16(void)
{
	addr control, x, y, pos;
	const byte16 str[] = { 'H','e','l','l','o',0 };

	lisp_push_control(&control);
	x = Lisp_hold();
	y = Lisp_hold();

	lisp_eval8_(NULL, "(defvar *test-lisp-warn16* nil)");
	lisp_find_class8_(x, "SIMPLE-WARNING");
	lisp_eval8_(y, "(lambda (c)"
			"  (setq *test-lisp-warn16* (simple-condition-format-control c)))");
	lisp_handler_case_(x, y);
	lisp_warn16_(str, NULL);
	test(lisp_break_control(), "lisp_warn16.1");
	lisp_reset_control();
	lisp_get_special8_(x, "*TEST-LISP-WARN16*");
	hold_value(x, &pos);
	test(strvect_equal_char(pos, "Hello"), "lisp_warn16.2");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_warn32(void)
{
	addr control, x, y, pos;
	const unicode str[] = { 'H','e','l','l','o',0 };

	lisp_push_control(&control);
	x = Lisp_hold();
	y = Lisp_hold();

	lisp_eval8_(NULL, "(defvar *test-lisp-warn32* nil)");
	lisp_find_class8_(x, "SIMPLE-WARNING");
	lisp_eval8_(y, "(lambda (c)"
			"  (setq *test-lisp-warn32* (simple-condition-format-control c)))");
	lisp_handler_case_(x, y);
	lisp_warn32_(str, NULL);
	test(lisp_break_control(), "lisp_warn32.1");
	lisp_reset_control();
	lisp_get_special8_(x, "*TEST-LISP-WARN32*");
	hold_value(x, &pos);
	test(strvect_equal_char(pos, "Hello"), "lisp_warn32.2");

	lisp_pop_control_(control);

	RETURN;
}


/*
 *  Main
 */
static int testcase_extern_error(void)
{
	/* abort */
	TestBreak(test_lisp_abort);
	TestBreak(test_lisp_abortf);
	TestBreak(test_lisp_abort8);
	TestBreak(test_lisp_abort16);
	TestBreak(test_lisp_abort32);
	/* signal */
	TestBreak(test_lisp_signal);
	TestBreak(test_lisp_error);
	/* error */
	TestBreak(test_lisp_error8);
	TestBreak(test_lisp_error16);
	TestBreak(test_lisp_error32);
	/* warn */
	TestBreak(test_lisp_warn8);
	TestBreak(test_lisp_warn16);
	TestBreak(test_lisp_warn32);

	return 0;
}

static void testinit_extern_error(Execute ptr)
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

int test_extern_error(void)
{
	DegradeTitle;
	return DegradeCode(extern_error);
}

