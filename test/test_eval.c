#include "eval.c"
#include "character.h"
#include "clos.h"
#include "common.h"
#include "condition.h"
#include "constant.h"
#include "degrade.h"
#include "readtable.h"
#include "package.h"
#include "pathname.h"
#include "readtable.h"
#include "stream.h"
#include "symbol.h"
#include "syscall.h"
#include "type.h"
#include "type_table.h"

static int test_eval_allocr(void)
{
	addr pos;

	pos = eval_allocr(NULL, EVAL_TYPE_DECLARE, 10, 20);
	test(GetType(pos) == LISPTYPE_EVAL, "eval_allocr1");
	test(GetUser(pos) == EVAL_TYPE_DECLARE, "eval_allocr2");

	RETURN;
}


/*
 *  Main
 */
static int testbreak_eval(void)
{
	TestBreak(test_eval_allocr);

	return 0;
}

int test_eval(void)
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
		lisp_init = 1;
		result = testbreak_eval();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

