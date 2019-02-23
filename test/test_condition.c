#include "condition.c"
#include "calltype.h"
#include "character.h"
#include "clos.h"
#include "code.h"
#include "common.h"
#include "constant.h"
#include "eval.h"
#include "degrade.h"
#include "object.h"
#include "package.h"
#include "pathname.h"
#include "stream.h"
#include "symbol.h"
#include "syscall.h"

static int test_condition_class(void)
{
	//invoke_debugger(Nil);
	return 0;
}


/*
 *  Main
 */
static int testbreak_condition(void)
{
	TestBreak(test_condition_class);

	return 0;
}

int test_condition(void)
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
		build_calltype();
		build_syscall();
		build_common();
		build_readtable();
		build_pathname();
		build_eval_declare();
		build_code();
		lisp_init = 1;
		result = testbreak_condition();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

