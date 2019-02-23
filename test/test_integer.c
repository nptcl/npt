#include "integer.c"
#include "bigcons.h"
#include "calltype.h"
#include "character.h"
#include "clos.h"
#include "common.h"
#include "control.h"
#include "constant.h"
#include "degrade.h"
#include "object.h"
#include "print.h"
#include "package.h"
#include "pathname.h"
#include "ratio.h"
#include "readtable.h"
#include "strtype.h"
#include "symbol.h"
#include "syscall.h"
#include "type.h"

/*
 *  main
 */
static int testbreak_integer(void)
{
	return 0;
}

int test_integer(void)
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
		lisp_init = 1;
		result = testbreak_integer();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

