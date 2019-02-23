#include "clos.c"
#include "degrade.h"
#include "execute.h"

/*
 *  call object
 */
static int test_build_clos(void)
{
	build_clos(Execute_Thread);
	test(1, "build_clos1");

	RETURN;
}


/*
 *  main
 */
static int testbreak_clos(void)
{
	TestBreak(test_build_clos);

	return 0;
}

int test_clos(void)
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
		build_package();
		lisp_init = 1;
		result = testbreak_clos();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

