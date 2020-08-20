#include "eval.c"

#if 0
#include "character.h"
#include "clos.h"
#include "common.h"
#include "condition.h"
#include "constant.h"
#include "degrade.h"
#include "reader.h"
#include "package.h"
#include "pathname.h"
#include "reader.h"
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
static int testcase_eval(void)
{
	TestBreak(test_eval_allocr);

	return 0;
}

static void testinit_eval(Execute ptr)
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
#endif

int test_eval(void)
{
#if 0
	DegradeTitle;
	return DegradeCode(eval);
#endif
	return 0;
}

