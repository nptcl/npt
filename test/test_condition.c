#include "condition.c"
#include "character.h"
#include "clos.h"
#include "code.h"
#include "common.h"
#include "constant.h"
#include "declare.h"
#include "eval.h"
#include "degrade.h"
#include "object.h"
#include "package.h"
#include "pathname.h"
#include "reader.h"
#include "stream.h"
#include "symbol.h"
#include "syscall.h"
#include "type_table.h"

static int test_condition_class(void)
{
	//invoke_debugger(Nil);
	return 0;
}


/*
 *  Main
 */
static int testcase_condition(void)
{
	TestBreak(test_condition_class);

	return 0;
}

static void testinit_condition(Execute ptr)
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

int test_condition(void)
{
	DegradeTitle;
	return DegradeCode(condition);
}

