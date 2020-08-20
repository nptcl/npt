#include "extern_object.c"
#include "character.h"
#include "clos.h"
#include "common.h"
#include "constant.h"
#include "control.h"
#include "degrade.h"
#include "object.h"
#include "package.h"
#include "pathname.h"
#include "reader.h"
#include "stream.h"
#include "symbol.h"
#include "syscall.h"
#include "type.h"
#include "type_table.h"

static int test_lisp0_car(void)
{
	return 0;
}


/*
 *  Main
 */
static int testcase_extern_object(void)
{
	TestBreak(test_lisp0_car);

	return 0;
}

static void testinit_extern_object(Execute ptr)
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
}

int test_extern_object(void)
{
	DegradeTitle;
	return DegradeCode(extern_object);
}

