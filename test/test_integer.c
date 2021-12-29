#include "integer.c"
#include "bignum_cons.h"
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
#include "reader.h"
#include "stream_init.h"
#include "strtype.h"
#include "symbol.h"
#include "syscall.h"
#include "type.h"
#include "type_table.h"

/*
 *  main
 */
static int testcase_integer(void)
{
	return 0;
}

static void testinit_integer(Execute ptr)
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
}

int test_integer(void)
{
	DegradeTitle;
	return DegradeCode(integer);
}

