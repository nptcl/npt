#include "extern_instance.c"
#include "character.h"
#include "clos.h"
#include "code.h"
#include "common.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "control.h"
#include "degrade.h"
#include "declare.h"
#include "extern_control.h"
#include "extern_sequence.h"
#include "extern_object.h"
#include "extern_type.h"
#include "integer.h"
#include "object.h"
#include "package.h"
#include "package_symbol.h"
#include "pathname.h"
#include "reader.h"
#include "stream.h"
#include "strvect.h"
#include "symbol.h"
#include "syscall.h"
#include "type.h"
#include "type_table.h"

/*
 *  eval
 */
static int test_lisp_find_class(void)
{
	return 0;
}


/*
 *  Main
 */
static int testcase_extern_instance(void)
{
	TestBreak(test_lisp_find_class);

	return 0;
}

static void testinit_extern_instance(Execute ptr)
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

int test_extern_instance(void)
{
	DegradeTitle;
	return DegradeCode(extern_instance);
}

