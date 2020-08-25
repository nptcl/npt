#include "extern_function.c"
#include "character.h"
#include "clos.h"
#include "common.h"
#include "constant.h"
#include "control.h"
#include "degrade.h"
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
 *  function
 */
static int test_lisp_get_function(void)
{
	enum PACKAGE_TYPE type;
	Execute ptr;
	addr control, x, pos;

	ptr = Execute_Thread;
	lisp_push_control(&control);
	x = Lisp_hold();

	internchar_default_(ptr, "HELLO", &pos, &type);
	setspecial_local(ptr, pos, Unbound);
	lisp0_get_function(&pos, pos);
	test(pos == NULL, "lisp_get_function.1");

	lisp0_intern8_(&pos, NULL, "CAR");
	setspecial_local(ptr, pos, Unbound);
	lisp0_get_function(&pos, pos);
	test(functionp(pos), "lisp_get_function.2");

	internchar_default_(ptr, "HELLO", &pos, &type);
	setspecial_local(ptr, pos, Unbound);
	lisp_get_function(x, pos);
	test(lisp_null_p(x), "lisp_get_function.3");

	lisp0_intern8_(&pos, NULL, "CAR");
	setspecial_local(ptr, pos, Unbound);
	lisp_get_function(x, pos);
	test(lisp_function_p(x), "lisp_get_function.4");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_get_setf(void)
{
	enum PACKAGE_TYPE type;
	Execute ptr;
	addr control, x, pos;

	ptr = Execute_Thread;
	lisp_push_control(&control);
	x = Lisp_hold();

	internchar_default_(ptr, "HELLO", &pos, &type);
	setsetf_symbol(pos, Unbound);
	lisp0_get_setf(&pos, pos);
	test(pos == NULL, "lisp_get_setf.1");

	lisp0_intern8_(&pos, NULL, "CAR");
	setspecial_local(ptr, pos, Unbound);
	lisp0_get_setf(&pos, pos);
	test(functionp(pos), "lisp_get_setf.2");

	internchar_default_(ptr, "HELLO", &pos, &type);
	setspecial_local(ptr, pos, Unbound);
	lisp_get_setf(x, pos);
	test(lisp_null_p(x), "lisp_get_setf.3");

	lisp0_intern8_(&pos, NULL, "CAR");
	setspecial_local(ptr, pos, Unbound);
	lisp_get_setf(x, pos);
	test(lisp_function_p(x), "lisp_get_setf.4");

	lisp_pop_control_(control);

	RETURN;
}


/*
 *  Main
 */
static int testcase_extern_function(void)
{
	/* function */
	TestBreak(test_lisp_get_function);
	TestBreak(test_lisp_get_setf);

	return 0;
}

static void testinit_extern_function(Execute ptr)
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

int test_extern_function(void)
{
	DegradeTitle;
	return DegradeCode(extern_function);
}

