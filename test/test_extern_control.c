#include "extern_control.c"
#include "character.h"
#include "clos.h"
#include "common.h"
#include "constant.h"
#include "control.h"
#include "degrade.h"
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
 *  object
 */
static int test_lisp_push_control(void)
{
	addr control;

	lisp_push_control(&control);
	test(lisp_control_p(control), "lisp_push_control.1");
	lisp_pop_control_(control);

	RETURN;
}


/*
 *  special
 */
static int test_lisp_push_special(void)
{
	addr control, y, z, control2;
	Execute ptr;

	ptr = Execute_Thread;
	lisp_push_control(&control);
	y = Lisp_hold();
	lisp_intern8_(y, NULL, "HELLO");
	lisp_push_special_(y, NULL);
	internchar_default_(ptr, "HELLO", &z, NULL);
	getspecial_local(ptr, z, &z);
	test(z == Unbound, "lisp_push_special.1");

	fixnum_heap(&z, 10);
	lisp_intern8_(y, NULL, "ABC");
	lisp_push_special_(y, z);
	internchar_default_(ptr, "ABC", &z, NULL);
	getspecial_local(ptr, z, &z);
	test(fixnump(z), "lisp_push_special.2");

	lisp_push_control(&control2);
	character_heap(&z, 'A');
	lisp_intern8_(y, NULL, "ABC");
	lisp_push_special_(y, z);
	internchar_default_(ptr, "ABC", &z, NULL);
	getspecial_local(ptr, z, &z);
	test(characterp(z), "lisp_push_special.3");
	lisp_pop_control_(control2);

	internchar_default_(ptr, "ABC", &z, NULL);
	getspecial_local(ptr, z, &z);
	test(fixnump(z), "lisp_push_special.4");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_push_special8(void)
{
	addr control, y, z;
	Execute ptr;
	fixnum v;

	ptr = Execute_Thread;
	lisp_push_control(&control);
	y = Lisp_hold();
	lisp_fixnum(y, 11);
	lisp_push_special8_("HELLO", y);

	internchar_default_(ptr, "HELLO", &z, NULL);
	getspecial_local(ptr, z, &z);
	GetFixnum(z, &v);
	test(v == 11, "lisp_push_special8.1");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_push_special16(void)
{
	addr control, y, z;
	Execute ptr;
	fixnum v;
	const byte16 str[] = { 'H','E','L','L','O',0 };

	ptr = Execute_Thread;
	lisp_push_control(&control);
	y = Lisp_hold();
	lisp_fixnum(y, 11);
	lisp_push_special16_((const void *)str, y);

	internchar_default_(ptr, "HELLO", &z, NULL);
	getspecial_local(ptr, z, &z);
	GetFixnum(z, &v);
	test(v == 11, "lisp_push_special16.1");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_push_special32(void)
{
	addr control, y, z;
	Execute ptr;
	fixnum v;
	const unicode str[] = { 'H','E','L','L','O',0 };

	ptr = Execute_Thread;
	lisp_push_control(&control);
	y = Lisp_hold();
	lisp_fixnum(y, 11);
	lisp_push_special32_((const void *)str, y);

	internchar_default_(ptr, "HELLO", &z, NULL);
	getspecial_local(ptr, z, &z);
	GetFixnum(z, &v);
	test(v == 11, "lisp_push_special32.1");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_get_special(void)
{
	addr control, x, y, z;
	fixnum value;

	lisp_push_control(&control);
	x = Lisp_hold();
	y = Lisp_hold();
	lisp_fixnum(y, 22);
	lisp_push_special8_("HELLO", y);

	lisp_intern8_(y, NULL, "HELLO");
	lisp0_get_special_(&z, y);
	value = 0;
	GetFixnum(z, &value);
	test(value == 22, "lisp_get_special.1");

	lisp_intern8_(y, NULL, "HELLO");
	lisp_get_special_(x, y);
	lisp_hold_value(x, &z);
	value = 0;
	GetFixnum(z, &value);
	test(value == 22, "lisp_get_special.2");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_get_special8(void)
{
	addr control, x, y, z;
	fixnum value;

	lisp_push_control(&control);
	x = Lisp_hold();
	y = Lisp_hold();
	lisp_fixnum(y, 22);
	lisp_push_special8_("HELLO", y);

	lisp0_get_special8_(&z, "HELLO");
	value = 0;
	GetFixnum(z, &value);
	test(value == 22, "lisp_get_special8.1");

	lisp_get_special8_(x, "HELLO");
	lisp_hold_value(x, &z);
	value = 0;
	GetFixnum(z, &value);
	test(value == 22, "lisp_get_special8.2");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_get_special16(void)
{
	addr control, x, y, z;
	fixnum value;
	const byte16 str[] = { 'H','E','L','L','O',0 };

	lisp_push_control(&control);
	x = Lisp_hold();
	y = Lisp_hold();
	lisp_fixnum(y, 22);
	lisp_push_special8_("HELLO", y);

	lisp0_get_special16_(&z, (const void *)str);
	value = 0;
	GetFixnum(z, &value);
	test(value == 22, "lisp_get_special16.1");

	lisp_get_special16_(x, (const void *)str);
	lisp_hold_value(x, &z);
	value = 0;
	GetFixnum(z, &value);
	test(value == 22, "lisp_get_special16.2");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_get_special32(void)
{
	addr control, x, y, z;
	fixnum value;
	const unicode str[] = { 'H','E','L','L','O',0 };

	lisp_push_control(&control);
	x = Lisp_hold();
	y = Lisp_hold();
	lisp_fixnum(y, 22);
	lisp_push_special8_("HELLO", y);

	lisp0_get_special32_(&z, (const void *)str);
	value = 0;
	GetFixnum(z, &value);
	test(value == 22, "lisp_get_special32.1");

	lisp_get_special32_(x, (const void *)str);
	lisp_hold_value(x, &z);
	value = 0;
	GetFixnum(z, &value);
	test(value == 22, "lisp_get_special32.2");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_set_special(void)
{
	addr control, x, y, z;
	fixnum value;

	lisp_push_control(&control);
	x = Lisp_hold();
	y = Lisp_hold();
	lisp_push_special8_("HELLO", NULL);

	lisp_intern8_(y, NULL, "HELLO");
	fixnum_heap(&z, 33);
	lisp_set_special_(y, z);
	lisp0_get_special8_(&z, "HELLO");
	value = 0;
	GetFixnum(z, &value);
	test(value == 33, "lisp_get_special.1");

	lisp_intern8_(y, NULL, "HELLO");
	lisp_set_special_(y, NULL);
	lisp_get_special8_(x, "HELLO");
	test(lisp_null_p(x), "lisp_get_special.2");

	lisp_intern8_(y, NULL, "HELLO");
	lisp_fixnum(x, 44);
	lisp_set_special_(y, x);
	lisp0_get_special8_(&z, "HELLO");
	value = 0;
	GetFixnum(z, &value);
	test(value == 44, "lisp_get_special.3");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_set_special8(void)
{
	addr control, x;
	fixnum value;

	lisp_push_control(&control);
	lisp_push_special8_("HELLO", NULL);

	fixnum_heap(&x, 33);
	lisp_set_special8_("HELLO", x);
	lisp0_get_special8_(&x, "HELLO");
	value = 0;
	GetFixnum(x, &value);
	test(value == 33, "lisp_get_special8.1");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_set_special16(void)
{
	addr control, x;
	fixnum value;
	const byte16 str[] = { 'H','E','L','L','O',0 };

	lisp_push_control(&control);
	lisp_push_special8_("HELLO", NULL);

	fixnum_heap(&x, 33);
	lisp_set_special16_((const void *)str, x);
	lisp0_get_special8_(&x, "HELLO");
	value = 0;
	GetFixnum(x, &value);
	test(value == 33, "lisp_get_special16.1");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_set_special32(void)
{
	addr control, x;
	fixnum value;
	const unicode str[] = { 'H','E','L','L','O',0 };

	lisp_push_control(&control);
	lisp_push_special8_("HELLO", NULL);

	fixnum_heap(&x, 33);
	lisp_set_special32_((const void *)str, x);
	lisp0_get_special8_(&x, "HELLO");
	value = 0;
	GetFixnum(x, &value);
	test(value == 33, "lisp_get_special32.1");

	lisp_pop_control_(control);

	RETURN;
}


/*
 *  Main
 */
static int testcase_extern_control(void)
{
	/* stack frame */
	TestBreak(test_lisp_push_control);
	/* special */
	TestBreak(test_lisp_push_special);
	TestBreak(test_lisp_push_special8);
	TestBreak(test_lisp_push_special16);
	TestBreak(test_lisp_push_special32);
	TestBreak(test_lisp_get_special);
	TestBreak(test_lisp_get_special8);
	TestBreak(test_lisp_get_special16);
	TestBreak(test_lisp_get_special32);
	TestBreak(test_lisp_set_special);
	TestBreak(test_lisp_set_special8);
	TestBreak(test_lisp_set_special16);
	TestBreak(test_lisp_set_special32);

	return 0;
}

static void testinit_extern_control(Execute ptr)
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

int test_extern_control(void)
{
	DegradeTitle;
	return DegradeCode(extern_control);
}

