#include "extern_object.c"
#include "character.h"
#include "clos.h"
#include "common.h"
#include "constant.h"
#include "control.h"
#include "degrade.h"
#include "integer.h"
#include "object.h"
#include "package.h"
#include "pathname.h"
#include "reader.h"
#include "stream.h"
#include "symbol.h"
#include "syscall.h"
#include "type.h"
#include "type_table.h"

/*
 *  object
 */
static int test_lisp_character(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;
	unicode c;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	lisp0_character_(&y, 'A');
	test(characterp(y), "lisp_character.1");
	GetCharacter(y, &c);
	test(c == 'A', "lisp_character.2");

	lisp_character_(x, 'a');
	test(lisp_character_p(x), "lisp_character.3");
	lisp_hold_value(x, &y);
	GetCharacter(y, &c);
	test(c == 'a', "lisp_character.4");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_fixnum(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;
	fixnum v;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	lisp0_fixnum(&y, 123);
	test(fixnump(y), "lisp_fixnum.1");
	GetFixnum(y, &v);
	test(v == 123, "lisp_fixnum.2");

	lisp_fixnum(x, 456);
	test(lisp_fixnum_p(x), "lisp_fixnum.3");
	lisp_hold_value(x, &y);
	GetFixnum(y, &v);
	test(v == 456, "lisp_fixnum.4");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_float(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;
	single_float v;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	lisp0_float_(&y, 1.23f);
	test(single_float_p(y), "lisp_single_float.1");
	GetSingleFloat(y, &v);
	test(v == 1.23f, "lisp_single_float.2");

	lisp_float_(x, 4.56f);
	test(lisp_single_float_p(x), "lisp_single_float.3");
	lisp_hold_value(x, &y);
	GetSingleFloat(y, &v);
	test(v == 4.56f, "lisp_single_float.4");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_double(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;
	double_float v;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	lisp0_double_(&y, 1.23);
	test(double_float_p(y), "lisp_double_float.1");
	GetDoubleFloat(y, &v);
	test(v == 1.23, "lisp_double_float.2");

	lisp_double_(x, 4.56);
	test(lisp_double_float_p(x), "lisp_double_float.3");
	lisp_hold_value(x, &y);
	GetDoubleFloat(y, &v);
	test(v == 4.56, "lisp_double_float.4");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_long_double(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;
	long_float v;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	lisp0_long_double_(&y, 1.23L);
	test(long_float_p(y), "lisp_long_float.1");
	GetLongFloat(y, &v);
	test(v == 1.23L, "lisp_long_float.2");

	lisp_long_double_(x, 4.56L);
	test(lisp_long_float_p(x), "lisp_long_float.3");
	lisp_hold_value(x, &y);
	GetLongFloat(y, &v);
	test(v == 4.56L, "lisp_long_float.4");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_zero_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	test(lisp_zero_p(T) == 0, "lisp_zero_p.1");
	fixnum_heap(&y, 0);
	test(lisp_zero_p(y), "lisp_zero_p.2");
	lisp_hold_set(x, y);
	test(lisp_zero_p(x), "lisp_zero_p.3");
	fixnum_heap(&y, 10);
	test(! lisp_zero_p(y), "lisp_zero_p.4");
	fixnum_heap(&y, -20);
	test(! lisp_zero_p(y), "lisp_zero_p.5");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_plus_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	test(lisp_plus_p(T) == 0, "lisp_plus_p.1");
	fixnum_heap(&y, 10);
	test(lisp_plus_p(y), "lisp_plus_p.2");
	lisp_hold_set(x, y);
	test(lisp_plus_p(x), "lisp_plus_p.3");
	fixnum_heap(&y, 0);
	test(! lisp_plus_p(y), "lisp_plus_p.4");
	fixnum_heap(&y, -20);
	test(! lisp_plus_p(y), "lisp_plus_p.5");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_minus_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	test(lisp_minus_p(T) == 0, "lisp_minus_p.1");
	fixnum_heap(&y, -10);
	test(lisp_minus_p(y), "lisp_minus_p.2");
	lisp_hold_set(x, y);
	test(lisp_minus_p(x), "lisp_minus_p.3");
	fixnum_heap(&y, 0);
	test(! lisp_minus_p(y), "lisp_minus_p.4");
	fixnum_heap(&y, 10);
	test(! lisp_minus_p(y), "lisp_minus_p.5");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_get_character(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;
	unicode c;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	character_heap(&y, 'A');
	lisp_get_character(y, &c);
	test(c == 'A', "lisp_get_character.1");

	character_heap(&y, 'z');
	lisp_hold_set(x, y);
	lisp_get_character(x, &c);
	test(c == 'z', "lisp_get_character.2");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_get_fixnum(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;
	fixnum v;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	fixnum_heap(&y, 123);
	lisp_get_fixnum(y, &v);
	test(v == 123, "lisp_get_fixnum.1");

	fixnum_heap(&y, 456);
	lisp_hold_set(x, y);
	lisp_get_fixnum(x, &v);
	test(v == 456, "lisp_get_fixnum.2");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_get_float(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;
	single_float v;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	single_float_heap(&y, 1.23f);
	lisp_get_float_(y, &v);
	test(v == 1.23f, "lisp_get_float.1");

	fixnum_heap(&y, 456);
	lisp_hold_set(x, y);
	lisp_get_float_(x, &v);
	test(v == 456.0f, "lisp_get_float.2");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_get_double(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;
	double_float v;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	double_float_heap(&y, 1.23);
	lisp_get_double_(y, &v);
	test(v == 1.23, "lisp_get_double.1");

	fixnum_heap(&y, 456);
	lisp_hold_set(x, y);
	lisp_get_double_(x, &v);
	test(v == 456.0, "lisp_get_double.2");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_get_long_double(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;
	long_float v;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	long_float_heap(&y, 1.23L);
	lisp_get_long_double_(y, &v);
	test(v == 1.23L, "lisp_get_long_double.1");

	fixnum_heap(&y, 456);
	lisp_hold_set(x, y);
	lisp_get_long_double_(x, &v);
	test(v == 456.0L, "lisp_get_long_double.2");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  Main
 */
static int testcase_extern_object(void)
{
	/* object */
	TestBreak(test_lisp_character);
	TestBreak(test_lisp_fixnum);
	TestBreak(test_lisp_float);
	TestBreak(test_lisp_double);
	TestBreak(test_lisp_long_double);
	TestBreak(test_lisp_zero_p);
	TestBreak(test_lisp_plus_p);
	TestBreak(test_lisp_minus_p);
	TestBreak(test_lisp_get_character);
	TestBreak(test_lisp_get_fixnum);
	TestBreak(test_lisp_get_float);
	TestBreak(test_lisp_get_double);
	TestBreak(test_lisp_get_long_double);

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

