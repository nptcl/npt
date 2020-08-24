#include "extern_type.c"
#include "bignum_object.h"
#include "character.h"
#include "clos.h"
#include "cmpl.h"
#include "common.h"
#include "condition.h"
#include "constant.h"
#include "control.h"
#include "degrade.h"
#include "integer.h"
#include "object.h"
#include "package.h"
#include "pathname.h"
#include "reader.h"
#include "stream.h"
#include "strvect.h"
#include "symbol.h"
#include "syscall.h"
#include "type.h"
#include "type_table.h"

/*
 *  hold
 */
static int test_lisp_hold_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos;

	test(! lisp_hold_p(NULL), "lisp_hold_p.1");
	test(! lisp_hold_p(Unbound), "lisp_hold_p.2");
	test(! lisp_hold_p(Nil), "lisp_hold_p.3");
	test(! lisp_hold_p(T), "lisp_hold_p.4");

	local = Local_Thread;
	push_local(local, &stack);
	hold_local(local, &pos, T);
	test(lisp_hold_p(pos), "lisp_hold_p.5");
	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_hold_value(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);
	hold_local(local, &x, T);
	lisp_hold_value(x, &x);
	test(x == T, "lisp_hold_value.1");

	lisp_hold_value(Nil, &x);
	test(x == Nil, "lisp_hold_value.2");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_hold_set(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;

	local = Local_Thread;
	push_local(local, &stack);
	hold_local(local, &x, T);
	lisp_hold_set(x, Nil);
	lisp_hold_value(x, &y);
	test(y == Nil, "lisp_hold_set.1");

	lisp_hold_set(x, T);
	lisp_hold_value(x, &y);
	test(y == T, "lisp_hold_set.2");

	rollback_local(local, stack);

	RETURN;
}

static int test_Lisp_holdv(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);
	hold_local(local, &x, T);
	test(Lisp_holdv(x) == T, "Lisp_holdv.1");
	hold_local(local, &x, Nil);
	test(Lisp_holdv(x) == Nil, "Lisp_holdv.2");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_hold(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);
	lisp_hold(&x, T);
	test(Lisp_holdv(x) == T, "lisp_hold.1");
	lisp_hold(&x, x);
	test(Lisp_holdv(x) == T, "lisp_hold.2");

	rollback_local(local, stack);

	RETURN;
}

static int test_Lisp_hold(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();
	test(Lisp_holdv(x) == Nil, "Lisp_hold.1");
	rollback_local(local, stack);

	RETURN;
}


/*
 *  nil, t
 */
static int test_lisp_nil(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);

	lisp0_nil(&x);
	test(x == Nil, "lisp_nil.1");

	test(Lisp_nil() == Nil, "lisp_nil.2");

	lisp_hold(&x, T);
	lisp_nil(x);
	test(Lisp_holdv(x) == Nil, "lisp_nil.3");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_t(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);

	lisp0_t(&x);
	test(x == T, "lisp_t.1");

	test(Lisp_t() == T, "lisp_t.2");

	lisp_hold(&x, Nil);
	lisp_t(x);
	test(Lisp_holdv(x) == T, "lisp_t.3");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  type
 */
static int test_lisp_nil_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	test(lisp_nil_p(Nil), "lisp_nil_p.1");
	test(! lisp_nil_p(T), "lisp_nil_p.2");
	lisp_hold_set(x, Nil);
	test(lisp_nil_p(x), "lisp_nil_p.3");
	lisp_hold_set(x, T);
	test(! lisp_nil_p(x), "lisp_nil_p.4");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_t_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	test(lisp_t_p(T), "lisp_t_p.1");
	test(! lisp_t_p(Nil), "lisp_t_p.2");
	lisp_hold_set(x, T);
	test(lisp_t_p(x), "lisp_t_p.3");
	lisp_hold_set(x, Nil);
	test(! lisp_t_p(x), "lisp_t_p.4");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_null_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	test(lisp_null_p(NULL), "lisp_null_p.1");
	test(! lisp_null_p(Nil), "lisp_null_p.2");
	lisp_hold_set(x, NULL);
	test(lisp_null_p(x), "lisp_null_p.3");
	lisp_hold_set(x, Nil);
	test(! lisp_null_p(x), "lisp_null_p.4");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_character_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	character_heap(&y, 'A');
	test(lisp_character_p(y), "lisp_character_p.1");
	test(! lisp_character_p(Nil), "lisp_character_p.2");
	lisp_hold_set(x, y);
	test(lisp_character_p(x), "lisp_character_p.3");
	lisp_hold_set(x, Nil);
	test(! lisp_character_p(x), "lisp_character_p.4");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_cons_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	consnil_heap(&y);
	test(lisp_cons_p(y), "lisp_cons_p.1");
	test(! lisp_cons_p(Nil), "lisp_cons_p.2");
	lisp_hold_set(x, y);
	test(lisp_cons_p(x), "lisp_cons_p.3");
	lisp_hold_set(x, Nil);
	test(! lisp_cons_p(x), "lisp_cons_p.4");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_list_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	consnil_heap(&y);
	test(lisp_list_p(y), "lisp_list_p.1");
	test(lisp_list_p(Nil), "lisp_list_p.2");
	test(! lisp_list_p(T), "lisp_list_p.3");
	lisp_hold_set(x, y);
	test(lisp_list_p(x), "lisp_list_p.4");
	lisp_hold_set(x, Nil);
	test(lisp_list_p(x), "lisp_list_p.5");
	lisp_hold_set(x, T);
	test(! lisp_list_p(x), "lisp_list_p.6");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_string_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y, z;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	strvect_char_heap(&y, "YYY");
	strarray_char_heap_(&z, "ZZZ");
	test(lisp_string_p(y), "lisp_string_p.1");
	test(lisp_string_p(z), "lisp_string_p.2");
	test(! lisp_string_p(T), "lisp_string_p.3");
	lisp_hold_set(x, y);
	test(lisp_string_p(x), "lisp_string_p.4");
	lisp_hold_set(x, z);
	test(lisp_string_p(x), "lisp_string_p.5");
	lisp_hold_set(x, T);
	test(! lisp_string_p(x), "lisp_string_p.6");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_strvect_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y, z;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	strvect_char_heap(&y, "YYY");
	strarray_char_heap_(&z, "ZZZ");
	test(lisp_strvect_p(y), "lisp_strvect_p.1");
	test(! lisp_strvect_p(z), "lisp_strvect_p.2");
	test(! lisp_strvect_p(T), "lisp_strvect_p.3");
	lisp_hold_set(x, y);
	test(lisp_strvect_p(x), "lisp_strvect_p.4");
	lisp_hold_set(x, z);
	test(! lisp_strvect_p(x), "lisp_strvect_p.5");
	lisp_hold_set(x, T);
	test(! lisp_strvect_p(x), "lisp_strvect_p.6");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_array_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y, z;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	strarray_char_heap_(&y, "YYY");
	strvect_char_heap(&z, "ZZZ");
	test(lisp_array_p(y), "lisp_array_p.1");
	test(! lisp_array_p(z), "lisp_array_p.2");
	test(! lisp_array_p(T), "lisp_array_p.3");
	lisp_hold_set(x, y);
	test(lisp_array_p(x), "lisp_array_p.4");
	lisp_hold_set(x, z);
	test(! lisp_array_p(x), "lisp_array_p.5");
	lisp_hold_set(x, T);
	test(! lisp_array_p(x), "lisp_array_p.6");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_vector_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y, z;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	vector_heap(&y, 10);
	strarray_char_heap_(&z, "ZZZ");
	test(lisp_vector_p(y), "lisp_vector_p.1");
	test(! lisp_vector_p(z), "lisp_vector_p.2");
	test(! lisp_vector_p(T), "lisp_vector_p.3");
	lisp_hold_set(x, y);
	test(lisp_vector_p(x), "lisp_vector_p.4");
	lisp_hold_set(x, z);
	test(! lisp_vector_p(x), "lisp_vector_p.5");
	lisp_hold_set(x, T);
	test(! lisp_vector_p(x), "lisp_vector_p.6");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  number
 */
static int test_lisp_fixnum_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	fixnum_heap(&y, 10);
	test(lisp_fixnum_p(y), "lisp_fixnum_p.1");
	test(! lisp_fixnum_p(Nil), "lisp_fixnum_p.2");
	lisp_hold_set(x, y);
	test(lisp_fixnum_p(x), "lisp_fixnum_p.3");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_bignum_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y, z;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	bignum_value_heap(&y, signplus_bignum, 1);
	fixnum_heap(&z, 2);
	test(lisp_bignum_p(y), "lisp_bignum_p.1");
	test(! lisp_bignum_p(z), "lisp_bignum_p.2");
	lisp_hold_set(x, y);
	test(lisp_bignum_p(x), "lisp_bignum_p.3");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_integer_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y, z;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	bignum_value_heap(&y, signplus_bignum, 1);
	fixnum_heap(&z, 2);
	test(lisp_integer_p(y), "lisp_integer_p.1");
	test(lisp_integer_p(z), "lisp_integer_p.2");
	test(! lisp_integer_p(T), "lisp_integer_p.3");
	lisp_hold_set(x, y);
	test(lisp_integer_p(x), "lisp_integer_p.4");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_ratio_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y, z;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	ratio_reduction_value_heap(local, &y, signplus_bignum, 4, 5);
	fixnum_heap(&z, 2);
	test(lisp_ratio_p(y), "lisp_ratio_p.1");
	test(! lisp_ratio_p(z), "lisp_ratio_p.2");
	lisp_hold_set(x, y);
	test(lisp_ratio_p(x), "lisp_ratio_p.3");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_rational_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y, z;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	ratio_reduction_value_heap(local, &y, signplus_bignum, 4, 5);
	fixnum_heap(&z, 2);
	test(lisp_rational_p(y), "lisp_rational_p.1");
	test(lisp_rational_p(z), "lisp_rational_p.2");
	test(! lisp_rational_p(T), "lisp_rational_p.3");
	lisp_hold_set(x, y);
	test(lisp_rational_p(x), "lisp_rational_p.4");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_single_float_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y, z;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	single_float_heap(&y, 1.23f);
	double_float_heap(&z, 4.56);
	test(lisp_single_float_p(y), "lisp_single_float_p.1");
	test(! lisp_single_float_p(z), "lisp_single_float_p.2");
	lisp_hold_set(x, y);
	test(lisp_single_float_p(x), "lisp_single_float_p.3");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_double_float_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y, z;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	double_float_heap(&y, 1.23);
	long_float_heap(&z, 4.56L);
	test(lisp_double_float_p(y), "lisp_double_float_p.1");
	test(! lisp_double_float_p(z), "lisp_double_float_p.2");
	lisp_hold_set(x, y);
	test(lisp_double_float_p(x), "lisp_double_float_p.3");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_long_float_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y, z;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	long_float_heap(&y, 1.23L);
	single_float_heap(&z, 4.56f);
	test(lisp_long_float_p(y), "lisp_long_float_p.1");
	test(! lisp_long_float_p(z), "lisp_long_float_p.2");
	lisp_hold_set(x, y);
	test(lisp_long_float_p(x), "lisp_long_float_p.3");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_float_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y, z;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	long_float_heap(&y, 1.23L);
	fixnum_heap(&z, 456);
	test(lisp_float_p(y), "lisp_float_p.1");
	test(! lisp_float_p(z), "lisp_float_p.2");
	lisp_hold_set(x, y);
	test(lisp_float_p(x), "lisp_float_p.3");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_real_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y, z;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	fixnum_heap(&y, 123);
	complex_double_heap_(&z, 2.3, 4.5);
	test(lisp_real_p(y), "lisp_real_p.1");
	test(! lisp_real_p(z), "lisp_real_p.2");
	lisp_hold_set(x, y);
	test(lisp_real_p(x), "lisp_real_p.3");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_number_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y, z;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	fixnum_heap(&y, 123);
	complex_double_heap_(&z, 2.3, 4.5);
	test(lisp_number_p(y), "lisp_number_p.1");
	test(lisp_number_p(z), "lisp_number_p.2");
	test(! lisp_number_p(T), "lisp_number_p.3");
	lisp_hold_set(x, y);
	test(lisp_number_p(x), "lisp_number_p.4");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  object
 */
static int test_lisp_clos_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	GetConst(CLOS_STANDARD_CLASS, &y);
	test(lisp_clos_p(y), "lisp_clos_p.1");
	test(! lisp_clos_p(T), "lisp_clos_p.2");
	lisp_hold_set(x, y);
	test(lisp_clos_p(x), "lisp_clos_p.3");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  Main
 */
static int testcase_extern_type(void)
{
	/* hold */
	TestBreak(test_lisp_hold_p);
	TestBreak(test_lisp_hold_value);
	TestBreak(test_lisp_hold_set);
	TestBreak(test_Lisp_holdv);
	TestBreak(test_lisp_hold);
	TestBreak(test_Lisp_hold);
	/* nil, t */
	TestBreak(test_lisp_nil);
	TestBreak(test_lisp_t);
	/* type */
	TestBreak(test_lisp_nil_p);
	TestBreak(test_lisp_t_p);
	TestBreak(test_lisp_null_p);
	TestBreak(test_lisp_character_p);
	TestBreak(test_lisp_cons_p);
	TestBreak(test_lisp_list_p);
	TestBreak(test_lisp_string_p);
	TestBreak(test_lisp_strvect_p);
	TestBreak(test_lisp_array_p);
	TestBreak(test_lisp_vector_p);
	/* number */
	TestBreak(test_lisp_fixnum_p);
	TestBreak(test_lisp_bignum_p);
	TestBreak(test_lisp_integer_p);
	TestBreak(test_lisp_ratio_p);
	TestBreak(test_lisp_rational_p);
	TestBreak(test_lisp_single_float_p);
	TestBreak(test_lisp_double_float_p);
	TestBreak(test_lisp_long_float_p);
	TestBreak(test_lisp_float_p);
	TestBreak(test_lisp_real_p);
	TestBreak(test_lisp_number_p);
	/* object */
	TestBreak(test_lisp_clos_p);

	return 0;
}

static void testinit_extern_type(Execute ptr)
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

int test_extern_type(void)
{
	DegradeTitle;
	return DegradeCode(extern_type);
}

