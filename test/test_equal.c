#include "equal.c"
#include "character.h"
#include "clos.h"
#include "common.h"
#include "constant.h"
#include "degrade.h"
#include "reader.h"
#include "package.h"
#include "pathname.h"
#include "stream.h"
#include "sequence.h"
#include "symbol.h"
#include "syscall.h"
#include "type.h"
#include "type_table.h"

static int test_atom_function(void)
{
	addr pos;

	fixnum_heap(&pos, 10);
	test(atom_function(pos), "atom_function1");
	test(atom_function(T), "atom_function2");
	test(atom_function(Nil), "atom_function3");
	consnil_heap(&pos);
	test(! atom_function(pos), "atom_function4");

	RETURN;
}

static int test_eq_function(void)
{
	addr left, right;

	test(eq_function(Nil, Nil), "eq_function1");
	test(eq_function(T, T), "eq_function2");
	fixnum_heap(&left, 10);
	test(eq_function(left, left), "eq_function3");
	strvect_char_heap(&left, "Hello");
	strvect_char_heap(&right, "Hello");
	test(! eq_function(left, right), "eq_function4");

	RETURN;
}

static int test_eql_function_fixnum(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, denom, numer;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_local(local, &left, 10);
	test(! eql_function_fixnum(left, LISPTYPE_NIL, Nil), "eql_function_fixnum1");

	fixnum_local(local, &right, 10);
	test(eql_function_fixnum(left, LISPTYPE_FIXNUM, right), "eql_function_fixnum2");
	test(! eq_function(left, right), "eql_function_fixnum3");

	fixnum_local(local, &left, 10);
	fixnum_local(local, &right, 20);
	test(! eql_function_fixnum(left, LISPTYPE_FIXNUM, right), "eql_function_fixnum4");
	bignum_value_alloc(local, &right, signplus_bignum, 10);
	test(eql_function_fixnum(left, LISPTYPE_BIGNUM, right), "eql_function_fixnum5");
	bignum_value_alloc(local, &right, signminus_bignum, 10);
	test(! eql_function_fixnum(left, LISPTYPE_BIGNUM, right), "eql_function_fixnum6");
	bignum_value_alloc(local, &right, signplus_bignum, 20);
	test(! eql_function_fixnum(left, LISPTYPE_BIGNUM, right), "eql_function_fixnum7");

	bignum_value_alloc(local, &numer, signplus_bignum, 10);
	bignum_value_alloc(local, &denom, signplus_bignum, 1);
	make_ratio_reduction_local(local, &right, signplus_bignum, numer, denom);
	test(eql_function_fixnum(left, LISPTYPE_RATIO, right), "eql_function_fixnum8");

	bignum_value_alloc(local, &numer, signplus_bignum, 20);
	bignum_value_alloc(local, &denom, signplus_bignum, 1);
	make_ratio_reduction_local(local, &right, signplus_bignum, numer, denom);
	test(! eql_function_fixnum(left, LISPTYPE_RATIO, right), "eql_function_fixnum9");

	rollback_local(local, stack);

	RETURN;
}

static int test_eql_function_bignum(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, denom, numer;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_alloc(local, &left, signminus_bignum, 10);
	test(! eql_function_bignum(left, LISPTYPE_NIL, Nil), "eql_function_bignum1");

	fixnum_local(local, &right, 11);
	test(! eql_function_bignum(left, LISPTYPE_FIXNUM, right), "eql_function_bignum1");
	fixnum_local(local, &right, -10);
	test(eql_function_bignum(left, LISPTYPE_FIXNUM, right), "eql_function_bignum2");

	bignum_value_alloc(local, &right, signminus_bignum, 20);
	test(! eql_function_bignum(left, LISPTYPE_BIGNUM, right), "eql_function_bignum3");
	bignum_value_alloc(local, &right, signminus_bignum, 10);
	test(eql_function_bignum(left, LISPTYPE_BIGNUM, right), "eql_function_bignum4");

	bignum_value_alloc(local, &numer, signplus_bignum, 20);
	bignum_value_alloc(local, &denom, signplus_bignum, 1);
	make_ratio_reduction_local(local, &right, signminus_bignum, numer, denom);
	test(! eql_function_bignum(left, LISPTYPE_RATIO, right), "eql_function_bignum5");
	bignum_value_alloc(local, &numer, signplus_bignum, 10);
	bignum_value_alloc(local, &denom, signplus_bignum, 1);
	make_ratio_reduction_local(local, &right, signminus_bignum, numer, denom);
	test(eql_function_bignum(left, LISPTYPE_RATIO, right), "eql_function_bignum6");

	rollback_local(local, stack);

	RETURN;
}

static int test_eql_function_ratio(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, denom, numer;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_alloc(local, &numer, signplus_bignum, 20);
	bignum_value_alloc(local, &denom, signplus_bignum, 1);
	make_ratio_reduction_local(local, &left, signplus_bignum, numer, denom);
	test(! eql_function_ratio(left, LISPTYPE_NIL, Nil), "eql_function_ratio1");

	fixnum_local(local, &right, 11);
	test(! eql_function_ratio(left, LISPTYPE_FIXNUM, right), "eql_function_ratio1");
	fixnum_local(local, &right, 20);
	test(eql_function_ratio(left, LISPTYPE_FIXNUM, right), "eql_function_ratio2");

	bignum_value_alloc(local, &right, signminus_bignum, 20);
	test(! eql_function_ratio(left, LISPTYPE_BIGNUM, right), "eql_function_ratio3");
	bignum_value_alloc(local, &right, signplus_bignum, 20);
	test(eql_function_ratio(left, LISPTYPE_BIGNUM, right), "eql_function_ratio4");

	bignum_value_alloc(local, &numer, signplus_bignum, 10);
	bignum_value_alloc(local, &denom, signplus_bignum, 1);
	make_ratio_reduction_local(local, &right, signminus_bignum, numer, denom);
	test(! eql_function_ratio(left, LISPTYPE_RATIO, right), "eql_function_ratio5");
	bignum_value_alloc(local, &numer, signplus_bignum, 20);
	bignum_value_alloc(local, &denom, signplus_bignum, 1);
	make_ratio_reduction_local(local, &right, signplus_bignum, numer, denom);
	test(eql_function_ratio(left, LISPTYPE_RATIO, right), "eql_function_ratio6");

	rollback_local(local, stack);

	RETURN;
}

static int test_eql_function(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, denom, numer;

	local = Local_Thread;
	push_local(local, &stack);

	/* eq */
	test(eql_function(T, T), "eql_function1");
	fixnum_local(local, &left, 10);
	test(eql_function(left, left), "eql_function2");

	/* others */
	test(! eql_function(T, left), "eql_function3");

	/* fixnum */
	test(! eql_function(left, T), "eql_function4");
	fixnum_local(local, &right, 10);
	test(eql_function(left, right), "eql_function5");

	/* bignum */
	bignum_value_alloc(local, &left, signminus_bignum, 20);
	test(! eql_function(left, Nil), "eql_function6");
	fixnum_local(local, &right, -20);
	test(eql_function(left, right), "eql_function7");

	/* ratio */
	bignum_value_alloc(local, &numer, signplus_bignum, 20);
	bignum_value_alloc(local, &denom, signplus_bignum, 1);
	make_ratio_reduction_local(local, &left, signplus_bignum, numer, denom);
	fixnum_local(local, &right, 20);
	test(eql_function(left, right), "eql_function8");

	/* type check */
	test(! eql_function(T, Nil), "eql_function9");
	single_float_local(local, &left, 1.0f);
	double_float_local(local, &right, 1.0);
	test(! eql_function(left, right), "eql_function10");

	/* float */
	single_float_local(local, &left, +0.0f);
	single_float_local(local, &right, -0.0f);
	test(eql_function(left, right), "eql_function11");
	single_float_local(local, &left, +1.0f);
	single_float_local(local, &right, +1.0f);
	test(eql_function(left, right), "eql_function12");
	single_float_local(local, &right, -1.0f);
	test(! eql_function(left, right), "eql_function13");

	double_float_local(local, &left, +2.2);
	double_float_local(local, &right, +2.2);
	test(eql_function(left, right), "eql_function14");
	double_float_local(local, &right, -2.2);
	test(! eql_function(left, right), "eql_function15");

	long_float_local(local, &left, +5.2L);
	long_float_local(local, &right, +5.2L);
	test(eql_function(left, right), "eql_function16");
	long_float_local(local, &right, -5.2L);
	test(! eql_function(left, right), "eql_function17");

	complex_local(local, &left, fixnum_localr(local, 10), fixnum_localr(local, -20));
	complex_local(local, &right, fixnum_localr(local, 10), fixnum_localr(local, -20));
	test(eql_function(left, right), "eql_function18");
	complex_local(local, &right, fixnum_localr(local, 10), fixnum_localr(local, 20));
	test(! eql_function(left, right), "eql_function19");

	character_local(local, &left, 'A');
	character_local(local, &right, 'A');
	test(eql_function(left, right), "eql_function20");
	character_local(local, &right, 'a');
	test(! eql_function(left, right), "eql_function21");

	rollback_local(local, stack);

	RETURN;
}

static int test_equal_function(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right;

	local = Local_Thread;
	push_local(local, &stack);

	/* cons */
	consnil_heap(&left);
	consnil_heap(&right);
	test(equal_function(left, right), "equal_function1");
	list_local(local, &left, fixnum_localr(local, 10), T, NULL);
	list_local(local, &right, fixnum_localr(local, 10), T, NULL);
	test(equal_function(left, right), "equal_function2");
	list_local(local, &right, fixnum_localr(local, 10), Nil, NULL);
	test(! equal_function(left, right), "equal_function3");
	list_local(local, &right, fixnum_localr(local, 20), T, NULL);
	test(! equal_function(left, right), "equal_function4");

	strvect_char_local(local, &left, "Hello");
	strvect_char_local(local, &right, "Hello");
	test(equal_function(left, right), "equal_function5");
	strvect_char_local(local, &right, "HELLO");
	test(! equal_function(left, right), "equal_function6");
	test(! equal_function(left, T), "equal_function7");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  Main
 */
static int testbreak_equal(void)
{
	TestBreak(test_atom_function);
	TestBreak(test_eq_function);
	TestBreak(test_eql_function_fixnum);
	TestBreak(test_eql_function_bignum);
	TestBreak(test_eql_function_ratio);
	TestBreak(test_eql_function);
	TestBreak(test_equal_function);

	return 0;
}

int test_equal(void)
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
		lisp_initialize = 1;
		result = testbreak_equal();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

