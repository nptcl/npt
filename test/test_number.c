#include "number.c"
#include "constant.h"
#include "degrade.h"
#include "integer.h"

static int test_fixnump(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_local(local, &pos, 10);
	test(fixnump(pos), "fixnump1");
	fixnum_heap(&pos, -20);
	test(fixnump(pos), "fixnump2");
	bignum_value_alloc(local, &pos, signplus_bignum, 10);
	test(! fixnump(pos), "fixnump3");

	rollback_local(local, stack);

	RETURN;
}

static int test_bignump(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_alloc(local, &pos, signplus_bignum, 10);
	test(bignump(pos), "bignump1");
	bignum_value_alloc(NULL, &pos, signminus_bignum, 20);
	test(bignump(pos), "bignump2");
	fixnum_local(local, &pos, 10);
	test(! bignump(pos), "bignump3");

	rollback_local(local, stack);

	RETURN;
}

static int test_integerp(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_local(local, &pos, -10);
	test(integerp(pos), "integerp1");
	bignum_value_alloc(local, &pos, signplus_bignum, 20);
	test(integerp(pos), "integerp2");
	single_float_local(local, &pos, 1.2f);
	test(! integerp(pos), "integerp3");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  Main
 */
static int testcase_number(void)
{
	TestBreak(test_fixnump);
	TestBreak(test_bignump);
	TestBreak(test_integerp);

	return 0;
}

static void testinit_number(Execute ptr)
{
	build_lisproot(ptr);
	build_constant();
	build_object();
}

int test_number(void)
{
	DegradeTitle;
	return DegradeCode(number);
}

