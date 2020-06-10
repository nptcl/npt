#include "ratio_multi.c"
#include "degrade.h"
#include "ratio_equal.h"

static void test_ratio_alloc(LocalRoot local,
		addr *ret, int sign, bigtype v1, bigtype v2)
{
	addr numer, denom;
	bignum_value_alloc(local, &numer, SignPlus, v1);
	bignum_value_alloc(local, &denom, SignPlus, v2);
	make_ratio_alloc(local, ret, sign, numer, denom);
}

static int test_equal_rv_nosign(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &pos, SignPlus, 7, 1);
	test(equal_rv_nosign(pos, 7), "equal_rv_nosign1");
	test_ratio_alloc(local, &pos, SignPlus, 7, 2);
	test(! equal_rv_nosign(pos, 7), "equal_rv_nosign2");

	test_ratio_alloc(local, &pos, SignMinus, 7, 1);
	test(equal_rv_nosign(pos, 7), "equal_rv_nosign3");
	test_ratio_alloc(local, &pos, SignMinus, 7, 2);
	test(! equal_rv_nosign(pos, 7), "equal_rv_nosign4");

	rollback_local(local, stack);

	RETURN;
}

static int test_multi_rv_ratio(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &pos, SignPlus, 7, 6);
	multi_rv_ratio(local, SignMinus, pos, 5, &pos);
	test(GetStatusDynamic(pos), "multi_rv_ratio1");
	test(ratiop(pos), "multi_rv_ratio2");
	test(equal_value_ratio(pos, SignMinus, 35, 6), "multi_rv_ratio3");

	test_ratio_alloc(local, &pos, SignPlus, 7, 6);
	multi_rv_ratio(local, SignPlus, pos, 4, &pos);
	test(GetStatusDynamic(pos), "multi_rv_ratio4");
	test(ratiop(pos), "multi_rv_ratio5");
	test(equal_value_ratio(pos, SignPlus, 14, 3), "multi_rv_ratio6");

	test_ratio_alloc(local, &pos, SignPlus, 2, 7);
	multi_rv_ratio(local, SignMinus, pos, 14, &pos);
	test(GetStatusDynamic(pos), "multi_rv_ratio7");
	test(ratiop(pos), "multi_rv_ratio8");
	test(equal_value_ratio(pos, SignMinus, 4, 1), "multi_rv_ratio9");

	rollback_local(local, stack);

	RETURN;
}

static int test_multi_rv_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &pos, SignPlus, 7, 6);
	multi_rv_local(local, SignMinus, pos, 5, &pos);
	test(GetStatusDynamic(pos), "multi_rv_local1");
	test(ratiop(pos), "multi_rv_local2");
	test(equal_value_ratio(pos, SignMinus, 35, 6), "multi_rv_local3");

	test_ratio_alloc(local, &pos, SignPlus, 7, 6);
	multi_rv_local(local, SignPlus, pos, 4, &pos);
	test(GetStatusDynamic(pos), "multi_rv_local4");
	test(ratiop(pos), "multi_rv_local5");
	test(equal_value_ratio(pos, SignPlus, 14, 3), "multi_rv_local6");

	test_ratio_alloc(local, &pos, SignPlus, 2, 7);
	multi_rv_local(local, SignMinus, pos, 14, &pos);
	test(GetStatusDynamic(pos), "multi_rv_local7");
	test(fixnump(pos), "multi_rv_local8");
	test(RefFixnum(pos) == -4, "multi_rv_local9");

	rollback_local(local, stack);

	RETURN;
}

static int test_multi_rv_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &pos, SignPlus, 7, 6);
	multi_rv_common(local, SignMinus, pos, 5, &pos);
	test(! GetStatusDynamic(pos), "multi_rv_common1");
	test(ratiop(pos), "multi_rv_common2");
	test(equal_value_ratio(pos, SignMinus, 35, 6), "multi_rv_common3");

	test_ratio_alloc(local, &pos, SignPlus, 7, 6);
	multi_rv_common(local, SignPlus, pos, 4, &pos);
	test(! GetStatusDynamic(pos), "multi_rv_common4");
	test(ratiop(pos), "multi_rv_common5");
	test(equal_value_ratio(pos, SignPlus, 14, 3), "multi_rv_common6");

	test_ratio_alloc(local, &pos, SignPlus, 2, 7);
	multi_rv_common(local, SignMinus, pos, 14, &pos);
	test(! GetStatusDynamic(pos), "multi_rv_common7");
	test(fixnump(pos), "multi_rv_common8");
	test(RefFixnum(pos) == -4, "multi_rv_common9");

	rollback_local(local, stack);

	RETURN;
}

static int test_multi_rf_ratio_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 0, 3);
	fixnum_local(local, &right, 10);
	multi_rf_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rf_ratio_local1");
	test(ratiop(pos), "multi_rf_ratio_local2");
	test(zerop_ratio(pos), "multi_rf_ratio_local3");

	test_ratio_alloc(local, &left, SignPlus, 1, 1);
	fixnum_local(local, &right, 10);
	multi_rf_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rf_ratio_local4");
	test(ratiop(pos), "multi_rf_ratio_local5");
	test(equal_value_ratio(pos, SignPlus, 10, 1), "multi_rf_ratio_local6");

	test_ratio_alloc(local, &left, SignMinus, 1, 1);
	fixnum_local(local, &right, 10);
	multi_rf_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rf_ratio_local7");
	test(ratiop(pos), "multi_rf_ratio_local8");
	test(equal_value_ratio(pos, SignMinus, 10, 1), "multi_rf_ratio_local9");

	test_ratio_alloc(local, &left, SignMinus, 3, 4);
	fixnum_local(local, &right, 0);
	multi_rf_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rf_ratio_local10");
	test(ratiop(pos), "multi_rf_ratio_local11");
	test(zerop_ratio(pos), "multi_rf_ratio_local12");

	test_ratio_alloc(local, &left, SignMinus, 3, 4);
	fixnum_local(local, &right, 1);
	multi_rf_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rf_ratio_local10");
	test(ratiop(pos), "multi_rf_ratio_local11");
	test(equal_value_ratio(pos, SignMinus, 3, 4), "multi_rf_ratio_local12");

	test_ratio_alloc(local, &left, SignMinus, 3, 4);
	fixnum_local(local, &right, -1);
	multi_rf_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rf_ratio_local13");
	test(ratiop(pos), "multi_rf_ratio_local14");
	test(equal_value_ratio(pos, SignPlus, 3, 4), "multi_rf_ratio_local15");

	test_ratio_alloc(local, &left, SignPlus, 3, 4);
	fixnum_local(local, &right, -5);
	multi_rf_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rf_ratio_local16");
	test(ratiop(pos), "multi_rf_ratio_local17");
	test(equal_value_ratio(pos, SignMinus, 15, 4),
			"multi_rf_ratio_local18");

	rollback_local(local, stack);

	RETURN;
}

static int test_multi_rf_real_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 0, 3);
	fixnum_local(local, &right, 10);
	multi_rf_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rf_real_local1");
	test(fixnump(pos), "multi_rf_real_local2");
	test(RefFixnum(pos) == 0, "multi_rf_real_local3");

	test_ratio_alloc(local, &left, SignPlus, 1, 1);
	fixnum_local(local, &right, 10);
	multi_rf_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rf_real_local4");
	test(fixnump(pos), "multi_rf_real_local5");
	test(RefFixnum(pos) == 10, "multi_rf_real_local6");

	test_ratio_alloc(local, &left, SignMinus, 1, 1);
	fixnum_local(local, &right, 10);
	multi_rf_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rf_real_local7");
	test(fixnump(pos), "multi_rf_real_local8");
	test(RefFixnum(pos) == -10, "multi_rf_real_local9");

	test_ratio_alloc(local, &left, SignMinus, 3, 4);
	fixnum_local(local, &right, 0);
	multi_rf_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rf_real_local10");
	test(fixnump(pos), "multi_rf_real_local11");
	test(RefFixnum(pos) == 0, "multi_rf_real_local12");

	test_ratio_alloc(local, &left, SignMinus, 3, 4);
	fixnum_local(local, &right, 1);
	multi_rf_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rf_real_local10");
	test(ratiop(pos), "multi_rf_real_local11");
	test(equal_value_ratio(pos, SignMinus, 3, 4), "multi_rf_real_local12");

	test_ratio_alloc(local, &left, SignMinus, 3, 4);
	fixnum_local(local, &right, -1);
	multi_rf_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rf_real_local13");
	test(ratiop(pos), "multi_rf_real_local14");
	test(equal_value_ratio(pos, SignPlus, 3, 4), "multi_rf_real_local15");

	test_ratio_alloc(local, &left, SignPlus, 3, 4);
	fixnum_local(local, &right, -5);
	multi_rf_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rf_real_local16");
	test(ratiop(pos), "multi_rf_real_local17");
	test(equal_value_ratio(pos, SignMinus, 15, 4),
			"multi_rf_real_local18");

	rollback_local(local, stack);

	RETURN;
}

static int test_multi_rf_real_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 0, 3);
	fixnum_local(local, &right, 10);
	multi_rf_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "multi_rf_real_common1");
	test(fixnump(pos), "multi_rf_real_common2");
	test(RefFixnum(pos) == 0, "multi_rf_real_common3");

	test_ratio_alloc(local, &left, SignPlus, 1, 1);
	fixnum_local(local, &right, 10);
	multi_rf_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "multi_rf_real_common4");
	test(fixnump(pos), "multi_rf_real_common5");
	test(RefFixnum(pos) == 10, "multi_rf_real_common6");

	test_ratio_alloc(local, &left, SignMinus, 1, 1);
	fixnum_local(local, &right, 10);
	multi_rf_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "multi_rf_real_common7");
	test(fixnump(pos), "multi_rf_real_common8");
	test(RefFixnum(pos) == -10, "multi_rf_real_common9");

	test_ratio_alloc(local, &left, SignMinus, 3, 4);
	fixnum_local(local, &right, 0);
	multi_rf_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "multi_rf_real_common10");
	test(fixnump(pos), "multi_rf_real_common11");
	test(RefFixnum(pos) == 0, "multi_rf_real_common12");

	test_ratio_alloc(local, &left, SignMinus, 3, 4);
	fixnum_local(local, &right, 1);
	multi_rf_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "multi_rf_real_common10");
	test(ratiop(pos), "multi_rf_real_common11");
	test(equal_value_ratio(pos, SignMinus, 3, 4), "multi_rf_real_common12");

	test_ratio_alloc(local, &left, SignMinus, 3, 4);
	fixnum_local(local, &right, -1);
	multi_rf_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "multi_rf_real_common13");
	test(ratiop(pos), "multi_rf_real_common14");
	test(equal_value_ratio(pos, SignPlus, 3, 4), "multi_rf_real_common15");

	test_ratio_alloc(local, &left, SignPlus, 3, 4);
	fixnum_local(local, &right, -5);
	multi_rf_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "multi_rf_real_common16");
	test(ratiop(pos), "multi_rf_real_common17");
	test(equal_value_ratio(pos, SignMinus, 15, 4),
			"multi_rf_real_common18");

	rollback_local(local, stack);

	RETURN;
}

static int test_multi_rb_ratio(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 7, 6);
	bignum_value_alloc(local, &right, SignPlus, 5);
	multi_rb_ratio(local, SignMinus, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rb_ratio1");
	test(ratiop(pos), "multi_rb_ratio2");
	test(equal_value_ratio(pos, SignMinus, 35, 6), "multi_rb_ratio3");

	test_ratio_alloc(local, &left, SignPlus, 7, 6);
	bignum_value_alloc(local, &right, SignMinus, 4);
	multi_rb_ratio(local, SignPlus, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rb_ratio4");
	test(ratiop(pos), "multi_rb_ratio5");
	test(equal_value_ratio(pos, SignPlus, 14, 3), "multi_rb_ratio6");

	test_ratio_alloc(local, &left, SignPlus, 2, 7);
	bignum_value_alloc(local, &right, SignMinus, 14);
	multi_rb_ratio(local, SignMinus, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rb_ratio7");
	test(ratiop(pos), "multi_rb_ratio8");
	test(equal_value_ratio(pos, SignMinus, 4, 1), "multi_rb_ratio9");

	rollback_local(local, stack);

	RETURN;
}

static int test_multi_rb_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 7, 6);
	bignum_value_alloc(local, &right, SignPlus, 5);
	multi_rb_local(local, SignMinus, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rb_local1");
	test(ratiop(pos), "multi_rb_local2");
	test(equal_value_ratio(pos, SignMinus, 35, 6), "multi_rb_local3");

	test_ratio_alloc(local, &left, SignPlus, 7, 6);
	bignum_value_alloc(local, &right, SignMinus, 4);
	multi_rb_local(local, SignPlus, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rb_local4");
	test(ratiop(pos), "multi_rb_local5");
	test(equal_value_ratio(pos, SignPlus, 14, 3), "multi_rb_local6");

	test_ratio_alloc(local, &left, SignPlus, 2, 7);
	bignum_value_alloc(local, &right, SignMinus, 14);
	multi_rb_local(local, SignMinus, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rb_local7");
	test(fixnump(pos), "multi_rb_local8");
	test(RefFixnum(pos) == -4, "multi_rb_local9");

	rollback_local(local, stack);

	RETURN;
}

static int test_multi_rb_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 7, 6);
	bignum_value_alloc(local, &right, SignPlus, 5);
	multi_rb_common(local, SignMinus, left, right, &pos);
	test(! GetStatusDynamic(pos), "multi_rb_common1");
	test(ratiop(pos), "multi_rb_common2");
	test(equal_value_ratio(pos, SignMinus, 35, 6), "multi_rb_common3");

	test_ratio_alloc(local, &left, SignPlus, 7, 6);
	bignum_value_alloc(local, &right, SignMinus, 4);
	multi_rb_common(local, SignPlus, left, right, &pos);
	test(! GetStatusDynamic(pos), "multi_rb_common4");
	test(ratiop(pos), "multi_rb_common5");
	test(equal_value_ratio(pos, SignPlus, 14, 3), "multi_rb_common6");

	test_ratio_alloc(local, &left, SignPlus, 2, 7);
	bignum_value_alloc(local, &right, SignMinus, 14);
	multi_rb_common(local, SignMinus, left, right, &pos);
	test(! GetStatusDynamic(pos), "multi_rb_common7");
	test(fixnump(pos), "multi_rb_common8");
	test(RefFixnum(pos) == -4, "multi_rb_common9");

	rollback_local(local, stack);

	RETURN;
}

static int test_multi_rb_ratio_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 0, 3);
	bignum_value_alloc(local, &right, SignPlus, 10);
	multi_rb_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rb_ratio_local1");
	test(ratiop(pos), "multi_rb_ratio_local2");
	test(zerop_ratio(pos), "multi_rb_ratio_local3");

	test_ratio_alloc(local, &left, SignPlus, 1, 1);
	bignum_value_alloc(local, &right, SignPlus, 10);
	multi_rb_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rb_ratio_local4");
	test(ratiop(pos), "multi_rb_ratio_local5");
	test(equal_value_ratio(pos, SignPlus, 10, 1), "multi_rb_ratio_local6");

	test_ratio_alloc(local, &left, SignMinus, 1, 1);
	bignum_value_alloc(local, &right, SignPlus, 10);
	multi_rb_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rb_ratio_local7");
	test(ratiop(pos), "multi_rb_ratio_local8");
	test(equal_value_ratio(pos, SignMinus, 10, 1), "multi_rb_ratio_local9");

	test_ratio_alloc(local, &left, SignMinus, 3, 4);
	bignum_value_alloc(local, &right, SignPlus, 0);
	multi_rb_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rb_ratio_local10");
	test(ratiop(pos), "multi_rb_ratio_local11");
	test(zerop_ratio(pos), "multi_rb_ratio_local12");

	test_ratio_alloc(local, &left, SignMinus, 3, 4);
	bignum_value_alloc(local, &right, SignPlus, 1);
	multi_rb_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rb_ratio_local13");
	test(ratiop(pos), "multi_rb_ratio_local14");
	test(equal_value_ratio(pos, SignMinus, 3, 4), "multi_rb_ratio_local15");

	test_ratio_alloc(local, &left, SignMinus, 3, 4);
	bignum_value_alloc(local, &right, SignMinus, 1);
	multi_rb_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rb_ratio_local16");
	test(ratiop(pos), "multi_rb_ratio_local17");
	test(equal_value_ratio(pos, SignPlus, 3, 4), "multi_rb_ratio_local18");

	test_ratio_alloc(local, &left, SignPlus, 3, 4);
	bignum_value_alloc(local, &right, SignMinus, 5);
	multi_rb_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rb_ratio_local19");
	test(ratiop(pos), "multi_rb_ratio_local20");
	test(equal_value_ratio(pos, SignMinus, 15, 4),
			"multi_rb_ratio_local21");

	rollback_local(local, stack);

	RETURN;
}

static int test_multi_rb_real_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 0, 3);
	bignum_value_alloc(local, &right, SignPlus, 10);
	multi_rb_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rb_real_local1");
	test(fixnump(pos), "multi_rb_real_local2");
	test(RefFixnum(pos) == 0, "multi_rb_real_local3");

	test_ratio_alloc(local, &left, SignPlus, 1, 1);
	bignum_value_alloc(local, &right, SignPlus, 10);
	multi_rb_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rb_real_local4");
	test(bignump(pos), "multi_rb_real_local5");
	test(equal_value_bignum(pos, SignPlus, 10), "multi_rb_real_local6");

	test_ratio_alloc(local, &left, SignMinus, 1, 1);
	bignum_value_alloc(local, &right, SignPlus, 10);
	multi_rb_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rb_real_local7");
	test(fixnump(pos), "multi_rb_real_local8");
	test(RefFixnum(pos) == -10, "multi_rb_real_local9");

	test_ratio_alloc(local, &left, SignMinus, 3, 4);
	bignum_value_alloc(local, &right, SignPlus, 0);
	multi_rb_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rb_real_local10");
	test(fixnump(pos), "multi_rb_real_local11");
	test(RefFixnum(pos) == 0, "multi_rb_real_local12");

	test_ratio_alloc(local, &left, SignMinus, 3, 4);
	bignum_value_alloc(local, &right, SignPlus, 1);
	multi_rb_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rb_real_local13");
	test(ratiop(pos), "multi_rb_real_local14");
	test(equal_value_ratio(pos, SignMinus, 3, 4), "multi_rb_real_local15");

	test_ratio_alloc(local, &left, SignMinus, 3, 4);
	bignum_value_alloc(local, &right, SignMinus, 1);
	multi_rb_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rb_real_local16");
	test(ratiop(pos), "multi_rb_real_local17");
	test(equal_value_ratio(pos, SignPlus, 3, 4), "multi_rb_real_local18");

	test_ratio_alloc(local, &left, SignPlus, 3, 4);
	bignum_value_alloc(local, &right, SignMinus, 5);
	multi_rb_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rb_real_local19");
	test(ratiop(pos), "multi_rb_real_local20");
	test(equal_value_ratio(pos, SignMinus, 15, 4),
			"multi_rb_real_local21");

	rollback_local(local, stack);

	RETURN;
}

static int test_multi_rb_real_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 0, 3);
	bignum_value_alloc(local, &right, SignPlus, 10);
	multi_rb_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "multi_rb_real_common1");
	test(fixnump(pos), "multi_rb_real_common2");
	test(RefFixnum(pos) == 0, "multi_rb_real_common3");

	test_ratio_alloc(local, &left, SignPlus, 1, 1);
	bignum_value_alloc(local, &right, SignPlus, 10);
	multi_rb_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "multi_rb_real_common4");
	test(bignump(pos), "multi_rb_real_common5");
	test(equal_value_bignum(pos, SignPlus, 10), "multi_rb_real_common6");

	test_ratio_alloc(local, &left, SignMinus, 1, 1);
	bignum_value_alloc(local, &right, SignPlus, 10);
	multi_rb_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "multi_rb_real_common7");
	test(fixnump(pos), "multi_rb_real_common8");
	test(RefFixnum(pos) == -10, "multi_rb_real_common9");

	test_ratio_alloc(local, &left, SignMinus, 3, 4);
	bignum_value_alloc(local, &right, SignPlus, 0);
	multi_rb_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "multi_rb_real_common10");
	test(fixnump(pos), "multi_rb_real_common11");
	test(RefFixnum(pos) == 0, "multi_rb_real_common12");

	test_ratio_alloc(local, &left, SignMinus, 3, 4);
	bignum_value_alloc(local, &right, SignPlus, 1);
	multi_rb_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "multi_rb_real_common13");
	test(ratiop(pos), "multi_rb_real_common14");
	test(equal_value_ratio(pos, SignMinus, 3, 4), "multi_rb_real_common15");

	test_ratio_alloc(local, &left, SignMinus, 3, 4);
	bignum_value_alloc(local, &right, SignMinus, 1);
	multi_rb_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "multi_rb_real_common16");
	test(ratiop(pos), "multi_rb_real_common17");
	test(equal_value_ratio(pos, SignPlus, 3, 4), "multi_rb_real_common18");

	test_ratio_alloc(local, &left, SignPlus, 3, 4);
	bignum_value_alloc(local, &right, SignMinus, 5);
	multi_rb_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "multi_rb_real_common19");
	test(ratiop(pos), "multi_rb_real_common20");
	test(equal_value_ratio(pos, SignMinus, 15, 4),
			"multi_rb_real_common21");

	rollback_local(local, stack);

	RETURN;
}

static int test_div_rb_ratio(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 7, 6);
	bignum_value_alloc(local, &right, SignPlus, 5);
	div_rb_ratio(local, SignMinus, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rb_ratio1");
	test(ratiop(pos), "div_rb_ratio2");
	test(equal_value_ratio(pos, SignMinus, 7, 30), "div_rb_ratio3");

	test_ratio_alloc(local, &left, SignPlus, 6, 7);
	bignum_value_alloc(local, &right, SignMinus, 4);
	div_rb_ratio(local, SignPlus, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rb_ratio4");
	test(ratiop(pos), "div_rb_ratio5");
	test(equal_value_ratio(pos, SignPlus, 3, 14), "div_rb_ratio6");

	test_ratio_alloc(local, &left, SignPlus, 2, 7);
	bignum_value_alloc(local, &right, SignMinus, 4);
	div_rb_ratio(local, SignMinus, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rb_ratio7");
	test(ratiop(pos), "div_rb_ratio8");
	test(equal_value_ratio(pos, SignMinus, 1, 14), "div_rb_ratio9");

	rollback_local(local, stack);

	RETURN;
}

static int test_div_rb_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 7, 6);
	bignum_value_alloc(local, &right, SignPlus, 5);
	div_rb_local(local, SignMinus, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rb_local1");
	test(ratiop(pos), "div_rb_local2");
	test(equal_value_ratio(pos, SignMinus, 7, 30), "div_rb_local3");

	test_ratio_alloc(local, &left, SignPlus, 6, 7);
	bignum_value_alloc(local, &right, SignMinus, 4);
	div_rb_local(local, SignPlus, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rb_local4");
	test(ratiop(pos), "div_rb_local5");
	test(equal_value_ratio(pos, SignPlus, 3, 14), "div_rb_local6");

	test_ratio_alloc(local, &left, SignPlus, 2, 7);
	bignum_value_alloc(local, &right, SignMinus, 4);
	div_rb_local(local, SignMinus, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rb_local7");
	test(ratiop(pos), "div_rb_local8");
	test(equal_value_ratio(pos, SignMinus, 1, 14), "div_rb_local9");

	rollback_local(local, stack);

	RETURN;
}

static int test_div_rb_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 7, 6);
	bignum_value_alloc(local, &right, SignPlus, 5);
	div_rb_common(local, SignMinus, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_rb_common1");
	test(ratiop(pos), "div_rb_common2");
	test(equal_value_ratio(pos, SignMinus, 7, 30), "div_rb_common3");

	test_ratio_alloc(local, &left, SignPlus, 6, 7);
	bignum_value_alloc(local, &right, SignMinus, 4);
	div_rb_common(local, SignPlus, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_rb_common4");
	test(ratiop(pos), "div_rb_common5");
	test(equal_value_ratio(pos, SignPlus, 3, 14), "div_rb_common6");

	test_ratio_alloc(local, &left, SignPlus, 2, 7);
	bignum_value_alloc(local, &right, SignMinus, 4);
	div_rb_common(local, SignMinus, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_rb_common7");
	test(ratiop(pos), "div_rb_common8");
	test(equal_value_ratio(pos, SignMinus, 1, 14), "div_rb_common9");

	rollback_local(local, stack);

	RETURN;
}

static int test_multi_rr_ratio(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 5, 6);
	test_ratio_alloc(local, &right, SignPlus, 7, 8);
	multi_rr_ratio(local, SignMinus, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rr_ratio1");
	test(ratiop(pos), "multi_rr_ratio2");
	test(equal_value_ratio(pos, SignMinus, 35, 48), "multi_rr_ratio3");

	test_ratio_alloc(local, &left, SignPlus, 55, 6);
	test_ratio_alloc(local, &right, SignPlus, 4, 35);
	multi_rr_ratio(local, SignPlus, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rr_ratio4");
	test(ratiop(pos), "multi_rr_ratio5");
	test(equal_value_ratio(pos, SignPlus, 22, 21), "multi_rr_ratio6");

	test_ratio_alloc(local, &left, SignPlus, 12, 6);
	test_ratio_alloc(local, &right, SignPlus, 6, 4);
	multi_rr_ratio(local, SignPlus, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rr_ratio7");
	test(ratiop(pos), "multi_rr_ratio8");
	test(equal_value_ratio(pos, SignPlus, 3, 1), "multi_rr_ratio9");

	rollback_local(local, stack);

	RETURN;
}

static int test_multi_rr_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 5, 6);
	test_ratio_alloc(local, &right, SignPlus, 7, 8);
	multi_rr_local(local, SignMinus, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rr_local1");
	test(ratiop(pos), "multi_rr_local2");
	test(equal_value_ratio(pos, SignMinus, 35, 48), "multi_rr_local3");

	test_ratio_alloc(local, &left, SignPlus, 55, 6);
	test_ratio_alloc(local, &right, SignPlus, 4, 35);
	multi_rr_local(local, SignPlus, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rr_local4");
	test(ratiop(pos), "multi_rr_local5");
	test(equal_value_ratio(pos, SignPlus, 22, 21), "multi_rr_local6");

	test_ratio_alloc(local, &left, SignPlus, 12, 6);
	test_ratio_alloc(local, &right, SignPlus, 6, 4);
	multi_rr_local(local, SignPlus, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rr_local7");
	test(fixnump(pos), "multi_rr_local8");
	test(RefFixnum(pos) == 3, "multi_rr_local9");

	rollback_local(local, stack);

	RETURN;
}

static int test_multi_rr_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 5, 6);
	test_ratio_alloc(local, &right, SignPlus, 7, 8);
	multi_rr_common(local, SignMinus, left, right, &pos);
	test(! GetStatusDynamic(pos), "multi_rr_common1");
	test(ratiop(pos), "multi_rr_common2");
	test(equal_value_ratio(pos, SignMinus, 35, 48), "multi_rr_common3");

	test_ratio_alloc(local, &left, SignPlus, 55, 6);
	test_ratio_alloc(local, &right, SignPlus, 4, 35);
	multi_rr_common(local, SignPlus, left, right, &pos);
	test(! GetStatusDynamic(pos), "multi_rr_common4");
	test(ratiop(pos), "multi_rr_common5");
	test(equal_value_ratio(pos, SignPlus, 22, 21), "multi_rr_common6");

	test_ratio_alloc(local, &left, SignPlus, 12, 6);
	test_ratio_alloc(local, &right, SignPlus, 6, 4);
	multi_rr_common(local, SignPlus, left, right, &pos);
	test(! GetStatusDynamic(pos), "multi_rr_common7");
	test(fixnump(pos), "multi_rr_common8");
	test(RefFixnum(pos) == 3, "multi_rr_common9");

	rollback_local(local, stack);

	RETURN;
}

static int test_inverse_ratio_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 1, 6);
	test(inverse_ratio_p(left), "inverse_ratio_p1");

	test_ratio_alloc(local, &left, SignPlus, 5, 6);
	test(! inverse_ratio_p(left), "inverse_ratio_p2");

	rollback_local(local, stack);

	RETURN;
}

static int test_multi_rr_ratio_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 0, 6);
	test_ratio_alloc(local, &right, SignPlus, 3, 4);
	multi_rr_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rr_ratio_local1");
	test(ratiop(pos), "multi_rr_ratio_local2");
	test(zerop_ratio(pos), "multi_rr_ratio_local3");

	test_ratio_alloc(local, &left, SignPlus, 1, 1);
	test_ratio_alloc(local, &right, SignPlus, 3, 4);
	multi_rr_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rr_ratio_local4");
	test(ratiop(pos), "multi_rr_ratio_local5");
	test(equal_value_ratio(pos, SignPlus, 3, 4), "multi_rr_ratio_local6");

	test_ratio_alloc(local, &left, SignMinus, 1, 1);
	test_ratio_alloc(local, &right, SignPlus, 3, 4);
	multi_rr_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rr_ratio_local7");
	test(ratiop(pos), "multi_rr_ratio_local8");
	test(equal_value_ratio(pos, SignMinus, 3, 4), "multi_rr_ratio_local9");

	test_ratio_alloc(local, &left, SignMinus, 1, 6);
	test_ratio_alloc(local, &right, SignPlus, 3, 4);
	multi_rr_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rr_ratio_local10");
	test(ratiop(pos), "multi_rr_ratio_local11");
	test(equal_value_ratio(pos, SignMinus, 1, 8), "multi_rr_ratio_local12");

	test_ratio_alloc(local, &left, SignMinus, 3, 4);
	test_ratio_alloc(local, &right, SignPlus, 0, 6);
	multi_rr_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rr_ratio_local13");
	test(ratiop(pos), "multi_rr_ratio_local14");
	test(zerop_ratio(pos), "multi_rr_ratio_local15");

	test_ratio_alloc(local, &left, SignMinus, 3, 4);
	test_ratio_alloc(local, &right, SignPlus, 1, 1);
	multi_rr_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rr_ratio_local16");
	test(ratiop(pos), "multi_rr_ratio_local17");
	test(equal_value_ratio(pos, SignMinus, 3, 4), "multi_rr_ratio_local18");

	test_ratio_alloc(local, &left, SignMinus, 3, 4);
	test_ratio_alloc(local, &right, SignMinus, 1, 1);
	multi_rr_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rr_ratio_local19");
	test(ratiop(pos), "multi_rr_ratio_local20");
	test(equal_value_ratio(pos, SignPlus, 3, 4), "multi_rr_ratio_local21");

	test_ratio_alloc(local, &left, SignMinus, 3, 4);
	test_ratio_alloc(local, &right, SignMinus, 1, 6);
	multi_rr_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rr_ratio_local22");
	test(ratiop(pos), "multi_rr_ratio_local23");
	test(equal_value_ratio(pos, SignPlus, 1, 8), "multi_rr_ratio_local24");

	test_ratio_alloc(local, &left, SignPlus, 55, 6);
	test_ratio_alloc(local, &right, SignMinus, 4, 35);
	multi_rr_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rr_ratio_local25");
	test(ratiop(pos), "multi_rr_ratio_local26");
	test(equal_value_ratio(pos, SignMinus, 22, 21), "multi_rr_ratio_local27");

	test_ratio_alloc(local, &left, SignPlus, 70, 4);
	test_ratio_alloc(local, &right, SignMinus, 4, 10);
	multi_rr_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rr_ratio_local28");
	test(ratiop(pos), "multi_rr_ratio_local29");
	test(equal_value_ratio(pos, SignMinus, 7, 1), "multi_rr_ratio_local30");

	rollback_local(local, stack);

	RETURN;
}

static int test_multi_rr_real_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 0, 6);
	test_ratio_alloc(local, &right, SignPlus, 3, 4);
	multi_rr_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rr_real_local1");
	test(fixnump(pos), "multi_rr_real_local2");
	test(RefFixnum(pos) == 0, "multi_rr_real_local3");

	test_ratio_alloc(local, &left, SignPlus, 1, 1);
	test_ratio_alloc(local, &right, SignPlus, 3, 4);
	multi_rr_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rr_real_local4");
	test(ratiop(pos), "multi_rr_real_local5");
	test(equal_value_ratio(pos, SignPlus, 3, 4), "multi_rr_real_local6");

	test_ratio_alloc(local, &left, SignMinus, 1, 1);
	test_ratio_alloc(local, &right, SignPlus, 3, 4);
	multi_rr_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rr_real_local7");
	test(ratiop(pos), "multi_rr_real_local8");
	test(equal_value_ratio(pos, SignMinus, 3, 4), "multi_rr_real_local9");

	test_ratio_alloc(local, &left, SignMinus, 1, 6);
	test_ratio_alloc(local, &right, SignPlus, 3, 4);
	multi_rr_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rr_real_local10");
	test(ratiop(pos), "multi_rr_real_local11");
	test(equal_value_ratio(pos, SignMinus, 1, 8), "multi_rr_real_local12");

	test_ratio_alloc(local, &left, SignMinus, 3, 4);
	test_ratio_alloc(local, &right, SignPlus, 0, 6);
	multi_rr_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rr_real_local13");
	test(fixnump(pos), "multi_rr_real_local14");
	test(RefFixnum(pos) == 0, "multi_rr_real_local15");

	test_ratio_alloc(local, &left, SignMinus, 3, 4);
	test_ratio_alloc(local, &right, SignPlus, 1, 1);
	multi_rr_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rr_real_local16");
	test(ratiop(pos), "multi_rr_real_local17");
	test(equal_value_ratio(pos, SignMinus, 3, 4), "multi_rr_real_local18");

	test_ratio_alloc(local, &left, SignMinus, 3, 4);
	test_ratio_alloc(local, &right, SignMinus, 1, 1);
	multi_rr_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rr_real_local19");
	test(ratiop(pos), "multi_rr_real_local20");
	test(equal_value_ratio(pos, SignPlus, 3, 4), "multi_rr_real_local21");

	test_ratio_alloc(local, &left, SignMinus, 3, 4);
	test_ratio_alloc(local, &right, SignMinus, 1, 6);
	multi_rr_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rr_real_local22");
	test(ratiop(pos), "multi_rr_real_local23");
	test(equal_value_ratio(pos, SignPlus, 1, 8), "multi_rr_real_local24");

	test_ratio_alloc(local, &left, SignPlus, 55, 6);
	test_ratio_alloc(local, &right, SignMinus, 4, 35);
	multi_rr_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rr_real_local25");
	test(ratiop(pos), "multi_rr_real_local26");
	test(equal_value_ratio(pos, SignMinus, 22, 21), "multi_rr_real_local27");

	test_ratio_alloc(local, &left, SignPlus, 70, 4);
	test_ratio_alloc(local, &right, SignMinus, 4, 10);
	multi_rr_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "multi_rr_real_local28");
	test(fixnump(pos), "multi_rr_real_local29");
	test(RefFixnum(pos) == -7, "multi_rr_real_local30");

	rollback_local(local, stack);

	RETURN;
}

static int test_multi_rr_real_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 0, 6);
	test_ratio_alloc(local, &right, SignPlus, 3, 4);
	multi_rr_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "multi_rr_real_common1");
	test(fixnump(pos), "multi_rr_real_common2");
	test(RefFixnum(pos) == 0, "multi_rr_real_common3");

	test_ratio_alloc(local, &left, SignPlus, 1, 1);
	test_ratio_alloc(local, &right, SignPlus, 3, 4);
	multi_rr_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "multi_rr_real_common4");
	test(ratiop(pos), "multi_rr_real_common5");
	test(equal_value_ratio(pos, SignPlus, 3, 4), "multi_rr_real_common6");

	test_ratio_alloc(local, &left, SignMinus, 1, 1);
	test_ratio_alloc(local, &right, SignPlus, 3, 4);
	multi_rr_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "multi_rr_real_common7");
	test(ratiop(pos), "multi_rr_real_common8");
	test(equal_value_ratio(pos, SignMinus, 3, 4), "multi_rr_real_common9");

	test_ratio_alloc(local, &left, SignMinus, 1, 6);
	test_ratio_alloc(local, &right, SignPlus, 3, 4);
	multi_rr_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "multi_rr_real_common10");
	test(ratiop(pos), "multi_rr_real_common11");
	test(equal_value_ratio(pos, SignMinus, 1, 8), "multi_rr_real_common12");

	test_ratio_alloc(local, &left, SignMinus, 3, 4);
	test_ratio_alloc(local, &right, SignPlus, 0, 6);
	multi_rr_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "multi_rr_real_common13");
	test(fixnump(pos), "multi_rr_real_common14");
	test(RefFixnum(pos) == 0, "multi_rr_real_common15");

	test_ratio_alloc(local, &left, SignMinus, 3, 4);
	test_ratio_alloc(local, &right, SignPlus, 1, 1);
	multi_rr_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "multi_rr_real_common16");
	test(ratiop(pos), "multi_rr_real_common17");
	test(equal_value_ratio(pos, SignMinus, 3, 4), "multi_rr_real_common18");

	test_ratio_alloc(local, &left, SignMinus, 3, 4);
	test_ratio_alloc(local, &right, SignMinus, 1, 1);
	multi_rr_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "multi_rr_real_common19");
	test(ratiop(pos), "multi_rr_real_common20");
	test(equal_value_ratio(pos, SignPlus, 3, 4), "multi_rr_real_common21");

	test_ratio_alloc(local, &left, SignMinus, 3, 4);
	test_ratio_alloc(local, &right, SignMinus, 1, 6);
	multi_rr_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "multi_rr_real_common22");
	test(ratiop(pos), "multi_rr_real_common23");
	test(equal_value_ratio(pos, SignPlus, 1, 8), "multi_rr_real_common24");

	test_ratio_alloc(local, &left, SignPlus, 55, 6);
	test_ratio_alloc(local, &right, SignMinus, 4, 35);
	multi_rr_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "multi_rr_real_common25");
	test(ratiop(pos), "multi_rr_real_common26");
	test(equal_value_ratio(pos, SignMinus, 22, 21), "multi_rr_real_common27");

	test_ratio_alloc(local, &left, SignPlus, 70, 4);
	test_ratio_alloc(local, &right, SignMinus, 4, 10);
	multi_rr_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "multi_rr_real_common28");
	test(fixnump(pos), "multi_rr_real_common29");
	test(RefFixnum(pos) == -7, "multi_rr_real_common30");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  division
 */
static int test_inverse_value_ratio(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	inverse_value_ratio(local, &pos, SignPlus, 1);
	test(ratiop(pos), "inverse_value_ratio1");
	test(GetStatusDynamic(pos), "inverse_value_ratio2");
	test(equal_value_ratio(pos, SignPlus, 1, 1), "inverse_value_ratio3");
	inverse_value_ratio(local, &pos, SignMinus, 1);
	test(ratiop(pos), "inverse_value_ratio4");
	test(GetStatusDynamic(pos), "inverse_value_ratio5");
	test(equal_value_ratio(pos, SignMinus, 1, 1), "inverse_value_ratio6");
	inverse_value_ratio(local, &pos, SignMinus, 10);
	test(ratiop(pos), "inverse_value_ratio7");
	test(GetStatusDynamic(pos), "inverse_value_ratio8");
	test(equal_value_ratio(pos, SignMinus, 1, 10), "inverse_value_ratio9");
	inverse_value_ratio(local, &pos, SignPlus, 20);
	test(ratiop(pos), "inverse_value_ratio10");
	test(GetStatusDynamic(pos), "inverse_value_ratio11");
	test(equal_value_ratio(pos, SignPlus, 1, 20), "inverse_value_ratio12");

	rollback_local(local, stack);

	RETURN;
}

static int test_inverse_value_local(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	inverse_value_local(local, &pos, SignPlus, 1);
	test(fixnump(pos), "inverse_value_local1");
	test(GetStatusDynamic(pos), "inverse_value_local2");
	test(RefFixnum(pos) == 1, "inverse_value_local3");
	inverse_value_local(local, &pos, SignMinus, 1);
	test(fixnump(pos), "inverse_value_local4");
	test(GetStatusDynamic(pos), "inverse_value_local5");
	test(RefFixnum(pos) == -1, "inverse_value_local6");
	inverse_value_local(local, &pos, SignMinus, 10);
	test(ratiop(pos), "inverse_value_local7");
	test(GetStatusDynamic(pos), "inverse_value_local8");
	test(equal_value_ratio(pos, SignMinus, 1, 10), "inverse_value_local9");
	inverse_value_local(local, &pos, SignPlus, 20);
	test(ratiop(pos), "inverse_value_local10");
	test(GetStatusDynamic(pos), "inverse_value_local11");
	test(equal_value_ratio(pos, SignPlus, 1, 20), "inverse_value_local12");

	rollback_local(local, stack);

	RETURN;
}

static int test_inverse_value_common(void)
{
	addr pos;

	inverse_value_common(&pos, SignPlus, 1);
	test(fixnump(pos), "inverse_value_common1");
	test(! GetStatusDynamic(pos), "inverse_value_common2");
	test(RefFixnum(pos) == 1, "inverse_value_common3");
	inverse_value_common(&pos, SignMinus, 1);
	test(fixnump(pos), "inverse_value_common4");
	test(! GetStatusDynamic(pos), "inverse_value_common5");
	test(RefFixnum(pos) == -1, "inverse_value_common6");
	inverse_value_common(&pos, SignMinus, 10);
	test(ratiop(pos), "inverse_value_common7");
	test(! GetStatusDynamic(pos), "inverse_value_common8");
	test(equal_value_ratio(pos, SignMinus, 1, 10), "inverse_value_common9");
	inverse_value_common(&pos, SignPlus, 20);
	test(ratiop(pos), "inverse_value_common10");
	test(! GetStatusDynamic(pos), "inverse_value_common11");
	test(equal_value_ratio(pos, SignPlus, 1, 20), "inverse_value_common12");

	RETURN;
}

static int test_div_rv_ratio(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &pos, SignPlus, 4, 7);
	div_rv_ratio(local, SignMinus, pos, 6, &pos);
	test(ratiop(pos), "div_rv_ratio1");
	test(GetStatusDynamic(pos), "div_rv_ratio2");
	test(equal_value_ratio(pos, SignMinus, 2, 21), "div_rv_ratio3");

	test_ratio_alloc(local, &pos, SignPlus, 30, 1);
	div_rv_ratio(local, SignMinus, pos, 6, &pos);
	test(ratiop(pos), "div_rv_ratio4");
	test(GetStatusDynamic(pos), "div_rv_ratio5");
	test(equal_value_ratio(pos, SignMinus, 5, 1), "div_rv_ratio6");

	rollback_local(local, stack);

	RETURN;
}

static int test_div_rv_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &pos, SignPlus, 4, 7);
	div_rv_local(local, SignMinus, pos, 6, &pos);
	test(ratiop(pos), "div_rv_local1");
	test(GetStatusDynamic(pos), "div_rv_local2");
	test(equal_value_ratio(pos, SignMinus, 2, 21), "div_rv_local3");

	test_ratio_alloc(local, &pos, SignPlus, 30, 1);
	div_rv_local(local, SignMinus, pos, 6, &pos);
	test(fixnump(pos), "div_rv_local4");
	test(GetStatusDynamic(pos), "div_rv_local5");
	test(RefFixnum(pos) == -5, "div_rv_local6");

	rollback_local(local, stack);

	RETURN;
}

static int test_div_rv_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &pos, SignPlus, 4, 7);
	div_rv_common(local, SignMinus, pos, 6, &pos);
	test(ratiop(pos), "div_rv_common1");
	test(! GetStatusDynamic(pos), "div_rv_common2");
	test(equal_value_ratio(pos, SignMinus, 2, 21), "div_rv_common3");

	test_ratio_alloc(local, &pos, SignPlus, 30, 1);
	div_rv_common(local, SignMinus, pos, 6, &pos);
	test(fixnump(pos), "div_rv_common4");
	test(! GetStatusDynamic(pos), "div_rv_common5");
	test(RefFixnum(pos) == -5, "div_rv_common6");

	rollback_local(local, stack);

	RETURN;
}

static int test_div_rf_ratio_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 0, 7);
	fixnum_local(local, &right, 10);
	div_rf_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rf_ratio_local1");
	test(ratiop(pos), "div_rf_ratio_local2");
	test(zerop_ratio(pos), "div_rf_ratio_local3");

	test_ratio_alloc(local, &left, SignMinus, 1, 1);
	fixnum_local(local, &right, 10);
	div_rf_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rf_ratio_local4");
	test(ratiop(pos), "div_rf_ratio_local5");
	test(equal_value_ratio(pos, SignMinus, 1, 10), "div_rf_ratio_local6");

	test_ratio_alloc(local, &left, SignPlus, 6, 7);
	fixnum_local(local, &right, 1);
	div_rf_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rf_ratio_local7");
	test(ratiop(pos), "div_rf_ratio_local8");
	test(equal_value_ratio(pos, SignPlus, 6, 7), "div_rf_ratio_local9");

	test_ratio_alloc(local, &left, SignPlus, 6, 7);
	fixnum_local(local, &right, -1);
	div_rf_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rf_ratio_local10");
	test(ratiop(pos), "div_rf_ratio_local11");
	test(equal_value_ratio(pos, SignMinus, 6, 7), "div_rf_ratio_local12");

	test_ratio_alloc(local, &left, SignMinus, 6, 7);
	fixnum_local(local, &right, 4);
	div_rf_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rf_ratio_local13");
	test(ratiop(pos), "div_rf_ratio_local14");
	test(equal_value_ratio(pos, SignMinus, 3, 14), "div_rf_ratio_local15");

	test_ratio_alloc(local, &left, SignMinus, 30, 1);
	fixnum_local(local, &right, 5);
	div_rf_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rf_ratio_local16");
	test(ratiop(pos), "div_rf_ratio_local17");
	test(equal_value_ratio(pos, SignMinus, 6, 1), "div_rf_ratio_local18");

	rollback_local(local, stack);

	RETURN;
}

static int test_div_rf_real_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 0, 7);
	fixnum_local(local, &right, 10);
	div_rf_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rf_real_local1");
	test(fixnump(pos), "div_rf_real_local2");
	test(RefFixnum(pos) == 0, "div_rf_real_local3");

	test_ratio_alloc(local, &left, SignMinus, 1, 1);
	fixnum_local(local, &right, 10);
	div_rf_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rf_real_local4");
	test(ratiop(pos), "div_rf_real_local5");
	test(equal_value_ratio(pos, SignMinus, 1, 10), "div_rf_real_local6");

	test_ratio_alloc(local, &left, SignPlus, 6, 7);
	fixnum_local(local, &right, 1);
	div_rf_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rf_real_local7");
	test(ratiop(pos), "div_rf_real_local8");
	test(equal_value_ratio(pos, SignPlus, 6, 7), "div_rf_real_local9");

	test_ratio_alloc(local, &left, SignPlus, 6, 7);
	fixnum_local(local, &right, -1);
	div_rf_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rf_real_local10");
	test(ratiop(pos), "div_rf_real_local11");
	test(equal_value_ratio(pos, SignMinus, 6, 7), "div_rf_real_local12");

	test_ratio_alloc(local, &left, SignMinus, 6, 7);
	fixnum_local(local, &right, 4);
	div_rf_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rf_real_local13");
	test(ratiop(pos), "div_rf_real_local14");
	test(equal_value_ratio(pos, SignMinus, 3, 14), "div_rf_real_local15");

	test_ratio_alloc(local, &left, SignMinus, 30, 1);
	fixnum_local(local, &right, 5);
	div_rf_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rf_real_local16");
	test(fixnump(pos), "div_rf_real_local17");
	test(RefFixnum(pos) == -6, "div_rf_real_local18");

	rollback_local(local, stack);

	RETURN;
}

static int test_div_rf_real_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 0, 7);
	fixnum_local(local, &right, 10);
	div_rf_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_rf_real_common1");
	test(fixnump(pos), "div_rf_real_common2");
	test(RefFixnum(pos) == 0, "div_rf_real_common3");

	test_ratio_alloc(local, &left, SignMinus, 1, 1);
	fixnum_local(local, &right, 10);
	div_rf_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_rf_real_common4");
	test(ratiop(pos), "div_rf_real_common5");
	test(equal_value_ratio(pos, SignMinus, 1, 10), "div_rf_real_common6");

	test_ratio_alloc(local, &left, SignPlus, 6, 7);
	fixnum_local(local, &right, 1);
	div_rf_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_rf_real_common7");
	test(ratiop(pos), "div_rf_real_common8");
	test(equal_value_ratio(pos, SignPlus, 6, 7), "div_rf_real_common9");

	test_ratio_alloc(local, &left, SignPlus, 6, 7);
	fixnum_local(local, &right, -1);
	div_rf_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_rf_real_common10");
	test(ratiop(pos), "div_rf_real_common11");
	test(equal_value_ratio(pos, SignMinus, 6, 7), "div_rf_real_common12");

	test_ratio_alloc(local, &left, SignMinus, 6, 7);
	fixnum_local(local, &right, 4);
	div_rf_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_rf_real_common13");
	test(ratiop(pos), "div_rf_real_common14");
	test(equal_value_ratio(pos, SignMinus, 3, 14), "div_rf_real_common15");

	test_ratio_alloc(local, &left, SignMinus, 30, 1);
	fixnum_local(local, &right, 5);
	div_rf_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_rf_real_common16");
	test(fixnump(pos), "div_rf_real_common17");
	test(RefFixnum(pos) == -6, "div_rf_real_common18");

	rollback_local(local, stack);

	RETURN;
}

static int test_ratio_sign_inverse_ratio(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &pos, SignPlus, 6, 7);
	ratio_sign_inverse_ratio(local, &pos, SignMinus, pos);
	test(ratiop(pos), "ratio_sign_inverse_ratio1");
	test(GetStatusDynamic(pos), "ratio_sign_inverse_ratio2");
	test(equal_value_ratio(pos, SignMinus, 7, 6), "ratio_sign_inverse_ratio3");

	test_ratio_alloc(local, &pos, SignMinus, 1, 7);
	ratio_sign_inverse_ratio(local, &pos, SignPlus, pos);
	test(ratiop(pos), "ratio_sign_inverse_ratio4");
	test(GetStatusDynamic(pos), "ratio_sign_inverse_ratio5");
	test(equal_value_ratio(pos, SignPlus, 7, 1), "ratio_sign_inverse_ratio6");

	test_ratio_alloc(local, &pos, SignMinus, 1, 6);
	ratio_sign_inverse_ratio(local, &pos, SignMinus, pos);
	test(ratiop(pos), "ratio_sign_inverse_ratio7");
	test(GetStatusDynamic(pos), "ratio_sign_inverse_ratio8");
	test(equal_value_ratio(pos, SignMinus, 6, 1), "ratio_sign_inverse_ratio9");

	rollback_local(local, stack);

	RETURN;
}

static int test_ratio_sign_inverse_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &pos, SignPlus, 6, 7);
	ratio_sign_inverse_local(local, &pos, SignMinus, pos);
	test(ratiop(pos), "ratio_sign_inverse_local1");
	test(GetStatusDynamic(pos), "ratio_sign_inverse_local2");
	test(equal_value_ratio(pos, SignMinus, 7, 6), "ratio_sign_inverse_local3");

	test_ratio_alloc(local, &pos, SignMinus, 1, 7);
	ratio_sign_inverse_local(local, &pos, SignPlus, pos);
	test(fixnump(pos), "ratio_sign_inverse_local4");
	test(GetStatusDynamic(pos), "ratio_sign_inverse_local5");
	test(RefFixnum(pos) == 7, "ratio_sign_inverse_local6");

	test_ratio_alloc(local, &pos, SignMinus, 1, 6);
	ratio_sign_inverse_local(local, &pos, SignMinus, pos);
	test(fixnump(pos), "ratio_sign_inverse_local7");
	test(GetStatusDynamic(pos), "ratio_sign_inverse_local8");
	test(RefFixnum(pos) == -6, "ratio_sign_inverse_local9");

	rollback_local(local, stack);

	RETURN;
}

static int test_ratio_sign_inverse_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &pos, SignPlus, 6, 7);
	ratio_sign_inverse_common(local, &pos, SignMinus, pos);
	test(ratiop(pos), "ratio_sign_inverse_common1");
	test(! GetStatusDynamic(pos), "ratio_sign_inverse_common2");
	test(equal_value_ratio(pos, SignMinus, 7, 6), "ratio_sign_inverse_common3");

	test_ratio_alloc(local, &pos, SignMinus, 1, 7);
	ratio_sign_inverse_common(local, &pos, SignPlus, pos);
	test(fixnump(pos), "ratio_sign_inverse_common4");
	test(! GetStatusDynamic(pos), "ratio_sign_inverse_common5");
	test(RefFixnum(pos) == 7, "ratio_sign_inverse_common6");

	test_ratio_alloc(local, &pos, SignMinus, 1, 6);
	ratio_sign_inverse_common(local, &pos, SignMinus, pos);
	test(fixnump(pos), "ratio_sign_inverse_common7");
	test(! GetStatusDynamic(pos), "ratio_sign_inverse_common8");
	test(RefFixnum(pos) == -6, "ratio_sign_inverse_common9");

	rollback_local(local, stack);

	RETURN;
}

static int test_div_vr_ratio(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &pos, SignPlus, 6, 7);
	div_vr_ratio(local, SignMinus, 11, pos, &pos);
	test(GetStatusDynamic(pos), "div_vr_ratio1");
	test(ratiop(pos), "div_vr_ratio2");
	test(equal_value_ratio(pos, SignMinus, 77, 6), "div_vr_ratio3");

	test_ratio_alloc(local, &pos, SignMinus, 6, 7);
	div_vr_ratio(local, SignMinus, 4, pos, &pos);
	test(GetStatusDynamic(pos), "div_vr_ratio4");
	test(ratiop(pos), "div_vr_ratio5");
	test(equal_value_ratio(pos, SignMinus, 14, 3), "div_vr_ratio6");

	test_ratio_alloc(local, &pos, SignMinus, 4, 7);
	div_vr_ratio(local, SignMinus, 8, pos, &pos);
	test(GetStatusDynamic(pos), "div_vr_ratio7");
	test(ratiop(pos), "div_vr_ratio8");
	test(equal_value_ratio(pos, SignMinus, 14, 1), "div_vr_ratio9");

	rollback_local(local, stack);

	RETURN;
}

static int test_div_vr_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &pos, SignPlus, 6, 7);
	div_vr_local(local, SignMinus, 11, pos, &pos);
	test(GetStatusDynamic(pos), "div_vr_local1");
	test(ratiop(pos), "div_vr_local2");
	test(equal_value_ratio(pos, SignMinus, 77, 6), "div_vr_local3");

	test_ratio_alloc(local, &pos, SignMinus, 6, 7);
	div_vr_local(local, SignMinus, 4, pos, &pos);
	test(GetStatusDynamic(pos), "div_vr_local4");
	test(ratiop(pos), "div_vr_local5");
	test(equal_value_ratio(pos, SignMinus, 14, 3), "div_vr_local6");

	test_ratio_alloc(local, &pos, SignMinus, 4, 7);
	div_vr_local(local, SignMinus, 8, pos, &pos);
	test(GetStatusDynamic(pos), "div_vr_local7");
	test(fixnump(pos), "div_vr_local8");
	test(RefFixnum(pos) == -14, "div_vr_local9");

	rollback_local(local, stack);

	RETURN;
}

static int test_div_vr_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &pos, SignPlus, 6, 7);
	div_vr_common(local, SignMinus, 11, pos, &pos);
	test(! GetStatusDynamic(pos), "div_vr_common1");
	test(ratiop(pos), "div_vr_common2");
	test(equal_value_ratio(pos, SignMinus, 77, 6), "div_vr_common3");

	test_ratio_alloc(local, &pos, SignMinus, 6, 7);
	div_vr_common(local, SignMinus, 4, pos, &pos);
	test(! GetStatusDynamic(pos), "div_vr_common4");
	test(ratiop(pos), "div_vr_common5");
	test(equal_value_ratio(pos, SignMinus, 14, 3), "div_vr_common6");

	test_ratio_alloc(local, &pos, SignMinus, 4, 7);
	div_vr_common(local, SignMinus, 8, pos, &pos);
	test(! GetStatusDynamic(pos), "div_vr_common7");
	test(fixnump(pos), "div_vr_common8");
	test(RefFixnum(pos) == -14, "div_vr_common9");

	rollback_local(local, stack);

	RETURN;
}

static int test_div_fr_ratio_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_local(local, &left, 0);
	test_ratio_alloc(local, &right, SignPlus, 6, 7);
	div_fr_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_fr_ratio_local1");
	test(ratiop(pos), "div_fr_ratio_local2");
	test(zerop_ratio(pos), "div_fr_ratio_local3");

	fixnum_local(local, &left, -1);
	test_ratio_alloc(local, &right, SignPlus, 6, 7);
	div_fr_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_fr_ratio_local4");
	test(ratiop(pos), "div_fr_ratio_local5");
	test(equal_value_ratio(pos, SignMinus, 7, 6), "div_fr_ratio_local6");

	fixnum_local(local, &left, -7);
	test_ratio_alloc(local, &right, SignPlus, 1, 1);
	div_fr_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_fr_ratio_local7");
	test(ratiop(pos), "div_fr_ratio_local8");
	test(equal_value_ratio(pos, SignMinus, 7, 1), "div_fr_ratio_local9");

	fixnum_local(local, &left, -7);
	test_ratio_alloc(local, &right, SignMinus, 1, 1);
	div_fr_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_fr_ratio_local10");
	test(ratiop(pos), "div_fr_ratio_local11");
	test(equal_value_ratio(pos, SignPlus, 7, 1), "div_fr_ratio_local12");

	fixnum_local(local, &left, -7);
	test_ratio_alloc(local, &right, SignPlus, 14, 5);
	div_fr_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_fr_ratio_local13");
	test(ratiop(pos), "div_fr_ratio_local14");
	test(equal_value_ratio(pos, SignMinus, 5, 2), "div_fr_ratio_local15");

	rollback_local(local, stack);

	RETURN;
}

static int test_div_fr_real_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_local(local, &left, 0);
	test_ratio_alloc(local, &right, SignPlus, 6, 7);
	div_fr_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_fr_real_local1");
	test(fixnump(pos), "div_fr_real_local2");
	test(RefFixnum(pos) == 0, "div_fr_real_local3");

	fixnum_local(local, &left, -1);
	test_ratio_alloc(local, &right, SignPlus, 6, 7);
	div_fr_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_fr_real_local4");
	test(ratiop(pos), "div_fr_real_local5");
	test(equal_value_ratio(pos, SignMinus, 7, 6), "div_fr_real_local6");

	fixnum_local(local, &left, -7);
	test_ratio_alloc(local, &right, SignPlus, 1, 1);
	div_fr_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_fr_real_local7");
	test(fixnump(pos), "div_fr_real_local8");
	test(RefFixnum(pos) == -7, "div_fr_real_local9");

	fixnum_local(local, &left, -7);
	test_ratio_alloc(local, &right, SignMinus, 1, 1);
	div_fr_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_fr_real_local10");
	test(fixnump(pos), "div_fr_real_local11");
	test(RefFixnum(pos) == 7, "div_fr_real_local12");

	fixnum_local(local, &left, -7);
	test_ratio_alloc(local, &right, SignPlus, 14, 5);
	div_fr_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_fr_real_local13");
	test(ratiop(pos), "div_fr_real_local14");
	test(equal_value_ratio(pos, SignMinus, 5, 2), "div_fr_real_local15");

	rollback_local(local, stack);

	RETURN;
}

static int test_div_fr_real_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_local(local, &left, 0);
	test_ratio_alloc(local, &right, SignPlus, 6, 7);
	div_fr_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_fr_real_common1");
	test(fixnump(pos), "div_fr_real_common2");
	test(RefFixnum(pos) == 0, "div_fr_real_common3");

	fixnum_local(local, &left, -1);
	test_ratio_alloc(local, &right, SignPlus, 6, 7);
	div_fr_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_fr_real_common4");
	test(ratiop(pos), "div_fr_real_common5");
	test(equal_value_ratio(pos, SignMinus, 7, 6), "div_fr_real_common6");

	fixnum_local(local, &left, -7);
	test_ratio_alloc(local, &right, SignPlus, 1, 1);
	div_fr_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_fr_real_common7");
	test(fixnump(pos), "div_fr_real_common8");
	test(RefFixnum(pos) == -7, "div_fr_real_common9");

	fixnum_local(local, &left, -7);
	test_ratio_alloc(local, &right, SignMinus, 1, 1);
	div_fr_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_fr_real_common10");
	test(fixnump(pos), "div_fr_real_common11");
	test(RefFixnum(pos) == 7, "div_fr_real_common12");

	fixnum_local(local, &left, -7);
	test_ratio_alloc(local, &right, SignPlus, 14, 5);
	div_fr_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_fr_real_common13");
	test(ratiop(pos), "div_fr_real_common14");
	test(equal_value_ratio(pos, SignMinus, 5, 2), "div_fr_real_common15");

	rollback_local(local, stack);

	RETURN;
}

static int test_bignum_sign_inverse_ratio(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &pos, SignMinus, 1);
	bignum_sign_inverse_ratio(local, &pos, SignPlus, pos);
	test(ratiop(pos), "bignum_sign_inverse_ratio1");
	test(GetStatusDynamic(pos), "bignum_sign_inverse_ratio2");
	test(equal_value_ratio(pos, SignPlus, 1, 1), "bignum_sign_inverse_ratio3");

	bignum_value_local(local, &pos, SignPlus, 1);
	bignum_sign_inverse_ratio(local, &pos, SignMinus, pos);
	test(ratiop(pos), "bignum_sign_inverse_ratio4");
	test(GetStatusDynamic(pos), "bignum_sign_inverse_ratio5");
	test(equal_value_ratio(pos, SignMinus, 1, 1), "bignum_sign_inverse_ratio6");

	bignum_value_local(local, &pos, SignPlus, 10);
	bignum_sign_inverse_ratio(local, &pos, SignMinus, pos);
	test(ratiop(pos), "bignum_sign_inverse_ratio7");
	test(GetStatusDynamic(pos), "bignum_sign_inverse_ratio8");
	test(equal_value_ratio(pos, SignMinus, 1, 10), "bignum_sign_inverse_ratio9");

	rollback_local(local, stack);

	RETURN;
}

static int test_bignum_sign_inverse_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &pos, SignMinus, 1);
	bignum_sign_inverse_local(local, &pos, SignPlus, pos);
	test(fixnump(pos), "bignum_sign_inverse_local1");
	test(GetStatusDynamic(pos), "bignum_sign_inverse_local2");
	test(RefFixnum(pos) == 1, "bignum_sign_inverse_local3");

	bignum_value_local(local, &pos, SignPlus, 1);
	bignum_sign_inverse_local(local, &pos, SignMinus, pos);
	test(fixnump(pos), "bignum_sign_inverse_local4");
	test(GetStatusDynamic(pos), "bignum_sign_inverse_local5");
	test(RefFixnum(pos) == -1, "bignum_sign_inverse_local6");

	bignum_value_local(local, &pos, SignPlus, 10);
	bignum_sign_inverse_local(local, &pos, SignMinus, pos);
	test(ratiop(pos), "bignum_sign_inverse_local7");
	test(GetStatusDynamic(pos), "bignum_sign_inverse_local8");
	test(equal_value_ratio(pos, SignMinus, 1, 10), "bignum_sign_inverse_local9");

	rollback_local(local, stack);

	RETURN;
}

static int test_bignum_sign_inverse_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &pos, SignMinus, 1);
	bignum_sign_inverse_common(&pos, SignPlus, pos);
	test(fixnump(pos), "bignum_sign_inverse_common1");
	test(! GetStatusDynamic(pos), "bignum_sign_inverse_common2");
	test(RefFixnum(pos) == 1, "bignum_sign_inverse_common3");

	bignum_value_local(local, &pos, SignPlus, 1);
	bignum_sign_inverse_common(&pos, SignMinus, pos);
	test(fixnump(pos), "bignum_sign_inverse_common4");
	test(! GetStatusDynamic(pos), "bignum_sign_inverse_common5");
	test(RefFixnum(pos) == -1, "bignum_sign_inverse_common6");

	bignum_value_local(local, &pos, SignPlus, 10);
	bignum_sign_inverse_common(&pos, SignMinus, pos);
	test(ratiop(pos), "bignum_sign_inverse_common7");
	test(! GetStatusDynamic(pos), "bignum_sign_inverse_common8");
	test(equal_value_ratio(pos, SignMinus, 1, 10), "bignum_sign_inverse_common9");

	rollback_local(local, stack);

	RETURN;
}

static int test_div_rb_ratio_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 0, 7);
	bignum_value_alloc(local, &right, SignPlus, 10);
	div_rb_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rb_ratio_local1");
	test(ratiop(pos), "div_rb_ratio_local2");
	test(zerop_ratio(pos), "div_rb_ratio_local3");

	test_ratio_alloc(local, &left, SignMinus, 1, 1);
	bignum_value_alloc(local, &right, SignPlus, 10);
	div_rb_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rb_ratio_local4");
	test(ratiop(pos), "div_rb_ratio_local5");
	test(equal_value_ratio(pos, SignMinus, 1, 10), "div_rb_ratio_local6");

	test_ratio_alloc(local, &left, SignPlus, 6, 7);
	bignum_value_alloc(local, &right, SignPlus, 1);
	div_rb_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rb_ratio_local7");
	test(ratiop(pos), "div_rb_ratio_local8");
	test(equal_value_ratio(pos, SignPlus, 6, 7), "div_rb_ratio_local9");

	test_ratio_alloc(local, &left, SignPlus, 6, 7);
	bignum_value_alloc(local, &right, SignMinus, 1);
	div_rb_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rb_ratio_local10");
	test(ratiop(pos), "div_rb_ratio_local11");
	test(equal_value_ratio(pos, SignMinus, 6, 7), "div_rb_ratio_local12");

	test_ratio_alloc(local, &left, SignMinus, 6, 7);
	bignum_value_alloc(local, &right, SignPlus, 4);
	div_rb_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rb_ratio_local13");
	test(ratiop(pos), "div_rb_ratio_local14");
	test(equal_value_ratio(pos, SignMinus, 3, 14), "div_rb_ratio_local15");

	rollback_local(local, stack);

	RETURN;
}

static int test_div_rb_real_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 0, 7);
	bignum_value_alloc(local, &right, SignPlus, 10);
	div_rb_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rb_real_local1");
	test(fixnump(pos), "div_rb_real_local2");
	test(RefFixnum(pos) == 0, "div_rb_real_local3");

	test_ratio_alloc(local, &left, SignMinus, 1, 1);
	bignum_value_alloc(local, &right, SignPlus, 10);
	div_rb_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rb_real_local4");
	test(ratiop(pos), "div_rb_real_local5");
	test(equal_value_ratio(pos, SignMinus, 1, 10), "div_rb_real_local6");

	test_ratio_alloc(local, &left, SignPlus, 6, 7);
	bignum_value_alloc(local, &right, SignPlus, 1);
	div_rb_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rb_real_local7");
	test(ratiop(pos), "div_rb_real_local8");
	test(equal_value_ratio(pos, SignPlus, 6, 7), "div_rb_real_local9");

	test_ratio_alloc(local, &left, SignPlus, 6, 7);
	bignum_value_alloc(local, &right, SignMinus, 1);
	div_rb_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rb_real_local10");
	test(ratiop(pos), "div_rb_real_local11");
	test(equal_value_ratio(pos, SignMinus, 6, 7), "div_rb_real_local12");

	test_ratio_alloc(local, &left, SignMinus, 6, 7);
	bignum_value_alloc(local, &right, SignPlus, 4);
	div_rb_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rb_real_local13");
	test(ratiop(pos), "div_rb_real_local14");
	test(equal_value_ratio(pos, SignMinus, 3, 14), "div_rb_real_local15");

	rollback_local(local, stack);

	RETURN;
}

static int test_div_rb_real_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 0, 7);
	bignum_value_alloc(local, &right, SignPlus, 10);
	div_rb_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_rb_real_common1");
	test(fixnump(pos), "div_rb_real_common2");
	test(RefFixnum(pos) == 0, "div_rb_real_common3");

	test_ratio_alloc(local, &left, SignMinus, 1, 1);
	bignum_value_alloc(local, &right, SignPlus, 10);
	div_rb_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_rb_real_common4");
	test(ratiop(pos), "div_rb_real_common5");
	test(equal_value_ratio(pos, SignMinus, 1, 10), "div_rb_real_common6");

	test_ratio_alloc(local, &left, SignPlus, 6, 7);
	bignum_value_alloc(local, &right, SignPlus, 1);
	div_rb_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_rb_real_common7");
	test(ratiop(pos), "div_rb_real_common8");
	test(equal_value_ratio(pos, SignPlus, 6, 7), "div_rb_real_common9");

	test_ratio_alloc(local, &left, SignPlus, 6, 7);
	bignum_value_alloc(local, &right, SignMinus, 1);
	div_rb_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_rb_real_common10");
	test(ratiop(pos), "div_rb_real_common11");
	test(equal_value_ratio(pos, SignMinus, 6, 7), "div_rb_real_common12");

	test_ratio_alloc(local, &left, SignMinus, 6, 7);
	bignum_value_alloc(local, &right, SignPlus, 4);
	div_rb_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_rb_real_common13");
	test(ratiop(pos), "div_rb_real_common14");
	test(equal_value_ratio(pos, SignMinus, 3, 14), "div_rb_real_common15");

	rollback_local(local, stack);

	RETURN;
}

static int test_div_br_ratio(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_alloc(local, &left, SignPlus, 11);
	test_ratio_alloc(local, &right, SignPlus, 6, 7);
	div_br_ratio(local, SignMinus, left, right, &pos);
	test(GetStatusDynamic(pos), "div_br_ratio1");
	test(ratiop(pos), "div_br_ratio2");
	test(equal_value_ratio(pos, SignMinus, 77, 6), "div_br_ratio3");

	bignum_value_alloc(local, &left, SignPlus, 4);
	test_ratio_alloc(local, &right, SignMinus, 6, 7);
	div_br_ratio(local, SignMinus, left, right, &pos);
	test(GetStatusDynamic(pos), "div_br_ratio4");
	test(ratiop(pos), "div_br_ratio5");
	test(equal_value_ratio(pos, SignMinus, 14, 3), "div_br_ratio6");

	bignum_value_alloc(local, &left, SignMinus, 8);
	test_ratio_alloc(local, &right, SignMinus, 4, 7);
	div_br_ratio(local, SignMinus, left, right, &pos);
	test(GetStatusDynamic(pos), "div_br_ratio7");
	test(ratiop(pos), "div_br_ratio8");
	test(equal_value_ratio(pos, SignMinus, 14, 1), "div_br_ratio9");

	rollback_local(local, stack);

	RETURN;
}

static int test_div_br_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_alloc(local, &left, SignPlus, 11);
	test_ratio_alloc(local, &right, SignPlus, 6, 7);
	div_br_local(local, SignMinus, left, right, &pos);
	test(GetStatusDynamic(pos), "div_br_local1");
	test(ratiop(pos), "div_br_local2");
	test(equal_value_ratio(pos, SignMinus, 77, 6), "div_br_local3");

	bignum_value_alloc(local, &left, SignPlus, 4);
	test_ratio_alloc(local, &right, SignMinus, 6, 7);
	div_br_local(local, SignMinus, left, right, &pos);
	test(GetStatusDynamic(pos), "div_br_local4");
	test(ratiop(pos), "div_br_local5");
	test(equal_value_ratio(pos, SignMinus, 14, 3), "div_br_local6");

	bignum_value_alloc(local, &left, SignMinus, 8);
	test_ratio_alloc(local, &right, SignMinus, 4, 7);
	div_br_local(local, SignMinus, left, right, &pos);
	test(GetStatusDynamic(pos), "div_br_local7");
	test(fixnump(pos), "div_br_local8");
	test(RefFixnum(pos) == -14, "div_br_local9");

	rollback_local(local, stack);

	RETURN;
}

static int test_div_br_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_alloc(local, &left, SignPlus, 11);
	test_ratio_alloc(local, &right, SignPlus, 6, 7);
	div_br_common(local, SignMinus, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_br_common1");
	test(ratiop(pos), "div_br_common2");
	test(equal_value_ratio(pos, SignMinus, 77, 6), "div_br_common3");

	bignum_value_alloc(local, &left, SignPlus, 4);
	test_ratio_alloc(local, &right, SignMinus, 6, 7);
	div_br_common(local, SignMinus, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_br_common4");
	test(ratiop(pos), "div_br_common5");
	test(equal_value_ratio(pos, SignMinus, 14, 3), "div_br_common6");

	bignum_value_alloc(local, &left, SignMinus, 8);
	test_ratio_alloc(local, &right, SignMinus, 4, 7);
	div_br_common(local, SignMinus, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_br_common7");
	test(fixnump(pos), "div_br_common8");
	test(RefFixnum(pos) == -14, "div_br_common9");

	rollback_local(local, stack);

	RETURN;
}

static int test_div_br_ratio_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_alloc(local, &left, SignPlus, 0);
	test_ratio_alloc(local, &right, SignPlus, 6, 7);
	div_br_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_br_ratio_local1");
	test(ratiop(pos), "div_br_ratio_local2");
	test(zerop_ratio(pos), "div_br_ratio_local3");

	bignum_value_alloc(local, &left, SignMinus, 1);
	test_ratio_alloc(local, &right, SignPlus, 6, 7);
	div_br_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_br_ratio_local4");
	test(ratiop(pos), "div_br_ratio_local5");
	test(equal_value_ratio(pos, SignMinus, 7, 6), "div_br_ratio_local6");

	bignum_value_alloc(local, &left, SignMinus, 7);
	test_ratio_alloc(local, &right, SignPlus, 1, 1);
	div_br_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_br_ratio_local7");
	test(ratiop(pos), "div_br_ratio_local8");
	test(equal_value_ratio(pos, SignMinus, 7, 1), "div_br_ratio_local9");

	bignum_value_alloc(local, &left, SignMinus, 7);
	test_ratio_alloc(local, &right, SignMinus, 1, 1);
	div_br_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_br_ratio_local10");
	test(ratiop(pos), "div_br_ratio_local11");
	test(equal_value_ratio(pos, SignPlus, 7, 1), "div_br_ratio_local12");

	bignum_value_alloc(local, &left, SignMinus, 7);
	test_ratio_alloc(local, &right, SignPlus, 14, 5);
	div_br_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_br_ratio_local13");
	test(ratiop(pos), "div_br_ratio_local14");
	test(equal_value_ratio(pos, SignMinus, 5, 2), "div_br_ratio_local15");

	rollback_local(local, stack);

	RETURN;
}

static int test_div_br_real_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_alloc(local, &left, SignPlus, 0);
	test_ratio_alloc(local, &right, SignPlus, 6, 7);
	div_br_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_br_real_local1");
	test(fixnump(pos), "div_br_real_local2");
	test(RefFixnum(pos) == 0, "div_br_real_local3");

	bignum_value_alloc(local, &left, SignMinus, 1);
	test_ratio_alloc(local, &right, SignPlus, 6, 7);
	div_br_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_br_real_local4");
	test(ratiop(pos), "div_br_real_local5");
	test(equal_value_ratio(pos, SignMinus, 7, 6), "div_br_real_local6");

	bignum_value_alloc(local, &left, SignMinus, 7);
	test_ratio_alloc(local, &right, SignPlus, 1, 1);
	div_br_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_br_real_local7");
	test(bignump(pos), "div_br_real_local8");
	test(equal_value_bignum(pos, SignMinus, 7), "div_br_real_local9");

	bignum_value_alloc(local, &left, SignMinus, 7);
	test_ratio_alloc(local, &right, SignMinus, 1, 1);
	div_br_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_br_real_local10");
	test(fixnump(pos), "div_br_real_local11");
	test(RefFixnum(pos) == 7, "div_br_real_local12");

	bignum_value_alloc(local, &left, SignMinus, 7);
	test_ratio_alloc(local, &right, SignPlus, 14, 5);
	div_br_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_br_real_local13");
	test(ratiop(pos), "div_br_real_local14");
	test(equal_value_ratio(pos, SignMinus, 5, 2), "div_br_real_local15");

	rollback_local(local, stack);

	RETURN;
}

static int test_div_br_real_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_alloc(local, &left, SignPlus, 0);
	test_ratio_alloc(local, &right, SignPlus, 6, 7);
	div_br_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_br_real_common1");
	test(fixnump(pos), "div_br_real_common2");
	test(RefFixnum(pos) == 0, "div_br_real_common3");

	bignum_value_alloc(local, &left, SignMinus, 1);
	test_ratio_alloc(local, &right, SignPlus, 6, 7);
	div_br_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_br_real_common4");
	test(ratiop(pos), "div_br_real_common5");
	test(equal_value_ratio(pos, SignMinus, 7, 6), "div_br_real_common6");

	bignum_value_alloc(local, &left, SignMinus, 7);
	test_ratio_alloc(local, &right, SignPlus, 1, 1);
	div_br_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_br_real_common7");
	test(bignump(pos), "div_br_real_common8");
	test(equal_value_bignum(pos, SignMinus, 7), "div_br_real_common9");

	bignum_value_alloc(local, &left, SignMinus, 7);
	test_ratio_alloc(local, &right, SignMinus, 1, 1);
	div_br_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_br_real_common10");
	test(fixnump(pos), "div_br_real_common11");
	test(RefFixnum(pos) == 7, "div_br_real_common12");

	bignum_value_alloc(local, &left, SignMinus, 7);
	test_ratio_alloc(local, &right, SignPlus, 14, 5);
	div_br_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_br_real_common13");
	test(ratiop(pos), "div_br_real_common14");
	test(equal_value_ratio(pos, SignMinus, 5, 2), "div_br_real_common15");

	rollback_local(local, stack);

	RETURN;
}

static int test_div_rr_ratio(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 5, 6);
	test_ratio_alloc(local, &right, SignPlus, 8, 7);
	div_rr_ratio(local, SignMinus, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rr_ratio1");
	test(ratiop(pos), "div_rr_ratio2");
	test(equal_value_ratio(pos, SignMinus, 35, 48), "div_rr_ratio3");

	test_ratio_alloc(local, &left, SignPlus, 55, 6);
	test_ratio_alloc(local, &right, SignPlus, 35, 4);
	div_rr_ratio(local, SignPlus, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rr_ratio4");
	test(ratiop(pos), "div_rr_ratio5");
	test(equal_value_ratio(pos, SignPlus, 22, 21), "div_rr_ratio6");

	test_ratio_alloc(local, &left, SignPlus, 10, 5);
	test_ratio_alloc(local, &right, SignPlus, 2, 5);
	div_rr_ratio(local, SignMinus, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rr_ratio7");
	test(ratiop(pos), "div_rr_ratio8");
	test(equal_value_ratio(pos, SignMinus, 5, 1), "div_rr_ratio9");

	rollback_local(local, stack);

	RETURN;
}

static int test_div_rr_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 5, 6);
	test_ratio_alloc(local, &right, SignPlus, 8, 7);
	div_rr_local(local, SignMinus, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rr_local1");
	test(ratiop(pos), "div_rr_local2");
	test(equal_value_ratio(pos, SignMinus, 35, 48), "div_rr_local3");

	test_ratio_alloc(local, &left, SignPlus, 55, 6);
	test_ratio_alloc(local, &right, SignPlus, 35, 4);
	div_rr_local(local, SignPlus, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rr_local4");
	test(ratiop(pos), "div_rr_local5");
	test(equal_value_ratio(pos, SignPlus, 22, 21), "div_rr_local6");

	test_ratio_alloc(local, &left, SignPlus, 10, 5);
	test_ratio_alloc(local, &right, SignPlus, 2, 5);
	div_rr_local(local, SignMinus, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rr_local7");
	test(fixnump(pos), "div_rr_local8");
	test(RefFixnum(pos) == -5, "div_rr_local9");

	rollback_local(local, stack);

	RETURN;
}

static int test_div_rr_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 5, 6);
	test_ratio_alloc(local, &right, SignPlus, 8, 7);
	div_rr_common(local, SignMinus, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_rr_common1");
	test(ratiop(pos), "div_rr_common2");
	test(equal_value_ratio(pos, SignMinus, 35, 48), "div_rr_common3");

	test_ratio_alloc(local, &left, SignPlus, 55, 6);
	test_ratio_alloc(local, &right, SignPlus, 35, 4);
	div_rr_common(local, SignPlus, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_rr_common4");
	test(ratiop(pos), "div_rr_common5");
	test(equal_value_ratio(pos, SignPlus, 22, 21), "div_rr_common6");

	test_ratio_alloc(local, &left, SignPlus, 10, 5);
	test_ratio_alloc(local, &right, SignPlus, 2, 5);
	div_rr_common(local, SignMinus, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_rr_common7");
	test(fixnump(pos), "div_rr_common8");
	test(RefFixnum(pos) == -5, "div_rr_common9");

	rollback_local(local, stack);

	RETURN;
}

static int test_div_bir_ratio(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_alloc(local, &left, SignPlus, 11);
	test_ratio_alloc(local, &right, SignPlus, 7, 6);
	div_bir_ratio(local, SignMinus, left, right, &pos);
	test(GetStatusDynamic(pos), "div_bir_ratio1");
	test(ratiop(pos), "div_bir_ratio2");
	test(equal_value_ratio(pos, SignMinus, 6, 77), "div_bir_ratio3");

	bignum_value_alloc(local, &left, SignPlus, 4);
	test_ratio_alloc(local, &right, SignMinus, 7, 6);
	div_bir_ratio(local, SignMinus, left, right, &pos);
	test(GetStatusDynamic(pos), "div_bir_ratio4");
	test(ratiop(pos), "div_bir_ratio5");
	test(equal_value_ratio(pos, SignMinus, 3, 14), "div_bir_ratio6");

	bignum_value_alloc(local, &left, SignMinus, 8);
	test_ratio_alloc(local, &right, SignMinus, 7, 4);
	div_bir_ratio(local, SignMinus, left, right, &pos);
	test(GetStatusDynamic(pos), "div_bir_ratio7");
	test(ratiop(pos), "div_bir_ratio8");
	test(equal_value_ratio(pos, SignMinus, 1, 14), "div_bir_ratio9");

	rollback_local(local, stack);

	RETURN;
}

static int test_div_bir_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_alloc(local, &left, SignPlus, 11);
	test_ratio_alloc(local, &right, SignPlus, 7, 6);
	div_bir_local(local, SignMinus, left, right, &pos);
	test(GetStatusDynamic(pos), "div_bir_local1");
	test(ratiop(pos), "div_bir_local2");
	test(equal_value_ratio(pos, SignMinus, 6, 77), "div_bir_local3");

	bignum_value_alloc(local, &left, SignPlus, 4);
	test_ratio_alloc(local, &right, SignMinus, 7, 6);
	div_bir_local(local, SignMinus, left, right, &pos);
	test(GetStatusDynamic(pos), "div_bir_local4");
	test(ratiop(pos), "div_bir_local5");
	test(equal_value_ratio(pos, SignMinus, 3, 14), "div_bir_local6");

	bignum_value_alloc(local, &left, SignMinus, 8);
	test_ratio_alloc(local, &right, SignMinus, 7, 4);
	div_bir_local(local, SignMinus, left, right, &pos);
	test(GetStatusDynamic(pos), "div_bir_local7");
	test(ratiop(pos), "div_bir_local8");
	test(equal_value_ratio(pos, SignMinus, 1, 14), "div_bir_local9");

	rollback_local(local, stack);

	RETURN;
}

static int test_div_bir_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_alloc(local, &left, SignPlus, 11);
	test_ratio_alloc(local, &right, SignPlus, 7, 6);
	div_bir_common(local, SignMinus, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_bir_common1");
	test(ratiop(pos), "div_bir_common2");
	test(equal_value_ratio(pos, SignMinus, 6, 77), "div_bir_common3");

	bignum_value_alloc(local, &left, SignPlus, 4);
	test_ratio_alloc(local, &right, SignMinus, 7, 6);
	div_bir_common(local, SignMinus, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_bir_common4");
	test(ratiop(pos), "div_bir_common5");
	test(equal_value_ratio(pos, SignMinus, 3, 14), "div_bir_common6");

	bignum_value_alloc(local, &left, SignMinus, 8);
	test_ratio_alloc(local, &right, SignMinus, 7, 4);
	div_bir_common(local, SignMinus, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_bir_common7");
	test(ratiop(pos), "div_bir_common8");
	test(equal_value_ratio(pos, SignMinus, 1, 14), "div_bir_common9");

	rollback_local(local, stack);

	RETURN;
}

static int test_div_rr_ratio_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 0, 6);
	test_ratio_alloc(local, &right, SignPlus, 3, 4);
	div_rr_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rr_ratio_local1");
	test(ratiop(pos), "div_rr_ratio_local2");
	test(zerop_ratio(pos), "div_rr_ratio_local3");

	test_ratio_alloc(local, &left, SignPlus, 1, 1);
	test_ratio_alloc(local, &right, SignPlus, 3, 4);
	div_rr_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rr_ratio_local4");
	test(ratiop(pos), "div_rr_ratio_local5");
	test(equal_value_ratio(pos, SignPlus, 4, 3), "div_rr_ratio_local6");

	test_ratio_alloc(local, &left, SignMinus, 1, 1);
	test_ratio_alloc(local, &right, SignPlus, 3, 4);
	div_rr_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rr_ratio_local7");
	test(ratiop(pos), "div_rr_ratio_local8");
	test(equal_value_ratio(pos, SignMinus, 4, 3), "div_rr_ratio_local9");

	test_ratio_alloc(local, &left, SignMinus, 1, 6);
	test_ratio_alloc(local, &right, SignPlus, 4, 3);
	div_rr_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rr_ratio_local10");
	test(ratiop(pos), "div_rr_ratio_local11");
	test(equal_value_ratio(pos, SignMinus, 1, 8), "div_rr_ratio_local12");

	test_ratio_alloc(local, &left, SignMinus, 3, 4);
	test_ratio_alloc(local, &right, SignPlus, 1, 1);
	div_rr_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rr_ratio_local13");
	test(ratiop(pos), "div_rr_ratio_local14");
	test(equal_value_ratio(pos, SignMinus, 3, 4), "div_rr_ratio_local15");

	test_ratio_alloc(local, &left, SignMinus, 3, 4);
	test_ratio_alloc(local, &right, SignMinus, 1, 1);
	div_rr_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rr_ratio_local16");
	test(ratiop(pos), "div_rr_ratio_local17");
	test(equal_value_ratio(pos, SignPlus, 3, 4), "div_rr_ratio_local18");

	test_ratio_alloc(local, &left, SignMinus, 3, 4);
	test_ratio_alloc(local, &right, SignMinus, 1, 6);
	div_rr_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rr_ratio_local19");
	test(ratiop(pos), "div_rr_ratio_local20");
	test(equal_value_ratio(pos, SignPlus, 1, 8), "div_rr_ratio_local21");

	test_ratio_alloc(local, &left, SignPlus, 55, 6);
	test_ratio_alloc(local, &right, SignMinus, 35, 4);
	div_rr_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rr_ratio_local22");
	test(ratiop(pos), "div_rr_ratio_local23");
	test(equal_value_ratio(pos, SignMinus, 22, 21), "div_rr_ratio_local24");

	test_ratio_alloc(local, &left, SignPlus, 10, 7);
	test_ratio_alloc(local, &right, SignMinus, 2, 7);
	div_rr_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rr_ratio_local25");
	test(ratiop(pos), "div_rr_ratio_local26");
	test(equal_value_ratio(pos, SignMinus, 5, 1), "div_rr_ratio_local27");

	rollback_local(local, stack);

	RETURN;
}

static int test_div_rr_real_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 0, 6);
	test_ratio_alloc(local, &right, SignPlus, 3, 4);
	div_rr_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rr_real_local1");
	test(fixnump(pos), "div_rr_real_local2");
	test(RefFixnum(pos) == 0, "div_rr_real_local3");

	test_ratio_alloc(local, &left, SignPlus, 1, 1);
	test_ratio_alloc(local, &right, SignPlus, 3, 4);
	div_rr_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rr_real_local4");
	test(ratiop(pos), "div_rr_real_local5");
	test(equal_value_ratio(pos, SignPlus, 4, 3), "div_rr_real_local6");

	test_ratio_alloc(local, &left, SignMinus, 1, 1);
	test_ratio_alloc(local, &right, SignPlus, 3, 4);
	div_rr_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rr_real_local7");
	test(ratiop(pos), "div_rr_real_local8");
	test(equal_value_ratio(pos, SignMinus, 4, 3), "div_rr_real_local9");

	test_ratio_alloc(local, &left, SignMinus, 1, 6);
	test_ratio_alloc(local, &right, SignPlus, 4, 3);
	div_rr_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rr_real_local10");
	test(ratiop(pos), "div_rr_real_local11");
	test(equal_value_ratio(pos, SignMinus, 1, 8), "div_rr_real_local12");

	test_ratio_alloc(local, &left, SignMinus, 3, 4);
	test_ratio_alloc(local, &right, SignPlus, 1, 1);
	div_rr_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rr_real_local13");
	test(ratiop(pos), "div_rr_real_local14");
	test(equal_value_ratio(pos, SignMinus, 3, 4), "div_rr_real_local15");

	test_ratio_alloc(local, &left, SignMinus, 3, 4);
	test_ratio_alloc(local, &right, SignMinus, 1, 1);
	div_rr_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rr_real_local16");
	test(ratiop(pos), "div_rr_real_local17");
	test(equal_value_ratio(pos, SignPlus, 3, 4), "div_rr_real_local18");

	test_ratio_alloc(local, &left, SignMinus, 3, 4);
	test_ratio_alloc(local, &right, SignMinus, 1, 6);
	div_rr_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rr_real_local19");
	test(ratiop(pos), "div_rr_real_local20");
	test(equal_value_ratio(pos, SignPlus, 1, 8), "div_rr_real_local21");

	test_ratio_alloc(local, &left, SignPlus, 55, 6);
	test_ratio_alloc(local, &right, SignMinus, 35, 4);
	div_rr_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rr_real_local22");
	test(ratiop(pos), "div_rr_real_local23");
	test(equal_value_ratio(pos, SignMinus, 22, 21), "div_rr_real_local24");

	test_ratio_alloc(local, &left, SignPlus, 10, 7);
	test_ratio_alloc(local, &right, SignMinus, 2, 7);
	div_rr_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "div_rr_real_local25");
	test(fixnump(pos), "div_rr_real_local26");
	test(RefFixnum(pos) == -5, "div_rr_real_local27");

	rollback_local(local, stack);

	RETURN;
}

static int test_div_rr_real_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 0, 6);
	test_ratio_alloc(local, &right, SignPlus, 3, 4);
	div_rr_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_rr_real_common1");
	test(fixnump(pos), "div_rr_real_common2");
	test(RefFixnum(pos) == 0, "div_rr_real_common3");

	test_ratio_alloc(local, &left, SignPlus, 1, 1);
	test_ratio_alloc(local, &right, SignPlus, 3, 4);
	div_rr_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_rr_real_common4");
	test(ratiop(pos), "div_rr_real_common5");
	test(equal_value_ratio(pos, SignPlus, 4, 3), "div_rr_real_common6");

	test_ratio_alloc(local, &left, SignMinus, 1, 1);
	test_ratio_alloc(local, &right, SignPlus, 3, 4);
	div_rr_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_rr_real_common7");
	test(ratiop(pos), "div_rr_real_common8");
	test(equal_value_ratio(pos, SignMinus, 4, 3), "div_rr_real_common9");

	test_ratio_alloc(local, &left, SignMinus, 1, 6);
	test_ratio_alloc(local, &right, SignPlus, 4, 3);
	div_rr_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_rr_real_common10");
	test(ratiop(pos), "div_rr_real_common11");
	test(equal_value_ratio(pos, SignMinus, 1, 8), "div_rr_real_common12");

	test_ratio_alloc(local, &left, SignMinus, 3, 4);
	test_ratio_alloc(local, &right, SignPlus, 1, 1);
	div_rr_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_rr_real_common13");
	test(ratiop(pos), "div_rr_real_common14");
	test(equal_value_ratio(pos, SignMinus, 3, 4), "div_rr_real_common15");

	test_ratio_alloc(local, &left, SignMinus, 3, 4);
	test_ratio_alloc(local, &right, SignMinus, 1, 1);
	div_rr_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_rr_real_common16");
	test(ratiop(pos), "div_rr_real_common17");
	test(equal_value_ratio(pos, SignPlus, 3, 4), "div_rr_real_common18");

	test_ratio_alloc(local, &left, SignMinus, 3, 4);
	test_ratio_alloc(local, &right, SignMinus, 1, 6);
	div_rr_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_rr_real_common19");
	test(ratiop(pos), "div_rr_real_common20");
	test(equal_value_ratio(pos, SignPlus, 1, 8), "div_rr_real_common21");

	test_ratio_alloc(local, &left, SignPlus, 55, 6);
	test_ratio_alloc(local, &right, SignMinus, 35, 4);
	div_rr_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_rr_real_common22");
	test(ratiop(pos), "div_rr_real_common23");
	test(equal_value_ratio(pos, SignMinus, 22, 21), "div_rr_real_common24");

	test_ratio_alloc(local, &left, SignPlus, 10, 7);
	test_ratio_alloc(local, &right, SignMinus, 2, 7);
	div_rr_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "div_rr_real_common25");
	test(fixnump(pos), "div_rr_real_common26");
	test(RefFixnum(pos) == -5, "div_rr_real_common27");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  Main
 */
static int testbreak_ratio_multi(void)
{
	TestBreak(test_equal_rv_nosign);
	TestBreak(test_multi_rv_ratio);
	TestBreak(test_multi_rv_local);
	TestBreak(test_multi_rv_common);
	TestBreak(test_multi_rf_ratio_local);
	TestBreak(test_multi_rf_real_local);
	TestBreak(test_multi_rf_real_common);
	TestBreak(test_multi_rb_ratio);
	TestBreak(test_multi_rb_local);
	TestBreak(test_multi_rb_common);
	TestBreak(test_multi_rb_ratio_local);
	TestBreak(test_multi_rb_real_local);
	TestBreak(test_multi_rb_real_common);
	TestBreak(test_div_rb_ratio);
	TestBreak(test_div_rb_local);
	TestBreak(test_div_rb_common);
	TestBreak(test_multi_rr_ratio);
	TestBreak(test_multi_rr_local);
	TestBreak(test_multi_rr_common);
	TestBreak(test_inverse_ratio_p);
	TestBreak(test_multi_rr_ratio_local);
	TestBreak(test_multi_rr_real_local);
	TestBreak(test_multi_rr_real_common);
	/* division */
	TestBreak(test_inverse_value_ratio);
	TestBreak(test_inverse_value_local);
	TestBreak(test_inverse_value_common);
	TestBreak(test_div_rv_ratio);
	TestBreak(test_div_rv_local);
	TestBreak(test_div_rv_common);
	TestBreak(test_div_rf_ratio_local);
	TestBreak(test_div_rf_real_local);
	TestBreak(test_div_rf_real_common);
	TestBreak(test_ratio_sign_inverse_ratio);
	TestBreak(test_ratio_sign_inverse_local);
	TestBreak(test_ratio_sign_inverse_common);
	TestBreak(test_div_vr_ratio);
	TestBreak(test_div_vr_local);
	TestBreak(test_div_vr_common);
	TestBreak(test_div_fr_ratio_local);
	TestBreak(test_div_fr_real_local);
	TestBreak(test_div_fr_real_common);
	TestBreak(test_bignum_sign_inverse_ratio);
	TestBreak(test_bignum_sign_inverse_local);
	TestBreak(test_bignum_sign_inverse_common);
	TestBreak(test_div_rb_ratio_local);
	TestBreak(test_div_rb_real_local);
	TestBreak(test_div_rb_real_common);
	TestBreak(test_div_br_ratio);
	TestBreak(test_div_br_local);
	TestBreak(test_div_br_common);
	TestBreak(test_div_br_ratio_local);
	TestBreak(test_div_br_real_local);
	TestBreak(test_div_br_real_common);
	TestBreak(test_div_rr_ratio);
	TestBreak(test_div_rr_local);
	TestBreak(test_div_rr_common);
	TestBreak(test_div_bir_ratio);
	TestBreak(test_div_bir_local);
	TestBreak(test_div_bir_common);
	TestBreak(test_div_rr_ratio_local);
	TestBreak(test_div_rr_real_local);
	TestBreak(test_div_rr_real_common);

	return 0;
}

int test_ratio_multi(void)
{
	int result;
	lispcode code;
	Execute ptr;

	TITLE;

	freelisp();
	alloclisp(0, 0);
	lisp_info_enable = 1;
	ptr = Execute_Thread;
	begin_setjmp(ptr, &code);
	if (code_run_p(code)) {
		build_lisproot(ptr);
		build_constant();
		build_object();
		lisp_initialize = 1;
		result = testbreak_ratio_multi();
	}
	end_setjmp(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

