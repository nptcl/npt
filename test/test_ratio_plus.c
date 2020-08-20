#include "ratio_plus.c"
#include "constant.h"
#include "degrade.h"

/*
 *  calcuration
 */
static int test_sign_reverse_ratio_inplace(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	ratio_noreduction_value_local(local, &pos, SignPlus, 10, 20);
	sign_reverse_ratio_inplace(pos);
	test(equal_value_ratio(pos, SignMinus, 10, 20), "sign_reverse_ratio_inplace1");
	sign_reverse_ratio_inplace(pos);
	test(equal_value_ratio(pos, SignPlus, 10, 20), "sign_reverse_ratio_inplace2");

	rollback_local(local, stack);

	RETURN;
}

static void test_ratio_alloc(LocalRoot local,
		addr *ret, int sign, bigtype v1, bigtype v2)
{
	addr numer, denom;
	bignum_value_alloc(local, &numer, SignPlus, v1);
	bignum_value_alloc(local, &denom, SignPlus, v2);
	make_ratio_alloc(local, ret, sign, numer, denom);
}

static int test_sign_reverse_ratio_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, numer, denom, check;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &pos, SignPlus, 10, 20);
	test(GetStatusDynamic(pos), "sign_reverse_ratio_common1");
	sign_reverse_ratio_common(pos, &pos);
	test(! GetStatusDynamic(pos), "sign_reverse_ratio_common2");
	test(IsMinus(RefSignRatio(pos)), "sign_reverse_ratio_common3");
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	test(! GetStatusDynamic(numer), "sign_reverse_ratio_common4");
	test(! GetStatusDynamic(denom), "sign_reverse_ratio_common5");
	test(equal_value_bignum(numer, SignPlus, 10), "sign_reverse_ratio_common6");
	test(equal_value_bignum(denom, SignPlus, 20), "sign_reverse_ratio_common7");

	sign_reverse_ratio_common(pos, &check);
	test(pos != check, "sign_reverse_ratio_common8");
	test(! GetStatusDynamic(check), "sign_reverse_ratio_common9");
	test(IsPlus(RefSignRatio(check)), "sign_reverse_ratio_common10");
	GetNumerRatio(check, &pos);
	test(pos == numer, "sign_reverse_ratio_common11");
	GetDenomRatio(check, &pos);
	test(pos == denom, "sign_reverse_ratio_common12");

	rollback_local(local, stack);

	RETURN;
}

static int test_sign_reverse_ratio_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, numer, denom, check;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(NULL, &pos, SignPlus, 10, 20);
	test(! GetStatusDynamic(pos), "sign_reverse_ratio_local1");
	sign_reverse_ratio_local(local, pos, &pos);
	test(GetStatusDynamic(pos), "sign_reverse_ratio_local2");
	test(IsMinus(RefSignRatio(pos)), "sign_reverse_ratio_local3");
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	test(GetStatusDynamic(numer), "sign_reverse_ratio_local4");
	test(GetStatusDynamic(denom), "sign_reverse_ratio_local5");
	test(equal_value_bignum(numer, SignPlus, 10), "sign_reverse_ratio_local6");
	test(equal_value_bignum(denom, SignPlus, 20), "sign_reverse_ratio_local7");

	sign_reverse_ratio_local(local, pos, &check);
	test(pos != check, "sign_reverse_ratio_local8");
	test(GetStatusDynamic(check), "sign_reverse_ratio_local9");
	test(IsPlus(RefSignRatio(check)), "sign_reverse_ratio_local10");
	GetNumerRatio(check, &pos);
	test(pos == numer, "sign_reverse_ratio_local11");
	GetDenomRatio(check, &pos);
	test(pos == denom, "sign_reverse_ratio_local12");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_rv_data_ratio(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, check;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &pos, SignMinus, 21, 1);
	plus_rv_data_ratio(local, SignPlus, pos, 2, &check);
	test(GetStatusDynamic(check), "plus_rv_data_ratio1");
	test(ratiop(check), "plus_rv_data_ratio2");
	test(equal_value_ratio(check, SignPlus, 23, 1), "plus_rv_data_ratio3");

	plus_rv_data_ratio(local, SignMinus, pos, 3, &check);
	test(GetStatusDynamic(check), "plus_rv_data_ratio4");
	test(ratiop(check), "plus_rv_data_ratio5");
	test(equal_value_ratio(check, SignMinus, 24, 1), "plus_rv_data_ratio6");

	test_ratio_alloc(local, &pos, SignPlus, 4, 3);
	plus_rv_data_ratio(local, SignPlus, pos, 5, &check);
	test(GetStatusDynamic(check), "plus_rv_data_ratio7");
	test(ratiop(check), "plus_rv_data_ratio8");
	test(equal_value_ratio(check, SignPlus, 19, 3), "plus_rv_data_ratio9");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_rv_data_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, check;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &pos, SignMinus, 21, 1);
	plus_rv_data_local(local, SignPlus, pos, 2, &check);
	test(GetStatusDynamic(check), "plus_rv_data_local1");
	test(GetType(check) == LISPTYPE_FIXNUM, "plus_rv_data_local2");
	test(RefFixnum(check) == 23, "plus_rv_data_local3");

	plus_rv_data_local(local, SignMinus, pos, 3, &check);
	test(GetStatusDynamic(check), "plus_rv_data_local4");
	test(GetType(check) == LISPTYPE_FIXNUM, "plus_rv_data_local5");
	test(RefFixnum(check) == -24, "plus_rv_data_local6");

	test_ratio_alloc(local, &pos, SignPlus, 4, 3);
	plus_rv_data_local(local, SignPlus, pos, 5, &check);
	test(GetStatusDynamic(check), "plus_rv_data_local7");
	test(GetType(check) == LISPTYPE_RATIO, "plus_rv_data_local8");
	test(equal_value_ratio(check, SignPlus, 19, 3), "plus_rv_data_local9");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_rv_data_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, check;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &pos, SignMinus, 21, 1);
	plus_rv_data_common(local, SignPlus, pos, 2, &check);
	test(! GetStatusDynamic(check), "plus_rv_data_common1");
	test(GetType(check) == LISPTYPE_FIXNUM, "plus_rv_data_common2");
	test(RefFixnum(check) == 23, "plus_rv_data_common3");

	plus_rv_data_common(local, SignMinus, pos, 3, &check);
	test(! GetStatusDynamic(check), "plus_rv_data_common4");
	test(GetType(check) == LISPTYPE_FIXNUM, "plus_rv_data_common5");
	test(RefFixnum(check) == -24, "plus_rv_data_common6");

	test_ratio_alloc(local, &pos, SignPlus, 4, 3);
	plus_rv_data_common(local, SignPlus, pos, 5, &check);
	test(! GetStatusDynamic(check), "plus_rv_data_common7");
	test(GetType(check) == LISPTYPE_RATIO, "plus_rv_data_common8");
	test(equal_value_ratio(check, SignPlus, 19, 3), "plus_rv_data_common9");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_rv_data_ratio(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, check;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &pos, SignMinus, 21, 1);
	minus_rv_data_ratio(local, SignPlus, pos, 2, &check);
	test(GetStatusDynamic(check), "minus_rv_data_ratio1");
	test(ratiop(check), "minus_rv_data_ratio2");
	test(equal_value_ratio(check, SignPlus, 19, 1), "minus_rv_data_ratio3");

	minus_rv_data_ratio(local, SignMinus, pos, 3, &check);
	test(GetStatusDynamic(check), "minus_rv_data_ratio4");
	test(ratiop(check), "minus_rv_data_ratio5");
	test(equal_value_ratio(check, SignMinus, 18, 1), "minus_rv_data_ratio6");

	minus_rv_data_ratio(local, SignPlus, pos, 23, &check);
	test(GetStatusDynamic(check), "minus_rv_data_ratio7");
	test(ratiop(check), "minus_rv_data_ratio8");
	test(equal_value_ratio(check, SignMinus, 2, 1), "minus_rv_data_ratio9");

	minus_rv_data_ratio(local, SignMinus, pos, 24, &check);
	test(GetStatusDynamic(check), "minus_rv_data_ratio10");
	test(ratiop(check), "minus_rv_data_ratio11");
	test(equal_value_ratio(check, SignPlus, 3, 1), "minus_rv_data_ratio12");

	test_ratio_alloc(local, &pos, SignPlus, 4, 3);
	minus_rv_data_ratio(local, SignPlus, pos, 5, &check);
	test(GetStatusDynamic(check), "minus_rv_data_ratio13");
	test(ratiop(check), "minus_rv_data_ratio14");
	test(equal_value_ratio(check, SignMinus, 11, 3), "minus_rv_data_ratio15");

	test_ratio_alloc(local, &pos, SignPlus, 4, 3);
	minus_rv_data_ratio(local, SignMinus, pos, 5, &check);
	test(GetStatusDynamic(check), "minus_rv_data_ratio16");
	test(ratiop(check), "minus_rv_data_ratio17");
	test(equal_value_ratio(check, SignPlus, 11, 3), "minus_rv_data_ratio18");

	test_ratio_alloc(local, &pos, SignPlus, 19, 3);
	minus_rv_data_ratio(local, SignPlus, pos, 5, &check);
	test(GetStatusDynamic(check), "minus_rv_data_ratio19");
	test(ratiop(check), "minus_rv_data_ratio20");
	test(equal_value_ratio(check, SignPlus, 4, 3), "minus_rv_data_ratio21");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_rv_data_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, check;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &pos, SignMinus, 21, 1);
	minus_rv_data_local(local, SignPlus, pos, 2, &check);
	test(GetStatusDynamic(check), "minus_rv_data_local1");
	test(GetType(check) == LISPTYPE_FIXNUM, "minus_rv_data_local2");
	test(RefFixnum(check) == 19, "minus_rv_data_local3");

	minus_rv_data_local(local, SignMinus, pos, 3, &check);
	test(GetStatusDynamic(check), "minus_rv_data_local4");
	test(GetType(check) == LISPTYPE_FIXNUM, "minus_rv_data_local5");
	test(RefFixnum(check) == -18, "minus_rv_data_local6");

	minus_rv_data_local(local, SignPlus, pos, 23, &check);
	test(GetStatusDynamic(check), "minus_rv_data_local7");
	test(GetType(check) == LISPTYPE_FIXNUM, "minus_rv_data_local8");
	test(RefFixnum(check) == -2, "minus_rv_data_local9");

	minus_rv_data_local(local, SignMinus, pos, 24, &check);
	test(GetStatusDynamic(check), "minus_rv_data_local10");
	test(GetType(check) == LISPTYPE_FIXNUM, "minus_rv_data_local11");
	test(RefFixnum(check) == 3, "minus_rv_data_local12");

	test_ratio_alloc(local, &pos, SignPlus, 4, 3);
	minus_rv_data_local(local, SignPlus, pos, 5, &check);
	test(GetStatusDynamic(check), "minus_rv_data_local13");
	test(GetType(check) == LISPTYPE_RATIO, "minus_rv_data_local14");
	test(equal_value_ratio(check, SignMinus, 11, 3), "minus_rv_data_local15");

	test_ratio_alloc(local, &pos, SignPlus, 4, 3);
	minus_rv_data_local(local, SignMinus, pos, 5, &check);
	test(GetStatusDynamic(check), "minus_rv_data_local16");
	test(GetType(check) == LISPTYPE_RATIO, "minus_rv_data_local17");
	test(equal_value_ratio(check, SignPlus, 11, 3), "minus_rv_data_local18");

	test_ratio_alloc(local, &pos, SignPlus, 19, 3);
	minus_rv_data_local(local, SignPlus, pos, 5, &check);
	test(GetStatusDynamic(check), "minus_rv_data_local19");
	test(GetType(check) == LISPTYPE_RATIO, "minus_rv_data_local20");
	test(equal_value_ratio(check, SignPlus, 4, 3), "minus_rv_data_local21");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_rv_data_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, check;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &pos, SignMinus, 21, 1);
	minus_rv_data_common(local, SignPlus, pos, 2, &check);
	test(! GetStatusDynamic(check), "minus_rv_data_common1");
	test(GetType(check) == LISPTYPE_FIXNUM, "minus_rv_data_common2");
	test(RefFixnum(check) == 19, "minus_rv_data_common3");

	minus_rv_data_common(local, SignMinus, pos, 3, &check);
	test(! GetStatusDynamic(check), "minus_rv_data_common4");
	test(GetType(check) == LISPTYPE_FIXNUM, "minus_rv_data_common5");
	test(RefFixnum(check) == -18, "minus_rv_data_common6");

	minus_rv_data_common(local, SignPlus, pos, 23, &check);
	test(! GetStatusDynamic(check), "minus_rv_data_common7");
	test(GetType(check) == LISPTYPE_FIXNUM, "minus_rv_data_common8");
	test(RefFixnum(check) == -2, "minus_rv_data_common9");

	minus_rv_data_common(local, SignMinus, pos, 24, &check);
	test(! GetStatusDynamic(check), "minus_rv_data_common10");
	test(GetType(check) == LISPTYPE_FIXNUM, "minus_rv_data_common11");
	test(RefFixnum(check) == 3, "minus_rv_data_common12");

	test_ratio_alloc(local, &pos, SignPlus, 4, 3);
	minus_rv_data_common(local, SignPlus, pos, 5, &check);
	test(! GetStatusDynamic(check), "minus_rv_data_common13");
	test(GetType(check) == LISPTYPE_RATIO, "minus_rv_data_common14");
	test(equal_value_ratio(check, SignMinus, 11, 3), "minus_rv_data_common15");

	test_ratio_alloc(local, &pos, SignPlus, 4, 3);
	minus_rv_data_common(local, SignMinus, pos, 5, &check);
	test(! GetStatusDynamic(check), "minus_rv_data_common16");
	test(GetType(check) == LISPTYPE_RATIO, "minus_rv_data_common17");
	test(equal_value_ratio(check, SignPlus, 11, 3), "minus_rv_data_common18");

	test_ratio_alloc(local, &pos, SignPlus, 19, 3);
	minus_rv_data_common(local, SignPlus, pos, 5, &check);
	test(! GetStatusDynamic(check), "minus_rv_data_common19");
	test(GetType(check) == LISPTYPE_RATIO, "minus_rv_data_common20");
	test(equal_value_ratio(check, SignPlus, 4, 3), "minus_rv_data_common21");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_rv_ratio(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, check;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &pos, SignPlus, 4, 3);
	plus_rv_ratio(local, pos, SignPlus, 0, &check);
	test(GetStatusDynamic(check), "plus_rv_ratio1");
	test(equal_value_ratio(check, SignPlus, 4, 3), "plus_rv_ratio2");

	test_ratio_alloc(local, &pos, SignPlus, 4, 3);
	plus_rv_ratio(local, pos, SignPlus, 2, &check);
	test(GetStatusDynamic(check), "plus_rv_ratio3");
	test(equal_value_ratio(check, SignPlus, 10, 3), "plus_rv_ratio4");
	plus_rv_ratio(local, pos, SignMinus, 2, &check);
	test(GetStatusDynamic(check), "plus_rv_ratio5");
	test(equal_value_ratio(check, SignMinus, 2, 3), "plus_rv_ratio6");

	test_ratio_alloc(local, &pos, SignMinus, 4, 3);
	plus_rv_ratio(local, pos, SignPlus, 2, &check);
	test(GetStatusDynamic(check), "plus_rv_ratio7");
	test(equal_value_ratio(check, SignPlus, 2, 3), "plus_rv_ratio8");
	plus_rv_ratio(local, pos, SignMinus, 2, &check);
	test(GetStatusDynamic(check), "plus_rv_ratio9");
	test(equal_value_ratio(check, SignMinus, 10, 3), "plus_rv_ratio10");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_rv_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, check;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &pos, SignPlus, 4, 3);
	plus_rv_local(local, pos, SignPlus, 0, &check);
	test(GetStatusDynamic(check), "plus_rv_local1");
	test(equal_value_ratio(check, SignPlus, 4, 3), "plus_rv_local2");

	test_ratio_alloc(local, &pos, SignPlus, 4, 3);
	plus_rv_local(local, pos, SignPlus, 2, &check);
	test(GetStatusDynamic(check), "plus_rv_local3");
	test(equal_value_ratio(check, SignPlus, 10, 3), "plus_rv_local4");
	plus_rv_local(local, pos, SignMinus, 2, &check);
	test(GetStatusDynamic(check), "plus_rv_local5");
	test(equal_value_ratio(check, SignMinus, 2, 3), "plus_rv_local6");

	test_ratio_alloc(local, &pos, SignMinus, 4, 3);
	plus_rv_local(local, pos, SignPlus, 2, &check);
	test(GetStatusDynamic(check), "plus_rv_local7");
	test(equal_value_ratio(check, SignPlus, 2, 3), "plus_rv_local8");
	plus_rv_local(local, pos, SignMinus, 2, &check);
	test(GetStatusDynamic(check), "plus_rv_local9");
	test(equal_value_ratio(check, SignMinus, 10, 3), "plus_rv_local10");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_rv_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, check;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &pos, SignPlus, 4, 3);
	plus_rv_common(local, pos, SignPlus, 0, &check);
	test(! GetStatusDynamic(check), "plus_rv_common1");
	test(equal_value_ratio(check, SignPlus, 4, 3), "plus_rv_common2");

	test_ratio_alloc(local, &pos, SignPlus, 4, 3);
	plus_rv_common(local, pos, SignPlus, 2, &check);
	test(! GetStatusDynamic(check), "plus_rv_common3");
	test(equal_value_ratio(check, SignPlus, 10, 3), "plus_rv_common4");
	plus_rv_common(local, pos, SignMinus, 2, &check);
	test(! GetStatusDynamic(check), "plus_rv_common5");
	test(equal_value_ratio(check, SignMinus, 2, 3), "plus_rv_common6");

	test_ratio_alloc(local, &pos, SignMinus, 4, 3);
	plus_rv_common(local, pos, SignPlus, 2, &check);
	test(! GetStatusDynamic(check), "plus_rv_common7");
	test(equal_value_ratio(check, SignPlus, 2, 3), "plus_rv_common8");
	plus_rv_common(local, pos, SignMinus, 2, &check);
	test(! GetStatusDynamic(check), "plus_rv_common9");
	test(equal_value_ratio(check, SignMinus, 10, 3), "plus_rv_common10");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_rv_ratio_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, check;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 4, 3);
	plus_rv_ratio_local(local, left, 0, &check);
	test(GetStatusDynamic(check), "plus_rv_ratio_local1");
	test(equal_value_ratio(check, SignPlus, 4, 3), "plus_rv_ratio_local2");

	test_ratio_alloc(local, &left, SignPlus, 4, 3);
	plus_rv_ratio_local(local, left, 2, &check);
	test(GetStatusDynamic(check), "plus_rv_ratio_local3");
	test(equal_value_ratio(check, SignPlus, 10, 3), "plus_rv_ratio_local4");
	plus_rv_ratio_local(local, left, -2, &check);
	test(GetStatusDynamic(check), "plus_rv_ratio_local5");
	test(equal_value_ratio(check, SignMinus, 2, 3), "plus_rv_ratio_local6");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	plus_rv_ratio_local(local, left, 2, &check);
	test(GetStatusDynamic(check), "plus_rv_ratio_local7");
	test(equal_value_ratio(check, SignPlus, 2, 3), "plus_rv_ratio_local8");
	plus_rv_ratio_local(local, left, -2, &check);
	test(GetStatusDynamic(check), "plus_rv_ratio_local9");
	test(equal_value_ratio(check, SignMinus, 10, 3), "plus_rv_ratio_local10");

	test_ratio_alloc(local, &left, SignPlus, 1, 3);
	plus_rv_ratio_local(local, left, 10, &check);
	test(ratiop(check), "plus_rv_ratio_local11");
	test(GetStatusDynamic(check), "plus_rv_ratio_local12");
	test(equal_value_ratio(check, SignPlus, 31, 3), "plus_rv_ratio_local13");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_rv_real_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, check;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 4, 3);
	plus_rv_real_local(local, left, 0, &check);
	test(GetStatusDynamic(check), "plus_rv_real_local1");
	test(equal_value_ratio(check, SignPlus, 4, 3), "plus_rv_real_local2");

	test_ratio_alloc(local, &left, SignPlus, 4, 3);
	plus_rv_real_local(local, left, 2, &check);
	test(GetStatusDynamic(check), "plus_rv_real_local3");
	test(equal_value_ratio(check, SignPlus, 10, 3), "plus_rv_real_local4");
	plus_rv_real_local(local, left, -2, &check);
	test(GetStatusDynamic(check), "plus_rv_real_local5");
	test(equal_value_ratio(check, SignMinus, 2, 3), "plus_rv_real_local6");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	plus_rv_real_local(local, left, 2, &check);
	test(GetStatusDynamic(check), "plus_rv_real_local7");
	test(equal_value_ratio(check, SignPlus, 2, 3), "plus_rv_real_local8");
	plus_rv_real_local(local, left, -2, &check);
	test(GetStatusDynamic(check), "plus_rv_real_local9");
	test(equal_value_ratio(check, SignMinus, 10, 3), "plus_rv_real_local10");

	test_ratio_alloc(local, &left, SignPlus, 0, 3);
	plus_rv_real_local(local, left, 10, &check);
	test(fixnump(check), "plus_rv_real_local11");
	test(GetStatusDynamic(check), "plus_rv_real_local12");
	test(RefFixnum(check) == 10, "plus_rv_real_local13");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_rv_real_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, check;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 4, 3);
	plus_rv_real_common(local, left, 0, &check);
	test(! GetStatusDynamic(check), "plus_rv_real_common1");
	test(equal_value_ratio(check, SignPlus, 4, 3), "plus_rv_real_common2");

	test_ratio_alloc(local, &left, SignPlus, 4, 3);
	plus_rv_real_common(local, left, 2, &check);
	test(! GetStatusDynamic(check), "plus_rv_real_common3");
	test(equal_value_ratio(check, SignPlus, 10, 3), "plus_rv_real_common4");
	plus_rv_real_common(local, left, -2, &check);
	test(! GetStatusDynamic(check), "plus_rv_real_common5");
	test(equal_value_ratio(check, SignMinus, 2, 3), "plus_rv_real_common6");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	plus_rv_real_common(local, left, 2, &check);
	test(! GetStatusDynamic(check), "plus_rv_real_common7");
	test(equal_value_ratio(check, SignPlus, 2, 3), "plus_rv_real_common8");
	plus_rv_real_common(local, left, -2, &check);
	test(! GetStatusDynamic(check), "plus_rv_real_common9");
	test(equal_value_ratio(check, SignMinus, 10, 3), "plus_rv_real_common10");

	test_ratio_alloc(local, &left, SignPlus, 0, 3);
	plus_rv_real_common(local, left, 10, &check);
	test(fixnump(check), "plus_rv_real_common11");
	test(! GetStatusDynamic(check), "plus_rv_real_common12");
	test(RefFixnum(check) == 10, "plus_rv_real_common13");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_rf_ratio_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, check;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 4, 3);
	fixnum_alloc(local, &right, 0);
	plus_rf_ratio_local(local, left, right, &check);
	test(GetStatusDynamic(check), "plus_rf_ratio_local1");
	test(equal_value_ratio(check, SignPlus, 4, 3), "plus_rf_ratio_local2");

	test_ratio_alloc(local, &left, SignPlus, 4, 3);
	fixnum_alloc(local, &right, 2);
	plus_rf_ratio_local(local, left, right, &check);
	test(GetStatusDynamic(check), "plus_rf_ratio_local3");
	test(equal_value_ratio(check, SignPlus, 10, 3), "plus_rf_ratio_local4");
	fixnum_alloc(local, &right, -2);
	plus_rf_ratio_local(local, left, right, &check);
	test(GetStatusDynamic(check), "plus_rf_ratio_local5");
	test(equal_value_ratio(check, SignMinus, 2, 3), "plus_rf_ratio_local6");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	fixnum_alloc(local, &right, 2);
	plus_rf_ratio_local(local, left, right, &check);
	test(GetStatusDynamic(check), "plus_rf_ratio_local7");
	test(equal_value_ratio(check, SignPlus, 2, 3), "plus_rf_ratio_local8");
	fixnum_alloc(local, &right, -2);
	plus_rf_ratio_local(local, left, right, &check);
	test(GetStatusDynamic(check), "plus_rf_ratio_local9");
	test(equal_value_ratio(check, SignMinus, 10, 3), "plus_rf_ratio_local10");

	test_ratio_alloc(local, &left, SignPlus, 1, 3);
	fixnum_alloc(local, &right, -20);
	plus_rf_ratio_local(local, left, right, &check);
	test(ratiop(check), "plus_rf_ratio_local11");
	test(GetStatusDynamic(check), "plus_rf_ratio_local12");
	test(equal_value_ratio(check, SignMinus, 59, 3), "plus_rf_ratio_local13");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_rf_real_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, check;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 4, 3);
	fixnum_alloc(local, &right, 0);
	plus_rf_real_local(local, left, right, &check);
	test(GetStatusDynamic(check), "plus_rf_real_local1");
	test(equal_value_ratio(check, SignPlus, 4, 3), "plus_rf_real_local2");

	test_ratio_alloc(local, &left, SignPlus, 4, 3);
	fixnum_alloc(local, &right, 2);
	plus_rf_real_local(local, left, right, &check);
	test(GetStatusDynamic(check), "plus_rf_real_local3");
	test(equal_value_ratio(check, SignPlus, 10, 3), "plus_rf_real_local4");
	fixnum_alloc(local, &right, -2);
	plus_rf_real_local(local, left, right, &check);
	test(GetStatusDynamic(check), "plus_rf_real_local5");
	test(equal_value_ratio(check, SignMinus, 2, 3), "plus_rf_real_local6");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	fixnum_alloc(local, &right, 2);
	plus_rf_real_local(local, left, right, &check);
	test(GetStatusDynamic(check), "plus_rf_real_local7");
	test(equal_value_ratio(check, SignPlus, 2, 3), "plus_rf_real_local8");
	fixnum_alloc(local, &right, -2);
	plus_rf_real_local(local, left, right, &check);
	test(GetStatusDynamic(check), "plus_rf_real_local9");
	test(equal_value_ratio(check, SignMinus, 10, 3), "plus_rf_real_local10");

	test_ratio_alloc(local, &left, SignPlus, 0, 3);
	fixnum_alloc(local, &right, -20);
	plus_rf_real_local(local, left, right, &check);
	test(fixnump(check), "plus_rf_real_local11");
	test(GetStatusDynamic(check), "plus_rf_real_local12");
	test(RefFixnum(check) == -20, "plus_rf_real_local13");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_rf_real_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, check;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 4, 3);
	fixnum_alloc(local, &right, 0);
	plus_rf_real_common(local, left, right, &check);
	test(! GetStatusDynamic(check), "plus_rf_real_common1");
	test(equal_value_ratio(check, SignPlus, 4, 3), "plus_rf_real_common2");

	test_ratio_alloc(local, &left, SignPlus, 4, 3);
	fixnum_alloc(local, &right, 2);
	plus_rf_real_common(local, left, right, &check);
	test(! GetStatusDynamic(check), "plus_rf_real_common3");
	test(equal_value_ratio(check, SignPlus, 10, 3), "plus_rf_real_common4");
	fixnum_alloc(local, &right, -2);
	plus_rf_real_common(local, left, right, &check);
	test(! GetStatusDynamic(check), "plus_rf_real_common5");
	test(equal_value_ratio(check, SignMinus, 2, 3), "plus_rf_real_common6");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	fixnum_alloc(local, &right, 2);
	plus_rf_real_common(local, left, right, &check);
	test(! GetStatusDynamic(check), "plus_rf_real_common7");
	test(equal_value_ratio(check, SignPlus, 2, 3), "plus_rf_real_common8");
	fixnum_alloc(local, &right, -2);
	plus_rf_real_common(local, left, right, &check);
	test(! GetStatusDynamic(check), "plus_rf_real_common9");
	test(equal_value_ratio(check, SignMinus, 10, 3), "plus_rf_real_common10");

	test_ratio_alloc(local, &left, SignPlus, 0, 3);
	fixnum_alloc(local, &right, -20);
	plus_rf_real_common(local, left, right, &check);
	test(fixnump(check), "plus_rf_real_common11");
	test(! GetStatusDynamic(check), "plus_rf_real_common12");
	test(RefFixnum(check) == -20, "plus_rf_real_common13");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_rv_ratio(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, check;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 4, 3);
	minus_rv_ratio(local, left, SignPlus, 5, &check, 0);
	test(GetStatusDynamic(check), "minus_rv_ratio1");
	test(equal_value_ratio(check, SignMinus, 11, 3), "minus_rv_ratio2");
	minus_rv_ratio(local, left, SignPlus, 5, &check, 1);
	test(equal_value_ratio(check, SignPlus, 11, 3), "minus_rv_ratio3");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	minus_rv_ratio(local, left, SignPlus, 5, &check, 0);
	test(GetStatusDynamic(check), "minus_rv_ratio1");
	test(equal_value_ratio(check, SignMinus, 19, 3), "minus_rv_ratio4");
	minus_rv_ratio(local, left, SignPlus, 5, &check, 1);
	test(equal_value_ratio(check, SignPlus, 19, 3), "minus_rv_ratio5");

	test_ratio_alloc(local, &left, SignPlus, 4, 3);
	minus_rv_ratio(local, left, SignMinus, 5, &check, 0);
	test(GetStatusDynamic(check), "minus_rv_ratio1");
	test(equal_value_ratio(check, SignPlus, 19, 3), "minus_rv_ratio6");
	minus_rv_ratio(local, left, SignMinus, 5, &check, 1);
	test(equal_value_ratio(check, SignMinus, 19, 3), "minus_rv_ratio7");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	minus_rv_ratio(local, left, SignMinus, 5, &check, 0);
	test(GetStatusDynamic(check), "minus_rv_ratio1");
	test(equal_value_ratio(check, SignPlus, 11, 3), "minus_rv_ratio8");
	minus_rv_ratio(local, left, SignMinus, 5, &check, 1);
	test(equal_value_ratio(check, SignMinus, 11, 3), "minus_rv_ratio9");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_rv_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, check;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 4, 3);
	minus_rv_local(local, left, SignPlus, 5, &check, 0);
	test(GetStatusDynamic(check), "minus_rv_local1");
	test(equal_value_ratio(check, SignMinus, 11, 3), "minus_rv_local2");
	minus_rv_local(local, left, SignPlus, 5, &check, 1);
	test(equal_value_ratio(check, SignPlus, 11, 3), "minus_rv_local3");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	minus_rv_local(local, left, SignPlus, 5, &check, 0);
	test(GetStatusDynamic(check), "minus_rv_local1");
	test(equal_value_ratio(check, SignMinus, 19, 3), "minus_rv_local4");
	minus_rv_local(local, left, SignPlus, 5, &check, 1);
	test(equal_value_ratio(check, SignPlus, 19, 3), "minus_rv_local5");

	test_ratio_alloc(local, &left, SignPlus, 4, 3);
	minus_rv_local(local, left, SignMinus, 5, &check, 0);
	test(GetStatusDynamic(check), "minus_rv_local1");
	test(equal_value_ratio(check, SignPlus, 19, 3), "minus_rv_local6");
	minus_rv_local(local, left, SignMinus, 5, &check, 1);
	test(equal_value_ratio(check, SignMinus, 19, 3), "minus_rv_local7");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	minus_rv_local(local, left, SignMinus, 5, &check, 0);
	test(GetStatusDynamic(check), "minus_rv_local1");
	test(equal_value_ratio(check, SignPlus, 11, 3), "minus_rv_local8");
	minus_rv_local(local, left, SignMinus, 5, &check, 1);
	test(equal_value_ratio(check, SignMinus, 11, 3), "minus_rv_local9");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_rv_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, check;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 4, 3);
	minus_rv_common(local, left, SignPlus, 5, &check, 0);
	test(! GetStatusDynamic(check), "minus_rv_common1");
	test(equal_value_ratio(check, SignMinus, 11, 3), "minus_rv_common2");
	minus_rv_common(local, left, SignPlus, 5, &check, 1);
	test(equal_value_ratio(check, SignPlus, 11, 3), "minus_rv_common3");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	minus_rv_common(local, left, SignPlus, 5, &check, 0);
	test(! GetStatusDynamic(check), "minus_rv_common1");
	test(equal_value_ratio(check, SignMinus, 19, 3), "minus_rv_common4");
	minus_rv_common(local, left, SignPlus, 5, &check, 1);
	test(equal_value_ratio(check, SignPlus, 19, 3), "minus_rv_common5");

	test_ratio_alloc(local, &left, SignPlus, 4, 3);
	minus_rv_common(local, left, SignMinus, 5, &check, 0);
	test(! GetStatusDynamic(check), "minus_rv_common1");
	test(equal_value_ratio(check, SignPlus, 19, 3), "minus_rv_common6");
	minus_rv_common(local, left, SignMinus, 5, &check, 1);
	test(equal_value_ratio(check, SignMinus, 19, 3), "minus_rv_common7");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	minus_rv_common(local, left, SignMinus, 5, &check, 0);
	test(! GetStatusDynamic(check), "minus_rv_common1");
	test(equal_value_ratio(check, SignPlus, 11, 3), "minus_rv_common8");
	minus_rv_common(local, left, SignMinus, 5, &check, 1);
	test(equal_value_ratio(check, SignMinus, 11, 3), "minus_rv_common9");

	rollback_local(local, stack);

	RETURN;
}

static int test_cast_fixnum_ratio_local(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_local(local, &pos, 10);
	cast_fixnum_ratio_local(local, pos, &pos);
	test(ratiop(pos), "cast_fixnum_ratio_local1");
	test(GetStatusDynamic(pos), "cast_fixnum_ratio_local2");
	test(equal_value_ratio(pos, SignPlus, 10, 1), "cast_fixnum_ratio_local3");

	fixnum_local(local, &pos, -20);
	cast_fixnum_ratio_local(local, pos, &pos);
	test(ratiop(pos), "cast_fixnum_ratio_local4");
	test(GetStatusDynamic(pos), "cast_fixnum_ratio_local5");
	test(equal_value_ratio(pos, SignMinus, 20, 1), "cast_fixnum_ratio_local6");

	rollback_local(local, stack);

	RETURN;
}

static int test_cast_bignum_ratio_local(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &pos, SignPlus, 10);
	cast_bignum_ratio_local(local, pos, &pos);
	test(ratiop(pos), "cast_bignum_ratio_local1");
	test(GetStatusDynamic(pos), "cast_bignum_ratio_local2");
	test(equal_value_ratio(pos, SignPlus, 10, 1), "cast_bignum_ratio_local3");

	bignum_value_local(local, &pos, SignMinus, 20);
	cast_bignum_ratio_local(local, pos, &pos);
	test(ratiop(pos), "cast_bignum_ratio_local4");
	test(GetStatusDynamic(pos), "cast_bignum_ratio_local5");
	test(equal_value_ratio(pos, SignMinus, 20, 1), "cast_bignum_ratio_local6");

	rollback_local(local, stack);

	RETURN;
}

static int test_sigrev_fixnum_ratio_local(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_local(local, &pos, 10);
	sigrev_fixnum_ratio_local(local, pos, &pos);
	test(ratiop(pos), "sigrev_fixnum_ratio_local1");
	test(GetStatusDynamic(pos), "sigrev_fixnum_ratio_local2");
	test(equal_value_ratio(pos, SignMinus, 10, 1), "sigrev_fixnum_ratio_local3");

	fixnum_local(local, &pos, -20);
	sigrev_fixnum_ratio_local(local, pos, &pos);
	test(ratiop(pos), "sigrev_fixnum_ratio_local4");
	test(GetStatusDynamic(pos), "sigrev_fixnum_ratio_local5");
	test(equal_value_ratio(pos, SignPlus, 20, 1), "sigrev_fixnum_ratio_local6");

	rollback_local(local, stack);

	RETURN;
}

static int test_sigrev_bignum_ratio_local(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &pos, SignPlus, 10);
	sigrev_bignum_ratio_local(local, pos, &pos);
	test(ratiop(pos), "sigrev_bignum_ratio_local1");
	test(GetStatusDynamic(pos), "sigrev_bignum_ratio_local2");
	test(equal_value_ratio(pos, SignMinus, 10, 1), "sigrev_bignum_ratio_local3");

	bignum_value_local(local, &pos, SignMinus, 20);
	sigrev_bignum_ratio_local(local, pos, &pos);
	test(ratiop(pos), "sigrev_bignum_ratio_local4");
	test(GetStatusDynamic(pos), "sigrev_bignum_ratio_local5");
	test(equal_value_ratio(pos, SignPlus, 20, 1), "sigrev_bignum_ratio_local6");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_rf_ratio_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, check;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignMinus, 0, 3);
	fixnum_local(local, &right, 5);
	minus_rf_ratio_local(local, left, right, &check);
	test(ratiop(check), "minus_rf_ratio_local1");
	test(GetStatusDynamic(check), "minus_rf_ratio_local2");
	test(equal_value_ratio(check, SignMinus, 5, 1), "minus_rf_ratio_local3");

	test_ratio_alloc(local, &left, SignMinus, 1, 3);
	fixnum_local(local, &right, 5);
	minus_rf_ratio_local(local, left, right, &check);
	test(ratiop(check), "minus_rf_ratio_local4");
	test(GetStatusDynamic(check), "minus_rf_ratio_local5");
	test(equal_value_ratio(check, SignMinus, 16, 3), "minus_rf_ratio_local6");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	fixnum_local(local, &right, 0);
	minus_rf_ratio_local(local, left, right, &check);
	test(GetStatusDynamic(check), "minus_rf_ratio_local7");
	test(equal_value_ratio(check, SignMinus, 4, 3), "minus_rf_ratio_local8");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	fixnum_local(local, &right, 5);
	minus_rf_ratio_local(local, left, right, &check);
	test(GetStatusDynamic(check), "minus_rf_ratio_local9");
	test(equal_value_ratio(check, SignMinus, 19, 3), "minus_rf_ratio_local10");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_rf_real_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, check;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignMinus, 0, 3);
	fixnum_local(local, &right, 5);
	minus_rf_real_local(local, left, right, &check);
	test(fixnump(check), "minus_rf_real_local1");
	test(GetStatusDynamic(check), "minus_rf_real_local2");
	test(RefFixnum(check) == -5, "minus_rf_real_local3");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	fixnum_local(local, &right, 0);
	minus_rf_real_local(local, left, right, &check);
	test(GetStatusDynamic(check), "minus_rf_real_local4");
	test(equal_value_ratio(check, SignMinus, 4, 3), "minus_rf_real_local5");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	fixnum_local(local, &right, 5);
	minus_rf_real_local(local, left, right, &check);
	test(GetStatusDynamic(check), "minus_rf_real_local6");
	test(equal_value_ratio(check, SignMinus, 19, 3), "minus_rf_real_local7");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_rf_real_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, check;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignMinus, 0, 3);
	fixnum_local(local, &right, 5);
	minus_rf_real_common(local, left, right, &check);
	test(! GetStatusDynamic(check), "minus_rf_real_common1");
	test(RefFixnum(check) == -5, "minus_rf_real_common2");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	fixnum_local(local, &right, 0);
	minus_rf_real_common(local, left, right, &check);
	test(! GetStatusDynamic(check), "minus_rf_real_common3");
	test(equal_value_ratio(check, SignMinus, 4, 3), "minus_rf_real_common4");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	fixnum_local(local, &right, 5);
	minus_rf_real_common(local, left, right, &check);
	test(! GetStatusDynamic(check), "minus_rf_real_common5");
	test(equal_value_ratio(check, SignMinus, 19, 3), "minus_rf_real_common6");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_fr_ratio_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, check;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_local(local, &left, 0);
	test_ratio_alloc(local, &right, SignMinus, 4, 3);
	minus_fr_ratio_local(local, left, right, &check);
	test(GetStatusDynamic(check), "minus_fr_ratio_local1");
	test(equal_value_ratio(check, SignPlus, 4, 3), "minus_fr_ratio_local2");

	fixnum_local(local, &left, 5);
	test_ratio_alloc(local, &right, SignMinus, 0, 3);
	minus_fr_ratio_local(local, left, right, &check);
	test(GetStatusDynamic(check), "minus_fr_ratio_local3");
	test(equal_value_ratio(check, SignPlus, 5, 1), "minus_fr_ratio_local4");

	fixnum_local(local, &left, 5);
	test_ratio_alloc(local, &right, SignMinus, 4, 3);
	minus_fr_ratio_local(local, left, right, &check);
	test(GetStatusDynamic(check), "minus_fr_ratio_local5");
	test(equal_value_ratio(check, SignPlus, 19, 3), "minus_fr_ratio_local6");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_fr_real_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, check;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_local(local, &left, 0);
	test_ratio_alloc(local, &right, SignMinus, 4, 3);
	minus_fr_real_local(local, left, right, &check);
	test(GetStatusDynamic(check), "minus_fr_real_local1");
	test(equal_value_ratio(check, SignPlus, 4, 3), "minus_fr_real_local2");

	fixnum_local(local, &left, 5);
	test_ratio_alloc(local, &right, SignMinus, 0, 3);
	minus_fr_real_local(local, left, right, &check);
	test(GetStatusDynamic(check), "minus_fr_real_local3");
	test(RefFixnum(check) == 5, "minus_fr_real_local4");

	fixnum_local(local, &left, 5);
	test_ratio_alloc(local, &right, SignMinus, 4, 3);
	minus_fr_real_local(local, left, right, &check);
	test(GetStatusDynamic(check), "minus_fr_real_local5");
	test(equal_value_ratio(check, SignPlus, 19, 3), "minus_fr_real_local6");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_fr_real_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, check;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_local(local, &left, 0);
	test_ratio_alloc(local, &right, SignMinus, 4, 3);
	minus_fr_real_common(local, left, right, &check);
	test(! GetStatusDynamic(check), "minus_fr_real_common1");
	test(equal_value_ratio(check, SignPlus, 4, 3), "minus_fr_real_common2");

	fixnum_local(local, &left, 5);
	test_ratio_alloc(local, &right, SignMinus, 0, 3);
	minus_fr_real_common(local, left, right, &check);
	test(! GetStatusDynamic(check), "minus_fr_real_common3");
	test(RefFixnum(check) == 5, "minus_fr_real_common4");

	fixnum_local(local, &left, 5);
	test_ratio_alloc(local, &right, SignMinus, 4, 3);
	minus_fr_real_common(local, left, right, &check);
	test(! GetStatusDynamic(check), "minus_fr_real_common5");
	test(equal_value_ratio(check, SignPlus, 19, 3), "minus_fr_real_common6");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_rb_ratio(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, check, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 4, 1);
	bignum_value_alloc(local, &right, SignPlus, 3);
	plus_rb_ratio(local, SignPlus, left, right, &check);
	test(GetStatusDynamic(check), "plus_rb_ratio1");
	test(ratiop(check), "plus_rb_ratio2");
	test(equal_value_ratio(check, SignPlus, 7, 1), "plus_rb_ratio3");

	test_ratio_alloc(local, &left, SignPlus, 4, 1);
	bignum_value_alloc(local, &right, SignPlus, 3);
	plus_rb_ratio(local, SignMinus, left, right, &check);
	test(GetStatusDynamic(check), "plus_rb_ratio4");
	test(ratiop(check), "plus_rb_ratio5");
	test(equal_value_ratio(check, SignMinus, 7, 1), "plus_rb_ratio6");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	bignum_value_alloc(local, &right, SignPlus, 5);
	plus_rb_ratio(local, SignMinus, left, right, &check);
	test(GetStatusDynamic(check), "plus_rb_ratio7");
	test(ratiop(check), "plus_rb_ratio8");
	test(IsMinus(RefSignRatio(check)), "plus_rb_ratio9");
	GetNumerRatio(check, &pos);
	test(GetStatusDynamic(pos), "plus_rb_ratio10");
	test(equal_value_bignum(pos, SignPlus, 19), "plus_rb_ratio11");
	GetDenomRatio(check, &pos);
	test(GetStatusDynamic(pos), "plus_rb_ratio12");
	test(equal_value_bignum(pos, SignPlus, 3), "plus_rb_ratio13");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_rb_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, check, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 4, 1);
	bignum_value_alloc(local, &right, SignPlus, 3);
	plus_rb_local(local, SignPlus, left, right, &check);
	test(GetStatusDynamic(check), "plus_rb_local1");
	test(fixnump(check), "plus_rb_local2");
	test(RefFixnum(check) == 7, "plus_rb_local3");

	test_ratio_alloc(local, &left, SignPlus, 4, 1);
	bignum_value_alloc(local, &right, SignPlus, 3);
	plus_rb_local(local, SignMinus, left, right, &check);
	test(GetStatusDynamic(check), "plus_rb_local4");
	test(fixnump(check), "plus_rb_local5");
	test(RefFixnum(check) == -7, "plus_rb_local6");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	bignum_value_alloc(local, &right, SignPlus, 5);
	plus_rb_local(local, SignMinus, left, right, &check);
	test(GetStatusDynamic(check), "plus_rb_local7");
	test(ratiop(check), "plus_rb_local8");
	test(IsMinus(RefSignRatio(check)), "plus_rb_local9");
	GetNumerRatio(check, &pos);
	test(GetStatusDynamic(pos), "plus_rb_local10");
	test(equal_value_bignum(pos, SignPlus, 19), "plus_rb_local11");
	GetDenomRatio(check, &pos);
	test(GetStatusDynamic(pos), "plus_rb_local12");
	test(equal_value_bignum(pos, SignPlus, 3), "plus_rb_local13");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_rb_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, check, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 4, 1);
	bignum_value_alloc(local, &right, SignPlus, 3);
	plus_rb_common(local, SignPlus, left, right, &check);
	test(! GetStatusDynamic(check), "plus_rb_common1");
	test(fixnump(check), "plus_rb_common2");
	test(RefFixnum(check) == 7, "plus_rb_common3");

	test_ratio_alloc(local, &left, SignPlus, 4, 1);
	bignum_value_alloc(local, &right, SignPlus, 3);
	plus_rb_common(local, SignMinus, left, right, &check);
	test(! GetStatusDynamic(check), "plus_rb_common4");
	test(fixnump(check), "plus_rb_common5");
	test(RefFixnum(check) == -7, "plus_rb_common6");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	bignum_value_alloc(local, &right, SignPlus, 5);
	plus_rb_common(local, SignMinus, left, right, &check);
	test(! GetStatusDynamic(check), "plus_rb_common7");
	test(ratiop(check), "plus_rb_common8");
	test(IsMinus(RefSignRatio(check)), "plus_rb_common9");
	GetNumerRatio(check, &pos);
	test(! GetStatusDynamic(pos), "plus_rb_common10");
	test(equal_value_bignum(pos, SignPlus, 19), "plus_rb_common11");
	GetDenomRatio(check, &pos);
	test(! GetStatusDynamic(pos), "plus_rb_common12");
	test(equal_value_bignum(pos, SignPlus, 3), "plus_rb_common13");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_rb_ratio(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 4, 1);
	bignum_value_alloc(local, &right, SignPlus, 3);
	minus_rb_ratio(local, SignPlus, left, right, &pos, 0);
	test(GetStatusDynamic(pos), "minus_rb_ratio1");
	test(ratiop(pos), "minus_rb_ratio2");
	test(equal_value_ratio(pos, SignPlus, 1, 1), "minus_rb_ratio3");

	test_ratio_alloc(local, &left, SignPlus, 4, 1);
	bignum_value_alloc(local, &right, SignPlus, 3);
	minus_rb_ratio(local, SignMinus, left, right, &pos, 0);
	test(GetStatusDynamic(pos), "minus_rb_ratio4");
	test(ratiop(pos), "minus_rb_ratio5");
	test(equal_value_ratio(pos, SignMinus, 1, 1), "minus_rb_ratio6");

	test_ratio_alloc(local, &left, SignPlus, 3, 1);
	bignum_value_alloc(local, &right, SignPlus, 5);
	minus_rb_ratio(local, SignPlus, left, right, &pos, 0);
	test(GetStatusDynamic(pos), "minus_rb_ratio7");
	test(ratiop(pos), "minus_rb_ratio8");
	test(equal_value_ratio(pos, SignMinus, 2, 1), "minus_rb_ratio9");

	test_ratio_alloc(local, &left, SignPlus, 3, 1);
	bignum_value_alloc(local, &right, SignPlus, 5);
	minus_rb_ratio(local, SignMinus, left, right, &pos, 0);
	test(GetStatusDynamic(pos), "minus_rb_ratio10");
	test(ratiop(pos), "minus_rb_ratio11");
	test(equal_value_ratio(pos, SignPlus, 2, 1), "minus_rb_ratio12");

	test_ratio_alloc(local, &left, SignMinus, 19, 3);
	bignum_value_alloc(local, &right, SignPlus, 5);
	minus_rb_ratio(local, SignPlus, left, right, &pos, 0);
	test(GetStatusDynamic(pos), "minus_rb_ratio13");
	test(ratiop(pos), "minus_rb_ratio14");
	test(equal_value_ratio(pos, SignPlus, 4, 3), "minus_rb_ratio15");

	test_ratio_alloc(local, &left, SignMinus, 19, 3);
	bignum_value_alloc(local, &right, SignPlus, 5);
	minus_rb_ratio(local, SignMinus, left, right, &pos, 0);
	test(GetStatusDynamic(pos), "minus_rb_ratio16");
	test(ratiop(pos), "minus_rb_ratio17");
	test(equal_value_ratio(pos, SignMinus, 4, 3), "minus_rb_ratio18");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	bignum_value_alloc(local, &right, SignPlus, 5);
	minus_rb_ratio(local, SignPlus, left, right, &pos, 0);
	test(GetStatusDynamic(pos), "minus_rb_ratio19");
	test(ratiop(pos), "minus_rb_ratio20");
	test(equal_value_ratio(pos, SignMinus, 11, 3), "minus_rb_ratio21");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	bignum_value_alloc(local, &right, SignPlus, 5);
	minus_rb_ratio(local, SignMinus, left, right, &pos, 0);
	test(GetStatusDynamic(pos), "minus_rb_ratio22");
	test(ratiop(pos), "minus_rb_ratio23");
	test(equal_value_ratio(pos, SignPlus, 11, 3), "minus_rb_ratio24");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_rb_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 4, 1);
	bignum_value_alloc(local, &right, SignPlus, 3);
	minus_rb_local(local, SignPlus, left, right, &pos, 0);
	test(GetStatusDynamic(pos), "minus_rb_local1");
	test(fixnump(pos), "minus_rb_local2");
	test(RefFixnum(pos) == 1, "minus_rb_local3");

	test_ratio_alloc(local, &left, SignPlus, 4, 1);
	bignum_value_alloc(local, &right, SignPlus, 3);
	minus_rb_local(local, SignMinus, left, right, &pos, 0);
	test(GetStatusDynamic(pos), "minus_rb_local4");
	test(fixnump(pos), "minus_rb_local5");
	test(RefFixnum(pos) == -1, "minus_rb_local6");

	test_ratio_alloc(local, &left, SignPlus, 3, 1);
	bignum_value_alloc(local, &right, SignPlus, 5);
	minus_rb_local(local, SignPlus, left, right, &pos, 0);
	test(GetStatusDynamic(pos), "minus_rb_local7");
	test(fixnump(pos), "minus_rb_local8");
	test(RefFixnum(pos) == -2, "minus_rb_local9");

	test_ratio_alloc(local, &left, SignPlus, 3, 1);
	bignum_value_alloc(local, &right, SignPlus, 5);
	minus_rb_local(local, SignMinus, left, right, &pos, 0);
	test(GetStatusDynamic(pos), "minus_rb_local10");
	test(fixnump(pos), "minus_rb_local11");
	test(RefFixnum(pos) == 2, "minus_rb_local12");

	test_ratio_alloc(local, &left, SignMinus, 19, 3);
	bignum_value_alloc(local, &right, SignPlus, 5);
	minus_rb_local(local, SignPlus, left, right, &pos, 0);
	test(GetStatusDynamic(pos), "minus_rb_local13");
	test(ratiop(pos), "minus_rb_local14");
	test(equal_value_ratio(pos, SignPlus, 4, 3), "minus_rb_local15");

	test_ratio_alloc(local, &left, SignMinus, 19, 3);
	bignum_value_alloc(local, &right, SignPlus, 5);
	minus_rb_local(local, SignMinus, left, right, &pos, 0);
	test(GetStatusDynamic(pos), "minus_rb_local16");
	test(ratiop(pos), "minus_rb_local17");
	test(equal_value_ratio(pos, SignMinus, 4, 3), "minus_rb_local18");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	bignum_value_alloc(local, &right, SignPlus, 5);
	minus_rb_local(local, SignPlus, left, right, &pos, 0);
	test(GetStatusDynamic(pos), "minus_rb_local19");
	test(ratiop(pos), "minus_rb_local20");
	test(equal_value_ratio(pos, SignMinus, 11, 3), "minus_rb_local21");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	bignum_value_alloc(local, &right, SignPlus, 5);
	minus_rb_local(local, SignMinus, left, right, &pos, 0);
	test(GetStatusDynamic(pos), "minus_rb_local22");
	test(ratiop(pos), "minus_rb_local23");
	test(equal_value_ratio(pos, SignPlus, 11, 3), "minus_rb_local24");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_rb_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 4, 1);
	bignum_value_alloc(local, &right, SignPlus, 3);
	minus_rb_common(local, SignPlus, left, right, &pos, 0);
	test(! GetStatusDynamic(pos), "minus_rb_common1");
	test(fixnump(pos), "minus_rb_common2");
	test(RefFixnum(pos) == 1, "minus_rb_common3");

	test_ratio_alloc(local, &left, SignPlus, 4, 1);
	bignum_value_alloc(local, &right, SignPlus, 3);
	minus_rb_common(local, SignMinus, left, right, &pos, 0);
	test(! GetStatusDynamic(pos), "minus_rb_common4");
	test(fixnump(pos), "minus_rb_common5");
	test(RefFixnum(pos) == -1, "minus_rb_common6");

	test_ratio_alloc(local, &left, SignPlus, 3, 1);
	bignum_value_alloc(local, &right, SignPlus, 5);
	minus_rb_common(local, SignPlus, left, right, &pos, 0);
	test(! GetStatusDynamic(pos), "minus_rb_common7");
	test(fixnump(pos), "minus_rb_common8");
	test(RefFixnum(pos) == -2, "minus_rb_common9");

	test_ratio_alloc(local, &left, SignPlus, 3, 1);
	bignum_value_alloc(local, &right, SignPlus, 5);
	minus_rb_common(local, SignMinus, left, right, &pos, 0);
	test(! GetStatusDynamic(pos), "minus_rb_common10");
	test(fixnump(pos), "minus_rb_common11");
	test(RefFixnum(pos) == 2, "minus_rb_common12");

	test_ratio_alloc(local, &left, SignMinus, 19, 3);
	bignum_value_alloc(local, &right, SignPlus, 5);
	minus_rb_common(local, SignPlus, left, right, &pos, 0);
	test(! GetStatusDynamic(pos), "minus_rb_common13");
	test(ratiop(pos), "minus_rb_common14");
	test(equal_value_ratio(pos, SignPlus, 4, 3), "minus_rb_common15");

	test_ratio_alloc(local, &left, SignMinus, 19, 3);
	bignum_value_alloc(local, &right, SignPlus, 5);
	minus_rb_common(local, SignMinus, left, right, &pos, 0);
	test(! GetStatusDynamic(pos), "minus_rb_common16");
	test(ratiop(pos), "minus_rb_common17");
	test(equal_value_ratio(pos, SignMinus, 4, 3), "minus_rb_common18");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	bignum_value_alloc(local, &right, SignPlus, 5);
	minus_rb_common(local, SignPlus, left, right, &pos, 0);
	test(! GetStatusDynamic(pos), "minus_rb_common19");
	test(ratiop(pos), "minus_rb_common20");
	test(equal_value_ratio(pos, SignMinus, 11, 3), "minus_rb_common21");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	bignum_value_alloc(local, &right, SignPlus, 5);
	minus_rb_common(local, SignMinus, left, right, &pos, 0);
	test(! GetStatusDynamic(pos), "minus_rb_common22");
	test(ratiop(pos), "minus_rb_common23");
	test(equal_value_ratio(pos, SignPlus, 11, 3), "minus_rb_common24");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_rb_ratio_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignMinus, 0, 3);
	bignum_value_alloc(local, &right, SignPlus, 5);
	plus_rb_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "plus_rb_ratio_local1");
	test(equal_value_ratio(pos, SignPlus, 5, 1), "plus_rb_ratio_local2");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	bignum_value_alloc(local, &right, SignPlus, 0);
	plus_rb_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "plus_rb_ratio_local3");
	test(equal_value_ratio(pos, SignMinus, 4, 3), "plus_rb_ratio_local4");

	test_ratio_alloc(local, &left, SignPlus, 4, 3);
	bignum_value_alloc(local, &right, SignPlus, 5);
	plus_rb_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "plus_rb_ratio_local5");
	test(equal_value_ratio(pos, SignPlus, 19, 3), "plus_rb_ratio_local6");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	bignum_value_alloc(local, &right, SignPlus, 5);
	plus_rb_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "plus_rb_ratio_local7");
	test(equal_value_ratio(pos, SignPlus, 11, 3), "plus_rb_ratio_local8");

	test_ratio_alloc(local, &left, SignPlus, 4, 3);
	bignum_value_alloc(local, &right, SignMinus, 5);
	plus_rb_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "plus_rb_ratio_local9");
	test(equal_value_ratio(pos, SignMinus, 11, 3), "plus_rb_ratio_local10");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	bignum_value_alloc(local, &right, SignMinus, 5);
	plus_rb_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "plus_rb_ratio_local11");
	test(equal_value_ratio(pos, SignMinus, 19, 3), "plus_rb_ratio_local12");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_rb_real_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignMinus, 0, 3);
	bignum_value_alloc(local, &right, SignPlus, 5);
	plus_rb_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "plus_rb_real_local1");
	test(equal_value_bignum(pos, SignPlus, 5), "plus_rb_real_local2");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	bignum_value_alloc(local, &right, SignPlus, 0);
	plus_rb_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "plus_rb_real_local3");
	test(equal_value_ratio(pos, SignMinus, 4, 3), "plus_rb_real_local4");

	test_ratio_alloc(local, &left, SignPlus, 4, 3);
	bignum_value_alloc(local, &right, SignPlus, 5);
	plus_rb_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "plus_rb_real_local5");
	test(equal_value_ratio(pos, SignPlus, 19, 3), "plus_rb_real_local6");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	bignum_value_alloc(local, &right, SignPlus, 5);
	plus_rb_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "plus_rb_real_local7");
	test(equal_value_ratio(pos, SignPlus, 11, 3), "plus_rb_real_local8");

	test_ratio_alloc(local, &left, SignPlus, 4, 3);
	bignum_value_alloc(local, &right, SignMinus, 5);
	plus_rb_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "plus_rb_real_local9");
	test(equal_value_ratio(pos, SignMinus, 11, 3), "plus_rb_real_local10");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	bignum_value_alloc(local, &right, SignMinus, 5);
	plus_rb_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "plus_rb_real_local11");
	test(equal_value_ratio(pos, SignMinus, 19, 3), "plus_rb_real_local12");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_rb_real_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignMinus, 0, 3);
	bignum_value_alloc(local, &right, SignPlus, 5);
	plus_rb_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "plus_rb_real_common1");
	test(equal_value_bignum(pos, SignPlus, 5), "plus_rb_real_common2");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	bignum_value_alloc(local, &right, SignPlus, 0);
	plus_rb_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "plus_rb_real_common3");
	test(equal_value_ratio(pos, SignMinus, 4, 3), "plus_rb_real_common4");

	test_ratio_alloc(local, &left, SignPlus, 4, 3);
	bignum_value_alloc(local, &right, SignPlus, 5);
	plus_rb_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "plus_rb_real_common5");
	test(equal_value_ratio(pos, SignPlus, 19, 3), "plus_rb_real_common6");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	bignum_value_alloc(local, &right, SignPlus, 5);
	plus_rb_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "plus_rb_real_common7");
	test(equal_value_ratio(pos, SignPlus, 11, 3), "plus_rb_real_common8");

	test_ratio_alloc(local, &left, SignPlus, 4, 3);
	bignum_value_alloc(local, &right, SignMinus, 5);
	plus_rb_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "plus_rb_real_common9");
	test(equal_value_ratio(pos, SignMinus, 11, 3), "plus_rb_real_common10");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	bignum_value_alloc(local, &right, SignMinus, 5);
	plus_rb_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "plus_rb_real_common11");
	test(equal_value_ratio(pos, SignMinus, 19, 3), "plus_rb_real_common12");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_rb_ratio_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignMinus, 0, 3);
	bignum_value_alloc(local, &right, SignPlus, 5);
	minus_rb_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_rb_ratio_local1");
	test(equal_value_ratio(pos, SignMinus, 5, 1), "minus_rb_ratio_local2");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	bignum_value_alloc(local, &right, SignPlus, 0);
	minus_rb_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_rb_ratio_local3");
	test(equal_value_ratio(pos, SignMinus, 4, 3), "minus_rb_ratio_local4");

	test_ratio_alloc(local, &left, SignPlus, 4, 3);
	bignum_value_alloc(local, &right, SignPlus, 5);
	minus_rb_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_rb_ratio_local5");
	test(equal_value_ratio(pos, SignMinus, 11, 3), "minus_rb_ratio_local6");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	bignum_value_alloc(local, &right, SignPlus, 5);
	minus_rb_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_rb_ratio_local7");
	test(equal_value_ratio(pos, SignMinus, 19, 3), "minus_rb_ratio_local8");

	test_ratio_alloc(local, &left, SignPlus, 4, 3);
	bignum_value_alloc(local, &right, SignMinus, 5);
	minus_rb_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_rb_ratio_local9");
	test(equal_value_ratio(pos, SignPlus, 19, 3), "minus_rb_ratio_local10");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	bignum_value_alloc(local, &right, SignMinus, 5);
	minus_rb_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_rb_ratio_local11");
	test(equal_value_ratio(pos, SignPlus, 11, 3), "minus_rb_ratio_local12");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_rb_real_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignMinus, 0, 3);
	bignum_value_alloc(local, &right, SignPlus, 5);
	minus_rb_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_rb_real_local1");
	test(RefFixnum(pos) == -5, "minus_rb_real_local2");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	bignum_value_alloc(local, &right, SignPlus, 0);
	minus_rb_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_rb_real_local3");
	test(equal_value_ratio(pos, SignMinus, 4, 3), "minus_rb_real_local4");

	test_ratio_alloc(local, &left, SignPlus, 4, 3);
	bignum_value_alloc(local, &right, SignPlus, 5);
	minus_rb_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_rb_real_local5");
	test(equal_value_ratio(pos, SignMinus, 11, 3), "minus_rb_real_local6");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	bignum_value_alloc(local, &right, SignPlus, 5);
	minus_rb_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_rb_real_local7");
	test(equal_value_ratio(pos, SignMinus, 19, 3), "minus_rb_real_local8");

	test_ratio_alloc(local, &left, SignPlus, 4, 3);
	bignum_value_alloc(local, &right, SignMinus, 5);
	minus_rb_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_rb_real_local9");
	test(equal_value_ratio(pos, SignPlus, 19, 3), "minus_rb_real_local10");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	bignum_value_alloc(local, &right, SignMinus, 5);
	minus_rb_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_rb_real_local11");
	test(equal_value_ratio(pos, SignPlus, 11, 3), "minus_rb_real_local12");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_rb_real_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignMinus, 0, 3);
	bignum_value_alloc(local, &right, SignPlus, 5);
	minus_rb_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "minus_rb_real_common1");
	test(RefFixnum(pos) == -5, "minus_rb_real_common2");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	bignum_value_alloc(local, &right, SignPlus, 0);
	minus_rb_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "minus_rb_real_common3");
	test(equal_value_ratio(pos, SignMinus, 4, 3), "minus_rb_real_common4");

	test_ratio_alloc(local, &left, SignPlus, 4, 3);
	bignum_value_alloc(local, &right, SignPlus, 5);
	minus_rb_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "minus_rb_real_common5");
	test(equal_value_ratio(pos, SignMinus, 11, 3), "minus_rb_real_common6");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	bignum_value_alloc(local, &right, SignPlus, 5);
	minus_rb_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "minus_rb_real_common7");
	test(equal_value_ratio(pos, SignMinus, 19, 3), "minus_rb_real_common8");

	test_ratio_alloc(local, &left, SignPlus, 4, 3);
	bignum_value_alloc(local, &right, SignMinus, 5);
	minus_rb_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "minus_rb_real_common9");
	test(equal_value_ratio(pos, SignPlus, 19, 3), "minus_rb_real_common10");

	test_ratio_alloc(local, &left, SignMinus, 4, 3);
	bignum_value_alloc(local, &right, SignMinus, 5);
	minus_rb_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "minus_rb_real_common11");
	test(equal_value_ratio(pos, SignPlus, 11, 3), "minus_rb_real_common12");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_br_ratio_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_alloc(local, &left, SignPlus, 0);
	test_ratio_alloc(local, &right, SignMinus, 4, 3);
	minus_br_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_br_ratio_local1");
	test(equal_value_ratio(pos, SignPlus, 4, 3), "minus_br_ratio_local2");

	bignum_value_alloc(local, &left, SignPlus, 5);
	test_ratio_alloc(local, &right, SignMinus, 0, 3);
	minus_br_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_br_ratio_local3");
	test(equal_value_ratio(pos, SignPlus, 5, 1), "minus_br_ratio_local4");

	bignum_value_alloc(local, &left, SignPlus, 5);
	test_ratio_alloc(local, &right, SignPlus, 4, 3);
	minus_br_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_br_ratio_local5");
	test(equal_value_ratio(pos, SignPlus, 11, 3), "minus_br_ratio_local6");

	bignum_value_alloc(local, &left, SignPlus, 5);
	test_ratio_alloc(local, &right, SignMinus, 4, 3);
	minus_br_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_br_ratio_local7");
	test(equal_value_ratio(pos, SignPlus, 19, 3), "minus_br_ratio_local8");

	bignum_value_alloc(local, &left, SignMinus, 5);
	test_ratio_alloc(local, &right, SignPlus, 4, 3);
	minus_br_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_br_ratio_local9");
	test(equal_value_ratio(pos, SignMinus, 19, 3), "minus_br_ratio_local10");

	bignum_value_alloc(local, &left, SignMinus, 5);
	test_ratio_alloc(local, &right, SignMinus, 4, 3);
	minus_br_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_br_ratio_local11");
	test(equal_value_ratio(pos, SignMinus, 11, 3), "minus_br_ratio_local12");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_br_real_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_alloc(local, &left, SignPlus, 0);
	test_ratio_alloc(local, &right, SignMinus, 4, 3);
	minus_br_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_br_real_local1");
	test(equal_value_ratio(pos, SignPlus, 4, 3), "minus_br_real_local2");

	bignum_value_alloc(local, &left, SignPlus, 5);
	test_ratio_alloc(local, &right, SignMinus, 0, 3);
	minus_br_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_br_real_local3");
	test(equal_value_bignum(pos, SignPlus, 5), "minus_br_real_local4");

	bignum_value_alloc(local, &left, SignPlus, 5);
	test_ratio_alloc(local, &right, SignPlus, 4, 3);
	minus_br_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_br_real_local5");
	test(equal_value_ratio(pos, SignPlus, 11, 3), "minus_br_real_local6");

	bignum_value_alloc(local, &left, SignPlus, 5);
	test_ratio_alloc(local, &right, SignMinus, 4, 3);
	minus_br_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_br_real_local7");
	test(equal_value_ratio(pos, SignPlus, 19, 3), "minus_br_real_local8");

	bignum_value_alloc(local, &left, SignMinus, 5);
	test_ratio_alloc(local, &right, SignPlus, 4, 3);
	minus_br_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_br_real_local9");
	test(equal_value_ratio(pos, SignMinus, 19, 3), "minus_br_real_local10");

	bignum_value_alloc(local, &left, SignMinus, 5);
	test_ratio_alloc(local, &right, SignMinus, 4, 3);
	minus_br_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_br_real_local11");
	test(equal_value_ratio(pos, SignMinus, 11, 3), "minus_br_real_local12");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_br_real_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_alloc(local, &left, SignPlus, 0);
	test_ratio_alloc(local, &right, SignMinus, 4, 3);
	minus_br_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "minus_br_real_common1");
	test(equal_value_ratio(pos, SignPlus, 4, 3), "minus_br_real_common2");

	bignum_value_alloc(local, &left, SignPlus, 5);
	test_ratio_alloc(local, &right, SignMinus, 0, 3);
	minus_br_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "minus_br_real_common3");
	test(equal_value_bignum(pos, SignPlus, 5), "minus_br_real_common4");

	bignum_value_alloc(local, &left, SignPlus, 5);
	test_ratio_alloc(local, &right, SignPlus, 4, 3);
	minus_br_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "minus_br_real_common5");
	test(equal_value_ratio(pos, SignPlus, 11, 3), "minus_br_real_common6");

	bignum_value_alloc(local, &left, SignPlus, 5);
	test_ratio_alloc(local, &right, SignMinus, 4, 3);
	minus_br_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "minus_br_real_common7");
	test(equal_value_ratio(pos, SignPlus, 19, 3), "minus_br_real_common8");

	bignum_value_alloc(local, &left, SignMinus, 5);
	test_ratio_alloc(local, &right, SignPlus, 4, 3);
	minus_br_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "minus_br_real_common9");
	test(equal_value_ratio(pos, SignMinus, 19, 3), "minus_br_real_common10");

	bignum_value_alloc(local, &left, SignMinus, 5);
	test_ratio_alloc(local, &right, SignMinus, 4, 3);
	minus_br_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "minus_br_real_common11");
	test(equal_value_ratio(pos, SignMinus, 11, 3), "minus_br_real_common12");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_rr_data_ratio(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 3, 7);
	test_ratio_alloc(local, &right, SignPlus, 5, 7);
	plus_rr_data_ratio(local, SignMinus, left, right, &pos);
	test(GetStatusDynamic(pos), "plus_rr_data_ratio1");
	test(equal_value_ratio(pos, SignMinus, 8, 7), "plus_rr_data_ratio2");

	test_ratio_alloc(local, &left, SignPlus, 9, 7);
	test_ratio_alloc(local, &right, SignPlus, 5, 7);
	plus_rr_data_ratio(local, SignMinus, left, right, &pos);
	test(GetStatusDynamic(pos), "plus_rr_data_ratio3");
	test(ratiop(pos), "plus_rr_data_ratio4");
	test(equal_value_ratio(pos, SignMinus, 2, 1), "plus_rr_data_ratio5");

	/* (+ 7/6 9/4) -> 41/12 */
	test_ratio_alloc(local, &left, SignPlus, 7, 6);
	test_ratio_alloc(local, &right, SignPlus, 9, 4);
	plus_rr_data_ratio(local, SignMinus, left, right, &pos);
	test(GetStatusDynamic(pos), "plus_rr_data_ratio6");
	test(ratiop(pos), "plus_rr_data_ratio7");
	test(equal_value_ratio(pos, SignMinus, 41, 12), "plus_rr_data_ratio8");

	/* (+ 7/10 7/6) -> 28/15 */
	test_ratio_alloc(local, &left, SignPlus, 7, 10);
	test_ratio_alloc(local, &right, SignPlus, 7, 6);
	plus_rr_data_ratio(local, SignPlus, left, right, &pos);
	test(GetStatusDynamic(pos), "plus_rr_data_ratio9");
	test(ratiop(pos), "plus_rr_data_ratio10");
	test(equal_value_ratio(pos, SignPlus, 28, 15), "plus_rr_data_ratio11");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_rr_data_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 3, 7);
	test_ratio_alloc(local, &right, SignPlus, 5, 7);
	plus_rr_data_local(local, SignMinus, left, right, &pos);
	test(GetStatusDynamic(pos), "plus_rr_data_local1");
	test(equal_value_ratio(pos, SignMinus, 8, 7), "plus_rr_data_local2");

	test_ratio_alloc(local, &left, SignPlus, 9, 7);
	test_ratio_alloc(local, &right, SignPlus, 5, 7);
	plus_rr_data_local(local, SignMinus, left, right, &pos);
	test(GetStatusDynamic(pos), "plus_rr_data_local3");
	test(GetType(pos) == LISPTYPE_FIXNUM, "plus_rr_data_local4");
	test(RefFixnum(pos) == -2, "plus_rr_data_local5");

	/* (+ 7/6 9/4) -> 41/12 */
	test_ratio_alloc(local, &left, SignPlus, 7, 6);
	test_ratio_alloc(local, &right, SignPlus, 9, 4);
	plus_rr_data_local(local, SignMinus, left, right, &pos);
	test(GetStatusDynamic(pos), "plus_rr_data_local6");
	test(ratiop(pos), "plus_rr_data_local7");
	test(equal_value_ratio(pos, SignMinus, 41, 12), "plus_rr_data_local8");

	/* (+ 7/10 7/6) -> 28/15 */
	test_ratio_alloc(local, &left, SignPlus, 7, 10);
	test_ratio_alloc(local, &right, SignPlus, 7, 6);
	plus_rr_data_local(local, SignPlus, left, right, &pos);
	test(GetStatusDynamic(pos), "plus_rr_data_local9");
	test(ratiop(pos), "plus_rr_data_local10");
	test(equal_value_ratio(pos, SignPlus, 28, 15), "plus_rr_data_local11");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_rr_data_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 3, 7);
	test_ratio_alloc(local, &right, SignPlus, 5, 7);
	plus_rr_data_common(local, SignMinus, left, right, &pos);
	test(! GetStatusDynamic(pos), "plus_rr_data_common1");
	test(equal_value_ratio(pos, SignMinus, 8, 7), "plus_rr_data_common2");

	test_ratio_alloc(local, &left, SignPlus, 9, 7);
	test_ratio_alloc(local, &right, SignPlus, 5, 7);
	plus_rr_data_common(local, SignMinus, left, right, &pos);
	test(! GetStatusDynamic(pos), "plus_rr_data_common3");
	test(GetType(pos) == LISPTYPE_FIXNUM, "plus_rr_data_common4");
	test(RefFixnum(pos) == -2, "plus_rr_data_common5");

	/* (+ 7/6 9/4) -> 41/12 */
	test_ratio_alloc(local, &left, SignPlus, 7, 6);
	test_ratio_alloc(local, &right, SignPlus, 9, 4);
	plus_rr_data_common(local, SignMinus, left, right, &pos);
	test(! GetStatusDynamic(pos), "plus_rr_data_common6");
	test(ratiop(pos), "plus_rr_data_common7");
	test(equal_value_ratio(pos, SignMinus, 41, 12), "plus_rr_data_common8");

	/* (+ 7/10 7/6) -> 28/15 */
	test_ratio_alloc(local, &left, SignPlus, 7, 10);
	test_ratio_alloc(local, &right, SignPlus, 7, 6);
	plus_rr_data_common(local, SignPlus, left, right, &pos);
	test(! GetStatusDynamic(pos), "plus_rr_data_common9");
	test(ratiop(pos), "plus_rr_data_common10");
	test(equal_value_ratio(pos, SignPlus, 28, 15), "plus_rr_data_common11");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_rr_data_ratio(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 3, 7);
	test_ratio_alloc(local, &right, SignPlus, 5, 7);
	minus_rr_data_ratio(local, SignPlus, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_rr_data_ratio1");
	test(equal_value_ratio(pos, SignMinus, 2, 7), "minus_rr_data_ratio2");

	test_ratio_alloc(local, &left, SignPlus, 5, 7);
	test_ratio_alloc(local, &right, SignPlus, 4, 7);
	minus_rr_data_ratio(local, SignPlus, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_rr_data_ratio3");
	test(equal_value_ratio(pos, SignPlus, 1, 7), "minus_rr_data_ratio4");

	test_ratio_alloc(local, &left, SignPlus, 15, 7);
	test_ratio_alloc(local, &right, SignPlus, 1, 7);
	minus_rr_data_ratio(local, SignMinus, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_rr_data_ratio5");
	test(ratiop(pos), "minus_rr_data_ratio6");
	test(equal_value_ratio(pos, SignMinus, 2, 1), "minus_rr_data_ratio7");

	test_ratio_alloc(local, &left, SignPlus, 1, 7);
	test_ratio_alloc(local, &right, SignPlus, 15, 7);
	minus_rr_data_ratio(local, SignMinus, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_rr_data_ratio8");
	test(ratiop(pos), "minus_rr_data_ratio9");
	test(equal_value_ratio(pos, SignPlus, 2, 1), "minus_rr_data_ratio10");

	/* (- 7/6 9/4) -> -13/12 */
	test_ratio_alloc(local, &left, SignPlus, 7, 6);
	test_ratio_alloc(local, &right, SignPlus, 9, 4);
	minus_rr_data_ratio(local, SignMinus, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_rr_data_ratio11");
	test(ratiop(pos), "minus_rr_data_ratio12");
	test(equal_value_ratio(pos, SignPlus, 13, 12), "minus_rr_data_ratio13");

	test_ratio_alloc(local, &left, SignPlus, 9, 4);
	test_ratio_alloc(local, &right, SignPlus, 7, 6);
	minus_rr_data_ratio(local, SignMinus, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_rr_data_ratio14");
	test(ratiop(pos), "minus_rr_data_ratio15");
	test(equal_value_ratio(pos, SignMinus, 13, 12), "minus_rr_data_ratio16");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_rr_data_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 3, 7);
	test_ratio_alloc(local, &right, SignPlus, 5, 7);
	minus_rr_data_local(local, SignPlus, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_rr_data_local1");
	test(equal_value_ratio(pos, SignMinus, 2, 7), "minus_rr_data_local2");

	test_ratio_alloc(local, &left, SignPlus, 5, 7);
	test_ratio_alloc(local, &right, SignPlus, 4, 7);
	minus_rr_data_local(local, SignPlus, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_rr_data_local3");
	test(equal_value_ratio(pos, SignPlus, 1, 7), "minus_rr_data_local4");

	test_ratio_alloc(local, &left, SignPlus, 15, 7);
	test_ratio_alloc(local, &right, SignPlus, 1, 7);
	minus_rr_data_local(local, SignMinus, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_rr_data_local5");
	test(fixnump(pos), "minus_rr_data_local6");
	test(RefFixnum(pos) == -2, "minus_rr_data_local7");

	test_ratio_alloc(local, &left, SignPlus, 1, 7);
	test_ratio_alloc(local, &right, SignPlus, 15, 7);
	minus_rr_data_local(local, SignMinus, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_rr_data_local8");
	test(fixnump(pos), "minus_rr_data_local9");
	test(RefFixnum(pos) == 2, "minus_rr_data_local10");

	/* (- 7/6 9/4) -> -13/12 */
	test_ratio_alloc(local, &left, SignPlus, 7, 6);
	test_ratio_alloc(local, &right, SignPlus, 9, 4);
	minus_rr_data_local(local, SignMinus, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_rr_data_local11");
	test(ratiop(pos), "minus_rr_data_local12");
	test(equal_value_ratio(pos, SignPlus, 13, 12), "minus_rr_data_local13");

	test_ratio_alloc(local, &left, SignPlus, 9, 4);
	test_ratio_alloc(local, &right, SignPlus, 7, 6);
	minus_rr_data_local(local, SignMinus, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_rr_data_local14");
	test(ratiop(pos), "minus_rr_data_local15");
	test(equal_value_ratio(pos, SignMinus, 13, 12), "minus_rr_data_local16");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_rr_data_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 3, 7);
	test_ratio_alloc(local, &right, SignPlus, 5, 7);
	minus_rr_data_common(local, SignPlus, left, right, &pos);
	test(! GetStatusDynamic(pos), "minus_rr_data_common1");
	test(equal_value_ratio(pos, SignMinus, 2, 7), "minus_rr_data_common2");

	test_ratio_alloc(local, &left, SignPlus, 5, 7);
	test_ratio_alloc(local, &right, SignPlus, 4, 7);
	minus_rr_data_common(local, SignPlus, left, right, &pos);
	test(! GetStatusDynamic(pos), "minus_rr_data_common3");
	test(equal_value_ratio(pos, SignPlus, 1, 7), "minus_rr_data_common4");

	test_ratio_alloc(local, &left, SignPlus, 15, 7);
	test_ratio_alloc(local, &right, SignPlus, 1, 7);
	minus_rr_data_common(local, SignMinus, left, right, &pos);
	test(! GetStatusDynamic(pos), "minus_rr_data_common5");
	test(fixnump(pos), "minus_rr_data_common6");
	test(RefFixnum(pos) == -2, "minus_rr_data_common7");

	test_ratio_alloc(local, &left, SignPlus, 1, 7);
	test_ratio_alloc(local, &right, SignPlus, 15, 7);
	minus_rr_data_common(local, SignMinus, left, right, &pos);
	test(! GetStatusDynamic(pos), "minus_rr_data_common8");
	test(fixnump(pos), "minus_rr_data_common9");
	test(RefFixnum(pos) == 2, "minus_rr_data_common10");

	/* (- 7/6 9/4) -> -13/12 */
	test_ratio_alloc(local, &left, SignPlus, 7, 6);
	test_ratio_alloc(local, &right, SignPlus, 9, 4);
	minus_rr_data_common(local, SignMinus, left, right, &pos);
	test(! GetStatusDynamic(pos), "minus_rr_data_common11");
	test(ratiop(pos), "minus_rr_data_common12");
	test(equal_value_ratio(pos, SignPlus, 13, 12), "minus_rr_data_common13");

	test_ratio_alloc(local, &left, SignPlus, 9, 4);
	test_ratio_alloc(local, &right, SignPlus, 7, 6);
	minus_rr_data_common(local, SignMinus, left, right, &pos);
	test(! GetStatusDynamic(pos), "minus_rr_data_common14");
	test(ratiop(pos), "minus_rr_data_common15");
	test(equal_value_ratio(pos, SignMinus, 13, 12), "minus_rr_data_common16");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_rr_ratio_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 0, 2);
	test_ratio_alloc(local, &right, SignPlus, 7, 6);
	plus_rr_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "plus_rr_ratio_local1");
	test(ratiop(pos), "plus_rr_ratio_local2");
	test(equal_value_ratio(pos, SignPlus, 7, 6), "plus_rr_ratio_local3");

	plus_rr_ratio_local(local, right, left, &pos);
	test(GetStatusDynamic(pos), "plus_rr_ratio_local4");
	test(ratiop(pos), "plus_rr_ratio_local5");
	test(equal_value_ratio(pos, SignPlus, 7, 6), "plus_rr_ratio_local6");

	/* (+ 4/7 7/6) 73/42 */
	/* (- 4/7 7/6) -25/42 */
	test_ratio_alloc(local, &left, SignPlus, 4, 7);
	test_ratio_alloc(local, &right, SignPlus, 7, 6);
	plus_rr_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "plus_rr_ratio_local7");
	test(ratiop(pos), "plus_rr_ratio_local8");
	test(equal_value_ratio(pos, SignPlus, 73, 42), "plus_rr_ratio_local9");

	test_ratio_alloc(local, &left, SignMinus, 4, 7);
	test_ratio_alloc(local, &right, SignPlus, 7, 6);
	plus_rr_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "plus_rr_ratio_local10");
	test(ratiop(pos), "plus_rr_ratio_local11");
	test(equal_value_ratio(pos, SignPlus, 25, 42), "plus_rr_ratio_local12");

	test_ratio_alloc(local, &left, SignPlus, 4, 7);
	test_ratio_alloc(local, &right, SignMinus, 7, 6);
	plus_rr_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "plus_rr_ratio_local13");
	test(ratiop(pos), "plus_rr_ratio_local14");
	test(equal_value_ratio(pos, SignMinus, 25, 42), "plus_rr_ratio_local15");

	test_ratio_alloc(local, &left, SignMinus, 4, 7);
	test_ratio_alloc(local, &right, SignMinus, 7, 6);
	plus_rr_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "plus_rr_ratio_local16");
	test(ratiop(pos), "plus_rr_ratio_local17");
	test(equal_value_ratio(pos, SignMinus, 73, 42), "plus_rr_ratio_local18");

	test_ratio_alloc(local, &left, SignMinus, 11, 4);
	test_ratio_alloc(local, &right, SignMinus, 1, 4);
	plus_rr_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "plus_rr_ratio_local19");
	test(ratiop(pos), "plus_rr_ratio_local20");
	test(equal_value_ratio(pos, SignMinus, 3, 1), "plus_rr_ratio_local21");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_rr_real_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 0, 2);
	test_ratio_alloc(local, &right, SignPlus, 7, 6);
	plus_rr_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "plus_rr_real_local1");
	test(ratiop(pos), "plus_rr_real_local2");
	test(equal_value_ratio(pos, SignPlus, 7, 6), "plus_rr_real_local3");

	plus_rr_real_local(local, right, left, &pos);
	test(GetStatusDynamic(pos), "plus_rr_real_local4");
	test(ratiop(pos), "plus_rr_real_local5");
	test(equal_value_ratio(pos, SignPlus, 7, 6), "plus_rr_real_local6");

	/* (+ 4/7 7/6) 73/42 */
	/* (- 4/7 7/6) -25/42 */
	test_ratio_alloc(local, &left, SignPlus, 4, 7);
	test_ratio_alloc(local, &right, SignPlus, 7, 6);
	plus_rr_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "plus_rr_real_local7");
	test(ratiop(pos), "plus_rr_real_local8");
	test(equal_value_ratio(pos, SignPlus, 73, 42), "plus_rr_real_local9");

	test_ratio_alloc(local, &left, SignMinus, 4, 7);
	test_ratio_alloc(local, &right, SignPlus, 7, 6);
	plus_rr_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "plus_rr_real_local10");
	test(ratiop(pos), "plus_rr_real_local11");
	test(equal_value_ratio(pos, SignPlus, 25, 42), "plus_rr_real_local12");

	test_ratio_alloc(local, &left, SignPlus, 4, 7);
	test_ratio_alloc(local, &right, SignMinus, 7, 6);
	plus_rr_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "plus_rr_real_local13");
	test(ratiop(pos), "plus_rr_real_local14");
	test(equal_value_ratio(pos, SignMinus, 25, 42), "plus_rr_real_local15");

	test_ratio_alloc(local, &left, SignMinus, 4, 7);
	test_ratio_alloc(local, &right, SignMinus, 7, 6);
	plus_rr_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "plus_rr_real_local16");
	test(ratiop(pos), "plus_rr_real_local17");
	test(equal_value_ratio(pos, SignMinus, 73, 42), "plus_rr_real_local18");

	test_ratio_alloc(local, &left, SignMinus, 11, 4);
	test_ratio_alloc(local, &right, SignMinus, 1, 4);
	plus_rr_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "plus_rr_real_local19");
	test(fixnump(pos), "plus_rr_real_local20");
	test(RefFixnum(pos) == -3, "plus_rr_real_local21");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_rr_real_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 0, 2);
	test_ratio_alloc(local, &right, SignPlus, 7, 6);
	plus_rr_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "plus_rr_real_common1");
	test(ratiop(pos), "plus_rr_real_common2");
	test(equal_value_ratio(pos, SignPlus, 7, 6), "plus_rr_real_common3");

	plus_rr_real_common(local, right, left, &pos);
	test(! GetStatusDynamic(pos), "plus_rr_real_common4");
	test(ratiop(pos), "plus_rr_real_common5");
	test(equal_value_ratio(pos, SignPlus, 7, 6), "plus_rr_real_common6");

	/* (+ 4/7 7/6) 73/42 */
	/* (- 4/7 7/6) -25/42 */
	test_ratio_alloc(local, &left, SignPlus, 4, 7);
	test_ratio_alloc(local, &right, SignPlus, 7, 6);
	plus_rr_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "plus_rr_real_common7");
	test(ratiop(pos), "plus_rr_real_common8");
	test(equal_value_ratio(pos, SignPlus, 73, 42), "plus_rr_real_common9");

	test_ratio_alloc(local, &left, SignMinus, 4, 7);
	test_ratio_alloc(local, &right, SignPlus, 7, 6);
	plus_rr_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "plus_rr_real_common10");
	test(ratiop(pos), "plus_rr_real_common11");
	test(equal_value_ratio(pos, SignPlus, 25, 42), "plus_rr_real_common12");

	test_ratio_alloc(local, &left, SignPlus, 4, 7);
	test_ratio_alloc(local, &right, SignMinus, 7, 6);
	plus_rr_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "plus_rr_real_common13");
	test(ratiop(pos), "plus_rr_real_common14");
	test(equal_value_ratio(pos, SignMinus, 25, 42), "plus_rr_real_common15");

	test_ratio_alloc(local, &left, SignMinus, 4, 7);
	test_ratio_alloc(local, &right, SignMinus, 7, 6);
	plus_rr_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "plus_rr_real_common16");
	test(ratiop(pos), "plus_rr_real_common17");
	test(equal_value_ratio(pos, SignMinus, 73, 42), "plus_rr_real_common18");

	test_ratio_alloc(local, &left, SignMinus, 11, 4);
	test_ratio_alloc(local, &right, SignMinus, 1, 4);
	plus_rr_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "plus_rr_real_common19");
	test(fixnump(pos), "plus_rr_real_common20");
	test(RefFixnum(pos) == -3, "plus_rr_real_common21");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_rr_ratio_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 0, 2);
	test_ratio_alloc(local, &right, SignPlus, 7, 6);
	minus_rr_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_rr_ratio_local1");
	test(ratiop(pos), "minus_rr_ratio_local2");
	test(equal_value_ratio(pos, SignMinus, 7, 6), "minus_rr_ratio_local3");

	minus_rr_ratio_local(local, right, left, &pos);
	test(GetStatusDynamic(pos), "minus_rr_ratio_local4");
	test(ratiop(pos), "minus_rr_ratio_local5");
	test(equal_value_ratio(pos, SignPlus, 7, 6), "minus_rr_ratio_local6");

	/* (+ 4/7 7/6) 73/42 */
	/* (- 4/7 7/6) -25/42 */
	test_ratio_alloc(local, &left, SignPlus, 4, 7);
	test_ratio_alloc(local, &right, SignPlus, 7, 6);
	minus_rr_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_rr_ratio_local7");
	test(ratiop(pos), "minus_rr_ratio_local8");
	test(equal_value_ratio(pos, SignMinus, 25, 42), "minus_rr_ratio_local9");

	test_ratio_alloc(local, &left, SignMinus, 4, 7);
	test_ratio_alloc(local, &right, SignPlus, 7, 6);
	minus_rr_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_rr_ratio_local10");
	test(ratiop(pos), "minus_rr_ratio_local11");
	test(equal_value_ratio(pos, SignMinus, 73, 42), "minus_rr_ratio_local12");

	test_ratio_alloc(local, &left, SignPlus, 4, 7);
	test_ratio_alloc(local, &right, SignMinus, 7, 6);
	minus_rr_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_rr_ratio_local13");
	test(ratiop(pos), "minus_rr_ratio_local14");
	test(equal_value_ratio(pos, SignPlus, 73, 42), "minus_rr_ratio_local15");

	test_ratio_alloc(local, &left, SignMinus, 4, 7);
	test_ratio_alloc(local, &right, SignMinus, 7, 6);
	minus_rr_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_rr_ratio_local16");
	test(ratiop(pos), "minus_rr_ratio_local17");
	test(equal_value_ratio(pos, SignPlus, 25, 42), "minus_rr_ratio_local18");

	test_ratio_alloc(local, &left, SignMinus, 11, 4);
	test_ratio_alloc(local, &right, SignPlus, 1, 4);
	minus_rr_ratio_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_rr_ratio_local19");
	test(ratiop(pos), "minus_rr_ratio_local20");
	test(equal_value_ratio(pos, SignMinus, 3, 1), "minus_rr_ratio_local21");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_rr_real_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 0, 2);
	test_ratio_alloc(local, &right, SignPlus, 7, 6);
	minus_rr_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_rr_real_local1");
	test(ratiop(pos), "minus_rr_real_local2");
	test(equal_value_ratio(pos, SignMinus, 7, 6), "minus_rr_real_local3");

	minus_rr_real_local(local, right, left, &pos);
	test(GetStatusDynamic(pos), "minus_rr_real_local4");
	test(ratiop(pos), "minus_rr_real_local5");
	test(equal_value_ratio(pos, SignPlus, 7, 6), "minus_rr_real_local6");

	/* (+ 4/7 7/6) 73/42 */
	/* (- 4/7 7/6) -25/42 */
	test_ratio_alloc(local, &left, SignPlus, 4, 7);
	test_ratio_alloc(local, &right, SignPlus, 7, 6);
	minus_rr_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_rr_real_local7");
	test(ratiop(pos), "minus_rr_real_local8");
	test(equal_value_ratio(pos, SignMinus, 25, 42), "minus_rr_real_local9");

	test_ratio_alloc(local, &left, SignMinus, 4, 7);
	test_ratio_alloc(local, &right, SignPlus, 7, 6);
	minus_rr_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_rr_real_local10");
	test(ratiop(pos), "minus_rr_real_local11");
	test(equal_value_ratio(pos, SignMinus, 73, 42), "minus_rr_real_local12");

	test_ratio_alloc(local, &left, SignPlus, 4, 7);
	test_ratio_alloc(local, &right, SignMinus, 7, 6);
	minus_rr_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_rr_real_local13");
	test(ratiop(pos), "minus_rr_real_local14");
	test(equal_value_ratio(pos, SignPlus, 73, 42), "minus_rr_real_local15");

	test_ratio_alloc(local, &left, SignMinus, 4, 7);
	test_ratio_alloc(local, &right, SignMinus, 7, 6);
	minus_rr_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_rr_real_local16");
	test(ratiop(pos), "minus_rr_real_local17");
	test(equal_value_ratio(pos, SignPlus, 25, 42), "minus_rr_real_local18");

	test_ratio_alloc(local, &left, SignMinus, 11, 4);
	test_ratio_alloc(local, &right, SignPlus, 1, 4);
	minus_rr_real_local(local, left, right, &pos);
	test(GetStatusDynamic(pos), "minus_rr_real_local19");
	test(fixnump(pos), "minus_rr_real_local20");
	test(RefFixnum(pos) == -3, "minus_rr_real_local21");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_rr_real_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, pos;

	local = Local_Thread;
	push_local(local, &stack);

	test_ratio_alloc(local, &left, SignPlus, 0, 2);
	test_ratio_alloc(local, &right, SignPlus, 7, 6);
	minus_rr_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "minus_rr_real_common1");
	test(ratiop(pos), "minus_rr_real_common2");
	test(equal_value_ratio(pos, SignMinus, 7, 6), "minus_rr_real_common3");

	minus_rr_real_common(local, right, left, &pos);
	test(! GetStatusDynamic(pos), "minus_rr_real_common4");
	test(ratiop(pos), "minus_rr_real_common5");
	test(equal_value_ratio(pos, SignPlus, 7, 6), "minus_rr_real_common6");

	/* (+ 4/7 7/6) 73/42 */
	/* (- 4/7 7/6) -25/42 */
	test_ratio_alloc(local, &left, SignPlus, 4, 7);
	test_ratio_alloc(local, &right, SignPlus, 7, 6);
	minus_rr_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "minus_rr_real_common7");
	test(ratiop(pos), "minus_rr_real_common8");
	test(equal_value_ratio(pos, SignMinus, 25, 42), "minus_rr_real_common9");

	test_ratio_alloc(local, &left, SignMinus, 4, 7);
	test_ratio_alloc(local, &right, SignPlus, 7, 6);
	minus_rr_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "minus_rr_real_common10");
	test(ratiop(pos), "minus_rr_real_common11");
	test(equal_value_ratio(pos, SignMinus, 73, 42), "minus_rr_real_common12");

	test_ratio_alloc(local, &left, SignPlus, 4, 7);
	test_ratio_alloc(local, &right, SignMinus, 7, 6);
	minus_rr_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "minus_rr_real_common13");
	test(ratiop(pos), "minus_rr_real_common14");
	test(equal_value_ratio(pos, SignPlus, 73, 42), "minus_rr_real_common15");

	test_ratio_alloc(local, &left, SignMinus, 4, 7);
	test_ratio_alloc(local, &right, SignMinus, 7, 6);
	minus_rr_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "minus_rr_real_common16");
	test(ratiop(pos), "minus_rr_real_common17");
	test(equal_value_ratio(pos, SignPlus, 25, 42), "minus_rr_real_common18");

	test_ratio_alloc(local, &left, SignMinus, 11, 4);
	test_ratio_alloc(local, &right, SignPlus, 1, 4);
	minus_rr_real_common(local, left, right, &pos);
	test(! GetStatusDynamic(pos), "minus_rr_real_common19");
	test(fixnump(pos), "minus_rr_real_common20");
	test(RefFixnum(pos) == -3, "minus_rr_real_common21");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  Main
 */
static int testcase_ratio_plus(void)
{
	TestBreak(test_sign_reverse_ratio_inplace);
	TestBreak(test_sign_reverse_ratio_common);
	TestBreak(test_sign_reverse_ratio_local);
	TestBreak(test_plus_rv_data_ratio);
	TestBreak(test_plus_rv_data_local);
	TestBreak(test_plus_rv_data_common);
	TestBreak(test_minus_rv_data_ratio);
	TestBreak(test_minus_rv_data_local);
	TestBreak(test_minus_rv_data_common);
	TestBreak(test_plus_rv_ratio);
	TestBreak(test_plus_rv_local);
	TestBreak(test_plus_rv_common);
	TestBreak(test_plus_rv_ratio_local);
	TestBreak(test_plus_rv_real_local);
	TestBreak(test_plus_rv_real_common);
	TestBreak(test_plus_rf_ratio_local);
	TestBreak(test_plus_rf_real_local);
	TestBreak(test_plus_rf_real_common);
	TestBreak(test_minus_rv_ratio);
	TestBreak(test_minus_rv_local);
	TestBreak(test_minus_rv_common);
	TestBreak(test_cast_fixnum_ratio_local);
	TestBreak(test_cast_bignum_ratio_local);
	TestBreak(test_sigrev_fixnum_ratio_local);
	TestBreak(test_sigrev_bignum_ratio_local);
	TestBreak(test_minus_rf_ratio_local);
	TestBreak(test_minus_rf_real_local);
	TestBreak(test_minus_rf_real_common);
	TestBreak(test_minus_fr_ratio_local);
	TestBreak(test_minus_fr_real_local);
	TestBreak(test_minus_fr_real_common);
	TestBreak(test_plus_rb_ratio);
	TestBreak(test_plus_rb_local);
	TestBreak(test_plus_rb_common);
	TestBreak(test_minus_rb_ratio);
	TestBreak(test_minus_rb_local);
	TestBreak(test_minus_rb_common);
	TestBreak(test_plus_rb_ratio_local);
	TestBreak(test_plus_rb_real_local);
	TestBreak(test_plus_rb_real_common);
	TestBreak(test_minus_rb_ratio_local);
	TestBreak(test_minus_rb_real_local);
	TestBreak(test_minus_rb_real_common);
	TestBreak(test_minus_br_ratio_local);
	TestBreak(test_minus_br_real_local);
	TestBreak(test_minus_br_real_common);
	TestBreak(test_plus_rr_data_ratio);
	TestBreak(test_plus_rr_data_local);
	TestBreak(test_plus_rr_data_common);
	TestBreak(test_minus_rr_data_ratio);
	TestBreak(test_minus_rr_data_local);
	TestBreak(test_minus_rr_data_common);
	TestBreak(test_plus_rr_ratio_local);
	TestBreak(test_plus_rr_real_local);
	TestBreak(test_plus_rr_real_common);
	TestBreak(test_minus_rr_ratio_local);
	TestBreak(test_minus_rr_real_local);
	TestBreak(test_minus_rr_real_common);

	return 0;
}

static void testinit_ratio_plus(Execute ptr)
{
	build_lisproot(ptr);
	build_constant();
	build_object();
}

int test_ratio_plus(void)
{
	DegradeTitle;
	return DegradeCode(ratio_plus);
}

