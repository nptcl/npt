#include "ratio_equal.c"
#include "degrade.h"
#include "heap.h"

static void make_ratio_heap_nocheck(addr *ret, int sign, addr numer, addr denom)
{
	addr pos;

	heap_array2(&pos, LISPTYPE_RATIO, 2);
	bignum_copy_heap(&numer, numer);
	bignum_copy_heap(&denom, denom);
	SetSignRatio(pos, sign);
	SetNumerRatio(pos, numer);
	SetDenomRatio(pos, denom);
	*ret = pos;
}

static int test_equal_value_nosign_ratio(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	ratio_noreduction_value_local(local, &pos, SignMinus, 10, 20);
	test(equal_value_nosign_ratio(pos, 10, 20), "equal_value_nosign_ratio1");
	test(! equal_value_nosign_ratio(pos, 11, 20), "equal_value_nosign_ratio2");
	test(! equal_value_nosign_ratio(pos, 10, 30), "equal_value_nosign_ratio3");

	rollback_local(local, stack);

	RETURN;
}

static int test_equal_value_ratio(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	ratio_noreduction_value_local(local, &pos, SignMinus, 10, 20);
	test(equal_value_ratio(pos, SignMinus, 10, 20), "equal_value_ratio1");
	test(! equal_value_ratio(pos, SignPlus, 10, 20), "equal_value_ratio2");
	test(! equal_value_ratio(pos, SignMinus, 11, 20), "equal_value_ratio3");
	test(! equal_value_ratio(pos, SignMinus, 10, 30), "equal_value_ratio4");

	rollback_local(local, stack);

	RETURN;
}

static int test_equal_fr_real(void)
{
	LocalRoot local;
	LocalStack stack;
	addr numer, denom, left, right;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &numer, SignPlus, 10);
	bignum_value_local(local, &denom, SignPlus, 1);
	make_ratio_heap_nocheck(&right, SignPlus, numer, denom);
	fixnum_local(local, &left, 10);
	test(equal_fr_real(left, right), "equal_fr_real1");

	SetSignRatio(right, SignMinus);
	test(! equal_fr_real(left, right), "equal_fr_real2");

	bignum_value_local(local, &numer, SignPlus, 10);
	bignum_value_local(local, &denom, SignPlus, 1);
	make_ratio_heap_nocheck(&right, SignPlus, numer, denom);
	fixnum_local(local, &left, 9);
	test(! equal_fr_real(left, right), "equal_fr_real3");

	fixnum_local(local, &left, -10);
	test(! equal_fr_real(left, right), "equal_fr_real4");

	fixnum_local(local, &left, 10);
	test(equal_fr_real(left, right), "equal_fr_real5");

	rollback_local(local, stack);

	RETURN;
}

static int test_equal_br_real(void)
{
	LocalRoot local;
	LocalStack stack;
	addr numer, denom, left, right;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &numer, SignPlus, 10);
	bignum_value_local(local, &denom, SignPlus, 1);
	make_ratio_heap_nocheck(&right, SignPlus, numer, denom);
	bignum_value_local(local, &left, SignPlus, 10);
	test(equal_br_real(left, right), "equal_br_real1");

	SetSignRatio(right, SignMinus);
	test(! equal_br_real(left, right), "equal_br_real2");

	bignum_value_local(local, &numer, SignPlus, 10);
	bignum_value_local(local, &denom, SignPlus, 1);
	make_ratio_heap_nocheck(&right, SignPlus, numer, denom);
	bignum_value_local(local, &left, SignPlus, 9);
	test(! equal_br_real(left, right), "equal_br_real3");

	bignum_value_local(local, &left, SignMinus, -10);
	test(! equal_br_real(left, right), "equal_br_real4");

	bignum_value_local(local, &left, SignPlus, 10);
	test(equal_br_real(left, right), "equal_br_real5");

	rollback_local(local, stack);

	RETURN;
}

static int test_equal_rr_real(void)
{
	LocalRoot local;
	LocalStack stack;
	addr numer, denom, left, right;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &numer, SignPlus, 19);
	bignum_value_local(local, &denom, SignPlus, 20);
	make_ratio_heap_nocheck(&left, SignPlus, numer, denom);
	bignum_value_local(local, &numer, SignPlus, 19);
	bignum_value_local(local, &denom, SignPlus, 20);
	make_ratio_heap_nocheck(&right, SignPlus, numer, denom);
	test(equal_rr_real(left, right), "equal_rr_real1");

	SetSignRatio(left, SignMinus);
	test(! equal_rr_real(left, right), "equal_rr_real2");

	bignum_value_local(local, &numer, SignPlus, 17);
	bignum_value_local(local, &denom, SignPlus, 20);
	make_ratio_heap_nocheck(&right, SignPlus, numer, denom);
	test(! equal_rr_real(left, right), "equal_rr_real3");

	bignum_value_local(local, &numer, SignPlus, 19);
	bignum_value_local(local, &denom, SignPlus, 17);
	make_ratio_heap_nocheck(&right, SignPlus, numer, denom);
	test(! equal_rr_real(left, right), "equal_rr_real4");

	rollback_local(local, stack);

	RETURN;
}

static int test_split_single_float(void)
{
	int sign, size;
	single_float value;

	split_single_float(0.0f, &sign, &size, &value);
	test(sign == SignPlus, "split_single_float1");
	test(size == 0, "split_single_float2");
	test(value == 0.0f, "split_single_float3");

	split_single_float(1.0f, &sign, &size, &value);
	test(sign == SignPlus, "split_single_float4");
	test(size == 0, "split_single_float5");
	test(value == 1.0f, "split_single_float6");

	split_single_float(-2.0f, &sign, &size, &value);
	test(sign == SignMinus, "split_single_float7");
	test(size == 1, "split_single_float8");
	test(value == 1.0f, "split_single_float9");

	split_single_float(7.0f, &sign, &size, &value);
	test(sign == SignPlus, "split_single_float10");
	test(size == 0, "split_single_float11");
	test(value == 7.0f, "split_single_float12");

	split_single_float(-8.0f, &sign, &size, &value);
	test(sign == SignMinus, "split_single_float13");
	test(size == 3, "split_single_float14");
	test(value == 1.0f, "split_single_float15");

	split_single_float(0.25f, &sign, &size, &value);
	test(sign == SignPlus, "split_single_float16");
	test(size == -2, "split_single_float17");
	test(value == 1.0f, "split_single_float18");

	RETURN;
}

static int test_split_double_float(void)
{
	int sign, size;
	double_float value;

	split_double_float(0.0, &sign, &size, &value);
	test(sign == SignPlus, "split_double_float1");
	test(size == 0, "split_double_float2");
	test(value == 0.0, "split_double_float3");

	split_double_float(1.0, &sign, &size, &value);
	test(sign == SignPlus, "split_double_float4");
	test(size == 0, "split_double_float5");
	test(value == 1.0, "split_double_float6");

	split_double_float(-2.0, &sign, &size, &value);
	test(sign == SignMinus, "split_double_float7");
	test(size == 1, "split_double_float8");
	test(value == 1.0, "split_double_float9");

	split_double_float(7.0, &sign, &size, &value);
	test(sign == SignPlus, "split_double_float10");
	test(size == 0, "split_double_float11");
	test(value == 7.0, "split_double_float12");

	split_double_float(-8.0, &sign, &size, &value);
	test(sign == SignMinus, "split_double_float13");
	test(size == 3, "split_double_float14");
	test(value == 1.0, "split_double_float15");

	split_double_float(0.25f, &sign, &size, &value);
	test(sign == SignPlus, "split_double_float16");
	test(size == -2, "split_double_float17");
	test(value == 1.0, "split_double_float18");

	RETURN;
}

static int test_split_long_float(void)
{
	int sign, size;
	long_float value;

	split_long_float(0.0L, &sign, &size, &value);
	test(sign == SignPlus, "split_long_float1");
	test(size == 0, "split_long_float2");
	test(value == 0.0L, "split_long_float3");

	split_long_float(1.0L, &sign, &size, &value);
	test(sign == SignPlus, "split_long_float4");
	test(size == 0, "split_long_float5");
	test(value == 1.0L, "split_long_float6");

	split_long_float(-2.0L, &sign, &size, &value);
	test(sign == SignMinus, "split_long_float7");
	test(size == 1, "split_long_float8");
	test(value == 1.0L, "split_long_float9");

	split_long_float(7.0L, &sign, &size, &value);
	test(sign == SignPlus, "split_long_float10");
	test(size == 0, "split_long_float11");
	test(value == 7.0L, "split_long_float12");

	split_long_float(-8.0L, &sign, &size, &value);
	test(sign == SignMinus, "split_long_float13");
	test(size == 3, "split_long_float14");
	test(value == 1.0L, "split_long_float15");

	split_long_float(0.25f, &sign, &size, &value);
	test(sign == SignPlus, "split_long_float16");
	test(size == -2, "split_long_float17");
	test(value == 1.0L, "split_long_float18");

	RETURN;
}

static int test_rational_return_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr numer, denom, pos;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &pos, SignPlus, 11);
	rational_return_local(local, SignPlus, 0, pos, &pos);
	test(GetType(pos) == LISPTYPE_FIXNUM, "rational_return_local1");
	test(RefFixnum(pos) == 11, "rational_return_local2");
	test(GetStatusDynamic(pos), "rational_return_local3");

	bignum_value_local(local, &pos, SignPlus, 11);
	rational_return_local(local, SignMinus, 2, pos, &pos);
	test(GetType(pos) == LISPTYPE_FIXNUM, "rational_return_local4");
	test(RefFixnum(pos) == -44, "rational_return_local5");
	test(GetStatusDynamic(pos), "rational_return_local6");

	bignum_value_local(local, &pos, SignPlus, 11);
	rational_return_local(local, SignMinus, -2, pos, &pos);
	test(GetType(pos) == LISPTYPE_RATIO, "rational_return_local7");
	test(IsMinus(RefSignRatio(pos)), "rational_return_local8");
	test(GetStatusDynamic(pos), "rational_return_local9");
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	test(equal_value_bignum(numer, SignPlus, 11), "rational_return_local10");
	test(equal_value_bignum(denom, SignPlus, 4), "rational_return_local11");
	test(GetStatusDynamic(numer), "rational_return_local12");
	test(GetStatusDynamic(denom), "rational_return_local13");

	rollback_local(local, stack);

	RETURN;
}

static int test_rational_float_single_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr numer, denom, pos;

	local = Local_Thread;
	push_local(local, &stack);

	rational_float_single_local(local, 0.0f, &pos);
	test(GetType(pos) == LISPTYPE_FIXNUM, "rational_float_single_local1");
	test(RefFixnum(pos) == 0, "rational_float_single_local2");
	test(GetStatusDynamic(pos), "rational_float_single_local3");

	rational_float_single_local(local, -1.0f, &pos);
	test(GetType(pos) == LISPTYPE_FIXNUM, "rational_float_single_local4");
	test(RefFixnum(pos) == -1, "rational_float_single_local5");
	test(GetStatusDynamic(pos), "rational_float_single_local6");

	rational_float_single_local(local, 2.0f, &pos);
	test(GetType(pos) == LISPTYPE_FIXNUM, "rational_float_single_local7");
	test(RefFixnum(pos) == 2, "rational_float_single_local8");
	test(GetStatusDynamic(pos), "rational_float_single_local9");

	rational_float_single_local(local, -0.125f, &pos);
	test(GetType(pos) == LISPTYPE_RATIO, "rational_float_single_local10");
	test(IsMinus(RefSignRatio(pos)), "rational_float_single_local11");
	test(GetStatusDynamic(pos), "rational_float_single_local12");
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	test(GetType(numer) == LISPTYPE_BIGNUM, "rational_float_single_local13");
	test(GetType(denom) == LISPTYPE_BIGNUM, "rational_float_single_local14");
	test(equal_value_bignum(numer, SignPlus, 1), "rational_float_single_local15");
	test(equal_value_bignum(denom, SignPlus, 8), "rational_float_single_local16");
	test(GetStatusDynamic(numer), "rational_float_single_local17");
	test(GetStatusDynamic(denom), "rational_float_single_local18");

	rollback_local(local, stack);

	RETURN;
}

static int test_rational_float_double_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr numer, denom, pos;

	local = Local_Thread;
	push_local(local, &stack);

	rational_float_double_local(local, 0.0, &pos);
	test(GetType(pos) == LISPTYPE_FIXNUM, "rational_float_double_local1");
	test(RefFixnum(pos) == 0, "rational_float_double_local2");
	test(GetStatusDynamic(pos), "rational_float_double_local3");

	rational_float_double_local(local, -1.0, &pos);
	test(GetType(pos) == LISPTYPE_FIXNUM, "rational_float_double_local4");
	test(RefFixnum(pos) == -1, "rational_float_double_local5");
	test(GetStatusDynamic(pos), "rational_float_double_local6");

	rational_float_double_local(local, 2.0, &pos);
	test(GetType(pos) == LISPTYPE_FIXNUM, "rational_float_double_local7");
	test(RefFixnum(pos) == 2, "rational_float_double_local8");
	test(GetStatusDynamic(pos), "rational_float_double_local9");

	rational_float_double_local(local, -0.125, &pos);
	test(GetType(pos) == LISPTYPE_RATIO, "rational_float_double_local10");
	test(IsMinus(RefSignRatio(pos)), "rational_float_double_local11");
	test(GetStatusDynamic(pos), "rational_float_double_local12");
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	test(GetType(numer) == LISPTYPE_BIGNUM, "rational_float_double_local13");
	test(GetType(denom) == LISPTYPE_BIGNUM, "rational_float_double_local14");
	test(equal_value_bignum(numer, SignPlus, 1), "rational_float_double_local15");
	test(equal_value_bignum(denom, SignPlus, 8), "rational_float_double_local16");
	test(GetStatusDynamic(numer), "rational_float_double_local17");
	test(GetStatusDynamic(denom), "rational_float_double_local18");

	rollback_local(local, stack);

	RETURN;
}

static int test_rational_float_long_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr numer, denom, pos;

	local = Local_Thread;
	push_local(local, &stack);

	rational_float_long_local(local, 0.0, &pos);
	test(GetType(pos) == LISPTYPE_FIXNUM, "rational_float_long_local1");
	test(RefFixnum(pos) == 0, "rational_float_long_local2");
	test(GetStatusDynamic(pos), "rational_float_long_local3");

	rational_float_long_local(local, -1.0, &pos);
	test(GetType(pos) == LISPTYPE_FIXNUM, "rational_float_long_local4");
	test(RefFixnum(pos) == -1, "rational_float_long_local5");
	test(GetStatusDynamic(pos), "rational_float_long_local6");

	rational_float_long_local(local, 2.0, &pos);
	test(GetType(pos) == LISPTYPE_FIXNUM, "rational_float_long_local7");
	test(RefFixnum(pos) == 2, "rational_float_long_local8");
	test(GetStatusDynamic(pos), "rational_float_long_local9");

	rational_float_long_local(local, -0.125, &pos);
	test(GetType(pos) == LISPTYPE_RATIO, "rational_float_long_local10");
	test(IsMinus(RefSignRatio(pos)), "rational_float_long_local11");
	test(GetStatusDynamic(pos), "rational_float_long_local12");
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	test(GetType(numer) == LISPTYPE_BIGNUM, "rational_float_long_local13");
	test(GetType(denom) == LISPTYPE_BIGNUM, "rational_float_long_local14");
	test(equal_value_bignum(numer, SignPlus, 1), "rational_float_long_local15");
	test(equal_value_bignum(denom, SignPlus, 8), "rational_float_long_local16");
	test(GetStatusDynamic(numer), "rational_float_long_local17");
	test(GetStatusDynamic(denom), "rational_float_long_local18");

	rollback_local(local, stack);

	RETURN;
}

static int test_equal_ratio_type(void)
{
	LocalRoot local;
	LocalStack stack;
	addr numer, denom, left, right;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &numer, SignPlus, 11);
	bignum_value_local(local, &denom, SignPlus, 3);
	ratio_reduction_nocopy_local(local, &left, SignPlus, numer, denom);

	fixnum_local(local, &right, 11);
	test(! equal_ratio_type(left, right), "equal_ratio_type1");

	bignum_value_local(local, &right, SignPlus, 11);
	test(! equal_ratio_type(left, right), "equal_ratio_type2");

	bignum_value_local(local, &numer, SignPlus, 11);
	bignum_value_local(local, &denom, SignPlus, 1);
	ratio_reduction_nocopy_local(local, &right, SignPlus, numer, denom);
	test(! equal_ratio_type(left, right), "equal_ratio_type3");

	bignum_value_local(local, &numer, SignPlus, 11);
	bignum_value_local(local, &denom, SignPlus, 3);
	ratio_reduction_nocopy_local(local, &right, SignPlus, numer, denom);
	test(equal_ratio_type(left, right), "equal_ratio_type4");

	rollback_local(local, stack);

	RETURN;
}

static int test_equal_rs_real(void)
{
	LocalRoot local;
	LocalStack stack;
	addr numer, denom, left, right;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &numer, SignPlus, 1);
	bignum_value_local(local, &denom, SignPlus, 4);
	ratio_reduction_nocopy_local(local, &left, SignPlus, numer, denom);

	single_float_local(local, &right, 0.25f);
	test(equal_rs_real(local, left, right), "equal_rs_real1");

	single_float_local(local, &right, -0.25f);
	test(! equal_rs_real(local, left, right), "equal_rs_real2");

	single_float_local(local, &right, 11.22f);
	test(! equal_rs_real(local, left, right), "equal_rs_real3");

	rollback_local(local, stack);

	RETURN;
}

static int test_equal_rd_real(void)
{
	LocalRoot local;
	LocalStack stack;
	addr numer, denom, left, right;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &numer, SignPlus, 1);
	bignum_value_local(local, &denom, SignPlus, 4);
	ratio_reduction_nocopy_local(local, &left, SignPlus, numer, denom);

	double_float_local(local, &right, 0.25);
	test(equal_rd_real(local, left, right), "equal_rd_real1");

	double_float_local(local, &right, -0.25);
	test(! equal_rd_real(local, left, right), "equal_rd_real2");

	double_float_local(local, &right, 11.22);
	test(! equal_rd_real(local, left, right), "equal_rd_real3");

	rollback_local(local, stack);

	RETURN;
}

static int test_equal_rl_real(void)
{
	LocalRoot local;
	LocalStack stack;
	addr numer, denom, left, right;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &numer, SignPlus, 1);
	bignum_value_local(local, &denom, SignPlus, 4);
	ratio_reduction_nocopy_local(local, &left, SignPlus, numer, denom);

	long_float_local(local, &right, 0.25L);
	test(equal_rl_real(local, left, right), "equal_rl_real1");

	long_float_local(local, &right, -0.25L);
	test(! equal_rl_real(local, left, right), "equal_rl_real2");

	long_float_local(local, &right, 11.22L);
	test(! equal_rl_real(local, left, right), "equal_rl_real3");

	rollback_local(local, stack);

	RETURN;
}

static int test_compare_bigtype_bignum(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos;
	bigtype *data;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_local(local, &pos, SignPlus, 5);
	GetDataBignum(pos, &data);
	data[0] = 10;
	data[1] = 0;
	SetSizeBignum(pos, 2);
	test(compare_bigtype_bignum(20, pos) < 0, "compare_bigtype_bignum1");
	bignum_value_local(local, &pos, SignMinus, 10);
	test(compare_bigtype_bignum(20, pos) > 0, "compare_bigtype_bignum2");
	test(compare_bigtype_bignum(5, pos) < 0, "compare_bigtype_bignum3");
	test(compare_bigtype_bignum(10, pos) == 0, "compare_bigtype_bignum4");

	rollback_local(local, stack);

	RETURN;
}

static int test_compare_bigtype_ratio_nosign(void)
{
	int check;
	LocalRoot local;
	LocalStack stack;
	addr numer, denom, pos;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &numer, SignPlus, 51);
	bignum_value_local(local, &denom, SignPlus, 10);
	make_ratio_reduction_local(local, &pos, SignMinus, numer, denom);
	check = compare_bigtype_ratio_nosign(local, 0, pos);
	test(check < 0, "compare_bigtype_ratio_nosign1");

	bignum_value_local(local, &numer, SignPlus, 0);
	bignum_value_local(local, &denom, SignPlus, 1);
	make_ratio_reduction_local(local, &pos, SignMinus, numer, denom);
	check = compare_bigtype_ratio_nosign(local, 0, pos);
	test(check == 0, "compare_bigtype_ratio_nosign2");
	check = compare_bigtype_ratio_nosign(local, 10, pos);
	test(check > 0, "compare_bigtype_ratio_nosign3");

	bignum_value_local(local, &numer, SignPlus, 51);
	bignum_value_local(local, &denom, SignPlus, 10);
	ratio_reduction_nocopy_local(local, &pos, SignPlus, numer, denom);
	check = compare_bigtype_ratio_nosign(local, 4, pos);
	test(check < 0, "compare_bigtype_ratio_nosign4");
	check = compare_bigtype_ratio_nosign(local, 5, pos);
	test(check < 0, "compare_bigtype_ratio_nosign5");
	check = compare_bigtype_ratio_nosign(local, 6, pos);
	test(check > 0, "compare_bigtype_ratio_nosign6");

	SetSignRatio(pos, SignMinus);
	check = compare_bigtype_ratio_nosign(local, 4, pos);
	test(check < 0, "compare_bigtype_ratio_nosign7");
	check = compare_bigtype_ratio_nosign(local, 5, pos);
	test(check < 0, "compare_bigtype_ratio_nosign8");
	check = compare_bigtype_ratio_nosign(local, 6, pos);
	test(check > 0, "compare_bigtype_ratio_nosign9");

	rollback_local(local, stack);

	RETURN;
}

static int test_compare_fr_real(void)
{
	int check;
	LocalRoot local;
	LocalStack stack;
	addr numer, denom, left, right;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &numer, SignPlus, 51);
	bignum_value_local(local, &denom, SignPlus, 10);
	ratio_reduction_nocopy_local(local, &right, SignPlus, numer, denom);

	fixnum_local(local, &left, 4);
	check = compare_fr_real(local, left, right);
	test(check < 0, "compare_fr_real1");
	fixnum_local(local, &left, 5);
	check = compare_fr_real(local, left, right);
	test(check < 0, "compare_fr_real2");
	fixnum_local(local, &left, 6);
	check = compare_fr_real(local, left, right);
	test(check > 0, "compare_fr_real3");

	SetSignRatio(right, SignMinus);
	fixnum_local(local, &left, 4);
	check = compare_fr_real(local, left, right);
	test(check > 0, "compare_fr_real4");
	fixnum_local(local, &left, 5);
	check = compare_fr_real(local, left, right);
	test(check > 0, "compare_fr_real5");
	fixnum_local(local, &left, 6);
	check = compare_fr_real(local, left, right);
	test(check > 0, "compare_fr_real6");

	SetSignRatio(right, SignPlus);
	fixnum_local(local, &left, -4);
	check = compare_fr_real(local, left, right);
	test(check < 0, "compare_fr_real7");
	fixnum_local(local, &left, -5);
	check = compare_fr_real(local, left, right);
	test(check < 0, "compare_fr_real8");
	fixnum_local(local, &left, -6);
	check = compare_fr_real(local, left, right);
	test(check < 0, "compare_fr_real9");

	SetSignRatio(right, SignMinus);
	fixnum_local(local, &left, -4);
	check = compare_fr_real(local, left, right);
	test(check > 0, "compare_fr_real10");
	fixnum_local(local, &left, -5);
	check = compare_fr_real(local, left, right);
	test(check > 0, "compare_fr_real11");
	fixnum_local(local, &left, -6);
	check = compare_fr_real(local, left, right);
	test(check < 0, "compare_fr_real12");

	fixnum_local(local, &left, 0);
	SetSignRatio(right, SignPlus);
	check = compare_fr_real(local, left, right);
	test(check < 0, "compare_fr_real13");
	SetSignRatio(right, SignMinus);
	check = compare_fr_real(local, left, right);
	test(check > 0, "compare_fr_real14");

	bignum_value_local(local, &numer, SignPlus, 0);
	bignum_value_local(local, &denom, SignPlus, 1);
	make_ratio_reduction_local(local, &right, SignPlus, numer, denom);
	fixnum_local(local, &left, 0);
	check = compare_fr_real(local, left, right);
	test(check == 0, "compare_fr_real15");

	fixnum_local(local, &left, 10);
	check = compare_fr_real(local, left, right);
	test(check > 0, "compare_fr_real16");

	fixnum_local(local, &left, -20);
	check = compare_fr_real(local, left, right);
	test(check < 0, "compare_fr_real17");

	fixnum_local(local, &left, 0);
	bignum_value_local(local, &numer, SignPlus, 10);
	bignum_value_local(local, &denom, SignPlus, 1);
	make_ratio_reduction_local(local, &right, SignPlus, numer, denom);
	check = compare_fr_real(local, left, right);
	test(check < 0, "compare_fr_real18");

	bignum_value_local(local, &numer, SignPlus, 20);
	bignum_value_local(local, &denom, SignPlus, 1);
	make_ratio_reduction_local(local, &right, SignMinus, numer, denom);
	check = compare_fr_real(local, left, right);
	test(check > 0, "compare_fr_real19");

	rollback_local(local, stack);

	RETURN;
}

static int test_compare_rf_real(void)
{
	int check;
	LocalRoot local;
	LocalStack stack;
	addr numer, denom, left, right;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &numer, SignPlus, 51);
	bignum_value_local(local, &denom, SignPlus, 10);
	ratio_reduction_nocopy_local(local, &left, SignPlus, numer, denom);

	fixnum_local(local, &right, 4);
	check = compare_rf_real(local, left, right);
	test(check > 0, "compare_rf_real1");
	fixnum_local(local, &right, 5);
	check = compare_rf_real(local, left, right);
	test(check > 0, "compare_rf_real2");
	fixnum_local(local, &right, 6);
	check = compare_rf_real(local, left, right);
	test(check < 0, "compare_rf_real3");

	rollback_local(local, stack);

	RETURN;
}

static int test_compare_bigdata_ratio_nosign(void)
{
	int check;
	LocalRoot local;
	LocalStack stack;
	addr numer, denom, left, right;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &numer, SignPlus, 10);
	bignum_value_local(local, &denom, SignPlus, 1);
	make_ratio_reduction_local(local, &right, SignPlus, numer, denom);

	bignum_value_local(local, &left, SignMinus, 5);
	check = compare_bigdata_ratio_nosign(local, left, right);
	test(check < 0, "compare_bigdata_ratio_nosign1");
	bignum_value_local(local, &left, SignMinus, 10);
	check = compare_bigdata_ratio_nosign(local, left, right);
	test(check == 0, "compare_bigdata_ratio_nosign2");
	bignum_value_local(local, &left, SignMinus, 11);
	check = compare_bigdata_ratio_nosign(local, left, right);
	test(check > 0, "compare_bigdata_ratio_nosign3");

	bignum_value_local(local, &numer, SignPlus, 51);
	bignum_value_local(local, &denom, SignPlus, 10);
	ratio_reduction_nocopy_local(local, &right, SignPlus, numer, denom);

	bignum_value_local(local, &left, SignMinus, 4);
	check = compare_bigdata_ratio_nosign(local, left, right);
	test(check < 0, "compare_bigdata_ratio_nosign4");
	bignum_value_local(local, &left, SignMinus, 5);
	check = compare_bigdata_ratio_nosign(local, left, right);
	test(check < 0, "compare_bigdata_ratio_nosign5");
	bignum_value_local(local, &left, SignMinus, 6);
	check = compare_bigdata_ratio_nosign(local, left, right);
	test(check > 0, "compare_bigdata_ratio_nosign6");

	rollback_local(local, stack);

	RETURN;
}

static int test_compare_br_real(void)
{
	int check;
	LocalRoot local;
	LocalStack stack;
	addr numer, denom, left, right;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &left, SignPlus, 0);
	bignum_value_local(local, &numer, SignPlus, 0);
	bignum_value_local(local, &denom, SignPlus, 1);
	make_ratio_reduction_local(local, &right, SignPlus, numer, denom);
	check = compare_br_real(local, left, right);
	test(check == 0, "compare_br_real1");

	bignum_value_local(local, &left, SignPlus, 10);
	check = compare_br_real(local, left, right);
	test(check > 0, "compare_br_real2");

	bignum_value_local(local, &left, SignMinus, 10);
	check = compare_br_real(local, left, right);
	test(check < 0, "compare_br_real3");

	bignum_value_local(local, &left, SignPlus, 0);
	bignum_value_local(local, &numer, SignPlus, 10);
	bignum_value_local(local, &denom, SignPlus, 1);
	make_ratio_reduction_local(local, &right, SignPlus, numer, denom);
	check = compare_br_real(local, left, right);
	test(check < 0, "compare_br_real4");

	bignum_value_local(local, &numer, SignPlus, 20);
	bignum_value_local(local, &denom, SignPlus, 1);
	make_ratio_reduction_local(local, &right, SignMinus, numer, denom);
	check = compare_br_real(local, left, right);
	test(check > 0, "compare_br_real5");

	bignum_value_local(local, &left, SignPlus, 10);
	bignum_value_local(local, &numer, SignPlus, 10);
	bignum_value_local(local, &denom, SignPlus, 1);
	make_ratio_reduction_local(local, &right, SignPlus, numer, denom);
	check = compare_br_real(local, left, right);
	test(check == 0, "compare_br_real6");

	bignum_value_local(local, &left, SignPlus, 10);
	bignum_value_local(local, &numer, SignPlus, 10);
	bignum_value_local(local, &denom, SignPlus, 1);
	make_ratio_reduction_local(local, &right, SignMinus, numer, denom);
	check = compare_br_real(local, left, right);
	test(check > 0, "compare_br_real7");

	bignum_value_local(local, &left, SignMinus, 10);
	bignum_value_local(local, &numer, SignPlus, 10);
	bignum_value_local(local, &denom, SignPlus, 1);
	make_ratio_reduction_local(local, &right, SignPlus, numer, denom);
	check = compare_br_real(local, left, right);
	test(check < 0, "compare_br_real8");

	bignum_value_local(local, &left, SignPlus, 20);
	bignum_value_local(local, &numer, SignPlus, 10);
	bignum_value_local(local, &denom, SignPlus, 1);
	make_ratio_reduction_local(local, &right, SignPlus, numer, denom);
	check = compare_br_real(local, left, right);
	test(check > 0, "compare_br_real9");

	bignum_value_local(local, &left, SignMinus, 20);
	bignum_value_local(local, &numer, SignPlus, 10);
	bignum_value_local(local, &denom, SignPlus, 1);
	make_ratio_reduction_local(local, &right, SignMinus, numer, denom);
	check = compare_br_real(local, left, right);
	test(check < 0, "compare_br_real10");

	rollback_local(local, stack);

	RETURN;
}

static int test_compare_rb_real(void)
{
	int check;
	LocalRoot local;
	LocalStack stack;
	addr numer, denom, left, right;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &numer, SignPlus, 10);
	bignum_value_local(local, &denom, SignPlus, 1);
	make_ratio_reduction_local(local, &left, SignPlus, numer, denom);
	bignum_value_local(local, &right, SignPlus, 20);
	check = compare_rb_real(local, left, right);
	test(check < 0, "compare_rb_real1");

	rollback_local(local, stack);

	RETURN;
}

static int test_compare_ratio_local(void)
{
	int check;
	LocalRoot local;
	LocalStack stack;
	addr numer, denom, left, right;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &numer, SignPlus, 5);
	bignum_value_local(local, &denom, SignPlus, 6);
	make_ratio_reduction_local(local, &left, SignPlus, numer, denom);
	bignum_value_local(local, &numer, SignPlus, 3);
	bignum_value_local(local, &denom, SignPlus, 4);
	make_ratio_reduction_local(local, &right, SignPlus, numer, denom);
	check = compare_ratio_local(local, left, right);
	test(check > 0, "compare_ratio_local1");

	bignum_value_local(local, &numer, SignPlus, 5);
	bignum_value_local(local, &denom, SignPlus, 4);
	make_ratio_reduction_local(local, &right, SignPlus, numer, denom);
	check = compare_ratio_local(local, left, right);
	test(check < 0, "compare_ratio_local2");

	rollback_local(local, stack);

	RETURN;
}

static int test_compare_rr_real(void)
{
	int check;
	LocalRoot local;
	LocalStack stack;
	addr numer, denom, left, right;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &numer, SignPlus, 0);
	bignum_value_local(local, &denom, SignPlus, 1);
	make_ratio_reduction_local(local, &left, SignPlus, numer, denom);
	make_ratio_reduction_local(local, &right, SignPlus, numer, denom);
	test(compare_rr_real(local, left, right) == 0 , "compare_rr_real1");

	bignum_value_local(local, &numer, SignPlus, 10);
	bignum_value_local(local, &denom, SignPlus, 1);
	make_ratio_reduction_local(local, &right, SignPlus, numer, denom);
	test(compare_rr_real(local, left, right) < 0 , "compare_rr_real2");
	make_ratio_reduction_local(local, &right, SignMinus, numer, denom);
	test(compare_rr_real(local, left, right) > 0 , "compare_rr_real3");

	bignum_value_local(local, &numer, SignPlus, 0);
	bignum_value_local(local, &denom, SignPlus, 1);
	make_ratio_reduction_local(local, &right, SignPlus, numer, denom);
	bignum_value_local(local, &numer, SignPlus, 20);
	bignum_value_local(local, &denom, SignPlus, 1);
	make_ratio_reduction_local(local, &left, SignPlus, numer, denom);
	test(compare_rr_real(local, left, right) > 0 , "compare_rr_real4");
	make_ratio_reduction_local(local, &left, SignMinus, numer, denom);
	test(compare_rr_real(local, left, right) < 0 , "compare_rr_real5");

	bignum_value_local(local, &numer, SignPlus, 20);
	bignum_value_local(local, &denom, SignPlus, 1);
	make_ratio_reduction_local(local, &left, SignPlus, numer, denom);
	make_ratio_reduction_local(local, &right, SignMinus, numer, denom);
	test(compare_rr_real(local, left, right) > 0 , "compare_rr_real6");
	make_ratio_reduction_local(local, &left, SignMinus, numer, denom);
	make_ratio_reduction_local(local, &right, SignPlus, numer, denom);
	test(compare_rr_real(local, left, right) < 0 , "compare_rr_real7");

	bignum_value_local(local, &numer, SignPlus, 4);
	bignum_value_local(local, &denom, SignPlus, 5);
	make_ratio_reduction_local(local, &left, SignPlus, numer, denom);
	bignum_value_local(local, &numer, SignPlus, 6);
	make_ratio_reduction_local(local, &right, SignPlus, numer, denom);
	test(compare_rr_real(local, left, right) < 0 , "compare_rr_real8");
	test(compare_rr_real(local, right, left) > 0 , "compare_rr_real9");

	bignum_value_local(local, &numer, SignPlus, 4);
	bignum_value_local(local, &denom, SignPlus, 5);
	make_ratio_reduction_local(local, &left, SignMinus, numer, denom);
	bignum_value_local(local, &numer, SignPlus, 6);
	make_ratio_reduction_local(local, &right, SignMinus, numer, denom);
	test(compare_rr_real(local, left, right) > 0 , "compare_rr_real10");
	test(compare_rr_real(local, right, left) < 0 , "compare_rr_real11");

	bignum_value_local(local, &numer, SignPlus, 5);
	bignum_value_local(local, &denom, SignPlus, 6);
	make_ratio_reduction_local(local, &left, SignPlus, numer, denom);
	bignum_value_local(local, &numer, SignPlus, 3);
	bignum_value_local(local, &denom, SignPlus, 4);
	make_ratio_reduction_local(local, &right, SignPlus, numer, denom);
	check = compare_rr_real(local, left, right);
	test(check > 0, "compare_rr_real12");

	bignum_value_local(local, &numer, SignPlus, 5);
	bignum_value_local(local, &denom, SignPlus, 4);
	make_ratio_reduction_local(local, &right, SignPlus, numer, denom);
	check = compare_rr_real(local, left, right);
	test(check < 0, "compare_rr_real13");

	rollback_local(local, stack);

	RETURN;
}

static int test_compare_rs_real(void)
{
	int check;
	LocalRoot local;
	LocalStack stack;
	addr numer, denom, left, right;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &numer, SignPlus, 51);
	bignum_value_local(local, &denom, SignPlus, 10);
	make_ratio_reduction_local(local, &left, SignPlus, numer, denom);
	single_float_local(local, &right, 5.0f);
	check = compare_rs_real(local, left, right);
	test(check > 0, "compare_rs_real1");

	single_float_local(local, &right, 5.2f);
	check = compare_rs_real(local, left, right);
	test(check < 0, "compare_rs_real2");

	single_float_local(local, &right, -5.2f);
	check = compare_rs_real(local, left, right);
	test(check > 0, "compare_rs_real3");

	rollback_local(local, stack);

	RETURN;
}

static int test_compare_rd_real(void)
{
	int check;
	LocalRoot local;
	LocalStack stack;
	addr numer, denom, left, right;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &numer, SignPlus, 51);
	bignum_value_local(local, &denom, SignPlus, 10);
	make_ratio_reduction_local(local, &left, SignPlus, numer, denom);
	double_float_local(local, &right, 5.0);
	check = compare_rd_real(local, left, right);
	test(check > 0, "compare_rd_real1");

	double_float_local(local, &right, 5.2);
	check = compare_rd_real(local, left, right);
	test(check < 0, "compare_rd_real2");

	double_float_local(local, &right, -5.2);
	check = compare_rd_real(local, left, right);
	test(check > 0, "compare_rd_real3");

	rollback_local(local, stack);

	RETURN;
}

static int test_compare_rl_real(void)
{
	int check;
	LocalRoot local;
	LocalStack stack;
	addr numer, denom, left, right;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &numer, SignPlus, 51);
	bignum_value_local(local, &denom, SignPlus, 10);
	make_ratio_reduction_local(local, &left, SignPlus, numer, denom);
	long_float_local(local, &right, 5.0L);
	check = compare_rl_real(local, left, right);
	test(check > 0, "compare_rl_real1");

	long_float_local(local, &right, 5.2L);
	check = compare_rl_real(local, left, right);
	test(check < 0, "compare_rl_real2");

	long_float_local(local, &right, -5.2L);
	check = compare_rl_real(local, left, right);
	test(check > 0, "compare_rl_real3");

	rollback_local(local, stack);

	RETURN;
}

static int test_compare_sr_real(void)
{
	int check;
	LocalRoot local;
	LocalStack stack;
	addr numer, denom, left, right;

	local = Local_Thread;
	push_local(local, &stack);

	single_float_local(local, &left, 5.0f);
	bignum_value_local(local, &numer, SignPlus, 51);
	bignum_value_local(local, &denom, SignPlus, 10);
	make_ratio_reduction_local(local, &right, SignPlus, numer, denom);
	check = compare_sr_real(local, left, right);
	test(check < 0, "compare_sr_real1");

	rollback_local(local, stack);

	RETURN;
}

static int test_compare_dr_real(void)
{
	int check;
	LocalRoot local;
	LocalStack stack;
	addr numer, denom, left, right;

	local = Local_Thread;
	push_local(local, &stack);

	double_float_local(local, &left, 5.0);
	bignum_value_local(local, &numer, SignPlus, 51);
	bignum_value_local(local, &denom, SignPlus, 10);
	make_ratio_reduction_local(local, &right, SignPlus, numer, denom);
	check = compare_dr_real(local, left, right);
	test(check < 0, "compare_dr_real1");

	rollback_local(local, stack);

	RETURN;
}

static int test_compare_lr_real(void)
{
	int check;
	LocalRoot local;
	LocalStack stack;
	addr numer, denom, left, right;

	local = Local_Thread;
	push_local(local, &stack);

	long_float_local(local, &left, 5.0L);
	bignum_value_local(local, &numer, SignPlus, 51);
	bignum_value_local(local, &denom, SignPlus, 10);
	make_ratio_reduction_local(local, &right, SignPlus, numer, denom);
	check = compare_lr_real(local, left, right);
	test(check < 0, "compare_lr_real1");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  Main
 */
static int testbreak_ratio_equal(void)
{
	TestBreak(test_equal_value_nosign_ratio);
	TestBreak(test_equal_value_ratio);
	TestBreak(test_equal_fr_real);
	TestBreak(test_equal_br_real);
	TestBreak(test_equal_rr_real);
	TestBreak(test_split_single_float);
	TestBreak(test_split_double_float);
	TestBreak(test_split_long_float);
	TestBreak(test_rational_return_local);
	TestBreak(test_rational_float_single_local);
	TestBreak(test_rational_float_double_local);
	TestBreak(test_rational_float_long_local);
	TestBreak(test_equal_ratio_type);
	TestBreak(test_equal_rs_real);
	TestBreak(test_equal_rd_real);
	TestBreak(test_equal_rl_real);
	TestBreak(test_compare_bigtype_bignum);
	TestBreak(test_compare_bigtype_ratio_nosign);
	TestBreak(test_compare_fr_real);
	TestBreak(test_compare_rf_real);
	TestBreak(test_compare_bigdata_ratio_nosign);
	TestBreak(test_compare_br_real);
	TestBreak(test_compare_rb_real);
	TestBreak(test_compare_ratio_local);
	TestBreak(test_compare_rr_real);
	TestBreak(test_compare_rs_real);
	TestBreak(test_compare_rd_real);
	TestBreak(test_compare_rl_real);
	TestBreak(test_compare_sr_real);
	TestBreak(test_compare_dr_real);
	TestBreak(test_compare_lr_real);

	return 0;
}

int test_ratio_equal(void)
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
		lisp_initialize = 1;
		result = testbreak_ratio_equal();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

