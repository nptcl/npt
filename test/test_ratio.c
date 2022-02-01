#include "ratio.c"
#include "degrade.h"

/*
 *  ratio
 */
static int test_reduction_single(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &left, SignPlus, 10);
	bignum_value_local(local, &right, SignPlus, 19);
	reduction_single(left, right);
	test(reffixed_bignum(left, 0) == 10, "reduction_single1");
	test(reffixed_bignum(right, 0) == 19, "reduction_single2");

	bignum_value_local(local, &left, SignPlus, 252);
	bignum_value_local(local, &right, SignPlus, 105);
	reduction_single(left, right);
	test(reffixed_bignum(left, 0) == 12, "reduction_single3");
	test(reffixed_bignum(right, 0) == 5, "reduction_single4");

	bignum_value_local(local, &left, SignPlus, 1071);
	bignum_value_local(local, &right, SignPlus, 1029);
	reduction_single(left, right);
	test(reffixed_bignum(left, 0) == 51, "reduction_single5");
	test(reffixed_bignum(right, 0) == 49, "reduction_single6");

	bignum_value_local(local, &left, SignPlus, 14);
	bignum_value_local(local, &right, SignPlus, 7);
	reduction_single(left, right);
	test(reffixed_bignum(left, 0) == 2, "reduction_single7");
	test(reffixed_bignum(right, 0) == 1, "reduction_single8");

	rollback_local(local, stack);

	RETURN;
}

static int test_reduction_multiple(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, cons, check;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &left, SignPlus, 1071);
	bignum_value_local(local, &right, SignPlus, 1029);
	reduction_multiple(local, left, right);
	test(reffixed_bignum(left, 0) == 51, "reduction_multiple1");
	test(reffixed_bignum(right, 0) == 49, "reduction_multiple2");

	bigcons_local(local, &cons);
	setchar_bigcons_(local, cons, 16, "AC8E074D2AC4CCD83E7C158CAB2BDE62A9BD0");
	bignum_cons_local(local, &left, SignPlus, cons);
	setchar_bigcons_(local, cons, 16, "13429222C878719C2E13D04BEE1AEE280815CF00");
	bignum_cons_local(local, &right, SignPlus, cons);
	reduction_multiple(local, left, right);
	setchar_bigcons_(local, cons, 16, "10FAD49DA1EAB68EDE0E49F7717B21AC5");
	bignum_cons_local(local, &check, SignPlus, cons);
	test(equal_bb_real(left, check), "reduction_multiple3");
	setchar_bigcons_(local, cons, 16, "1E52E158A845559AE45A9B18141794A4A70");
	bignum_cons_local(local, &check, SignPlus, cons);
	test(equal_bb_real(right, check), "reduction_multiple4");

	bignum_value_local(local, &left, SignPlus, 14);
	bignum_value_local(local, &right, SignPlus, 7);
	reduction_multiple(local, left, right);
	test(reffixed_bignum(left, 0) == 2, "reduction_multiple5");
	test(reffixed_bignum(right, 0) == 1, "reduction_multiple6");

	rollback_local(local, stack);

	RETURN;
}

static int test_reduction_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, cons, check;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &left, SignPlus, 1071);
	bignum_value_local(local, &right, SignPlus, 1029);
	reduction_local(local, left, right);
	test(reffixed_bignum(left, 0) == 51, "reduction_local1");
	test(reffixed_bignum(right, 0) == 49, "reduction_local2");

	bigcons_local(local, &cons);
	setchar_bigcons_(local, cons, 16, "AC8E074D2AC4CCD83E7C158CAB2BDE62A9BD0");
	bignum_cons_local(local, &left, SignPlus, cons);
	setchar_bigcons_(local, cons, 16, "13429222C878719C2E13D04BEE1AEE280815CF00");
	bignum_cons_local(local, &right, SignPlus, cons);
	reduction_local(local, left, right);
	setchar_bigcons_(local, cons, 16, "10FAD49DA1EAB68EDE0E49F7717B21AC5");
	bignum_cons_local(local, &check, SignPlus, cons);
	test(equal_bb_real(left, check), "reduction_local3");
	setchar_bigcons_(local, cons, 16, "1E52E158A845559AE45A9B18141794A4A70");
	bignum_cons_local(local, &check, SignPlus, cons);
	test(equal_bb_real(right, check), "reduction_local4");

	rollback_local(local, stack);

	RETURN;
}

static int test_make_ratio_heap(void)
{
	addr pos, numer, denom;

	bignum_value_heap(&numer, SignPlus, 10);
	bignum_value_heap(&denom, SignPlus, 20);
	make_ratio_heap(&pos, SignMinus, numer, denom);
	numer = denom = Nil;
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	test(! GetStatusDynamic(pos), "make_ratio_heap1");
	test(! GetStatusDynamic(numer), "make_ratio_heap2");
	test(! GetStatusDynamic(denom), "make_ratio_heap3");
	test(IsMinus(RefSignRatio(pos)), "make_ratio_heap4");
	test(equal_value_bignum(numer, SignPlus, 10), "make_ratio_heap5");
	test(equal_value_bignum(denom, SignPlus, 20), "make_ratio_heap6");

	RETURN;
}

static int test_make_ratio_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, numer, denom;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &numer, SignPlus, 10);
	bignum_value_local(local, &denom, SignPlus, 20);
	make_ratio_local(local, &pos, SignMinus, numer, denom);
	numer = denom = Nil;
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	test(GetStatusDynamic(pos), "make_ratio_local1");
	test(GetStatusDynamic(numer), "make_ratio_local2");
	test(GetStatusDynamic(denom), "make_ratio_local3");
	test(IsMinus(RefSignRatio(pos)), "make_ratio_local4");
	test(equal_value_bignum(numer, SignPlus, 10), "make_ratio_local5");
	test(equal_value_bignum(denom, SignPlus, 20), "make_ratio_local6");

	rollback_local(local, stack);

	RETURN;
}

static int test_make_ratio_alloc(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, numer, denom;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &numer, SignPlus, 10);
	bignum_value_local(local, &denom, SignPlus, 20);
	make_ratio_alloc(local, &pos, SignMinus, numer, denom);
	test(GetStatusDynamic(pos), "make_ratio_alloc1");

	bignum_value_heap(&numer, SignPlus, 10);
	bignum_value_heap(&denom, SignPlus, 20);
	make_ratio_alloc(NULL, &pos, SignMinus, numer, denom);
	test(! GetStatusDynamic(pos), "make_ratio_alloc2");

	rollback_local(local, stack);

	RETURN;
}

static int test_make_ratio_alloc_unsafe(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, numer, denom;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &numer, SignPlus, 10);
	bignum_value_local(local, &denom, SignPlus, 20);
	make_ratio_alloc_unsafe(local, &pos, SignMinus, numer, denom);
	test(GetStatusDynamic(pos), "make_ratio_alloc_unsafe1");

	bignum_value_heap(&numer, SignPlus, 10);
	bignum_value_heap(&denom, SignPlus, 20);
	make_ratio_alloc_unsafe(NULL, &pos, SignMinus, numer, denom);
	test(! GetStatusDynamic(pos), "make_ratio_alloc_unsafe2");

	rollback_local(local, stack);

	RETURN;
}

static int test_make_copy_ratio_heap(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, numer, denom;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &numer, SignPlus, 10);
	bignum_value_local(local, &denom, SignPlus, 20);
	make_copy_ratio_heap(&pos, SignMinus, numer, denom);
	numer = denom = Nil;
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	test(! GetStatusDynamic(pos), "make_copy_ratio_heap1");
	test(! GetStatusDynamic(numer), "make_copy_ratio_heap2");
	test(! GetStatusDynamic(denom), "make_copy_ratio_heap3");
	test(IsMinus(RefSignRatio(pos)), "make_copy_ratio_heap4");
	test(equal_value_bignum(numer, SignPlus, 10), "make_copy_ratio_heap5");
	test(equal_value_bignum(denom, SignPlus, 20), "make_copy_ratio_heap6");

	rollback_local(local, stack);

	RETURN;
}

static int test_make_copy_ratio_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, numer, denom;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &numer, SignPlus, 10);
	bignum_value_local(local, &denom, SignPlus, 20);
	make_copy_ratio_local(local, &pos, SignMinus, numer, denom);
	numer = denom = Nil;
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	test(GetStatusDynamic(pos), "make_copy_ratio_local1");
	test(GetStatusDynamic(numer), "make_copy_ratio_local2");
	test(GetStatusDynamic(denom), "make_copy_ratio_local3");
	test(IsMinus(RefSignRatio(pos)), "make_copy_ratio_local4");
	test(equal_value_bignum(numer, SignPlus, 10), "make_copy_ratio_local5");
	test(equal_value_bignum(denom, SignPlus, 20), "make_copy_ratio_local6");

	rollback_local(local, stack);

	RETURN;
}

static int test_ratio_reduction_heap(void)
{
	LocalRoot local;
	LocalStack stack;
	addr numer, denom, numer1, denom1, pos;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_zero_local(local, &numer);
	bignum_value_local(local, &denom, SignPlus, 10);
	ratio_reduction_heap(local, &pos, SignMinus, numer, denom);
	test(GetType(pos) == LISPTYPE_FIXNUM, "ratio_reduction_heap1");
	test(! GetStatusDynamic(pos), "ratio_reduction_heap2");
	test(RefFixnum(pos) == 0, "ratio_reduction_heap3");

	bignum_value_local(local, &numer, SignPlus, 23);
	bignum_value_local(local, &denom, SignPlus, 1);
	ratio_reduction_heap(local, &pos, SignMinus, numer, denom);
	test(GetType(pos) == LISPTYPE_FIXNUM, "ratio_reduction_heap4");
	test(! GetStatusDynamic(pos), "ratio_reduction_heap5");
	test(RefFixnum(pos) == -23, "ratio_reduction_heap6");

	bignum_value_local(local, &numer, SignPlus, 1071);
	bignum_value_local(local, &denom, SignPlus, 1029);
	ratio_reduction_heap(local, &pos, SignMinus, numer, denom);
	test(! GetStatusDynamic(pos), "ratio_reduction_heap7");
	test(GetType(pos) == LISPTYPE_RATIO, "ratio_reduction_heap8");
	test(IsMinus(RefSignRatio(pos)), "ratio_reduction_heap9");
	GetNumerRatio(pos, &numer1);
	GetDenomRatio(pos, &denom1);
	test(equal_value_bignum(numer1, SignPlus, 51), "ratio_reduction_heap10");
	test(equal_value_bignum(denom1, SignPlus, 49), "ratio_reduction_heap11");
	test(! GetStatusDynamic(numer1), "ratio_reduction_heap12");
	test(! GetStatusDynamic(denom1), "ratio_reduction_heap13");

	test(numer != numer1, "ratio_reduction_heap14");
	test(denom != denom1, "ratio_reduction_heap15");
	test(equal_value_bignum(numer, SignPlus, 51), "ratio_reduction_heap16");
	test(equal_value_bignum(denom, SignPlus, 49), "ratio_reduction_heap17");

	rollback_local(local, stack);

	RETURN;
}

static int test_ratio_reduction_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr numer, denom, numer1, denom1, pos;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_zero_local(local, &numer);
	bignum_value_local(local, &denom, SignPlus, 10);
	ratio_reduction_local(local, &pos, SignMinus, numer, denom);
	test(GetType(pos) == LISPTYPE_FIXNUM, "ratio_reduction_local1");
	test(GetStatusDynamic(pos), "ratio_reduction_local2");
	test(RefFixnum(pos) == 0, "ratio_reduction_local3");

	bignum_value_local(local, &numer, SignPlus, 23);
	bignum_value_local(local, &denom, SignPlus, 1);
	ratio_reduction_local(local, &pos, SignMinus, numer, denom);
	test(GetType(pos) == LISPTYPE_FIXNUM, "ratio_reduction_local4");
	test(GetStatusDynamic(pos), "ratio_reduction_local5");
	test(RefFixnum(pos) == -23, "ratio_reduction_local6");

	bignum_value_local(local, &numer, SignPlus, 1071);
	bignum_value_local(local, &denom, SignPlus, 1029);
	ratio_reduction_local(local, &pos, SignMinus, numer, denom);
	test(GetStatusDynamic(pos), "ratio_reduction_local7");
	test(GetType(pos) == LISPTYPE_RATIO, "ratio_reduction_local8");
	test(IsMinus(RefSignRatio(pos)), "ratio_reduction_local9");
	GetNumerRatio(pos, &numer1);
	GetDenomRatio(pos, &denom1);
	test(equal_value_bignum(numer1, SignPlus, 51), "ratio_reduction_local10");
	test(equal_value_bignum(denom1, SignPlus, 49), "ratio_reduction_local11");
	test(GetStatusDynamic(numer1), "ratio_reduction_local12");
	test(GetStatusDynamic(denom1), "ratio_reduction_local13");

	test(numer != numer1, "ratio_reduction_local14");
	test(denom != denom1, "ratio_reduction_local15");
	test(equal_value_bignum(numer, SignPlus, 51), "ratio_reduction_local16");
	test(equal_value_bignum(denom, SignPlus, 49), "ratio_reduction_local17");

	rollback_local(local, stack);

	RETURN;
}

static int test_ratio_noreduction_heap(void)
{
	LocalRoot local;
	LocalStack stack;
	addr numer, denom, numer1, denom1, pos;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_zero_local(local, &numer);
	bignum_value_local(local, &denom, SignPlus, 10);
	ratio_noreduction_heap(&pos, SignMinus, numer, denom);
	test(GetType(pos) == LISPTYPE_FIXNUM, "ratio_noreduction_heap1");
	test(! GetStatusDynamic(pos), "ratio_noreduction_heap2");
	test(RefFixnum(pos) == 0, "ratio_noreduction_heap3");

	bignum_value_local(local, &numer, SignPlus, 23);
	bignum_value_local(local, &denom, SignPlus, 1);
	ratio_noreduction_heap(&pos, SignMinus, numer, denom);
	test(GetType(pos) == LISPTYPE_FIXNUM, "ratio_noreduction_heap4");
	test(! GetStatusDynamic(pos), "ratio_noreduction_heap5");
	test(RefFixnum(pos) == -23, "ratio_noreduction_heap6");

	bignum_value_local(local, &numer, SignPlus, 1071);
	bignum_value_local(local, &denom, SignPlus, 1029);
	ratio_noreduction_heap(&pos, SignMinus, numer, denom);
	test(! GetStatusDynamic(pos), "ratio_noreduction_heap7");
	test(GetType(pos) == LISPTYPE_RATIO, "ratio_noreduction_heap8");
	test(IsMinus(RefSignRatio(pos)), "ratio_noreduction_heap9");
	GetNumerRatio(pos, &numer1);
	GetDenomRatio(pos, &denom1);
	test(equal_value_bignum(numer1, SignPlus, 1071), "ratio_noreduction_heap10");
	test(equal_value_bignum(denom1, SignPlus, 1029), "ratio_noreduction_heap11");
	test(! GetStatusDynamic(numer1), "ratio_noreduction_heap12");
	test(! GetStatusDynamic(denom1), "ratio_noreduction_heap13");

	test(numer != numer1, "ratio_noreduction_heap14");
	test(denom != denom1, "ratio_noreduction_heap15");
	test(equal_value_bignum(numer, SignPlus, 1071), "ratio_noreduction_heap16");
	test(equal_value_bignum(denom, SignPlus, 1029), "ratio_noreduction_heap17");

	rollback_local(local, stack);

	RETURN;
}

static int test_ratio_noreduction_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr numer, denom, numer1, denom1, pos;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_zero_local(local, &numer);
	bignum_value_local(local, &denom, SignPlus, 10);
	ratio_noreduction_local(local, &pos, SignMinus, numer, denom);
	test(GetType(pos) == LISPTYPE_FIXNUM, "ratio_noreduction_local1");
	test(GetStatusDynamic(pos), "ratio_noreduction_local2");
	test(RefFixnum(pos) == 0, "ratio_noreduction_local3");

	bignum_value_local(local, &numer, SignPlus, 23);
	bignum_value_local(local, &denom, SignPlus, 1);
	ratio_noreduction_local(local, &pos, SignMinus, numer, denom);
	test(GetType(pos) == LISPTYPE_FIXNUM, "ratio_noreduction_local4");
	test(GetStatusDynamic(pos), "ratio_noreduction_local5");
	test(RefFixnum(pos) == -23, "ratio_noreduction_local6");

	bignum_value_local(local, &numer, SignPlus, 1071);
	bignum_value_local(local, &denom, SignPlus, 1029);
	ratio_noreduction_local(local, &pos, SignMinus, numer, denom);
	test(GetStatusDynamic(pos), "ratio_noreduction_local7");
	test(GetType(pos) == LISPTYPE_RATIO, "ratio_noreduction_local8");
	test(IsMinus(RefSignRatio(pos)), "ratio_noreduction_local9");
	GetNumerRatio(pos, &numer1);
	GetDenomRatio(pos, &denom1);
	test(equal_value_bignum(numer1, SignPlus, 1071), "ratio_noreduction_local10");
	test(equal_value_bignum(denom1, SignPlus, 1029), "ratio_noreduction_local11");
	test(GetStatusDynamic(numer1), "ratio_noreduction_local12");
	test(GetStatusDynamic(denom1), "ratio_noreduction_local13");

	test(numer != numer1, "ratio_noreduction_local14");
	test(denom != denom1, "ratio_noreduction_local15");
	test(equal_value_bignum(numer, SignPlus, 1071), "ratio_noreduction_local16");
	test(equal_value_bignum(denom, SignPlus, 1029), "ratio_noreduction_local17");

	rollback_local(local, stack);

	RETURN;
}

static int test_ratio_reduction_nocopy_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr numer, denom, numer1, denom1, pos;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_zero_local(local, &numer);
	bignum_value_local(local, &denom, SignPlus, 10);
	ratio_reduction_nocopy_local(local, &pos, SignMinus, numer, denom);
	test(GetType(pos) == LISPTYPE_FIXNUM, "ratio_reduction_nocopy_local1");
	test(GetStatusDynamic(pos), "ratio_reduction_nocopy_local2");
	test(RefFixnum(pos) == 0, "ratio_reduction_nocopy_local3");

	bignum_value_local(local, &numer, SignPlus, 23);
	bignum_value_local(local, &denom, SignPlus, 1);
	ratio_reduction_nocopy_local(local, &pos, SignMinus, numer, denom);
	test(GetType(pos) == LISPTYPE_FIXNUM, "ratio_reduction_nocopy_local4");
	test(GetStatusDynamic(pos), "ratio_reduction_nocopy_local5");
	test(RefFixnum(pos) == -23, "ratio_reduction_nocopy_local6");

	bignum_value_local(local, &numer, SignPlus, 1071);
	bignum_value_local(local, &denom, SignPlus, 1029);
	ratio_reduction_nocopy_local(local, &pos, SignMinus, numer, denom);
	test(GetStatusDynamic(pos), "ratio_reduction_nocopy_local7");
	test(GetType(pos) == LISPTYPE_RATIO, "ratio_reduction_nocopy_local8");
	test(IsMinus(RefSignRatio(pos)), "ratio_reduction_nocopy_local9");
	GetNumerRatio(pos, &numer1);
	GetDenomRatio(pos, &denom1);
	test(equal_value_bignum(numer1, SignPlus, 51), "ratio_reduction_nocopy_local10");
	test(equal_value_bignum(denom1, SignPlus, 49), "ratio_reduction_nocopy_local11");
	test(GetStatusDynamic(numer1), "ratio_reduction_nocopy_local12");
	test(GetStatusDynamic(denom1), "ratio_reduction_nocopy_local13");

	test(numer == numer1, "ratio_reduction_nocopy_local14");
	test(denom == denom1, "ratio_reduction_nocopy_local15");

	rollback_local(local, stack);

	RETURN;
}

static int test_make_ratio_reduction_heap(void)
{
	LocalRoot local;
	LocalStack stack;
	addr numer, denom, numer1, denom1, pos;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_zero_local(local, &numer);
	bignum_value_local(local, &denom, SignPlus, 10);
	make_ratio_reduction_heap(local, &pos, SignMinus, numer, denom);
	test(ratiop(pos), "make_ratio_reduction_heap1");
	test(! GetStatusDynamic(pos), "make_ratio_reduction_heap2");
	test(equal_value_ratio(pos, SignMinus, 0, 10), "make_ratio_reduction_heap3");

	bignum_value_local(local, &numer, SignPlus, 23);
	bignum_value_local(local, &denom, SignPlus, 1);
	make_ratio_reduction_heap(local, &pos, SignMinus, numer, denom);
	test(ratiop(pos), "make_ratio_reduction_heap4");
	test(! GetStatusDynamic(pos), "make_ratio_reduction_heap5");
	test(equal_value_ratio(pos, SignMinus, 23, 1), "make_ratio_reduction_heap6");

	bignum_value_local(local, &numer, SignPlus, 1071);
	bignum_value_local(local, &denom, SignPlus, 1029);
	make_ratio_reduction_heap(local, &pos, SignMinus, numer, denom);
	test(ratiop(pos), "make_ratio_reduction_heap7");
	test(! GetStatusDynamic(pos), "make_ratio_reduction_heap8");
	test(IsMinus(RefSignRatio(pos)), "make_ratio_reduction_heap9");
	GetNumerRatio(pos, &numer1);
	GetDenomRatio(pos, &denom1);
	test(equal_value_bignum(numer1, SignPlus, 51), "make_ratio_reduction_heap10");
	test(equal_value_bignum(denom1, SignPlus, 49), "make_ratio_reduction_heap11");
	test(! GetStatusDynamic(numer1), "make_ratio_reduction_heap12");
	test(! GetStatusDynamic(denom1), "make_ratio_reduction_heap13");

	rollback_local(local, stack);

	RETURN;
}

static int test_make_ratio_reduction_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr numer, denom, numer1, denom1, pos;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_zero_local(local, &numer);
	bignum_value_local(local, &denom, SignPlus, 10);
	make_ratio_reduction_local(local, &pos, SignMinus, numer, denom);
	test(ratiop(pos), "make_ratio_reduction_local1");
	test(GetStatusDynamic(pos), "make_ratio_reduction_local2");
	test(equal_value_ratio(pos, SignMinus, 0, 10), "make_ratio_reduction_local3");

	bignum_value_local(local, &numer, SignPlus, 23);
	bignum_value_local(local, &denom, SignPlus, 1);
	make_ratio_reduction_local(local, &pos, SignMinus, numer, denom);
	test(ratiop(pos), "make_ratio_reduction_local4");
	test(GetStatusDynamic(pos), "make_ratio_reduction_local5");
	test(equal_value_ratio(pos, SignMinus, 23, 1), "make_ratio_reduction_local6");

	bignum_value_local(local, &numer, SignPlus, 1071);
	bignum_value_local(local, &denom, SignPlus, 1029);
	make_ratio_reduction_local(local, &pos, SignMinus, numer, denom);
	test(ratiop(pos), "make_ratio_reduction_local7");
	test(GetStatusDynamic(pos), "make_ratio_reduction_local8");
	test(IsMinus(RefSignRatio(pos)), "make_ratio_reduction_local9");
	GetNumerRatio(pos, &numer1);
	GetDenomRatio(pos, &denom1);
	test(equal_value_bignum(numer1, SignPlus, 51), "make_ratio_reduction_local10");
	test(equal_value_bignum(denom1, SignPlus, 49), "make_ratio_reduction_local11");
	test(GetStatusDynamic(numer1), "make_ratio_reduction_local12");
	test(GetStatusDynamic(denom1), "make_ratio_reduction_local13");

	rollback_local(local, stack);

	RETURN;
}

static int test_ratio_reduction_value_local(void)
{
	addr pos, check;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	ratio_reduction_value_local(local, &pos, SignMinus, 0, 10);
	test(ratiop(pos), "ratio_reduction_value_local1");
	test(GetStatusDynamic(pos), "ratio_reduction_value_local2");
	test(IsMinus(RefSignRatio(pos)), "ratio_reduction_value_local3");
	GetNumerRatio(pos, &check);
	test(equal_value_bignum(check, SignPlus, 0), "ratio_reduction_value_local4");
	GetDenomRatio(pos, &check);
	test(equal_value_bignum(check, SignPlus, 10), "ratio_reduction_value_local5");

	ratio_reduction_value_local(local, &pos, SignPlus, 4, 6);
	test(ratiop(pos), "ratio_reduction_value_local6");
	test(GetStatusDynamic(pos), "ratio_reduction_value_local7");
	test(IsPlus(RefSignRatio(pos)), "ratio_reduction_value_local8");
	GetNumerRatio(pos, &check);
	test(equal_value_bignum(check, SignPlus, 2), "ratio_reduction_value_local9");
	GetDenomRatio(pos, &check);
	test(equal_value_bignum(check, SignPlus, 3), "ratio_reduction_value_local10");

	rollback_local(local, stack);

	RETURN;
}

static int test_ratio_reduction_value_heap(void)
{
	addr pos, check;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	ratio_reduction_value_heap(local, &pos, SignMinus, 0, 10);
	test(ratiop(pos), "ratio_reduction_value_heap1");
	test(! GetStatusDynamic(pos), "ratio_reduction_value_heap2");
	test(IsMinus(RefSignRatio(pos)), "ratio_reduction_value_heap3");
	GetNumerRatio(pos, &check);
	test(equal_value_bignum(check, SignPlus, 0), "ratio_reduction_value_heap4");
	GetDenomRatio(pos, &check);
	test(equal_value_bignum(check, SignPlus, 10), "ratio_reduction_value_heap5");

	ratio_reduction_value_heap(local, &pos, SignPlus, 4, 6);
	test(ratiop(pos), "ratio_reduction_value_heap6");
	test(! GetStatusDynamic(pos), "ratio_reduction_value_heap7");
	test(IsPlus(RefSignRatio(pos)), "ratio_reduction_value_heap8");
	GetNumerRatio(pos, &check);
	test(equal_value_bignum(check, SignPlus, 2), "ratio_reduction_value_heap9");
	GetDenomRatio(pos, &check);
	test(equal_value_bignum(check, SignPlus, 3), "ratio_reduction_value_heap10");

	rollback_local(local, stack);

	RETURN;
}

static int test_ratio_noreduction_value_local(void)
{
	addr pos, check;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	ratio_noreduction_value_local(local, &pos, SignMinus, 0, 10);
	test(ratiop(pos), "ratio_noreduction_value_local1");
	test(GetStatusDynamic(pos), "ratio_noreduction_value_local2");
	test(IsMinus(RefSignRatio(pos)), "ratio_noreduction_value_local3");
	GetNumerRatio(pos, &check);
	test(equal_value_bignum(check, SignPlus, 0), "ratio_noreduction_value_local4");
	GetDenomRatio(pos, &check);
	test(equal_value_bignum(check, SignPlus, 10), "ratio_noreduction_value_local5");

	ratio_noreduction_value_local(local, &pos, SignPlus, 4, 6);
	test(ratiop(pos), "ratio_noreduction_value_local6");
	test(GetStatusDynamic(pos), "ratio_noreduction_value_local7");
	test(IsPlus(RefSignRatio(pos)), "ratio_noreduction_value_local8");
	GetNumerRatio(pos, &check);
	test(equal_value_bignum(check, SignPlus, 4), "ratio_noreduction_value_local9");
	GetDenomRatio(pos, &check);
	test(equal_value_bignum(check, SignPlus, 6), "ratio_noreduction_value_local10");

	rollback_local(local, stack);

	RETURN;
}

static int test_ratio_noreduction_value_heap(void)
{
	addr pos, check;

	ratio_noreduction_value_heap(&pos, SignMinus, 0, 10);
	test(ratiop(pos), "ratio_noreduction_value_heap1");
	test(! GetStatusDynamic(pos), "ratio_noreduction_value_heap2");
	test(IsMinus(RefSignRatio(pos)), "ratio_noreduction_value_heap3");
	GetNumerRatio(pos, &check);
	test(equal_value_bignum(check, SignPlus, 0), "ratio_noreduction_value_heap4");
	GetDenomRatio(pos, &check);
	test(equal_value_bignum(check, SignPlus, 10), "ratio_noreduction_value_heap5");

	ratio_noreduction_value_heap(&pos, SignPlus, 4, 6);
	test(ratiop(pos), "ratio_noreduction_value_heap6");
	test(! GetStatusDynamic(pos), "ratio_noreduction_value_heap7");
	test(IsPlus(RefSignRatio(pos)), "ratio_noreduction_value_heap8");
	GetNumerRatio(pos, &check);
	test(equal_value_bignum(check, SignPlus, 4), "ratio_noreduction_value_heap9");
	GetDenomRatio(pos, &check);
	test(equal_value_bignum(check, SignPlus, 6), "ratio_noreduction_value_heap10");

	RETURN;
}

static int test_ratio_copy_nosign_alloc(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, check, numer, denom;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &numer, SignPlus, 10);
	bignum_value_local(local, &denom, SignPlus, 20);
	make_ratio_local(local, &pos, SignMinus, numer, denom);
	ratio_copy_nosign_alloc(NULL, &check, pos);
	test(pos != check, "ratio_copy_nosign_alloc1");
	test(! GetStatusDynamic(check), "ratio_copy_nosign_alloc2");
	GetNumerRatio(check, &pos);
	test(! GetStatusDynamic(pos), "ratio_copy_nosign_alloc3");
	test(equal_bb_real(numer, pos), "ratio_copy_nosign_alloc4");
	GetDenomRatio(check, &pos);
	test(! GetStatusDynamic(pos), "ratio_copy_nosign_alloc5");
	test(equal_bb_real(denom, pos), "ratio_copy_nosign_alloc6");

	rollback_local(local, stack);

	RETURN;
}

static int test_ratio_copy_alloc(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, check, numer, denom;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &numer, SignPlus, 10);
	bignum_value_local(local, &denom, SignPlus, 20);
	make_ratio_local(local, &pos, SignMinus, numer, denom);
	ratio_copy_alloc(NULL, &check, pos);
	test(pos != check, "ratio_copy_alloc1");
	test(! GetStatusDynamic(check), "ratio_copy_alloc2");
	test(RefSignRatio(pos) == RefSignRatio(check), "ratio_copy_alloc3");
	GetNumerRatio(check, &pos);
	test(! GetStatusDynamic(pos), "ratio_copy_alloc4");
	test(equal_bb_real(numer, pos), "ratio_copy_alloc5");
	GetDenomRatio(check, &pos);
	test(! GetStatusDynamic(pos), "ratio_copy_alloc6");
	test(equal_bb_real(denom, pos), "ratio_copy_alloc7");

	rollback_local(local, stack);

	RETURN;
}

static int test_ratio_throw_heap(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, check, numer, denom;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_heap(&numer, SignPlus, 10);
	bignum_value_heap(&denom, SignPlus, 20);
	make_ratio_heap(&pos, SignMinus, numer, denom);
	ratio_throw_heap(pos, &check);
	test(pos == check, "ratio_throw_heap1");

	bignum_value_local(local, &numer, SignPlus, 10);
	bignum_value_local(local, &denom, SignPlus, 20);
	make_ratio_local(local, &pos, SignMinus, numer, denom);
	ratio_throw_heap(pos, &check);
	test(pos != check, "ratio_throw_heap2");
	test(! GetStatusDynamic(check), "ratio_throw_heap3");
	numer = denom = Nil;
	GetNumerRatio(check, &numer);
	GetDenomRatio(check, &denom);
	test(! GetStatusDynamic(numer), "ratio_throw_heap4");
	test(! GetStatusDynamic(denom), "ratio_throw_heap5");
	test(equal_value_bignum(numer, SignPlus, 10), "ratio_throw_heap6");
	test(equal_value_bignum(denom, SignPlus, 20), "ratio_throw_heap7");

	rollback_local(local, stack);

	RETURN;
}

static int test_ratio_throw_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, check, numer, denom;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &numer, SignPlus, 10);
	bignum_value_local(local, &denom, SignPlus, 20);
	make_ratio_local(local, &pos, SignMinus, numer, denom);
	ratio_throw_local(local, pos, &check);
	test(pos == check, "ratio_throw_local1");

	bignum_value_heap(&numer, SignPlus, 10);
	bignum_value_heap(&denom, SignPlus, 20);
	make_ratio_heap(&pos, SignMinus, numer, denom);
	ratio_throw_local(local, pos, &check);
	test(pos != check, "ratio_throw_local2");
	test(GetStatusDynamic(check), "ratio_throw_local3");
	numer = denom = Nil;
	GetNumerRatio(check, &numer);
	GetDenomRatio(check, &denom);
	test(GetStatusDynamic(numer), "ratio_throw_local4");
	test(GetStatusDynamic(denom), "ratio_throw_local5");
	test(equal_value_bignum(numer, SignPlus, 10), "ratio_throw_local6");
	test(equal_value_bignum(denom, SignPlus, 20), "ratio_throw_local7");

	rollback_local(local, stack);

	RETURN;
}

static int test_ratio_throw_alloc(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, check, numer, denom;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_heap(&numer, SignPlus, 10);
	bignum_value_heap(&denom, SignPlus, 20);
	make_ratio_heap(&pos, SignMinus, numer, denom);
	ratio_throw_heap(pos, &check);
	test(pos == check, "ratio_throw_alloc1");

	bignum_value_local(local, &numer, SignPlus, 10);
	bignum_value_local(local, &denom, SignPlus, 20);
	make_ratio_local(local, &pos, SignMinus, numer, denom);
	ratio_throw_alloc(local, pos, &check);
	test(pos == check, "ratio_throw_alloc2");

	rollback_local(local, stack);

	RETURN;
}

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

static int test_zerop_ratio(void)
{
	LocalRoot local;
	LocalStack stack;
	addr numer, denom, pos;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_zero_local(local, &numer);
	bignum_value_local(local, &denom, SignPlus, 10);
	make_ratio_heap_nocheck(&pos, SignPlus, numer, denom);
	test(zerop_ratio(pos), "zerop_ratio1");
	SetSignRatio(pos, SignMinus);
	test(zerop_ratio(pos), "zerop_ratio2");

	bignum_value_local(local, &numer, SignPlus, 9);
	bignum_value_local(local, &denom, SignPlus, 10);
	make_ratio_heap_nocheck(&pos, SignPlus, numer, denom);
	test(! zerop_ratio(pos), "zerop_ratio3");

	rollback_local(local, stack);

	RETURN;
}

static int test_plusp_ratio(void)
{
	LocalRoot local;
	LocalStack stack;
	addr numer, denom, pos;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_zero_local(local, &numer);
	bignum_value_local(local, &denom, SignPlus, 10);
	make_ratio_heap_nocheck(&pos, SignPlus, numer, denom);
	test(! plusp_ratio(pos), "plusp_ratio1");
	SetSignRatio(pos, SignMinus);
	test(! plusp_ratio(pos), "plusp_ratio2");

	bignum_value_local(local, &numer, SignPlus, 9);
	bignum_value_local(local, &denom, SignPlus, 10);
	make_ratio_heap_nocheck(&pos, SignPlus, numer, denom);
	test(plusp_ratio(pos), "plusp_ratio3");
	SetSignRatio(pos, SignMinus);
	test(! plusp_ratio(pos), "plusp_ratio4");

	rollback_local(local, stack);

	RETURN;
}

static int test_minusp_ratio(void)
{
	LocalRoot local;
	LocalStack stack;
	addr numer, denom, pos;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_zero_local(local, &numer);
	bignum_value_local(local, &denom, SignPlus, 10);
	make_ratio_heap_nocheck(&pos, SignPlus, numer, denom);
	test(! minusp_ratio(pos), "minusp_ratio1");
	SetSignRatio(pos, SignMinus);
	test(! minusp_ratio(pos), "minusp_ratio2");

	bignum_value_local(local, &numer, SignPlus, 9);
	bignum_value_local(local, &denom, SignPlus, 10);
	make_ratio_heap_nocheck(&pos, SignPlus, numer, denom);
	test(! minusp_ratio(pos), "minusp_ratio3");
	SetSignRatio(pos, SignMinus);
	test(minusp_ratio(pos), "minusp_ratio4");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  cast float
 */
static int test_hexfloat_fixed_exponent(void)
{
	size_t a;

	a = hexfloat_fixed_exponent(0);
	test(a == 0, "hexfloat_fixed_exponent1");
	a = hexfloat_fixed_exponent(1);
	test(a == 4, "hexfloat_fixed_exponent2");
	a = hexfloat_fixed_exponent(0xF);
	test(a == 4, "hexfloat_fixed_exponent3");
	a = hexfloat_fixed_exponent(0x10);
	test(a == 8, "hexfloat_fixed_exponent4");
	a = hexfloat_fixed_exponent(0xFFFF);
	test(a == 16, "hexfloat_fixed_exponent5");
	a = hexfloat_fixed_exponent(0x1FFFF);
	test(a == 20, "hexfloat_fixed_exponent6");
	a = hexfloat_fixed_exponent(0xABCDEF);
	test(a == 24, "hexfloat_fixed_exponent7");

	RETURN;
}

static int test_hexfloat_exponent(void)
{
	int i;
	addr pos;
	LocalRoot local;
	LocalStack stack;
	fixed *data;
	size_t a, b;

	local = Local_Thread;
	push_local(local, &stack);
	alloc_bignum(local, &pos, 10);
	SetSizeBignum(pos, 1);
	GetDataBignum(pos, &data);
	for (i = 0; i < 10; i++)
		data[i] = 0;
	data[0] = 1;
	a = hexfloat_exponent(pos);
	test(a == 0, "hexfloat_exponent1");

	SetSizeBignum(pos, 3);
	data[2] = 0xAF;
	a = hexfloat_exponent(pos);
	b = BIGNUM_FULLBIT * 2 + 2*4 - 4;
	test(a == b, "hexfloat_exponent2");

	rollback_local(local, stack);

	RETURN;
}

static int test_diff_exponent_ratio(void)
{
	size_t size1, size2;

	size1 = size2 = 0;
	diff_exponent_ratio_(&size1, &size2, Nil);
	test(size1 == 0, "diff_exponent_ratio1");
	test(size2 == 0, "diff_exponent_ratio2");

	size1 = size2 = 10;
	diff_exponent_ratio_(&size1, &size2, Nil);
	test(size1 == 0, "diff_exponent_ratio3");
	test(size2 == 0, "diff_exponent_ratio4");

	size1 = 5;
	size2 = 7;
	diff_exponent_ratio_(&size1, &size2, Nil);
	test(size1 == 0, "diff_exponent_ratio5");
	test(size2 == 2, "diff_exponent_ratio6");

	size1 = 9;
	size2 = 3;
	diff_exponent_ratio_(&size1, &size2, Nil);
	test(size1 == 6, "diff_exponent_ratio7");
	test(size2 == 0, "diff_exponent_ratio8");

	size1 = 0;
	size2 = 6;
	diff_exponent_ratio_(&size1, &size2, Nil);
	test(size1 == 0, "diff_exponent_ratio9");
	test(size2 == 6, "diff_exponent_ratio10");

	size1 = 6;
	size2 = 0;
	diff_exponent_ratio_(&size1, &size2, Nil);
	test(size1 == 6, "diff_exponent_ratio11");
	test(size2 == 0, "diff_exponent_ratio12");

	RETURN;
}

static int test_hexadecimal_fixed(void)
{
	char ptr[128], *ret;
	size_t bit;

	bit = 0;
	ret = hexadecimal_fixed(ptr, 4, 0, &bit);
	*ret = 0;
	test(strcmp(ptr, "0") == 0, "hexadecimal_fixed1");

	bit = 4;
	ret = hexadecimal_fixed(ptr, 4, 0, &bit);
	*ret = 0;
	test(strcmp(ptr, "0") == 0, "hexadecimal_fixed2");

	bit = 5;
	ret = hexadecimal_fixed(ptr, 4, 0, &bit);
	*ret = 0;
	test(strcmp(ptr, "00") == 0, "hexadecimal_fixed3");

	bit = 16;
	ret = hexadecimal_fixed(ptr, 4, 0, &bit);
	*ret = 0;
	test(strcmp(ptr, "0000") == 0, "hexadecimal_fixed4");

	bit = 16;
	ret = hexadecimal_fixed(ptr, 4, 1, &bit);
	*ret = 0;
	test(strcmp(ptr, "4.") == 0, "hexadecimal_fixed5");

	bit = 16;
	ret = hexadecimal_fixed(ptr, 0x4E, 1, &bit);
	*ret = 0;
	test(strcmp(ptr, "4.E") == 0, "hexadecimal_fixed6");

	RETURN;
}

static int test_hexadecimal_ratio(void)
{
	char ptr[128], *ret;
	addr cons, pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	bigcons_local(local, &cons);
	setchar_bigcons_(local, cons, 16, "ABC0123456789ABCDEF0123456");
	bignum_cons_alloc(local, &pos, SignPlus, cons);
	ret = hexadecimal_ratio(ptr, pos, 508);
	*ret = 0;
	test(strcmp(ptr, "A.BC0123456789ABCDEF0123456") == 0, "hexadecimal_ratio1");

	setchar_bigcons_(local, cons, 16, "ABC0123456789ABCDEF012345678");
	bignum_cons_alloc(local, &pos, SignPlus, cons);
	ret = hexadecimal_ratio(ptr, pos, 508);
	*ret = 0;
	test(strcmp(ptr, "A.BC0123456789ABCDEF012345678") == 0, "hexadecimal_ratio2");

	setchar_bigcons_(local, cons, 16, "ABC0123456789ABCDEF012345678");
	bignum_cons_alloc(local, &pos, SignPlus, cons);
	ret = hexadecimal_ratio(ptr, pos, 22);
	*ret = 0;
	test(strcmp(ptr, "A.BC012") == 0, "hexadecimal_ratio3");

	setchar_bigcons_(local, cons, 16, "F");
	bignum_cons_alloc(local, &pos, SignPlus, cons);
	ret = hexadecimal_ratio(ptr, pos, 100);
	*ret = 0;
	test(strcmp(ptr, "F.") == 0, "hexadecimal_ratio4");

	rollback_local(local, stack);

	RETURN;
}

static int test_float_string_ratio(void)
{
	char ptr[128];
	addr cons, pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	bigcons_local(local, &cons);
	setchar_bigcons_(local, cons, 16, "ABC");
	bignum_cons_alloc(local, &pos, SignPlus, cons);
	float_string_ratio(0, pos, ptr, 10, 50);
	test(strcmp(ptr, "+0xA.BCp10") == 0, "float_string_ratio1");

	rollback_local(local, stack);

	RETURN;
}

static int test_single_float_ratio(void)
{
	addr numer, denom, pos;
	LocalRoot local;
	LocalStack stack;
	single_float value;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_alloc(local, &numer, signplus_bignum, 0);
	bignum_value_alloc(local, &denom, signplus_bignum, 4);
	make_ratio_alloc_unsafe(local, &pos, signplus_bignum, numer, denom);
	single_float_ratio_(pos, &value);
	test(value == 0.0f, "single_float_ratio1");

	bignum_value_alloc(local, &numer, signminus_bignum, 123);
	bignum_value_alloc(local, &denom, signplus_bignum, 1);
	make_ratio_alloc_unsafe(local, &pos, signplus_bignum, numer, denom);
	single_float_ratio_(pos, &value);
	test(value == 123.0f, "single_float_ratio2");

	bignum_value_alloc(local, &numer, signplus_bignum, 1);
	bignum_value_alloc(local, &denom, signminus_bignum, 4);
	make_ratio_alloc_unsafe(local, &pos, signminus_bignum, numer, denom);
	single_float_ratio_(pos, &value);
	test(value == -0.25f, "single_float_ratio3");

	bignum_value_alloc(local, &numer, signminus_bignum, 123);
	bignum_value_alloc(local, &denom, signminus_bignum, 984);
	make_ratio_alloc_unsafe(local, &pos, signminus_bignum, numer, denom);
	single_float_ratio_(pos, &value);
	test(value == -0.125f, "single_float_ratio4");

	rollback_local(local, stack);

	RETURN;
}

static int test_double_float_ratio(void)
{
	addr numer, denom, pos;
	LocalRoot local;
	LocalStack stack;
	double_float value;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_alloc(local, &numer, signplus_bignum, 0);
	bignum_value_alloc(local, &denom, signplus_bignum, 4);
	make_ratio_alloc_unsafe(local, &pos, signplus_bignum, numer, denom);
	double_float_ratio_(pos, &value);
	test(value == 0.0, "double_float_ratio1");

	bignum_value_alloc(local, &numer, signminus_bignum, 123);
	bignum_value_alloc(local, &denom, signplus_bignum, 1);
	make_ratio_alloc_unsafe(local, &pos, signplus_bignum, numer, denom);
	double_float_ratio_(pos, &value);
	test(value == 123.0, "double_float_ratio2");

	bignum_value_alloc(local, &numer, signplus_bignum, 1);
	bignum_value_alloc(local, &denom, signminus_bignum, 4);
	make_ratio_alloc_unsafe(local, &pos, signminus_bignum, numer, denom);
	double_float_ratio_(pos, &value);
	test(value == -0.25, "double_float_ratio3");

	bignum_value_alloc(local, &numer, signminus_bignum, 123);
	bignum_value_alloc(local, &denom, signminus_bignum, 984);
	make_ratio_alloc_unsafe(local, &pos, signminus_bignum, numer, denom);
	double_float_ratio_(pos, &value);
	test(value == -0.125, "double_float_ratio4");

	rollback_local(local, stack);

	RETURN;
}

static int test_long_float_ratio(void)
{
	addr numer, denom, pos;
	LocalRoot local;
	LocalStack stack;
	long_float value;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_alloc(local, &numer, signplus_bignum, 0);
	bignum_value_alloc(local, &denom, signplus_bignum, 4);
	make_ratio_alloc_unsafe(local, &pos, signplus_bignum, numer, denom);
	long_float_ratio_(pos, &value);
	test(value == 0.0L, "long_float_ratio1");

	bignum_value_alloc(local, &numer, signminus_bignum, 123);
	bignum_value_alloc(local, &denom, signplus_bignum, 1);
	make_ratio_alloc_unsafe(local, &pos, signplus_bignum, numer, denom);
	long_float_ratio_(pos, &value);
	test(value == 123.0L, "long_float_ratio2");

	bignum_value_alloc(local, &numer, signplus_bignum, 1);
	bignum_value_alloc(local, &denom, signminus_bignum, 4);
	make_ratio_alloc_unsafe(local, &pos, signminus_bignum, numer, denom);
	long_float_ratio_(pos, &value);
	test(value == -0.25L, "long_float_ratio3");

	bignum_value_alloc(local, &numer, signminus_bignum, 123);
	bignum_value_alloc(local, &denom, signminus_bignum, 984);
	make_ratio_alloc_unsafe(local, &pos, signminus_bignum, numer, denom);
	long_float_ratio_(pos, &value);
	test(value == -0.125L, "long_float_ratio4");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  output
 */
static int equalstream(addr stream, const char *right)
{
	int result;
	addr left;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	string_stream_local_(local, stream, &left);
	result = string_equal_char_debug(left, right);
	rollback_local(local, stack);
	clear_output_string_stream(stream);

	return result;
}

static void test_ratio_alloc(LocalRoot local,
		addr *ret, int sign, fixed v1, fixed v2)
{
	addr numer, denom;
	bignum_value_alloc(local, &numer, SignPlus, v1);
	bignum_value_alloc(local, &denom, SignPlus, v2);
	make_ratio_alloc(local, ret, sign, numer, denom);
}

static int test_output_nosign_ratio(void)
{
	LocalRoot local;
	LocalStack stack;
	addr stream, pos;

	local = Local_Thread;
	push_local(local, &stack);

	open_output_string_stream(&stream, 0);
	test_ratio_alloc(local, &pos, SignPlus, 0, 20);
	output_nosign_ratio_(local, stream, pos, 10, 1);
	test(equalstream(stream, "0"), "output_nosign_ratio1");

	clear_output_string_stream(stream);
	test_ratio_alloc(local, &pos, SignMinus, 10, 1);
	output_nosign_ratio_(local, stream, pos, 10, 1);
	test(equalstream(stream, "10"), "output_nosign_ratio2");

	clear_output_string_stream(stream);
	test_ratio_alloc(local, &pos, SignMinus, 7, 8);
	output_nosign_ratio_(local, stream, pos, 10, 1);
	test(equalstream(stream, "7/8"), "output_nosign_ratio3");

	close_output_string_stream(stream);
	rollback_local(local, stack);

	RETURN;
}


/*
 *  Main
 */
static int testcase_ratio(void)
{
	/* operation */
	TestBreak(test_reduction_single);
	TestBreak(test_reduction_multiple);
	TestBreak(test_reduction_local);
	TestBreak(test_make_ratio_heap);
	TestBreak(test_make_ratio_local);
	TestBreak(test_make_ratio_alloc);
	TestBreak(test_make_ratio_alloc_unsafe);
	TestBreak(test_make_copy_ratio_heap);
	TestBreak(test_make_copy_ratio_local);
	TestBreak(test_ratio_reduction_heap);
	TestBreak(test_ratio_reduction_local);
	TestBreak(test_ratio_noreduction_heap);
	TestBreak(test_ratio_noreduction_local);
	TestBreak(test_ratio_reduction_nocopy_local);
	TestBreak(test_make_ratio_reduction_heap);
	TestBreak(test_make_ratio_reduction_local);
	TestBreak(test_ratio_reduction_value_local);
	TestBreak(test_ratio_reduction_value_heap);
	TestBreak(test_ratio_noreduction_value_local);
	TestBreak(test_ratio_noreduction_value_heap);
	TestBreak(test_ratio_copy_nosign_alloc);
	TestBreak(test_ratio_copy_alloc);
	TestBreak(test_ratio_throw_heap);
	TestBreak(test_ratio_throw_local);
	TestBreak(test_ratio_throw_alloc);
	TestBreak(test_zerop_ratio);
	TestBreak(test_plusp_ratio);
	TestBreak(test_minusp_ratio);
	/* cast float */
	TestBreak(test_hexfloat_fixed_exponent);
	TestBreak(test_hexfloat_exponent);
	TestBreak(test_diff_exponent_ratio);
	TestBreak(test_hexadecimal_fixed);
	TestBreak(test_hexadecimal_ratio);
	TestBreak(test_float_string_ratio);
	TestBreak(test_single_float_ratio);
	TestBreak(test_double_float_ratio);
	TestBreak(test_long_float_ratio);
	/* output */
	TestBreak(test_output_nosign_ratio);

	return 0;
}

static void testinit_ratio(Execute ptr)
{
	build_lisproot(ptr);
	build_constant();
	build_object();
}

int test_ratio(void)
{
	DegradeTitle;
	return DegradeCode(ratio);
}

