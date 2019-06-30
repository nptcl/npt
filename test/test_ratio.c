#include "ratio.c"
#include "bigcons.h"
#include "constant.h"
#include "degrade.h"
#include "strtype.h"
#include "stream_string.h"

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
	setchar_bigcons(local, cons, 16, "AC8E074D2AC4CCD83E7C158CAB2BDE62A9BD0");
	bignum_cons_local(local, &left, SignPlus, cons);
	setchar_bigcons(local, cons, 16, "13429222C878719C2E13D04BEE1AEE280815CF00");
	bignum_cons_local(local, &right, SignPlus, cons);
	reduction_multiple(local, left, right);
	setchar_bigcons(local, cons, 16, "10FAD49DA1EAB68EDE0E49F7717B21AC5");
	bignum_cons_local(local, &check, SignPlus, cons);
	test(equal_bb_real(left, check), "reduction_multiple3");
	setchar_bigcons(local, cons, 16, "1E52E158A845559AE45A9B18141794A4A70");
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
	setchar_bigcons(local, cons, 16, "AC8E074D2AC4CCD83E7C158CAB2BDE62A9BD0");
	bignum_cons_local(local, &left, SignPlus, cons);
	setchar_bigcons(local, cons, 16, "13429222C878719C2E13D04BEE1AEE280815CF00");
	bignum_cons_local(local, &right, SignPlus, cons);
	reduction_local(local, left, right);
	setchar_bigcons(local, cons, 16, "10FAD49DA1EAB68EDE0E49F7717B21AC5");
	bignum_cons_local(local, &check, SignPlus, cons);
	test(equal_bb_real(left, check), "reduction_local3");
	setchar_bigcons(local, cons, 16, "1E52E158A845559AE45A9B18141794A4A70");
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
 *  cast float
 */
static int test_hexfloat_bigtype_exponent(void)
{
	size_t a;

	a = hexfloat_bigtype_exponent(0);
	test(a == 0, "hexfloat_bigtype_exponent1");
	a = hexfloat_bigtype_exponent(1);
	test(a == 4, "hexfloat_bigtype_exponent2");
	a = hexfloat_bigtype_exponent(0xF);
	test(a == 4, "hexfloat_bigtype_exponent3");
	a = hexfloat_bigtype_exponent(0x10);
	test(a == 8, "hexfloat_bigtype_exponent4");
	a = hexfloat_bigtype_exponent(0xFFFF);
	test(a == 16, "hexfloat_bigtype_exponent5");
	a = hexfloat_bigtype_exponent(0x1FFFF);
	test(a == 20, "hexfloat_bigtype_exponent6");
	a = hexfloat_bigtype_exponent(0xABCDEF);
	test(a == 24, "hexfloat_bigtype_exponent7");

	RETURN;
}

static int test_hexfloat_exponent(void)
{
	int i;
	addr pos;
	LocalRoot local;
	LocalStack stack;
	bigtype *data;
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
	diff_exponent_ratio(&size1, &size2, Nil);
	test(size1 == 0, "diff_exponent_ratio1");
	test(size2 == 0, "diff_exponent_ratio2");

	size1 = size2 = 10;
	diff_exponent_ratio(&size1, &size2, Nil);
	test(size1 == 0, "diff_exponent_ratio3");
	test(size2 == 0, "diff_exponent_ratio4");

	size1 = 5;
	size2 = 7;
	diff_exponent_ratio(&size1, &size2, Nil);
	test(size1 == 0, "diff_exponent_ratio5");
	test(size2 == 2, "diff_exponent_ratio6");

	size1 = 9;
	size2 = 3;
	diff_exponent_ratio(&size1, &size2, Nil);
	test(size1 == 6, "diff_exponent_ratio7");
	test(size2 == 0, "diff_exponent_ratio8");

	size1 = 0;
	size2 = 6;
	diff_exponent_ratio(&size1, &size2, Nil);
	test(size1 == 0, "diff_exponent_ratio9");
	test(size2 == 6, "diff_exponent_ratio10");

	size1 = 6;
	size2 = 0;
	diff_exponent_ratio(&size1, &size2, Nil);
	test(size1 == 6, "diff_exponent_ratio11");
	test(size2 == 0, "diff_exponent_ratio12");

	RETURN;
}

static int test_hexadecimal_bigtype(void)
{
	char ptr[128], *ret;
	size_t bit;

	bit = 0;
	ret = hexadecimal_bigtype(ptr, 4, 0, &bit);
	*ret = 0;
	test(strcmp(ptr, "0") == 0, "hexadecimal_bigtype1");

	bit = 4;
	ret = hexadecimal_bigtype(ptr, 4, 0, &bit);
	*ret = 0;
	test(strcmp(ptr, "0") == 0, "hexadecimal_bigtype2");

	bit = 5;
	ret = hexadecimal_bigtype(ptr, 4, 0, &bit);
	*ret = 0;
	test(strcmp(ptr, "00") == 0, "hexadecimal_bigtype3");

	bit = 16;
	ret = hexadecimal_bigtype(ptr, 4, 0, &bit);
	*ret = 0;
	test(strcmp(ptr, "0000") == 0, "hexadecimal_bigtype4");

	bit = 16;
	ret = hexadecimal_bigtype(ptr, 4, 1, &bit);
	*ret = 0;
	test(strcmp(ptr, "4.") == 0, "hexadecimal_bigtype5");

	bit = 16;
	ret = hexadecimal_bigtype(ptr, 0x4E, 1, &bit);
	*ret = 0;
	test(strcmp(ptr, "4.E") == 0, "hexadecimal_bigtype6");

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
	setchar_bigcons(local, cons, 16, "ABC0123456789ABCDEF0123456");
	bignum_cons_alloc(local, &pos, SignPlus, cons);
	ret = hexadecimal_ratio(ptr, pos, 508);
	*ret = 0;
	test(strcmp(ptr, "A.BC0123456789ABCDEF0123456") == 0, "hexadecimal_ratio1");

	setchar_bigcons(local, cons, 16, "ABC0123456789ABCDEF012345678");
	bignum_cons_alloc(local, &pos, SignPlus, cons);
	ret = hexadecimal_ratio(ptr, pos, 508);
	*ret = 0;
	test(strcmp(ptr, "A.BC0123456789ABCDEF012345678") == 0, "hexadecimal_ratio2");

	setchar_bigcons(local, cons, 16, "ABC0123456789ABCDEF012345678");
	bignum_cons_alloc(local, &pos, SignPlus, cons);
	ret = hexadecimal_ratio(ptr, pos, 22);
	*ret = 0;
	test(strcmp(ptr, "A.BC012") == 0, "hexadecimal_ratio3");

	setchar_bigcons(local, cons, 16, "F");
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
	setchar_bigcons(local, cons, 16, "ABC");
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

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_alloc(local, &numer, signplus_bignum, 0);
	bignum_value_alloc(local, &denom, signplus_bignum, 4);
	make_ratio_alloc_unsafe(local, &pos, signplus_bignum, numer, denom);
	test(single_float_ratio(pos) == 0.0f, "single_float_ratio1");

	bignum_value_alloc(local, &numer, signminus_bignum, 123);
	bignum_value_alloc(local, &denom, signplus_bignum, 1);
	make_ratio_alloc_unsafe(local, &pos, signplus_bignum, numer, denom);
	test(single_float_ratio(pos) == 123.0f, "single_float_ratio2");

	bignum_value_alloc(local, &numer, signplus_bignum, 1);
	bignum_value_alloc(local, &denom, signminus_bignum, 4);
	make_ratio_alloc_unsafe(local, &pos, signminus_bignum, numer, denom);
	test(single_float_ratio(pos) == -0.25f, "single_float_ratio3");

	bignum_value_alloc(local, &numer, signminus_bignum, 123);
	bignum_value_alloc(local, &denom, signminus_bignum, 984);
	make_ratio_alloc_unsafe(local, &pos, signminus_bignum, numer, denom);
	test(single_float_ratio(pos) == -0.125f, "single_float_ratio4");

	rollback_local(local, stack);

	RETURN;
}

static int test_double_float_ratio(void)
{
	addr numer, denom, pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_alloc(local, &numer, signplus_bignum, 0);
	bignum_value_alloc(local, &denom, signplus_bignum, 4);
	make_ratio_alloc_unsafe(local, &pos, signplus_bignum, numer, denom);
	test(double_float_ratio(pos) == 0.0, "double_float_ratio1");

	bignum_value_alloc(local, &numer, signminus_bignum, 123);
	bignum_value_alloc(local, &denom, signplus_bignum, 1);
	make_ratio_alloc_unsafe(local, &pos, signplus_bignum, numer, denom);
	test(double_float_ratio(pos) == 123.0, "double_float_ratio2");

	bignum_value_alloc(local, &numer, signplus_bignum, 1);
	bignum_value_alloc(local, &denom, signminus_bignum, 4);
	make_ratio_alloc_unsafe(local, &pos, signminus_bignum, numer, denom);
	test(double_float_ratio(pos) == -0.25, "double_float_ratio3");

	bignum_value_alloc(local, &numer, signminus_bignum, 123);
	bignum_value_alloc(local, &denom, signminus_bignum, 984);
	make_ratio_alloc_unsafe(local, &pos, signminus_bignum, numer, denom);
	test(double_float_ratio(pos) == -0.125, "double_float_ratio4");

	rollback_local(local, stack);

	RETURN;
}

static int test_long_float_ratio(void)
{
	addr numer, denom, pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_alloc(local, &numer, signplus_bignum, 0);
	bignum_value_alloc(local, &denom, signplus_bignum, 4);
	make_ratio_alloc_unsafe(local, &pos, signplus_bignum, numer, denom);
	test(long_float_ratio(pos) == 0.0L, "long_float_ratio1");

	bignum_value_alloc(local, &numer, signminus_bignum, 123);
	bignum_value_alloc(local, &denom, signplus_bignum, 1);
	make_ratio_alloc_unsafe(local, &pos, signplus_bignum, numer, denom);
	test(long_float_ratio(pos) == 123.0L, "long_float_ratio2");

	bignum_value_alloc(local, &numer, signplus_bignum, 1);
	bignum_value_alloc(local, &denom, signminus_bignum, 4);
	make_ratio_alloc_unsafe(local, &pos, signminus_bignum, numer, denom);
	test(long_float_ratio(pos) == -0.25L, "long_float_ratio3");

	bignum_value_alloc(local, &numer, signminus_bignum, 123);
	bignum_value_alloc(local, &denom, signminus_bignum, 984);
	make_ratio_alloc_unsafe(local, &pos, signminus_bignum, numer, denom);
	test(long_float_ratio(pos) == -0.125L, "long_float_ratio4");

	rollback_local(local, stack);

	RETURN;
}


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
 *  multiple
 */
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
	string_stream_local(local, stream, &left);
	result = string_equal_char(left, right);
	rollback_local(local, stack);
	clear_output_string_stream(stream);

	return result;
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
	output_nosign_ratio(local, stream, pos, 10, 1);
	test(equalstream(stream, "0"), "output_nosign_ratio1");

	clear_output_string_stream(stream);
	test_ratio_alloc(local, &pos, SignMinus, 10, 1);
	output_nosign_ratio(local, stream, pos, 10, 1);
	test(equalstream(stream, "10"), "output_nosign_ratio2");

	clear_output_string_stream(stream);
	test_ratio_alloc(local, &pos, SignMinus, 7, 8);
	output_nosign_ratio(local, stream, pos, 10, 1);
	test(equalstream(stream, "7/8"), "output_nosign_ratio3");

	close_stream(stream);
	rollback_local(local, stack);

	RETURN;
}


/*
 *  Main
 */
static int testbreak_ratio(void)
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
	/* cast float */
	TestBreak(test_hexfloat_bigtype_exponent);
	TestBreak(test_hexfloat_exponent);
	TestBreak(test_diff_exponent_ratio);
	TestBreak(test_hexadecimal_bigtype);
	TestBreak(test_hexadecimal_ratio);
	TestBreak(test_float_string_ratio);
	TestBreak(test_single_float_ratio);
	TestBreak(test_double_float_ratio);
	TestBreak(test_long_float_ratio);
	/* calcuration */
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
	/* multiple */
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
	/* output */
	TestBreak(test_output_nosign_ratio);

	return 0;
}

int test_ratio(void)
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
		result = testbreak_ratio();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

