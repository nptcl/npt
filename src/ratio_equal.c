#include <math.h>
#include "bignum.h"
#include "bignum_data.h"
#include "bignum_equal.h"
#include "bignum_multi.h"
#include "bignum_object.h"
#include "condition.h"
#include "memory.h"
#include "ratio.h"
#include "real_equal.h"
#include "typedef.h"

_g int equal_value_nosign_ratio(addr pos, bigtype numer, bigtype denom)
{
	addr pos1, pos2;

	CheckType(pos, LISPTYPE_RATIO);
	GetNumerRatio(pos, &pos1);
	GetDenomRatio(pos, &pos2);
	return equal_value_nosign_bignum(pos1, numer)
		&& equal_value_nosign_bignum(pos2, denom);
}

_g int equal_value_ratio(addr pos, int sign, bigtype numer, bigtype denom)
{
	CheckType(pos, LISPTYPE_RATIO);
	return (RefSignRatio(pos) == sign)
		&& equal_value_nosign_ratio(pos, numer, denom);
}

_g int equal_fr_real(addr left, addr right)
{
	int sign;
	addr pos;
	bigtype value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	/* denom == 1 */
	GetDenomRatio(right, &pos);
	if (! equal_value_nosign_bignum(pos, 1))
		return 0;
	/* numer */
	GetNumerRatio(right, &pos);
	castfixed_fixnum(left, &sign, &value);
	if (! equal_value_nosign_bignum(pos, value))
		return 0;
	/* sign */
	return RefSignRatio(right) == sign;
}

_g int equal_br_real(addr left, addr right)
{
	addr pos;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	/* denom == 1 */
	GetDenomRatio(right, &pos);
	if (! equal_value_nosign_bignum(pos, 1))
		return 0;
	/* numer */
	GetNumerRatio(right, &pos);
	if (! equal_bigdata(left, pos))
		return 0;
	/* sign */
	return RefSignBignum(left) == RefSignRatio(right);
}

_g int equal_rr_real(addr left, addr right)
{
	addr pos1, pos2;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	/* sign */
	if (RefSignRatio(left) != RefSignRatio(right))
		return 0;
	/* numer */
	GetNumerRatio(left, &pos1);
	GetNumerRatio(right, &pos2);
	if (! equal_bb_real(pos1, pos2))
		return 0;
	/* denom */
	GetDenomRatio(left, &pos1);
	GetDenomRatio(right, &pos2);
	return equal_bb_real(pos1, pos2);
}

static void split_single_float(single_float value, int *rs, int *re, single_float *rv)
{
	int exp;

	if (value < 0.0f) {
		*rs = SignMinus;
		value = -value;
	}
	else {
		*rs = SignPlus;
	}
	value = frexpf(value, &exp);
	while (! IsIntegerFloat(value)) {
		value *= 2.0f;
		exp--;
	}
	*re = exp;
	*rv = value;
}

static void split_double_float(double_float value, int *rs, int *re, double_float *rv)
{
	int exp;

	if (value < 0) {
		*rs = SignMinus;
		value = -value;
	}
	else {
		*rs = SignPlus;
	}
	value = frexp(value, &exp);
	while (! IsIntegerDouble(value)) {
		value *= 2.0;
		exp--;
	}
	*re = exp;
	*rv = value;
}

static void split_long_float(long_float value, int *rs, int *re, long_float *rv)
{
	int exp;

	if (value < 0.0L) {
		*rs = SignMinus;
		value = -value;
	}
	else {
		*rs = SignPlus;
	}
	value = frexpl(value, &exp);
	while (! IsIntegerLongFloat(value)) {
		value *= 2.0L;
		exp--;
	}
	*re = exp;
	*rv = value;
}

static void rational_return_local(LocalRoot local,
		int sign, int exp, addr numer, addr *ret)
{
	addr denom;

	if (exp == 0) {
		SetSignBignum(numer, sign);
		bignum_result_local(local, numer, ret);
	}
	else if (0 < exp) {
		SetSignBignum(numer, sign);
		shiftup_bignum_local(local, ret, numer, (size_t)exp);
	}
	else {
		power2_bigdata_alloc(local, &denom, (size_t)-exp);
		ratio_reduction_nocopy_local(local, ret, sign, numer, denom);
	}
}

static int rational_float_single_local_(LocalRoot local, single_float value, addr *ret)
{
	int sign, exponent;
	addr numer;

	split_single_float(value, &sign, &exponent, &value);
	Return(bignum_single_float_local_(local, value, &numer, NULL));
	rational_return_local(local, sign, exponent, numer, ret);

	return 0;
}

static int rational_float_double_local_(LocalRoot local, double_float value, addr *ret)
{
	int sign, exponent;
	addr numer;

	split_double_float(value, &sign, &exponent, &value);
	Return(bignum_double_float_local_(local, value, &numer, NULL));
	rational_return_local(local, sign, exponent, numer, ret);

	return 0;
}

static int rational_float_long_local_(LocalRoot local, long_float value, addr *ret)
{
	int sign, exponent;
	addr numer;

	split_long_float(value, &sign, &exponent, &value);
	Return(bignum_long_float_local_(local, value, &numer, NULL));
	rational_return_local(local, sign, exponent, numer, ret);

	return 0;
}

static int equal_ratio_type(addr left, addr right)
{
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return equal_fr_real(right, left);

		case LISPTYPE_BIGNUM:
			return equal_br_real(right, left);

		case LISPTYPE_RATIO:
			return equal_rr_real(left, right);

		default:
			TypeError(right, INTEGER);
			break;
	}

	return 0;
}

_g int equal_rs_real_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	LocalStack stack;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	push_local(local, &stack);
	Return(rational_float_single_local_(local, RefSingleFloat(right), &right));
	check = equal_ratio_type(left, right);
	rollback_local(local, stack);

	return Result(ret, check);
}

_g int equal_rd_real_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	LocalStack stack;

	push_local(local, &stack);
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	Return(rational_float_double_local_(local, RefDoubleFloat(right), &right));
	check = equal_ratio_type(left, right);
	rollback_local(local, stack);

	return Result(ret, check);
}

_g int equal_rl_real_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	LocalStack stack;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	push_local(local, &stack);
	Return(rational_float_long_local_(local, RefLongFloat(right), &right));
	check = equal_ratio_type(left, right);
	rollback_local(local, stack);

	return Result(ret, check);
}

static int compare_bigtype_bignum(bigtype left, addr right)
{
	bigtype value;

	if (1 < RefSizeBignum(right))
		return -1;
	GetRootBignum(right, &right);
	value = PtrDataBignum(right)[0];
	if (left < value)
		return -1;
	if (left > value)
		return 1;

	return 0;
}

static int compare_bigtype_ratio_nosign(LocalRoot local, bigtype left, addr right)
{
	int result;
	addr numer, pos;
	LocalStack stack;
	size_t size;

	/* denom == 1 */
	GetNumerRatio(right, &numer);
	GetDenomRatio(right, &right);
	if (equal_value_nosign_bignum(right, 1))
		return compare_bigtype_bignum(left, numer);

	/* compare */
	push_local(local, &stack);
	GetSizeBignum(numer, &size);
	alloc_bignum(local, &pos, size + 1);
	setmultivalue_bigdata(pos, right, left);
	Check(IsMinus(RefSignBignum(pos)), "sign pos error");
	Check(IsMinus(RefSignBignum(numer)), "sign numer error");
	result = compare_bigdata(pos, numer);
	rollback_local(local, stack);

	return result;
}

_g int compare_fr_real(LocalRoot local, addr left, addr right)
{
	int sign1, sign2, result;
	bigtype value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	castfixed_fixnum(left, &sign1, &value);
	result = zerop_ratio(right);
	if (value == 0) {
		if (result)
			return 0;
		GetSignRatio(right, &sign2);
		return IsPlus(sign2)? -1: 1;
	}
	if (result) {
		return IsPlus(sign1)? 1: -1;
	}
	GetSignRatio(right, &sign2);
	if (IsPlus(sign1) && IsMinus(sign2))
		return 1;
	if (IsMinus(sign1) && IsPlus(sign2))
		return -1;
	result = compare_bigtype_ratio_nosign(local, value, right);

	return IsPlus(sign1)? result: -result;
}

_g int compare_rf_real(LocalRoot local, addr left, addr right)
{
	return -compare_fr_real(local, right, left);
}

static int compare_bigdata_ratio_nosign(LocalRoot local, addr left, addr right)
{
	int result;
	addr numer;
	LocalStack stack;

	GetNumerRatio(right, &numer);
	GetDenomRatio(right, &right);
	if (equal_value_nosign_bignum(right, 1))
		return compare_bigdata(left, numer);

	push_local(local, &stack);
	multi_bb_nosign_bignum_local(local, left, right, &left);
	result = compare_bigdata(left, numer);
	rollback_local(local, stack);

	return result;
}

_g int compare_br_real(LocalRoot local, addr left, addr right)
{
	int sign1, sign2, result, check;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	check = zerop_ratio(right);
	if (zerop_bignum(left)) {
		if (check)
			return 0;
		GetSignRatio(right, &sign2);
		return IsPlus(sign2)? -1: 1;
	}
	if (check) {
		GetSignBignum(left, &sign1);
		return IsPlus(sign1)? 1: -1;
	}
	GetSignBignum(left, &sign1);
	GetSignRatio(right, &sign2);
	if (IsPlus(sign1) && IsMinus(sign2))
		return 1;
	if (IsMinus(sign1) && IsPlus(sign2))
		return -1;
	result = compare_bigdata_ratio_nosign(local, left, right);

	return IsPlus(sign1)? result: -result;
}

_g int compare_rb_real(LocalRoot local, addr left, addr right)
{
	return -compare_br_real(local, right, left);
}

static int compare_ratio_local(LocalRoot local, addr left, addr right)
{
	int result;
	addr denom1, denom2;
	LocalStack stack;

	GetDenomRatio(left, &denom1);
	GetDenomRatio(right, &denom2);
	push_local(local, &stack);
	bignum_copy_local(local, &denom1, denom1);
	bignum_copy_local(local, &denom2, denom2);
	reduction_local(local, denom1, denom2);
	/* cross multiple */
	GetNumerRatio(left, &left);
	GetNumerRatio(right, &right);
	multi_bb_nosign_bignum_local(local, left, denom2, &left);
	multi_bb_nosign_bignum_local(local, right, denom1, &right);
	result = compare_bigdata(left, right);
	rollback_local(local, stack);

	return result;
}

_g int compare_rr_real(LocalRoot local, addr left, addr right)
{
	int check1, check2, sign1, sign2;
	addr denom1, denom2;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	/* zero check */
	check1 = zerop_ratio(left);
	check2 = zerop_ratio(right);
	if (check1 && check2)
		return 0;
	if (check1) {
		GetSignRatio(right, &sign2);
		return IsPlus(sign2)? -1: 1;
	}
	if (check2) {
		GetSignRatio(left, &sign1);
		return IsPlus(sign1)? 1: -1;
	}

	/* sign check */
	GetSignRatio(left, &sign1);
	GetSignRatio(right, &sign2);
	if (IsPlus(sign1) && IsMinus(sign2))
		return 1;
	if (IsMinus(sign1) && IsPlus(sign2))
		return -1;

	/* denom check */
	GetDenomRatio(left, &denom1);
	GetDenomRatio(right, &denom2);
	if (equal_bb_real(denom1, denom2)) {
		GetNumerRatio(left, &left);
		GetNumerRatio(right, &right);
		check1 = compare_bigdata(left, right);
		return IsPlus(sign1)? check1: -check1;
	}

	/* compare */
	check1 = compare_ratio_local(local, left, right);
	return IsPlus(sign1)? check1: -check1;
}

_g int compare_rs_real_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	LocalStack stack;

	push_local(local, &stack);
	Return(rational_float_single_local_(local, RefSingleFloat(right), &right));
	Return(compare_ratio_real_(local, left, right, &check));
	rollback_local(local, stack);

	return Result(ret, check);
}

_g int compare_rd_real_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	LocalStack stack;

	push_local(local, &stack);
	Return(rational_float_double_local_(local, RefDoubleFloat(right), &right));
	Return(compare_ratio_real_(local, left, right, &check));
	rollback_local(local, stack);

	return Result(ret, check);
}

_g int compare_rl_real_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	LocalStack stack;

	push_local(local, &stack);
	Return(rational_float_long_local_(local, RefLongFloat(right), &right));
	Return(compare_ratio_real_(local, left, right, &check));
	rollback_local(local, stack);

	return Result(ret, check);
}

_g int compare_sr_real_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	Return(compare_rs_real_(local, right, left, &check));
	return Result(ret, -check);
}

_g int compare_dr_real_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	Return(compare_rd_real_(local, right, left, &check));
	return Result(ret, -check);
}

_g int compare_lr_real_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	Return(compare_rl_real_(local, right, left, &check));
	return Result(ret, -check);
}

