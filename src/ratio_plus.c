#include "bigdata.h"
#include "bignum.h"
#include "memory.h"
#include "ratio.h"
#include "ratio_equal.h"
#include "typedef.h"

_g void sign_reverse_ratio_inplace(addr pos)
{
	int sign;

	CheckType(pos, LISPTYPE_RATIO);
	GetSignRatio(pos, &sign);
	sign = SignNot(sign);
	SetSignRatio(pos, sign);
}

_g void sign_reverse_ratio_local(LocalRoot local, addr pos, addr *ret)
{
	int sign;
	addr numer, denom;

	Check(local == NULL, "local error");
	GetSignRatio(pos, &sign);
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	if (! GetStatusDynamic(pos)) {
		bignum_copy_local(local, &numer, numer);
		bignum_copy_local(local, &denom, denom);
	}
	make_ratio_local(local, ret, SignNot(sign), numer, denom);
}

_g void sign_reverse_ratio_common(addr pos, addr *ret)
{
	int sign;
	addr numer, denom;

	GetSignRatio(pos, &sign);
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	if (GetStatusDynamic(pos)) {
		bignum_copy_heap(&numer, numer);
		bignum_copy_heap(&denom, denom);
	}
	make_ratio_heap(ret, SignNot(sign), numer, denom);
}

/* ratio - fixnum */
static void plus_rv_data_ratio(LocalRoot local,
		int sign, addr left, bigtype right, addr *ret)
{
	addr numer, denom, pos;
	size_t size1, size2;

	Check(local == NULL, "local error");
	GetNumerRatio(left, &numer);
	GetDenomRatio(left, &denom);
	if (equal_value_nosign_bignum(denom, 1)) {
		/* numer + right */
		plusvalue_bigdata_alloc(local, numer, SignPlus, right, &numer);
	}
	else {
		/* denom * right */
		GetSizeBignum(numer, &size1);
		GetSizeBignum(denom, &size2);
		size2++;
		size1 = (size1 < size2)? size2: size1;
		alloc_bignum(local, &pos, size1 + 1UL);
		setmultivalue_bigdata(pos, denom, right);
		/* numer + (denom * right) */
		letplus_noexpand_bigdata(pos, numer);
		bignum_throw_local(local, pos, &numer);
	}
	bignum_throw_local(local, denom, &denom);
	make_ratio_local(local, ret, sign, numer, denom);
}

static void plus_rv_data_local(LocalRoot local,
		int sign, addr left, bigtype right, addr *ret)
{
	addr numer, denom, pos;
	size_t size1, size2;

	Check(local == NULL, "local error");
	GetNumerRatio(left, &numer);
	GetDenomRatio(left, &denom);
	if (equal_value_nosign_bignum(denom, 1)) {
		plusvalue_bigdata_alloc(local, numer, sign, right, &pos);
		bignum_result_local(local, pos, ret);
		return;
	}

	/* denom * right */
	GetSizeBignum(numer, &size1);
	GetSizeBignum(denom, &size2);
	size2++;
	size1 = (size1 < size2)? size2: size1;
	alloc_bignum(local, &pos, size1 + 1UL);
	setmultivalue_bigdata(pos, denom, right);

	/* numer + (denom * right) */
	letplus_noexpand_bigdata(pos, numer);
	bignum_throw_local(local, pos, &numer);
	bignum_throw_local(local, denom, &denom);
	make_ratio_local(local, ret, sign, numer, denom);
}

static void plus_rv_data_common(LocalRoot local,
		int sign, addr left, bigtype right, addr *ret)
{
	addr numer, denom, pos;
	size_t size1, size2;
	LocalStack stack;

	Check(local == NULL, "local error");
	GetNumerRatio(left, &numer);
	GetDenomRatio(left, &denom);
	push_local(local, &stack);
	if (equal_value_nosign_bignum(denom, 1)) {
		plusvalue_bigdata_alloc(local, numer, sign, right, &pos);
		bignum_result_heap(pos, ret);
		goto finish;
	}

	/* denom * right */
	GetSizeBignum(numer, &size1);
	GetSizeBignum(denom, &size2);
	size2++;
	size1 = (size1 < size2)? size2: size1;
	alloc_bignum(local, &pos, size1 + 1UL);
	setmultivalue_bigdata(pos, denom, right);

	/* numer + (denom * right) */
	letplus_noexpand_bigdata(pos, numer);
	bignum_throw_heap(pos, &numer);
	bignum_throw_heap(denom, &denom);
	make_ratio_heap(ret, sign, numer, denom);

finish:
	rollback_local(local, stack);
}

static void minus_rv_data_ratio(LocalRoot local,
		int sign, addr left, bigtype right, addr *ret)
{
	int check;
	addr numer, denom, pos;
	size_t size1, size2;

	Check(local == NULL, "local error");
	GetNumerRatio(left, &numer);
	GetDenomRatio(left, &denom);
	if (equal_value_nosign_bignum(denom, 1)) {
		minusvalue_bigdata_alloc(local, numer, sign, right, &numer);
		GetSignBignum(numer, &sign);
		SetSignBignum(numer, SignPlus);
		bignum_throw_local(local, denom, &denom);
		make_ratio_local(local, ret, sign, numer, denom);
		return;
	}

	/* denom * right */
	GetSizeBignum(numer, &size1);
	GetSizeBignum(denom, &size2);
	size2++;
	size1 = (size1 < size2)? size2: size1;
	alloc_bignum(local, &pos, size1); /* no +1UL */
	setmultivalue_bigdata(pos, denom, right);

	/* numer - (denom * right) */
	check = letminus_noexpand_bigdata(pos, numer);
	bignum_throw_local(local, pos, &numer);
	bignum_throw_local(local, denom, &denom);
	make_ratio_local(local, ret, (check? sign: SignNot(sign)), numer, denom);
}

static void minus_rv_data_local(LocalRoot local,
		int sign, addr left, bigtype right, addr *ret)
{
	int check;
	addr numer, denom, pos;
	size_t size1, size2;

	Check(local == NULL, "local error");
	GetNumerRatio(left, &numer);
	GetDenomRatio(left, &denom);
	if (equal_value_nosign_bignum(denom, 1)) {
		minusvalue_bigdata_alloc(local, numer, sign, right, &pos);
		bignum_result_local(local, pos, ret);
		return;
	}

	/* denom * right */
	GetSizeBignum(numer, &size1);
	GetSizeBignum(denom, &size2);
	size2++;
	size1 = (size1 < size2)? size2: size1;
	alloc_bignum(local, &pos, size1); /* no +1UL */
	setmultivalue_bigdata(pos, denom, right);

	/* numer - (denom * right) */
	check = letminus_noexpand_bigdata(pos, numer);
	bignum_throw_local(local, pos, &numer);
	bignum_throw_local(local, denom, &denom);
	make_ratio_local(local, ret, (check? sign: SignNot(sign)), numer, denom);
}

static void minus_rv_data_common(LocalRoot local,
		int sign, addr left, bigtype right, addr *ret)
{
	int check;
	addr numer, denom, pos;
	size_t size1, size2;
	LocalStack stack;

	Check(local == NULL, "local error");
	GetNumerRatio(left, &numer);
	GetDenomRatio(left, &denom);
	push_local(local, &stack);
	if (equal_value_nosign_bignum(denom, 1)) {
		minusvalue_bigdata_alloc(local, numer, sign, right, &pos);
		bignum_result_heap(pos, ret);
		goto finish;
	}

	/* denom * right */
	GetSizeBignum(numer, &size1);
	GetSizeBignum(denom, &size2);
	size2++;
	size1 = (size1 < size2)? size2: size1;
	alloc_bignum(local, &pos, size1); /* no +1UL */
	setmultivalue_bigdata(pos, denom, right);

	/* numer - (denom * right) */
	check = letminus_noexpand_bigdata(pos, numer);
	bignum_throw_heap(pos, &numer);
	bignum_throw_heap(denom, &denom);
	make_ratio_heap(ret, (check? sign: SignNot(sign)), numer, denom);

finish:
	rollback_local(local, stack);
}

static void plus_rv_ratio(LocalRoot local,
		addr left, int sign2, bigtype right, addr *ret)
{
	int sign1, check1, check2;

	if (right == 0) {
		ratio_throw_local(local, left, ret);
		return;
	}

	GetSignRatio(left, &sign1);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if ((check1 && check2) || ((! check1) && (! check2)))
		plus_rv_data_ratio(local, sign1, left, right, ret);
	else
		minus_rv_data_ratio(local, sign1, left, right, ret);
}

static void plus_rv_local(LocalRoot local,
		addr left, int sign2, bigtype right, addr *ret)
{
	int sign1, check1, check2;

	if (right == 0) {
		ratio_throw_local(local, left, ret);
		return;
	}

	GetSignRatio(left, &sign1);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if ((check1 && check2) || ((! check1) && (! check2)))
		plus_rv_data_local(local, sign1, left, right, ret);
	else
		minus_rv_data_local(local, sign1, left, right, ret);
}

static void plus_rv_common(LocalRoot local,
		addr left, int sign2, bigtype right, addr *ret)
{
	int sign1, check1, check2;

	Check(local == NULL, "local error");
	if (right == 0) {
		ratio_throw_heap(left, ret);
		return;
	}

	GetSignRatio(left, &sign1);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if ((check1 && check2) || ((! check1) && (! check2)))
		plus_rv_data_common(local, sign1, left, right, ret);
	else
		minus_rv_data_common(local, sign1, left, right, ret);
}

_g void plus_rv_ratio_local(LocalRoot local, addr left, fixnum right, addr *ret)
{
	int sign;
	bigtype value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");

	if (zerop_ratio(left)) {
		ratio_zero_local(local, ret);
	}
	else {
		castfixed(right, &sign, &value);
		plus_rv_ratio(local, left, sign, value, ret);
	}
}

_g void plus_rv_real_local(LocalRoot local, addr left, fixnum right, addr *ret)
{
	int sign;
	bigtype value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");

	if (zerop_ratio(left)) {
		fixnum_local(local, ret, right);
	}
	else {
		castfixed(right, &sign, &value);
		plus_rv_local(local, left, sign, value, ret);
	}
}

_g void plus_rv_real_common(LocalRoot local, addr left, fixnum right, addr *ret)
{
	int sign;
	bigtype value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");

	if (zerop_ratio(left)) {
		fixnum_heap(ret, right);
	}
	else {
		castfixed(right, &sign, &value);
		plus_rv_common(local, left, sign, value, ret);
	}
}

_g void plus_rf_ratio_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign;
	bigtype value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	if (zerop_ratio(left)) {
		ratio_zero_local(local, ret);
	}
	else {
		castfixed_fixnum(right, &sign, &value);
		plus_rv_ratio(local, left, sign, value, ret);
	}
}

_g void plus_rf_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign;
	bigtype value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	if (zerop_ratio(left)) {
		fixnum_throw_local(local, right, ret);
	}
	else {
		castfixed_fixnum(right, &sign, &value);
		plus_rv_local(local, left, sign, value, ret);
	}
}

_g void plus_rf_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign;
	bigtype value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	if (zerop_ratio(left)) {
		fixnum_throw_heap(right, ret);
	}
	else {
		castfixed_fixnum(right, &sign, &value);
		plus_rv_common(local, left, sign, value, ret);
	}
}

static void minus_rv_ratio(LocalRoot local,
		addr left, int sign2, bigtype right, addr *ret, int reverse)
{
	int sign1, check1, check2;

	GetSignRatio(left, &sign1);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if (reverse) sign1 = SignNot(sign1);

	if ((check1 && check2) || ((! check1) && (! check2)))
		minus_rv_data_ratio(local, sign1, left, right, ret);
	else
		plus_rv_data_ratio(local, sign1, left, right, ret);
}

static void minus_rv_local(LocalRoot local,
		addr left, int sign2, bigtype right, addr *ret, int reverse)
{
	int sign1, check1, check2;

	GetSignRatio(left, &sign1);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if (reverse) sign1 = SignNot(sign1);

	if ((check1 && check2) || ((! check1) && (! check2)))
		minus_rv_data_local(local, sign1, left, right, ret);
	else
		plus_rv_data_local(local, sign1, left, right, ret);
}

static void minus_rv_common(LocalRoot local,
		addr left, int sign2, bigtype right, addr *ret, int reverse)
{
	int sign1, check1, check2;

	Check(local == NULL, "local error");
	GetSignRatio(left, &sign1);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if (reverse) sign1 = SignNot(sign1);
	if ((check1 && check2) || ((! check1) && (! check2)))
		minus_rv_data_common(local, sign1, left, right, ret);
	else
		plus_rv_data_common(local, sign1, left, right, ret);
}

_g void sigrev_fixnum_ratio_local(LocalRoot local, addr pos, addr *ret)
{
	cast_fixnum_ratio_local(local, pos, ret);
	sign_reverse_ratio_inplace(*ret);
}

_g void sigrev_bignum_ratio_local(LocalRoot local, addr pos, addr *ret)
{
	cast_bignum_ratio_local(local, pos, ret);
	sign_reverse_ratio_inplace(*ret);
}

_g void minus_rf_ratio_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign;
	bigtype value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	if (zerop_ratio(left)) {
		sigrev_fixnum_ratio_local(local, right, ret);
		return;
	}
	castfixed_fixnum(right, &sign, &value);
	if (value == 0) {
		ratio_throw_local(local, left, ret);
		return;
	}

	minus_rv_ratio(local, left, sign, value, ret, 0);
}

_g void minus_rf_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign;
	bigtype value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	if (zerop_ratio(left)) {
		sigrev_fixnum_integer_local(local, right, ret);
		return;
	}
	castfixed_fixnum(right, &sign, &value);
	if (value == 0) {
		ratio_throw_local(local, left, ret);
		return;
	}

	minus_rv_local(local, left, sign, value, ret, 0);
}

_g void minus_rf_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign;
	bigtype value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	if (zerop_ratio(left)) {
		sigrev_fixnum_integer_common(right, ret);
		return;
	}
	castfixed_fixnum(right, &sign, &value);
	if (value == 0) {
		ratio_throw_heap(left, ret);
		return;
	}

	minus_rv_common(local, left, sign, value, ret, 0);
}

_g void minus_fr_ratio_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign;
	bigtype value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	castfixed_fixnum(left, &sign, &value);
	if (value == 0) {
		sign_reverse_ratio_local(local, right, ret);
		return;
	}
	if (zerop_ratio(right)) {
		cast_fixnum_ratio_local(local, left, ret);
		return;
	}

	/* right - left = -(left - right) */
	minus_rv_ratio(local, right, sign, value, ret, 1);
}

_g void minus_fr_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign;
	bigtype value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	castfixed_fixnum(left, &sign, &value);
	if (value == 0) {
		sign_reverse_ratio_local(local, right, ret);
		return;
	}
	if (zerop_ratio(right)) {
		fixnum_throw_local(local, left, ret);
		return;
	}

	/* right - left = -(left - right) */
	minus_rv_local(local, right, sign, value, ret, 1);
}

_g void minus_fr_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign;
	bigtype value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	castfixed_fixnum(left, &sign, &value);
	if (value == 0) {
		sign_reverse_ratio_common(right, ret);
		return;
	}
	if (zerop_ratio(right)) {
		fixnum_throw_heap(left, ret);
		return;
	}

	/* right - left = -(left - right) */
	minus_rv_common(local, right, sign, value, ret, 1);
}


/* ratio - bignum */
static void plus_rb_ratio(LocalRoot local, int sign, addr left, addr right, addr *ret)
{
	addr pos, numer, denom;
	size_t size1, size2, size3;

	Check(local == NULL, "local error");
	GetNumerRatio(left, &numer);
	GetDenomRatio(left, &denom);
	if (equal_value_nosign_bignum(denom, 1)) {
		plus_bigdata_alloc(local, numer, right, &numer);
		bignum_throw_local(local, denom, &denom);
		make_ratio_local(local, ret, sign, numer, denom);
		return;
	}

	/* denom * right */
	GetSizeBignum(numer, &size1);
	GetSizeBignum(denom, &size2);
	GetSizeBignum(right, &size3);
	size2 += size3;
	size1 = (size1 < size2)? size2: size1;
	alloc_bignum(local, &pos, size1 + 1UL);
	setmulti_bigdata(pos, denom, right);

	/* numer + (denom * right) */
	letplus_noexpand_bigdata(pos, numer);
	bignum_throw_local(local, pos, &numer);
	bignum_throw_local(local, denom, &denom);
	make_ratio_local(local, ret, sign, numer, denom);
}

static void plus_rb_local(LocalRoot local, int sign, addr left, addr right, addr *ret)
{
	addr pos, numer, denom;
	size_t size1, size2, size3;

	Check(local == NULL, "local error");
	GetNumerRatio(left, &numer);
	GetDenomRatio(left, &denom);
	if (equal_value_nosign_bignum(denom, 1)) {
		plus_bigdata_alloc(local, numer, right, &pos);
		SetSignBignum(pos, sign);
		bignum_result_local(local, pos, ret);
		return;
	}

	/* denom * right */
	GetSizeBignum(numer, &size1);
	GetSizeBignum(denom, &size2);
	GetSizeBignum(right, &size3);
	size2 += size3;
	size1 = (size1 < size2)? size2: size1;
	alloc_bignum(local, &pos, size1 + 1UL);
	setmulti_bigdata(pos, denom, right);

	/* numer + (denom * right) */
	letplus_noexpand_bigdata(pos, numer);
	bignum_throw_local(local, pos, &numer);
	bignum_throw_local(local, denom, &denom);
	make_ratio_local(local, ret, sign, numer, denom);
}

static void plus_rb_common(LocalRoot local, int sign, addr left, addr right, addr *ret)
{
	addr pos, numer, denom;
	size_t size1, size2, size3;
	LocalStack stack;

	Check(local == NULL, "local error");
	GetNumerRatio(left, &numer);
	GetDenomRatio(left, &denom);
	push_local(local, &stack);
	if (equal_value_nosign_bignum(denom, 1)) {
		plus_bigdata_alloc(local, numer, right, &pos);
		SetSignBignum(pos, sign);
		bignum_result_heap(pos, ret);
		goto finish;
	}

	/* denom * right */
	GetSizeBignum(numer, &size1);
	GetSizeBignum(denom, &size2);
	GetSizeBignum(right, &size3);
	size2 += size3;
	size1 = (size1 < size2)? size2: size1;
	alloc_bignum(local, &pos, size1 + 1UL);
	setmulti_bigdata(pos, denom, right);

	/* numer + (denom * right) */
	letplus_noexpand_bigdata(pos, numer);
	bignum_throw_heap(pos, &numer);
	bignum_throw_heap(denom, &denom);
	make_ratio_heap(ret, sign, numer, denom);

finish:
	rollback_local(local, stack);
}

static void minus_rb_ratio(LocalRoot local,
		int sign, addr left, addr right, addr *ret, int reverse)
{
	int check;
	addr pos, numer, denom;
	size_t size1, size2, size3;

	Check(local == NULL, "local error");
	GetNumerRatio(left, &numer);
	GetDenomRatio(left, &denom);
	if (equal_value_nosign_bignum(denom, 1)) {
		check = minuscheck_bigdata_alloc(local, numer, right, &numer);
		sign = check? SignNot(sign): sign;
		sign = reverse? SignNot(sign): sign;
		bignum_throw_local(local, denom, &denom);
		make_ratio_local(local, ret, sign, numer, denom);
		return;
	}

	/* denom * right */
	GetSizeBignum(numer, &size1);
	GetSizeBignum(denom, &size2);
	GetSizeBignum(right, &size3);
	size2 += size3;
	size1 = (size1 < size2)? size2: size1;
	alloc_bignum(local, &pos, size1);
	setmulti_bigdata(pos, denom, right);

	/* numer + (denom * right) */
	check = letminus_noexpand_bigdata(pos, numer);
	bignum_throw_local(local, pos, &numer);
	bignum_throw_local(local, denom, &denom);
	sign = check? sign: SignNot(sign);
	sign = reverse? SignNot(sign): sign;
	make_ratio_local(local, ret, sign, numer, denom);
}

static void minus_rb_local(LocalRoot local,
		int sign, addr left, addr right, addr *ret, int reverse)
{
	int check;
	addr pos, numer, denom;
	size_t size1, size2, size3;

	Check(local == NULL, "local error");
	GetNumerRatio(left, &numer);
	GetDenomRatio(left, &denom);
	if (equal_value_nosign_bignum(denom, 1)) {
		check = minuscheck_bigdata_alloc(local, numer, right, &pos);
		sign = check? SignNot(sign): sign;
		sign = reverse? SignNot(sign): sign;
		SetSignBignum(pos, sign);
		bignum_result_local(local, pos, ret);
		return;
	}

	/* denom * right */
	GetSizeBignum(numer, &size1);
	GetSizeBignum(denom, &size2);
	GetSizeBignum(right, &size3);
	size2 += size3;
	size1 = (size1 < size2)? size2: size1;
	alloc_bignum(local, &pos, size1);
	setmulti_bigdata(pos, denom, right);

	/* numer + (denom * right) */
	check = letminus_noexpand_bigdata(pos, numer);
	bignum_throw_local(local, pos, &numer);
	bignum_throw_local(local, denom, &denom);
	sign = check? sign: SignNot(sign);
	sign = reverse? SignNot(sign): sign;
	make_ratio_local(local, ret, sign, numer, denom);
}

static void minus_rb_common(LocalRoot local,
		int sign, addr left, addr right, addr *ret, int reverse)
{
	int check;
	addr pos, numer, denom;
	size_t size1, size2, size3;
	LocalStack stack;

	Check(local == NULL, "local error");
	GetNumerRatio(left, &numer);
	GetDenomRatio(left, &denom);
	push_local(local, &stack);
	if (equal_value_nosign_bignum(denom, 1)) {
		check = minuscheck_bigdata_alloc(local, numer, right, &pos);
		sign = check? SignNot(sign): sign;
		sign = reverse? SignNot(sign): sign;
		SetSignBignum(pos, sign);
		bignum_result_heap(pos, ret);
		goto finish;
	}

	/* denom * right */
	GetSizeBignum(numer, &size1);
	GetSizeBignum(denom, &size2);
	GetSizeBignum(right, &size3);
	size2 += size3;
	size1 = (size1 < size2)? size2: size1;
	alloc_bignum(local, &pos, size1);
	setmulti_bigdata(pos, denom, right);

	/* numer + (denom * right) */
	check = letminus_noexpand_bigdata(pos, numer);
	bignum_throw_heap(pos, &numer);
	bignum_throw_heap(denom, &denom);
	sign = check? sign: SignNot(sign);
	sign = reverse? SignNot(sign): sign;
	make_ratio_heap(ret, sign, numer, denom);

finish:
	rollback_local(local, stack);
}

_g void plus_rb_ratio_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, check1, check2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	if (zerop_ratio(left)) {
		cast_bignum_ratio_local(local, right, ret);
		return;
	}
	if (zerop_bignum(right)) {
		ratio_throw_local(local, left, ret);
		return;
	}

	GetSignRatio(left, &sign1);
	GetSignBignum(right, &sign2);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if ((check1 && check2) || ((! check1) && (! check2)))
		plus_rb_ratio(local, sign1, left, right, ret);
	else
		minus_rb_ratio(local, sign1, left, right, ret, 0);
}

_g void plus_rb_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, check1, check2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	if (zerop_ratio(left)) {
		bignum_throw_local(local, right, ret);
		return;
	}
	if (zerop_bignum(right)) {
		ratio_throw_local(local, left, ret);
		return;
	}

	GetSignRatio(left, &sign1);
	GetSignBignum(right, &sign2);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if ((check1 && check2) || ((! check1) && (! check2)))
		plus_rb_local(local, sign1, left, right, ret);
	else
		minus_rb_local(local, sign1, left, right, ret, 0);
}

_g void plus_rb_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, check1, check2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	if (zerop_ratio(left)) {
		bignum_throw_heap(right, ret);
		return;
	}
	if (zerop_bignum(right)) {
		ratio_throw_heap(left, ret);
		return;
	}

	GetSignRatio(left, &sign1);
	GetSignBignum(right, &sign2);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if ((check1 && check2) || ((! check1) && (! check2)))
		plus_rb_common(local, sign1, left, right, ret);
	else
		minus_rb_common(local, sign1, left, right, ret, 0);
}

_g void minus_rb_ratio_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, check1, check2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	if (zerop_ratio(left)) {
		sigrev_bignum_ratio_local(local, right, ret);
		return;
	}
	if (zerop_bignum(right)) {
		ratio_throw_local(local, left, ret);
		return;
	}

	GetSignRatio(left, &sign1);
	GetSignBignum(right, &sign2);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if ((check1 && check2) || ((! check1) && (! check2)))
		minus_rb_ratio(local, sign1, left, right, ret, 0);
	else
		plus_rb_ratio(local, sign1, left, right, ret);
}

_g void minus_rb_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, check1, check2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	if (zerop_ratio(left)) {
		sigrev_bignum_integer_local(local, right, ret);
		return;
	}
	if (zerop_bignum(right)) {
		ratio_throw_local(local, left, ret);
		return;
	}

	GetSignRatio(left, &sign1);
	GetSignBignum(right, &sign2);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if ((check1 && check2) || ((! check1) && (! check2)))
		minus_rb_local(local, sign1, left, right, ret, 0);
	else
		plus_rb_local(local, sign1, left, right, ret);
}

_g void minus_rb_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, check1, check2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	if (zerop_ratio(left)) {
		sigrev_bignum_integer_common(right, ret);
		return;
	}
	if (zerop_bignum(right)) {
		ratio_throw_heap(left, ret);
		return;
	}

	GetSignRatio(left, &sign1);
	GetSignBignum(right, &sign2);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if ((check1 && check2) || ((! check1) && (! check2)))
		minus_rb_common(local, sign1, left, right, ret, 0);
	else
		plus_rb_common(local, sign1, left, right, ret);
}

_g void minus_br_ratio_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, check1, check2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	if (zerop_bignum(left)) {
		sign_reverse_ratio_local(local, right, ret);
		return;
	}
	if (zerop_ratio(right)) {
		cast_bignum_ratio_local(local, left, ret);
		return;
	}

	GetSignBignum(left, &sign1);
	GetSignRatio(right, &sign2);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if ((check1 && check2) || ((! check1) && (! check2)))
		minus_rb_ratio(local, sign1, right, left, ret, 1);
	else
		plus_rb_ratio(local, sign1, right, left, ret);
}

_g void minus_br_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, check1, check2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	if (zerop_bignum(left)) {
		sign_reverse_ratio_local(local, right, ret);
		return;
	}
	if (zerop_ratio(right)) {
		bignum_throw_local(local, left, ret);
		return;
	}

	GetSignBignum(left, &sign1);
	GetSignRatio(right, &sign2);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if ((check1 && check2) || ((! check1) && (! check2)))
		minus_rb_local(local, sign1, right, left, ret, 1);
	else
		plus_rb_local(local, sign1, right, left, ret);
}

_g void minus_br_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, check1, check2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	if (zerop_bignum(left)) {
		sign_reverse_ratio_common(right, ret);
		return;
	}
	if (zerop_ratio(right)) {
		bignum_throw_heap(left, ret);
		return;
	}

	GetSignBignum(left, &sign1);
	GetSignRatio(right, &sign2);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if ((check1 && check2) || ((! check1) && (! check2)))
		minus_rb_common(local, sign1, right, left, ret, 1);
	else
		plus_rb_common(local, sign1, right, left, ret);
}


/* ratio - ratio */
static void plus_rr_data_result(LocalRoot local,
		addr left, addr right, addr *rnumer, addr *rdenom)
{
	addr numer1, numer2, denom1, denom2, reduct1, reduct2;

	GetNumerRatio(left, &numer1);
	GetDenomRatio(left, &denom1);
	GetNumerRatio(right, &numer2);
	GetDenomRatio(right, &denom2);
	if (equal_bigdata(denom1, denom2)) {
		/*
		 *  numer1 + numer2
		 *  ---------------
		 *      denom1
		 */
		plus_bigdata_alloc(local, numer1, numer2, &numer1);
		bignum_copy_local(local, &denom1, denom1);
	}
	else {
		/*
		 *  numer1   numer2
		 *  ------ + ------
		 *  denom1   denom2
		 */

		/*
		 *  (denom1 denom2) reduction-> (reduct1 reduct2)
		 */
		bignum_copy_local(local, &reduct1, denom1);
		bignum_copy_local(local, &reduct2, denom2);
		reduction_local(local, reduct1, reduct2);

		/*
		 *  numer1*reduct2    numer2*reduct1    numer1*reduct2 + numer2*reduct1
		 *  -------------- + ---------------- = -------------------------------
		 *  denom1*reduct2   (denom2*reduct1)           denom1 * reduct2
		 */
		multi_bigdata_alloc(local, denom1, reduct2, &denom1);
		multi_bigdata_alloc(local, numer1, reduct2, &numer1);
		multi_bigdata_alloc(local, numer2, reduct1, &numer2);
		plus_bigdata_alloc(local, numer1, numer2, &numer1);
	}
	*rnumer = numer1;
	*rdenom = denom1;
}

static void plus_rr_data_ratio(LocalRoot local,
		int sign, addr left, addr right, addr *ret)
{
	plus_rr_data_result(local, left, right, &left, &right);
	make_ratio_reduction_local(local, ret, sign, left, right);
}

static void plus_rr_data_local(LocalRoot local,
		int sign, addr left, addr right, addr *ret)
{
	plus_rr_data_result(local, left, right, &left, &right);
	ratio_reduction_local(local, ret, sign, left, right);
}

static void plus_rr_data_common(LocalRoot local,
		int sign, addr left, addr right, addr *ret)
{
	LocalStack stack;

	push_local(local, &stack);
	plus_rr_data_result(local, left, right, &left, &right);
	ratio_reduction_heap(local, ret, sign, left, right);
	rollback_local(local, stack);
}

static void minus_rr_data_result(LocalRoot local,
		int sign, addr left, addr right,
		int *rsign, addr *rnumer, addr *rdenom)
{
	int check;
	addr numer1, numer2, denom1, denom2, reduct1, reduct2;

	GetNumerRatio(left, &numer1);
	GetDenomRatio(left, &denom1);
	GetNumerRatio(right, &numer2);
	GetDenomRatio(right, &denom2);
	if (equal_bigdata(denom1, denom2)) {
		/*
		 *  numer1 - numer2
		 *  --------------
		 *      denom1
		 */
		check = minuscheck_bigdata_alloc(local, numer1, numer2, &numer1);
		if (check) sign = SignNot(sign);
		bignum_copy_local(local, &denom1, denom1);
	}
	else {
		/*
		 *  numer1   numer2
		 *  ------ - ------
		 *  denom1   denom2
		 */

		/*
		 *  (denom1 denom2) reduction-> (reduct1 reduct2)
		 */
		bignum_copy_local(local, &reduct1, denom1);
		bignum_copy_local(local, &reduct2, denom2);
		reduction_local(local, reduct1, reduct2);

		/*
		 *  numer1*reduct2    numer2*reduct1    numer1*reduct2 - numer2*reduct1
		 *  -------------- - ---------------- = -------------------------------
		 *  denom1*reduct2   (denom2*reduct1)           denom1 * reduct2
		 */
		multi_bigdata_alloc(local, denom1, reduct2, &denom1);
		multi_bigdata_alloc(local, numer1, reduct2, &numer1);
		multi_bigdata_alloc(local, numer2, reduct1, &numer2);
		check = minuscheck_bigdata_alloc(local, numer1, numer2, &numer1);
		if (check) sign = SignNot(sign);
	}
	*rsign = sign;
	*rnumer = numer1;
	*rdenom = denom1;
}

static void minus_rr_data_ratio(LocalRoot local,
		int sign, addr left, addr right, addr *ret)
{
	minus_rr_data_result(local, sign, left, right, &sign, &left, &right);
	make_ratio_reduction_local(local, ret, sign, left, right);
}

static void minus_rr_data_local(LocalRoot local,
		int sign, addr left, addr right, addr *ret)
{
	minus_rr_data_result(local, sign, left, right, &sign, &left, &right);
	ratio_reduction_local(local, ret, sign, left, right);
}

static void minus_rr_data_common(LocalRoot local,
		int sign, addr left, addr right, addr *ret)
{
	LocalStack stack;

	push_local(local, &stack);
	minus_rr_data_result(local, sign, left, right, &sign, &left, &right);
	ratio_reduction_heap(local, ret, sign, left, right);
	rollback_local(local, stack);
}

_g void plus_rr_ratio_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, check1, check2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	if (zerop_ratio(left)) {
		ratio_throw_local(local, right, ret);
		return;
	}
	if (zerop_ratio(right)) {
		ratio_throw_local(local, left, ret);
		return;
	}

	GetSignRatio(left, &sign1);
	GetSignRatio(right, &sign2);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if ((check1 && check2) || ((! check1) && (! check2)))
		plus_rr_data_ratio(local, sign1, left, right, ret);
	else
		minus_rr_data_ratio(local, sign1, left, right, ret);
}

_g void plus_rr_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, check1, check2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	if (zerop_ratio(left)) {
		ratio_throw_local(local, right, ret);
		return;
	}
	if (zerop_ratio(right)) {
		ratio_throw_local(local, left, ret);
		return;
	}

	GetSignRatio(left, &sign1);
	GetSignRatio(right, &sign2);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if ((check1 && check2) || ((! check1) && (! check2)))
		plus_rr_data_local(local, sign1, left, right, ret);
	else
		minus_rr_data_local(local, sign1, left, right, ret);
}

_g void plus_rr_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, check1, check2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	if (zerop_ratio(left)) {
		ratio_throw_heap(right, ret);
		return;
	}
	if (zerop_ratio(right)) {
		ratio_throw_heap(left, ret);
		return;
	}

	GetSignRatio(left, &sign1);
	GetSignRatio(right, &sign2);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if ((check1 && check2) || ((! check1) && (! check2)))
		plus_rr_data_common(local, sign1, left, right, ret);
	else
		minus_rr_data_common(local, sign1, left, right, ret);
}

_g void minus_rr_ratio_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, check1, check2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	if (zerop_ratio(left)) {
		sign_reverse_ratio_local(local, right, ret);
		return;
	}
	if (zerop_ratio(right)) {
		ratio_throw_local(local, left, ret);
		return;
	}

	GetSignRatio(left, &sign1);
	GetSignRatio(right, &sign2);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if ((check1 && check2) || ((! check1) && (! check2)))
		minus_rr_data_ratio(local, sign1, left, right, ret);
	else
		plus_rr_data_ratio(local, sign1, left, right, ret);
}

_g void minus_rr_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, check1, check2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	if (zerop_ratio(left)) {
		sign_reverse_ratio_local(local, right, ret);
		return;
	}
	if (zerop_ratio(right)) {
		ratio_throw_local(local, left, ret);
		return;
	}

	GetSignRatio(left, &sign1);
	GetSignRatio(right, &sign2);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if ((check1 && check2) || ((! check1) && (! check2)))
		minus_rr_data_local(local, sign1, left, right, ret);
	else
		plus_rr_data_local(local, sign1, left, right, ret);
}

_g void minus_rr_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, check1, check2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	if (zerop_ratio(left)) {
		sign_reverse_ratio_common(right, ret);
		return;
	}
	if (zerop_ratio(right)) {
		ratio_throw_heap(left, ret);
		return;
	}

	GetSignRatio(left, &sign1);
	GetSignRatio(right, &sign2);
	check1 = IsPlus(sign1);
	check2 = IsPlus(sign2);
	if ((check1 && check2) || ((! check1) && (! check2)))
		minus_rr_data_common(local, sign1, left, right, ret);
	else
		plus_rr_data_common(local, sign1, left, right, ret);
}

