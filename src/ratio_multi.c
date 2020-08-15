#include "bignum.h"
#include "bignum_data.h"
#include "bignum_equal.h"
#include "bignum_object.h"
#include "bignum_plus.h"
#include "condition.h"
#include "integer.h"
#include "memory.h"
#include "ratio.h"
#include "ratio_plus.h"
#include "typedef.h"

static int equal_rv_nosign(addr left, bigtype value)
{
	addr check;

	Check(GetType(left) != LISPTYPE_RATIO, "type error");
	GetDenomRatio(left, &check);
	if (! equal_value_nosign_bignum(check, 1))
		return 0;
	GetNumerRatio(left, &check);

	return equal_value_nosign_bignum(check, value);
}

static void multi_rv_result(LocalRoot local,
		addr left, bigtype value, addr *rnumer, addr *rdenom)
{
	addr right, numer, denom;

	/*
	 *  numer   value
	 *  ----- * -----
	 *  denom     1
	 */
	GetNumerRatio(left, &numer);
	GetDenomRatio(left, &denom);
	bignum_value_local(local, &right, SignPlus, value);
	bignum_copy_local(local, &denom, denom);
	reduction_local(local, right, denom);
	multi_bigdata_alloc(local, right, numer, &numer);
	*rnumer = numer;
	*rdenom = denom;
}

static void multi_rv_ratio(LocalRoot local,
		int sign, addr left, bigtype value, addr *ret)
{
	addr numer, denom;

	multi_rv_result(local, left, value, &numer, &denom);
	make_ratio_reduction_local(local, ret, sign, numer, denom);
}

static void multi_rv_local(LocalRoot local,
		int sign, addr left, bigtype value, addr *ret)
{
	addr numer, denom;

	multi_rv_result(local, left, value, &numer, &denom);
	ratio_noreduction_local(local, ret, sign, numer, denom);
}

static void multi_rv_common(LocalRoot local,
		int sign, addr left, bigtype value, addr *ret)
{
	LocalStack stack;
	addr numer, denom;

	push_local(local, &stack);
	multi_rv_result(local, left, value, &numer, &denom);
	ratio_noreduction_heap(ret, sign, numer, denom);
	rollback_local(local, stack);
}

_g void multi_rf_ratio_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	bigtype value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* ratio */
	if (zerop_ratio(left)) {
		ratio_zero_local(local, ret);
		return;
	}
	GetSignRatio(left, &sign1);
	if (equal_rv_nosign(left, 1)) {
		if (IsPlus(sign1))
			cast_fixnum_ratio_local(local, right, ret);
		else
			sigrev_fixnum_ratio_local(local, right, ret);
		return;
	}

	/* fixnum */
	castfixed_fixnum(right, &sign2, &value);
	if (value == 0) {
		ratio_zero_local(local, ret);
		return;
	}
	if (value == 1) {
		if (IsPlus(sign2))
			ratio_throw_local(local, left, ret);
		else
			sign_reverse_ratio_local(local, left, ret);
		return;
	}

	/* multiple */
	sign1 = SignMulti(sign1, sign2);
	multi_rv_ratio(local, sign1, left, value, ret);
}

_g void multi_rf_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	bigtype value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* ratio */
	if (zerop_ratio(left)) {
		fixnum_local(local, ret, 0);
		return;
	}
	GetSignRatio(left, &sign1);
	if (equal_rv_nosign(left, 1)) {
		if (IsPlus(sign1))
			fixnum_throw_local(local, right, ret);
		else
			sigrev_fixnum_integer_local(local, right, ret);
		return;
	}

	/* fixnum */
	castfixed_fixnum(right, &sign2, &value);
	if (value == 0) {
		fixnum_local(local, ret, 0);
		return;
	}
	if (value == 1) {
		if (IsPlus(sign2))
			ratio_throw_local(local, left, ret);
		else
			sign_reverse_ratio_local(local, left, ret);
		return;
	}

	/* multiple */
	sign1 = SignMulti(sign1, sign2);
	multi_rv_local(local, sign1, left, value, ret);
}

_g void multi_rf_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	bigtype value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* ratio */
	if (zerop_ratio(left)) {
		fixnum_heap(ret, 0);
		return;
	}
	GetSignRatio(left, &sign1);
	if (equal_rv_nosign(left, 1)) {
		if (IsPlus(sign1))
			fixnum_throw_heap(right, ret);
		else
			sigrev_fixnum_integer_common(right, ret);
		return;
	}

	/* fixnum */
	castfixed_fixnum(right, &sign2, &value);
	if (value == 0) {
		fixnum_heap(ret, 0);
		return;
	}
	if (value == 1) {
		if (IsPlus(sign2))
			ratio_throw_heap(left, ret);
		else
			sign_reverse_ratio_common(left, ret);
		return;
	}

	/* multiple */
	sign1 = SignMulti(sign1, sign2);
	multi_rv_common(local, sign1, left, value, ret);
}

static void multi_rb_result(LocalRoot local,
		addr left, addr right, addr *rnumer, addr *rdenom)
{
	addr numer, denom;

	/*
	 *  numer   right
	 *  ----- * -----
	 *  denom     1
	 */
	GetNumerRatio(left, &numer);
	GetDenomRatio(left, &denom);
	bignum_copy_local(local, &right, right);
	bignum_copy_local(local, &denom, denom);
	reduction_local(local, right, denom);
	multi_bigdata_alloc(local, right, numer, &numer);
	*rnumer = numer;
	*rdenom = denom;
}

static void multi_rb_ratio(LocalRoot local,
		int sign, addr left, addr right, addr *ret)
{
	multi_rb_result(local, left, right, &left, &right);
	make_ratio_reduction_local(local, ret, sign, left, right);
}

static void multi_rb_local(LocalRoot local,
		int sign, addr left, addr right, addr *ret)
{
	multi_rb_result(local, left, right, &left, &right);
	ratio_noreduction_local(local, ret, sign, left, right);
}

static void multi_rb_common(LocalRoot local,
		int sign, addr left, addr right, addr *ret)
{
	LocalStack stack;

	push_local(local, &stack);
	multi_rb_result(local, left, right, &left, &right);
	ratio_noreduction_heap(ret, sign, left, right);
	rollback_local(local, stack);
}

_g void multi_rb_ratio_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	/* ratio */
	if (zerop_ratio(left)) {
		ratio_zero_local(local, ret);
		return;
	}
	GetSignRatio(left, &sign1);
	if (equal_rv_nosign(left, 1)) {
		if (IsPlus(sign1))
			cast_bignum_ratio_local(local, right, ret);
		else
			sigrev_bignum_ratio_local(local, right, ret);
		return;
	}

	/* bignum */
	if (zerop_bignum(right)) {
		ratio_zero_local(local, ret);
		return;
	}
	GetSignBignum(right, &sign2);
	if (equal_value_nosign_bignum(right, 1)) {
		if (IsPlus(sign2))
			ratio_throw_local(local, left, ret);
		else
			sign_reverse_ratio_local(local, left, ret);
		return;
	}

	/* multiple */
	sign1 = SignMulti(sign1, sign2);
	multi_rb_ratio(local, sign1, left, right, ret);
}

_g void multi_rb_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	/* ratio */
	if (zerop_ratio(left)) {
		fixnum_local(local, ret, 0);
		return;
	}
	GetSignRatio(left, &sign1);
	if (equal_rv_nosign(left, 1)) {
		if (IsPlus(sign1))
			bignum_throw_local(local, right, ret);
		else
			sigrev_bignum_integer_local(local, right, ret);
		return;
	}

	/* bignum */
	if (zerop_bignum(right)) {
		fixnum_local(local, ret, 0);
		return;
	}
	GetSignBignum(right, &sign2);
	if (equal_value_nosign_bignum(right, 1)) {
		if (IsPlus(sign2))
			ratio_throw_local(local, left, ret);
		else
			sign_reverse_ratio_local(local, left, ret);
		return;
	}

	/* multiple */
	sign1 = SignMulti(sign1, sign2);
	multi_rb_local(local, sign1, left, right, ret);
}

_g void multi_rb_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	/* ratio */
	if (zerop_ratio(left)) {
		fixnum_heap(ret, 0);
		return;
	}
	GetSignRatio(left, &sign1);
	if (equal_rv_nosign(left, 1)) {
		if (IsPlus(sign1))
			bignum_throw_heap(right, ret);
		else
			sigrev_bignum_integer_common(right, ret);
		return;
	}

	/* bignum */
	if (zerop_bignum(right)) {
		fixnum_heap(ret, 0);
		return;
	}
	GetSignBignum(right, &sign2);
	if (equal_value_nosign_bignum(right, 1)) {
		if (IsPlus(sign2))
			ratio_throw_heap(left, ret);
		else
			sign_reverse_ratio_common(left, ret);
		return;
	}

	/* multiple */
	sign1 = SignMulti(sign1, sign2);
	multi_rb_common(local, sign1, left, right, ret);
}

static void div_rb_result(LocalRoot local,
		addr left, addr right, addr *rnumer, addr *rdenom)
{
	addr numer, denom;

	/*
	 *  numer           numer     1
	 *  ----- / right = ----- * -----
	 *  denom           denom   right
	 */
	GetNumerRatio(left, &numer);
	GetDenomRatio(left, &denom);
	bignum_copy_local(local, &right, right);
	bignum_copy_local(local, &numer, numer);
	reduction_local(local, numer, right);
	multi_bigdata_alloc(local, denom, right, &denom);
	*rnumer = numer;
	*rdenom = denom;
}

static void div_rb_ratio(LocalRoot local, int sign, addr left, addr right, addr *ret)
{
	div_rb_result(local, left, right, &left, &right);
	make_ratio_reduction_local(local, ret, sign, left, right);
}

static void div_rb_local(LocalRoot local, int sign, addr left, addr right, addr *ret)
{
	div_rb_result(local, left, right, &left, &right);
	ratio_noreduction_local(local, ret, sign, left, right);
}

static void div_rb_common(LocalRoot local, int sign, addr left, addr right, addr *ret)
{
	LocalStack stack;

	push_local(local, &stack);
	div_rb_result(local, left, right, &left, &right);
	ratio_noreduction_heap(ret, sign, left, right);
	rollback_local(local, stack);
}

static void multi_rr_result(LocalRoot local,
		addr left, addr right, addr *rnumer, addr *rdenom)
{
	addr numer1, numer2, denom1, denom2;

	/*
	 *  numer1   numer2
	 *  ------ * ------
	 *  denom1   denom2
	 */
	GetNumerRatio(left, &numer1);
	GetDenomRatio(left, &denom1);
	GetNumerRatio(right, &numer2);
	GetDenomRatio(right, &denom2);
	bignum_copy_local(local, &numer1, numer1);
	bignum_copy_local(local, &denom1, denom1);
	bignum_copy_local(local, &numer2, numer2);
	bignum_copy_local(local, &denom2, denom2);
	reduction_local(local, numer1, denom2);
	reduction_local(local, numer2, denom1);
	multi_bigdata_alloc(local, numer1, numer2, &numer1);
	multi_bigdata_alloc(local, denom1, denom2, &denom1);
	*rnumer = numer1;
	*rdenom = denom1;
}

static void multi_rr_ratio(LocalRoot local,
		int sign, addr left, addr right, addr *ret)
{
	multi_rr_result(local, left, right, &left, &right);
	make_ratio_reduction_local(local, ret, sign, left, right);
}

static void multi_rr_local(LocalRoot local,
		int sign, addr left, addr right, addr *ret)
{
	multi_rr_result(local, left, right, &left, &right);
	ratio_noreduction_local(local, ret, sign, left, right);
}

static void multi_rr_common(LocalRoot local,
		int sign, addr left, addr right, addr *ret)
{
	LocalStack stack;

	push_local(local, &stack);
	multi_rr_result(local, left, right, &left, &right);
	ratio_noreduction_heap(ret, sign, left, right);
	rollback_local(local, stack);
}

static inline int inverse_ratio_p(addr pos)
{
	GetNumerRatio(pos, &pos);
	return equal_value_nosign_bignum(pos, 1);
}

_g void multi_rr_ratio_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign, sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	GetSignRatio(left, &sign1);
	GetSignRatio(right, &sign2);
	sign = SignMulti(sign1, sign2);

	/* left */
	if (zerop_ratio(left)) {
		ratio_zero_local(local, ret);
		return;
	}
	if (equal_rv_nosign(left, 1)) {
		if (IsPlus(sign1))
			ratio_throw_local(local, right, ret);
		else
			sign_reverse_ratio_local(local, right, ret);
		return;
	}
	if (inverse_ratio_p(left)) {
		GetDenomRatio(left, &left);
		div_rb_ratio(local, sign, right, left, ret);
		return;
	}

	/* right */
	if (zerop_ratio(right)) {
		ratio_zero_local(local, ret);
		return;
	}
	if (equal_rv_nosign(right, 1)) {
		if (IsPlus(sign2))
			ratio_throw_local(local, left, ret);
		else
			sign_reverse_ratio_local(local, left, ret);
		return;
	}
	if (inverse_ratio_p(right)) {
		GetDenomRatio(right, &right);
		div_rb_ratio(local, sign, left, right, ret);
		return;
	}

	/* multiple */
	multi_rr_ratio(local, sign, left, right, ret);
}

_g void multi_rr_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign, sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	GetSignRatio(left, &sign1);
	GetSignRatio(right, &sign2);
	sign = SignMulti(sign1, sign2);

	/* left */
	if (zerop_ratio(left)) {
		fixnum_local(local, ret, 0);
		return;
	}
	if (equal_rv_nosign(left, 1)) {
		if (IsPlus(sign1))
			ratio_throw_local(local, right, ret);
		else
			sign_reverse_ratio_local(local, right, ret);
		return;
	}
	if (inverse_ratio_p(left)) {
		GetDenomRatio(left, &left);
		div_rb_local(local, sign, right, left, ret);
		return;
	}

	/* right */
	if (zerop_ratio(right)) {
		fixnum_local(local, ret, 0);
		return;
	}
	if (equal_rv_nosign(right, 1)) {
		if (IsPlus(sign2))
			ratio_throw_local(local, left, ret);
		else
			sign_reverse_ratio_local(local, left, ret);
		return;
	}
	if (inverse_ratio_p(right)) {
		GetDenomRatio(right, &right);
		div_rb_local(local, sign, left, right, ret);
		return;
	}

	/* multiple */
	multi_rr_local(local, sign, left, right, ret);
}

_g void multi_rr_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign, sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	GetSignRatio(left, &sign1);
	GetSignRatio(right, &sign2);
	sign = SignMulti(sign1, sign2);

	/* left */
	if (zerop_ratio(left)) {
		fixnum_heap(ret, 0);
		return;
	}
	if (equal_rv_nosign(left, 1)) {
		if (IsPlus(sign1))
			ratio_throw_heap(right, ret);
		else
			sign_reverse_ratio_common(right, ret);
		return;
	}
	if (inverse_ratio_p(left)) {
		GetDenomRatio(left, &left);
		div_rb_common(local, sign, right, left, ret);
		return;
	}

	/* right */
	if (zerop_ratio(right)) {
		fixnum_heap(ret, 0);
		return;
	}
	if (equal_rv_nosign(right, 1)) {
		if (IsPlus(sign2))
			ratio_throw_heap(left, ret);
		else
			sign_reverse_ratio_common(left, ret);
		return;
	}
	if (inverse_ratio_p(right)) {
		GetDenomRatio(right, &right);
		div_rb_common(local, sign, left, right, ret);
		return;
	}

	/* multiple */
	multi_rr_common(local, sign, left, right, ret);
}


/*
 *  division
 */
static inline void inverse_value_ratio(LocalRoot local,
		addr *ret, int sign, bigtype value)
{
	ratio_noreduction_value_local(local, ret, sign, 1, value);
}

static inline void inverse_value_local(LocalRoot local,
		addr *ret, int sign, bigtype value)
{
	if (value == 1)
		fixnum_local(local, ret, IsPlus(sign)? 1: -1);
	else
		ratio_noreduction_value_local(local, ret, sign, 1, value);
}

static inline void inverse_value_common(addr *ret, int sign, bigtype value)
{
	if (value == 1)
		fixnum_heap(ret, IsPlus(sign)? 1: -1);
	else
		ratio_noreduction_value_heap(ret, sign, 1, value);
}

static void div_rv_result(LocalRoot local,
		addr left, bigtype value, addr *rnumer, addr *rdenom)
{
	addr right, numer, denom;

	/*
	 *  numer           numer     1
	 *  ----- / value = ----- * -----
	 *  denom           denom   value
	 */
	GetNumerRatio(left, &numer);
	GetDenomRatio(left, &denom);
	bignum_value_local(local, &right, SignPlus, value);
	bignum_copy_local(local, &numer, numer);
	reduction_local(local, numer, right);
	multi_bigdata_alloc(local, denom, right, &denom);
	*rnumer = numer;
	*rdenom = denom;
}

static void div_rv_ratio(LocalRoot local,
		int sign, addr left, bigtype value, addr *ret)
{
	addr numer, denom;

	div_rv_result(local, left, value, &numer, &denom);
	make_ratio_reduction_local(local, ret, sign, numer, denom);
}

static void div_rv_local(LocalRoot local,
		int sign, addr left, bigtype value, addr *ret)
{
	addr numer, denom;

	div_rv_result(local, left, value, &numer, &denom);
	ratio_noreduction_local(local, ret, sign, numer, denom);
}

static void div_rv_common(LocalRoot local, int sign, addr left, bigtype value, addr *ret)
{
	LocalStack stack;
	addr numer, denom;

	push_local(local, &stack);
	div_rv_result(local, left, value, &numer, &denom);
	ratio_noreduction_heap(ret, sign, numer, denom);
	rollback_local(local, stack);
}

_g int div_rf_ratio_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	bigtype value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	GetSignRatio(left, &sign1);
	castfixed_fixnum(right, &sign2, &value);
	sign1 = SignMulti(sign1, sign2);
	if (value == 0) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* ratio */
	if (zerop_ratio(left)) {
		ratio_zero_local(local, ret);
		return 0;
	}
	if (equal_rv_nosign(left, 1)) {
		inverse_value_ratio(local, ret, sign1, value);
		return 0;
	}

	/* fixnum */
	if (value == 1) {
		if (IsPlus(sign2))
			ratio_throw_local(local, left, ret);
		else
			sign_reverse_ratio_local(local, left, ret);
		return 0;
	}

	/* division */
	div_rv_ratio(local, sign1, left, value, ret);

	return 0;
}

_g int div_rf_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	bigtype value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	GetSignRatio(left, &sign1);
	castfixed_fixnum(right, &sign2, &value);
	sign1 = SignMulti(sign1, sign2);
	if (value == 0) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* ratio */
	if (zerop_ratio(left)) {
		fixnum_local(local, ret, 0);
		return 0;
	}
	if (equal_rv_nosign(left, 1)) {
		inverse_value_local(local, ret, sign1, value);
		return 0;
	}

	/* fixnum */
	if (value == 1) {
		if (IsPlus(sign2))
			ratio_throw_local(local, left, ret);
		else
			sign_reverse_ratio_local(local, left, ret);
		return 0;
	}

	/* division */
	div_rv_local(local, sign1, left, value, ret);

	return 0;
}

_g int div_rf_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	bigtype value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	GetSignRatio(left, &sign1);
	castfixed_fixnum(right, &sign2, &value);
	sign1 = SignMulti(sign1, sign2);
	if (value == 0) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* ratio */
	if (zerop_ratio(left)) {
		fixnum_heap(ret, 0);
		return 0;
	}
	if (equal_rv_nosign(left, 1)) {
		inverse_value_common(ret, sign1, value);
		return 0;
	}

	/* fixnum */
	if (value == 1) {
		if (IsPlus(sign2))
			ratio_throw_heap(left, ret);
		else
			sign_reverse_ratio_common(left, ret);
		return 0;
	}

	/* division */
	div_rv_common(local, sign1, left, value, ret);

	return 0;
}

static void ratio_sign_inverse_ratio(LocalRoot local, addr *ret, int sign, addr pos)
{
	addr numer, denom;

	Check(local == NULL, "local error");
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	bignum_throw_local(local, numer, &numer);
	bignum_throw_local(local, denom, &denom);
	make_ratio_local(local, ret, sign, denom, numer);
}

static void ratio_sign_inverse_local(LocalRoot local, addr *ret, int sign, addr pos)
{
	addr numer, denom;

	Check(local == NULL, "local error");
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	if (equal_value_nosign_bignum(numer, 1)) {
		bignum_copy_nosign_local(local, &pos, denom);
		SetSignBignum(pos, sign);
		bignum_result_local(local, pos, ret);
	}
	else {
		bignum_throw_local(local, numer, &numer);
		bignum_throw_local(local, denom, &denom);
		make_ratio_local(local, ret, sign, denom, numer);
	}
}

static void ratio_sign_inverse_common(LocalRoot local, addr *ret, int sign, addr pos)
{
	addr numer, denom;
	LocalStack stack;

	Check(local == NULL, "local error");
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	if (equal_value_nosign_bignum(numer, 1)) {
		push_local(local, &stack);
		bignum_copy_nosign_local(local, &pos, denom);
		SetSignBignum(pos, sign);
		bignum_result_heap(pos, ret);
		rollback_local(local, stack);
	}
	else {
		bignum_throw_heap(numer, &numer);
		bignum_throw_heap(denom, &denom);
		make_ratio_heap(ret, sign, denom, numer);
	}
}

static void div_vr_result(LocalRoot local,
		bigtype value, addr right, addr *rnumer, addr *rdenom)
{
	addr left, numer, denom;

	/*
	 *          numer     value   denom
	 *  value / -----  =  ----- * -----
	 *          denom       1     numer
	 */
	GetNumerRatio(right, &numer);
	GetDenomRatio(right, &denom);
	bignum_value_local(local, &left, SignPlus, value);
	bignum_copy_local(local, &numer, numer);
	reduction_local(local, left, numer);
	multi_bigdata_alloc(local, left, denom, &denom);
	*rnumer = numer;
	*rdenom = denom;
}

static void div_vr_ratio(LocalRoot local,
		int sign, bigtype value, addr right, addr *ret)
{
	addr numer, denom;

	div_vr_result(local, value, right, &numer, &denom);
	make_ratio_reduction_local(local, ret, sign, denom, numer);
}

static void div_vr_local(LocalRoot local,
		int sign, bigtype value, addr right, addr *ret)
{
	addr numer, denom;

	div_vr_result(local, value, right, &numer, &denom);
	ratio_noreduction_local(local, ret, sign, denom, numer);
}

static void div_vr_common(LocalRoot local,
		int sign, bigtype value, addr right, addr *ret)
{
	addr numer, denom;
	LocalStack stack;

	push_local(local, &stack);
	div_vr_result(local, value, right, &numer, &denom);
	ratio_noreduction_heap(ret, sign, denom, numer);
	rollback_local(local, stack);
}

_g int div_fr_ratio_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	bigtype value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	castfixed_fixnum(left, &sign1, &value);
	GetSignRatio(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_ratio(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* fixnum */
	if (value == 0) {
		ratio_zero_local(local, ret);
		return 0;
	}
	if (value == 1) {
		ratio_sign_inverse_ratio(local, ret, sign1, right);
		return 0;
	}

	/* ratio */
	if (equal_rv_nosign(right, 1)) {
		if (IsPlus(sign2))
			cast_fixnum_ratio_local(local, left, ret);
		else
			sigrev_fixnum_ratio_local(local, left, ret);
		return 0;
	}

	/* division */
	div_vr_ratio(local, sign1, value, right, ret);

	return 0;
}

_g int div_fr_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	bigtype value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	castfixed_fixnum(left, &sign1, &value);
	GetSignRatio(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_ratio(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* fixnum */
	if (value == 0) {
		fixnum_local(local, ret, 0);
		return 0;
	}
	if (value == 1) {
		ratio_sign_inverse_local(local, ret, sign1, right);
		return 0;
	}

	/* ratio */
	if (equal_rv_nosign(right, 1)) {
		if (IsPlus(sign2))
			fixnum_throw_local(local, left, ret);
		else
			sigrev_fixnum_integer_local(local, left, ret);
		return 0;
	}

	/* division */
	div_vr_local(local, sign1, value, right, ret);

	return 0;
}

_g int div_fr_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	bigtype value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	castfixed_fixnum(left, &sign1, &value);
	GetSignRatio(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_ratio(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* fixnum */
	if (value == 0) {
		fixnum_heap(ret, 0);
		return 0;
	}
	if (value == 1) {
		ratio_sign_inverse_common(local, ret, sign1, right);
		return 0;
	}

	/* ratio */
	if (equal_rv_nosign(right, 1)) {
		if (IsPlus(sign2))
			fixnum_throw_heap(left, ret);
		else
			sigrev_fixnum_integer_common(left, ret);
		return 0;
	}

	/* division */
	div_vr_common(local, sign1, value, right, ret);

	return 0;
}

static void bignum_throw_ratio_local(LocalRoot local, addr pos, addr *ret)
{
	if (IsMinus(RefSignBignum(pos))) {
		bignum_copy_local(local, ret, pos);
		SetSignBignum(*ret, SignPlus);
	}
	else {
		bignum_throw_local(local, pos, ret);
	}
}

static void bignum_throw_ratio_common(addr pos, addr *ret)
{
	if (IsMinus(RefSignBignum(pos))) {
		bignum_copy_heap(ret, pos);
		SetSignBignum(*ret, SignPlus);
	}
	else {
		bignum_throw_heap(pos, ret);
	}
}

static inline void bignum_sign_inverse_ratio(LocalRoot local,
		addr *ret, int sign, addr denom)
{
	addr numer;

	Check(local == NULL, "local error");
	bignum_value_local(local, &numer, SignPlus, 1);
	bignum_throw_ratio_local(local, denom, &denom);
	make_ratio_local(local, ret, sign, numer, denom);
}

static inline void bignum_sign_inverse_local(LocalRoot local,
		addr *ret, int sign, addr denom)
{
	addr numer;

	Check(local == NULL, "local error");
	if (equal_value_nosign_bignum(denom, 1)) {
		fixnum_local(local, ret, IsPlus(sign)? 1: -1);
	}
	else {
		bignum_value_local(local, &numer, SignPlus, 1);
		bignum_throw_ratio_local(local, denom, &denom);
		make_ratio_local(local, ret, sign, numer, denom);
	}
}

static inline void bignum_sign_inverse_common(addr *ret, int sign, addr denom)
{
	addr numer;

	if (equal_value_nosign_bignum(denom, 1)) {
		fixnum_heap(ret, IsPlus(sign)? 1: -1);
	}
	else {
		bignum_value_heap(&numer, SignPlus, 1);
		bignum_throw_ratio_common(denom, &denom);
		make_ratio_heap(ret, sign, numer, denom);
	}
}

_g int div_rb_ratio_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	GetSignRatio(left, &sign1);
	GetSignBignum(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_bignum(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* ratio */
	if (zerop_ratio(left)) {
		ratio_zero_local(local, ret);
		return 0;
	}
	if (equal_rv_nosign(left, 1)) {
		bignum_sign_inverse_ratio(local, ret, sign1, right);
		return 0;
	}

	/* bignum */
	if (equal_value_nosign_bignum(right, 1)) {
		if (IsPlus(sign2))
			ratio_throw_local(local, left, ret);
		else
			sign_reverse_ratio_local(local, left, ret);
		return 0;
	}

	/* division */
	div_rb_ratio(local, sign1, left, right, ret);

	return 0;
}

_g int div_rb_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	GetSignRatio(left, &sign1);
	GetSignBignum(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_bignum(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* ratio */
	if (zerop_ratio(left)) {
		fixnum_local(local, ret, 0);
		return 0;
	}
	if (equal_rv_nosign(left, 1)) {
		bignum_sign_inverse_local(local, ret, sign1, right);
		return 0;
	}

	/* bignum */
	if (equal_value_nosign_bignum(right, 1)) {
		if (IsPlus(sign2))
			ratio_throw_local(local, left, ret);
		else
			sign_reverse_ratio_local(local, left, ret);
		return 0;
	}

	/* division */
	div_rb_local(local, sign1, left, right, ret);

	return 0;
}

_g int div_rb_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	GetSignRatio(left, &sign1);
	GetSignBignum(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_bignum(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* ratio */
	if (zerop_ratio(left)) {
		fixnum_heap(ret, 0);
		return 0;
	}
	if (equal_rv_nosign(left, 1)) {
		bignum_sign_inverse_common(ret, sign1, right);
		return 0;
	}

	/* bignum */
	if (equal_value_nosign_bignum(right, 1)) {
		if (IsPlus(sign2))
			ratio_throw_heap(left, ret);
		else
			sign_reverse_ratio_common(left, ret);
		return 0;
	}

	/* division */
	div_rb_common(local, sign1, left, right, ret);

	return 0;
}

static void div_br_result(LocalRoot local,
		addr left, addr right, addr *rnumer, addr *rdenom)
{
	addr numer, denom;

	/*
	 *          numer  left    denom
	 *  left / ----- = ----- * -----
	 *          denom    1     numer
	 */
	GetNumerRatio(right, &numer);
	GetDenomRatio(right, &denom);
	bignum_copy_local(local, &left, left);
	bignum_copy_local(local, &numer, numer);
	reduction_local(local, left, numer);
	multi_bigdata_alloc(local, left, denom, &denom);
	*rnumer = denom;
	*rdenom = numer;
}

static void div_br_ratio(LocalRoot local, int sign, addr left, addr right, addr *ret)
{
	div_br_result(local, left, right, &left, &right);
	make_ratio_reduction_local(local, ret, sign, left, right);
}

static void div_br_local(LocalRoot local, int sign, addr left, addr right, addr *ret)
{
	div_br_result(local, left, right, &left, &right);
	ratio_noreduction_local(local, ret, sign, left, right);
}

static void div_br_common(LocalRoot local, int sign, addr left, addr right, addr *ret)
{
	LocalStack stack;

	push_local(local, &stack);
	div_br_result(local, left, right, &left, &right);
	ratio_noreduction_heap(ret, sign, left, right);
	rollback_local(local, stack);
}

_g int div_br_ratio_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	GetSignBignum(left, &sign1);
	GetSignRatio(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_ratio(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* bignum */
	if (zerop_bignum(left)) {
		ratio_zero_local(local, ret);
		return 0;
	}
	if (equal_value_nosign_bignum(left, 1)) {
		ratio_sign_inverse_ratio(local, ret, sign1, right);
		return 0;
	}

	/* ratio */
	if (equal_rv_nosign(right, 1)) {
		if (IsPlus(sign2))
			cast_bignum_ratio_local(local, left, ret);
		else
			sigrev_bignum_ratio_local(local, left, ret);
		return 0;
	}

	/* division */
	div_br_ratio(local, sign1, left, right, ret);

	return 0;
}

_g int div_br_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	GetSignBignum(left, &sign1);
	GetSignRatio(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_ratio(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* bignum */
	if (zerop_bignum(left)) {
		fixnum_local(local, ret, 0);
		return 0;
	}
	if (equal_value_nosign_bignum(left, 1)) {
		ratio_sign_inverse_local(local, ret, sign1, right);
		return 0;
	}

	/* ratio */
	if (equal_rv_nosign(right, 1)) {
		if (IsPlus(sign2))
			bignum_throw_local(local, left, ret);
		else
			sigrev_bignum_integer_local(local, left, ret);
		return 0;
	}

	/* division */
	div_br_local(local, sign1, left, right, ret);

	return 0;
}

_g int div_br_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	GetSignBignum(left, &sign1);
	GetSignRatio(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_ratio(right)) {
		*ret = 0;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* bignum */
	if (zerop_bignum(left)) {
		fixnum_heap(ret, 0);
		return 0;
	}
	if (equal_value_nosign_bignum(left, 1)) {
		ratio_sign_inverse_common(local, ret, sign1, right);
		return 0;
	}

	/* ratio */
	if (equal_rv_nosign(right, 1)) {
		if (IsPlus(sign2))
			bignum_throw_heap(left, ret);
		else
			sigrev_bignum_integer_common(left, ret);
		return 0;
	}

	/* division */
	div_br_common(local, sign1, left, right, ret);

	return 0;
}

static void div_rr_result(LocalRoot local,
		addr left, addr right, addr *rnumer, addr *rdenom)
{
	addr numer1, numer2, denom1, denom2;

	/*
	 *  numer1   numer2   numer1  denom2
	 *  ------ / ------ = ----- * ------
	 *  denom1   denom2   denom1  numer2
	 */
	GetNumerRatio(left, &numer1);
	GetDenomRatio(left, &denom1);
	GetNumerRatio(right, &denom2);
	GetDenomRatio(right, &numer2);
	bignum_copy_local(local, &numer1, numer1);
	bignum_copy_local(local, &denom1, denom1);
	bignum_copy_local(local, &numer2, numer2);
	bignum_copy_local(local, &denom2, denom2);
	reduction_local(local, numer1, denom2);
	reduction_local(local, numer2, denom1);
	multi_bigdata_alloc(local, numer1, numer2, &numer1);
	multi_bigdata_alloc(local, denom1, denom2, &denom1);
	*rnumer = numer1;
	*rdenom = denom1;
}

static void div_rr_ratio(LocalRoot local, int sign, addr left, addr right, addr *ret)
{
	div_rr_result(local, left, right, &left, &right);
	make_ratio_reduction_local(local, ret, sign, left, right);
}

static void div_rr_local(LocalRoot local, int sign, addr left, addr right, addr *ret)
{
	div_rr_result(local, left, right, &left, &right);
	ratio_noreduction_local(local, ret, sign, left, right);
}

static void div_rr_common(LocalRoot local, int sign, addr left, addr right, addr *ret)
{
	LocalStack stack;

	push_local(local, &stack);
	div_rr_result(local, left, right, &left, &right);
	ratio_noreduction_heap(ret, sign, left, right);
	rollback_local(local, stack);
}

static void div_bir_result(LocalRoot local,
		addr left, addr right, addr *rnumer, addr *rdenom)
{
	addr numer, denom;

	/*
	 *    1     numer     1     denom
	 *  ----- / ----- = ----- * -----
	 *  left    denom   left    numer
	 */
	GetNumerRatio(right, &numer);
	GetDenomRatio(right, &denom);
	bignum_copy_local(local, &left, left);
	bignum_copy_local(local, &denom, denom);
	reduction_local(local, left, denom);
	multi_bigdata_alloc(local, left, numer, &numer);
	*rnumer = denom;
	*rdenom = numer;
}

static void div_bir_ratio(LocalRoot local, int sign, addr left, addr right, addr *ret)
{
	div_bir_result(local, left, right, &left, &right);
	make_ratio_reduction_local(local, ret, sign, left, right);
}

static void div_bir_local(LocalRoot local, int sign, addr left, addr right, addr *ret)
{
	div_bir_result(local, left, right, &left, &right);
	ratio_noreduction_local(local, ret, sign, left, right);
}

static void div_bir_common(LocalRoot local, int sign, addr left, addr right, addr *ret)
{
	LocalStack stack;

	push_local(local, &stack);
	div_bir_result(local, left, right, &left, &right);
	ratio_noreduction_heap(ret, sign, left, right);
	rollback_local(local, stack);
}

_g int div_rr_ratio_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	GetSignRatio(left, &sign1);
	GetSignRatio(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_ratio(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* ratio */
	if (zerop_ratio(left)) {
		ratio_zero_local(local, ret);
		return 0;
	}
	if (equal_rv_nosign(left, 1)) {
		ratio_sign_inverse_ratio(local, ret, sign1, right);
		return 0;
	}
	if (inverse_ratio_p(left)) {
		GetDenomRatio(left, &left);
		div_bir_ratio(local, sign1, left, right, ret);
		return 0;
	}

	/* ratio */
	if (equal_rv_nosign(right, 1)) {
		if (IsPlus(sign2))
			ratio_throw_local(local, left, ret);
		else
			sign_reverse_ratio_local(local, left, ret);
		return 0;
	}
	if (inverse_ratio_p(right)) {
		GetDenomRatio(right, &right);
		div_rb_ratio(local, sign1, left, right, ret);
		return 0;
	}

	/* division */
	div_rr_ratio(local, sign1, left, right, ret);

	return 0;
}

_g int div_rr_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	GetSignRatio(left, &sign1);
	GetSignRatio(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_ratio(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* ratio */
	if (zerop_ratio(left)) {
		fixnum_local(local, ret, 0);
		return 0;
	}
	if (equal_rv_nosign(left, 1)) {
		ratio_sign_inverse_local(local, ret, sign1, right);
		return 0;
	}
	if (inverse_ratio_p(left)) {
		GetDenomRatio(left, &left);
		div_bir_local(local, sign1, left, right, ret);
		return 0;
	}

	/* ratio */
	if (equal_rv_nosign(right, 1)) {
		if (IsPlus(sign2))
			ratio_throw_local(local, left, ret);
		else
			sign_reverse_ratio_local(local, left, ret);
		return 0;
	}
	if (inverse_ratio_p(right)) {
		GetDenomRatio(right, &right);
		div_rb_local(local, sign1, left, right, ret);
		return 0;
	}

	/* division */
	div_rr_local(local, sign1, left, right, ret);

	return 0;
}

_g int div_rr_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	GetSignRatio(left, &sign1);
	GetSignRatio(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_ratio(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* ratio */
	if (zerop_ratio(left)) {
		fixnum_heap(ret, 0);
		return 0;
	}
	if (equal_rv_nosign(left, 1)) {
		ratio_sign_inverse_common(local, ret, sign1, right);
		return 0;
	}
	if (inverse_ratio_p(left)) {
		GetDenomRatio(left, &left);
		div_bir_common(local, sign1, left, right, ret);
		return 0;
	}

	/* ratio */
	if (equal_rv_nosign(right, 1)) {
		if (IsPlus(sign2))
			ratio_throw_heap(left, ret);
		else
			sign_reverse_ratio_common(left, ret);
		return 0;
	}
	if (inverse_ratio_p(right)) {
		GetDenomRatio(right, &right);
		div_rb_common(local, sign1, left, right, ret);
		return 0;
	}

	/* division */
	div_rr_common(local, sign1, left, right, ret);

	return 0;
}


/*
 *  division - integer
 */
_g int div_ff_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	bigtype value1, value2;
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	castfixed_fixnum(left, &sign1, &value1);
	castfixed_fixnum(right, &sign2, &value2);
	sign1 = SignMulti(sign1, sign2);
	if (value2 == 0) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* left */
	if (value1 == 0) {
		fixnum_heap(ret, 0);
		return 0;
	}
	if (value1 == 1) {
		inverse_value_common(ret, sign1, value2);
		return 0;
	}

	/* right */
	if (value2 == 1) {
		if (IsPlus(sign2))
			fixnum_throw_heap(left, ret);
		else
			sigrev_fixnum_integer_common(left, ret);
		return 0;
	}

	push_local(local, &stack);
	bignum_value_alloc(local, &left, SignPlus, value1);
	bignum_value_alloc(local, &right, SignPlus, value2);
	ratio_reduction_heap(local, ret, sign1, left, right);
	rollback_local(local, stack);

	return 0;
}

_g int div_ff_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	bigtype value1, value2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	castfixed_fixnum(left, &sign1, &value1);
	castfixed_fixnum(right, &sign2, &value2);
	sign1 = SignMulti(sign1, sign2);
	if (value2 == 0) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* left */
	if (value1 == 0) {
		fixnum_local(local, ret, 0);
		return 0;
	}
	if (value1 == 1) {
		inverse_value_local(local, ret, sign1, value2);
		return 0;
	}

	/* right */
	if (value2 == 1) {
		if (IsPlus(sign2))
			fixnum_throw_local(local, left, ret);
		else
			sigrev_fixnum_bignum_local(local, left, ret);
		return 0;
	}

	bignum_value_alloc(local, &left, SignPlus, value1);
	bignum_value_alloc(local, &right, SignPlus, value2);
	ratio_reduction_local(local, ret, sign1, left, right);

	return 0;
}

_g int div_fb_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	bigtype value1;
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	castfixed_fixnum(left, &sign1, &value1);
	GetSignBignum(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_bignum(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* left */
	if (value1 == 0) {
		fixnum_heap(ret, 0);
		return 0;
	}
	if (value1 == 1) {
		bignum_sign_inverse_common(ret, sign1, right);
		return 0;
	}

	/* right */
	if (equal_value_nosign_bignum(right, 1)) {
		fixnum_throw_heap(left, ret);
		return 0;
	}

	push_local(local, &stack);
	bignum_value_alloc(local, &left, SignPlus, value1);
	bignum_copy_alloc(local, &right, right);
	SetSignBignum(right, SignPlus);
	ratio_reduction_heap(local, ret, sign1, left, right);
	rollback_local(local, stack);

	return 0;
}

_g int div_fb_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	bigtype value1;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	castfixed_fixnum(left, &sign1, &value1);
	GetSignBignum(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_bignum(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* left */
	if (value1 == 0) {
		fixnum_local(local, ret, 0);
		return 0;
	}
	if (value1 == 1) {
		bignum_sign_inverse_local(local, ret, sign1, right);
		return 0;
	}

	/* right */
	if (equal_value_nosign_bignum(right, 1)) {
		fixnum_throw_local(local, left, ret);
		return 0;
	}

	bignum_value_alloc(local, &left, SignPlus, value1);
	bignum_copy_alloc(local, &right, right);
	SetSignBignum(right, SignPlus);
	ratio_reduction_local(local, ret, sign1, left, right);

	return 0;
}

_g int div_bf_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	bigtype value2;
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	GetSignBignum(left, &sign1);
	castfixed_fixnum(right, &sign2, &value2);
	sign1 = SignMulti(sign1, sign2);
	if (value2 == 0) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* left */
	if (zerop_bignum(left)) {
		fixnum_heap(ret, 0);
		return 0;
	}
	if (equal_value_nosign_bignum(left, 1)) {
		inverse_value_common(ret, sign2, value2);
		return 0;
	}

	/* right */
	if (value2 == 1) {
		if (IsPlus(sign2))
			bignum_throw_heap(left, ret);
		else
			sigrev_bignum_integer_common(left, ret);
		return 0;
	}

	push_local(local, &stack);
	bignum_copy_alloc(local, &left, left);
	SetSignBignum(left, SignPlus);
	bignum_value_alloc(local, &right, SignPlus, value2);
	ratio_reduction_heap(local, ret, sign1, left, right);
	rollback_local(local, stack);

	return 0;
}

_g int div_bf_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	bigtype value2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	GetSignBignum(left, &sign1);
	castfixed_fixnum(right, &sign2, &value2);
	sign1 = SignMulti(sign1, sign2);
	if (value2 == 0) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* left */
	if (zerop_bignum(left)) {
		fixnum_local(local, ret, 0);
		return 0;
	}
	if (equal_value_nosign_bignum(left, 1)) {
		inverse_value_local(local, ret, sign2, value2);
		return 0;
	}

	/* right */
	if (value2 == 1) {
		if (IsPlus(sign2))
			bignum_throw_local(local, left, ret);
		else
			sigrev_bignum_bignum_local(local, left, ret);
		return 0;
	}

	bignum_copy_alloc(local, &left, left);
	SetSignBignum(left, SignPlus);
	bignum_value_alloc(local, &right, SignPlus, value2);
	ratio_reduction_local(local, ret, sign1, left, right);

	return 0;
}

_g int div_bb_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	GetSignBignum(left, &sign1);
	GetSignBignum(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_bignum(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* left */
	if (zerop_bignum(left)) {
		fixnum_heap(ret, 0);
		return 0;
	}
	if (equal_value_nosign_bignum(left, 1)) {
		bignum_sign_inverse_common(ret, sign1, left);
		return 0;
	}

	/* right */
	if (equal_value_nosign_bignum(right, 1)) {
		if (IsPlus(sign1))
			return integer_copy_heap_(left, ret);
		else
			return integer_copysign_heap_(SignMinus, left, ret);
	}

	push_local(local, &stack);
	bignum_copy_nosign_alloc(local, &left, left);
	bignum_copy_nosign_alloc(local, &right, right);
	ratio_reduction_heap(local, ret, sign1, left, right);
	rollback_local(local, stack);

	return 0;
}

_g int div_bb_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	GetSignBignum(left, &sign1);
	GetSignBignum(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_bignum(right)) {
		*ret = Nil;
		return call_division_by_zero2_(NULL, left, right);
	}

	/* left */
	if (zerop_bignum(left)) {
		fixnum_local(local, ret, 0);
		return 0;
	}
	if (equal_value_nosign_bignum(left, 1)) {
		bignum_sign_inverse_local(local, ret, sign1, left);
		return 0;
	}

	/* right */
	if (equal_value_nosign_bignum(right, 1)) {
		if (IsPlus(sign1))
			return integer_copy_local_(local, left, ret);
		else
			return integer_copysign_local_(local, SignMinus, left, ret);
	}

	bignum_copy_nosign_alloc(local, &left, left);
	bignum_copy_nosign_alloc(local, &right, right);
	ratio_reduction_local(local, ret, sign1, left, right);

	return 0;
}


/*
 *  inverse
 */
_g int inverse_fixnum_ratio_local_(LocalRoot local, addr pos, addr *ret)
{
	int sign;
	bigtype value;

	CheckType(pos, LISPTYPE_FIXNUM);
	castfixed_fixnum(pos, &sign, &value);
	if (value == 0) {
		*ret = Nil;
		return call_division_by_zero1_(NULL, pos);
	}
	ratio_noreduction_value_local(local, ret, sign, 1, value);

	return 0;
}

_g int inverse_bignum_ratio_local_(LocalRoot local, addr pos, addr *ret)
{
	int sign;
	addr numer, denom;

	CheckType(pos, LISPTYPE_BIGNUM);
	if (zerop_bignum(pos)) {
		*ret = Nil;
		return call_division_by_zero1_(NULL, pos);
	}
	GetSignBignum(pos, &sign);
	bignum_value_local(local, &numer, SignPlus, 1);
	bignum_copy_local(local, &denom, pos);
	make_ratio_local(local, ret, sign, numer, denom);

	return 0;
}

_g int inverse_ratio_local_(LocalRoot local, addr pos, addr *ret)
{
	int sign;
	addr numer, denom;

	CheckType(pos, LISPTYPE_RATIO);
	if (zerop_ratio(pos)) {
		*ret = Nil;
		return call_division_by_zero1_(NULL, pos);
	}
	GetSignRatio(pos, &sign);
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	bignum_copy_local(local, &numer, numer);
	bignum_copy_local(local, &denom, denom);
	make_ratio_local(local, ret, sign, denom, numer);

	return 0;
}

_g int inverse_fixnum_common_(addr left, addr *ret)
{
	int sign;
	bigtype value;

	castfixed_fixnum(left, &sign, &value);
	if (value == 0) {
		*ret = Nil;
		return call_division_by_zero1_(NULL, left);
	}
	if (value == 1) {
		fixnum_throw_heap(left, ret);
		return 0;
	}
	inverse_value_common(ret, sign, value);

	return 0;
}

_g int inverse_bignum_common_(addr left, addr *ret)
{
	int sign;

	if (zerop_bignum(left)) {
		*ret = Nil;
		return call_division_by_zero1_(NULL, left);
	}
	if (equal_rv_nosign(left, 1)) {
		bignum_throw_heap(left, ret);
		return 0;
	}
	GetSignBignum(left, &sign);
	bignum_sign_inverse_common(ret, sign, left);

	return 0;
}

_g int inverse_ratio_common_(LocalRoot local, addr left, addr *ret)
{
	int sign;

	if (zerop_ratio(left)) {
		*ret = Nil;
		return call_division_by_zero1_(NULL, left);
	}
	GetSignRatio(left, &sign);
	ratio_sign_inverse_common(local, ret, sign, left);

	return 0;
}

