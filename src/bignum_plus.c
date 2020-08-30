#include "bignum_data.h"
#include "bignum_equal.h"
#include "bignum_object.h"
#include "bignum_plus.h"
#include "local.h"
#include "memory.h"
#include "typedef.h"

static void carryvalue_alloc(LocalRoot local, addr *ret, int sign, bigtype value)
{
	addr pos, root;
	bigtype *data;

	Check(sign != SignPlus && sign != SignMinus, "sign error");
	alloc_bignum(local, &pos, 2);
	SetSignBignum(pos, sign);
	SetSizeBignum(pos, 2);
	GetRootDataBignum(pos, &root, &data);
	data[0] = (bigtype)value;
	data[1] = 1;
	*ret = pos;
}

/*****************************************************************************
  plus
 *****************************************************************************/
static void plusvalue(LocalRoot local, addr left, bigtype value, addr *ret)
{
	int sign;
	size_t size;

	GetSignBignum(left, &sign);
	GetSizeBignum(left, &size);
	if (IsPlus(sign)) {
		alloc_bignum(local, ret, size + 1);
		setplusvalue_bigdata(*ret, left, sign, value);
	}
	else {
		alloc_bignum(local, ret, size);
		setminusvalue_bigdata(*ret, left, sign, value);
	}
}

static void minusvalue(LocalRoot local, addr left, bigtype value, addr *ret)
{
	int sign;
	size_t size;

	GetSignBignum(left, &sign);
	GetSizeBignum(left, &size);
	if (IsPlus(sign)) {
		alloc_bignum(local, ret, size);
		setminusvalue_bigdata(*ret, left, sign, value);
	}
	else {
		alloc_bignum(local, ret, size + 1);
		setplusvalue_bigdata(*ret, left, sign, value);
	}
}

#define plus_vv_overflow(v1, v2) \
	(((v2) > 0 && (v1) > (FIXNUM_MAX - (v2))) || \
	 ((v2) < 0 && (v1) < (FIXNUM_MIN - (v2))))

static inline void plus_vv_bignum_local(LocalRoot local,
		fixnum v1, fixnum v2, addr *ret)
{
	int sign;
	bigtype fixed1, fixed2;

	if (plus_vv_overflow(v1, v2)) {
		castfixed(v1, &sign, &fixed1);
		castfixed(v2, &sign, &fixed2);
		plusnumber_bigdata(&fixed1, &fixed2);
		if (fixed2)
			carryvalue_alloc(local, ret, sign, fixed1);
		else
			bignum_value_local(local, ret, sign, fixed1);
	}
	else {
		castfixed(v1 + v2, &sign, &fixed1);
		bignum_value_local(local, ret, sign, fixed1);
	}
}

static inline void plus_vv_real_alloc(LocalRoot local, fixnum v1, fixnum v2, addr *ret)
{
	int sign;
	bigtype fixed1, fixed2;

	if (plus_vv_overflow(v1, v2)) {
		castfixed(v1, &sign, &fixed1);
		castfixed(v2, &sign, &fixed2);
		plusnumber_bigdata(&fixed1, &fixed2);
		if (fixed2)
			carryvalue_alloc(local, ret, sign, fixed1);
		else
			bignum_value_alloc(local, ret, sign, fixed1);
	}
	else {
		fixnum_alloc(local, ret, v1 + v2);
	}
}

_g void plus_fv_bignum_local(LocalRoot local, addr left, fixnum value2, addr *ret)
{
	fixnum value1;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type error");

	/* left + 0 */
	GetFixnum(left, &value1);
	if (value2 == 0) {
		bignum_fixnum_value_local(local, ret, value1);
		return;
	}

	/* 0 + right */
	if (value1 == 0) {
		bignum_fixnum_value_local(local, ret, value2);
		return;
	}

	plus_vv_bignum_local(local, value1, value2, ret);
}

static void plus_fv_real_alloc(LocalRoot local, addr left, fixnum value2, addr *ret)
{
	fixnum value1;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type error");

	/* left + 0 */
	if (value2 == 0) {
		fixnum_throw_alloc(local, left, ret);
		return;
	}

	/* 0 + right */
	GetFixnum(left, &value1);
	if (value1 == 0) {
		fixnum_alloc(local, ret, value2);
		return;
	}

	plus_vv_real_alloc(local, value1, value2, ret);
}

_g void plus_fv_real_local(LocalRoot local, addr left, fixnum value2, addr *ret)
{
	Check(local == NULL, "local error");
	plus_fv_real_alloc(local, left, value2, ret);
}

_g void plus_fv_real_common(addr left, fixnum value2, addr *ret)
{
	plus_fv_real_alloc(NULL, left, value2, ret);
}

_g void plus_ff_bignum_local(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value1, value2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* 0 + right */
	GetFixnum(left, &value1);
	GetFixnum(right, &value2);
	if (value1 == 0) {
		bignum_fixnum_value_local(local, ret, value2);
		return;
	}

	/* left + 0 */
	if (value2 == 0) {
		bignum_fixnum_value_local(local, ret, value1);
		return;
	}

	plus_vv_bignum_local(local, value1, value2, ret);
}

static void plus_ff_real_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value1, value2;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* 0 + right */
	GetFixnum(left, &value1);
	if (value1 == 0) {
		fixnum_throw_alloc(local, right, ret);
		return;
	}

	/* left + 0 */
	GetFixnum(right, &value2);
	if (value2 == 0) {
		fixnum_throw_alloc(local, left, ret);
		return;
	}

	plus_vv_real_alloc(local, value1, value2, ret);
}

_g void plus_ff_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	plus_ff_real_alloc(local, left, right, ret);
}

_g void plus_ff_real_common(addr left, addr right, addr *ret)
{
	plus_ff_real_alloc(NULL, left, right, ret);
}

static inline void plusfixnum_bignum_local(LocalRoot local,
		addr left, fixnum right, addr *ret)
{
	int sign;
	bigtype value;

	Check(local == NULL, "local error");
	castfixed(right, &sign, &value);
	if (IsPlus(sign)) {
		plusvalue(local, left, value, ret);
	}
	else {
		minusvalue(local, left, value, ret);
	}
}

static inline void plusfixnum_real_local(LocalRoot local,
		addr left, fixnum right, addr *ret)
{
	plusfixnum_bignum_local(local, left, right, &left);
	bignum_result_local(local, left, ret);
}

static inline void plusfixnum_real_common(LocalRoot local,
		addr left, fixnum right, addr *ret)
{
	LocalStack stack;

	push_local(local, &stack);
	plusfixnum_bignum_local(local, left, right, &left);
	bignum_result_heap(left, ret);
	rollback_local(local, stack);
}

_g void plus_bv_bignum_local(LocalRoot local, addr left, fixnum right, addr *ret)
{
	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type error");

	/* left + 0 */
	if (right == 0) {
		bignum_throw_local(local, left, ret);
		return;
	}

	/* 0 + right */
	if (zerop_bignum(left)) {
		bignum_fixnum_value_local(local, ret, right);
		return;
	}

	plusfixnum_bignum_local(local, left, right, ret);
}

_g void plus_bv_real_local(LocalRoot local, addr left, fixnum right, addr *ret)
{
	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type error");

	/* left + 0 */
	if (right == 0) {
		bignum_throw_local(local, left, ret);
		return;
	}

	/* 0 + right */
	if (zerop_bignum(left)) {
		fixnum_local(local, ret, right);
		return;
	}

	plusfixnum_real_local(local, left, right, ret);
}

_g void plus_bv_real_common(LocalRoot local, addr left, fixnum right, addr *ret)
{
	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type error");

	/* left + 0 */
	if (right == 0) {
		bignum_throw_heap(left, ret);
		return;
	}

	/* 0 + right */
	if (zerop_bignum(left)) {
		fixnum_heap(ret, right);
		return;
	}

	plusfixnum_real_common(local, left, right, ret);
}

_g void plus_bf_bignum_local(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum check;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* 0 + right */
	if (zerop_bignum(left)) {
		bignum_fixnum_local(local, ret, right);
		return;
	}

	/* left + 0 */
	GetFixnum(right, &check);
	if (check == 0) {
		bignum_throw_local(local, left, ret);
		return;
	}

	plusfixnum_bignum_local(local, left, check, ret);
}

_g void plus_bf_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum check;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* 0 + right */
	if (zerop_bignum(left)) {
		fixnum_throw_local(local, right, ret);
		return;
	}

	/* left + 0 */
	GetFixnum(right, &check);
	if (check == 0) {
		bignum_throw_local(local, left, ret);
		return;
	}

	plusfixnum_real_local(local, left, check, ret);
}

_g void plus_bf_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum check;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* 0 + right */
	if (zerop_bignum(left)) {
		fixnum_throw_heap(right, ret);
		return;
	}

	/* left + 0 */
	GetFixnum(right, &check);
	if (check == 0) {
		bignum_throw_heap(left, ret);
		return;
	}

	plusfixnum_real_common(local, left, check, ret);
}

_g void plus_bb_bignum_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, compare;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	if (zerop_bignum(left)) {
		bignum_throw_local(local, right, ret);
		return;
	}
	if (zerop_bignum(right)) {
		bignum_throw_local(local, left, ret);
		return;
	}
	GetSignBignum(left, &sign1);
	GetSignBignum(right, &sign2);
	if (sign1 == sign2) {
		plus_bigdata_alloc(local, left, right, &left);
		SetSignBignum(left, sign1);
		*ret = left;
		return;
	}
	compare = compare_bigdata(left, right);
	if (compare == 0) {
		bignum_value_alloc(local, ret, SignPlus, 0);
		return;
	}
	if (compare < 0) {
		minus_bigdata_alloc(local, right, left, &left);
		compare = sign2;
	}
	else {
		minus_bigdata_alloc(local, left, right, &left);
		compare = sign1;
	}
	SetSignBignum(left, compare);
	*ret = left;
}

_g void plus_bb_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, compare;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	if (zerop_bignum(left)) {
		bignum_throw_local(local, right, ret);
		return;
	}
	if (zerop_bignum(right)) {
		bignum_throw_local(local, left, ret);
		return;
	}
	GetSignBignum(left, &sign1);
	GetSignBignum(right, &sign2);
	if (sign1 == sign2) {
		plus_bigdata_alloc(local, left, right, &left);
		SetSignBignum(left, sign1);
		goto finish;
	}
	compare = compare_bigdata(left, right);
	if (compare == 0) {
		fixnum_local(local, ret, 0);
		return;
	}
	if (compare < 0) {
		minus_bigdata_alloc(local, right, left, &left);
		compare = sign2;
	}
	else {
		minus_bigdata_alloc(local, left, right, &left);
		compare = sign1;
	}
	SetSignBignum(left, compare);

finish:
	bignum_result_local(local, left, ret);
}

_g void plus_bb_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, compare;
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	if (zerop_bignum(left)) {
		bignum_throw_heap(right, ret);
		return;
	}
	if (zerop_bignum(right)) {
		bignum_throw_heap(left, ret);
		return;
	}
	GetSignBignum(left, &sign1);
	GetSignBignum(right, &sign2);
	if (sign1 == sign2) {
		push_local(local, &stack);
		plus_bigdata_alloc(local, left, right, &left);
		SetSignBignum(left, sign1);
		goto finish;
	}
	compare = compare_bigdata(left, right);
	if (compare == 0) {
		fixnum_heap(ret, 0);
		return;
	}
	push_local(local, &stack);
	if (compare < 0) {
		minus_bigdata_alloc(local, right, left, &left);
		compare = sign2;
	}
	else {
		minus_bigdata_alloc(local, left, right, &left);
		compare = sign1;
	}
	SetSignBignum(left, compare);

finish:
	bignum_result_heap(left, ret);
	rollback_local(local, stack);
}


/*****************************************************************************
  minus
 *****************************************************************************/
_g void sigrev_bignum_inplace(addr pos)
{
	int sign;

	Check(GetType(pos) != LISPTYPE_BIGNUM, "type error");
	GetSignBignum(pos, &sign);
	sign = SignNot(sign);
	SetSignBignum(pos, sign);
}

_g void sigrev_fixnum_bignum_local(LocalRoot local, addr left, addr *ret)
{
	int sign;
	bigtype value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type error");
	castfixed_fixnum(left, &sign, &value);
	bignum_value_local(local, ret, SignNot(sign), value);
}

_g void sigrev_fixnum_integer_alloc(LocalRoot local, addr left, addr *ret)
{
	fixnum value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type error");
	GetFixnum(left, &value);
	if (value == FIXNUM_MIN) {
		bignum_value_alloc(local, ret, SignPlus, FIXNUM_UMIN);
	}
	else {
		fixnum_alloc(local, ret, -value);
	}
}

_g void sigrev_fixnum_integer_local(LocalRoot local, addr left, addr *ret)
{
	Check(local == NULL, "local error");
	sigrev_fixnum_integer_alloc(local, left, ret);
}

_g void sigrev_fixnum_integer_common(addr left, addr *ret)
{
	sigrev_fixnum_integer_alloc(NULL, left, ret);
}

_g void sigrev_bignum_bignum_local(LocalRoot local, addr left, addr *ret)
{
	int sign;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type error");
	GetSignBignum(left, &sign);
	bignum_copy_local(local, &left, left);
	SetSignBignum(left, SignNot(sign));
	*ret = left;
}

static void inline sigrev_bignum_integer_alloc(LocalRoot local, addr left, addr *ret)
{
	int sign;
	addr root;
	bigtype *data, value;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type error");
	GetSignBignum(left, &sign);
	if (RefSizeBignum(left) != 1) {
		bignum_copy_nosign_alloc(local, &left, left);
		SetSignBignum(left, SignNot(sign));
		*ret = left;
		return;
	}

	GetRootDataBignum(left, &root, &data);
	value = data[0];
	if (IsPlus(sign)) {
		if (value <= FIXNUM_UMIN)
			fixnum_alloc(local, ret, -(fixnum)value);
		else
			bignum_value_alloc(local, ret, SignMinus, value);
	}
	else {
		if (value <= FIXNUM_MAX)
			fixnum_alloc(local, ret, (fixnum)value);
		else
			bignum_value_alloc(local, ret, SignPlus, value);
	}
}

_g void sigrev_bignum_integer_local(LocalRoot local, addr left, addr *ret)
{
	Check(local == NULL, "local error");
	sigrev_bignum_integer_alloc(local, left, ret);
}

_g void sigrev_bignum_integer_common(addr left, addr *ret)
{
	sigrev_bignum_integer_alloc(NULL, left, ret);
}

#define minus_vv_overflow(v1, v2) \
	(((v2) > 0 && (v1) < (FIXNUM_MIN + (v2))) || \
	 ((v2) < 0 && (v1) > (FIXNUM_MAX + (v2))))

static inline void minus_vv_bignum_local(LocalRoot local,
		fixnum v1, fixnum v2, addr *ret)
{
	int sign;
	bigtype fixed1, fixed2;

	if (minus_vv_overflow(v1, v2)) {
		castfixed(v2, &sign, &fixed2);
		castfixed(v1, &sign, &fixed1);
		bignum_value_local(local, ret, sign, fixed1 + fixed2);
	}
	else {
		castfixed(v1 - v2, &sign, &fixed1);
		bignum_value_local(local, ret, sign, fixed1);
	}
}

static inline void minus_vv_real_alloc(LocalRoot local,
		fixnum v1, fixnum v2, addr *ret)
{
	int sign;
	bigtype fixed1, fixed2;

	if (minus_vv_overflow(v1, v2)) {
		castfixed(v2, &sign, &fixed2);
		castfixed(v1, &sign, &fixed1);
		bignum_value_alloc(local, ret, sign, fixed1 + fixed2);
	}
	else {
		fixnum_alloc(local, ret, v1 - v2);
	}
}

_g void minus_ff_bignum_local(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value1, value2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* 0 - right */
	GetFixnum(left, &value1);
	if (value1 == 0) {
		sigrev_fixnum_bignum_local(local, right, ret);
		return;
	}

	/* left - 0 */
	GetFixnum(right, &value2);
	if (value2 == 0) {
		fixnum_throw_local(local, left, ret);
		return;
	}

	minus_vv_bignum_local(local, value1, value2, ret);
}

static void minus_ff_real_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum value1, value2;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* 0 - right */
	GetFixnum(left, &value1);
	if (value1 == 0) {
		sigrev_fixnum_integer_alloc(local, right, ret);
		return;
	}

	/* left - 0 */
	GetFixnum(right, &value2);
	if (value2 == 0) {
		fixnum_throw_alloc(local, left, ret);
		return;
	}

	minus_vv_real_alloc(local, value1, value2, ret);
}

_g void minus_ff_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	minus_ff_real_alloc(local, left, right, ret);
}

_g void minus_ff_real_common(addr left, addr right, addr *ret)
{
	minus_ff_real_alloc(NULL, left, right, ret);
}

static inline void minusfixnum_local(LocalRoot local,
		addr left, fixnum right, addr *ret)
{
	int sign;
	bigtype value;

	Check(local == NULL, "local error");
	castfixed(right, &sign, &value);
	if (IsPlus(sign)) {
		minusvalue(local, left, value, ret);
	}
	else {
		plusvalue(local, left, value, ret);
	}
}

_g void minus_bf_bignum_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign;
	fixnum check;
	fixed value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* 0 - right */
	GetFixnum(right, &check);
	if (zerop_bignum(left)) {
		castfixed(check, &sign, &value);
		bignum_value_local(local, ret, SignNot(sign), value);
		return;
	}

	/* left - 0 */
	if (check == 0) {
		bignum_throw_local(local, left, ret);
		return;
	}

	minusfixnum_local(local, left, check, ret);
}

_g void minus_bf_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum check;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* 0 - right */
	if (zerop_bignum(left)) {
		sigrev_fixnum_integer_local(local, right, ret);
		return;
	}

	/* left - 0 */
	GetFixnum(right, &check);
	if (check == 0) {
		bignum_throw_local(local, left, ret);
		return;
	}

	minusfixnum_local(local, left, check, &left);
	bignum_result_local(local, left, ret);
}

_g void minus_bf_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum check;
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	/* 0 - right */
	if (zerop_bignum(left)) {
		sigrev_fixnum_integer_common(right, ret);
		return;
	}

	/* left - 0 */
	GetFixnum(right, &check);
	if (check == 0) {
		bignum_throw_heap(left, ret);
		return;
	}

	push_local(local, &stack);
	minusfixnum_local(local, left, check, &left);
	bignum_result_heap(left, ret);
	rollback_local(local, stack);
}

_g void minus_fb_bignum_local(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum check;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	/* 0 - right */
	GetFixnum(left, &check);
	if (check == 0) {
		sigrev_bignum_bignum_local(local, right, ret);
		return;
	}

	/* left - 0 */
	if (zerop_bignum(right)) {
		bignum_fixnum_value_local(local, ret, check);
		return;
	}

	/* fixnum - bignum = -(bignum - fixnum) */
	minusfixnum_local(local, right, check, &right);
	sigrev_bignum_inplace(right);
	*ret = right;
}

_g void minus_fb_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum check;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	/* 0 - right */
	GetFixnum(left, &check);
	if (check == 0) {
		sigrev_bignum_integer_local(local, right, ret);
		return;
	}

	/* left - 0 */
	if (zerop_bignum(right)) {
		fixnum_throw_local(local, left, ret);
		return;
	}

	/* fixnum - bignum = -(bignum - fixnum) */
	minusfixnum_local(local, right, check, &right);
	sigrev_bignum_inplace(right);
	bignum_result_local(local, right, ret);
}

_g void minus_fb_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	fixnum check;
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	/* 0 - right */
	GetFixnum(left, &check);
	if (check == 0) {
		sigrev_bignum_integer_common(right, ret);
		return;
	}

	/* left - 0 */
	if (zerop_bignum(right)) {
		fixnum_throw_heap(left, ret);
		return;
	}

	/* fixnum - bignum = -(bignum - fixnum) */
	push_local(local, &stack);
	minusfixnum_local(local, right, check, &right);
	sigrev_bignum_inplace(right);
	bignum_result_heap(right, ret);
	rollback_local(local, stack);
}

_g void minus_bb_bignum_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, compare;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	if (zerop_bignum(left)) {
		sigrev_bignum_bignum_local(local, right, ret);
		return;
	}
	if (zerop_bignum(right)) {
		bignum_throw_local(local, left, ret);
		return;
	}
	GetSignBignum(left, &sign1);
	GetSignBignum(right, &sign2);
	if (sign1 != sign2) {
		plus_bigdata_alloc(local, left, right, &left);
		SetSignBignum(left, sign1);
		*ret = left;
		return;
	}
	compare = compare_bigdata(left, right);
	if (compare == 0) {
		bignum_zero_local(local, ret);
		return;
	}
	if (compare < 0) {
		minus_bigdata_alloc(local, right, left, &left);
		compare = SignNot(sign2);
	}
	else {
		minus_bigdata_alloc(local, left, right, &left);
		compare = sign1;
	}
	SetSignBignum(left, compare);
	*ret = left;
}

_g void minus_bb_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, compare;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	if (zerop_bignum(left)) {
		sigrev_bignum_integer_local(local, right, ret);
		return;
	}
	if (zerop_bignum(right)) {
		bignum_throw_local(local, left, ret);
		return;
	}
	GetSignBignum(left, &sign1);
	GetSignBignum(right, &sign2);
	if (sign1 != sign2) {
		plus_bigdata_alloc(local, left, right, &left);
		SetSignBignum(left, sign1);
		goto finish;
	}
	compare = compare_bigdata(left, right);
	if (compare == 0) {
		fixnum_local(local, ret, 0);
		return;
	}
	if (compare < 0) {
		minus_bigdata_alloc(local, right, left, &left);
		compare = SignNot(sign2);
	}
	else {
		minus_bigdata_alloc(local, left, right, &left);
		compare = sign1;
	}
	SetSignBignum(left, compare);

finish:
	bignum_result_local(local, left, ret);
}

_g void minus_bb_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2, compare;
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	if (zerop_bignum(left)) {
		sigrev_bignum_integer_common(right, ret);
		return;
	}
	if (zerop_bignum(right)) {
		bignum_throw_heap(left, ret);
		return;
	}
	push_local(local, &stack);
	GetSignBignum(left, &sign1);
	GetSignBignum(right, &sign2);
	if (sign1 != sign2) {
		plus_bigdata_alloc(local, left, right, &left);
		SetSignBignum(left, sign1);
		goto finish;
	}
	compare = compare_bigdata(left, right);
	if (compare == 0) {
		fixnum_heap(ret, 0);
		rollback_local(local, stack);
		return;
	}
	if (compare < 0) {
		minus_bigdata_alloc(local, right, left, &left);
		compare = SignNot(sign2);
	}
	else {
		minus_bigdata_alloc(local, left, right, &left);
		compare = sign1;
	}
	SetSignBignum(left, compare);

finish:
	bignum_result_heap(left, ret);
	rollback_local(local, stack);
}

