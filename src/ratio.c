/*
 *  ratio
 */
#include <math.h>
#include "bigdata.h"
#include "bignum.h"
#include "character.h"
#include "condition.h"
#include "heap.h"
#include "integer.h"
#include "local.h"
#include "memory.h"
#include "number.h"
#include "object.h"
#include "ratio.h"
#include "real_float.h"
#include "stream.h"

int ratiop(addr pos)
{
	return GetType(pos) == LISPTYPE_RATIO;
}

void setnumer_ratio(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RATIO);
	SetNumerRatio_Low(pos, value);
}

void getnumer_ratio(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RATIO);
	GetNumerRatio_Low(pos, ret);
}

void setdenom_ratio(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RATIO);
	SetDenomRatio_Low(pos, value);
}

void getdenom_ratio(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RATIO);
	GetDenomRatio_Low(pos, ret);
}

void setsign_ratio(addr pos, int sign)
{
	CheckType(pos, LISPTYPE_RATIO);
	SetSignRatio_Low(pos, sign);
}

void getsign_ratio(addr pos, int *ret)
{
	CheckType(pos, LISPTYPE_RATIO);
	GetSignRatio_Low(pos, ret);
}

int refsign_ratio(addr pos)
{
	CheckType(pos, LISPTYPE_RATIO);
	return RefSignRatio_Low(pos);
}


/*****************************************************************************
  operation
 *****************************************************************************/
static void reduction_single(addr numer_root, addr denom_root)
{
	bigtype *numer, *denom;
	bigtype a, b, n;

	GetRootDataBignum(numer_root, &numer_root, &numer);
	GetRootDataBignum(denom_root, &denom_root, &denom);

	/* Euclidean Algorithm */
	Check(*numer == 0, "reduction error");
	Check(*denom == 0, "division-by-zero error");
	a = *numer;
	b = *denom;
	if (a == b) {
		*numer = *denom = 1;
		return;
	}
	if (a < b) {
		n = a; a = b; b = n;
	}
	while (b) {
		n = a % b;
		a = b;
		b = n;
	}
	*numer /= a;
	*denom /= a;
}

static void reduction_multiple(LocalRoot local, addr numer, addr denom)
{
	int compare;
	addr a, b, n;
	size_t size;

	/* Euclidean Algorithm */
	Check(local == NULL, "local error");
	Check(zerop_bignum(numer), "reduction error");
	Check(zerop_bignum(denom), "division-by-zero error");
	compare = compare_bigdata(numer, denom);
	if (compare == 0) {
		setvalue_bignum(numer, SignPlus, 1);
		setvalue_bignum(denom, SignPlus, 1);
		return;
	}
	if (compare < 0) {
		bignum_copy_local(local, &a, denom);
		bignum_copy_local(local, &b, numer);
	}
	else {
		bignum_copy_local(local, &a, numer);
		bignum_copy_local(local, &b, denom);
	}

	GetSizeBignum(b, &size);
	bignum_local(local, &n, SignPlus, size);
	while (! zerop_bignum(b)) {
		setrem_noexpand_bigdata(local, n, a, b);
		copy_bignum(local, a, b, 0);
		copy_bignum(local, b, n, 0);
	}
	letdiv_noexpand_bigdata(local, numer, a);
	letdiv_noexpand_bigdata(local, denom, a);
}

void reduction_local(LocalRoot local, addr numer, addr denom)
{
	size_t size1, size2;

	Check(local == NULL, "local error");
	Check(zerop_bignum(denom), "division-by-zero error");
	if (zerop_bignum(numer)) return;
	GetSizeBignum(numer, &size1);
	GetSizeBignum(denom, &size2);
	if (size1 == 1 && size2 == 1)
		reduction_single(numer, denom);
	else
		reduction_multiple(local, numer, denom);
}

static void make_ratio_heap(addr *ret, int sign, addr numer, addr denom)
{
	addr pos;

	Check(sign != SignPlus && sign != SignMinus, "sign error");
	Check(GetType(numer) != LISPTYPE_BIGNUM, "type numer error");
	Check(GetType(denom) != LISPTYPE_BIGNUM, "type denom error");
	Check(GetStatusDynamic(numer), "dynamic numer error");
	Check(GetStatusDynamic(denom), "dynamic denom error");

	heap_array2(&pos, LISPTYPE_RATIO, 2);
	SetSignRatio(pos, sign);
	SetNumerRatio(pos, numer);
	SetDenomRatio(pos, denom);
	*ret = pos;
}

static void make_ratio_local(LocalRoot local,
		addr *ret, int sign, addr numer, addr denom)
{
	addr pos;

	Check(local == NULL, "local error");
	Check(sign != SignPlus && sign != SignMinus, "sign error");
	Check(GetType(numer) != LISPTYPE_BIGNUM, "type numer error");
	Check(GetType(denom) != LISPTYPE_BIGNUM, "type denom error");
	Check(! GetStatusDynamic(numer), "dynamic numer error");
	Check(! GetStatusDynamic(denom), "dynamic denom error");

	local_array2(local, &pos, LISPTYPE_RATIO, 2);
	SetSignRatio(pos, sign);
	SetNumerRatio(pos, numer);
	SetDenomRatio(pos, denom);
	*ret = pos;
}

static void make_ratio_alloc(LocalRoot local,
		addr *ret, int sign, addr numer, addr denom)
{
	if (local)
		make_ratio_local(local, ret, sign, numer, denom );
	else
		make_ratio_heap(ret, sign, numer, denom);
}

void make_ratio_alloc_unsafe(LocalRoot local,
		addr *ret, int sign, addr numer, addr denom)
{
	make_ratio_alloc(local, ret, sign, numer, denom);
}

static void make_copy_ratio_heap(addr *ret, int sign, addr numer, addr denom)
{
	Check(sign != SignPlus && sign != SignMinus, "sign error");
	Check(GetType(numer) != LISPTYPE_BIGNUM, "type numer error");
	Check(GetType(denom) != LISPTYPE_BIGNUM, "type denom error");
	Check(! GetStatusDynamic(numer), "dynamic numer error");
	Check(! GetStatusDynamic(denom), "dynamic denom error");

	bignum_copy_heap(&numer, numer);
	bignum_copy_heap(&denom, denom);
	make_ratio_heap(ret, sign, numer, denom);
}

static void make_copy_ratio_local(LocalRoot local,
		addr *ret, int sign, addr numer, addr denom)
{
	Check(local == NULL, "local error");
	Check(sign != SignPlus && sign != SignMinus, "sign error");
	Check(GetType(numer) != LISPTYPE_BIGNUM, "type numer error");
	Check(GetType(denom) != LISPTYPE_BIGNUM, "type denom error");
	Check(! GetStatusDynamic(numer), "dynamic numer error");
	Check(! GetStatusDynamic(denom), "dynamic denom error");

	bignum_copy_local(local, &numer, numer);
	bignum_copy_local(local, &denom, denom);
	make_ratio_local(local, ret, sign, numer, denom);
}

void ratio_reduction_heap(LocalRoot local, addr *ret, int sign, addr numer, addr denom)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(sign != SignPlus && sign != SignMinus, "sign error");
	Check(GetType(numer) != LISPTYPE_BIGNUM, "type numer error");
	Check(GetType(denom) != LISPTYPE_BIGNUM, "type denom error");
	Check(! GetStatusDynamic(numer), "dynamic numer error");
	Check(! GetStatusDynamic(denom), "dynamic denom error");
	Check(RefSignBignum(numer) != SignPlus, "sign numer error");
	Check(RefSignBignum(denom) != SignPlus, "sign denom error");

	if (zerop_bignum(numer)) {
		fixnum_heap(ret, 0);
		return;
	}

	push_local(local, &stack);
	reduction_local(local, numer, denom);
	if (equal_value_nosign_bignum(denom, 1)) {
		SetSignBignum(numer, sign);
		integer_bignum_heap(ret, numer);
		goto finish;
	}
	make_copy_ratio_heap(ret, sign, numer, denom);

finish:
	rollback_local(local, stack);
}

void ratio_reduction_local(LocalRoot local, addr *ret, int sign, addr numer, addr denom)
{
	Check(local == NULL, "local error");
	Check(sign != SignPlus && sign != SignMinus, "sign error");
	Check(GetType(numer) != LISPTYPE_BIGNUM, "type numer error");
	Check(GetType(denom) != LISPTYPE_BIGNUM, "type denom error");
	Check(! GetStatusDynamic(numer), "dynamic numer error");
	Check(! GetStatusDynamic(denom), "dynamic denom error");
	Check(RefSignBignum(numer) != SignPlus, "sign numer error");
	Check(RefSignBignum(denom) != SignPlus, "sign denom error");

	if (zerop_bignum(numer)) {
		fixnum_local(local, ret, 0);
		return;
	}

	reduction_local(local, numer, denom);
	if (equal_value_nosign_bignum(denom, 1)) {
		SetSignBignum(numer, sign);
		integer_bignum_local(local, ret, numer);
		return;
	}
	make_copy_ratio_local(local, ret, sign, numer, denom);
}

static void ratio_noreduction_heap(addr *ret, int sign, addr numer, addr denom)
{
	Check(sign != SignPlus && sign != SignMinus, "sign error");
	Check(GetType(numer) != LISPTYPE_BIGNUM, "type numer error");
	Check(GetType(denom) != LISPTYPE_BIGNUM, "type denom error");
	Check(! GetStatusDynamic(numer), "dynamic numer error");
	Check(! GetStatusDynamic(denom), "dynamic denom error");
	Check(RefSignBignum(numer) != SignPlus, "sign numer error");
	Check(RefSignBignum(denom) != SignPlus, "sign denom error");

	if (zerop_bignum(numer)) {
		fixnum_heap(ret, 0);
		return;
	}
	if (equal_value_nosign_bignum(denom, 1)) {
		SetSignBignum(numer, sign);
		integer_bignum_heap(ret, numer);
		return;
	}
	make_copy_ratio_heap(ret, sign, numer, denom);
}

static void ratio_noreduction_local(LocalRoot local,
		addr *ret, int sign, addr numer, addr denom)
{
	Check(local == NULL, "local error");
	Check(sign != SignPlus && sign != SignMinus, "sign error");
	Check(GetType(numer) != LISPTYPE_BIGNUM, "type numer error");
	Check(GetType(denom) != LISPTYPE_BIGNUM, "type denom error");
	Check(! GetStatusDynamic(numer), "dynamic numer error");
	Check(! GetStatusDynamic(denom), "dynamic denom error");
	Check(RefSignBignum(numer) != SignPlus, "sign numer error");
	Check(RefSignBignum(denom) != SignPlus, "sign denom error");

	if (zerop_bignum(numer)) {
		fixnum_local(local, ret, 0);
		return;
	}
	if (equal_value_nosign_bignum(denom, 1)) {
		SetSignBignum(numer, sign);
		integer_bignum_local(local, ret, numer);
		return;
	}
	make_copy_ratio_local(local, ret, sign, numer, denom);
}

static void ratio_reduction_nocopy_local(LocalRoot local,
		addr *ret, int sign, addr numer, addr denom)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(sign != SignPlus && sign != SignMinus, "sign error");
	Check(GetType(numer) != LISPTYPE_BIGNUM, "type numer error");
	Check(GetType(denom) != LISPTYPE_BIGNUM, "type denom error");
	Check(! GetStatusDynamic(numer), "dynamic numer error");
	Check(! GetStatusDynamic(denom), "dynamic denom error");
	Check(RefSignBignum(numer) != SignPlus, "sign numer error");
	Check(RefSignBignum(denom) != SignPlus, "sign denom error");

	if (zerop_bignum(numer)) {
		fixnum_local(local, ret, 0);
		return;
	}

	push_local(local, &stack);
	reduction_local(local, numer, denom);
	rollback_local(local, stack);
	if (equal_value_nosign_bignum(denom, 1)) {
		SetSignBignum(numer, sign);
		integer_bignum_local(local, ret, numer);
		return;
	}
	make_ratio_local(local, ret, sign, numer, denom);
}

void make_ratio_reduction_heap(LocalRoot local,
		addr *ret, int sign, addr numer, addr denom)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(sign != SignPlus && sign != SignMinus, "sign error");
	Check(GetType(numer) != LISPTYPE_BIGNUM, "type numer error");
	Check(GetType(denom) != LISPTYPE_BIGNUM, "type denom error");
	Check(! GetStatusDynamic(numer), "dynamic numer error");
	Check(! GetStatusDynamic(denom), "dynamic denom error");
	Check(RefSignBignum(numer) != SignPlus, "sign numer error");
	Check(RefSignBignum(denom) != SignPlus, "sign denom error");

	push_local(local, &stack);
	reduction_local(local, numer, denom);
	make_copy_ratio_heap(ret, sign, numer, denom);
	rollback_local(local, stack);
}

void make_ratio_reduction_local(LocalRoot local,
		addr *ret, int sign, addr numer, addr denom)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(sign != SignPlus && sign != SignMinus, "sign error");
	Check(GetType(numer) != LISPTYPE_BIGNUM, "type numer error");
	Check(GetType(denom) != LISPTYPE_BIGNUM, "type denom error");
	Check(! GetStatusDynamic(numer), "dynamic numer error");
	Check(! GetStatusDynamic(denom), "dynamic denom error");
	Check(RefSignBignum(numer) != SignPlus, "sign numer error");
	Check(RefSignBignum(denom) != SignPlus, "sign denom error");

	push_local(local, &stack);
	reduction_local(local, numer, denom);
	rollback_local(local, stack);
	make_ratio_local(local, ret, sign, numer, denom);
}

void ratio_reduction_value_local(LocalRoot local, addr *ret,
		int sign, bigtype numer, bigtype denom)
{
	addr num, den;

	Check(local == NULL, "local error");
	bignum_value_local(local, &num, SignPlus, numer);
	bignum_value_local(local, &den, SignPlus, denom);
	make_ratio_reduction_local(local, ret, sign, num, den);
}

void ratio_reduction_value_heap(LocalRoot local, addr *ret,
		int sign, bigtype numer, bigtype denom)
{
	addr num, den;
	LocalStack stack;

	Check(local == NULL, "local error");
	push_local(local, &stack);
	bignum_value_local(local, &num, SignPlus, numer);
	bignum_value_local(local, &den, SignPlus, denom);
	make_ratio_reduction_heap(local, ret, sign, num, den);
	rollback_local(local, stack);
}

void ratio_noreduction_value_local(LocalRoot local, addr *ret,
		int sign, bigtype numer, bigtype denom)
{
	addr num, den;

	Check(local == NULL, "local error");
	bignum_value_local(local, &num, SignPlus, numer);
	bignum_value_local(local, &den, SignPlus, denom);
	make_ratio_local(local, ret, sign, num, den);
}

void ratio_noreduction_value_heap(addr *ret,
		int sign, bigtype numer, bigtype denom)
{
	addr num, den;

	bignum_value_heap(&num, SignPlus, numer);
	bignum_value_heap(&den, SignPlus, denom);
	make_ratio_heap(ret, sign, num, den);
}

void ratio_zero_alloc(LocalRoot local, addr *ret)
{
	addr numer, denom;

	bignum_zero_alloc(local, &numer);
	bignum_value_alloc(local, &denom, SignPlus, 1);
	make_ratio_alloc(local, ret, SignPlus, numer, denom);
}

void ratio_zero_local(LocalRoot local, addr *ret)
{
	Check(local == NULL, "local error");
	ratio_zero_alloc(local, ret);
}

void ratio_zero_heap(addr *ret)
{
	ratio_zero_alloc(NULL, ret);
}

void ratio_copy_nosign_alloc(LocalRoot local, addr *ret, addr pos)
{
	addr numer, denom;

	Check(GetType(pos) != LISPTYPE_RATIO, "type error");
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	bignum_copy_alloc(local, &numer, numer);
	bignum_copy_alloc(local, &denom, denom);
	make_ratio_alloc(local, ret, SignPlus, numer, denom);
}

void ratio_copy_nosign_local(LocalRoot local, addr *ret, addr pos)
{
	Check(local == NULL, "local error");
	ratio_copy_nosign_alloc(local, ret, pos);
}

void ratio_copy_nosign_heap(addr *ret, addr pos)
{
	ratio_copy_nosign_alloc(NULL, ret, pos);
}

void ratio_copy_alloc(LocalRoot local, addr *ret, addr pos)
{
	int sign;
	addr numer, denom;

	Check(GetType(pos) != LISPTYPE_RATIO, "type error");
	GetSignRatio(pos, &sign);
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	bignum_copy_alloc(local, &numer, numer);
	bignum_copy_alloc(local, &denom, denom);
	make_ratio_alloc(local, ret, sign, numer, denom);
}

void ratio_copy_local(LocalRoot local, addr *ret, addr pos)
{
	Check(local == NULL, "local error");
	ratio_copy_alloc(local, ret, pos);
}

void ratio_copy_heap(addr *ret, addr pos)
{
	ratio_copy_alloc(NULL, ret, pos);
}

void ratio_throw_heap(addr pos, addr *ret)
{
	Check(GetType(pos) != LISPTYPE_RATIO, "type error");
	if (GetStatusDynamic(pos))
		ratio_copy_heap(ret, pos);
	else
		*ret = pos;
}

void ratio_throw_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	Check(GetType(pos) != LISPTYPE_RATIO, "type error");
	if (GetStatusDynamic(pos))
		*ret = pos;
	else
		ratio_copy_local(local, ret, pos);
}

void ratio_throw_alloc(LocalRoot local, addr pos, addr *ret)
{
	Check(GetType(pos) != LISPTYPE_RATIO, "type error");
	if (local)
		ratio_throw_local(local, pos, ret);
	else
		ratio_throw_heap(pos, ret);
}

int ratio_result_noreduction_local(LocalRoot local, addr pos, addr *ret)
{
	int sign;
	addr numer, denom;

	Check(local == NULL, "local error");
	CheckType(pos, LISPTYPE_RATIO);
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	if (zerop_bignum(numer)) {
		fixnum_local(local, ret, 0);
		return 1;
	}
	else if (equal_value_nosign_bignum(denom, 1)) {
		bignum_copy_local(local, &numer, numer);
		GetSignRatio(pos, &sign);
		SetSignBignum(numer, sign);
		bignum_result_local(local, numer, ret);
		return 1;
	}
	else {
		ratio_throw_local(local, pos, ret);
		return 0;
	}
}

int ratio_result_noreduction_heap(LocalRoot local, addr pos, addr *ret)
{
	int sign;
	addr numer, denom;
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(pos, LISPTYPE_RATIO);
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	if (zerop_bignum(numer)) {
		fixnum_heap(ret, 0);
		return 1;
	}
	else if (equal_value_nosign_bignum(denom, 1)) {
		push_local(local, &stack);
		bignum_copy_local(local, &numer, numer);
		GetSignRatio(pos, &sign);
		SetSignBignum(numer, sign);
		bignum_result_heap(numer, ret);
		rollback_local(local, stack);
		return 1;
	}
	else {
		ratio_throw_heap(pos, ret);
		return 0;
	}
}

int zerop_ratio(addr left)
{
	Check(GetType(left) != LISPTYPE_RATIO, "type error");
	GetNumerRatio(left, &left);
	return zerop_bignum(left);
}

int plusp_ratio(addr left)
{
	Check(GetType(left) != LISPTYPE_RATIO, "type error");
	if (IsMinus(RefSignRatio(left))) return 0;
	return ! zerop_ratio(left);
}

int minusp_ratio(addr left)
{
	Check(GetType(left) != LISPTYPE_RATIO, "type error");
	if (IsPlus(RefSignRatio(left))) return 0;
	return ! zerop_ratio(left);
}

int equal_value_nosign_ratio(addr pos, bigtype numer, bigtype denom)
{
	addr pos1, pos2;

	CheckType(pos, LISPTYPE_RATIO);
	GetNumerRatio(pos, &pos1);
	GetDenomRatio(pos, &pos2);
	return equal_value_nosign_bignum(pos1, numer)
		&& equal_value_nosign_bignum(pos2, denom);
}

int equal_value_ratio(addr pos, int sign, bigtype numer, bigtype denom)
{
	CheckType(pos, LISPTYPE_RATIO);
	return (RefSignRatio(pos) == sign)
		&& equal_value_nosign_ratio(pos, numer, denom);
}

int equal_fr_real(addr left, addr right)
{
	int sign;
	addr pos;
	bigtype value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	/* denom == 1 */
	GetDenomRatio(right, &pos);
	if (! equal_value_nosign_bignum(pos, 1)) return 0;
	/* numer */
	GetNumerRatio(right, &pos);
	castfixed_fixnum(left, &sign, &value);
	if (! equal_value_nosign_bignum(pos, value)) return 0;
	/* sign */
	return RefSignRatio(right) == sign;
}

int equal_br_real(addr left, addr right)
{
	addr pos;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	/* denom == 1 */
	GetDenomRatio(right, &pos);
	if (! equal_value_nosign_bignum(pos, 1)) return 0;
	/* numer */
	GetNumerRatio(right, &pos);
	if (! equal_bigdata(left, pos)) return 0;
	/* sign */
	return RefSignBignum(left) == RefSignRatio(right);
}

int equal_rr_real(addr left, addr right)
{
	addr pos1, pos2;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	/* sign */
	if (RefSignRatio(left) != RefSignRatio(right)) return 0;
	/* numer */
	GetNumerRatio(left, &pos1);
	GetNumerRatio(right, &pos2);
	if (! equal_bb_real(pos1, pos2)) return 0;
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

static void rational_result_local(LocalRoot local,
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

static void rational_float_single_local(LocalRoot local, single_float value, addr *ret)
{
	int sign, exponent;
	addr numer;

	split_single_float(value, &sign, &exponent, &value);
	if (bignum_single_float_local(local, &numer, value))
		fmte("Invalid floating value ~S.", value, NULL);
	rational_result_local(local, sign, exponent, numer, ret);
}

static void rational_float_double_local(LocalRoot local, double_float value, addr *ret)
{
	int sign, exponent;
	addr numer;

	split_double_float(value, &sign, &exponent, &value);
	if (bignum_double_float_local(local, &numer, value))
		fmte("Invalid floating value ~S.", value, NULL);
	rational_result_local(local, sign, exponent, numer, ret);
}

static void rational_float_long_local(LocalRoot local, long_float value, addr *ret)
{
	int sign, exponent;
	addr numer;

	split_long_float(value, &sign, &exponent, &value);
	if (bignum_long_float_local(local, &numer, value))
		fmte("Invaild floating value ~S.", value, NULL);
	rational_result_local(local, sign, exponent, numer, ret);
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

int equal_rs_real(LocalRoot local, addr left, addr right)
{
	int check;
	LocalStack stack;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	push_local(local, &stack);
	rational_float_single_local(local, RefSingleFloat(right), &right);
	check = equal_ratio_type(left, right);
	rollback_local(local, stack);

	return check;
}

int equal_rd_real(LocalRoot local, addr left, addr right)
{
	int check;
	LocalStack stack;

	push_local(local, &stack);
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	rational_float_double_local(local, RefDoubleFloat(right), &right);
	check = equal_ratio_type(left, right);
	rollback_local(local, stack);

	return check;
}

int equal_rl_real(LocalRoot local, addr left, addr right)
{
	int check;
	LocalStack stack;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	push_local(local, &stack);
	rational_float_long_local(local, RefLongFloat(right), &right);
	check = equal_ratio_type(left, right);
	rollback_local(local, stack);

	return check;
}

static int compare_bigtype_bignum(bigtype left, addr right)
{
	bigtype value;

	if (1 < RefSizeBignum(right)) return -1;
	GetRootBignum(right, &right);
	value = PtrDataBignum(right)[0];
	if (left < value) return -1;
	if (left > value) return 1;

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

int compare_fr_real(LocalRoot local, addr left, addr right)
{
	int sign1, sign2, result;
	bigtype value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	castfixed_fixnum(left, &sign1, &value);
	result = zerop_ratio(right);
	if (value == 0) {
		if (result) return 0;
		GetSignRatio(right, &sign2);
		return IsPlus(sign2)? -1: 1;
	}
	if (result) {
		return IsPlus(sign1)? 1: -1;
	}
	GetSignRatio(right, &sign2);
	if (IsPlus(sign1) && IsMinus(sign2)) return 1;
	if (IsMinus(sign1) && IsPlus(sign2)) return -1;
	result = compare_bigtype_ratio_nosign(local, value, right);

	return IsPlus(sign1)? result: -result;
}

int compare_rf_real(LocalRoot local, addr left, addr right)
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

int compare_br_real(LocalRoot local, addr left, addr right)
{
	int sign1, sign2, result, check;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	check = zerop_ratio(right);
	if (zerop_bignum(left)) {
		if (check) return 0;
		GetSignRatio(right, &sign2);
		return IsPlus(sign2)? -1: 1;
	}
	if (check) {
		GetSignBignum(left, &sign1);
		return IsPlus(sign1)? 1: -1;
	}
	GetSignBignum(left, &sign1);
	GetSignRatio(right, &sign2);
	if (IsPlus(sign1) && IsMinus(sign2)) return 1;
	if (IsMinus(sign1) && IsPlus(sign2)) return -1;
	result = compare_bigdata_ratio_nosign(local, left, right);

	return IsPlus(sign1)? result: -result;
}

int compare_rb_real(LocalRoot local, addr left, addr right)
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

int compare_rr_real(LocalRoot local, addr left, addr right)
{
	int check1, check2, sign1, sign2;
	addr denom1, denom2;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	/* zero check */
	check1 = zerop_ratio(left);
	check2 = zerop_ratio(right);
	if (check1 && check2) return 0;
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
	if (IsPlus(sign1) && IsMinus(sign2)) return 1;
	if (IsMinus(sign1) && IsPlus(sign2)) return -1;

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

int compare_rs_real(LocalRoot local, addr left, addr right)
{
	int check;
	LocalStack stack;

	push_local(local, &stack);
	rational_float_single_local(local, RefSingleFloat(right), &right);
	check = compare_ratio_real(local, left, right);
	rollback_local(local, stack);

	return check;
}

int compare_rd_real(LocalRoot local, addr left, addr right)
{
	int check;
	LocalStack stack;

	push_local(local, &stack);
	rational_float_double_local(local, RefDoubleFloat(right), &right);
	check = compare_ratio_real(local, left, right);
	rollback_local(local, stack);

	return check;
}

int compare_rl_real(LocalRoot local, addr left, addr right)
{
	int check;
	LocalStack stack;

	push_local(local, &stack);
	rational_float_long_local(local, RefLongFloat(right), &right);
	check = compare_ratio_real(local, left, right);
	rollback_local(local, stack);

	return check;
}

int compare_sr_real(LocalRoot local, addr left, addr right)
{
	return -compare_rs_real(local, right, left);
}

int compare_dr_real(LocalRoot local, addr left, addr right)
{
	return -compare_rd_real(local, right, left);
}

int compare_lr_real(LocalRoot local, addr left, addr right)
{
	return -compare_rl_real(local, right, left);
}


/*
 *  cast float
 */
static size_t hexfloat_bigtype_exponent(bigtype value)
{
	size_t i;

	for (i = 0; value; i += 4UL) {
		value >>= 4UL;
	}

	return i;
}

static size_t hexfloat_exponent(addr pos)
{
	size_t size;
	bigtype *data;

	GetSizeBignum(pos, &size);
	GetDataBignum(pos, &data);
	Check(size == 0, "size error");
	if (size == 1) {
		size =  hexfloat_bigtype_exponent(data[0]);
	}
	else {
		size--;
		size = size * BIGNUM_FULLBIT + hexfloat_bigtype_exponent(data[size]);
	}
	Check(size < 4UL, "size error");

	return size - 4UL;
}

#define OVERFLOW_EXPONENT		20000
static void diff_exponent_ratio(size_t *size1, size_t *size2, addr pos)
{
	if (*size1 < *size2) {
		*size2 -= *size1;
		*size1 = 0;
	}
	else {
		*size1 -= *size2;
		*size2 = 0;
	}
	if (OVERFLOW_EXPONENT < *size1) {
		floating_point_overflow_stdarg(CONSTANT_COMMON_COERCE, pos, NULL);
	}
	if (OVERFLOW_EXPONENT < *size2) {
		floating_point_underflow_stdarg(CONSTANT_COMMON_COERCE, pos, NULL);
	}
}

#define HexToChar(x) (((x) < 10)? ('0' + (x)): ('A' - 10 + (x)))
static char *hexadecimal_bigtype(char *ptr, bigtype v, int first, size_t *bit)
{
	unsigned i, n, index, size;

	size = BIGNUM_FULLBIT / 4UL;
	for (i = 0; i < size; i++) {
		index = size - i - 1;
		n = (v >> (index * 4UL)) & 0x0F;
		if (first == 0 || n) {
			*(ptr++) = HexToChar(n);
			if (first) {
				*(ptr++) = '.';
				first = 0;
			}
			if (*bit <= 4UL) {
				*bit = 0;
				break;
			}
			*bit -= 4UL;
		}
	}
	Check(first, "first error");

	return ptr;
}

static char *hexadecimal_ratio(char *ptr, addr pos, size_t bit)
{
	size_t size, i, index;
	bigtype *data;

	GetSizeBignum(pos, &size);
	GetDataBignum(pos, &data);
	Check(size == 0, "size error");
	for (i = 0; i < size; i++) {
		index = size - i - 1;
		ptr = hexadecimal_bigtype(ptr, data[index], (i == 0), &bit);
		if (bit == 0) break;
	}

	return ptr;
}

static void float_string_ratio(int sign, addr pos, char *str, size_t exp, size_t bit)
{
	*(str++) = IsPlus(sign)? '+': '-';
	*(str++) = '0';
	*(str++) = 'x';
	str = hexadecimal_ratio(str, pos, bit);
	*(str++) = 'p';
	snprintf(str, 8, "%d", (int)exp); /* less than OVERFLOW_EXPONENT */
}

single_float single_float_ratio(addr pos)
{
	char str1[64], str2[64];
	int sign;
	addr numer, denom;
	size_t size1, size2;
	single_float v1, v2;

	Check(GetType(pos) != LISPTYPE_RATIO, "type error");
	GetSignRatio(pos, &sign);
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	if (zerop_bignum(denom))
		division_by_zero_real1(CONSTANT_COMMON_COERCE, pos);
	if (zerop_bignum(numer))
		return sign? -0.0f: +0.0f;
	size1 = hexfloat_exponent(numer);
	size2 = hexfloat_exponent(denom);
	diff_exponent_ratio(&size1, &size2, pos);
	float_string_ratio(sign, numer, str1, size1, LISP_FLOAT_SINGLE_FRACTION);
	float_string_ratio(SignPlus, denom, str2, size2, LISP_FLOAT_SINGLE_FRACTION);
	v1 = check_strtof(str1, pos);
	v2 = check_strtof_reverse(str2, pos);
	v1 /= v2;
	float_errorcheck1(CONSTANT_COMMON_COERCE, v1, pos);

	return v1;
}

double_float double_float_ratio(addr pos)
{
	char str1[64], str2[64];
	int sign;
	addr numer, denom;
	size_t size1, size2;
	double_float v1, v2;

	Check(GetType(pos) != LISPTYPE_RATIO, "type error");
	GetSignRatio(pos, &sign);
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	if (zerop_bignum(denom))
		division_by_zero_real1(CONSTANT_COMMON_COERCE, pos);
	if (zerop_bignum(numer))
		return sign? -0.0: +0.0;
	size1 = hexfloat_exponent(numer);
	size2 = hexfloat_exponent(denom);
	diff_exponent_ratio(&size1, &size2, pos);
	float_string_ratio(sign, numer, str1, size1, LISP_FLOAT_DOUBLE_FRACTION);
	float_string_ratio(SignPlus, denom, str2, size2, LISP_FLOAT_DOUBLE_FRACTION);
	v1 = check_strtod(str1, pos);
	v2 = check_strtod_reverse(str2, pos);
	v1 /= v2;
	float_errorcheck1(CONSTANT_COMMON_COERCE, v1, pos);

	return v1;
}

long_float long_float_ratio(addr pos)
{
	char str1[64], str2[64];
	int sign;
	addr numer, denom;
	size_t size1, size2;
	long_float v1, v2;

	Check(GetType(pos) != LISPTYPE_RATIO, "type error");
	GetSignRatio(pos, &sign);
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	if (zerop_bignum(denom))
		division_by_zero_real1(CONSTANT_COMMON_COERCE, pos);
	if (zerop_bignum(numer))
		return sign? -0.0L: +0.0L;
	size1 = hexfloat_exponent(numer);
	size2 = hexfloat_exponent(denom);
	diff_exponent_ratio(&size1, &size2, pos);
	float_string_ratio(sign, numer, str1, size1, LISP_FLOAT_LONG_FRACTION);
	float_string_ratio(SignPlus, denom, str2, size2, LISP_FLOAT_LONG_FRACTION);
	v1 = check_strtold(str1, pos);
	v2 = check_strtold_reverse(str2, pos);
	v1 /= v2;
	float_errorcheck1(CONSTANT_COMMON_COERCE, v1, pos);

	return v1;
}


/*****************************************************************************
  calculation
 *****************************************************************************/
/* sign-reverse */
void sign_reverse_ratio_inplace(addr pos)
{
	int sign;

	CheckType(pos, LISPTYPE_RATIO);
	GetSignRatio(pos, &sign);
	sign = SignNot(sign);
	SetSignRatio(pos, sign);
}

void sign_reverse_ratio_local(LocalRoot local, addr pos, addr *ret)
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

void sign_reverse_ratio_common(addr pos, addr *ret)
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

void plus_rv_ratio_local(LocalRoot local, addr left, fixnum right, addr *ret)
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

void plus_rv_real_local(LocalRoot local, addr left, fixnum right, addr *ret)
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

void plus_rv_real_common(LocalRoot local, addr left, fixnum right, addr *ret)
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

void plus_rf_ratio_local(LocalRoot local, addr left, addr right, addr *ret)
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

void plus_rf_real_local(LocalRoot local, addr left, addr right, addr *ret)
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

void plus_rf_real_common(LocalRoot local, addr left, addr right, addr *ret)
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

static void cast_fixnum_ratio_local(LocalRoot local, addr pos, addr *ret)
{
	int sign;
	addr numer, denom;
	bigtype value;

	Check(local == NULL, "local error");
	CheckType(pos, LISPTYPE_FIXNUM);
	castfixed_fixnum(pos, &sign, &value);
	bignum_value_local(local, &numer, SignPlus, value);
	bignum_value_local(local, &denom, SignPlus, 1);
	make_ratio_local(local, ret, sign, numer, denom);
}

static void cast_bignum_ratio_local(LocalRoot local, addr pos, addr *ret)
{
	int sign;
	addr numer, denom;

	Check(local == NULL, "local error");
	CheckType(pos, LISPTYPE_BIGNUM);
	GetSignBignum(pos, &sign);
	bignum_copy_nosign_local(local, &numer, pos);
	bignum_value_local(local, &denom, SignPlus, 1);
	make_ratio_local(local, ret, sign, numer, denom);
}

static void sigrev_fixnum_ratio_local(LocalRoot local, addr pos, addr *ret)
{
	cast_fixnum_ratio_local(local, pos, ret);
	sign_reverse_ratio_inplace(*ret);
}

static void sigrev_bignum_ratio_local(LocalRoot local, addr pos, addr *ret)
{
	cast_bignum_ratio_local(local, pos, ret);
	sign_reverse_ratio_inplace(*ret);
}

void minus_rf_ratio_local(LocalRoot local, addr left, addr right, addr *ret)
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

void minus_rf_real_local(LocalRoot local, addr left, addr right, addr *ret)
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

void minus_rf_real_common(LocalRoot local, addr left, addr right, addr *ret)
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

void minus_fr_ratio_local(LocalRoot local, addr left, addr right, addr *ret)
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

void minus_fr_real_local(LocalRoot local, addr left, addr right, addr *ret)
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

void minus_fr_real_common(LocalRoot local, addr left, addr right, addr *ret)
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

void plus_rb_ratio_local(LocalRoot local, addr left, addr right, addr *ret)
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

void plus_rb_real_local(LocalRoot local, addr left, addr right, addr *ret)
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

void plus_rb_real_common(LocalRoot local, addr left, addr right, addr *ret)
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

void minus_rb_ratio_local(LocalRoot local, addr left, addr right, addr *ret)
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

void minus_rb_real_local(LocalRoot local, addr left, addr right, addr *ret)
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

void minus_rb_real_common(LocalRoot local, addr left, addr right, addr *ret)
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

void minus_br_ratio_local(LocalRoot local, addr left, addr right, addr *ret)
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

void minus_br_real_local(LocalRoot local, addr left, addr right, addr *ret)
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

void minus_br_real_common(LocalRoot local, addr left, addr right, addr *ret)
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

void plus_rr_ratio_local(LocalRoot local, addr left, addr right, addr *ret)
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

void plus_rr_real_local(LocalRoot local, addr left, addr right, addr *ret)
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

void plus_rr_real_common(LocalRoot local, addr left, addr right, addr *ret)
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

void minus_rr_ratio_local(LocalRoot local, addr left, addr right, addr *ret)
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

void minus_rr_real_local(LocalRoot local, addr left, addr right, addr *ret)
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

void minus_rr_real_common(LocalRoot local, addr left, addr right, addr *ret)
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


/*
 *  multiple
 */
static int equal_rv_nosign(addr left, bigtype value)
{
	addr check;

	Check(GetType(left) != LISPTYPE_RATIO, "type error");
	GetDenomRatio(left, &check);
	if (! equal_value_nosign_bignum(check, 1)) return 0;
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

void multi_rf_ratio_local(LocalRoot local, addr left, addr right, addr *ret)
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

void multi_rf_real_local(LocalRoot local, addr left, addr right, addr *ret)
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

void multi_rf_real_common(LocalRoot local, addr left, addr right, addr *ret)
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

void multi_rb_ratio_local(LocalRoot local, addr left, addr right, addr *ret)
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

void multi_rb_real_local(LocalRoot local, addr left, addr right, addr *ret)
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

void multi_rb_real_common(LocalRoot local, addr left, addr right, addr *ret)
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

void multi_rr_ratio_local(LocalRoot local, addr left, addr right, addr *ret)
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

void multi_rr_real_local(LocalRoot local, addr left, addr right, addr *ret)
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

void multi_rr_real_common(LocalRoot local, addr left, addr right, addr *ret)
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

void div_rf_ratio_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	bigtype value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	GetSignRatio(left, &sign1);
	castfixed_fixnum(right, &sign2, &value);
	sign1 = SignMulti(sign1, sign2);
	if (value == 0)
		division_by_zero2(left, right);

	/* ratio */
	if (zerop_ratio(left)) {
		ratio_zero_local(local, ret);
		return;
	}
	if (equal_rv_nosign(left, 1)) {
		inverse_value_ratio(local, ret, sign1, value);
		return;
	}

	/* fixnum */
	if (value == 1) {
		if (IsPlus(sign2))
			ratio_throw_local(local, left, ret);
		else
			sign_reverse_ratio_local(local, left, ret);
		return;
	}

	/* division */
	div_rv_ratio(local, sign1, left, value, ret);
}

void div_rf_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	bigtype value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	GetSignRatio(left, &sign1);
	castfixed_fixnum(right, &sign2, &value);
	sign1 = SignMulti(sign1, sign2);
	if (value == 0)
		division_by_zero2(left, right);

	/* ratio */
	if (zerop_ratio(left)) {
		fixnum_local(local, ret, 0);
		return;
	}
	if (equal_rv_nosign(left, 1)) {
		inverse_value_local(local, ret, sign1, value);
		return;
	}

	/* fixnum */
	if (value == 1) {
		if (IsPlus(sign2))
			ratio_throw_local(local, left, ret);
		else
			sign_reverse_ratio_local(local, left, ret);
		return;
	}

	/* division */
	div_rv_local(local, sign1, left, value, ret);
}

void div_rf_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	bigtype value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	GetSignRatio(left, &sign1);
	castfixed_fixnum(right, &sign2, &value);
	sign1 = SignMulti(sign1, sign2);
	if (value == 0)
		division_by_zero2(left, right);

	/* ratio */
	if (zerop_ratio(left)) {
		fixnum_heap(ret, 0);
		return;
	}
	if (equal_rv_nosign(left, 1)) {
		inverse_value_common(ret, sign1, value);
		return;
	}

	/* fixnum */
	if (value == 1) {
		if (IsPlus(sign2))
			ratio_throw_heap(left, ret);
		else
			sign_reverse_ratio_common(left, ret);
		return;
	}

	/* division */
	div_rv_common(local, sign1, left, value, ret);
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

void div_fr_ratio_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	bigtype value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	castfixed_fixnum(left, &sign1, &value);
	GetSignRatio(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_ratio(right))
		division_by_zero2(left, right);

	/* fixnum */
	if (value == 0) {
		ratio_zero_local(local, ret);
		return;
	}
	if (value == 1) {
		ratio_sign_inverse_ratio(local, ret, sign1, right);
		return;
	}

	/* ratio */
	if (equal_rv_nosign(right, 1)) {
		if (IsPlus(sign2))
			cast_fixnum_ratio_local(local, left, ret);
		else
			sigrev_fixnum_ratio_local(local, left, ret);
		return;
	}

	/* division */
	div_vr_ratio(local, sign1, value, right, ret);
}

void div_fr_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	bigtype value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	castfixed_fixnum(left, &sign1, &value);
	GetSignRatio(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_ratio(right))
		division_by_zero2(left, right);

	/* fixnum */
	if (value == 0) {
		fixnum_local(local, ret, 0);
		return;
	}
	if (value == 1) {
		ratio_sign_inverse_local(local, ret, sign1, right);
		return;
	}

	/* ratio */
	if (equal_rv_nosign(right, 1)) {
		if (IsPlus(sign2))
			fixnum_throw_local(local, left, ret);
		else
			sigrev_fixnum_integer_local(local, left, ret);
		return;
	}

	/* division */
	div_vr_local(local, sign1, value, right, ret);
}

void div_fr_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	bigtype value;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	castfixed_fixnum(left, &sign1, &value);
	GetSignRatio(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_ratio(right))
		division_by_zero2(left, right);

	/* fixnum */
	if (value == 0) {
		fixnum_heap(ret, 0);
		return;
	}
	if (value == 1) {
		ratio_sign_inverse_common(local, ret, sign1, right);
		return;
	}

	/* ratio */
	if (equal_rv_nosign(right, 1)) {
		if (IsPlus(sign2))
			fixnum_throw_heap(left, ret);
		else
			sigrev_fixnum_integer_common(left, ret);
		return;
	}

	/* division */
	div_vr_common(local, sign1, value, right, ret);
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

void div_rb_ratio_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	GetSignRatio(left, &sign1);
	GetSignBignum(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_bignum(right))
		division_by_zero2(left, right);

	/* ratio */
	if (zerop_ratio(left)) {
		ratio_zero_local(local, ret);
		return;
	}
	if (equal_rv_nosign(left, 1)) {
		bignum_sign_inverse_ratio(local, ret, sign1, right);
		return;
	}

	/* bignum */
	if (equal_value_nosign_bignum(right, 1)) {
		if (IsPlus(sign2))
			ratio_throw_local(local, left, ret);
		else
			sign_reverse_ratio_local(local, left, ret);
		return;
	}

	/* division */
	div_rb_ratio(local, sign1, left, right, ret);
}

void div_rb_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	GetSignRatio(left, &sign1);
	GetSignBignum(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_bignum(right))
		division_by_zero2(left, right);

	/* ratio */
	if (zerop_ratio(left)) {
		fixnum_local(local, ret, 0);
		return;
	}
	if (equal_rv_nosign(left, 1)) {
		bignum_sign_inverse_local(local, ret, sign1, right);
		return;
	}

	/* bignum */
	if (equal_value_nosign_bignum(right, 1)) {
		if (IsPlus(sign2))
			ratio_throw_local(local, left, ret);
		else
			sign_reverse_ratio_local(local, left, ret);
		return;
	}

	/* division */
	div_rb_local(local, sign1, left, right, ret);
}

void div_rb_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	GetSignRatio(left, &sign1);
	GetSignBignum(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_bignum(right))
		division_by_zero2(left, right);

	/* ratio */
	if (zerop_ratio(left)) {
		fixnum_heap(ret, 0);
		return;
	}
	if (equal_rv_nosign(left, 1)) {
		bignum_sign_inverse_common(ret, sign1, right);
		return;
	}

	/* bignum */
	if (equal_value_nosign_bignum(right, 1)) {
		if (IsPlus(sign2))
			ratio_throw_heap(left, ret);
		else
			sign_reverse_ratio_common(left, ret);
		return;
	}

	/* division */
	div_rb_common(local, sign1, left, right, ret);
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

void div_br_ratio_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	GetSignBignum(left, &sign1);
	GetSignRatio(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_ratio(right))
		division_by_zero2(left, right);

	/* bignum */
	if (zerop_bignum(left)) {
		ratio_zero_local(local, ret);
		return;
	}
	if (equal_value_nosign_bignum(left, 1)) {
		ratio_sign_inverse_ratio(local, ret, sign1, right);
		return;
	}

	/* ratio */
	if (equal_rv_nosign(right, 1)) {
		if (IsPlus(sign2))
			cast_bignum_ratio_local(local, left, ret);
		else
			sigrev_bignum_ratio_local(local, left, ret);
		return;
	}

	/* division */
	div_br_ratio(local, sign1, left, right, ret);
}

void div_br_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	GetSignBignum(left, &sign1);
	GetSignRatio(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_ratio(right))
		division_by_zero2(left, right);

	/* bignum */
	if (zerop_bignum(left)) {
		fixnum_local(local, ret, 0);
		return;
	}
	if (equal_value_nosign_bignum(left, 1)) {
		ratio_sign_inverse_local(local, ret, sign1, right);
		return;
	}

	/* ratio */
	if (equal_rv_nosign(right, 1)) {
		if (IsPlus(sign2))
			bignum_throw_local(local, left, ret);
		else
			sigrev_bignum_integer_local(local, left, ret);
		return;
	}

	/* division */
	div_br_local(local, sign1, left, right, ret);
}

void div_br_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	GetSignBignum(left, &sign1);
	GetSignRatio(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_ratio(right))
		division_by_zero2(left, right);

	/* bignum */
	if (zerop_bignum(left)) {
		fixnum_heap(ret, 0);
		return;
	}
	if (equal_value_nosign_bignum(left, 1)) {
		ratio_sign_inverse_common(local, ret, sign1, right);
		return;
	}

	/* ratio */
	if (equal_rv_nosign(right, 1)) {
		if (IsPlus(sign2))
			bignum_throw_heap(left, ret);
		else
			sigrev_bignum_integer_common(left, ret);
		return;
	}

	/* division */
	div_br_common(local, sign1, left, right, ret);
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

void div_rr_ratio_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	GetSignRatio(left, &sign1);
	GetSignRatio(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_ratio(right))
		division_by_zero2(left, right);

	/* ratio */
	if (zerop_ratio(left)) {
		ratio_zero_local(local, ret);
		return;
	}
	if (equal_rv_nosign(left, 1)) {
		ratio_sign_inverse_ratio(local, ret, sign1, right);
		return;
	}
	if (inverse_ratio_p(left)) {
		GetDenomRatio(left, &left);
		div_bir_ratio(local, sign1, left, right, ret);
		return;
	}

	/* ratio */
	if (equal_rv_nosign(right, 1)) {
		if (IsPlus(sign2))
			ratio_throw_local(local, left, ret);
		else
			sign_reverse_ratio_local(local, left, ret);
		return;
	}
	if (inverse_ratio_p(right)) {
		GetDenomRatio(right, &right);
		div_rb_ratio(local, sign1, left, right, ret);
		return;
	}

	/* division */
	div_rr_ratio(local, sign1, left, right, ret);
}

void div_rr_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	GetSignRatio(left, &sign1);
	GetSignRatio(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_ratio(right))
		division_by_zero2(left, right);

	/* ratio */
	if (zerop_ratio(left)) {
		fixnum_local(local, ret, 0);
		return;
	}
	if (equal_rv_nosign(left, 1)) {
		ratio_sign_inverse_local(local, ret, sign1, right);
		return;
	}
	if (inverse_ratio_p(left)) {
		GetDenomRatio(left, &left);
		div_bir_local(local, sign1, left, right, ret);
		return;
	}

	/* ratio */
	if (equal_rv_nosign(right, 1)) {
		if (IsPlus(sign2))
			ratio_throw_local(local, left, ret);
		else
			sign_reverse_ratio_local(local, left, ret);
		return;
	}
	if (inverse_ratio_p(right)) {
		GetDenomRatio(right, &right);
		div_rb_local(local, sign1, left, right, ret);
		return;
	}

	/* division */
	div_rr_local(local, sign1, left, right, ret);
}

void div_rr_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");

	GetSignRatio(left, &sign1);
	GetSignRatio(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_ratio(right))
		division_by_zero2(left, right);

	/* ratio */
	if (zerop_ratio(left)) {
		fixnum_heap(ret, 0);
		return;
	}
	if (equal_rv_nosign(left, 1)) {
		ratio_sign_inverse_common(local, ret, sign1, right);
		return;
	}
	if (inverse_ratio_p(left)) {
		GetDenomRatio(left, &left);
		div_bir_common(local, sign1, left, right, ret);
		return;
	}

	/* ratio */
	if (equal_rv_nosign(right, 1)) {
		if (IsPlus(sign2))
			ratio_throw_heap(left, ret);
		else
			sign_reverse_ratio_common(left, ret);
		return;
	}
	if (inverse_ratio_p(right)) {
		GetDenomRatio(right, &right);
		div_rb_common(local, sign1, left, right, ret);
		return;
	}

	/* division */
	div_rr_common(local, sign1, left, right, ret);
}


/*
 *  division - integer
 */
void div_ff_real_common(LocalRoot local, addr left, addr right, addr *ret)
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
	if (value2 == 0)
		division_by_zero2(left, right);

	/* left */
	if (value1 == 0) {
		fixnum_heap(ret, 0);
		return;
	}
	if (value1 == 1) {
		inverse_value_common(ret, sign1, value2);
		return;
	}

	/* right */
	if (value2 == 1) {
		if (IsPlus(sign2))
			fixnum_throw_heap(left, ret);
		else
			sigrev_fixnum_integer_common(left, ret);
		return;
	}

	push_local(local, &stack);
	bignum_value_alloc(local, &left, SignPlus, value1);
	bignum_value_alloc(local, &right, SignPlus, value2);
	ratio_reduction_heap(local, ret, sign1, left, right);
	rollback_local(local, stack);
}

void div_ff_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	bigtype value1, value2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	castfixed_fixnum(left, &sign1, &value1);
	castfixed_fixnum(right, &sign2, &value2);
	sign1 = SignMulti(sign1, sign2);
	if (value2 == 0)
		division_by_zero2(left, right);

	/* left */
	if (value1 == 0) {
		fixnum_local(local, ret, 0);
		return;
	}
	if (value1 == 1) {
		inverse_value_local(local, ret, sign1, value2);
		return;
	}

	/* right */
	if (value2 == 1) {
		if (IsPlus(sign2))
			fixnum_throw_local(local, left, ret);
		else
			sigrev_fixnum_bignum_local(local, left, ret);
		return;
	}

	bignum_value_alloc(local, &left, SignPlus, value1);
	bignum_value_alloc(local, &right, SignPlus, value2);
	ratio_reduction_local(local, ret, sign1, left, right);
}

void div_fb_real_common(LocalRoot local, addr left, addr right, addr *ret)
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
	if (zerop_bignum(right))
		division_by_zero2(left, right);

	/* left */
	if (value1 == 0) {
		fixnum_heap(ret, 0);
		return;
	}
	if (value1 == 1) {
		bignum_sign_inverse_common(ret, sign1, right);
		return;
	}

	/* right */
	if (equal_value_nosign_bignum(right, 1)) {
		fixnum_throw_heap(left, ret);
		return;
	}

	push_local(local, &stack);
	bignum_value_alloc(local, &left, SignPlus, value1);
	bignum_copy_alloc(local, &right, right);
	SetSignBignum(right, SignPlus);
	ratio_reduction_heap(local, ret, sign1, left, right);
	rollback_local(local, stack);
}

void div_fb_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	bigtype value1;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	castfixed_fixnum(left, &sign1, &value1);
	GetSignBignum(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_bignum(right))
		division_by_zero2(left, right);

	/* left */
	if (value1 == 0) {
		fixnum_local(local, ret, 0);
		return;
	}
	if (value1 == 1) {
		bignum_sign_inverse_local(local, ret, sign1, right);
		return;
	}

	/* right */
	if (equal_value_nosign_bignum(right, 1)) {
		fixnum_throw_local(local, left, ret);
		return;
	}

	bignum_value_alloc(local, &left, SignPlus, value1);
	bignum_copy_alloc(local, &right, right);
	SetSignBignum(right, SignPlus);
	ratio_reduction_local(local, ret, sign1, left, right);
}

void div_bf_real_common(LocalRoot local, addr left, addr right, addr *ret)
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
	if (value2 == 0)
		division_by_zero2(left, right);

	/* left */
	if (zerop_bignum(left)) {
		fixnum_heap(ret, 0);
		return;
	}
	if (equal_value_nosign_bignum(left, 1)) {
		inverse_value_common(ret, sign2, value2);
		return;
	}

	/* right */
	if (value2 == 1) {
		if (IsPlus(sign2))
			bignum_throw_heap(left, ret);
		else
			sigrev_bignum_integer_common(left, ret);
		return;
	}

	push_local(local, &stack);
	bignum_copy_alloc(local, &left, left);
	SetSignBignum(left, SignPlus);
	bignum_value_alloc(local, &right, SignPlus, value2);
	ratio_reduction_heap(local, ret, sign1, left, right);
	rollback_local(local, stack);
}

void div_bf_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	bigtype value2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");

	GetSignBignum(left, &sign1);
	castfixed_fixnum(right, &sign2, &value2);
	sign1 = SignMulti(sign1, sign2);
	if (value2 == 0)
		division_by_zero2(left, right);

	/* left */
	if (zerop_bignum(left)) {
		fixnum_local(local, ret, 0);
		return;
	}
	if (equal_value_nosign_bignum(left, 1)) {
		inverse_value_local(local, ret, sign2, value2);
		return;
	}

	/* right */
	if (value2 == 1) {
		if (IsPlus(sign2))
			bignum_throw_local(local, left, ret);
		else
			sigrev_bignum_bignum_local(local, left, ret);
		return;
	}

	bignum_copy_alloc(local, &left, left);
	SetSignBignum(left, SignPlus);
	bignum_value_alloc(local, &right, SignPlus, value2);
	ratio_reduction_local(local, ret, sign1, left, right);
}

void div_bb_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	GetSignBignum(left, &sign1);
	GetSignBignum(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_bignum(right))
		division_by_zero2(left, right);

	/* left */
	if (zerop_bignum(left)) {
		fixnum_heap(ret, 0);
		return;
	}
	if (equal_value_nosign_bignum(left, 1)) {
		bignum_sign_inverse_common(ret, sign1, left);
		return;
	}

	/* right */
	if (equal_value_nosign_bignum(right, 1)) {
		if (IsPlus(sign1))
			integer_copy_heap(left, ret);
		else
			integer_copysign_heap(SignMinus, left, ret);
		return;
	}

	push_local(local, &stack);
	bignum_copy_nosign_alloc(local, &left, left);
	bignum_copy_nosign_alloc(local, &right, right);
	ratio_reduction_heap(local, ret, sign1, left, right);
	rollback_local(local, stack);
}

void div_bb_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	int sign1, sign2;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");

	GetSignBignum(left, &sign1);
	GetSignBignum(right, &sign2);
	sign1 = SignMulti(sign1, sign2);
	if (zerop_bignum(right))
		division_by_zero2(left, right);

	/* left */
	if (zerop_bignum(left)) {
		fixnum_local(local, ret, 0);
		return;
	}
	if (equal_value_nosign_bignum(left, 1)) {
		bignum_sign_inverse_local(local, ret, sign1, left);
		return;
	}

	/* right */
	if (equal_value_nosign_bignum(right, 1)) {
		if (IsPlus(sign1))
			integer_copy_local(local, left, ret);
		else
			integer_copysign_local(local, SignMinus, left, ret);
		return;
	}

	bignum_copy_nosign_alloc(local, &left, left);
	bignum_copy_nosign_alloc(local, &right, right);
	ratio_reduction_local(local, ret, sign1, left, right);
}


/*
 *  inverse
 */
void inverse_fixnum_common(addr left, addr *ret)
{
	int sign;
	bigtype value;

	castfixed_fixnum(left, &sign, &value);
	if (value == 0)
		division_by_zero1(left);
	if (value == 1) {
		fixnum_throw_heap(left, ret);
		return;
	}
	inverse_value_common(ret, sign, value);
}

void inverse_bignum_common(addr left, addr *ret)
{
	int sign;

	if (zerop_bignum(left))
		division_by_zero1(left);
	if (equal_rv_nosign(left, 1)) {
		bignum_throw_heap(left, ret);
		return;
	}
	GetSignBignum(left, &sign);
	bignum_sign_inverse_common(ret, sign, left);
}

void inverse_ratio_common(LocalRoot local, addr left, addr *ret)
{
	int sign;

	if (zerop_ratio(left))
		division_by_zero1(left);
	GetSignRatio(left, &sign);
	ratio_sign_inverse_common(local, ret, sign, left);
}

void abs_ratio_alloc(LocalRoot local, addr left, addr *ret)
{
	int sign;

	CheckType(left, LISPTYPE_RATIO);
	GetSignRatio(left, &sign);
	if (IsPlus(sign))
		ratio_throw_alloc(local, left, ret);
	else
		ratio_copy_nosign_alloc(local, ret, left);
}

void abs_ratio_local(LocalRoot local, addr left, addr *ret)
{
	Check(local == NULL, "local error");
	abs_ratio_alloc(local, left, ret);
}

void abs_ratio_heap(addr left, addr *ret)
{
	abs_ratio_alloc(NULL, left, ret);
}


/*
 *  output
 */
void output_nosign_ratio(LocalRoot local,
		addr stream, addr pos, unsigned base, int upperp)
{
	addr numer, denom;

	Check(! isBaseChar(base), "base error");
	/* zero */
	GetNumerRatio(pos, &numer);
	if (zerop_bignum(numer)) {
		write_char_stream(stream, '0');
		return;
	}

	/* integer */
	GetDenomRatio(pos, &denom);
	if (equal_value_nosign_bignum(denom, 1)) {
		output_nosign_bignum(local, stream, numer, base, upperp);
		return;
	}

	/* ratio */
	output_nosign_bignum(local, stream, numer, base, upperp);
	write_char_stream(stream, '/');
	output_nosign_bignum(local, stream, denom, base, upperp);
}

