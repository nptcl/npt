#include "bignum.h"
#include "bignum_cons.h"
#include "bignum_data.h"
#include "bignum_equal.h"
#include "bignum_object.h"
#include "bignum_output.h"
#include "character.h"
#include "condition.h"
#include "float_object.h"
#include "heap.h"
#include "local.h"
#include "memory.h"
#include "ratio.h"
#include "ratio_equal.h"
#include "stream.h"
#include "stream_function.h"
#include "stream_string.h"
#include "strtype.h"

_g int ratiop(addr pos)
{
	return GetType(pos) == LISPTYPE_RATIO;
}

_g void setnumer_ratio(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RATIO);
	SetNumerRatio_Low(pos, value);
}

_g void getnumer_ratio(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RATIO);
	GetNumerRatio_Low(pos, ret);
}

_g void setdenom_ratio(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_RATIO);
	SetDenomRatio_Low(pos, value);
}

_g void getdenom_ratio(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RATIO);
	GetDenomRatio_Low(pos, ret);
}

_g void setsign_ratio(addr pos, int sign)
{
	CheckType(pos, LISPTYPE_RATIO);
	SetSignRatio_Low(pos, sign);
}

_g void getsign_ratio(addr pos, int *ret)
{
	CheckType(pos, LISPTYPE_RATIO);
	GetSignRatio_Low(pos, ret);
}

_g int refsign_ratio(addr pos)
{
	CheckType(pos, LISPTYPE_RATIO);
	return RefSignRatio_Low(pos);
}

_g int getfixnum_ratio(addr pos, fixnum *ret)
{
	int sign;
	addr denom;
	bigtype value;

	CheckType(pos, LISPTYPE_RATIO);
	GetDenomRatio(pos, &denom);
	if (! equal_value_nosign_bignum(denom, 1))
		return 1;
	GetSignRatio(pos, &sign);
	GetNumerRatio(pos, &pos);
	if (RefSizeBignum(pos) != 1)
		return 1;
	getfixed_bignum(pos, 0, &value);
	if (IsPlus(sign)) {
		if (FIXNUM_MAX < value)
			return 1;
		*ret = (fixnum)value;
		return 0;
	}
	else {
		if (FIXNUM_UMIN < value)
			return 1;
		*ret = -(fixnum)value;
		return 0;
	}

	return 1;
}

_g int getfixed1_ratio(addr pos, int *sign, fixed *ret)
{
	addr denom;
	bigtype value;

	CheckType(pos, LISPTYPE_RATIO);
	GetDenomRatio(pos, &denom);
	if (! equal_value_nosign_bignum(denom, 1))
		return 1;
	GetNumerRatio(pos, &pos);
	if (RefSizeBignum(pos) != 1)
		return 1;
	GetSignRatio(pos, sign);
	getfixed_bignum(pos, 0, &value);

	return 0;
}


/*
 *  reduction
 */
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

_g void euclidean_bignum(LocalRoot local, addr numer, addr denom)
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
	copy_noexpand_bignum(numer, a);
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

_g void reduction_local(LocalRoot local, addr numer, addr denom)
{
	size_t size1, size2;

	Check(local == NULL, "local error");
	Check(zerop_bignum(denom), "division-by-zero error");
	if (zerop_bignum(numer))
		return;
	GetSizeBignum(numer, &size1);
	GetSizeBignum(denom, &size2);
	if (size1 == 1 && size2 == 1)
		reduction_single(numer, denom);
	else
		reduction_multiple(local, numer, denom);
}


/*
 *  ratio object
 */
_g void make_ratio_heap(addr *ret, int sign, addr numer, addr denom)
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

_g void make_ratio_local(LocalRoot local,
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

_g void make_ratio_alloc(LocalRoot local,
		addr *ret, int sign, addr numer, addr denom)
{
	if (local)
		make_ratio_local(local, ret, sign, numer, denom);
	else
		make_ratio_heap(ret, sign, numer, denom);
}

_g void make_ratio_alloc_unsafe(LocalRoot local,
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

_g void ratio_reduction_heap(LocalRoot local, addr *ret, int sign, addr numer, addr denom)
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

_g void ratio_reduction_local(LocalRoot local, addr *ret, int sign, addr numer, addr denom)
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

_g void ratio_noreduction_heap(addr *ret, int sign, addr numer, addr denom)
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

_g void ratio_noreduction_local(LocalRoot local,
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

_g void ratio_reduction_nocopy_local(LocalRoot local,
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

_g void make_ratio_reduction_heap(LocalRoot local,
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

_g void make_ratio_reduction_local(LocalRoot local,
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

_g void ratio_reduction_value_local(LocalRoot local, addr *ret,
		int sign, bigtype numer, bigtype denom)
{
	addr num, den;

	Check(local == NULL, "local error");
	bignum_value_local(local, &num, SignPlus, numer);
	bignum_value_local(local, &den, SignPlus, denom);
	make_ratio_reduction_local(local, ret, sign, num, den);
}

_g void ratio_reduction_value_heap(LocalRoot local, addr *ret,
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

_g void ratio_noreduction_value_local(LocalRoot local, addr *ret,
		int sign, bigtype numer, bigtype denom)
{
	addr num, den;

	Check(local == NULL, "local error");
	bignum_value_local(local, &num, SignPlus, numer);
	bignum_value_local(local, &den, SignPlus, denom);
	make_ratio_local(local, ret, sign, num, den);
}

_g void ratio_noreduction_value_heap(addr *ret,
		int sign, bigtype numer, bigtype denom)
{
	addr num, den;

	bignum_value_heap(&num, SignPlus, numer);
	bignum_value_heap(&den, SignPlus, denom);
	make_ratio_heap(ret, sign, num, den);
}

_g void ratio_zero_alloc(LocalRoot local, addr *ret)
{
	addr numer, denom;

	bignum_zero_alloc(local, &numer);
	bignum_value_alloc(local, &denom, SignPlus, 1);
	make_ratio_alloc(local, ret, SignPlus, numer, denom);
}

_g void ratio_zero_local(LocalRoot local, addr *ret)
{
	Check(local == NULL, "local error");
	ratio_zero_alloc(local, ret);
}

_g void ratio_zero_heap(addr *ret)
{
	ratio_zero_alloc(NULL, ret);
}

_g void ratio_copy_nosign_alloc(LocalRoot local, addr *ret, addr pos)
{
	addr numer, denom;

	Check(GetType(pos) != LISPTYPE_RATIO, "type error");
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	bignum_copy_alloc(local, &numer, numer);
	bignum_copy_alloc(local, &denom, denom);
	make_ratio_alloc(local, ret, SignPlus, numer, denom);
}

_g void ratio_copy_nosign_local(LocalRoot local, addr *ret, addr pos)
{
	Check(local == NULL, "local error");
	ratio_copy_nosign_alloc(local, ret, pos);
}

_g void ratio_copy_nosign_heap(addr *ret, addr pos)
{
	ratio_copy_nosign_alloc(NULL, ret, pos);
}

_g void ratio_copy_alloc(LocalRoot local, addr *ret, addr pos)
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

_g void ratio_copy_local(LocalRoot local, addr *ret, addr pos)
{
	Check(local == NULL, "local error");
	ratio_copy_alloc(local, ret, pos);
}

_g void ratio_copy_heap(addr *ret, addr pos)
{
	ratio_copy_alloc(NULL, ret, pos);
}

_g void ratio_throw_heap(addr pos, addr *ret)
{
	Check(GetType(pos) != LISPTYPE_RATIO, "type error");
	if (GetStatusDynamic(pos))
		ratio_copy_heap(ret, pos);
	else
		*ret = pos;
}

_g void ratio_throw_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	Check(GetType(pos) != LISPTYPE_RATIO, "type error");
	if (GetStatusDynamic(pos))
		*ret = pos;
	else
		ratio_copy_local(local, ret, pos);
}

_g void ratio_throw_alloc(LocalRoot local, addr pos, addr *ret)
{
	Check(GetType(pos) != LISPTYPE_RATIO, "type error");
	if (local)
		ratio_throw_local(local, pos, ret);
	else
		ratio_throw_heap(pos, ret);
}

_g void ratio_result_noreduction_local(LocalRoot local, addr pos, addr *ret)
{
	int sign;
	addr numer, denom;

	Check(local == NULL, "local error");
	CheckType(pos, LISPTYPE_RATIO);
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	if (zerop_bignum(numer)) {
		fixnum_local(local, ret, 0);
	}
	else if (equal_value_nosign_bignum(denom, 1)) {
		bignum_copy_local(local, &numer, numer);
		GetSignRatio(pos, &sign);
		SetSignBignum(numer, sign);
		bignum_result_local(local, numer, ret);
	}
	else {
		ratio_throw_local(local, pos, ret);
	}
}

_g void ratio_result_noreduction_heap(LocalRoot local, addr pos, addr *ret)
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
	}
	else if (equal_value_nosign_bignum(denom, 1)) {
		push_local(local, &stack);
		bignum_copy_local(local, &numer, numer);
		GetSignRatio(pos, &sign);
		SetSignBignum(numer, sign);
		bignum_result_heap(numer, ret);
		rollback_local(local, stack);
	}
	else {
		ratio_throw_heap(pos, ret);
	}
}

_g int zerop_ratio(addr left)
{
	Check(GetType(left) != LISPTYPE_RATIO, "type error");
	GetNumerRatio(left, &left);
	return zerop_bignum(left);
}

_g int plusp_ratio(addr left)
{
	Check(GetType(left) != LISPTYPE_RATIO, "type error");
	if (IsMinus(RefSignRatio(left)))
		return 0;
	return ! zerop_ratio(left);
}

_g int minusp_ratio(addr left)
{
	Check(GetType(left) != LISPTYPE_RATIO, "type error");
	if (IsPlus(RefSignRatio(left)))
		return 0;
	return ! zerop_ratio(left);
}


/*
 *  cast integer
 */
_g void cast_fixnum_ratio_local(LocalRoot local, addr pos, addr *ret)
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

_g void cast_bignum_ratio_local(LocalRoot local, addr pos, addr *ret)
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
static int diff_exponent_ratio_(size_t *size1, size_t *size2, addr pos)
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
		*size1 = *size2 = 0;
		return call_float_overflow_va_(NULL, CONSTANT_COMMON_COERCE, pos, NULL);
	}
	if (OVERFLOW_EXPONENT < *size2) {
		*size1 = *size2 = 0;
		return call_float_underflow_va_(NULL, CONSTANT_COMMON_COERCE, pos, NULL);
	}

	return 0;
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

_g int single_float_ratio_(addr pos, single_float *ret)
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
	if (zerop_bignum(denom)) {
		*ret = 0.0f;
		return call_division_by_zero_real1_(NULL, CONSTANT_COMMON_COERCE, pos);
	}
	if (zerop_bignum(numer))
		return Result(ret, sign? -0.0f: +0.0f);
	size1 = hexfloat_exponent(numer);
	size2 = hexfloat_exponent(denom);
	Return(diff_exponent_ratio_(&size1, &size2, pos));
	float_string_ratio(sign, numer, str1, size1, LISP_FLOAT_SINGLE_FRACTION);
	float_string_ratio(SignPlus, denom, str2, size2, LISP_FLOAT_SINGLE_FRACTION);
	Return(check_strtof_(str1, pos, &v1));
	Return(check_strtof_reverse_(str2, pos, &v2));
	v1 /= v2;
	Return_float_errorcheck1(CONSTANT_COMMON_COERCE, v1, pos);

	return Result(ret, v1);
}

_g int double_float_ratio_(addr pos, double_float *ret)
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
	if (zerop_bignum(denom)) {
		*ret = 0.0;
		return call_division_by_zero_real1_(NULL, CONSTANT_COMMON_COERCE, pos);
	}
	if (zerop_bignum(numer))
		return Result(ret, sign? -0.0: +0.0);
	size1 = hexfloat_exponent(numer);
	size2 = hexfloat_exponent(denom);
	Return(diff_exponent_ratio_(&size1, &size2, pos));
	float_string_ratio(sign, numer, str1, size1, LISP_FLOAT_DOUBLE_FRACTION);
	float_string_ratio(SignPlus, denom, str2, size2, LISP_FLOAT_DOUBLE_FRACTION);
	Return(check_strtod_(str1, pos, &v1));
	Return(check_strtod_reverse_(str2, pos, &v2));
	v1 /= v2;
	Return_float_errorcheck1(CONSTANT_COMMON_COERCE, v1, pos);

	return Result(ret, v1);
}

_g int long_float_ratio_(addr pos, long_float *ret)
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
	if (zerop_bignum(denom)) {
		*ret = 0.0L;
		return call_division_by_zero_real1_(NULL, CONSTANT_COMMON_COERCE, pos);
	}
	if (zerop_bignum(numer))
		return Result(ret, sign? -0.0L: +0.0L);
	size1 = hexfloat_exponent(numer);
	size2 = hexfloat_exponent(denom);
	Return(diff_exponent_ratio_(&size1, &size2, pos));
	float_string_ratio(sign, numer, str1, size1, LISP_FLOAT_LONG_FRACTION);
	float_string_ratio(SignPlus, denom, str2, size2, LISP_FLOAT_LONG_FRACTION);
	Return(check_strtold_(str1, pos, &v1));
	Return(check_strtold_reverse_(str2, pos, &v2));
	v1 /= v2;
	Return_float_errorcheck1(CONSTANT_COMMON_COERCE, v1, pos);

	return Result(ret, v1);
}

_g int single_float_ratio_alloc_(LocalRoot local, addr *ret, addr pos)
{
	single_float value;

	CheckType(pos, LISPTYPE_RATIO);
	Return(single_float_ratio_(pos, &value));
	single_float_alloc(local, ret, value);

	return 0;
}
_g int single_float_ratio_local_(LocalRoot local, addr *ret, addr pos)
{
	CheckLocal(local);
	return single_float_ratio_alloc_(local, ret, pos);
}
_g int single_float_ratio_heap_(addr *ret, addr pos)
{
	return single_float_ratio_alloc_(NULL, ret, pos);
}

_g int double_float_ratio_alloc_(LocalRoot local, addr *ret, addr pos)
{
	double_float value;

	CheckType(pos, LISPTYPE_RATIO);
	Return(double_float_ratio_(pos, &value));
	double_float_alloc(local, ret, value);

	return 0;
}
_g int double_float_ratio_local_(LocalRoot local, addr *ret, addr pos)
{
	CheckLocal(local);
	return double_float_ratio_alloc_(local, ret, pos);
}
_g int double_float_ratio_heap_(addr *ret, addr pos)
{
	return double_float_ratio_alloc_(NULL, ret, pos);
}

_g int long_float_ratio_alloc_(LocalRoot local, addr *ret, addr pos)
{
	long_float value;

	CheckType(pos, LISPTYPE_RATIO);
	Return(long_float_ratio_(pos, &value));
	long_float_alloc(local, ret, value);

	return 0;
}
_g int long_float_ratio_local_(LocalRoot local, addr *ret, addr pos)
{
	CheckLocal(local);
	return long_float_ratio_alloc_(local, ret, pos);
}
_g int long_float_ratio_heap_(addr *ret, addr pos)
{
	return long_float_ratio_alloc_(NULL, ret, pos);
}


/*
 *  abs
 */
_g void abs_ratio_alloc(LocalRoot local, addr left, addr *ret)
{
	int sign;

	CheckType(left, LISPTYPE_RATIO);
	GetSignRatio(left, &sign);
	if (IsPlus(sign))
		ratio_throw_alloc(local, left, ret);
	else
		ratio_copy_nosign_alloc(local, ret, left);
}

_g void abs_ratio_local(LocalRoot local, addr left, addr *ret)
{
	Check(local == NULL, "local error");
	abs_ratio_alloc(local, left, ret);
}

_g void abs_ratio_heap(addr left, addr *ret)
{
	abs_ratio_alloc(NULL, left, ret);
}


/*
 *  output
 */
_g int output_nosign_ratio_(LocalRoot local,
		addr stream, addr pos, unsigned base, int upperp)
{
	addr numer, denom;

	Check(! isBaseChar(base), "base error");
	/* zero */
	GetNumerRatio(pos, &numer);
	if (zerop_bignum(numer))
		return write_char_stream_(stream, '0');

	/* integer */
	GetDenomRatio(pos, &denom);
	if (equal_value_nosign_bignum(denom, 1))
		return output_nosign_bignum_(local, stream, numer, base, upperp);

	/* ratio */
	Return(output_nosign_bignum_(local, stream, numer, base, upperp));
	Return(write_char_stream_(stream, '/'));
	Return(output_nosign_bignum_(local, stream, denom, base, upperp));

	return 0;
}

