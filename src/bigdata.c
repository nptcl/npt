/*
 *  bignum
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
#include "object.h"

/*****************************************************************************
  operator
 *****************************************************************************/
static void multinumber3(bigtype *result, bigtype rvalue, bigtype *carry)
{
	/*     2 1  [rvalue]
	 *     2 1  [cvalue]
	 *   -----
	 *   a b c
	 * d e f
	 * -------
	 * C C R R
	 */
	bigtype cvalue, r1, r2, c1, c2, a, b, c, d, e, f;

	cvalue = *carry;
	if (rvalue == 0 || cvalue == 0) {
		*result = *carry = 0;
		return;
	}
	if (rvalue == 1) {
		*result = cvalue;
		*carry = 0;
		return;
	}
	if (cvalue == 1) {
		*result = rvalue;
		*carry = 0;
		return;
	}

	r2 = HIGHVALUE(rvalue);
	c2 = HIGHVALUE(cvalue);
	if (r2 == 0 && c2 == 0) {
		*result = rvalue * cvalue;
		*carry = 0;
		return;
	}
	r1 = LOWVALUE(rvalue);
	c1 = LOWVALUE(cvalue);
	if (c2 == 0) {
		c = r1 * c1;
		b = r2 * c1;
		b += HIGHVALUE(c);
		a = HIGHVALUE(b);
		b = LOWVALUE(b);
		c = LOWVALUE(c);
		*result = HIGHLOW(b, c);
		*carry = a;
		return;
	}
	if (r2 == 0) {
		c = r1 * c1;
		b = r1 * c2;
		b += HIGHVALUE(c);
		a = HIGHVALUE(b);
		b = LOWVALUE(b);
		c = LOWVALUE(c);
		*result = HIGHLOW(b, c);
		*carry = a;
		return;
	}

	/* first */
	c = r1 * c1;
	b = r2 * c1;
	b += HIGHVALUE(c);
	a = HIGHVALUE(b);
	b = LOWVALUE(b);
	c = LOWVALUE(c);

	/* second */
	f = r1 * c2;
	e = r2 * c2;
	e += HIGHVALUE(f);
	d = HIGHVALUE(e);
	e = LOWVALUE(e);
	f = LOWVALUE(f);

	/* result
	 *   a b c
	 * d e f
	 * -------
	 * C C R R
	 */
	b += f;
	*result = HIGHLOW(LOWVALUE(b), c);
	a += e + HIGHVALUE(b);
	*carry = HIGHLOW(d + HIGHVALUE(a), LOWVALUE(a));
}

static inline void multinumber(bigtype *result, bigtype *carry)
{
	multinumber3(result, *result, carry);
}

static inline void plusnumber(bigtype *result, bigtype *carry)
{
	/*
	 *  *ret = c1 + c2;
	 *  return (*ret < c1);
	 */
#ifdef BIGNUM_FULLCODE
	*result += *carry;
	*carry = *result < *carry;
#else
	*result += *carry;
	*carry = (*result >> BIGNUM_FULLBIT) != 0;
	*result &= BIGNUM_FULL;
#endif
}

void plusnumber_bigdata(bigtype *result, bigtype *carry)
{
	plusnumber(result, carry);
}

static inline void plusnumber3(bigtype *result, bigtype value, bigtype *carry)
{
	/*
	 *  *ret = c1 + c2;
	 *  return (*ret < c1);
	 */
#ifdef BIGNUM_FULLCODE
	*result = value + *carry;
	*carry = *result < *carry;
#else
	*result = value + *carry;
	*carry = (*result >> BIGNUM_FULLBIT) != 0;
	*result &= BIGNUM_FULL;
#endif
}

static void pluscarry(bigtype *result, bigtype value, bigtype *carry)
{
	plusnumber(result, &value);
	plusnumber(result, carry);
	*carry += value;
}

static inline void pluscarry4(bigtype *result,
		bigtype left, bigtype right, bigtype *carry)
{
	plusnumber3(result, left, &right);
	plusnumber(result, carry);
	*carry += right;
}

static inline void multicarry(bigtype *result, bigtype value, bigtype *carry)
{
	multinumber(result, &value);
	plusnumber(result, carry);
	*carry += value;
}

void multicarry_bigdata(bigtype *result, bigtype value, bigtype *carry)
{
	multicarry(result, value, carry);
}

static void inline multicarry4(bigtype *result,
		bigtype left, bigtype right, bigtype *carry)
{
	multinumber3(result, left, &right);
	plusnumber(result, carry);
	*carry += right;
}


/*****************************************************************************
  compare
 *****************************************************************************/
int equal_bigdata(addr left, addr right)
{
	size_t size1, size2;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	GetSizeBignum(left, &size1);
	GetSizeBignum(right, &size2);
	if (size1 != size2) return 0;
	GetRootBignum(left, &left);
	GetRootBignum(right, &right);
	return bigcmp(PtrDataBignum(left), PtrDataBignum(right), size1) == 0;
}

int compare_bigdata(addr left, addr right)
{
	const bigtype *data1, *data2;
	size_t size1, size2, i, index;
	bigtype value1, value2;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	GetSizeBignum(left, &size1);
	GetSizeBignum(right, &size2);
	if (size1 != size2)
		return (size1 < size2)? -1: 1;
	GetRootDataBignum(left, &left, &data1);
	GetRootDataBignum(right, &right, &data2);
	for (i = 0; i < size1; i++) {
		index = size1 - i - 1;
		value1 = data1[index];
		value2 = data2[index];
		if (value1 < value2) return -1;
		if (value1 > value2) return 1;
	}

	return 0;
}


/*****************************************************************************
  plus / minus
 *****************************************************************************/
static inline void minusnumber(bigtype *result, bigtype *carry)
{
	/*
	 * *ret = c1 - c2;
	 * return (*ret > c1);
	 */
	bigtype value = *result;
#ifdef BIGNUM_FULLCODE
	*result -= *carry;
#else
	*result = BIGNUM_FULL & (value - *carry);
#endif
	*carry = *result > value;
}

static inline void minusnumber3(bigtype *result, bigtype value, bigtype *carry)
{
	/*
	 * *ret = c1 - c2;
	 * return (*ret > c1);
	 */
#ifdef BIGNUM_FULLCODE
	*result = value - *carry;
#else
	*result = BIGNUM_FULL & (value - *carry);
#endif
	*carry = *result > value;
}

static void minuscarry(bigtype *result, bigtype value, bigtype *carry)
{
	minusnumber(result, &value);
	minusnumber(result, carry);
	*carry += value;
}

static inline void minuscarry4(bigtype *result,
		bigtype left, bigtype right, bigtype *carry)
{
	minusnumber3(result, left, &right);
	minusnumber(result, carry);
	*carry += right;
}

#define TailCopy(a,b,s,i) bigcpy((a)+(i), (b)+(i), ((s)-(i)))

void setplusvalue_bigdata(addr set, addr left, int sign, fixed right)
{
	addr root;
	bigtype *data1;
	const bigtype *data2;
	size_t size, i;

	GetSizeBignum(left, &size);
	GetRootDataBignum(set, &root, &data1);
	GetRootDataBignum(left, &root, &data2);
	for (i = 0; right && i < size; i++)
		plusnumber3(&data1[i], data2[i], &right);
	if (i < size)
		TailCopy(data1, data2, size, i);
	if (right) {
		data1[i] = right;
		size++;
	}
	Check(RefAllocBignum(set) <= size, "bignum size error");
	SetSizeBignum(set, size);
	SetSignBignum(set, sign);
}

void plusvalue_bigdata_alloc(LocalRoot local,
		addr left, int sign, fixed right, addr *ret)
{
	addr pos;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type error");
	alloc_bignum(local, &pos, RefSizeBignum(left) + 1UL);
	setplusvalue_bigdata(pos, left, sign, right);
	*ret = pos;
}

void setminusvalue_bigdata(addr set, addr left, int sign, fixed right)
{
	addr root;
	bigtype *data1, value;
	const bigtype *data2;
	size_t size, i;

	GetSizeBignum(left, &size);
	SetSizeBignum(set, size);
	GetRootDataBignum(set, &root, &data1);
	GetRootDataBignum(left, &root, &data2);

	/* single value */
	Check(size == 0, "size error");
	if (size == 1) {
		value = data2[0];
		if (value == right) {
			setzero_bignum(set);
		}
		else if (value < right) {
			data1[0] = right - value;
			SetSignBignum(set, SignNot(sign));
		}
		else {
			data1[0] = value - right;
			SetSignBignum(set, sign);
		}
		return;
	}

	/* multi value */
	SetSignBignum(set, sign);
	for (i = 0; right; i++)
		minusnumber3(&data1[i], data2[i], &right);
	if (i < size)
		TailCopy(data1, data2, size, i);
	sizepress_bignum(set);
}

void minusvalue_bigdata_alloc(LocalRoot local,
		addr left, int sign, fixed right, addr *ret)
{
	addr pos;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type error");
	alloc_bignum(local, &pos, RefSizeBignum(left));
	setminusvalue_bigdata(pos, left, sign, right);
	*ret = pos;
}

static void plusloop(addr pos, size_t size1, size_t size2,
		const bigtype *data1, const bigtype *data2)
{
	size_t i;
	bigtype carry, *data;
	struct bignuminfo *ptr;

	ptr = StructBignum(pos);
	Check(size2 < size1, "size error");
	Check(ptr->alloc <= size2, "alloc error");
	GetRootDataBignum(pos, &pos, &data);
	carry = 0;
	for (i = 0; i < size1; i++)
		pluscarry4(&data[i], data1[i], data2[i], &carry);
	for (; carry && i < size2; i++)
		plusnumber3(&data[i], data2[i], &carry);
	if (i < size2)
		TailCopy(data, data2, size2, i);
	if (carry) {
		data[i] = carry;
		size2++;
	}
	ptr->size = size2;
}

void plus_bigdata_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	addr pos;
	const bigtype *data1, *data2;
	size_t size1, size2;

	GetSizeBignum(left, &size1);
	GetSizeBignum(right, &size2);
	GetRootDataBignum(left, &left, &data1);
	GetRootDataBignum(right, &right, &data2);
	if (size1 < size2) {
		alloc_bignum(local, &pos, size2 + 1);
		plusloop(pos, size1, size2, data1, data2);
	}
	else {
		alloc_bignum(local, &pos, size1 + 1);
		plusloop(pos, size2, size1, data2, data1);
	}
	*ret = pos;
}

void letplus_noexpand_bigdata(addr left, addr right)
{
	addr root;
	const bigtype *data1, *data2;
	size_t size1, size2;

	GetSizeBignum(left, &size1);
	GetSizeBignum(right, &size2);
	Check(RefAllocBignum(left) < size1 + 1, "size1 error");
	Check(RefAllocBignum(left) < size2 + 1, "size2 error");
	GetRootDataBignum(left, &root, &data1);
	GetRootDataBignum(right, &root, &data2);
	if (size1 < size2) {
		plusloop(left, size1, size2, data1, data2);
	}
	else {
		plusloop(left, size2, size1, data2, data1);
	}
}

static inline void setminus_bigdata_call(addr pos,
		const bigtype *data1,
		size_t size1,
		const bigtype *data2,
		size_t size2)
{
	addr root;
	bigtype carry, *data;
	size_t i;

	GetRootDataBignum(pos, &root, &data);
	SetSizeBignum(pos, size1);
	carry = 0;
	for (i = 0; i < size2; i++)
		minuscarry4(&data[i], data1[i], data2[i], &carry);
	for (; carry; i++)
		minusnumber3(&data[i], data1[i], &carry);
	if (i < size1)
		TailCopy(data, data1, size1, i);
	sizepress_bignum(pos);
}

static void minus_bigdata_call(LocalRoot local,
		addr *ret,
		const bigtype *data1,
		size_t size1,
		const bigtype *data2,
		size_t size2)
{
	alloc_bignum(local, ret, size1);
	setminus_bigdata_call(*ret, data1, size1, data2, size2);
}

void minus_bigdata_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	addr root;
	size_t size1, size2;
	const bigtype *data1, *data2;

	GetSizeBignum(left, &size1);
	GetSizeBignum(right, &size2);
	GetRootDataBignum(left, &root, &data1);
	GetRootDataBignum(right, &root, &data2);
	Check(size1 < size2, "size error");
	Check((size1 == 1) && (data1[0] < data2[0]), "value error");
	Check(size1 == size2 && compare_bigdata(left, right) < 0, "compare error");
	minus_bigdata_call(local, ret, data1, size1, data2, size2);
}

int minuscheck_bigdata_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	int check;
	addr root;
	size_t size1, size2;
	const bigtype *data1, *data2;

	check = compare_bigdata(left, right);
	if (check == 0) {
		bignum_value_alloc(local, ret, SignPlus, 0);
		return 0;
	}
	if (0 < check) {
		GetSizeBignum(left, &size1);
		GetSizeBignum(right, &size2);
		GetRootDataBignum(left, &root, &data1);
		GetRootDataBignum(right, &root, &data2);
		check = 0;
	}
	else {
		GetSizeBignum(left, &size2);
		GetSizeBignum(right, &size1);
		GetRootDataBignum(left, &root, &data2);
		GetRootDataBignum(right, &root, &data1);
		check = 1;
	}
	minus_bigdata_call(local, ret, data1, size1, data2, size2);

	return check;
}

static void setminus_noexpand(addr pos, addr left, addr right)
{
	addr root;
	size_t size1, size2;
	const bigtype *data1, *data2;

	GetSizeBignum(left, &size1);
	GetSizeBignum(right, &size2);
	GetRootDataBignum(left, &root, &data1);
	GetRootDataBignum(right, &root, &data2);
	Check(size1 < size2, "size error");
	Check((size1 == 1) && (data1[0] < data2[0]), "value error");
	Check(size1 == size2 && compare_bigdata(left, right) < 0, "compare error");
	Check(RefAllocBignum(pos) < size1, "alloc error");
	setminus_bigdata_call(pos, data1, size1, data2, size2);
}

int letminus_noexpand_bigdata(addr left, addr right)
{
	int check;

	check = compare_bigdata(left, right);
	if (check == 0) {
		setzero_bignum(left);
		return 0;
	}
	if (check < 0) {
		setminus_noexpand(left, right, left);
		return 1;
	}
	else {
		setminus_noexpand(left, left, right);
		return 0;
	}
}


/*****************************************************************************
  multiple
 *****************************************************************************/
void multicarry_fixnum(LocalRoot local, fixnum left, fixnum right, addr *ret)
{
	int sign1, sign2;
	fixed fixed1, fixed2;

	castfixed(left, &sign1, &fixed1);
	castfixed(right, &sign2, &fixed2);
	sign1 = SignMulti(sign1, sign2);
	multinumber(&fixed1, &fixed2);
	if (fixed2 != 0) {
		bignum_value2_alloc(local, ret, sign1, fixed2, fixed1);
	}
	else if (IsPlus(sign1) && (fixed1 <= FIXNUM_MAX)) {
		fixnum_alloc(local, ret, (fixnum)fixed1);
	}
	else if (IsMinus(sign1) && (fixed1 <= FIXNUM_UMIN)) {
		fixnum_alloc(local, ret, -(fixnum)fixed1);
	}
	else {
		bignum_value_alloc(local, ret, sign1, fixed1);
	}
}

void multicarry_bignum(LocalRoot local, fixnum left, fixnum right, addr *ret)
{
	int sign1, sign2;
	fixed fixed1, fixed2;

	castfixed(left, &sign1, &fixed1);
	castfixed(right, &sign2, &fixed2);
	sign1 = SignMulti(sign1, sign2);
	multinumber(&fixed1, &fixed2);
	if (fixed2)
		bignum_value2_alloc(local, ret, sign1, fixed2, fixed1);
	else
		bignum_value_alloc(local, ret, sign1, fixed1);
}

void setmultivalue_bigdata(addr pos, addr left, bigtype right)
{
	addr root;
	bigtype *data1, carry;
	const bigtype *data2;
	size_t i, size;

	Check(GetType(pos) != LISPTYPE_BIGNUM, "type pos error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");

	GetSizeBignum(left, &size);
	Check(RefAllocBignum(pos) <= size, "size error");
	GetRootDataBignum(pos, &root, &data1);
	GetRootDataBignum(left, &root, &data2);

	carry = 0;
	for (i = 0; i < size; i++)
		multicarry4(&data1[i], data2[i], right, &carry);
	if (carry)
		data1[i++] = carry;
	SetSizeBignum(pos, i);
}

void setmulti_bigdata(addr pos, addr left, addr right)
{
	addr root;
	size_t size1, size2, a, b, c;
	bigtype *data, value1, value2, carry;
	const bigtype *data1, *data2;

	GetSizeBignum(left, &size1);
	GetSizeBignum(right, &size2);
	Check(RefAllocBignum(pos) < size1 + size2, "size error");

	GetRootDataBignum(pos, &root, &data);
	GetRootDataBignum(left, &root, &data1);
	GetRootDataBignum(right, &root, &data2);

	bigset(data, 0, size1 + size2);
	for (b = c = 0; b < size2; b++) {
		value2 = data2[b];
		carry = 0;
		for (a = 0; a < size1; a++) {
			value1 = data1[a];
			multicarry(&value1, value2, &carry);
			for (c = a + b; value1; c++)
				plusnumber(&data[c], &value1);
		}
		for (c = a + b; carry; c++)
			plusnumber(&data[c], &carry);
	}
	SetSizeBignum(pos, c);
}

void multi_bigdata_alloc(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	alloc_plus_bignum(local, ret, RefSizeBignum(left), RefSizeBignum(right));
	setmulti_bigdata(*ret, left, right);
}


/*****************************************************************************
  division
 *****************************************************************************/
static void divhalf(bigtype *high, bigtype *low, bigtype denom, bigtype *carry)
{
	bigtype v1, v2, n3, n4, a2, a3, a4, a;

	v1 = CUTB(*high);
	v2 = CUTB(*low);
	n3 = HIGHVALUE(v2);
	n4 = LOWVALUE(v2);

	a2 = v1 / denom;
	a = v1 % denom;
	v1 = HIGHLOW(a, n3);
	a3 = v1 / denom;
	a = v1 % denom;
	v1 = HIGHLOW(a, n4);
	a4 = v1 / denom;

	*high = CUTB(a2);
	*low = HIGHLOW(a3, a4);
	*carry = CUTB(v1 % denom);
}

static void divloop(bigtype m1, bigtype denom,
		bigtype *quot, bigtype *rem,
		bigtype n1, bigtype n2, bigtype n3)
{
	/*
	 *  a: [n1 n2 n3] / denom
	 */
	bigtype a, nn, v1, v2, v3;

	/* nn = [n2 n3] */
	SETB(n1);
	SETB(n2);
	SETB(n3);
	nn = CUTB(HIGHLOW(n2, n3));
	if (n1 == 0 && nn < denom) {
		*quot = 0;
		*rem = nn;
		return;
	}
	/* a = [n1 n2] / m1; */
	a = CUTB(HIGHLOW(n1, n2) / m1);
	/* a * denom = [v1 v2 v2] */
	v1 = 0;
	multicarry4(&v2, a, denom, &v1);
	SETB(v2);
	/* [n1 n2 n3] - [v1 v2 v2] */
	v3 = 0;
	minuscarry(&nn, v2, &v3);
	SETB(nn);
	SETB(v3);
	minuscarry(&n1, v1, &v3);
	SETB(n1);
	SETB(v3);
	while (v3) {
		a--;
		SETB(a);
		v3 = 0;
		pluscarry(&nn, denom, &v3);
		SETB(nn);
		SETB(v3);
		v3 = (v3 == 0);
	}
	*quot = a;
	*rem = nn;
}

#define GETSHIFTVALUECHECK (1ULL << (BIGNUM_FULLBIT - 1))
static int getshiftvalue(bigtype *value)
{
	int count;

#ifdef LISP_BIGNUM_DEBUG
	if (*value == 0)
		fmte("getshiftvalue error", NULL);
#endif
	for (count = 0; *value < GETSHIFTVALUECHECK; count++)
		*value <<= 1;

	return count;
}

static void divfull(bigtype *high, bigtype *low, bigtype denom, bigtype *carry)
{
	int shift, nshift;
	bigtype m1, a2, a3, a4;
	bigtype nn, s1, s2;

	/* shift denom */
	shift = getshiftvalue(&denom);
#ifdef LISP_BIGNUM_DEBUG
	if (BIGNUM_HALFBIT <= shift)
		fmte("getshiftvalue error", NULL);
#endif

	m1 = HIGHVALUE(denom);
	s1 = CUTB(*high);
	s2 = CUTB(*low);
	if (shift == 0) {
		/* a2: [n1 n2] / denom */
		if (s1 < denom) {
			a2 = 0;
			nn = s1;
		}
		else {
			a2 = s1 / denom;
			nn = s1 % denom;
		}
		shift = 0;
		goto second;
	}

	/* shift */
	nshift = BIGNUM_FULLBIT - shift;
	nn = CUTB(s1 >> nshift);
	s1 = CUTB((s1 << shift) | (s2 >> nshift));
	s2 = CUTB(s2 << shift);

	/* a2: [n0 n1 n2] / denom */
	divloop(m1, denom, &a2, &nn, CUTB(nn), HIGHVALUE(s1), LOWVALUE(s1));
second:
	/* a3: [n1 n2 n3] / denom */
	divloop(m1, denom, &a3, &nn, HIGHVALUE(nn), LOWVALUE(nn), HIGHVALUE(s2));
	/* a4: [n2 n3 n4] / denom */
	divloop(m1, denom, &a4, &nn, HIGHVALUE(nn), LOWVALUE(nn), LOWVALUE(s2));

	/* result */
	*high = CUTB(a2);
	*low = HIGHLOW(a3, a4);
	*carry = CUTB(nn >> shift);
}

static void divdouble(bigtype *high, bigtype *low, bigtype denom, bigtype *carry)
{
	/*
	 *  1. ----/00  -> error
	 *  2. 0000/--  -> 0
	 *  3. 00bb/cc  -> b/c
	 *  4. aabb/-c  -> divhalf(aabb/-c)
	 *  5. aabb/cc  -> divfull(aabb/cc)
	 */
	bigtype value;

#ifdef LISP_BIGNUM_DEBUG
	/* denom is zero */
	if (denom == 0)
		fmte("divdouble error, denom is zero.", NULL);
#endif

	/* single division */
	if (*high == 0) {
		/* numerator is zero */
		value = *low;
		if (value == 0) {
			*carry = 0;
			return;
		}
		/* normal division */
		*low = value / denom;
		*carry = value % denom;
		return;
	}

	if (denom == 1) {
		*carry = 0;
		return;
	}

	if (HIGHVALUE(denom))
		divfull(high, low, denom, carry);
	else
		divhalf(high, low, denom, carry);
}

static void divcarry4_half(bigtype *ptr, bigtype left, bigtype denom, bigtype *carry)
{
	bigtype n1;
#ifndef LISP_BIGNUM_DEBUG
	bigtype n2;
#endif

#ifdef LISP_BIGNUM_DEBUG
	if (denom <= *carry)
		fmte("divcarry4_half error. (carry)", NULL);
	if (HIGHVALUE(denom))
		fmte("divcarry4_half error. (denom)", NULL);
#endif

	if (*carry == 0) {
		*ptr = left / denom;
		*carry = left % denom;
		return;
	}

#ifdef LISP_BIGNUM_DEBUG
	n1 = *carry;
	divhalf(&n1, &left, denom, carry);
	if (n1)
		fmte("divcarry4_half error", NULL);
	*ptr = left;
#else
	n1 = HIGHLOW(*carry, HIGHVALUE(left));
	n2 = n1 / denom;
	n1 = n1 % denom;
	n1 = HIGHLOW(n1, LOWVALUE(left));
	*carry = n1 % denom;
	*ptr = HIGHLOW(n2, n1 / denom);
#endif
}

static void divcarry4_full(bigtype *ptr, bigtype left, bigtype denom, bigtype *carry)
{
	bigtype high;

	if (*carry == 0) {
		*ptr = left / denom;
		*carry = left % denom;
		return;
	}

	high = *carry;
	divfull(&high, &left, denom, carry);
#ifdef LISP_BIGNUM_DEBUG
	if (high)
		fmte("divcarry4_full error", NULL);
#endif
	*ptr = left;
}

static void multiminusdata(
		bigtype *r, const bigtype *data2, size_t size, bigtype a, bigtype *carry)
{
	size_t i;
	bigtype m, check;

	check = 0;
	for (i = 0; i < size; i++) {
		multicarry4(&m, data2[i], a, &check);
		minuscarry(&r[i], m, carry);
	}
	minuscarry(&r[i], check, carry);
}

static void pluscarrydata(bigtype *r, const bigtype *data2, size_t size, bigtype *carry)
{
	size_t i;
	bigtype check;

	check = 0;
	for (i = 0; i < size; i++)
		pluscarry(&r[i], data2[i], &check);
	if (check)
		*carry = 0;
}

static int comparedata(
		const bigtype *data1, size_t size1,
		const bigtype *data2, size_t size2)
{
	size_t i, index1, index2;
	bigtype a, b;

	for (i = 0; i < size2; i++) {
		index1 = size1 - i - 1;
		index2 = size2 - i - 1;
		a = data1[index1];
		b = data2[index2];
		if (a < b) return -1;
		if (a > b) return 1;
	}

	return 0;
}

static int comparedata2(
		const bigtype *data1, size_t size1,
		const bigtype *data2, size_t size2)
{
	size_t i, index;
	bigtype a, b;

	for (; size1 && data1[size1 - 1] == 0; size1--) continue;
	for (; size2 && data2[size2 - 1] == 0; size2--) continue;
	if (size1 < size2) return -1;
	if (size1 > size2) return 1;

	for (i = 0; i < size1; i++) {
		index = size1 - i - 1;
		a = data1[index];
		b = data2[index];
		if (a < b) return -1;
		if (a > b) return 1;
	}

	return 0;
}

static void quotdata(bigtype *datar, const bigtype *data2, size_t size2, bigtype *q)
{
	bigtype carry;

	carry = 0;
	multiminusdata(datar, data2, size2, *q, &carry);
	while (carry) {
		(*q)--;
		pluscarrydata(datar, data2, size2, &carry);
	}
	datar[size2] = 0;
}

static void divrem_calculate(addr quot, addr rem, addr left, addr right)
{
	int compare;
	size_t pos, size1, size2, sizer, sizeq, len;
	const bigtype *data1, *data2;
	bigtype *datar, *dataq, high, q, dn, carry;

	GetSizeBignum(left, &size1);
	GetSizeBignum(right, &size2);
	GetSizeBignum(quot, &sizeq);
	GetSizeBignum(rem, &sizer);
	GetRootDataBignum(left, &left, &data1);
	GetRootDataBignum(right, &right, &data2);
	GetRootDataBignum(quot, &quot, &dataq);
	GetRootDataBignum(rem, &rem, &datar);

	pos = 0;
	len = size1 - size2 + 1;
	dn = data2[size2 - 1];

	/* first check */
	compare = comparedata(data1, size1, data2, size2);
	if (compare == 0) { /* equal */
		dataq[sizeq - pos++ - 1] = 1;
		bigset(datar, 0, sizer);
	}
	else if (0 < compare) { /* N division */
		q = data1[size1 - 1] / dn;
		bigcpy(datar, data1 + size1 - size2, size2);
		datar[size2] = 0;
		quotdata(datar, data2, size2, &q);
		dataq[sizeq - pos++ - 1] = q;
	}
	else { /* N+1 division */
		bigcpy(datar, data1 + size1 - size2, size2);
		datar[size2] = 0;
		dataq[sizeq - pos++ - 1] = 0;
	}

	/* N+1 division */
	for (; pos < len; pos++) {
		/* shift */
		bigmove(datar + 1, datar, size2);
		datar[0] = data1[size1 - size2 - pos];
		compare = comparedata2(datar, sizer, data2, size2);
		if (compare == 0) {
			dataq[sizeq - pos - 1] = 1;
			bigset(datar, 0, sizer);
			continue;
		}
		else if (compare < 0) {
			dataq[sizeq - pos - 1] = 0;
			continue;
		}

		/* quot */
		high = datar[sizer - 1];
		q = datar[sizer - 2];
		divdouble(&high, &q, dn, &carry);
		if (high) q = BIGNUM_FULL;
		quotdata(datar, data2, size2, &q);
		dataq[sizeq - pos - 1] = q;
	}
}

static void shiftup(int shift, int nshift,
		bigtype *dst, const bigtype *src, size_t size, int final)
{
	size_t i;
	bigtype carry, temp;

	carry = 0;
	for (i = 0; i < size; i++) {
		temp = src[i];
		dst[i] = CUTB((temp << shift) | (carry >> nshift));
		carry = temp;
	}
	if (final)
		dst[i] = carry >> nshift;
}

static void shiftdown(int shift, int nshift, bigtype *dst, size_t size)
{
	size_t i, index;
	bigtype carry, temp;

	carry = 0;
	for (i = 0; i < size; i++) {
		index = size - i - 1;
		temp = dst[index];
		dst[index] = CUTB((carry << nshift) | (temp >> shift));
		carry = temp;
	}
}

#define make_bignum_local(local, ret, size) { \
	addr __pos; \
	alloc_bignum(local, &__pos, (size)); \
	SetSizeBignum(__pos, (size)); \
	*(ret) = __pos; \
}

static void div_noexpand(LocalRoot local, addr *rq, addr *rr, addr left, addr right)
{
	int shift, nshift;
	addr root, quot, rem, a, b;
	bigtype *data1, *data2, *dataa, *datab, carry;
	size_t size1, size2, sizea;

	GetSizeBignum(left, &size1);
	GetSizeBignum(right, &size2);
	GetRootDataBignum(right, &root, &data2);
	carry = data2[size2 - 1];
	shift = getshiftvalue(&carry);
	if (shift == 0) {
		make_bignum_local(local, &quot, size1 - size2 + 1);
		make_bignum_local(local, &rem, size2 + 1);
		divrem_calculate(quot, rem, left, right);
	}
	else {
		/* memory allocate */
		make_bignum_local(local, &a, size1 + 1);
		make_bignum_local(local, &b, size2);

		/* shiftup */
		nshift = BIGNUM_FULLBIT - shift;
		GetRootDataBignum(left, &root, &data1);
		GetRootDataBignum(right, &root, &data2);
		GetRootDataBignum(a, &root, &dataa);
		GetRootDataBignum(b, &root, &datab);
		shiftup(shift, nshift, dataa, data1, size1, 1);
		shiftup(shift, nshift, datab, data2, size2, 0);
		sizepress_bignum(a);

		/* run calculate */
		GetSizeBignum(a, &sizea);
		make_bignum_local(local, &quot, sizea - size2 + 1);
		make_bignum_local(local, &rem, size2 + 1);
		divrem_calculate(quot, rem, a, b);

		/* shift reverse rem */
		GetRootDataBignum(rem, &root, &data1);
		GetSizeBignum(rem, &size1);
		shiftdown(shift, nshift, data1, size1);
	}

	sizepress_bignum(quot);
	sizepress_bignum(rem);
	*rq = quot;
	*rr = rem;
}

static void letdiv_noexpand(LocalRoot local, addr left, addr right)
{
	addr quot, rem;
	LocalStack stack;

	push_local(local, &stack);
	div_noexpand(local, &quot, &rem, left, right);
	copy_noexpand_bignum(left, quot);
	rollback_local(local, stack);
}

static bigtype letdivvalue_buffer(addr left, bigtype right)
{
	addr root;
	bigtype *data, carry;
	size_t size, i, index;

	GetSizeBignum(left, &size);
	GetRootDataBignum(left, &root, &data);

	/* single */
	if (size == 1) {
		carry = data[0] % right;
		data[0] /= right;
		return carry;
	}

	/* multiple */
	carry = 0;
	index = size - 1;
	if (HIGHVALUE(right)) {
		for (i = 0; i < size; i++) {
			divcarry4_full(&data[index], data[index], right, &carry);
			index--;
		}
	}
	else {
		for (i = 0; i < size; i++) {
			divcarry4_half(&data[index], data[index], right, &carry);
			index--;
		}
	}
	sizepress_bignum(left);

	return carry;
}

void letdiv_noexpand_bigdata(LocalRoot local, addr left, addr right)
{
	int check1, check2, compare;
	size_t size1, size2;
	bigtype a, b;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type left error");
	if (zerop_bignum(right)) {
		division_by_zero2(left, right);
		return;
	}
	if (zerop_bignum(left)) {
		setzero_bignum(left);
		return;
	}
	GetSizeBignum(left, &size1);
	GetSizeBignum(right, &size2);
	check1 = (size1 == 1);
	check2 = (size2 == 1);
	if (check1 && check2) {
		GetRootBignum(right, &right);
		b = PtrDataBignum(right)[0];
		GetRootBignum(left, &right);
		a = PtrDataBignum(right)[0];
		setvalue_bignum(left, SignPlus, a / b);
		return;
	}
	if (check2) {
		GetRootBignum(right, &right);
		b = PtrDataBignum(right)[0];
		if (b == 1) return;
		(void)letdivvalue_buffer(left, b);
		return;
	}
	compare = compare_bigdata(left, right);
	if (compare == 0) {
		setvalue_bignum(left, SignPlus, 1);
		return;
	}
	if (compare < 0) {
		setzero_bignum(left);
		return;
	}
	letdiv_noexpand(local, left, right);
}

static void setrem_noexpand(LocalRoot local, addr set, addr left, addr right)
{
	addr quot, rem;
	LocalStack stack;

	push_local(local, &stack);
	div_noexpand(local, &quot, &rem, left, right);
	copy_noexpand_bignum(set, rem);
	rollback_local(local, stack);
}

static bigtype remvalue_buffer(addr left, bigtype right)
{
	addr root;
	bigtype *data, carry, dummy;
	size_t size, i, index;

	GetSizeBignum(left, &size);
	GetRootDataBignum(left, &root, &data);

	/* single */
	if (size == 1)
		return data[0] % right;

	/* multiple */
	carry = 0;
	index = size - 1;
	if (HIGHVALUE(right)) {
		for (i = 0; i < size; i++) {
			divcarry4_full(&dummy, data[index], right, &carry);
			index--;
		}
	}
	else {
		for (i = 0; i < size; i++) {
			divcarry4_half(&dummy, data[index], right, &carry);
			index--;
		}
	}

	return carry;
}

void setrem_noexpand_bigdata(LocalRoot local, addr set, addr left, addr right)
{
	int check1, check2, compare;
	size_t size1, size2;
	bigtype a, b;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type left error");
	if (zerop_bignum(right)) {
		division_by_zero2(left, right);
		return;
	}
	if (zerop_bignum(left)) {
		setzero_bignum(set);
		return;
	}
	GetSizeBignum(left, &size1);
	GetSizeBignum(right, &size2);
	check1 = (size1 == 1);
	check2 = (size2 == 1);
	if (check1 && check2) {
		GetRootBignum(left, &left);
		GetRootBignum(right, &right);
		a = PtrDataBignum(left)[0];
		b = PtrDataBignum(right)[0];
		setvalue_bignum(set, SignPlus, a % b);
		return;
	}
	if (check2) {
		GetRootBignum(right, &right);
		b = PtrDataBignum(right)[0];
		if (b == 1) {
			setzero_bignum(set);
			return;
		}
		b = remvalue_buffer(left, b);
		setvalue_bignum(set, SignPlus, b);
		return;
	}
	compare = compare_bigdata(left, right);
	if (compare == 0) {
		setzero_bignum(set);
		return;
	}
	if (compare < 0) {
		copy_noexpand_bignum(set, left);
		return;
	}
	setrem_noexpand(local, set, left, right);
}

void divrem_bigdata_local(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	int check1, check2, compare;
	size_t size1, size2;
	bigtype a, b;

	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type left error");
	if (zerop_bignum(right)) {
		division_by_zero2(left, right);
		return;
	}
	if (zerop_bignum(left)) {
		bignum_value_alloc(local, quot, SignPlus, 0);
		bignum_value_alloc(local, rem, SignPlus, 0);
		return;
	}
	GetSizeBignum(left, &size1);
	GetSizeBignum(right, &size2);
	Check(size1 == 0, "size1 error");
	Check(size2 == 0, "size2 error");
	check1 = (size1 <= 1);
	check2 = (size2 <= 1);
	if (check1 && check2) {
		GetRootBignum(left, &left);
		GetRootBignum(right, &right);
		a = PtrDataBignum(left)[0];
		b = PtrDataBignum(right)[0];
		bignum_value_alloc(local, quot, SignPlus, a / b);
		bignum_value_alloc(local, rem, SignPlus, a % b);
		return;
	}
	if (check2) {
		GetRootBignum(right, &right);
		b = PtrDataBignum(right)[0];
		if (b == 1) {
			bignum_copy_nosign_alloc(local, quot, left);
			bignum_value_alloc(local, rem, SignPlus, 0);
		}
		else {
			bignum_copy_nosign_alloc(local, quot, left);
			b = letdivvalue_buffer(*quot, b);
			bignum_value_alloc(local, rem, SignPlus, b);
		}
		return;
	}
	compare = compare_bigdata(left, right);
	if (compare == 0) {
		bignum_value_alloc(local, quot, SignPlus, 1);
		bignum_value_alloc(local, rem, SignPlus, 0);
		return;
	}
	if (compare < 0) {
		bignum_value_alloc(local, quot, SignPlus, 0);
		bignum_copy_nosign_alloc(local, rem, left);
		return;
	}
	div_noexpand(local, quot, rem, left, right);
}


/*
 *  shift
 */
void power2_bigdata_alloc(LocalRoot local, addr *ret, size_t value)
{
	size_t count;
	bigtype *data;
	addr pos, root;

	count = value / BIGNUM_FULLBIT;
	value = value % BIGNUM_FULLBIT;
	alloc_bignum(local, &pos, count + 1);
	GetRootDataBignum(pos, &root, &data);
	if (count) {
		bigset(data, 0, count);
	}
	data[count] = (1ULL << (bigtype)value);
	SetSizeBignum(pos, count + 1);
	*ret = pos;
}

void shiftup_bigdata_alloc(LocalRoot local, addr *ret, addr left, size_t value)
{
	addr pos, root;
	size_t size, count, dsize, i;
	bigtype *data1, *data2, next, temp;

	if (value == 0) {
		bignum_throw_alloc(local, left, ret);
		return;
	}
	count = value / BIGNUM_FULLBIT;
	value = value % BIGNUM_FULLBIT;
	GetSizeBignum(left, &size);
	GetRootDataBignum(left, &root, &data1);
	if (size == 1 && data1[0] == 0) {
		bignum_zero_alloc(local, ret);
		return;
	}

	dsize = size + count;
	if (value) {
		alloc_bignum(local, &pos, dsize + 1);
		GetRootDataBignum(pos, &root, &data2);
		SetSizeBignum(pos, dsize + 1);
		bigset(data2, 0, count);
		next = 0;
		for (i = 0; i < size; i++) {
			temp = data1[i];
			data2[i + count] = CUTB(temp << value) | next;
			next = temp >> (BIGNUM_FULLBIT - value);
		}
		data2[i + count] = next;
		sizepress_bignum(pos);
	}
	else {
		alloc_bignum(local, &pos, dsize);
		GetRootDataBignum(pos, &root, &data2);
		SetSizeBignum(pos, dsize);
		bigcpy(data2 + count, data1, size);
		bigset(data2, 0, count);
	}

	SetSignBignum(pos, RefSignBignum(left));
	*ret = pos;
}

void division2_bigdata_alloc(LocalRoot local, addr *ret, addr left)
{
	addr pos, root;
	bigtype *data1, *data2, carry, value;
	size_t size, i, index;

	GetSizeBignum(left, &size);
	GetRootDataBignum(left, &root, &data1);
	if (size == 1) {
		bignum_value_alloc(local, ret, SignPlus, data1[0] >> 1);
		return;
	}
	if (data1[size - 1] == 1) {
		carry = 1;
		size--;
	}
	else {
		carry = 0;
	}
	alloc_bignum(local, &pos, size);
	SetSizeBignum(pos, size);
	GetRootDataBignum(pos, &root, &data2);
	for (i = 0; i < size; i++) {
		index = size - i - 1ULL;
		value = data1[index];
		data2[index] = (carry << (BIGNUM_FULLBIT - 1ULL)) | (value >> 1ULL);
		carry = (value & 1ULL);
	}
	*ret = pos;
}


/*
 *  output
 */
bigtype letdiv_half_bigdata(addr left, bigtype right)
{
	addr root;
	bigtype *data, carry;
	size_t size, i, index;

	Check(HIGHVALUE(right), "right size error");
	GetSizeBignum(left, &size);
	GetRootDataBignum(left, &root, &data);

	/* single */
	if (size == 1) {
		carry = data[0] % right;
		data[0] /= right;
		return carry;
	}

	/* multiple */
	carry = 0;
	index = size - 1;
	for (i = 0; i < size; i++) {
		divcarry4_half(&data[index], data[index], right, &carry);
		index--;
	}
	sizepress_bignum(left);

	return carry;
}

