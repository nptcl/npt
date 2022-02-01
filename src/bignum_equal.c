#include "bignum_data.h"
#include "bignum_equal.h"
#include "bignum_object.h"
#include "bignum.h"
#include "memory.h"
#include "typedef.h"

/*
 *  check
 */
int zerop_or_plusp_bignum(addr pos)
{
	return zerop_bignum(pos) || (IsPlus(RefSignBignum(pos)));
}

int plusp_bignum(addr pos)
{
	return (! zerop_bignum(pos)) && (IsPlus(RefSignBignum(pos)));
}

int minusp_bignum(addr pos)
{
	return (! zerop_bignum(pos)) && (IsMinus(RefSignBignum(pos)));
}

int zerop_bignum(addr pos)
{
	addr root;

	Check(GetType(pos) != LISPTYPE_BIGNUM, "type error");
	GetRootBignum(pos, &root);

	return RefSizeBignum(pos) == 1 && PtrDataBignum(root)[0] == 0;
}

int evenp_bignum(addr pos)
{
	fixed value;
	getfixed_bignum(pos, 0, &value);
	return (value & 1) == 0;
}

int oddp_bignum(addr pos)
{
	fixed value;
	getfixed_bignum(pos, 0, &value);
	return (value & 1) != 0;
}


/*
 *  equal
 */
int equal_fb_real(addr left, addr right)
{
	int sign1, sign2;
	fixed check1, check2;
	fixnum value;
	addr root;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	if (1 < RefSizeBignum(right))
		return 0;
	GetFixnum(left, &value);
	GetRootBignum(right, &root);
	check2 = PtrDataBignum(root)[0];
	if (value == 0 && check2 == 0)
		return 1;
	GetSignBignum(right, &sign2);
	castfixed(value, &sign1, &check1);

	return sign1 == sign2 && check1 == check2;
}

int equal_bb_real(addr left, addr right)
{
	addr root;
	size_t size1, size2;
	fixed check1, check2;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	GetSizeBignum(left, &size1);
	GetSizeBignum(right, &size2);
	if (size1 == 1 && size2 == 1) {
		GetRootBignum(left, &root);
		check1 = PtrDataBignum(root)[0];
		GetRootBignum(right, &root);
		check2 = PtrDataBignum(root)[0];
		return (check1 == 0 && check2 == 0)
			|| ((RefSignBignum(left) == RefSignBignum(right)) && (check1 == check2));
	}

	if (RefSignBignum(left) != RefSignBignum(right))
		return 0;
	if (size1 != size2)
		return 0;
	GetRootBignum(left, &left);
	GetRootBignum(right, &right);
	return bigcmp(PtrDataBignum(left), PtrDataBignum(right), size1) == 0;
}

int equal_nosign_bignum(addr left, addr right)
{
	size_t size1, size2;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	GetSizeBignum(left, &size1);
	GetSizeBignum(right, &size2);
	if (size1 != size2)
		return 0;
	GetRootBignum(left, &left);
	GetRootBignum(right, &right);
	return bigcmp(PtrDataBignum(left), PtrDataBignum(right), size1) == 0;
}

int equal_value_nosign_bignum(addr left, fixed value)
{
	if (RefSizeBignum(left) != 1)
		return 0;
	GetRootBignum(left, &left);
	return PtrDataBignum(left)[0] == value;
}

int equal_value_bignum(addr left, int sign1, fixed value1)
{
	int sign2;
	addr root;
	fixed value2;

	if (RefSizeBignum(left) != 1)
		return 0;
	GetRootBignum(left, &root);
	value2 = PtrDataBignum(root)[0];
	if (value1 == 0 && value2 == 0)
		return 1;
	GetSignBignum(left, &sign2);
	if (sign1 != sign2)
		return 0;

	return value1 == value2;
}

int equal_value2_nosign_bignum(addr left, fixed high, fixed low)
{
	addr root;
	fixed *data;

	if (high == 0)
		return equal_value_nosign_bignum(left, low);
	if (RefSizeBignum(left) != 2)
		return 0;
	GetRootDataBignum(left, &root, &data);

	return data[0] == low && data[1] == high;
}

int equal_value2_bignum(addr left, int sign1, fixed high, fixed low)
{
	int sign2;
	addr root;
	fixed *data;

	if (high == 0)
		return equal_value_bignum(left, sign1, low);
	if (RefSizeBignum(left) != 2)
		return 0;
	GetRootDataBignum(left, &root, &data);
	if (data[0] != low || data[1] != high)
		return 0;
	GetSignBignum(left, &sign2);

	return sign1 == sign2;
}


/*
 *  compare
 */
static int compare_fixed(int sign1, fixed check1, int sign2, fixed check2)
{
	if (check1 == 0 && check2 == 0)
		return 0;
	if (sign1 != sign2)
		return IsPlus(sign1)? 1: -1;
	if (IsPlus(sign1)) {
		if (check1 < check2)
			return -1;
		if (check1 > check2)
			return 1;
	}
	else {
		if (check1 < check2)
			return 1;
		if (check1 > check2)
			return -1;
	}

	return 0;
}

int compare_value_bignum(fixnum left, addr right)
{
	int sign1, sign2;
	fixed check1, check2;

	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	if (1 < RefSizeBignum(right))
		return IsPlus(RefSignBignum(right))? -1: 1;

	castfixed(left, &sign1, &check1);
	GetSignBignum(right, &sign2);
	GetRootBignum(right, &right);
	check2 = PtrDataBignum(right)[0];

	return compare_fixed(sign1, check1, sign2, check2);
}

int compare_bignum_value(addr value, fixnum right)
{
	return -compare_value_bignum(right, value);
}

int compare_fb_real(addr left, addr right)
{
	int sign1, sign2;
	fixed check1, check2;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	if (1 < RefSizeBignum(right))
		return IsPlus(RefSignBignum(right))? -1: 1;

	castfixed_fixnum(left, &sign1, &check1);
	GetSignBignum(right, &sign2);
	GetRootBignum(right, &right);
	check2 = PtrDataBignum(right)[0];

	return compare_fixed(sign1, check1, sign2, check2);
}

int compare_bf_real(addr left, addr right)
{
	return -(compare_fb_real(right, left));
}

int compare_bb_real(addr left, addr right)
{
	int sign1, sign2;

	if (zerop_bignum(left)) {
		if (zerop_bignum(right))
			return 0;
		GetSignBignum(right, &sign2);
		return IsPlus(sign2)? -1: 1;
	}
	if (zerop_bignum(right)) {
		GetSignBignum(left, &sign1);
		return IsPlus(sign1)? 1: -1;
	}

	GetSignBignum(left, &sign1);
	GetSignBignum(right, &sign2);
	if (sign1 != sign2)
		return IsPlus(sign1)? 1: -1;

	sign2 = compare_bigdata(left, right);
	return IsPlus(sign1)? sign2: -sign2;
}

int compare_bs_real_(addr left, addr right, int *ret)
{
	float value1, value2;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	Return(single_float_bignum_(left, &value1));
	GetSingleFloat(right, &value2);
	if (value1 < value2)
		return Result(ret, -1);
	if (value1 > value2)
		return Result(ret, 1);

	return Result(ret, 0);
}

int compare_bd_real_(addr left, addr right, int *ret)
{
	double_float value1, value2;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	Return(double_float_bignum_(left, &value1));
	GetDoubleFloat(right, &value2);
	if (value1 < value2)
		return Result(ret, -1);
	if (value1 > value2)
		return Result(ret, 1);

	return Result(ret, 0);
}

int compare_bl_real_(addr left, addr right, int *ret)
{
	long_float value1, value2;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	Return(long_float_bignum_(left, &value1));
	GetLongFloat(right, &value2);
	if (value1 < value2)
		return Result(ret, -1);
	if (value1 > value2)
		return Result(ret, 1);

	return Result(ret, 0);
}

int compare_sb_real_(addr left, addr right, int *ret)
{
	int check;
	Return(compare_bs_real_(right, left, &check));
	return Result(ret, -check);
}

int compare_db_real_(addr left, addr right, int *ret)
{
	int check;
	Return(compare_bd_real_(right, left, &check));
	return Result(ret, -check);
}

int compare_lb_real_(addr left, addr right, int *ret)
{
	int check;
	Return(compare_bl_real_(right, left, &check));
	return Result(ret, -check);
}

/* compare byte */
int fixnum_unsigned_byte_p(addr value, size_t size)
{
	fixnum check;

	Check(size == 0, "unsigned-byte error");
	GetFixnum(value, &check);
	if (check < 0)
		return 0;
	if (BIGNUM_FULLBIT <= size)
		return 1;

	return ((fixed)check) < (1ULL << ((fixed)size));
}

int bignum_unsigned_byte_p(addr value, size_t size)
{
	addr root;
	fixed *data, left, right;
	size_t m, n;

	Check(size == 0, "unsigned-byte error");
	if (zerop_bignum(value))
		return 1;
	if (IsMinus(RefSignBignum(value)))
		return 0;
	n = size / BIGNUM_FULLBIT;
	GetSizeBignum(value, &m);
	m--;
	if (m < n)
		return 1;
	if (n < m)
		return 0;
	GetRootDataBignum(value, &root, &data);
	left = data[n];
	m = size % BIGNUM_FULLBIT;
	right = (fixed)(1ULL << m);

	return left < right;
}

int fixnum_signed_byte_p(addr value, size_t size)
{
	fixnum check;

	Check(size == 0, "signed-byte error");
	if (BIGNUM_FULLBIT <= size)
		return 1;
	GetFixnum(value, &check);
	size = 1ULL << ((fixed)(size - 1ULL));
	if (0 <= check)
		return ((fixed)check) < size;
	else
		return ((fixed)-check) <= size;
}

int bignum_signed_byte_p(addr value, size_t size)
{
	addr root;
	fixed *data, left, right;
	size_t m, n;

	Check(size == 0, "signed-byte error");
	if (zerop_bignum(value))
		return 1;
	if (IsPlus(RefSignBignum(value))) {
		if (size <= 1)
			return 0;
		return bignum_unsigned_byte_p(value, size - 1);
	}
	size--;
	n = size / BIGNUM_FULLBIT;
	GetSizeBignum(value, &m);
	m--;
	if (m < n)
		return 1;
	if (n < m)
		return 0;
	GetRootDataBignum(value, &root, &data);
	left = data[n];
	m = size % BIGNUM_FULLBIT;
	right = (fixed)(1ULL << m);
	if (left < right)
		return 1;
	if (right < left)
		return 0;
	if (n == 0)
		return 1;
	for (m = 0; m < n; m++) {
		if (data[m])
			return 0;
	}

	return 1;
}

