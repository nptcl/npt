#include "bignum.h"
#include "bignum_object.h"
#include "condition.h"
#include "ratio.h"
#include "integer.h"
#include "typedef.h"

_g int rationalp(addr pos)
{
	enum LISPTYPE type = GetType(pos);
	return type == LISPTYPE_FIXNUM
		|| type == LISPTYPE_BIGNUM
		|| type == LISPTYPE_RATIO;
}

/*
 *  throw
 */
_g int rational_result_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	if (rationalp(pos)) {
		ratio_result_noreduction_local(local, pos, ret);
		return 0;
	}
	else {
		return integer_result_local_(local, pos, ret);
	}
}

_g int rational_result_heap_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	if (rationalp(pos)) {
		ratio_result_noreduction_heap(local, pos, ret);
		return 0;
	}
	else {
		return integer_result_heap_(pos, ret);
	}
}

_g int rational_throw_alloc_(LocalRoot local, addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			fixnum_throw_alloc(local, pos, ret);
			break;

		case LISPTYPE_BIGNUM:
			bignum_throw_alloc(local, pos, ret);
			break;

		case LISPTYPE_RATIO:
			ratio_throw_alloc(local, pos, ret);
			break;

		default:
			*ret = Nil;
			return TypeError_(pos, RATIONAL);
	}

	return 0;
}

_g int rational_throw_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	return rational_throw_alloc_(local, pos, ret);
}

_g int rational_throw_heap_(addr pos, addr *ret)
{
	return rational_throw_alloc_(NULL, pos, ret);
}

_g int rational_copy_alloc_(LocalRoot local, addr pos, addr *ret)
{
	if (ratiop(pos)) {
		ratio_copy_alloc(local, ret, pos);
		return 0;
	}
	else {
		return integer_copy_alloc_(local, pos, ret);
	}
}

_g int rational_copy_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	return rational_copy_alloc_(local, pos, ret);
}

_g int rational_copy_heap_(addr pos, addr *ret)
{
	return rational_copy_alloc_(NULL, pos, ret);
}


/*
 *  float
 */
_g int single_float_rational_(addr pos, single_float *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, single_float_fixnum(pos));

		case LISPTYPE_BIGNUM:
			return single_float_bignum_(pos, ret);

		case LISPTYPE_RATIO:
			return single_float_ratio_(pos, ret);

		default:
			*ret = 0.0f;
			return TypeError_(pos, RATIONAL);
	}
}

_g int double_float_rational_(addr pos, double_float *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, double_float_fixnum(pos));

		case LISPTYPE_BIGNUM:
			return double_float_bignum_(pos, ret);

		case LISPTYPE_RATIO:
			return double_float_ratio_(pos, ret);

		default:
			*ret = 0.0;
			return TypeError_(pos, RATIONAL);
	}
}

_g int long_float_rational_(addr pos, long_float *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, long_float_fixnum(pos));

		case LISPTYPE_BIGNUM:
			return long_float_bignum_(pos, ret);

		case LISPTYPE_RATIO:
			return long_float_ratio_(pos, ret);

		default:
			*ret = 0.0L;
			return TypeError_(pos, RATIONAL);
	}
}


/*
 *  numerator
 */
_g int numerator_common_(addr pos, addr *ret)
{
	int sign;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			fixnum_throw_heap(pos, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			bignum_throw_heap(pos, ret);
			return 0;

		case LISPTYPE_RATIO:
			GetSignRatio(pos, &sign);
			GetNumerRatio(pos, &pos);
			bignum_copy_heap(&pos, pos);
			SetSignBignum(pos, sign);
			return Result(ret, pos);

		default:
			*ret = Nil;
			return TypeError_(pos, RATIONAL);
	}
}


/*
 *  denominator
 */
_g int denominator_common_(addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
			fixnum_heap(ret, 1);
			return 0;

		case LISPTYPE_RATIO:
			GetDenomRatio(pos, &pos);
			bignum_copy_heap(ret, pos);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(pos, RATIONAL);
	}
}

