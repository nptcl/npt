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
_g void rational_result_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	if (rationalp(pos))
		ratio_result_noreduction_local(local, pos, ret);
	else
		integer_result_local(local, pos, ret);
}

_g void rational_result_heap(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	if (rationalp(pos))
		ratio_result_noreduction_heap(local, pos, ret);
	else
		integer_result_heap(pos, ret);
}

_g void rational_throw_alloc(LocalRoot local, addr pos, addr *ret)
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
			TypeError(pos, RATIONAL);
			break;
	}
}

_g void rational_throw_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	rational_throw_alloc(local, pos, ret);
}

_g void rational_throw_heap(addr pos, addr *ret)
{
	rational_throw_alloc(NULL, pos, ret);
}

_g void rational_copy_alloc(LocalRoot local, addr pos, addr *ret)
{
	if (ratiop(pos))
		ratio_copy_alloc(local, ret, pos);
	else
		integer_copy_alloc(local, pos, ret);
}

_g void rational_copy_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	rational_copy_alloc(local, pos, ret);
}

_g void rational_copy_heap(addr pos, addr *ret)
{
	rational_copy_alloc(NULL, pos, ret);
}


/*
 *  float
 */
_g single_float single_float_rational(addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return single_float_fixnum(pos);

		case LISPTYPE_BIGNUM:
			return single_float_bignum(pos);

		case LISPTYPE_RATIO:
			return single_float_ratio(pos);

		default:
			TypeError(pos, RATIONAL);
			return 0.0f;
	}
}

_g double_float double_float_rational(addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return double_float_fixnum(pos);

		case LISPTYPE_BIGNUM:
			return double_float_bignum(pos);

		case LISPTYPE_RATIO:
			return double_float_ratio(pos);

		default:
			TypeError(pos, RATIONAL);
			return 0.0;
	}
}

_g long_float long_float_rational(addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return long_float_fixnum(pos);

		case LISPTYPE_BIGNUM:
			return long_float_bignum(pos);

		case LISPTYPE_RATIO:
			return long_float_ratio(pos);

		default:
			TypeError(pos, RATIONAL);
			return 0.0L;
	}
}


/*
 *  numerator
 */
_g void numerator_common(addr pos, addr *ret)
{
	int sign;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			fixnum_throw_heap(pos, ret);
			break;

		case LISPTYPE_BIGNUM:
			bignum_throw_heap(pos, ret);
			break;

		case LISPTYPE_RATIO:
			GetSignRatio(pos, &sign);
			GetNumerRatio(pos, &pos);
			bignum_copy_heap(&pos, pos);
			SetSignBignum(pos, sign);
			*ret = pos;
			break;

		default:
			TypeError(pos, RATIONAL);
			*ret = 0;
			return;
	}
}


/*
 *  denominator
 */
_g void denominator_common(addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
			fixnum_heap(ret, 1);
			break;

		case LISPTYPE_RATIO:
			GetDenomRatio(pos, &pos);
			bignum_copy_heap(ret, pos);
			break;

		default:
			TypeError(pos, RATIONAL);
			*ret = 0;
			return;
	}
}

