#include "bignum.h"
#include "bignum_object.h"
#include "condition.h"
#include "float_object.h"
#include "ratio.h"
#include "rational.h"
#include "real.h"
#include "real_common.h"
#include "typedef.h"

int floatp(addr pos)
{
	enum LISPTYPE type = GetType(pos);
	return type == LISPTYPE_SINGLE_FLOAT
		|| type == LISPTYPE_DOUBLE_FLOAT
		|| type == LISPTYPE_LONG_FLOAT
		|| type == LISPTYPE_SHORT_FLOAT;
}

int realp(addr pos)
{
	enum LISPTYPE type = GetType(pos);
	return type == LISPTYPE_FIXNUM
		|| type == LISPTYPE_BIGNUM
		|| type == LISPTYPE_RATIO
		|| type == LISPTYPE_SINGLE_FLOAT
		|| type == LISPTYPE_DOUBLE_FLOAT
		|| type == LISPTYPE_LONG_FLOAT
		|| type == LISPTYPE_SHORT_FLOAT;
}

int real_result_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	if (floatp(pos))
		return float_result_local_(local, pos, ret);
	else
		return rational_result_local_(local, pos, ret);
}

int real_result_heap_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	if (floatp(pos))
		return float_result_heap_(pos, ret);
	else
		return rational_result_heap_(local, pos, ret);
}

int real_throw_alloc_(LocalRoot local, addr pos, addr *ret)
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

		case LISPTYPE_SINGLE_FLOAT:
			single_float_throw_alloc(local, pos, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			double_float_throw_alloc(local, pos, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			long_float_throw_alloc(local, pos, ret);
			break;

		default:
			*ret = Nil;
			return TypeError_(pos, REAL);
	}

	return 0;
}

int real_throw_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	return real_throw_alloc_(local, pos, ret);
}

int real_throw_heap_(addr pos, addr *ret)
{
	return real_throw_alloc_(NULL, pos, ret);
}

int real_copy_alloc_(LocalRoot local, addr pos, addr *ret)
{
	if (floatp(pos)) {
		float_copy_alloc(local, pos, ret);
		return 0;
	}
	else {
		return rational_copy_alloc_(local, pos, ret);
	}
}

int real_copy_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	return real_copy_alloc_(local, pos, ret);
}

int real_copy_heap_(addr pos, addr *ret)
{
	return real_copy_alloc_(NULL, pos, ret);
}

int cast_double_float_unsafe_(addr value, double_float *ret)
{
	switch (GetType(value)) {
		case LISPTYPE_FIXNUM:
			*ret = (double_float)RefFixnum(value);
			break;

		case LISPTYPE_BIGNUM:
			return double_float_bignum_(value, ret);

		case LISPTYPE_RATIO:
			return double_float_ratio_(value, ret);

		case LISPTYPE_SINGLE_FLOAT:
			*ret = (double_float)RefSingleFloat(value);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = RefDoubleFloat(value);
			break;

		case LISPTYPE_LONG_FLOAT:
			*ret = (double_float)RefLongFloat(value);
			break;

		default:
			*ret = 0.0;
			return TypeError_(value, REAL);
	}

	return 0;
}


/*
 *  build_real
 */
void build_real(void)
{
	build_real_common();
}

