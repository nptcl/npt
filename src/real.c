#include "bignum.h"
#include "bignum_object.h"
#include "condition.h"
#include "float_object.h"
#include "ratio.h"
#include "rational.h"
#include "typedef.h"

_g int floatp(addr pos)
{
	enum LISPTYPE type = GetType(pos);
	return type == LISPTYPE_SINGLE_FLOAT
		|| type == LISPTYPE_DOUBLE_FLOAT
		|| type == LISPTYPE_LONG_FLOAT
		|| type == LISPTYPE_SHORT_FLOAT;
}

_g int realp(addr pos)
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

_g void real_result_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	if (floatp(pos))
		float_result_local(local, pos, ret);
	else
		rational_result_local(local, pos, ret);
}

_g void real_result_heap(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	if (floatp(pos))
		float_result_heap(pos, ret);
	else
		rational_result_heap(local, pos, ret);
}

_g void real_throw_alloc(LocalRoot local, addr pos, addr *ret)
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
			TypeError(pos, REAL);
			break;
	}
}

_g void real_throw_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	real_throw_alloc(local, pos, ret);
}

_g void real_throw_heap(addr pos, addr *ret)
{
	real_throw_alloc(NULL, pos, ret);
}

_g void real_copy_alloc(LocalRoot local, addr pos, addr *ret)
{
	if (floatp(pos))
		float_copy_alloc(local, pos, ret);
	else
		rational_copy_alloc(local, pos, ret);
}

_g void real_copy_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	real_copy_alloc(local, pos, ret);
}

_g void real_copy_heap(addr pos, addr *ret)
{
	real_copy_alloc(NULL, pos, ret);
}

_g double_float cast_double_float_unsafe(addr value)
{
	switch (GetType(value)) {
		case LISPTYPE_FIXNUM:
			return (double_float)RefFixnum(value);

		case LISPTYPE_BIGNUM:
			return double_float_bignum(value);

		case LISPTYPE_RATIO:
			return double_float_ratio(value);

		case LISPTYPE_SINGLE_FLOAT:
			return (double_float)RefSingleFloat(value);

		case LISPTYPE_DOUBLE_FLOAT:
			return RefDoubleFloat(value);

		case LISPTYPE_LONG_FLOAT:
			return (double_float)RefLongFloat(value);

		default:
			TypeError(value, REAL);
			return 0.0;
	}
}

