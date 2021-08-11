#include <float.h>
#include <math.h>
#include "bignum.h"
#include "cmpl.h"
#include "condition.h"
#include "integer.h"
#include "object.h"
#include "ratio.h"
#include "real_common.h"
#include "real_floor.h"

/*
 *  constant
 */
#ifdef FLT_TRUE_MIN
static void single_float_least_positive(addr *ret)
{
	single_float_heap(ret, FLT_TRUE_MIN);
}
void double_float_least_positive(addr *ret)
{
	double_float_heap(ret, DBL_TRUE_MIN);
}
void long_float_least_positive(addr *ret)
{
	long_float_heap(ret, LDBL_TRUE_MIN);
}
static void single_float_least_negative(addr *ret)
{
	single_float_heap(ret, -FLT_TRUE_MIN);
}
void double_float_least_negative(addr *ret)
{
	double_float_heap(ret, -DBL_TRUE_MIN);
}
void long_float_least_negative(addr *ret)
{
	long_float_heap(ret, -LDBL_TRUE_MIN);
}
#else
static void single_float_least_positive(addr *ret)
{
	const static single_float v = 1.40129846E-45F;
	single_float_heap(ret, v == 0.0f? FLT_MIN: v);
}
void double_float_least_positive(addr *ret)
{
	const static double_float v = 4.9406564584124654E-324;
	double_float_heap(ret, v == 0.0? DBL_MIN: v);
}
void long_float_least_positive(addr *ret)
{
	const static long_float v = 3.6451995318824746025E-4951L;
	long_float_heap(ret, v == 0.0L? LDBL_MIN: v);
}
static void single_float_least_negative(addr *ret)
{
	const static single_float v = 1.40129846E-45F;
	single_float_heap(ret, v == 0.0f? -FLT_MIN: -v);
}
void double_float_least_negative(addr *ret)
{
	const static double_float v = 4.9406564584124654E-324;
	double_float_heap(ret, v == 0.0? -DBL_MIN: -v);
}
void long_float_least_negative(addr *ret)
{
	const static long_float v = 3.6451995318824746025E-4951L;
	long_float_heap(ret, v == 0.0L? -LDBL_MIN: -v);
}
#endif

static void single_float_least_positive_normalized(addr *ret)
{
	single_float_heap(ret, FLT_MIN);
}
void double_float_least_positive_normalized(addr *ret)
{
	double_float_heap(ret, DBL_MIN);
}
void long_float_least_positive_normalized(addr *ret)
{
	long_float_heap(ret, LDBL_MIN);
}
static void single_float_least_negative_normalized(addr *ret)
{
	single_float_heap(ret, -FLT_MIN);
}
void double_float_least_negative_normalized(addr *ret)
{
	double_float_heap(ret, -DBL_MIN);
}
void long_float_least_negative_normalized(addr *ret)
{
	long_float_heap(ret, -LDBL_MIN);
}

static void single_float_epsilon(addr *ret)
{
	single_float_heap(ret, FLT_EPSILON * 0.5f);
}

static void single_float_negative_epsilon(addr *ret)
{
	single_float_heap(ret, FLT_EPSILON * 0.25f);
}

void double_float_epsilon(addr *ret)
{
	double_float_heap(ret, DBL_EPSILON * 0.5);
}

void double_float_negative_epsilon(addr *ret)
{
	double_float_heap(ret, DBL_EPSILON * 0.25);
}

void long_float_epsilon(addr *ret)
{
	long_float_heap(ret, LDBL_EPSILON * 0.5L);
}

void long_float_negative_epsilon(addr *ret)
{
	long_float_heap(ret, LDBL_EPSILON * 0.25L);
}

static void build_index_max(void)
{
	addr pos;

	make_indexmax_alloc(NULL, &pos);
	SetConstant(CONSTANT_INDEX_MAX, pos);
}

static void build_float_max(void)
{
	addr value;

	single_float_heap(&value, FLT_MAX);
	SetConstant(CONSTANT_SINGLE_FLOAT_MOST_POSITIVE, value);
	single_float_heap(&value, -FLT_MAX);
	SetConstant(CONSTANT_SINGLE_FLOAT_MOST_NEGATIVE, value);
}

static void build_float_min(void)
{
	addr value;

	/* positive */
	single_float_least_positive(&value);
	SetConstant(CONSTANT_SINGLE_FLOAT_LEAST_POSITIVE, value);
	single_float_least_positive_normalized(&value);
	SetConstant(CONSTANT_SINGLE_FLOAT_LEAST_POSITIVE_NORMALIZED, value);
	/* negative */
	single_float_least_negative(&value);
	SetConstant(CONSTANT_SINGLE_FLOAT_LEAST_NEGATIVE, value);
	single_float_least_negative_normalized(&value);
	SetConstant(CONSTANT_SINGLE_FLOAT_LEAST_NEGATIVE_NORMALIZED, value);
}

static void build_float_epsilon(void)
{
	addr value;

	/* positive */
	single_float_epsilon(&value);
	SetConstant(CONSTANT_SINGLE_FLOAT_EPSILON, value);
	/* negative */
	single_float_negative_epsilon(&value);
	SetConstant(CONSTANT_SINGLE_FLOAT_NEGATIVE_EPSILON, value);
}

void build_real_common(void)
{
	build_index_max();
	build_float_max();
	build_float_min();
	build_float_epsilon();
}


/*
 *  common-lisp
 */
static int unbound_float_cast_(addr *ret, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			single_float_fixnum_heap(ret, pos);
			return 0;

		case LISPTYPE_BIGNUM:
			return single_float_bignum_heap_(ret, pos);

		case LISPTYPE_RATIO:
			return single_float_ratio_heap_(ret, pos);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
		case LISPTYPE_DOUBLE_FLOAT:
		case LISPTYPE_LONG_FLOAT:
			return Result(ret, pos);

		default:
			*ret = Nil;
			return TypeError_(pos, REAL);
	}
}

static int single_from_double_cast_(addr *ret, addr pos)
{
	double_float v;

	CheckType(pos, LISPTYPE_DOUBLE_FLOAT);
	GetDoubleFloat(pos, &v);
	v = fabs(v);
	if (v < FLT_MIN) {
		*ret = Nil;
		return call_float_underflow_va_(NULL, CONSTANT_COMMON_FLOAT, pos, NULL);
	}
	if (FLT_MAX < v) {
		*ret = Nil;
		return call_float_overflow_va_(NULL, CONSTANT_COMMON_FLOAT, pos, NULL);
	}
	single_float_heap(ret, (single_float)v);

	return 0;
}

static int single_from_long_cast_(addr *ret, addr pos)
{
	long_float v;

	CheckType(pos, LISPTYPE_LONG_FLOAT);
	GetLongFloat(pos, &v);
	v = fabsl(v);
	if (v < FLT_MIN) {
		*ret = Nil;
		return call_float_underflow_va_(NULL, CONSTANT_COMMON_FLOAT, pos, NULL);
	}
	if (FLT_MAX < v) {
		*ret = Nil;
		return call_float_overflow_va_(NULL, CONSTANT_COMMON_FLOAT, pos, NULL);
	}
	single_float_heap(ret, (single_float)v);

	return 0;
}

static int double_from_long_cast_(addr *ret, addr pos)
{
	long_float v;

	CheckType(pos, LISPTYPE_LONG_FLOAT);
	GetLongFloat(pos, &v);
	v = fabsl(v);
	if (v < DBL_MIN) {
		*ret = Nil;
		return call_float_underflow_va_(NULL, CONSTANT_COMMON_FLOAT, pos, NULL);
	}
	if (DBL_MAX < v) {
		*ret = Nil;
		return call_float_overflow_va_(NULL, CONSTANT_COMMON_FLOAT, pos, NULL);
	}
	double_float_heap(ret, (double_float)v);

	return 0;
}

static int single_float_cast_(addr *ret, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			single_float_fixnum_heap(ret, pos);
			break;

		case LISPTYPE_BIGNUM:
			return single_float_bignum_heap_(ret, pos);

		case LISPTYPE_RATIO:
			return single_float_ratio_heap_(ret, pos);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			*ret = pos;
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			return single_from_double_cast_(ret, pos);

		case LISPTYPE_LONG_FLOAT:
			return single_from_long_cast_(ret, pos);

		default:
			*ret = Nil;
			return TypeError_(pos, REAL);
	}

	return 0;
}

static int double_float_cast_(addr *ret, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			double_float_fixnum_heap(ret, pos);
			break;

		case LISPTYPE_BIGNUM:
			return double_float_bignum_heap_(ret, pos);

		case LISPTYPE_RATIO:
			return double_float_ratio_heap_(ret, pos);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			double_float_heap(ret, (double_float)RefSingleFloat(pos));
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = pos;
			break;

		case LISPTYPE_LONG_FLOAT:
			return double_from_long_cast_(ret, pos);

		default:
			*ret = Nil;
			return TypeError_(pos, REAL);
	}

	return 0;
}

static int long_float_cast_(addr *ret, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			long_float_fixnum_heap(ret, pos);
			break;

		case LISPTYPE_BIGNUM:
			return long_float_bignum_heap_(ret, pos);

		case LISPTYPE_RATIO:
			return long_float_ratio_heap_(ret, pos);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			long_float_heap(ret, (long_float)RefSingleFloat(pos));
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			long_float_heap(ret, (long_float)RefDoubleFloat(pos));
			break;

		case LISPTYPE_LONG_FLOAT:
			*ret = pos;
			break;

		default:
			*ret = Nil;
			return TypeError_(pos, REAL);
	}

	return 0;
}

int float_common_(addr *ret, addr pos, addr type)
{
	if (type == Unbound)
		return unbound_float_cast_(ret, pos);

	switch (GetType(type)) {
		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return single_float_cast_(ret, pos);

		case LISPTYPE_DOUBLE_FLOAT:
			return double_float_cast_(ret, pos);

		case LISPTYPE_LONG_FLOAT:
			return long_float_cast_(ret, pos);

		default:
			*ret = Nil;
			return TypeError_(type, FLOAT);
	}
}

