#include "bignum.h"
#include "bignum_equal.h"
#include "bignum_object.h"
#include "cmpl.h"
#include "cmpl_math.h"
#include "condition.h"
#include "float_object.h"
#include "math_type.h"
#include "number.h"
#include "ratio.h"
#include "rational.h"
#include "real.h"
#include "typedef.h"

int numberp(addr pos)
{
	enum LISPTYPE type = GetType(pos);
	return type == LISPTYPE_FIXNUM
		|| type == LISPTYPE_BIGNUM
		|| type == LISPTYPE_RATIO
		|| type == LISPTYPE_SINGLE_FLOAT
		|| type == LISPTYPE_DOUBLE_FLOAT
		|| type == LISPTYPE_LONG_FLOAT
		|| type == LISPTYPE_COMPLEX
		|| type == LISPTYPE_SHORT_FLOAT;
}

int number_result_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	if (complexp(pos))
		return complex_result_local_(local, pos, ret);
	else
		return rational_result_local_(local, pos, ret);
}

int number_result_heap_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	if (complexp(pos))
		return complex_result_heap_(local, pos, ret);
	else
		return rational_result_heap_(local, pos, ret);
}

int number_throw_alloc_(LocalRoot local, addr pos, addr *ret)
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

		case LISPTYPE_COMPLEX:
			return complex_throw_alloc_(local, pos, ret);

		default:
			*ret = Nil;
			return TypeError_(pos, REAL);
	}

	return 0;
}

int number_throw_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	return number_throw_alloc_(local, pos, ret);
}

int number_throw_heap_(addr pos, addr *ret)
{
	return number_throw_alloc_(NULL, pos, ret);
}

int number_copy_alloc_(LocalRoot local, addr pos, addr *ret)
{
	if (complexp(pos))
		return complex_copy_alloc_(local, pos, ret);
	else
		return real_copy_alloc_(local, pos, ret);
}

int number_copy_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	return number_copy_alloc_(local, pos, ret);
}

int number_copy_heap_(addr pos, addr *ret)
{
	return number_copy_alloc_(NULL, pos, ret);
}


/*
 *  abs
 */
int abs_number_common_(addr left, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			abs_fixnum_integer_common(left, ret);
			break;

		case LISPTYPE_BIGNUM:
			abs_bignum_integer_common(left, ret);
			break;

		case LISPTYPE_RATIO:
			abs_ratio_heap(left, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			abs_floats_heap(left, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			abs_floatd_heap(left, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			abs_floatl_heap(left, ret);
			break;

		case LISPTYPE_COMPLEX:
			return abs_complex_common_(left, ret);

		default:
			return TypeError_(left, NUMBER);
	}

	return 0;
}


/*
 *  signum
 */
static void signum_single_common(addr pos, addr *ret)
{
	single_float value;

	CheckType(pos, LISPTYPE_SINGLE_FLOAT);
	GetSingleFloat(pos, &value);
	if (value == 0.0f)
		single_float_heap(ret, 0.0f);
	else
		single_float_heap(ret, (value < 0.0f)? -1.0f: 1.0f);
}

static void signum_double_common(addr pos, addr *ret)
{
	double_float value;

	CheckType(pos, LISPTYPE_DOUBLE_FLOAT);
	GetDoubleFloat(pos, &value);
	if (value == 0.0)
		double_float_heap(ret, 0.0);
	else
		double_float_heap(ret, (value < 0.0)? -1.0: 1.0);
}

static void signum_long_common(addr pos, addr *ret)
{
	long_float value;

	CheckType(pos, LISPTYPE_LONG_FLOAT);
	GetLongFloat(pos, &value);
	if (value == 0.0L)
		long_float_heap(ret, 0.0L);
	else
		long_float_heap(ret, (value < 0.0L)? -1.0L: 1.0L);
}

static void signum_fixnum_common(addr pos, addr *ret)
{
	fixnum value;

	CheckType(pos, LISPTYPE_FIXNUM);
	GetFixnum(pos, &value);
	if (value == 0)
		fixnum_heap(ret, 0);
	else
		fixnum_heap(ret, (value < 0)? -1: 1);
}

static void signum_bignum_common(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	if (zerop_bignum(pos))
		fixnum_heap(ret, 0);
	else
		fixnum_heap(ret, minusp_bignum(pos)? -1: 1);
}

static void signum_ratio_common(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_RATIO);
	if (zerop_ratio(pos))
		fixnum_heap(ret, 0);
	else
		fixnum_heap(ret, minusp_ratio(pos)? -1: 1);
}

int signum_number_common_(addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			signum_single_common(pos, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			signum_double_common(pos, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			signum_long_common(pos, ret);
			break;

		case LISPTYPE_COMPLEX:
			return signum_complex_common_(pos, ret);

		case LISPTYPE_FIXNUM:
			signum_fixnum_common(pos, ret);
			break;

		case LISPTYPE_BIGNUM:
			signum_bignum_common(pos, ret);
			break;

		case LISPTYPE_RATIO:
			signum_ratio_common(pos, ret);
			break;

		default:
			*ret = Nil;
			return TypeError_(pos, NUMBER);
	}

	return 0;
}


/*
 *  sqrt
 */
static int sqrt_single_common_(struct mathreal2_struct *ptr, addr *ret)
{
	single_float real, imag;

	real = ptr->v.s.a;
	imag = ptr->v.s.b;
	if (0.0f <= real && imag == 0.0f) {
		return single_float_check_heap_(ret, sqrtf(real));
	}
	else {
		csqrt_f(real, imag, &real, &imag);
		return complex_single_heap_(ret, real, imag);
	}
}

static int sqrt_double_common_(struct mathreal2_struct *ptr, addr *ret)
{
	double_float real, imag;

	real = ptr->v.d.a;
	imag = ptr->v.d.b;
	if (0.0 <= real && imag == 0.0) {
		return double_float_check_heap_(ret, sqrt(real));
	}
	else {
		csqrt_d(real, imag, &real, &imag);
		return complex_double_heap_(ret, real, imag);
	}
}

static int sqrt_long_common_(struct mathreal2_struct *ptr, addr *ret)
{
	long_float real, imag;

	real = ptr->v.l.a;
	imag = ptr->v.l.b;
	if (0.0L <= real && imag == 0.0L) {
		return long_float_check_heap_(ret, sqrtl(real));
	}
	else {
		csqrt_l(real, imag, &real, &imag);
		return complex_long_heap_(ret, real, imag);
	}
}

int sqrt_number_common_(addr pos, addr *ret)
{
	enum MathType type;
	struct mathreal2_struct str;

	Return(getmathcomplex1_sqrt_(&str, pos, &type));
	switch (type) {
		case MathType_single:
			return sqrt_single_common_(&str, ret);

		case MathType_double:
			return sqrt_double_common_(&str, ret);

		case MathType_long:
			return sqrt_long_common_(&str, ret);

		case MathType_complex:
		case MathType_rational:
		case MathType_error:
		default:
			*ret = Nil;
			return TypeError_(pos, NUMBER);
	}
}

