#include "bignum.h"
#include "bignum_data.h"
#include "bignum_equal.h"
#include "bignum_multi.h"
#include "bignum_object.h"
#include "bignum_plus.h"
#include "condition.h"
#include "float_object.h"
#include "format.h"
#include "math_type.h"
#include "object.h"
#include "ratio.h"
#include "ratio_plus.h"
#include "ratio_multi.h"
#include "real_division.h"
#include "real_floor.h"
#include "real_truncate.h"
#include <float.h>
#include <math.h>

static int division_by_zero_f_(constindex index, fixnum a, fixnum b)
{
	addr left, right;

	fixnum_heap(&left, a);
	fixnum_heap(&right, b);
	return call_division_by_zero_real2_(NULL, index, left, right);
}

static int division_by_zero_s_(constindex index, single_float a, single_float b)
{
	addr left, right;

	single_float_heap(&left, a);
	single_float_heap(&right, b);
	return call_division_by_zero_real2_(NULL, index, left, right);
}

static int division_by_zero_d_(constindex index, double_float a, double_float b)
{
	addr left, right;

	double_float_heap(&left, a);
	double_float_heap(&right, b);
	return call_division_by_zero_real2_(NULL, index, left, right);
}

static int division_by_zero_l_(constindex index, long_float a, long_float b)
{
	addr left, right;

	long_float_heap(&left, a);
	long_float_heap(&right, b);
	return call_division_by_zero_real2_(NULL, index, left, right);
}


/*
 *  floor
 */
int float_floor_s_(single_float a, single_float b, single_float *q, single_float *r)
{
	single_float m;

	if (b == 0.0f) {
		*q = *r = 0.0f;
		return division_by_zero_s_(CONSTANT_COMMON_FLOOR, a, b);
	}
	if (a == 0.0f) {
		*q = *r = 0.0f;
		return 0;
	}

	m = fmodf(a, b);
	if (m == 0.0f) {
		*q = a / b;
		*r = 0.0f;
		return 0;
	}
	if (0.0f < a) {
		if (b < 0.0f)
			goto floor2;
	}
	else if (0.0f < b) {
		goto floor2;
	}
	*q = floorf(a / b);
	*r = m;
	return 0;

floor2:
	*q = floorf(a / b);
	*r = b + m;
	return 0;
}

int float_floor_d_(double_float a, double_float b, double_float *q, double_float *r)
{
	double_float m;

	if (b == 0.0) {
		*q = *r = 0.0;
		return division_by_zero_d_(CONSTANT_COMMON_FLOOR, a, b);
	}
	if (a == 0.0) {
		*q = *r = 0.0;
		return 0;
	}

	m = fmod(a, b);
	if (m == 0.0) {
		*q = a / b;
		*r = 0.0;
		return 0;
	}
	if (0.0 < a) {
		if (b < 0.0)
			goto floor2;
	}
	else if (0.0 < b) {
		goto floor2;
	}
	*q = floor(a / b);
	*r = m;
	return 0;

floor2:
	*q = floor(a / b);
	*r = b + m;
	return 0;
}

int float_floor_l_(long_float a, long_float b, long_float *q, long_float *r)
{
	long_float m;

	if (b == 0.0L) {
		*q = *r = 0.0L;
		return division_by_zero_l_(CONSTANT_COMMON_FLOOR, a, b);
	}
	if (a == 0.0L) {
		*q = *r = 0.0L;
		return 0;
	}

	m = fmodl(a, b);
	if (m == 0.0L) {
		*q = a / b;
		*r = 0.0L;
		return 0;
	}
	if (0.0L < a) {
		if (b < 0.0L)
			goto floor2;
	}
	else if (0.0L < b) {
		goto floor2;
	}
	*q = floorl(a / b);
	*r = m;
	return 0;

floor2:
	*q = floorl(a / b);
	*r = b + m;
	return 0;
}

int float_floor1_s_(single_float a, single_float *q, single_float *r)
{
	single_float m;

	if (a == 0.0f) {
		*q = *r = 0.0f;
		return 0;
	}

	m = fmodf(a, 1.0f);
	if (m == 0.0f) {
		*q = a;
		*r = 0.0f;
		return 0;
	}
	if (0.0f < a)
		*r = m;
	else
		*r = 1.0f + m;

	*q = floorf(a);
	return 0;
}

int float_floor1_d_(double_float a, double_float *q, double_float *r)
{
	double_float m;

	if (a == 0.0) {
		*q = *r = 0.0;
		return 0;
	}

	m = fmod(a, 1.0);
	if (m == 0.0) {
		*q = a;
		*r = 0.0;
		return 0;
	}
	if (0.0 < a)
		*r = m;
	else
		*r = 1.0 + m;

	*q = floor(a);
	return 0;
}

int float_floor1_l_(long_float a, long_float *q, long_float *r)
{
	long_float m;

	if (a == 0.0L) {
		*q = *r = 0.0L;
		return 0;
	}

	m = fmodl(a, 1.0L);
	if (m == 0.0L) {
		*q = a;
		*r = 0.0L;
		return 0;
	}
	if (0.0L < a)
		*r = m;
	else
		*r = 1.0L + m;

	*q = floorl(a);
	return 0;
}


/*
 *  ceiling
 */
int float_ceiling_s_(single_float a, single_float b, single_float *q, single_float *r)
{
	single_float m;

	if (b == 0.0f) {
		*q = *r = 0.0f;
		return division_by_zero_s_(CONSTANT_COMMON_CEILING, a, b);
	}
	if (a == 0.0f) {
		*q = *r = 0.0f;
		return 0;
	}

	m = fmodf(a, b);
	if (m == 0.0f) {
		*q = a / b;
		*r = 0.0f;
		return 0;
	}
	if (0.0f < a) {
		if (b < 0.0f)
			goto ceiling2;
	}
	else if (0.0f < b) {
		goto ceiling2;
	}

	*q = ceilf(a / b);
	*r = m - b;
	return 0;

ceiling2:
	*q = ceilf(a / b);
	*r = m;
	return 0;
}

int float_ceiling_d_(double_float a, double_float b, double_float *q, double_float *r)
{
	double_float m;

	if (b == 0.0) {
		*q = *r = 0.0;
		return division_by_zero_d_(CONSTANT_COMMON_CEILING, a, b);
	}
	if (a == 0.0) {
		*q = *r = 0.0;
		return 0;
	}

	m = fmod(a, b);
	if (m == 0.0) {
		*q = a / b;
		*r = 0.0;
		return 0;
	}
	if (0.0 < a) {
		if (b < 0.0)
			goto ceiling2;
	}
	else if (0.0 < b) {
		goto ceiling2;
	}

	*q = ceil(a / b);
	*r = m - b;
	return 0;

ceiling2:
	*q = ceil(a / b);
	*r = m;
	return 0;
}

int float_ceiling_l_(long_float a, long_float b, long_float *q, long_float *r)
{
	long_float m;

	if (b == 0.0L) {
		*q = *r = 0.0L;
		return division_by_zero_l_(CONSTANT_COMMON_CEILING, a, b);
	}
	if (a == 0.0L) {
		*q = *r = 0.0L;
		return 0;
	}

	m = fmodl(a, b);
	if (m == 0.0L) {
		*q = a / b;
		*r = 0.0L;
		return 0;
	}
	if (0.0L < a) {
		if (b < 0.0L)
			goto ceiling2;
	}
	else if (0.0L < b) {
		goto ceiling2;
	}

	*q = ceill(a / b);
	*r = m - b;
	return 0;

ceiling2:
	*q = ceill(a / b);
	*r = m;
	return 0;
}

int float_ceiling1_s_(single_float a, single_float *q, single_float *r)
{
	single_float m;

	if (a == 0.0f) {
		*q = *r = 0.0f;
		return 0;
	}

	m = fmodf(a, 1.0f);
	if (m == 0.0f) {
		*q = a;
		*r = 0.0f;
		return 0;
	}
	if (0.0f < a)
		*r = m - 1.0f;
	else
		*r = m;

	*q = ceilf(a);
	return 0;
}

int float_ceiling1_d_(double_float a, double_float *q, double_float *r)
{
	double_float m;

	if (a == 0.0) {
		*q = *r = 0.0;
		return 0;
	}

	m = fmod(a, 1.0);
	if (m == 0.0) {
		*q = a;
		*r = 0.0;
		return 0;
	}
	if (0.0 < a)
		*r = m - 1.0;
	else
		*r = m;

	*q = ceil(a);
	return 0;
}

int float_ceiling1_l_(long_float a, long_float *q, long_float *r)
{
	long_float m;

	if (a == 0.0L) {
		*q = *r = 0.0L;
		return 0;
	}

	m = fmodl(a, 1.0L);
	if (m == 0.0L) {
		*q = a;
		*r = 0.0L;
		return 0;
	}
	if (0.0L < a)
		*r = m - 1.0L;
	else
		*r = m;

	*q = ceill(a);
	return 0;
}


/*
 *  truncate
 */
int float_truncate_s_(single_float a, single_float b,
		single_float *q, single_float *r)
{
	single_float m;

	if (b == 0.0f) {
		*q = *r = 0.0f;
		return division_by_zero_s_(CONSTANT_COMMON_TRUNCATE, a, b);
	}
	if (a == 0.0f) {
		*q = *r = 0.0f;
		return 0;
	}

	m = fmodf(a, b);
	if (m == 0.0f) {
		*q = a / b;
		*r = 0.0f;
		return 0;
	}
	*r = m;

	if (0.0f < a) {
		if (b < 0.0f)
			goto ceiling;
	}
	else if (0.0f < b) {
		goto ceiling;
	}
	*q = floorf(a / b);
	return 0;

ceiling:
	*q = ceilf(a / b);
	return 0;
}

int float_truncate_d_(double_float a, double_float b,
		double_float *q, double_float *r)
{
	double_float m;

	if (b == 0.0) {
		*q = *r = 0.0;
		return division_by_zero_d_(CONSTANT_COMMON_TRUNCATE, a, b);
	}
	if (a == 0.0) {
		*q = *r = 0.0;
		return 0;
	}

	m = fmod(a, b);
	if (m == 0.0) {
		*q = a / b;
		*r = 0.0;
		return 0;
	}
	*r = m;

	if (0.0 < a) {
		if (b < 0.0)
			goto ceiling;
	}
	else if (0.0 < b) {
		goto ceiling;
	}
	*q = floor(a / b);
	return 0;

ceiling:
	*q = ceil(a / b);
	return 0;
}

int float_truncate_l_(long_float a, long_float b,
		long_float *q, long_float *r)
{
	long_float m;

	if (b == 0.0L) {
		*q = *r = 0.0L;
		return division_by_zero_l_(CONSTANT_COMMON_TRUNCATE, a, b);
	}
	if (a == 0.0L) {
		*q = *r = 0.0L;
		return 0;
	}

	m = fmodl(a, b);
	if (m == 0.0L) {
		*q = a / b;
		*r = 0.0L;
		return 0;
	}
	*r = m;

	if (0.0L < a) {
		if (b < 0.0L)
			goto ceiling;
	}
	else if (0.0L < b) {
		goto ceiling;
	}
	*q = floorl(a / b);
	return 0;

ceiling:
	*q = ceill(a / b);
	return 0;
}

int float_truncate1_s_(single_float a, single_float *q, single_float *r)
{
	single_float m;

	if (a == 0.0f) {
		*q = *r = 0.0f;
		return 0;
	}

	m = fmodf(a, 1.0f);
	if (m == 0.0f) {
		*q = a;
		*r = 0.0f;
		return 0;
	}
	*q = (0.0f < a)? floorf(a): ceilf(a);
	*r = m;
	return 0;
}

int float_truncate1_d_(double_float a, double_float *q, double_float *r)
{
	double_float m;

	if (a == 0.0) {
		*q = *r = 0.0;
		return 0;
	}

	m = fmod(a, 1.0);
	if (m == 0.0) {
		*q = a;
		*r = 0.0;
		return 0;
	}
	*q = (0.0 < a)? floor(a): ceil(a);
	*r = m;
	return 0;
}

int float_truncate1_l_(long_float a, long_float *q, long_float *r)
{
	long_float m;

	if (a == 0.0L) {
		*q = *r = 0.0L;
		return 0;
	}

	m = fmodl(a, 1.0L);
	if (m == 0.0L) {
		*q = a;
		*r = 0.0L;
		return 0;
	}
	*q = (0.0L < a)? floorl(a): ceill(a);
	*r = m;
	return 0;
}


/*
 *  round
 */
static int float_round_even_s(single_float f)
{
	return fmodf(f, 2.0f) == 0.0f;
}

int float_round_s_(single_float a, single_float b, single_float *q, single_float *r)
{
	single_float i, f;

	if (b == 0.0f) {
		*q = *r = 0.0f;
		return division_by_zero_s_(CONSTANT_COMMON_ROUND, a, b);
	}
	f = modff(a / b, &i);
	if (0.0f <= f) {
		if (f < 0.5f)
			return float_floor_s_(a, b, q, r);
		else if (0.5f < f)
			return float_ceiling_s_(a, b, q, r);
		else if (float_round_even_s(i))
			return float_floor_s_(a, b, q, r);
		else
			return float_ceiling_s_(a, b, q, r);
	}
	else {
		if (-0.5f < f)
			return float_ceiling_s_(a, b, q, r);
		else if (f < -0.5f)
			return float_floor_s_(a, b, q, r);
		else if (float_round_even_s(i))
			return float_ceiling_s_(a, b, q, r);
		else
			return float_floor_s_(a, b, q, r);
	}
}

static int float_round_even_d(double_float f)
{
	return fmod(f, 2.0) == 0.0;
}

int float_round_d_(double_float a, double_float b, double_float *q, double_float *r)
{
	double_float i, f;

	if (b == 0.0) {
		*q = *r = 0.0;
		return division_by_zero_d_(CONSTANT_COMMON_ROUND, a, b);
	}
	f = modf(a / b, &i);
	if (0.0 <= f) {
		if (f < 0.5)
			return float_floor_d_(a, b, q, r);
		else if (0.5 < f)
			return float_ceiling_d_(a, b, q, r);
		else if (float_round_even_d(i))
			return float_floor_d_(a, b, q, r);
		else
			return float_ceiling_d_(a, b, q, r);
	}
	else {
		if (-0.5 < f)
			return float_ceiling_d_(a, b, q, r);
		else if (f < -0.5)
			return float_floor_d_(a, b, q, r);
		else if (float_round_even_d(i))
			return float_ceiling_d_(a, b, q, r);
		else
			return float_floor_d_(a, b, q, r);
	}
}

static int float_round_even_l(long_float f)
{
	return fmodl(f, 2.0L) == 0.0L;
}

int float_round_l_(long_float a, long_float b, long_float *q, long_float *r)
{
	long_float i, f;

	if (b == 0.0L) {
		*q = *r = 0.0L;
		return division_by_zero_l_(CONSTANT_COMMON_ROUND, a, b);
	}
	f = modfl(a / b, &i);
	if (0.0L <= f) {
		if (f < 0.5L)
			return float_floor_l_(a, b, q, r);
		else if (0.5L < f)
			return float_ceiling_l_(a, b, q, r);
		else if (float_round_even_l(i))
			return float_floor_l_(a, b, q, r);
		else
			return float_ceiling_l_(a, b, q, r);
	}
	else {
		if (-0.5L < f)
			return float_ceiling_l_(a, b, q, r);
		else if (f < -0.5L)
			return float_floor_l_(a, b, q, r);
		else if (float_round_even_l(i))
			return float_ceiling_l_(a, b, q, r);
		else
			return float_floor_l_(a, b, q, r);
	}
}

int float_round1_s_(single_float a, single_float *q, single_float *r)
{
	single_float i, f;

	f = modff(a, &i);
	if (0.0f <= f) {
		if (f < 0.5f)
			return float_floor1_s_(a, q, r);
		else if (0.5f < f)
			return float_ceiling1_s_(a, q, r);
		else if (float_round_even_s(i))
			return float_floor1_s_(a, q, r);
		else
			return float_ceiling1_s_(a, q, r);
	}
	else {
		if (-0.5f < f)
			return float_ceiling1_s_(a, q, r);
		else if (f < -0.5f)
			return float_floor1_s_(a, q, r);
		else if (float_round_even_s(i))
			return float_ceiling1_s_(a, q, r);
		else
			return float_floor1_s_(a, q, r);
	}
}

int float_round1_d_(double_float a, double_float *q, double_float *r)
{
	double_float i, f;

	f = modf(a, &i);
	if (0.0 <= f) {
		if (f < 0.5)
			return float_floor1_d_(a, q, r);
		else if (0.5 < f)
			return float_ceiling1_d_(a, q, r);
		else if (float_round_even_d(i))
			return float_floor1_d_(a, q, r);
		else
			return float_ceiling1_d_(a, q, r);
	}
	else {
		if (-0.5 < f)
			return float_ceiling1_d_(a, q, r);
		else if (f < -0.5)
			return float_floor1_d_(a, q, r);
		else if (float_round_even_d(i))
			return float_ceiling1_d_(a, q, r);
		else
			return float_floor1_d_(a, q, r);
	}
}

int float_round1_l_(long_float a, long_float *q, long_float *r)
{
	long_float i, f;

	f = modfl(a, &i);
	if (0.0L <= f) {
		if (f < 0.5L)
			return float_floor1_l_(a, q, r);
		else if (0.5L < f)
			return float_ceiling1_l_(a, q, r);
		else if (float_round_even_l(i))
			return float_floor1_l_(a, q, r);
		else
			return float_ceiling1_l_(a, q, r);
	}
	else {
		if (-0.5L < f)
			return float_ceiling1_l_(a, q, r);
		else if (f < -0.5L)
			return float_floor1_l_(a, q, r);
		else if (float_round_even_l(i))
			return float_ceiling1_l_(a, q, r);
		else
			return float_floor1_l_(a, q, r);
	}
}


/*
 *  fixnum
 */
int float_floor_f_(fixnum a, fixnum b, fixnum *quot, fixnum *rem)
{
	fixnum q, r;

	if (b == 0) {
		*quot = *rem = 0;
		return division_by_zero_f_(CONSTANT_COMMON_FLOOR, a, b);
	}
	Check(a == FIXNUM_MIN && b == -1, "floor fixnum overflow.");

	q = a / b;
	r = a % b;
	if (r == 0) {
		*quot = q;
		*rem = 0;
		return 0;
	}
	if (0 < a) {
		if (b < 0)
			goto floor2;
	}
	else if (0 < b) {
		goto floor2;
	}
	*quot = q;
	*rem = r;
	return 0;

floor2:
	*quot = q - 1;
	*rem = r + b;
	return 0;
}

int float_ceiling_f_(fixnum a, fixnum b, fixnum *quot, fixnum *rem)
{
	fixnum q, r;

	if (b == 0) {
		*quot = *rem = 0;
		return division_by_zero_f_(CONSTANT_COMMON_CEILING, a, b);
	}
	Check(a == FIXNUM_MIN && b == -1, "ceiling fixnum overflow.");

	q = a / b;
	r = a % b;
	if (r == 0) {
		*quot = q;
		*rem = 0;
		return 0;
	}
	if (0 < a) {
		if (b < 0)
			goto ceiling2;
	}
	else if (0 < b) {
		goto ceiling2;
	}
	*quot = q + 1;
	*rem = r - b;
	return 0;

ceiling2:
	*quot = q;
	*rem = r;
	return 0;
}

int float_truncate_f_(fixnum a, fixnum b, fixnum *quot, fixnum *rem)
{
	if (b == 0) {
		*quot = *rem = 0;
		return division_by_zero_f_(CONSTANT_COMMON_TRUNCATE, a, b);
	}
	Check(a == FIXNUM_MIN && b == -1, "truncate fixnum overflow.");

	*quot = a / b;
	*rem = a % b;
	return 0;
}

int float_round_f_(fixnum a, fixnum b, fixnum *quot, fixnum *rem)
{
	fixnum q, r, b2;

	/* error */
	if (b == 0) {
		*quot = *rem = 0;
		return division_by_zero_f_(CONSTANT_COMMON_ROUND, a, b);
	}

	/* |b| = 1 */
	if (b == 1) {
		*quot = a;
		*rem = 0;
		return 0;
	}
	if (b == -1) {
		Check(a == FIXNUM_MIN && b == -1, "round fixnum overflow.");
		*quot = -a;
		*rem = 0;
		return 0;
	}

	/* r = 0 */
	q = a / b;
	r = a % b;
	if (r == 0) {
		*quot = q;
		*rem = 0;
		return 0;
	}

	/* plus, minus */
	if (0 < a) {
		if (0 < b) {
			/* a:plus, b:plus, q:plus, r:plus */
			b2 = b / 2;
			if (r < b2)
				return float_truncate_f_(a, b, quot, rem);
			if (b2 < r)
				return float_ceiling_f_(a, b, quot, rem);
			if (b % 2)
				return float_truncate_f_(a, b, quot, rem);
			else if (q % 2)
				return float_ceiling_f_(a, b, quot, rem);
			else
				return float_truncate_f_(a, b, quot, rem);
		}
		else {
			/* a:plus, b:minus, q:minus, r:plus */
			b2 = -(b / 2);
			if (r < b2)
				return float_truncate_f_(a, b, quot, rem);
			if (b2 < r)
				return float_floor_f_(a, b, quot, rem);
			if (b % 2)
				return float_truncate_f_(a, b, quot, rem);
			else if (q % 2)
				return float_floor_f_(a, b, quot, rem);
			else
				return float_truncate_f_(a, b, quot, rem);
		}
	}
	else {
		if (0 < b) {
			/* a:minus, b:plus, q:minus, r:minus */
			b2 = -(b / 2);
			if (b2 < r)
				return float_truncate_f_(a, b, quot, rem);
			if (r < b2)
				return float_floor_f_(a, b, quot, rem);
			if (b % 2)
				return float_truncate_f_(a, b, quot, rem);
			else if (q % 2)
				return float_floor_f_(a, b, quot, rem);
			else
				return float_truncate_f_(a, b, quot, rem);
		}
		else {
			/* a:minus, b:minus, q:plus, r:minus */
			b2 = b / 2;
			if (r < b2)
				return float_ceiling_f_(a, b, quot, rem);
			if (b2 < r)
				return float_truncate_f_(a, b, quot, rem);
			if (b % 2)
				return float_truncate_f_(a, b, quot, rem);
			else if (q % 2)
				return float_ceiling_f_(a, b, quot, rem);
			else
				return float_truncate_f_(a, b, quot, rem);
		}
	}
}

int float_floor_fixnum_(addr *quot, addr *rem, fixnum a, fixnum b)
{
	if (a == FIXNUM_MIN && b == -1) {
		bignum_value_heap(quot, signminus_bignum, FIXNUM_UMIN);
		fixnum_heap(rem, 0);
	}
	else {
		Return(float_floor_f_(a, b, &a, &b));
		fixnum_heap(quot, a);
		fixnum_heap(rem, b);
	}

	return 0;
}

int float_ceiling_fixnum_(addr *quot, addr *rem, fixnum a, fixnum b)
{
	if (a == FIXNUM_MIN && b == -1) {
		bignum_value_heap(quot, signminus_bignum, FIXNUM_UMIN);
		fixnum_heap(rem, 0);
	}
	else {
		Return(float_ceiling_f_(a, b, &a, &b));
		fixnum_heap(quot, a);
		fixnum_heap(rem, b);
	}

	return 0;
}

int float_truncate_fixnum_(addr *quot, addr *rem, fixnum a, fixnum b)
{
	if (a == FIXNUM_MIN && b == -1) {
		bignum_value_heap(quot, signminus_bignum, FIXNUM_UMIN);
		fixnum_heap(rem, 0);
	}
	else {
		Return(float_truncate_f_(a, b, &a, &b));
		fixnum_heap(quot, a);
		fixnum_heap(rem, b);
	}

	return 0;
}

int float_round_fixnum_(addr *quot, addr *rem, fixnum a, fixnum b)
{
	if (a == FIXNUM_MIN && b == -1) {
		bignum_value_heap(quot, signminus_bignum, FIXNUM_UMIN);
		fixnum_heap(rem, 0);
	}
	else {
		Return(float_round_f_(a, b, &a, &b));
		fixnum_heap(quot, a);
		fixnum_heap(rem, b);
	}

	return 0;
}

int float_ffloor_fixnum_(addr *quot, addr *rem, fixnum a, fixnum b)
{
	if (a == FIXNUM_MIN && b == -1) {
		single_float_heap(quot, -((single_float)FIXNUM_UMIN));
		fixnum_heap(rem, 0);
	}
	else {
		Return(float_floor_f_(a, b, &a, &b));
		single_float_heap(quot, (single_float)a);
		fixnum_heap(rem, b);
	}

	return 0;
}

int float_fceiling_fixnum_(addr *quot, addr *rem, fixnum a, fixnum b)
{
	if (a == FIXNUM_MIN && b == -1) {
		single_float_heap(quot, -((single_float)FIXNUM_UMIN));
		fixnum_heap(rem, 0);
	}
	else {
		Return(float_ceiling_f_(a, b, &a, &b));
		single_float_heap(quot, (single_float)a);
		fixnum_heap(rem, b);
	}

	return 0;
}

int float_ftruncate_fixnum_(addr *quot, addr *rem, fixnum a, fixnum b)
{
	if (a == FIXNUM_MIN && b == -1) {
		single_float_heap(quot, -((single_float)FIXNUM_UMIN));
		fixnum_heap(rem, 0);
	}
	else {
		Return(float_truncate_f_(a, b, &a, &b));
		single_float_heap(quot, (single_float)a);
		fixnum_heap(rem, b);
	}

	return 0;
}

int float_fround_fixnum_(addr *quot, addr *rem, fixnum a, fixnum b)
{
	if (a == FIXNUM_MIN && b == -1) {
		single_float_heap(quot, -((single_float)FIXNUM_UMIN));
		fixnum_heap(rem, 0);
	}
	else {
		Return(float_round_f_(a, b, &a, &b));
		single_float_heap(quot, (single_float)a);
		fixnum_heap(rem, b);
	}

	return 0;
}


/*
 *  bignum
 */
struct divrem_struct {
	LocalRoot local;
	LocalStack stack;
	addr quot, rem;
	addr a, b, pos;
	int sign;
};

static void divrem_struct_initialize(LocalRoot local,
		struct divrem_struct *ptr, addr a, addr b)
{
	push_local(local, &(ptr->stack));
	ptr->local = local;
	ptr->quot = ptr->rem = NULL;
	ptr->a = a;
	ptr->b = b;
	ptr->pos = NULL;
}

static void divrem_struct_initialize1(LocalRoot local,
		struct divrem_struct *ptr, addr pos)
{
	addr numer, denom;

	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	divrem_struct_initialize(local, ptr, numer, denom);
	ptr->pos = pos;
}

static void divrem_struct_call(struct divrem_struct *ptr)
{
	int sign, sign1, sign2;
	addr q, r;

	if (ptr->quot == NULL) {
		GetSignBignum(ptr->a, &sign1);
		GetSignBignum(ptr->b, &sign2);
		sign = SignMulti(sign1, sign2);
		divrem_bigdata_local(ptr->local, &q, &r, ptr->a, ptr->b);
		SetSignBignum(q, sign);
		SetSignBignum(r, sign1);
		ptr->quot = q;
		ptr->rem = r;
		ptr->sign = sign;
	}
}

static void divrem_struct_integer(struct divrem_struct *ptr, addr *quot, addr *rem)
{
	bignum_result_heap(ptr->quot, quot);
	*rem = ptr->rem;
	rollback_local(ptr->local, ptr->stack);
#ifdef LISP_DEBUG
	ptr->local = (LocalRoot)Unbound;
#endif
}

static void rem_struct_integer(struct divrem_struct *ptr, addr *rem)
{
	*rem = ptr->rem;
	rollback_local(ptr->local, ptr->stack);
#ifdef LISP_DEBUG
	ptr->local = (LocalRoot)Unbound;
#endif
}

static int divrem_struct_float_(struct divrem_struct *ptr, addr *quot, addr *rem)
{
	Return(single_float_bignum_heap_(quot, ptr->quot));
	*rem = ptr->rem;
	rollback_local(ptr->local, ptr->stack);
#ifdef LISP_DEBUG
	ptr->local = (LocalRoot)Unbound;
#endif

	return 0;
}

static void divrem_struct_close(struct divrem_struct *ptr)
{
	rollback_local(ptr->local, ptr->stack);
#ifdef LISP_DEBUG
	ptr->local = (LocalRoot)Unbound;
#endif
}

static void float_floor_bb(struct divrem_struct *ptr)
{
	divrem_struct_call(ptr);
	if (zerop_bignum(ptr->rem)) {
		fixnum_heap(&(ptr->rem), 0);
		return;
	}
	if (IsMinus(ptr->sign)) {
		plus_bv_bignum_local(ptr->local, ptr->quot, -1, &(ptr->quot));
		plus_bb_bignum_local(ptr->local, ptr->b, ptr->rem, &(ptr->rem));
	}
	bignum_result_heap(ptr->rem, &(ptr->rem));
}

int float_floor_bignum_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FLOOR, a, b);
	}
	if (zerop_bignum(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return 0;
	}

	divrem_struct_initialize(local, &str, a, b);
	float_floor_bb(&str);
	divrem_struct_integer(&str, quot, rem);

	return 0;
}

int float_ffloor_bignum_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FFLOOR, a, b);
	}
	if (zerop_bignum(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return 0;
	}

	divrem_struct_initialize(local, &str, a, b);
	float_floor_bb(&str);
	Return(divrem_struct_float_(&str, quot, rem));

	return 0;
}

static void float_ceiling_bb(struct divrem_struct *ptr)
{
	divrem_struct_call(ptr);
	if (zerop_bignum(ptr->rem)) {
		fixnum_heap(&(ptr->rem), 0);
		return;
	}
	if (IsPlus(ptr->sign)) {
		plus_bv_bignum_local(ptr->local, ptr->quot, 1, &(ptr->quot));
		minus_bb_bignum_local(ptr->local, ptr->rem, ptr->b, &(ptr->rem));
	}
	bignum_result_heap(ptr->rem, &(ptr->rem));
}

int float_ceiling_bignum_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_CEILING, a, b);
	}
	if (zerop_bignum(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return 0;
	}

	divrem_struct_initialize(local, &str, a, b);
	float_ceiling_bb(&str);
	divrem_struct_integer(&str, quot, rem);

	return 0;
}

int float_fceiling_bignum_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FCEILING, a, b);
	}
	if (zerop_bignum(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return 0;
	}

	divrem_struct_initialize(local, &str, a, b);
	float_ceiling_bb(&str);
	Return(divrem_struct_float_(&str, quot, rem));

	return 0;
}

static void float_truncate_bb(struct divrem_struct *ptr)
{
	divrem_struct_call(ptr);
	bignum_result_heap(ptr->rem, &(ptr->rem));
}

int float_truncate_bignum_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_TRUNCATE, a, b);
	}
	if (zerop_bignum(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return 0;
	}

	divrem_struct_initialize(local, &str, a, b);
	float_truncate_bb(&str);
	divrem_struct_integer(&str, quot, rem);

	return 0;
}

int float_ftruncate_bignum_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FTRUNCATE, a, b);
	}
	if (zerop_bignum(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return 0;
	}

	divrem_struct_initialize(local, &str, a, b);
	float_truncate_bb(&str);
	Return(divrem_struct_float_(&str, quot, rem));

	return 0;
}

static int float_round_bb_check(struct divrem_struct *ptr)
{
	int sign, check;
	addr b;
	LocalRoot local;
	LocalStack stack;

	divrem_struct_call(ptr);
	if (zerop_bignum(ptr->rem))
		return 0;

	local = ptr->local;
	push_local(local, &stack);
	division2_bigdata_alloc(local, &b, ptr->b);
	GetSignBignum(ptr->quot, &sign);
	if (IsPlus(sign)) {
		check = compare_bigdata(ptr->rem, b);
		if (check == 0) {
			if (evenp_bignum(ptr->b))
				check = evenp_bignum(ptr->quot)? -1: 1;
			else
				check = -1;
		}
	}
	else {
		check = compare_bigdata(b, ptr->rem);
		if (check == 0) {
			if (evenp_bignum(ptr->b))
				check = evenp_bignum(ptr->quot)? 1: -1;
			else
				check = 1;
		}

	}
	rollback_local(local, stack);

	return check;
}

static void float_round_bb(struct divrem_struct *ptr)
{
	int check;

	Check(zerop_bignum(ptr->a), "zero error: a");
	Check(zerop_bignum(ptr->b), "zero error: b");

	check = float_round_bb_check(ptr);
	if (check < 0)
		float_floor_bb(ptr);
	else if (0 < check)
		float_ceiling_bb(ptr);
	else
		fixnum_heap(&(ptr->rem), 0);
}

int float_round_bignum_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_ROUND, a, b);
	}
	if (zerop_bignum(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return 0;
	}

	divrem_struct_initialize(local, &str, a, b);
	float_round_bb(&str);
	divrem_struct_integer(&str, quot, rem);

	return 0;
}

int float_fround_bignum_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FROUND, a, b);
	}
	if (zerop_bignum(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return 0;
	}

	divrem_struct_initialize(local, &str, a, b);
	float_round_bb(&str);
	Return(divrem_struct_float_(&str, quot, rem));

	return 0;
}


/*
 *  ratio
 */
static void float_floor1_nosign(struct divrem_struct *ptr, int sign1, int sign2)
{
	int check;
	addr quot, rem, temp;

	check = compare_bigdata(ptr->a, ptr->b);
	Check(check == 0, "ratio error");
	if (check <= 0) {
		bignum_zero_local(ptr->local, &quot);
		ratio_copy_nosign_heap(&rem, ptr->pos);
		SetSignRatio(rem, sign2);
	}
	else {
		divrem_struct_call(ptr);
		Check(zerop_bignum(ptr->rem), "ratio zero error");
		/* quot */
		bignum_copy_nosign_local(ptr->local, &quot, ptr->quot);
		SetSignBignum(quot, sign1);
		/* rem */
		bignum_throw_heap(ptr->rem, &rem);
		bignum_throw_heap(ptr->b, &temp);
		make_ratio_alloc_unsafe(NULL, &rem, sign2, rem, temp);
	}
	ptr->quot = quot;
	ptr->rem = rem;
}

static void float_ceiling1_nosign(struct divrem_struct *ptr, int sign1, int sign2)
{
	int check;
	addr quot, rem, temp;

	check = compare_bigdata(ptr->a, ptr->b);
	Check(check == 0, "ratio error");
	if (check <= 0) {
		bignum_value_local(ptr->local, &quot, sign1, 1);
		minus_bigdata_alloc(NULL, ptr->b, ptr->a, &rem);
		bignum_throw_heap(ptr->b, &temp);
		make_ratio_alloc_unsafe(NULL, &rem, sign2, rem, temp);
	}
	else {
		divrem_struct_call(ptr);
		Check(zerop_bignum(ptr->rem), "ratio zero error");
		/* quot */
		plusvalue_bigdata_alloc(ptr->local, ptr->quot, sign1, 1, &quot);
		/* rem */
		minus_bigdata_alloc(NULL, ptr->b, ptr->rem, &rem);
		bignum_throw_heap(ptr->b, &temp);
		make_ratio_alloc_unsafe(NULL, &rem, sign2, rem, temp);
	}
	ptr->quot = quot;
	ptr->rem = rem;
}

static void float_floor1_r(struct divrem_struct *ptr)
{
	int sign;

	GetSignRatio(ptr->pos, &sign);
	if (IsPlus(sign))
		float_floor1_nosign(ptr, SignPlus, SignPlus);
	else
		float_ceiling1_nosign(ptr, SignMinus, SignPlus);
}

int float_floor1_ratio_(LocalRoot local, addr *quot, addr *rem, addr pos)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(pos, LISPTYPE_RATIO);
	if (zerop_ratio(pos)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return 0;
	}

	divrem_struct_initialize1(local, &str, pos);
	float_floor1_r(&str);
	divrem_struct_integer(&str, quot, rem);

	return 0;
}

static void float_ceiling1_r(struct divrem_struct *ptr)
{
	int sign;

	GetSignRatio(ptr->pos, &sign);
	if (IsPlus(sign))
		float_ceiling1_nosign(ptr, SignPlus, SignMinus);
	else
		float_floor1_nosign(ptr, SignMinus, SignMinus);
}

int float_ceiling1_ratio_(LocalRoot local, addr *quot, addr *rem, addr pos)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(pos, LISPTYPE_RATIO);
	if (zerop_ratio(pos)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return 0;
	}

	divrem_struct_initialize1(local, &str, pos);
	float_ceiling1_r(&str);
	divrem_struct_integer(&str, quot, rem);

	return 0;
}

int float_truncate1_ratio_(LocalRoot local, addr *quot, addr *rem, addr pos)
{
	int sign;
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(pos, LISPTYPE_RATIO);
	if (zerop_ratio(pos)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return 0;
	}

	divrem_struct_initialize1(local, &str, pos);
	GetSignRatio(pos, &sign);
	float_floor1_nosign(&str, sign, sign);
	divrem_struct_integer(&str, quot, rem);

	return 0;
}

static int float_round1_half(addr numer, addr denom)
{
	return equal_value_nosign_bignum(numer, 1)
		&& equal_value_nosign_bignum(denom, 2);
}

static void float_round1_r(struct divrem_struct *ptr)
{
	addr denom2;

	divrem_struct_call(ptr);
	if (float_round1_half(ptr->rem, ptr->b)) {
		/* rem = 1/2 */
		if (plusp_ratio(ptr->pos)) {
			if (evenp_bignum(ptr->quot))
				float_floor1_r(ptr);
			else
				float_ceiling1_r(ptr);
		}
		else {
			if (evenp_bignum(ptr->quot))
				float_ceiling1_r(ptr);
			else
				float_floor1_r(ptr);
		}
	}
	else {
		/* otherwise */
		division2_bigdata_alloc(ptr->local, &denom2, ptr->b);
		if (plusp_ratio(ptr->pos)) {
			if (compare_bb_real(ptr->rem, denom2) <= 0)
				float_floor1_r(ptr);
			else
				float_ceiling1_r(ptr);
		}
		else {
			if (compare_bb_real(ptr->rem, denom2) <= 0)
				float_ceiling1_r(ptr);
			else
				float_floor1_r(ptr);
		}
	}
}

int float_round1_ratio_(LocalRoot local, addr *quot, addr *rem, addr pos)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(pos, LISPTYPE_RATIO);
	if (zerop_ratio(pos)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return 0;
	}

	divrem_struct_initialize1(local, &str, pos);
	float_round1_r(&str);
	divrem_struct_integer(&str, quot, rem);

	return 0;
}

int float_ffloor1_ratio_(LocalRoot local, addr *quot, addr *rem, addr pos)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(pos, LISPTYPE_RATIO);
	if (zerop_ratio(pos)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return 0;
	}

	divrem_struct_initialize1(local, &str, pos);
	float_floor1_r(&str);
	Return(divrem_struct_float_(&str, quot, rem));

	return 0;
}

int float_fceiling1_ratio_(LocalRoot local, addr *quot, addr *rem, addr pos)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(pos, LISPTYPE_RATIO);
	if (zerop_ratio(pos)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return 0;
	}

	divrem_struct_initialize1(local, &str, pos);
	float_ceiling1_r(&str);
	Return(divrem_struct_float_(&str, quot, rem));

	return 0;
}

int float_ftruncate1_ratio_(LocalRoot local, addr *quot, addr *rem, addr pos)
{
	int sign;
	struct divrem_struct str;

	Check(local == NULL, "local error");
	if (zerop_ratio(pos)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return 0;
	}

	divrem_struct_initialize1(local, &str, pos);
	GetSignRatio(pos, &sign);
	float_floor1_nosign(&str, sign, sign);
	Return(divrem_struct_float_(&str, quot, rem));

	return 0;
}

int float_fround1_ratio_(LocalRoot local, addr *quot, addr *rem, addr pos)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(pos, LISPTYPE_RATIO);
	if (zerop_ratio(pos)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return 0;
	}

	divrem_struct_initialize1(local, &str, pos);
	float_round1_r(&str);
	Return(divrem_struct_float_(&str, quot, rem));

	return 0;
}


/*
 *  bignum-ratio
 */
static int float_floor_br_plus(LocalRoot local, addr *ret, int sign, addr a, addr b)
{
	addr numer, denom;

	/*   a     b.numer       a * b.denom
	 *  --- / ---------  =  -------------
	 *   1     b.denom         b.numer
	 */
	GetNumerRatio(b, &numer);
	GetDenomRatio(b, &denom);
	bignum_copy_nosign_local(local, &a, a);
	bignum_copy_nosign_local(local, &b, numer);
	reduction_local(local, a, b);
	multi_bigdata_alloc(local, a, denom, &a);
	if (equal_value_nosign_bignum(b, 1)) {
		SetSignBignum(a, sign);
		*ret = a;
		return 0;
	}
	else {
		divrem_bigdata_local(local, ret, &b, a, b);
		SetSignBignum(*ret, sign);
		return 1;
	}
}

static int float_floor_br_minus(LocalRoot local, addr *ret, int sign, addr a, addr b)
{
	int check;

	check = float_floor_br_plus(local, ret, sign, a, b);
	if (check)
		plus_bv_bignum_local(local, *ret, -1, ret);

	return check;
}

static void remainder_br_ratio(LocalRoot local, addr *ret, addr a, addr b, addr q)
{
	/* remainder = a - quotient*b */
	multi_br_ratio_local(local, q, b, &b);
	minus_br_ratio_local(local, a, b, ret);
}

static void float_floor_br(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int sign, sign1, sign2, check;
	addr q, r;

	GetSignBignum(a, &sign1);
	GetSignRatio(b, &sign2);
	sign = SignMulti(sign1, sign2);
	if (IsPlus(sign))
		check = float_floor_br_plus(local, &q, sign, a, b);
	else
		check = float_floor_br_minus(local, &q, sign, a, b);
	if (check) {
		remainder_br_ratio(local, &r, a, b, q);
		ratio_result_noreduction_heap(local, r, rem);
	}
	else {
		fixnum_heap(rem, 0);
	}
	*quot = q;
}

int float_floor_br_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FLOOR, a, b);
	}
	if (zerop_bignum(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return 0;
	}

	push_local(local, &stack);
	float_floor_br(local, quot, rem, a, b);
	bignum_result_heap(*quot, quot);
	rollback_local(local, stack);

	return 0;
}

static int float_ceiling_br_minus(LocalRoot local, addr *ret, int sign, addr a, addr b)
{
	int check;

	check = float_floor_br_plus(local, ret, sign, a, b);
	if (check)
		plus_bv_bignum_local(local, *ret, 1, ret);

	return check;
}

static void float_ceiling_br(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int sign, sign1, sign2, check;
	addr q, r;

	GetSignBignum(a, &sign1);
	GetSignRatio(b, &sign2);
	sign = SignMulti(sign1, sign2);
	if (IsPlus(sign))
		check = float_ceiling_br_minus(local, &q, sign, a, b);
	else
		check = float_floor_br_plus(local, &q, sign, a, b);
	if (check) {
		remainder_br_ratio(local, &r, a, b, q);
		ratio_result_noreduction_heap(local, r, rem);
	}
	else {
		fixnum_heap(rem, 0);
	}
	*quot = q;
}

int float_ceiling_br_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_CEILING, a, b);
	}
	if (zerop_bignum(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return 0;
	}

	push_local(local, &stack);
	float_ceiling_br(local, quot, rem, a, b);
	bignum_result_heap(*quot, quot);
	rollback_local(local, stack);

	return 0;
}

static void float_truncate_br(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int sign, sign1, sign2;
	addr q, r;

	GetSignBignum(a, &sign1);
	GetSignRatio(b, &sign2);
	sign = SignMulti(sign1, sign2);
	if (float_floor_br_plus(local, &q, sign, a, b)) {
		remainder_br_ratio(local, &r, a, b, q);
		ratio_result_noreduction_heap(local, r, rem);
	}
	else {
		fixnum_heap(rem, 0);
	}
	*quot = q;
}

int float_truncate_br_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_TRUNCATE, a, b);
	}
	if (zerop_bignum(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return 0;
	}

	push_local(local, &stack);
	float_truncate_br(local, quot, rem, a, b);
	bignum_result_heap(*quot, quot);
	rollback_local(local, stack);

	return 0;
}

static int float_round_br_check(LocalRoot local, addr a, addr b)
{
	int check, sign1, sign2;
	addr numer, denom;
	LocalStack stack;
	struct divrem_struct str;

	Check(zerop_bignum(a), "zero error: left");
	Check(zerop_ratio(b), "zero error: right");
	GetSignBignum(a, &sign1);
	GetSignRatio(b, &sign2);
	GetNumerRatio(b, &numer);
	GetDenomRatio(b, &denom);

	push_local(local, &stack);
	multi_bigdata_alloc(local, a, denom, &a);
	bignum_copy_nosign_alloc(local, &b, numer);
	SetSignBignum(a, sign1);
	SetSignBignum(b, sign2);

	divrem_struct_initialize(local, &str, a, b);
	check = float_round_bb_check(&str);
	divrem_struct_close(&str);
	rollback_local(local, stack);

	return check;
}

static void float_round_br(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int check;

	check = float_round_br_check(local, a, b);
	if (check < 0)
		float_floor_br(local, quot, rem, a, b);
	else
		float_ceiling_br(local, quot, rem, a, b);
}

int float_round_br_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_ROUND, a, b);
	}
	if (zerop_bignum(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return 0;
	}

	push_local(local, &stack);
	float_round_br(local, quot, rem, a, b);
	bignum_result_heap(*quot, quot);
	rollback_local(local, stack);

	return 0;
}

int float_ffloor_br_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FFLOOR, a, b);
	}
	if (zerop_bignum(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return 0;
	}

	push_local(local, &stack);
	float_floor_br(local, quot, rem, a, b);
	Return(single_float_bignum_heap_(quot, *quot));
	rollback_local(local, stack);

	return 0;
}

int float_fceiling_br_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FCEILING, a, b);
	}
	if (zerop_bignum(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return 0;
	}

	push_local(local, &stack);
	float_ceiling_br(local, quot, rem, a, b);
	Return(single_float_bignum_heap_(quot, *quot));
	rollback_local(local, stack);

	return 0;
}

int float_ftruncate_br_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FTRUNCATE, a, b);
	}
	if (zerop_bignum(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return 0;
	}

	push_local(local, &stack);
	float_truncate_br(local, quot, rem, a, b);
	Return(single_float_bignum_heap_(quot, *quot));
	rollback_local(local, stack);

	return 0;
}

int float_fround_br_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FROUND, a, b);
	}
	if (zerop_bignum(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return 0;
	}

	push_local(local, &stack);
	float_round_br(local, quot, rem, a, b);
	Return(single_float_bignum_heap_(quot, *quot));
	rollback_local(local, stack);

	return 0;
}


/*
 *  ratio-bignum
 */
static int float_floor_rb_plus(LocalRoot local, addr *ret, int sign, addr a, addr b)
{
	addr numer, denom;

	/*   a.numer     b         a.numer
	 *  --------- / ---  =  -------------
	 *   a.denom     1       a.denom * b
	 */
	GetNumerRatio(a, &numer);
	GetDenomRatio(a, &denom);
	bignum_copy_nosign_local(local, &a, numer);
	bignum_copy_nosign_local(local, &b, b);
	reduction_local(local, a, b);
	multi_bigdata_alloc(local, b, denom, &b);
	if (equal_value_nosign_bignum(b, 1)) {
		SetSignBignum(a, sign);
		*ret = a;
		return 0;
	}
	else {
		divrem_bigdata_local(local, ret, &b, a, b);
		SetSignBignum(*ret, sign);
		return 1;
	}
}

static int float_floor_rb_minus(LocalRoot local, addr *ret, int sign, addr a, addr b)
{
	int check;

	check = float_floor_rb_plus(local, ret, sign, a, b);
	if (check)
		plus_bv_bignum_local(local, *ret, -1, ret);

	return check;
}

static void remainder_rb_ratio(LocalRoot local, addr *ret, addr a, addr b, addr q)
{
	/* remainder = a - quotient*b */
	multi_bb_bignum_local(local, q, b, &b);
	minus_rb_ratio_local(local, a, b, ret);
}

static void float_floor_rb(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int sign, sign1, sign2, check;
	addr q, r;

	GetSignRatio(a, &sign1);
	GetSignBignum(b, &sign2);
	sign = SignMulti(sign1, sign2);
	if (IsPlus(sign))
		check = float_floor_rb_plus(local, &q, sign, a, b);
	else
		check = float_floor_rb_minus(local, &q, sign, a, b);
	if (check) {
		remainder_rb_ratio(local, &r, a, b, q);
		ratio_result_noreduction_heap(local, r, rem);
	}
	else {
		fixnum_heap(rem, 0);
	}
	*quot = q;
}

int float_floor_rb_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FLOOR, a, b);
	}
	if (zerop_ratio(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return 0;
	}
	push_local(local, &stack);
	float_floor_rb(local, quot, rem, a, b);
	bignum_result_heap(*quot, quot);
	rollback_local(local, stack);

	return 0;
}

static int float_ceiling_rb_minus(LocalRoot local, addr *ret, int sign, addr a, addr b)
{
	int check;

	check = float_floor_rb_plus(local, ret, sign, a, b);
	if (check)
		plus_bv_bignum_local(local, *ret, 1, ret);

	return check;
}

static void float_ceiling_rb(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int sign, sign1, sign2, check;
	addr q, r;

	GetSignRatio(a, &sign1);
	GetSignBignum(b, &sign2);
	sign = SignMulti(sign1, sign2);
	if (IsPlus(sign))
		check = float_ceiling_rb_minus(local, &q, sign, a, b);
	else
		check = float_floor_rb_plus(local, &q, sign, a, b);
	if (check) {
		remainder_rb_ratio(local, &r, a, b, q);
		ratio_result_noreduction_heap(local, r, rem);
	}
	else {
		fixnum_heap(rem, 0);
	}
	*quot = q;
}

int float_ceiling_rb_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_CEILING, a, b);
	}
	if (zerop_ratio(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return 0;
	}
	push_local(local, &stack);
	float_ceiling_rb(local, quot, rem, a, b);
	bignum_result_heap(*quot, quot);
	rollback_local(local, stack);

	return 0;
}

static void float_truncate_rb(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int sign, sign1, sign2;
	addr q, r;

	GetSignRatio(a, &sign1);
	GetSignBignum(b, &sign2);
	sign = SignMulti(sign1, sign2);
	if (float_floor_rb_plus(local, &q, sign, a, b)) {
		remainder_rb_ratio(local, &r, a, b, q);
		ratio_result_noreduction_heap(local, r, rem);
	}
	else {
		fixnum_heap(rem, 0);
	}
	*quot = q;
}

int float_truncate_rb_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_TRUNCATE, a, b);
	}
	if (zerop_ratio(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return 0;
	}
	push_local(local, &stack);
	float_truncate_rb(local, quot, rem, a, b);
	bignum_result_heap(*quot, quot);
	rollback_local(local, stack);

	return 0;
}

static int float_round_rb_check(LocalRoot local, addr a, addr b)
{
	int check, sign1, sign2;
	addr numer, denom;
	LocalStack stack;
	struct divrem_struct str;

	Check(zerop_ratio(a), "zero error: left");
	Check(zerop_bignum(b), "zero error: right");
	GetSignRatio(a, &sign1);
	GetSignBignum(b, &sign2);
	GetNumerRatio(a, &numer);
	GetDenomRatio(a, &denom);

	push_local(local, &stack);
	bignum_copy_nosign_alloc(local, &a, numer);
	multi_bigdata_alloc(local, b, denom, &b);
	SetSignBignum(a, sign1);
	SetSignBignum(b, sign2);

	divrem_struct_initialize(local, &str, a, b);
	check = float_round_bb_check(&str);
	divrem_struct_close(&str);
	rollback_local(local, stack);

	return check;
}

static void float_round_rb(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int check;

	check = float_round_rb_check(local, a, b);
	if (check < 0)
		float_floor_rb(local, quot, rem, a, b);
	else
		float_ceiling_rb(local, quot, rem, a, b);
}

int float_round_rb_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_ROUND, a, b);
	}
	if (zerop_ratio(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return 0;
	}
	push_local(local, &stack);
	float_round_rb(local, quot, rem, a, b);
	bignum_result_heap(*quot, quot);
	rollback_local(local, stack);

	return 0;
}

int float_ffloor_rb_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FFLOOR, a, b);
	}
	if (zerop_ratio(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return 0;
	}
	push_local(local, &stack);
	float_floor_rb(local, quot, rem, a, b);
	Return(single_float_bignum_heap_(quot, *quot));
	rollback_local(local, stack);

	return 0;
}

int float_fceiling_rb_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FCEILING, a, b);
	}
	if (zerop_ratio(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return 0;
	}
	push_local(local, &stack);
	float_ceiling_rb(local, quot, rem, a, b);
	Return(single_float_bignum_heap_(quot, *quot));
	rollback_local(local, stack);

	return 0;
}

int float_ftruncate_rb_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FTRUNCATE, a, b);
	}
	if (zerop_ratio(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return 0;
	}
	push_local(local, &stack);
	float_truncate_rb(local, quot, rem, a, b);
	Return(single_float_bignum_heap_(quot, *quot));
	rollback_local(local, stack);

	return 0;
}

int float_fround_rb_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FROUND, a, b);
	}
	if (zerop_ratio(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return 0;
	}
	push_local(local, &stack);
	float_round_rb(local, quot, rem, a, b);
	Return(single_float_bignum_heap_(quot, *quot));
	rollback_local(local, stack);

	return 0;
}


/*
 *  ratio-ratio
 */
static int float_floor_rr_plus(LocalRoot local, addr *ret, int sign, addr a, addr b)
{
	addr numer1, denom1, numer2, denom2;

	/*   a.numer     b.numer       a.numer * b.denom
	 *  --------- / ---------  =  -------------------
	 *   a.denom     b.denom       a.denom * b.numer
	 */
	GetNumerRatio(a, &numer1);
	GetDenomRatio(a, &denom1);
	GetNumerRatio(b, &numer2);
	GetDenomRatio(b, &denom2);
	bignum_copy_nosign_local(local, &numer1, numer1);
	bignum_copy_nosign_local(local, &denom1, denom1);
	bignum_copy_nosign_local(local, &numer2, numer2);
	bignum_copy_nosign_local(local, &denom2, denom2);
	reduction_local(local, numer1, numer2);
	reduction_local(local, denom1, denom2);
	multi_bigdata_alloc(local, numer1, denom2, &a);
	multi_bigdata_alloc(local, denom1, numer2, &b);
	if (equal_value_nosign_bignum(b, 1)) {
		SetSignBignum(a, sign);
		*ret = a;
		return 0;
	}
	else {
		divrem_bigdata_local(local, ret, &b, a, b);
		SetSignBignum(*ret, sign);
		return 1;
	}
}

static int float_floor_rr_minus(LocalRoot local, addr *ret, int sign, addr a, addr b)
{
	int check;

	check = float_floor_rr_plus(local, ret, sign, a, b);
	if (check)
		plus_bv_bignum_local(local, *ret, -1, ret);

	return check;
}

static void remainder_rr_ratio(LocalRoot local, addr *ret, addr a, addr b, addr q)
{
	/* remainder = a - quotient*b */
	multi_br_ratio_local(local, q, b, &b);
	minus_rr_ratio_local(local, a, b, ret);
}

static void float_floor_rr(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int sign, sign1, sign2, check;
	addr q, r;

	GetSignRatio(a, &sign1);
	GetSignRatio(b, &sign2);
	sign = SignMulti(sign1, sign2);
	if (IsPlus(sign))
		check = float_floor_rr_plus(local, &q, sign, a, b);
	else
		check = float_floor_rr_minus(local, &q, sign, a, b);
	if (check) {
		remainder_rr_ratio(local, &r, a, b, q);
		ratio_result_noreduction_heap(local, r, rem);
	}
	else {
		fixnum_heap(rem, 0);
	}
	*quot = q;
}

int float_floor_rr_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FLOOR, a, b);
	}
	if (zerop_ratio(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return 0;
	}
	push_local(local, &stack);
	float_floor_rr(local, quot, rem, a, b);
	bignum_result_heap(*quot, quot);
	rollback_local(local, stack);

	return 0;
}

static int float_ceiling_rr_minus(LocalRoot local, addr *ret, int sign, addr a, addr b)
{
	int check;

	check = float_floor_rr_plus(local, ret, sign, a, b);
	if (check)
		plus_bv_bignum_local(local, *ret, 1, ret);

	return check;
}

static void float_ceiling_rr(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int sign, sign1, sign2, check;
	addr q, r;

	GetSignRatio(a, &sign1);
	GetSignRatio(b, &sign2);
	sign = SignMulti(sign1, sign2);
	if (IsPlus(sign))
		check = float_ceiling_rr_minus(local, &q, sign, a, b);
	else
		check = float_floor_rr_plus(local, &q, sign, a, b);
	if (check) {
		remainder_rr_ratio(local, &r, a, b, q);
		ratio_result_noreduction_heap(local, r, rem);
	}
	else {
		fixnum_heap(rem, 0);
	}
	*quot = q;
}

int float_ceiling_rr_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_CEILING, a, b);
	}
	if (zerop_ratio(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return 0;
	}
	push_local(local, &stack);
	float_ceiling_rr(local, quot, rem, a, b);
	bignum_result_heap(*quot, quot);
	rollback_local(local, stack);

	return 0;
}

static void float_truncate_rr(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int sign, sign1, sign2;
	addr q, r;

	GetSignRatio(a, &sign1);
	GetSignRatio(b, &sign2);
	sign = SignMulti(sign1, sign2);
	if (float_floor_rr_plus(local, &q, sign, a, b)) {
		remainder_rr_ratio(local, &r, a, b, q);
		ratio_result_noreduction_heap(local, r, rem);
	}
	else {
		fixnum_heap(rem, 0);
	}
	*quot = q;
}

int float_truncate_rr_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_TRUNCATE, a, b);
	}
	if (zerop_ratio(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return 0;
	}
	push_local(local, &stack);
	float_truncate_rr(local, quot, rem, a, b);
	bignum_result_heap(*quot, quot);
	rollback_local(local, stack);

	return 0;
}

static int float_round_rr_check(LocalRoot local, addr a, addr b)
{
	int check, sign1, sign2;
	addr numer1, denom1, numer2, denom2, reduct1, reduct2;
	LocalStack stack;
	struct divrem_struct str;

	Check(zerop_ratio(a), "zero error: left");
	Check(zerop_ratio(b), "zero error: right");

	GetSignRatio(a, &sign1);
	GetSignRatio(b, &sign2);
	GetNumerRatio(a, &numer1);
	GetDenomRatio(a, &denom1);
	GetNumerRatio(b, &numer2);
	GetDenomRatio(b, &denom2);

	push_local(local, &stack);
	if (equal_bigdata(denom1, denom2)) {
		/*
		 *  numer1 + numer2
		 *  ---------------
		 *      denom1
		 */
		bignum_copy_nosign_alloc(local, &a, numer1);
		bignum_copy_nosign_alloc(local, &b, numer2);
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
		multi_bigdata_alloc(local, numer1, reduct2, &a);
		multi_bigdata_alloc(local, numer2, reduct1, &b);
	}

	SetSignBignum(a, sign1);
	SetSignBignum(b, sign2);
	divrem_struct_initialize(local, &str, a, b);
	check = float_round_bb_check(&str);
	divrem_struct_close(&str);
	rollback_local(local, stack);

	return check;
}

static void float_round_rr(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int check;

	check = float_round_rr_check(local, a, b);
	if (check < 0)
		float_floor_rr(local, quot, rem, a, b);
	else
		float_ceiling_rr(local, quot, rem, a, b);
}

int float_round_rr_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_ROUND, a, b);
	}
	if (zerop_ratio(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return 0;
	}
	push_local(local, &stack);
	float_round_rr(local, quot, rem, a, b);
	bignum_result_heap(*quot, quot);
	rollback_local(local, stack);

	return 0;
}

int float_ffloor_rr_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FFLOOR, a, b);
	}
	if (zerop_ratio(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return 0;
	}
	push_local(local, &stack);
	float_floor_rr(local, quot, rem, a, b);
	Return(single_float_bignum_heap_(quot, *quot));
	rollback_local(local, stack);

	return 0;
}

int float_fceiling_rr_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FCEILING, a, b);
	}
	if (zerop_ratio(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return 0;
	}
	push_local(local, &stack);
	float_ceiling_rr(local, quot, rem, a, b);
	Return(single_float_bignum_heap_(quot, *quot));
	rollback_local(local, stack);

	return 0;
}

int float_ftruncate_rr_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FTRUNCATE, a, b);
	}
	if (zerop_ratio(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return 0;
	}
	push_local(local, &stack);
	float_truncate_rr(local, quot, rem, a, b);
	Return(single_float_bignum_heap_(quot, *quot));
	rollback_local(local, stack);

	return 0;
}

int float_fround_rr_ratio_(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		*quot = *rem = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FROUND, a, b);
	}
	if (zerop_ratio(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return 0;
	}
	push_local(local, &stack);
	float_round_rr(local, quot, rem, a, b);
	Return(single_float_bignum_heap_(quot, *quot));
	rollback_local(local, stack);

	return 0;
}


/*
 *  mod -> floor
 */
int float_mod_fixnum_(addr *ret, fixnum a, fixnum b)
{
	if (a == FIXNUM_MIN && b == -1) {
		fixnum_heap(ret, 0);
	}
	else {
		Return(float_floor_f_(a, b, &a, &b));
		fixnum_heap(ret, b);
	}

	return 0;
}

int float_mod_bignum_(LocalRoot local, addr *ret, addr a, addr b)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		*ret = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FLOOR, a, b);
	}
	if (zerop_bignum(a)) {
		fixnum_heap(ret, 0);
		return 0;
	}

	divrem_struct_initialize(local, &str, a, b);
	float_floor_bb(&str);
	rem_struct_integer(&str, ret);

	return 0;
}

int float_mod_br_ratio_(LocalRoot local, addr *ret, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		*ret = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FLOOR, a, b);
	}
	if (zerop_bignum(a)) {
		fixnum_heap(ret, 0);
		return 0;
	}

	push_local(local, &stack);
	float_floor_br(local, &a, ret, a, b);
	rollback_local(local, stack);

	return 0;
}

int float_mod_rb_ratio_(LocalRoot local, addr *ret, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		*ret = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FLOOR, a, b);
	}
	if (zerop_ratio(a)) {
		fixnum_heap(ret, 0);
		return 0;
	}

	push_local(local, &stack);
	float_floor_rb(local, &a, ret, a, b);
	rollback_local(local, stack);

	return 0;
}

int float_mod_rr_ratio_(LocalRoot local, addr *ret, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		*ret = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_FLOOR, a, b);
	}
	if (zerop_ratio(a)) {
		fixnum_heap(ret, 0);
		return 0;
	}

	push_local(local, &stack);
	float_floor_rr(local, &a, ret, a, b);
	rollback_local(local, stack);

	return 0;
}

int mod_number_common_(LocalRoot local, addr a, addr b, addr *ret)
{
	enum MathType type;
	single_float vs, igs;
	double_float vd, igd;
	long_float vl, igl;
	struct mathreal2_struct str;

	Return(getmathreal2_addr_(&str, a, b, &type));
	switch (type) {
		case MathType_single:
			Return(float_floor_s_(str.v.s.a, str.v.s.b, &igs, &vs));
			return single_float_check_heap_(ret, vs);

		case MathType_double:
			Return(float_floor_d_(str.v.d.a, str.v.d.b, &igd, &vd));
			return double_float_check_heap_(ret, vd);

		case MathType_long:
			Return(float_floor_l_(str.v.l.a, str.v.l.b, &igl, &vl));
			return long_float_check_heap_(ret, vl);

		case MathType_rational:
			return mod_rational_common_(local, a, b, ret);

		case MathType_complex:
		case MathType_error:
		default:
			*ret = Nil;
			return fmte_("type error", NULL);
	}
}


/*
 *  rem -> truncate
 */
int float_rem_fixnum_(addr *ret, fixnum a, fixnum b)
{
	if (a == FIXNUM_MIN && b == -1) {
		fixnum_heap(ret, 0);
	}
	else {
		Return(float_truncate_f_(a, b, &a, &b));
		fixnum_heap(ret, b);
	}

	return 0;
}

int float_rem_bignum_(LocalRoot local, addr *ret, addr a, addr b)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		*ret = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_TRUNCATE, a, b);
	}
	if (zerop_bignum(a)) {
		fixnum_heap(ret, 0);
		return 0;
	}

	divrem_struct_initialize(local, &str, a, b);
	float_truncate_bb(&str);
	rem_struct_integer(&str, ret);

	return 0;
}

int float_rem_br_ratio_(LocalRoot local, addr *ret, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		*ret = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_TRUNCATE, a, b);
	}
	if (zerop_bignum(a)) {
		fixnum_heap(ret, 0);
		return 0;
	}

	push_local(local, &stack);
	float_truncate_br(local, &a, ret, a, b);
	rollback_local(local, stack);

	return 0;
}

int float_rem_rb_ratio_(LocalRoot local, addr *ret, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		*ret = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_TRUNCATE, a, b);
	}
	if (zerop_ratio(a)) {
		fixnum_heap(ret, 0);
		return 0;
	}

	push_local(local, &stack);
	float_truncate_rb(local, &a, ret, a, b);
	rollback_local(local, stack);

	return 0;
}

int float_rem_rr_ratio_(LocalRoot local, addr *ret, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		*ret = Nil;
		return call_division_by_zero_real2_(NULL, CONSTANT_COMMON_TRUNCATE, a, b);
	}
	if (zerop_ratio(a)) {
		fixnum_heap(ret, 0);
		return 0;
	}

	push_local(local, &stack);
	float_truncate_rr(local, &a, ret, a, b);
	rollback_local(local, stack);

	return 0;
}

int rem_number_common_(LocalRoot local, addr a, addr b, addr *ret)
{
	enum MathType type;
	single_float vs, igs;
	double_float vd, igd;
	long_float vl, igl;
	struct mathreal2_struct str;

	Return(getmathreal2_addr_(&str, a, b, &type));
	switch (type) {
		case MathType_single:
			Return(float_truncate_s_(str.v.s.a, str.v.s.b, &igs, &vs));
			return single_float_check_heap_(ret, vs);

		case MathType_double:
			Return(float_truncate_d_(str.v.d.a, str.v.d.b, &igd, &vd));
			return double_float_check_heap_(ret, vd);

		case MathType_long:
			Return(float_truncate_l_(str.v.l.a, str.v.l.b, &igl, &vl));
			return long_float_check_heap_(ret, vl);

		case MathType_rational:
			return rem_rational_common_(local, a, b, ret);

		case MathType_complex:
		case MathType_error:
		default:
			*ret = Nil;
			return fmte_("type error", NULL);
	}
}


/*
 *  integer-heap
 */
void single_float_integer_heap(LocalRoot local, addr *ret, single_float v)
{
	addr pos;
	LocalStack stack;

	push_local(local, &stack);
	bignum_single_float_unsafe(local, v, 0, &pos);
	bignum_result_heap(pos, ret);
	rollback_local(local, stack);
}

void double_float_integer_heap(LocalRoot local, addr *ret, double_float v)
{
	addr pos;
	LocalStack stack;

	push_local(local, &stack);
	bignum_double_float_unsafe(local, v, 0, &pos);
	bignum_result_heap(pos, ret);
	rollback_local(local, stack);
}

void long_float_integer_heap(LocalRoot local, addr *ret, long_float v)
{
	addr pos;
	LocalStack stack;

	push_local(local, &stack);
	bignum_long_float_unsafe(local, v, 0, &pos);
	bignum_result_heap(pos, ret);
	rollback_local(local, stack);
}

