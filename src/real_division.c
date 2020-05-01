#include "bigdata.h"
#include "bignum.h"
#include "condition.h"
#include "format.h"
#include "math_type.h"
#include "object.h"
#include "ratio.h"
#include "ratio_plus.h"
#include "ratio_multi.h"
#include "real_division.h"
#include "real_float.h"
#include "real_floor.h"
#include "real_truncate.h"
#include <float.h>
#include <math.h>

static void division_by_zero_f(constindex index, fixnum a, fixnum b)
{
	addr left, right;

	fixnum_heap(&left, a);
	fixnum_heap(&right, b);
	division_by_zero_real2(index, left, right);
}

static void division_by_zero_s(constindex index, single_float a, single_float b)
{
	addr left, right;

	single_float_heap(&left, a);
	single_float_heap(&right, b);
	division_by_zero_real2(index, left, right);
}

static void division_by_zero_d(constindex index, double_float a, double_float b)
{
	addr left, right;

	double_float_heap(&left, a);
	double_float_heap(&right, b);
	division_by_zero_real2(index, left, right);
}

static void division_by_zero_l(constindex index, long_float a, long_float b)
{
	addr left, right;

	long_float_heap(&left, a);
	long_float_heap(&right, b);
	division_by_zero_real2(index, left, right);
}


/*
 *  floor
 */
_g single_float lisp_floor_s(single_float a, single_float b, single_float *rem)
{
	single_float m;

	if (b == 0.0f) {
		division_by_zero_s(CONSTANT_COMMON_FLOOR, a, b);
		*rem = 0.0f;
		return 0.0f;
	}
	if (a == 0.0f) {
		*rem = 0.0f;
		return 0.0f;
	}

	m = fmodf(a, b);
	if (m == 0.0f) {
		*rem = 0.0f;
		return a / b;
	}
	if (0.0f < a) {
		if (b < 0.0f)
			goto floor2;
	}
	else if (0.0f < b) {
		goto floor2;
	}
	*rem = m;
	return floorf(a / b);

floor2:
	*rem = b + m;
	return floorf(a / b);
}

_g double_float lisp_floor_d(double_float a, double_float b, double_float *rem)
{
	double_float m;

	if (b == 0.0) {
		division_by_zero_d(CONSTANT_COMMON_FLOOR, a, b);
		*rem = 0.0;
		return 0.0;
	}
	if (a == 0.0) {
		*rem = 0.0;
		return 0.0;
	}

	m = fmod(a, b);
	if (m == 0.0) {
		*rem = 0.0;
		return a / b;
	}
	if (0.0 < a) {
		if (b < 0.0)
			goto floor2;
	}
	else if (0.0 < b) {
		goto floor2;
	}
	*rem = m;
	return floor(a / b);

floor2:
	*rem = b + m;
	return floor(a / b);
}

_g long_float lisp_floor_l(long_float a, long_float b, long_float *rem)
{
	long_float m;

	if (b == 0.0L) {
		division_by_zero_l(CONSTANT_COMMON_FLOOR, a, b);
		*rem = 0.0L;
		return 0.0L;
	}
	if (a == 0.0L) {
		*rem = 0.0L;
		return 0.0L;
	}

	m = fmodl(a, b);
	if (m == 0.0L) {
		*rem = 0.0L;
		return a / b;
	}
	if (0.0L < a) {
		if (b < 0.0L)
			goto floor2;
	}
	else if (0.0L < b) {
		goto floor2;
	}
	*rem = m;
	return floorl(a / b);

floor2:
	*rem = b + m;
	return floorl(a / b);
}

_g single_float lisp_floor1_s(single_float a, single_float *rem)
{
	single_float m;

	if (a == 0.0f) {
		*rem = 0.0f;
		return 0.0f;
	}

	m = fmodf(a, 1.0f);
	if (m == 0.0f) {
		*rem = 0.0f;
		return a;
	}
	if (0.0f < a)
		*rem = m;
	else
		*rem = 1.0f + m;

	return floorf(a);
}

_g double_float lisp_floor1_d(double_float a, double_float *rem)
{
	double_float m;

	if (a == 0.0) {
		*rem = 0.0;
		return 0.0;
	}

	m = fmod(a, 1.0);
	if (m == 0.0) {
		*rem = 0.0;
		return a;
	}
	if (0.0 < a)
		*rem = m;
	else
		*rem = 1.0 + m;

	return floor(a);
}

_g long_float lisp_floor1_l(long_float a, long_float *rem)
{
	long_float m;

	if (a == 0.0L) {
		*rem = 0.0L;
		return 0.0L;
	}

	m = fmodl(a, 1.0L);
	if (m == 0.0L) {
		*rem = 0.0L;
		return a;
	}
	if (0.0L < a)
		*rem = m;
	else
		*rem = 1.0L + m;

	return floorl(a);
}


/*
 *  ceiling
 */
_g single_float lisp_ceiling_s(single_float a, single_float b, single_float *rem)
{
	single_float m;

	if (b == 0.0f) {
		division_by_zero_s(CONSTANT_COMMON_CEILING, a, b);
		*rem = 0.0f;
		return 0.0f;
	}
	if (a == 0.0f) {
		*rem = 0.0f;
		return 0.0f;
	}

	m = fmodf(a, b);
	if (m == 0.0f) {
		*rem = 0.0f;
		return a / b;
	}
	if (0.0f < a) {
		if (b < 0.0f)
			goto ceiling2;
	}
	else if (0.0f < b) {
		goto ceiling2;
	}

	*rem = m - b;
	return ceilf(a / b);

ceiling2:
	*rem = m;
	return ceilf(a / b);
}

_g double_float lisp_ceiling_d(double_float a, double_float b, double_float *rem)
{
	double_float m;

	if (b == 0.0) {
		division_by_zero_d(CONSTANT_COMMON_CEILING, a, b);
		*rem = 0.0;
		return 0.0;
	}
	if (a == 0.0) {
		*rem = 0.0;
		return 0.0;
	}

	m = fmod(a, b);
	if (m == 0.0) {
		*rem = 0.0;
		return a / b;
	}
	if (0.0 < a) {
		if (b < 0.0)
			goto ceiling2;
	}
	else if (0.0 < b) {
		goto ceiling2;
	}

	*rem = m - b;
	return ceil(a / b);

ceiling2:
	*rem = m;
	return ceil(a / b);
}

_g long_float lisp_ceiling_l(long_float a, long_float b, long_float *rem)
{
	long_float m;

	if (b == 0.0L) {
		division_by_zero_l(CONSTANT_COMMON_CEILING, a, b);
		*rem = 0.0L;
		return 0.0L;
	}
	if (a == 0.0L) {
		*rem = 0.0L;
		return 0.0L;
	}

	m = fmodl(a, b);
	if (m == 0.0L) {
		*rem = 0.0L;
		return a / b;
	}
	if (0.0L < a) {
		if (b < 0.0L)
			goto ceiling2;
	}
	else if (0.0L < b) {
		goto ceiling2;
	}

	*rem = m - b;
	return ceill(a / b);

ceiling2:
	*rem = m;
	return ceill(a / b);
}

_g single_float lisp_ceiling1_s(single_float a, single_float *rem)
{
	single_float m;

	if (a == 0.0f) {
		*rem = 0.0f;
		return 0.0f;
	}

	m = fmodf(a, 1.0f);
	if (m == 0.0f) {
		*rem = 0.0f;
		return a;
	}
	if (0.0f < a)
		*rem = m - 1.0f;
	else
		*rem = m;

	return ceilf(a);
}

_g double_float lisp_ceiling1_d(double_float a, double_float *rem)
{
	double_float m;

	if (a == 0.0) {
		*rem = 0.0;
		return 0.0;
	}

	m = fmod(a, 1.0);
	if (m == 0.0) {
		*rem = 0.0;
		return a;
	}
	if (0.0 < a)
		*rem = m - 1.0;
	else
		*rem = m;

	return ceil(a);
}

_g long_float lisp_ceiling1_l(long_float a, long_float *rem)
{
	long_float m;

	if (a == 0.0L) {
		*rem = 0.0L;
		return 0.0L;
	}

	m = fmodl(a, 1.0L);
	if (m == 0.0L) {
		*rem = 0.0L;
		return a;
	}
	if (0.0L < a)
		*rem = m - 1.0L;
	else
		*rem = m;

	return ceill(a);
}


/*
 *  truncate
 */
_g single_float lisp_truncate_s(single_float a, single_float b, single_float *rem)
{
	single_float m;

	if (b == 0.0f) {
		division_by_zero_s(CONSTANT_COMMON_TRUNCATE, a, b);
		*rem = 0.0f;
		return 0.0f;
	}
	if (a == 0.0f) {
		*rem = 0.0f;
		return 0.0f;
	}

	m = fmodf(a, b);
	if (m == 0.0f) {
		*rem = 0.0f;
		return a / b;
	}
	*rem = m;

	if (0.0f < a) {
		if (b < 0.0f)
			goto ceiling;
	}
	else if (0.0f < b) {
		goto ceiling;
	}
	return floorf(a / b);

ceiling:
	return ceilf(a / b);
}

_g double_float lisp_truncate_d(double_float a, double_float b, double_float *rem)
{
	double_float m;

	if (b == 0.0) {
		division_by_zero_d(CONSTANT_COMMON_TRUNCATE, a, b);
		*rem = 0.0;
		return 0.0;
	}
	if (a == 0.0) {
		*rem = 0.0;
		return 0.0;
	}

	m = fmod(a, b);
	if (m == 0.0) {
		*rem = 0.0;
		return a / b;
	}
	*rem = m;

	if (0.0 < a) {
		if (b < 0.0)
			goto ceiling;
	}
	else if (0.0 < b) {
		goto ceiling;
	}
	return floor(a / b);

ceiling:
	return ceil(a / b);
}

_g long_float lisp_truncate_l(long_float a, long_float b, long_float *rem)
{
	long_float m;

	if (b == 0.0L) {
		division_by_zero_l(CONSTANT_COMMON_TRUNCATE, a, b);
		*rem = 0.0L;
		return 0.0L;
	}
	if (a == 0.0L) {
		*rem = 0.0L;
		return 0.0L;
	}

	m = fmodl(a, b);
	if (m == 0.0L) {
		*rem = 0.0L;
		return a / b;
	}
	*rem = m;

	if (0.0L < a) {
		if (b < 0.0L)
			goto ceiling;
	}
	else if (0.0L < b) {
		goto ceiling;
	}
	return floorl(a / b);

ceiling:
	return ceill(a / b);
}

_g single_float lisp_truncate1_s(single_float a, single_float *rem)
{
	single_float m;

	if (a == 0.0f) {
		*rem = 0.0f;
		return 0.0f;
	}

	m = fmodf(a, 1.0f);
	if (m == 0.0f) {
		*rem = 0.0f;
		return a;
	}
	*rem = m;

	return (0.0f < a)? floorf(a): ceilf(a);
}

_g double_float lisp_truncate1_d(double_float a, double_float *rem)
{
	double_float m;

	if (a == 0.0) {
		*rem = 0.0;
		return 0.0;
	}

	m = fmod(a, 1.0);
	if (m == 0.0) {
		*rem = 0.0;
		return a;
	}
	*rem = m;

	return (0.0 < a)? floor(a): ceil(a);
}

_g long_float lisp_truncate1_l(long_float a, long_float *rem)
{
	long_float m;

	if (a == 0.0L) {
		*rem = 0.0L;
		return 0.0L;
	}

	m = fmodl(a, 1.0L);
	if (m == 0.0L) {
		*rem = 0.0L;
		return a;
	}
	*rem = m;

	return (0.0L < a)? floorl(a): ceill(a);
}


/*
 *  round
 */
static int lisp_round_even_s(single_float f)
{
	return fmodf(f, 2.0f) == 0.0f;
}

_g single_float lisp_round_s(single_float a, single_float b, single_float *rem)
{
	single_float i, f;

	if (b == 0.0f) {
		division_by_zero_s(CONSTANT_COMMON_ROUND, a, b);
		*rem = 0.0f;
		return 0.0f;
	}
	f = modff(a / b, &i);
	if (0.0f <= f) {
		if (f < 0.5f)
			return lisp_floor_s(a, b, rem);
		else if (0.5f < f)
			return lisp_ceiling_s(a, b, rem);
		else if (lisp_round_even_s(i))
			return lisp_floor_s(a, b, rem);
		else
			return lisp_ceiling_s(a, b, rem);
	}
	else {
		if (-0.5f < f)
			return lisp_ceiling_s(a, b, rem);
		else if (f < -0.5f)
			return lisp_floor_s(a, b, rem);
		else if (lisp_round_even_s(i))
			return lisp_ceiling_s(a, b, rem);
		else
			return lisp_floor_s(a, b, rem);
	}
}

static int lisp_round_even_d(double_float f)
{
	return fmod(f, 2.0) == 0.0;
}

_g double_float lisp_round_d(double_float a, double_float b, double_float *rem)
{
	double_float i, f;

	if (b == 0.0) {
		division_by_zero_d(CONSTANT_COMMON_ROUND, a, b);
		*rem = 0.0;
		return 0.0;
	}
	f = modf(a / b, &i);
	if (0.0 <= f) {
		if (f < 0.5)
			return lisp_floor_d(a, b, rem);
		else if (0.5 < f)
			return lisp_ceiling_d(a, b, rem);
		else if (lisp_round_even_d(i))
			return lisp_floor_d(a, b, rem);
		else
			return lisp_ceiling_d(a, b, rem);
	}
	else {
		if (-0.5 < f)
			return lisp_ceiling_d(a, b, rem);
		else if (f < -0.5)
			return lisp_floor_d(a, b, rem);
		else if (lisp_round_even_d(i))
			return lisp_ceiling_d(a, b, rem);
		else
			return lisp_floor_d(a, b, rem);
	}
}

static int lisp_round_even_l(long_float f)
{
	return fmodl(f, 2.0L) == 0.0L;
}

_g long_float lisp_round_l(long_float a, long_float b, long_float *rem)
{
	long_float i, f;

	if (b == 0.0L) {
		division_by_zero_l(CONSTANT_COMMON_ROUND, a, b);
		*rem = 0.0L;
		return 0.0L;
	}
	f = modfl(a / b, &i);
	if (0.0L <= f) {
		if (f < 0.5L)
			return lisp_floor_l(a, b, rem);
		else if (0.5L < f)
			return lisp_ceiling_l(a, b, rem);
		else if (lisp_round_even_l(i))
			return lisp_floor_l(a, b, rem);
		else
			return lisp_ceiling_l(a, b, rem);
	}
	else {
		if (-0.5L < f)
			return lisp_ceiling_l(a, b, rem);
		else if (f < -0.5L)
			return lisp_floor_l(a, b, rem);
		else if (lisp_round_even_l(i))
			return lisp_ceiling_l(a, b, rem);
		else
			return lisp_floor_l(a, b, rem);
	}
}

_g single_float lisp_round1_s(single_float a, single_float *rem)
{
	single_float i, f;

	f = modff(a, &i);
	if (0.0f <= f) {
		if (f < 0.5f)
			return lisp_floor1_s(a, rem);
		else if (0.5f < f)
			return lisp_ceiling1_s(a, rem);
		else if (lisp_round_even_s(i))
			return lisp_floor1_s(a, rem);
		else
			return lisp_ceiling1_s(a, rem);
	}
	else {
		if (-0.5f < f)
			return lisp_ceiling1_s(a, rem);
		else if (f < -0.5f)
			return lisp_floor1_s(a, rem);
		else if (lisp_round_even_s(i))
			return lisp_ceiling1_s(a, rem);
		else
			return lisp_floor1_s(a, rem);
	}
}

_g double_float lisp_round1_d(double_float a, double_float *rem)
{
	double_float i, f;

	f = modf(a, &i);
	if (0.0 <= f) {
		if (f < 0.5)
			return lisp_floor1_d(a, rem);
		else if (0.5 < f)
			return lisp_ceiling1_d(a, rem);
		else if (lisp_round_even_d(i))
			return lisp_floor1_d(a, rem);
		else
			return lisp_ceiling1_d(a, rem);
	}
	else {
		if (-0.5 < f)
			return lisp_ceiling1_d(a, rem);
		else if (f < -0.5)
			return lisp_floor1_d(a, rem);
		else if (lisp_round_even_d(i))
			return lisp_ceiling1_d(a, rem);
		else
			return lisp_floor1_d(a, rem);
	}
}

_g long_float lisp_round1_l(long_float a, long_float *rem)
{
	long_float i, f;

	f = modfl(a, &i);
	if (0.0L <= f) {
		if (f < 0.5L)
			return lisp_floor1_l(a, rem);
		else if (0.5L < f)
			return lisp_ceiling1_l(a, rem);
		else if (lisp_round_even_l(i))
			return lisp_floor1_l(a, rem);
		else
			return lisp_ceiling1_l(a, rem);
	}
	else {
		if (-0.5L < f)
			return lisp_ceiling1_l(a, rem);
		else if (f < -0.5L)
			return lisp_floor1_l(a, rem);
		else if (lisp_round_even_l(i))
			return lisp_ceiling1_l(a, rem);
		else
			return lisp_floor1_l(a, rem);
	}
}


/*
 *  fixnum
 */
_g fixnum lisp_floor_f(fixnum a, fixnum b, fixnum *rem)
{
	fixnum q, r;

	if (b == 0) {
		division_by_zero_f(CONSTANT_COMMON_FLOOR, a, b);
		*rem = 0;
		return 0;
	}
	Check(a == FIXNUM_MIN && b == -1, "floor fixnum overflow.");

	q = a / b;
	r = a % b;
	if (r == 0) {
		*rem = 0;
		return q;
	}
	if (0 < a) {
		if (b < 0)
			goto floor2;
	}
	else if (0 < b) {
		goto floor2;
	}
	*rem = r;
	return q;

floor2:
	*rem = r + b;
	return q - 1;
}

_g fixnum lisp_ceiling_f(fixnum a, fixnum b, fixnum *rem)
{
	fixnum q, r;

	if (b == 0) {
		division_by_zero_f(CONSTANT_COMMON_CEILING, a, b);
		*rem = 0;
		return 0;
	}
	Check(a == FIXNUM_MIN && b == -1, "ceiling fixnum overflow.");

	q = a / b;
	r = a % b;
	if (r == 0) {
		*rem = 0;
		return q;
	}
	if (0 < a) {
		if (b < 0)
			goto ceiling2;
	}
	else if (0 < b) {
		goto ceiling2;
	}
	*rem = r - b;
	return q + 1;

ceiling2:
	*rem = r;
	return q;
}

_g fixnum lisp_truncate_f(fixnum a, fixnum b, fixnum *rem)
{
	if (b == 0) {
		division_by_zero_f(CONSTANT_COMMON_TRUNCATE, a, b);
		*rem = 0;
		return 0;
	}
	Check(a == FIXNUM_MIN && b == -1, "truncate fixnum overflow.");

	*rem = a % b;
	return a / b;
}

_g fixnum lisp_round_f(fixnum a, fixnum b, fixnum *rem)
{
	fixnum q, r, b2;

	/* error */
	if (b == 0) {
		division_by_zero_f(CONSTANT_COMMON_ROUND, a, b);
		*rem = 0;
		return 0;
	}

	/* |b| = 1 */
	if (b == 1) {
		*rem = 0;
		return a;
	}
	if (b == -1) {
		Check(a == FIXNUM_MIN && b == -1, "round fixnum overflow.");
		*rem = 0;
		return -a;
	}

	/* r = 0 */
	q = a / b;
	r = a % b;
	if (r == 0) {
		*rem = 0;
		return q;
	}

	/* plus, minus */
	if (0 < a) {
		if (0 < b) {
			/* a:plus, b:plus, q:plus, r:plus */
			b2 = b / 2;
			if (r < b2)
				return lisp_truncate_f(a, b, rem);
			if (b2 < r)
				return lisp_ceiling_f(a, b, rem);
			if (b % 2)
				return lisp_truncate_f(a, b, rem);
			else if (q % 2)
				return lisp_ceiling_f(a, b, rem);
			else
				return lisp_truncate_f(a, b, rem);
		}
		else {
			/* a:plus, b:minus, q:minus, r:plus */
			b2 = -(b / 2);
			if (r < b2)
				return lisp_truncate_f(a, b, rem);
			if (b2 < r)
				return lisp_floor_f(a, b, rem);
			if (b % 2)
				return lisp_truncate_f(a, b, rem);
			else if (q % 2)
				return lisp_floor_f(a, b, rem);
			else
				return lisp_truncate_f(a, b, rem);
		}
	}
	else {
		if (0 < b) {
			/* a:minus, b:plus, q:minus, r:minus */
			b2 = -(b / 2);
			if (b2 < r)
				return lisp_truncate_f(a, b, rem);
			if (r < b2)
				return lisp_floor_f(a, b, rem);
			if (b % 2)
				return lisp_truncate_f(a, b, rem);
			else if (q % 2)
				return lisp_floor_f(a, b, rem);
			else
				return lisp_truncate_f(a, b, rem);
		}
		else {
			/* a:minus, b:minus, q:plus, r:minus */
			b2 = b / 2;
			if (r < b2)
				return lisp_ceiling_f(a, b, rem);
			if (b2 < r)
				return lisp_truncate_f(a, b, rem);
			if (b % 2)
				return lisp_truncate_f(a, b, rem);
			else if (q % 2)
				return lisp_ceiling_f(a, b, rem);
			else
				return lisp_truncate_f(a, b, rem);
		}
	}
}

_g void lisp_floor_fixnum(addr *quot, addr *rem, fixnum a, fixnum b)
{
	if (a == FIXNUM_MIN && b == -1) {
		bignum_value_heap(quot, signminus_bignum, FIXNUM_UMIN);
		fixnum_heap(rem, 0);
	}
	else {
		a = lisp_floor_f(a, b, &b);
		fixnum_heap(quot, a);
		fixnum_heap(rem, b);
	}
}

_g void lisp_ceiling_fixnum(addr *quot, addr *rem, fixnum a, fixnum b)
{
	if (a == FIXNUM_MIN && b == -1) {
		bignum_value_heap(quot, signminus_bignum, FIXNUM_UMIN);
		fixnum_heap(rem, 0);
	}
	else {
		a = lisp_ceiling_f(a, b, &b);
		fixnum_heap(quot, a);
		fixnum_heap(rem, b);
	}
}

_g void lisp_truncate_fixnum(addr *quot, addr *rem, fixnum a, fixnum b)
{
	if (a == FIXNUM_MIN && b == -1) {
		bignum_value_heap(quot, signminus_bignum, FIXNUM_UMIN);
		fixnum_heap(rem, 0);
	}
	else {
		a = lisp_truncate_f(a, b, &b);
		fixnum_heap(quot, a);
		fixnum_heap(rem, b);
	}
}

_g void lisp_round_fixnum(addr *quot, addr *rem, fixnum a, fixnum b)
{
	if (a == FIXNUM_MIN && b == -1) {
		bignum_value_heap(quot, signminus_bignum, FIXNUM_UMIN);
		fixnum_heap(rem, 0);
	}
	else {
		a = lisp_round_f(a, b, &b);
		fixnum_heap(quot, a);
		fixnum_heap(rem, b);
	}
}

_g void lisp_ffloor_fixnum(addr *quot, addr *rem, fixnum a, fixnum b)
{
	if (a == FIXNUM_MIN && b == -1) {
		single_float_heap(quot, -((single_float)FIXNUM_UMIN));
		fixnum_heap(rem, 0);
	}
	else {
		a = lisp_floor_f(a, b, &b);
		single_float_heap(quot, (single_float)a);
		fixnum_heap(rem, b);
	}
}

_g void lisp_fceiling_fixnum(addr *quot, addr *rem, fixnum a, fixnum b)
{
	if (a == FIXNUM_MIN && b == -1) {
		single_float_heap(quot, -((single_float)FIXNUM_UMIN));
		fixnum_heap(rem, 0);
	}
	else {
		a = lisp_ceiling_f(a, b, &b);
		single_float_heap(quot, (single_float)a);
		fixnum_heap(rem, b);
	}
}

_g void lisp_ftruncate_fixnum(addr *quot, addr *rem, fixnum a, fixnum b)
{
	if (a == FIXNUM_MIN && b == -1) {
		single_float_heap(quot, -((single_float)FIXNUM_UMIN));
		fixnum_heap(rem, 0);
	}
	else {
		a = lisp_truncate_f(a, b, &b);
		single_float_heap(quot, (single_float)a);
		fixnum_heap(rem, b);
	}
}

_g void lisp_fround_fixnum(addr *quot, addr *rem, fixnum a, fixnum b)
{
	if (a == FIXNUM_MIN && b == -1) {
		single_float_heap(quot, -((single_float)FIXNUM_UMIN));
		fixnum_heap(rem, 0);
	}
	else {
		a = lisp_round_f(a, b, &b);
		single_float_heap(quot, (single_float)a);
		fixnum_heap(rem, b);
	}
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

static void divrem_struct_float(struct divrem_struct *ptr, addr *quot, addr *rem)
{
	single_float_bignum_heap(quot, ptr->quot);
	*rem = ptr->rem;
	rollback_local(ptr->local, ptr->stack);
#ifdef LISP_DEBUG
	ptr->local = (LocalRoot)Unbound;
#endif
}

static void divrem_struct_close(struct divrem_struct *ptr)
{
	rollback_local(ptr->local, ptr->stack);
#ifdef LISP_DEBUG
	ptr->local = (LocalRoot)Unbound;
#endif
}

static void lisp_floor_bb(struct divrem_struct *ptr)
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

_g void lisp_floor_bignum(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		division_by_zero_real2(CONSTANT_COMMON_FLOOR, a, b);
		return;
	}
	if (zerop_bignum(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return;
	}

	divrem_struct_initialize(local, &str, a, b);
	lisp_floor_bb(&str);
	divrem_struct_integer(&str, quot, rem);
}

_g void lisp_ffloor_bignum(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		division_by_zero_real2(CONSTANT_COMMON_FFLOOR, a, b);
		return;
	}
	if (zerop_bignum(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return;
	}

	divrem_struct_initialize(local, &str, a, b);
	lisp_floor_bb(&str);
	divrem_struct_float(&str, quot, rem);
}

static void lisp_ceiling_bb(struct divrem_struct *ptr)
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

_g void lisp_ceiling_bignum(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		division_by_zero_real2(CONSTANT_COMMON_CEILING, a, b);
		return;
	}
	if (zerop_bignum(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return;
	}

	divrem_struct_initialize(local, &str, a, b);
	lisp_ceiling_bb(&str);
	divrem_struct_integer(&str, quot, rem);
}

_g void lisp_fceiling_bignum(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		division_by_zero_real2(CONSTANT_COMMON_FCEILING, a, b);
		return;
	}
	if (zerop_bignum(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return;
	}

	divrem_struct_initialize(local, &str, a, b);
	lisp_ceiling_bb(&str);
	divrem_struct_float(&str, quot, rem);
}

static void lisp_truncate_bb(struct divrem_struct *ptr)
{
	divrem_struct_call(ptr);
	bignum_result_heap(ptr->rem, &(ptr->rem));
}

_g void lisp_truncate_bignum(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		division_by_zero_real2(CONSTANT_COMMON_TRUNCATE, a, b);
		return;
	}
	if (zerop_bignum(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return;
	}

	divrem_struct_initialize(local, &str, a, b);
	lisp_truncate_bb(&str);
	divrem_struct_integer(&str, quot, rem);
}

_g void lisp_ftruncate_bignum(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		division_by_zero_real2(CONSTANT_COMMON_FTRUNCATE, a, b);
		return;
	}
	if (zerop_bignum(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return;
	}

	divrem_struct_initialize(local, &str, a, b);
	lisp_truncate_bb(&str);
	divrem_struct_float(&str, quot, rem);
}

static int lisp_round_bb_check(struct divrem_struct *ptr)
{
	int sign, check;
	addr b;
	LocalRoot local;
	LocalStack stack;

	divrem_struct_call(ptr);
	if (zerop_bignum(ptr->rem)) {
		return 0;
	}

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

static void lisp_round_bb(struct divrem_struct *ptr)
{
	int check;

	Check(zerop_bignum(ptr->a), "zero error: a");
	Check(zerop_bignum(ptr->b), "zero error: b");

	check = lisp_round_bb_check(ptr);
	if (check < 0)
		lisp_floor_bb(ptr);
	else if (0 < check)
		lisp_ceiling_bb(ptr);
	else
		fixnum_heap(&(ptr->rem), 0);
}

_g void lisp_round_bignum(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		division_by_zero_real2(CONSTANT_COMMON_ROUND, a, b);
		return;
	}
	if (zerop_bignum(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return;
	}

	divrem_struct_initialize(local, &str, a, b);
	lisp_round_bb(&str);
	divrem_struct_integer(&str, quot, rem);
}

_g void lisp_fround_bignum(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		division_by_zero_real2(CONSTANT_COMMON_FROUND, a, b);
		return;
	}
	if (zerop_bignum(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return;
	}

	divrem_struct_initialize(local, &str, a, b);
	lisp_round_bb(&str);
	divrem_struct_float(&str, quot, rem);
}


/*
 *  ratio
 */
static void lisp_floor1_nosign(struct divrem_struct *ptr, int sign1, int sign2)
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

static void lisp_ceiling1_nosign(struct divrem_struct *ptr, int sign1, int sign2)
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

static void lisp_floor1_r(struct divrem_struct *ptr)
{
	int sign;

	GetSignRatio(ptr->pos, &sign);
	if (IsPlus(sign))
		lisp_floor1_nosign(ptr, SignPlus, SignPlus);
	else
		lisp_ceiling1_nosign(ptr, SignMinus, SignPlus);
}

_g void lisp_floor1_ratio(LocalRoot local, addr *quot, addr *rem, addr pos)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(pos, LISPTYPE_RATIO);
	if (zerop_ratio(pos)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return;
	}

	divrem_struct_initialize1(local, &str, pos);
	lisp_floor1_r(&str);
	divrem_struct_integer(&str, quot, rem);
}

static void lisp_ceiling1_r(struct divrem_struct *ptr)
{
	int sign;

	GetSignRatio(ptr->pos, &sign);
	if (IsPlus(sign))
		lisp_ceiling1_nosign(ptr, SignPlus, SignMinus);
	else
		lisp_floor1_nosign(ptr, SignMinus, SignMinus);
}

_g void lisp_ceiling1_ratio(LocalRoot local, addr *quot, addr *rem, addr pos)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(pos, LISPTYPE_RATIO);
	if (zerop_ratio(pos)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return;
	}

	divrem_struct_initialize1(local, &str, pos);
	lisp_ceiling1_r(&str);
	divrem_struct_integer(&str, quot, rem);
}

_g void lisp_truncate1_ratio(LocalRoot local, addr *quot, addr *rem, addr pos)
{
	int sign;
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(pos, LISPTYPE_RATIO);
	if (zerop_ratio(pos)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return;
	}

	divrem_struct_initialize1(local, &str, pos);
	GetSignRatio(pos, &sign);
	lisp_floor1_nosign(&str, sign, sign);
	divrem_struct_integer(&str, quot, rem);
}

static int lisp_round1_half(addr numer, addr denom)
{
	return equal_value_nosign_bignum(numer, 1)
		&& equal_value_nosign_bignum(denom, 2);
}

static void lisp_round1_r(struct divrem_struct *ptr)
{
	addr denom2;

	divrem_struct_call(ptr);
	if (lisp_round1_half(ptr->rem, ptr->b)) {
		/* rem = 1/2 */
		if (plusp_ratio(ptr->pos)) {
			if (evenp_bignum(ptr->quot))
				lisp_floor1_r(ptr);
			else
				lisp_ceiling1_r(ptr);
		}
		else {
			if (evenp_bignum(ptr->quot))
				lisp_ceiling1_r(ptr);
			else
				lisp_floor1_r(ptr);
		}
	}
	else {
		/* otherwise */
		division2_bigdata_alloc(ptr->local, &denom2, ptr->b);
		if (plusp_ratio(ptr->pos)) {
			if (compare_bb_real(ptr->rem, denom2) <= 0)
				lisp_floor1_r(ptr);
			else
				lisp_ceiling1_r(ptr);
		}
		else {
			if (compare_bb_real(ptr->rem, denom2) <= 0)
				lisp_ceiling1_r(ptr);
			else
				lisp_floor1_r(ptr);
		}
	}
}

_g void lisp_round1_ratio(LocalRoot local, addr *quot, addr *rem, addr pos)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(pos, LISPTYPE_RATIO);
	if (zerop_ratio(pos)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return;
	}

	divrem_struct_initialize1(local, &str, pos);
	lisp_round1_r(&str);
	divrem_struct_integer(&str, quot, rem);
}

_g void lisp_ffloor1_ratio(LocalRoot local, addr *quot, addr *rem, addr pos)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(pos, LISPTYPE_RATIO);
	if (zerop_ratio(pos)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return;
	}

	divrem_struct_initialize1(local, &str, pos);
	lisp_floor1_r(&str);
	divrem_struct_float(&str, quot, rem);
}

_g void lisp_fceiling1_ratio(LocalRoot local, addr *quot, addr *rem, addr pos)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(pos, LISPTYPE_RATIO);
	if (zerop_ratio(pos)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return;
	}

	divrem_struct_initialize1(local, &str, pos);
	lisp_ceiling1_r(&str);
	divrem_struct_float(&str, quot, rem);
}

_g void lisp_ftruncate1_ratio(LocalRoot local, addr *quot, addr *rem, addr pos)
{
	int sign;
	struct divrem_struct str;

	Check(local == NULL, "local error");
	if (zerop_ratio(pos)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return;
	}

	divrem_struct_initialize1(local, &str, pos);
	GetSignRatio(pos, &sign);
	lisp_floor1_nosign(&str, sign, sign);
	divrem_struct_float(&str, quot, rem);
}

_g void lisp_fround1_ratio(LocalRoot local, addr *quot, addr *rem, addr pos)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(pos, LISPTYPE_RATIO);
	if (zerop_ratio(pos)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return;
	}

	divrem_struct_initialize1(local, &str, pos);
	lisp_round1_r(&str);
	divrem_struct_float(&str, quot, rem);
}


/*
 *  bignum-ratio
 */
static int lisp_floor_br_plus(LocalRoot local, addr *ret, int sign, addr a, addr b)
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

static int lisp_floor_br_minus(LocalRoot local, addr *ret, int sign, addr a, addr b)
{
	int check;

	check = lisp_floor_br_plus(local, ret, sign, a, b);
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

_g void lisp_floor_br(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int sign, sign1, sign2, check;
	addr q, r;

	GetSignBignum(a, &sign1);
	GetSignRatio(b, &sign2);
	sign = SignMulti(sign1, sign2);
	if (IsPlus(sign))
		check = lisp_floor_br_plus(local, &q, sign, a, b);
	else
		check = lisp_floor_br_minus(local, &q, sign, a, b);
	if (check) {
		remainder_br_ratio(local, &r, a, b, q);
		ratio_result_noreduction_heap(local, r, rem);
	}
	else {
		fixnum_heap(rem, 0);
	}
	*quot = q;
}

_g void lisp_floor_br_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		division_by_zero_real2(CONSTANT_COMMON_FLOOR, a, b);
		return;
	}
	if (zerop_bignum(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return;
	}

	push_local(local, &stack);
	lisp_floor_br(local, quot, rem, a, b);
	bignum_result_heap(*quot, quot);
	rollback_local(local, stack);
}

static int lisp_ceiling_br_minus(LocalRoot local, addr *ret, int sign, addr a, addr b)
{
	int check;

	check = lisp_floor_br_plus(local, ret, sign, a, b);
	if (check)
		plus_bv_bignum_local(local, *ret, 1, ret);

	return check;
}

_g void lisp_ceiling_br(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int sign, sign1, sign2, check;
	addr q, r;

	GetSignBignum(a, &sign1);
	GetSignRatio(b, &sign2);
	sign = SignMulti(sign1, sign2);
	if (IsPlus(sign))
		check = lisp_ceiling_br_minus(local, &q, sign, a, b);
	else
		check = lisp_floor_br_plus(local, &q, sign, a, b);
	if (check) {
		remainder_br_ratio(local, &r, a, b, q);
		ratio_result_noreduction_heap(local, r, rem);
	}
	else {
		fixnum_heap(rem, 0);
	}
	*quot = q;
}

_g void lisp_ceiling_br_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		division_by_zero_real2(CONSTANT_COMMON_CEILING, a, b);
		return;
	}
	if (zerop_bignum(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return;
	}

	push_local(local, &stack);
	lisp_ceiling_br(local, quot, rem, a, b);
	bignum_result_heap(*quot, quot);
	rollback_local(local, stack);
}

_g void lisp_truncate_br(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int sign, sign1, sign2;
	addr q, r;

	GetSignBignum(a, &sign1);
	GetSignRatio(b, &sign2);
	sign = SignMulti(sign1, sign2);
	if (lisp_floor_br_plus(local, &q, sign, a, b)) {
		remainder_br_ratio(local, &r, a, b, q);
		ratio_result_noreduction_heap(local, r, rem);
	}
	else {
		fixnum_heap(rem, 0);
	}
	*quot = q;
}

_g void lisp_truncate_br_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		division_by_zero_real2(CONSTANT_COMMON_TRUNCATE, a, b);
		return;
	}
	if (zerop_bignum(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return;
	}

	push_local(local, &stack);
	lisp_truncate_br(local, quot, rem, a, b);
	bignum_result_heap(*quot, quot);
	rollback_local(local, stack);
}

static int lisp_round_br_check(LocalRoot local, addr a, addr b)
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
	check = lisp_round_bb_check(&str);
	divrem_struct_close(&str);
	rollback_local(local, stack);

	return check;
}

_g void lisp_round_br(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int check;

	check = lisp_round_br_check(local, a, b);
	if (check < 0)
		lisp_floor_br(local, quot, rem, a, b);
	else
		lisp_ceiling_br(local, quot, rem, a, b);
}

_g void lisp_round_br_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		division_by_zero_real2(CONSTANT_COMMON_ROUND, a, b);
		return;
	}
	if (zerop_bignum(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return;
	}

	push_local(local, &stack);
	lisp_round_br(local, quot, rem, a, b);
	bignum_result_heap(*quot, quot);
	rollback_local(local, stack);
}

_g void lisp_ffloor_br_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		division_by_zero_real2(CONSTANT_COMMON_FFLOOR, a, b);
		return;
	}
	if (zerop_bignum(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return;
	}

	push_local(local, &stack);
	lisp_floor_br(local, quot, rem, a, b);
	single_float_bignum_heap(quot, *quot);
	rollback_local(local, stack);
}

_g void lisp_fceiling_br_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		division_by_zero_real2(CONSTANT_COMMON_FCEILING, a, b);
		return;
	}
	if (zerop_bignum(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return;
	}

	push_local(local, &stack);
	lisp_ceiling_br(local, quot, rem, a, b);
	single_float_bignum_heap(quot, *quot);
	rollback_local(local, stack);
}

_g void lisp_ftruncate_br_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		division_by_zero_real2(CONSTANT_COMMON_FTRUNCATE, a, b);
		return;
	}
	if (zerop_bignum(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return;
	}

	push_local(local, &stack);
	lisp_truncate_br(local, quot, rem, a, b);
	single_float_bignum_heap(quot, *quot);
	rollback_local(local, stack);
}

_g void lisp_fround_br_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		division_by_zero_real2(CONSTANT_COMMON_FROUND, a, b);
		return;
	}
	if (zerop_bignum(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return;
	}

	push_local(local, &stack);
	lisp_round_br(local, quot, rem, a, b);
	single_float_bignum_heap(quot, *quot);
	rollback_local(local, stack);
}


/*
 *  ratio-bignum
 */
static int lisp_floor_rb_plus(LocalRoot local, addr *ret, int sign, addr a, addr b)
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

static int lisp_floor_rb_minus(LocalRoot local, addr *ret, int sign, addr a, addr b)
{
	int check;

	check = lisp_floor_rb_plus(local, ret, sign, a, b);
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

_g void lisp_floor_rb(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int sign, sign1, sign2, check;
	addr q, r;

	GetSignRatio(a, &sign1);
	GetSignBignum(b, &sign2);
	sign = SignMulti(sign1, sign2);
	if (IsPlus(sign))
		check = lisp_floor_rb_plus(local, &q, sign, a, b);
	else
		check = lisp_floor_rb_minus(local, &q, sign, a, b);
	if (check) {
		remainder_rb_ratio(local, &r, a, b, q);
		ratio_result_noreduction_heap(local, r, rem);
	}
	else {
		fixnum_heap(rem, 0);
	}
	*quot = q;
}

_g void lisp_floor_rb_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		division_by_zero_real2(CONSTANT_COMMON_FLOOR, a, b);
		return;
	}
	if (zerop_ratio(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return;
	}
	push_local(local, &stack);
	lisp_floor_rb(local, quot, rem, a, b);
	bignum_result_heap(*quot, quot);
	rollback_local(local, stack);
}

static int lisp_ceiling_rb_minus(LocalRoot local, addr *ret, int sign, addr a, addr b)
{
	int check;

	check = lisp_floor_rb_plus(local, ret, sign, a, b);
	if (check)
		plus_bv_bignum_local(local, *ret, 1, ret);

	return check;
}

_g void lisp_ceiling_rb(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int sign, sign1, sign2, check;
	addr q, r;

	GetSignRatio(a, &sign1);
	GetSignBignum(b, &sign2);
	sign = SignMulti(sign1, sign2);
	if (IsPlus(sign))
		check = lisp_ceiling_rb_minus(local, &q, sign, a, b);
	else
		check = lisp_floor_rb_plus(local, &q, sign, a, b);
	if (check) {
		remainder_rb_ratio(local, &r, a, b, q);
		ratio_result_noreduction_heap(local, r, rem);
	}
	else {
		fixnum_heap(rem, 0);
	}
	*quot = q;
}

_g void lisp_ceiling_rb_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		division_by_zero_real2(CONSTANT_COMMON_CEILING, a, b);
		return;
	}
	if (zerop_ratio(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return;
	}
	push_local(local, &stack);
	lisp_ceiling_rb(local, quot, rem, a, b);
	bignum_result_heap(*quot, quot);
	rollback_local(local, stack);
}

_g void lisp_truncate_rb(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int sign, sign1, sign2;
	addr q, r;

	GetSignRatio(a, &sign1);
	GetSignBignum(b, &sign2);
	sign = SignMulti(sign1, sign2);
	if (lisp_floor_rb_plus(local, &q, sign, a, b)) {
		remainder_rb_ratio(local, &r, a, b, q);
		ratio_result_noreduction_heap(local, r, rem);
	}
	else {
		fixnum_heap(rem, 0);
	}
	*quot = q;
}

_g void lisp_truncate_rb_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		division_by_zero_real2(CONSTANT_COMMON_TRUNCATE, a, b);
		return;
	}
	if (zerop_ratio(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return;
	}
	push_local(local, &stack);
	lisp_truncate_rb(local, quot, rem, a, b);
	bignum_result_heap(*quot, quot);
	rollback_local(local, stack);
}

static int lisp_round_rb_check(LocalRoot local, addr a, addr b)
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
	check = lisp_round_bb_check(&str);
	divrem_struct_close(&str);
	rollback_local(local, stack);

	return check;
}

_g void lisp_round_rb(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int check;

	check = lisp_round_rb_check(local, a, b);
	if (check < 0)
		lisp_floor_rb(local, quot, rem, a, b);
	else
		lisp_ceiling_rb(local, quot, rem, a, b);
}

_g void lisp_round_rb_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		division_by_zero_real2(CONSTANT_COMMON_ROUND, a, b);
		return;
	}
	if (zerop_ratio(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return;
	}
	push_local(local, &stack);
	lisp_round_rb(local, quot, rem, a, b);
	bignum_result_heap(*quot, quot);
	rollback_local(local, stack);
}

_g void lisp_ffloor_rb_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		division_by_zero_real2(CONSTANT_COMMON_FFLOOR, a, b);
		return;
	}
	if (zerop_ratio(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return;
	}
	push_local(local, &stack);
	lisp_floor_rb(local, quot, rem, a, b);
	single_float_bignum_heap(quot, *quot);
	rollback_local(local, stack);
}

_g void lisp_fceiling_rb_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		division_by_zero_real2(CONSTANT_COMMON_FCEILING, a, b);
		return;
	}
	if (zerop_ratio(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return;
	}
	push_local(local, &stack);
	lisp_ceiling_rb(local, quot, rem, a, b);
	single_float_bignum_heap(quot, *quot);
	rollback_local(local, stack);
}

_g void lisp_ftruncate_rb_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		division_by_zero_real2(CONSTANT_COMMON_FTRUNCATE, a, b);
		return;
	}
	if (zerop_ratio(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return;
	}
	push_local(local, &stack);
	lisp_truncate_rb(local, quot, rem, a, b);
	single_float_bignum_heap(quot, *quot);
	rollback_local(local, stack);
}

_g void lisp_fround_rb_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		division_by_zero_real2(CONSTANT_COMMON_FROUND, a, b);
		return;
	}
	if (zerop_ratio(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return;
	}
	push_local(local, &stack);
	lisp_round_rb(local, quot, rem, a, b);
	single_float_bignum_heap(quot, *quot);
	rollback_local(local, stack);
}


/*
 *  ratio-ratio
 */
static int lisp_floor_rr_plus(LocalRoot local, addr *ret, int sign, addr a, addr b)
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

static int lisp_floor_rr_minus(LocalRoot local, addr *ret, int sign, addr a, addr b)
{
	int check;

	check = lisp_floor_rr_plus(local, ret, sign, a, b);
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

_g void lisp_floor_rr(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int sign, sign1, sign2, check;
	addr q, r;

	GetSignRatio(a, &sign1);
	GetSignRatio(b, &sign2);
	sign = SignMulti(sign1, sign2);
	if (IsPlus(sign))
		check = lisp_floor_rr_plus(local, &q, sign, a, b);
	else
		check = lisp_floor_rr_minus(local, &q, sign, a, b);
	if (check) {
		remainder_rr_ratio(local, &r, a, b, q);
		ratio_result_noreduction_heap(local, r, rem);
	}
	else {
		fixnum_heap(rem, 0);
	}
	*quot = q;
}

_g void lisp_floor_rr_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		division_by_zero_real2(CONSTANT_COMMON_FLOOR, a, b);
		return;
	}
	if (zerop_ratio(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return;
	}
	push_local(local, &stack);
	lisp_floor_rr(local, quot, rem, a, b);
	bignum_result_heap(*quot, quot);
	rollback_local(local, stack);
}

static int lisp_ceiling_rr_minus(LocalRoot local, addr *ret, int sign, addr a, addr b)
{
	int check;

	check = lisp_floor_rr_plus(local, ret, sign, a, b);
	if (check)
		plus_bv_bignum_local(local, *ret, 1, ret);

	return check;
}

_g void lisp_ceiling_rr(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int sign, sign1, sign2, check;
	addr q, r;

	GetSignRatio(a, &sign1);
	GetSignRatio(b, &sign2);
	sign = SignMulti(sign1, sign2);
	if (IsPlus(sign))
		check = lisp_ceiling_rr_minus(local, &q, sign, a, b);
	else
		check = lisp_floor_rr_plus(local, &q, sign, a, b);
	if (check) {
		remainder_rr_ratio(local, &r, a, b, q);
		ratio_result_noreduction_heap(local, r, rem);
	}
	else {
		fixnum_heap(rem, 0);
	}
	*quot = q;
}

_g void lisp_ceiling_rr_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		division_by_zero_real2(CONSTANT_COMMON_CEILING, a, b);
		return;
	}
	if (zerop_ratio(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return;
	}
	push_local(local, &stack);
	lisp_ceiling_rr(local, quot, rem, a, b);
	bignum_result_heap(*quot, quot);
	rollback_local(local, stack);
}

_g void lisp_truncate_rr(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int sign, sign1, sign2;
	addr q, r;

	GetSignRatio(a, &sign1);
	GetSignRatio(b, &sign2);
	sign = SignMulti(sign1, sign2);
	if (lisp_floor_rr_plus(local, &q, sign, a, b)) {
		remainder_rr_ratio(local, &r, a, b, q);
		ratio_result_noreduction_heap(local, r, rem);
	}
	else {
		fixnum_heap(rem, 0);
	}
	*quot = q;
}

_g void lisp_truncate_rr_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		division_by_zero_real2(CONSTANT_COMMON_TRUNCATE, a, b);
		return;
	}
	if (zerop_ratio(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return;
	}
	push_local(local, &stack);
	lisp_truncate_rr(local, quot, rem, a, b);
	bignum_result_heap(*quot, quot);
	rollback_local(local, stack);
}

static int lisp_round_rr_check(LocalRoot local, addr a, addr b)
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
	check = lisp_round_bb_check(&str);
	divrem_struct_close(&str);
	rollback_local(local, stack);

	return check;
}

_g void lisp_round_rr(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int check;

	check = lisp_round_rr_check(local, a, b);
	if (check < 0)
		lisp_floor_rr(local, quot, rem, a, b);
	else
		lisp_ceiling_rr(local, quot, rem, a, b);
}

_g void lisp_round_rr_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		division_by_zero_real2(CONSTANT_COMMON_ROUND, a, b);
		return;
	}
	if (zerop_ratio(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return;
	}
	push_local(local, &stack);
	lisp_round_rr(local, quot, rem, a, b);
	bignum_result_heap(*quot, quot);
	rollback_local(local, stack);
}

_g void lisp_ffloor_rr_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		division_by_zero_real2(CONSTANT_COMMON_FFLOOR, a, b);
		return;
	}
	if (zerop_ratio(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return;
	}
	push_local(local, &stack);
	lisp_floor_rr(local, quot, rem, a, b);
	single_float_bignum_heap(quot, *quot);
	rollback_local(local, stack);
}

_g void lisp_fceiling_rr_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		division_by_zero_real2(CONSTANT_COMMON_FCEILING, a, b);
		return;
	}
	if (zerop_ratio(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return;
	}
	push_local(local, &stack);
	lisp_ceiling_rr(local, quot, rem, a, b);
	single_float_bignum_heap(quot, *quot);
	rollback_local(local, stack);
}

_g void lisp_ftruncate_rr_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		division_by_zero_real2(CONSTANT_COMMON_FTRUNCATE, a, b);
		return;
	}
	if (zerop_ratio(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return;
	}
	push_local(local, &stack);
	lisp_truncate_rr(local, quot, rem, a, b);
	single_float_bignum_heap(quot, *quot);
	rollback_local(local, stack);
}

_g void lisp_fround_rr_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		division_by_zero_real2(CONSTANT_COMMON_FROUND, a, b);
		return;
	}
	if (zerop_ratio(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return;
	}
	push_local(local, &stack);
	lisp_round_rr(local, quot, rem, a, b);
	single_float_bignum_heap(quot, *quot);
	rollback_local(local, stack);
}


/*
 *  mod -> floor
 */
_g void lisp_mod_fixnum(addr *ret, fixnum a, fixnum b)
{
	if (a == FIXNUM_MIN && b == -1) {
		fixnum_heap(ret, 0);
	}
	else {
		(void)lisp_floor_f(a, b, &b);
		fixnum_heap(ret, b);
	}
}

_g void lisp_mod_bignum(LocalRoot local, addr *ret, addr a, addr b)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		division_by_zero_real2(CONSTANT_COMMON_FLOOR, a, b);
		return;
	}
	if (zerop_bignum(a)) {
		fixnum_heap(ret, 0);
		return;
	}

	divrem_struct_initialize(local, &str, a, b);
	lisp_floor_bb(&str);
	rem_struct_integer(&str, ret);
}

_g void lisp_mod_br_ratio(LocalRoot local, addr *ret, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		division_by_zero_real2(CONSTANT_COMMON_FLOOR, a, b);
		return;
	}
	if (zerop_bignum(a)) {
		fixnum_heap(ret, 0);
		return;
	}

	push_local(local, &stack);
	lisp_floor_br(local, &a, ret, a, b);
	rollback_local(local, stack);
}

_g void lisp_mod_rb_ratio(LocalRoot local, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		division_by_zero_real2(CONSTANT_COMMON_FLOOR, a, b);
		return;
	}
	if (zerop_ratio(a)) {
		fixnum_heap(rem, 0);
		return;
	}
	push_local(local, &stack);
	lisp_floor_rb(local, &a, rem, a, b);
	rollback_local(local, stack);
}

_g void lisp_mod_rr_ratio(LocalRoot local, addr *ret, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		division_by_zero_real2(CONSTANT_COMMON_FLOOR, a, b);
		return;
	}
	if (zerop_ratio(a)) {
		fixnum_heap(ret, 0);
		return;
	}
	push_local(local, &stack);
	lisp_floor_rr(local, &a, ret, a, b);
	rollback_local(local, stack);
}

_g void mod_number_common(LocalRoot local, addr a, addr b, addr *ret)
{
	single_float vs;
	double_float vd;
	long_float vl;
	struct mathreal2_struct str;

	switch (getmathreal2_addr(&str, a, b)) {
		case MathType_single:
			lisp_floor_s(str.v.s.a, str.v.s.b, &vs);
			single_float_check_heap(ret, vs);
			break;

		case MathType_double:
			lisp_floor_d(str.v.d.a, str.v.d.b, &vd);
			double_float_check_heap(ret, vd);
			break;

		case MathType_long:
			lisp_floor_l(str.v.l.a, str.v.l.b, &vl);
			long_float_check_heap(ret, vl);
			break;

		case MathType_rational:
			mod_rational_common(local, a, b, ret);
			break;

		case MathType_complex:
		case MathType_error:
		default:
			fmte("type error", NULL);
			break;
	}
}


/*
 *  rem -> truncate
 */
_g void lisp_rem_fixnum(addr *ret, fixnum a, fixnum b)
{
	if (a == FIXNUM_MIN && b == -1) {
		fixnum_heap(ret, 0);
	}
	else {
		(void)lisp_truncate_f(a, b, &b);
		fixnum_heap(ret, b);
	}
}

_g void lisp_rem_bignum(LocalRoot local, addr *ret, addr a, addr b)
{
	struct divrem_struct str;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		division_by_zero_real2(CONSTANT_COMMON_TRUNCATE, a, b);
		return;
	}
	if (zerop_bignum(a)) {
		fixnum_heap(ret, 0);
		return;
	}

	divrem_struct_initialize(local, &str, a, b);
	lisp_truncate_bb(&str);
	rem_struct_integer(&str, ret);
}

_g void lisp_rem_br_ratio(LocalRoot local, addr *ret, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		division_by_zero_real2(CONSTANT_COMMON_TRUNCATE, a, b);
		return;
	}
	if (zerop_bignum(a)) {
		fixnum_heap(ret, 0);
		return;
	}

	push_local(local, &stack);
	lisp_truncate_br(local, &a, ret, a, b);
	rollback_local(local, stack);
}

_g void lisp_rem_rb_ratio(LocalRoot local, addr *ret, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		division_by_zero_real2(CONSTANT_COMMON_TRUNCATE, a, b);
		return;
	}
	if (zerop_ratio(a)) {
		fixnum_heap(ret, 0);
		return;
	}
	push_local(local, &stack);
	lisp_truncate_rb(local, &a, ret, a, b);
	rollback_local(local, stack);
}

_g void lisp_rem_rr_ratio(LocalRoot local, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_RATIO);
	if (zerop_ratio(b)) {
		division_by_zero_real2(CONSTANT_COMMON_TRUNCATE, a, b);
		return;
	}
	if (zerop_ratio(a)) {
		fixnum_heap(rem, 0);
		return;
	}
	push_local(local, &stack);
	lisp_truncate_rr(local, &a, rem, a, b);
	rollback_local(local, stack);
}

_g void rem_number_common(LocalRoot local, addr a, addr b, addr *ret)
{
	single_float vs;
	double_float vd;
	long_float vl;
	struct mathreal2_struct str;

	switch (getmathreal2_addr(&str, a, b)) {
		case MathType_single:
			lisp_truncate_s(str.v.s.a, str.v.s.b, &vs);
			single_float_check_heap(ret, vs);
			break;

		case MathType_double:
			lisp_truncate_d(str.v.d.a, str.v.d.b, &vd);
			double_float_check_heap(ret, vd);
			break;

		case MathType_long:
			lisp_truncate_l(str.v.l.a, str.v.l.b, &vl);
			long_float_check_heap(ret, vl);
			break;

		case MathType_rational:
			rem_rational_common(local, a, b, ret);
			break;

		case MathType_complex:
		case MathType_error:
		default:
			fmte("type error", NULL);
			break;
	}
}


/*
 *  integer-heap
 */
_g void single_float_integer_heap(LocalRoot local, addr *ret, single_float v)
{
	addr pos;
	LocalStack stack;

	push_local(local, &stack);
	if (bignum_single_float_local(local, &pos, v))
		fmte("bignum_single_float_local error.", NULL);
	bignum_result_heap(pos, ret);
	rollback_local(local, stack);
}

_g void double_float_integer_heap(LocalRoot local, addr *ret, double_float v)
{
	addr pos;
	LocalStack stack;

	push_local(local, &stack);
	if (bignum_double_float_local(local, &pos, v))
		fmte("bignum_double_float_local error.", NULL);
	bignum_result_heap(pos, ret);
	rollback_local(local, stack);
}

_g void long_float_integer_heap(LocalRoot local, addr *ret, long_float v)
{
	addr pos;
	LocalStack stack;

	push_local(local, &stack);
	if (bignum_long_float_local(local, &pos, v))
		fmte("bignum_long_float_local error.", NULL);
	bignum_result_heap(pos, ret);
	rollback_local(local, stack);
}

