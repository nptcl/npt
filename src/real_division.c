#include "bigdata.h"
#include "bignum.h"
#include "condition.h"
#include "object.h"
#include "ratio.h"
#include "real_division.h"
#include "real_float.h"
#include <float.h>
#include <math.h>

static void division_by_zero_f(constindex index, fixnum a, fixnum b)
{
	addr left, right;

	fixnum_heap(&left, a);
	fixnum_heap(&right, b);
	division_by_zero_stdarg(index, left, right, NULL);
}

static void division_by_zero_s(constindex index, single_float a, single_float b)
{
	addr left, right;

	single_float_heap(&left, a);
	single_float_heap(&right, b);
	division_by_zero_stdarg(index, left, right, NULL);
}

static void division_by_zero_d(constindex index, double_float a, double_float b)
{
	addr left, right;

	double_float_heap(&left, a);
	double_float_heap(&right, b);
	division_by_zero_stdarg(index, left, right, NULL);
}

static void division_by_zero_l(constindex index, long_float a, long_float b)
{
	addr left, right;

	long_float_heap(&left, a);
	long_float_heap(&right, b);
	division_by_zero_stdarg(index, left, right, NULL);
}


/*
 *  floor
 */
single_float lisp_floor_s(single_float a, single_float b, single_float *rem)
{
	single_float m;

	if (b == 0.0f) {
		division_by_zero_s(CONSTANT_COMMON_FLOOR, a, b);
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

double_float lisp_floor_d(double_float a, double_float b, double_float *rem)
{
	double_float m;

	if (b == 0.0) {
		division_by_zero_d(CONSTANT_COMMON_FLOOR, a, b);
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

long_float lisp_floor_l(long_float a, long_float b, long_float *rem)
{
	long_float m;

	if (b == 0.0L) {
		division_by_zero_l(CONSTANT_COMMON_FLOOR, a, b);
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

single_float lisp_floor1_s(single_float a, single_float *rem)
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

double_float lisp_floor1_d(double_float a, double_float *rem)
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

long_float lisp_floor1_l(long_float a, long_float *rem)
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
single_float lisp_ceiling_s(single_float a, single_float b, single_float *rem)
{
	single_float m;

	if (b == 0.0f) {
		division_by_zero_s(CONSTANT_COMMON_CEILING, a, b);
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

double_float lisp_ceiling_d(double_float a, double_float b, double_float *rem)
{
	double_float m;

	if (b == 0.0) {
		division_by_zero_d(CONSTANT_COMMON_CEILING, a, b);
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

long_float lisp_ceiling_l(long_float a, long_float b, long_float *rem)
{
	long_float m;

	if (b == 0.0L) {
		division_by_zero_l(CONSTANT_COMMON_CEILING, a, b);
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

single_float lisp_ceiling1_s(single_float a, single_float *rem)
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

double_float lisp_ceiling1_d(double_float a, double_float *rem)
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

long_float lisp_ceiling1_l(long_float a, long_float *rem)
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
single_float lisp_truncate_s(single_float a, single_float b, single_float *rem)
{
	single_float m;

	if (b == 0.0f) {
		division_by_zero_s(CONSTANT_COMMON_TRUNCATE, a, b);
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

double_float lisp_truncate_d(double_float a, double_float b, double_float *rem)
{
	double_float m;

	if (b == 0.0) {
		division_by_zero_d(CONSTANT_COMMON_TRUNCATE, a, b);
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

long_float lisp_truncate_l(long_float a, long_float b, long_float *rem)
{
	long_float m;

	if (b == 0.0L) {
		division_by_zero_l(CONSTANT_COMMON_TRUNCATE, a, b);
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

single_float lisp_truncate1_s(single_float a, single_float *rem)
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

double_float lisp_truncate1_d(double_float a, double_float *rem)
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

long_float lisp_truncate1_l(long_float a, long_float *rem)
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

single_float lisp_round_s(single_float a, single_float b, single_float *rem)
{
	single_float i, f;

	if (b == 0.0f) {
		division_by_zero_s(CONSTANT_COMMON_ROUND, a, b);
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

double_float lisp_round_d(double_float a, double_float b, double_float *rem)
{
	double_float i, f;

	if (b == 0.0) {
		division_by_zero_d(CONSTANT_COMMON_ROUND, a, b);
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

long_float lisp_round_l(long_float a, long_float b, long_float *rem)
{
	long_float i, f;

	if (b == 0.0L) {
		division_by_zero_l(CONSTANT_COMMON_ROUND, a, b);
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

single_float lisp_round1_s(single_float a, single_float *rem)
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

double_float lisp_round1_d(double_float a, double_float *rem)
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

long_float lisp_round1_l(long_float a, long_float *rem)
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
fixnum lisp_floor_f(fixnum a, fixnum b, fixnum *rem)
{
	fixnum q, r;

	if (b == 0) {
		division_by_zero_f(CONSTANT_COMMON_FLOOR, a, b);
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

fixnum lisp_ceiling_f(fixnum a, fixnum b, fixnum *rem)
{
	fixnum q, r;

	if (b == 0) {
		division_by_zero_f(CONSTANT_COMMON_CEILING, a, b);
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

fixnum lisp_truncate_f(fixnum a, fixnum b, fixnum *rem)
{
	if (b == 0) {
		division_by_zero_f(CONSTANT_COMMON_TRUNCATE, a, b);
	}
	Check(a == FIXNUM_MIN && b == -1, "truncate fixnum overflow.");

	*rem = a % b;
	return a / b;
}

fixnum lisp_round_f(fixnum a, fixnum b, fixnum *rem)
{
	fixnum q, r, b2;

	/* error */
	if (b == 0) {
		division_by_zero_f(CONSTANT_COMMON_ROUND, a, b);
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

void lisp_floor_fixnum(addr *quot, addr *rem, fixnum a, fixnum b)
{
	if (a == FIXNUM_MIN && b == -1) {
		bignum_value_alloc(NULL, quot, signminus_bignum, FIXNUM_UMIN);
		fixnum_heap(rem, 0);
	}
	else {
		a = lisp_floor_f(a, b, &b);
		fixnum_heap(quot, a);
		fixnum_heap(rem, b);
	}
}

void lisp_ceiling_fixnum(addr *quot, addr *rem, fixnum a, fixnum b)
{
	if (a == FIXNUM_MIN && b == -1) {
		bignum_value_alloc(NULL, quot, signminus_bignum, FIXNUM_UMIN);
		fixnum_heap(rem, 0);
	}
	else {
		a = lisp_ceiling_f(a, b, &b);
		fixnum_heap(quot, a);
		fixnum_heap(rem, b);
	}
}

void lisp_truncate_fixnum(addr *quot, addr *rem, fixnum a, fixnum b)
{
	if (a == FIXNUM_MIN && b == -1) {
		bignum_value_alloc(NULL, quot, signminus_bignum, FIXNUM_UMIN);
		fixnum_heap(rem, 0);
	}
	else {
		a = lisp_truncate_f(a, b, &b);
		fixnum_heap(quot, a);
		fixnum_heap(rem, b);
	}
}

void lisp_round_fixnum(addr *quot, addr *rem, fixnum a, fixnum b)
{
	if (a == FIXNUM_MIN && b == -1) {
		bignum_value_alloc(NULL, quot, signminus_bignum, FIXNUM_UMIN);
		fixnum_heap(rem, 0);
	}
	else {
		a = lisp_round_f(a, b, &b);
		fixnum_heap(quot, a);
		fixnum_heap(rem, b);
	}
}

void lisp_ffloor_fixnum(addr *quot, addr *rem, fixnum a, fixnum b)
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

void lisp_fceiling_fixnum(addr *quot, addr *rem, fixnum a, fixnum b)
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

void lisp_ftruncate_fixnum(addr *quot, addr *rem, fixnum a, fixnum b)
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

void lisp_fround_fixnum(addr *quot, addr *rem, fixnum a, fixnum b)
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
static void lisp_floor_b(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int sign, sign1, sign2;
	addr q, r;

	GetSignBignum(a, &sign1);
	GetSignBignum(b, &sign2);
	sign = SignMulti(sign1, sign2);
	divrem_bigdata_local(local, &q, &r, a, b);
	SetSignBignum(q, sign);
	SetSignBignum(r, sign2);
	if (zerop_bignum(r)) {
		bignum_result_heap(q, quot);
		fixnum_heap(rem, 0);
		return;
	}
	if (plusp_bignum(a)) {
		if (minusp_bignum(b))
			goto floor2;
	}
	else if (plusp_bignum(b)) {
		goto floor2;
	}
	bignum_result_heap(q, quot);
	bignum_result_heap(r, rem);
	return;

floor2:
	plus_fv_bignum_local(local, q, -1, &q);
	plus_bb_bignum_local(local, r, b, &r);
	bignum_result_heap(q, quot);
	bignum_result_heap(r, rem);
}

void lisp_floor_bignum(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		division_by_zero_stdarg(CONSTANT_COMMON_FLOOR, a, b, NULL);
	}
	if (zerop_bignum(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return;
	}

	push_local(local, &stack);
	lisp_floor_b(local, quot, rem, a, b);
	rollback_local(local, stack);
}

static void lisp_ffloor_b(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int sign, sign1, sign2;
	addr q, r;

	GetSignBignum(a, &sign1);
	GetSignBignum(b, &sign2);
	sign = SignMulti(sign1, sign2);
	divrem_bigdata_local(local, &q, &r, a, b);
	SetSignBignum(q, sign);
	SetSignBignum(r, sign2);
	if (zerop_bignum(r)) {
		single_float_bignum_heap(quot, q);
		fixnum_heap(rem, 0);
		return;
	}
	if (plusp_bignum(a)) {
		if (minusp_bignum(b))
			goto floor2;
	}
	else if (plusp_bignum(b)) {
		goto floor2;
	}
	single_float_bignum_heap(quot, q);
	bignum_result_heap(r, rem);
	return;

floor2:
	plus_fv_bignum_local(local, q, -1, &q);
	plus_bb_bignum_local(local, r, b, &r);
	single_float_bignum_heap(quot, q);
	bignum_result_heap(r, rem);
}

void lisp_ffloor_bignum(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		division_by_zero_stdarg(CONSTANT_COMMON_FFLOOR, a, b, NULL);
	}
	if (zerop_bignum(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return;
	}

	push_local(local, &stack);
	lisp_ffloor_b(local, quot, rem, a, b);
	rollback_local(local, stack);
}

static void lisp_ceiling_b(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int sign, sign1, sign2;
	addr q, r;

	GetSignBignum(a, &sign1);
	GetSignBignum(b, &sign2);
	sign = SignMulti(sign1, sign2);
	divrem_bigdata_local(local, &q, &r, a, b);
	SetSignBignum(q, sign);
	SetSignBignum(r, sign2);
	if (zerop_bignum(r)) {
		bignum_result_heap(q, quot);
		fixnum_heap(rem, 0);
		return;
	}
	if (plusp_bignum(a)) {
		if (minusp_bignum(b))
			goto ceiling2;
	}
	else if (plusp_bignum(b)) {
		goto ceiling2;
	}
	plus_fv_bignum_local(local, q, 1, &q);
	minus_bb_real_local(local, r, b, &r);
	bignum_result_heap(q, quot);
	bignum_result_heap(r, rem);
	return;

ceiling2:
	bignum_result_heap(q, quot);
	bignum_result_heap(r, rem);
}

void lisp_ceiling_bignum(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		division_by_zero_stdarg(CONSTANT_COMMON_CEILING, a, b, NULL);
	}
	if (zerop_bignum(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return;
	}

	push_local(local, &stack);
	lisp_ceiling_b(local, quot, rem, a, b);
	rollback_local(local, stack);
}

static void lisp_fceiling_b(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int sign, sign1, sign2;
	addr q, r;

	GetSignBignum(a, &sign1);
	GetSignBignum(b, &sign2);
	sign = SignMulti(sign1, sign2);
	divrem_bigdata_local(local, &q, &r, a, b);
	SetSignBignum(q, sign);
	SetSignBignum(r, sign2);
	if (zerop_bignum(r)) {
		single_float_bignum_heap(quot, q);
		fixnum_heap(rem, 0);
		return;
	}
	if (plusp_bignum(a)) {
		if (minusp_bignum(b))
			goto ceiling2;
	}
	else if (plusp_bignum(b)) {
		goto ceiling2;
	}
	plus_fv_bignum_local(local, q, 1, &q);
	minus_bb_real_local(local, r, b, &r);
	single_float_bignum_heap(quot, q);
	bignum_result_heap(r, rem);
	return;

ceiling2:
	single_float_bignum_heap(quot, q);
	bignum_result_heap(r, rem);
}

void lisp_fceiling_bignum(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		division_by_zero_stdarg(CONSTANT_COMMON_FCEILING, a, b, NULL);
	}
	if (zerop_bignum(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return;
	}

	push_local(local, &stack);
	lisp_fceiling_b(local, quot, rem, a, b);
	rollback_local(local, stack);
}

static void lisp_truncate_b(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int sign, sign1, sign2;
	addr q, r;

	GetSignBignum(a, &sign1);
	GetSignBignum(b, &sign2);
	sign = SignMulti(sign1, sign2);
	divrem_bigdata_local(local, &q, &r, a, b);
	SetSignBignum(q, sign);
	SetSignBignum(r, sign2);
	bignum_result_heap(q, quot);
	bignum_result_heap(r, rem);
}

void lisp_truncate_bignum(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		division_by_zero_stdarg(CONSTANT_COMMON_TRUNCATE, a, b, NULL);
	}
	if (zerop_bignum(a)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return;
	}

	push_local(local, &stack);
	lisp_truncate_b(local, quot, rem, a, b);
	rollback_local(local, stack);
}

static void lisp_ftruncate_b(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int sign, sign1, sign2;
	addr q, r;

	GetSignBignum(a, &sign1);
	GetSignBignum(b, &sign2);
	sign = SignMulti(sign1, sign2);
	divrem_bigdata_local(local, &q, &r, a, b);
	SetSignBignum(q, sign);
	SetSignBignum(r, sign2);
	single_float_bignum_heap(quot, q);
	bignum_result_heap(r, rem);
}

void lisp_ftruncate_bignum(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		division_by_zero_stdarg(CONSTANT_COMMON_FTRUNCATE, a, b, NULL);
	}
	if (zerop_bignum(a)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return;
	}

	push_local(local, &stack);
	lisp_ftruncate_b(local, quot, rem, a, b);
	rollback_local(local, stack);
}

static void lisp_round_b2_plus(LocalRoot local, addr *ret, addr pos)
{
	int sign;

	GetSignBignum(pos, &sign);
	division2_bigdata_alloc(local, ret, pos);
	SetSignBignum(*ret, sign);
}

static void lisp_round_b2_minus(LocalRoot local, addr *ret, addr pos)
{
	int sign;

	GetSignBignum(pos, &sign);
	division2_bigdata_alloc(local, ret, pos);
	SetSignBignum(*ret, SignNot(sign));
}

static void lisp_round_b(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int sign, sign1, sign2;
	addr q, r, b2;

	/* r = 0 */
	GetSignBignum(a, &sign1);
	GetSignBignum(b, &sign2);
	sign = SignMulti(sign1, sign2);
	divrem_bigdata_local(local, &q, &r, a, b);
	SetSignBignum(q, sign);
	SetSignBignum(r, sign2);
	if (zerop_bignum(r)) {
		bignum_result_heap(q, quot);
		fixnum_heap(rem, 0);
		return;
	}

	/* plus, minus */
	if (plusp_bignum(a)) {
		if (plusp_bignum(b)) {
			/* a:plus, b:plus, q:plus, r:plus */
			lisp_round_b2_plus(local, &b2, b);
			if (compare_bb_real(r, b2) < 0)
				lisp_truncate_bignum(local, quot, rem, a, b);
			else if (compare_bb_real(b2, r) < 0)
				lisp_ceiling_bignum(local, quot, rem, a, b);
			else if (oddp_bignum(b))
				lisp_truncate_bignum(local, quot, rem, a, b);
			else if (oddp_bignum(q))
				lisp_ceiling_bignum(local, quot, rem, a, b);
			else
				lisp_truncate_bignum(local, quot, rem, a, b);
		}
		else {
			/* a:plus, b:minus, q:minus, r:plus */
			lisp_round_b2_minus(local, &b2, b);
			if (compare_bb_real(r, b2) < 0)
				lisp_truncate_bignum(local, quot, rem, a, b);
			else if (compare_bb_real(b2, r) < 0)
				lisp_floor_bignum(local, quot, rem, a, b);
			else if (oddp_bignum(b))
				lisp_truncate_bignum(local, quot, rem, a, b);
			else if (oddp_bignum(q))
				lisp_floor_bignum(local, quot, rem, a, b);
			else
				lisp_truncate_bignum(local, quot, rem, a, b);
		}
	}
	else {
		if (plusp_bignum(b)) {
			/* a:minus, b:plus, q:minus, r:minus */
			lisp_round_b2_minus(local, &b2, b);
			if (compare_bb_real(b2, r) < 0)
				lisp_truncate_bignum(local, quot, rem, a, b);
			else if (compare_bb_real(r, b2) < 0)
				lisp_floor_bignum(local, quot, rem, a, b);
			else if (oddp_bignum(b))
				lisp_truncate_bignum(local, quot, rem, a, b);
			else if (oddp_bignum(q))
				lisp_floor_bignum(local, quot, rem, a, b);
			else
				lisp_truncate_bignum(local, quot, rem, a, b);
		}
		else {
			/* a:minus, b:minus, q:plus, r:minus */
			lisp_round_b2_plus(local, &b2, b);
			if (compare_bb_real(r, b2) < 0)
				lisp_ceiling_bignum(local, quot, rem, a, b);
			else if (compare_bb_real(b2, r) < 0)
				lisp_truncate_bignum(local, quot, rem, a, b);
			else if (oddp_bignum(b))
				lisp_truncate_bignum(local, quot, rem, a, b);
			else if (oddp_bignum(q))
				lisp_ceiling_bignum(local, quot, rem, a, b);
			else
				lisp_truncate_bignum(local, quot, rem, a, b);
		}
	}
}

void lisp_round_bignum(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int sign;
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		division_by_zero_stdarg(CONSTANT_COMMON_ROUND, a, b, NULL);
	}

	/* |b| = 1 */
	if (equal_value_bignum(b, SignPlus, 1)) {
		bignum_result_heap(a, quot);
		fixnum_heap(rem, 0);
		return;
	}
	if (equal_value_bignum(b, SignMinus, 1)) {
		GetSignBignum(a, &sign);
		push_local(local, &stack);
		bignum_copy_nosign_alloc(local, &a, a);
		SetSignBignum(a, SignNot(sign));
		bignum_result_heap(a, quot);
		rollback_local(local, stack);
		fixnum_heap(rem, 0);
		return;
	}

	push_local(local, &stack);
	lisp_round_b(local, quot, rem, a, b);
	rollback_local(local, stack);
}

static void lisp_fround_b(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int sign, sign1, sign2;
	addr q, r, b2;

	/* r = 0 */
	GetSignBignum(a, &sign1);
	GetSignBignum(b, &sign2);
	sign = SignMulti(sign1, sign2);
	divrem_bigdata_local(local, &q, &r, a, b);
	SetSignBignum(q, sign);
	SetSignBignum(r, sign2);
	if (zerop_bignum(r)) {
		single_float_bignum_heap(quot, q);
		fixnum_heap(rem, 0);
		return;
	}

	/* plus, minus */
	if (plusp_bignum(a)) {
		if (plusp_bignum(b)) {
			/* a:plus, b:plus, q:plus, r:plus */
			lisp_round_b2_plus(local, &b2, b);
			if (compare_bb_real(r, b2) < 0)
				lisp_truncate_bignum(local, quot, rem, a, b);
			else if (compare_bb_real(b2, r) < 0)
				lisp_ceiling_bignum(local, quot, rem, a, b);
			else if (oddp_bignum(b))
				lisp_truncate_bignum(local, quot, rem, a, b);
			else if (oddp_bignum(q))
				lisp_ceiling_bignum(local, quot, rem, a, b);
			else
				lisp_truncate_bignum(local, quot, rem, a, b);
		}
		else {
			/* a:plus, b:minus, q:minus, r:plus */
			lisp_round_b2_minus(local, &b2, b);
			if (compare_bb_real(r, b2) < 0)
				lisp_truncate_bignum(local, quot, rem, a, b);
			else if (compare_bb_real(b2, r) < 0)
				lisp_floor_bignum(local, quot, rem, a, b);
			else if (oddp_bignum(b))
				lisp_truncate_bignum(local, quot, rem, a, b);
			else if (oddp_bignum(q))
				lisp_floor_bignum(local, quot, rem, a, b);
			else
				lisp_truncate_bignum(local, quot, rem, a, b);
		}
	}
	else {
		if (plusp_bignum(b)) {
			/* a:minus, b:plus, q:minus, r:minus */
			lisp_round_b2_minus(local, &b2, b);
			if (compare_bb_real(b2, r) < 0)
				lisp_truncate_bignum(local, quot, rem, a, b);
			else if (compare_bb_real(r, b2) < 0)
				lisp_floor_bignum(local, quot, rem, a, b);
			else if (oddp_bignum(b))
				lisp_truncate_bignum(local, quot, rem, a, b);
			else if (oddp_bignum(q))
				lisp_floor_bignum(local, quot, rem, a, b);
			else
				lisp_truncate_bignum(local, quot, rem, a, b);
		}
		else {
			/* a:minus, b:minus, q:plus, r:minus */
			lisp_round_b2_plus(local, &b2, b);
			if (compare_bb_real(r, b2) < 0)
				lisp_ceiling_bignum(local, quot, rem, a, b);
			else if (compare_bb_real(b2, r) < 0)
				lisp_truncate_bignum(local, quot, rem, a, b);
			else if (oddp_bignum(b))
				lisp_truncate_bignum(local, quot, rem, a, b);
			else if (oddp_bignum(q))
				lisp_ceiling_bignum(local, quot, rem, a, b);
			else
				lisp_truncate_bignum(local, quot, rem, a, b);
		}
	}
}

void lisp_fround_bignum(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int sign;
	LocalStack stack;

	Check(local == NULL, "local error");
	CheckType(a, LISPTYPE_BIGNUM);
	CheckType(b, LISPTYPE_BIGNUM);
	if (zerop_bignum(b)) {
		division_by_zero_stdarg(CONSTANT_COMMON_ROUND, a, b, NULL);
	}

	/* |b| = 1 */
	if (equal_value_bignum(b, SignPlus, 1)) {
		single_float_bignum_heap(quot, a);
		fixnum_heap(rem, 0);
		return;
	}
	if (equal_value_bignum(b, SignMinus, 1)) {
		GetSignBignum(a, &sign);
		push_local(local, &stack);
		bignum_copy_nosign_alloc(local, &a, a);
		SetSignBignum(a, SignNot(sign));
		single_float_bignum_heap(quot, a);
		rollback_local(local, stack);
		fixnum_heap(rem, 0);
		return;
	}

	push_local(local, &stack);
	lisp_fround_b(local, quot, rem, a, b);
	rollback_local(local, stack);
}


/*
 *  ratio
 */
static void lisp_floor_integer_nosign_ratio(LocalRoot local,
		addr *quot, addr *rem, addr pos)
{
	int check;
	addr numer, denom, temp;
	LocalStack stack;

	GetNumerRatio(pos, &numer);
	GetNumerRatio(pos, &denom);
	check = equal_nosign_bignum(numer, denom);
	Check(check == 0, "ratio error");
	if (check <= 0) {
		fixnum_heap(quot, 0);
		ratio_copy_nosign_alloc(NULL, rem, pos);
	}
	else {
		push_local(local, &stack);
		divrem_bigdata_local(local, &numer, &temp, numer, denom);
		Check(zerop_bignum(temp), "ratio zero error");
		bignum_copy_nosign_alloc(NULL, quot, numer);
		bignum_throw_heap(temp, &temp);
		bignum_throw_heap(denom, &denom);
		make_ratio_alloc_unsafe(NULL, rem, SignPlus, temp, denom);
		rollback_local(local, stack);
	}
}

static void lisp_ceiling_integer_nosign_ratio(LocalRoot local,
		addr *quot, addr *rem, addr pos)
{
	int check;
	addr numer, denom, temp;
	LocalStack stack;

	GetNumerRatio(pos, &numer);
	GetNumerRatio(pos, &denom);
	check = equal_nosign_bignum(numer, denom);
	Check(check == 0, "ratio error");
	if (check <= 0) {
		fixnum_heap(quot, 0);
		minus_bigdata_alloc(NULL, denom, numer, &numer);
		bignum_throw_heap(denom, &denom);
		make_ratio_alloc_unsafe(NULL, rem, SignPlus, numer, denom);
	}
	else {
		push_local(local, &stack);
		divrem_bigdata_local(local, &numer, &temp, numer, denom);
		Check(zerop_bignum(temp), "ratio zero error");
		plusvalue_bigdata_alloc(NULL, numer, SignPlus, 1, quot);
		minus_bigdata_alloc(NULL, denom, temp, &temp);
		bignum_throw_heap(denom, &denom);
		make_ratio_alloc_unsafe(NULL, rem, SignPlus, numer, denom);
		rollback_local(local, stack);
	}
}

void lisp_floor_integer_ratio(LocalRoot local, addr *quot, addr *rem, addr pos)
{
	int sign;

	CheckType(pos, LISPTYPE_RATIO);
	if (zerop_ratio(pos)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
	}
	else {
		GetSignRatio(pos, &sign);
		if (IsPlus(sign)) {
			lisp_floor_integer_nosign_ratio(local, quot, rem, pos);
		}
		else {
			lisp_ceiling_integer_nosign_ratio(local, quot, rem, pos);
			SetSignRatio(*quot, SignMinus);
		}
	}
}

void lisp_ceiling_integer_ratio(LocalRoot local, addr *quot, addr *rem, addr pos)
{
	int sign;

	CheckType(pos, LISPTYPE_RATIO);
	if (zerop_ratio(pos)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
	}
	else {
		GetSignRatio(pos, &sign);
		if (IsPlus(sign)) {
			lisp_ceiling_integer_nosign_ratio(local, quot, rem, pos);
			SetSignRatio(*rem, SignMinus);
		}
		else {
			lisp_floor_integer_nosign_ratio(local, quot, rem, pos);
			SetSignRatio(*quot, SignMinus);
			SetSignRatio(*rem, SignMinus);
		}
	}
}

void lisp_truncate_integer_ratio(LocalRoot local, addr *quot, addr *rem, addr pos)
{
	int sign;

	CheckType(pos, LISPTYPE_RATIO);
	if (zerop_ratio(pos)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
	}
	else {
		GetSignRatio(pos, &sign);
		lisp_floor_integer_nosign_ratio(local, quot, rem, pos);
		if (IsMinus(sign)) {
			SetSignRatio(*quot, SignMinus);
			SetSignRatio(*rem, SignMinus);
		}
	}
}

static void lisp_round_integer_r(LocalRoot local,
		addr *quot, addr *rem, addr pos)
{
	int sign;
	addr numer, denom, denom2, temp;

	GetSignRatio(pos, &sign);
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	/* rem = 1/2 */
	divrem_bigdata_local(local, &numer, &temp, numer, denom);
	if (equal_value_bignum(temp, SignPlus, 1)
			&& equal_value_bignum(denom, SignPlus, 2)) {
		if (IsPlus(sign)) {
			if (evenp_bignum(numer))
				lisp_floor_integer_ratio(local, quot, rem, pos);
			else
				lisp_ceiling_integer_ratio(local, quot, rem, pos);
		}
		else {
			if (evenp_bignum(numer))
				lisp_ceiling_integer_ratio(local, quot, rem, pos);
			else
				lisp_floor_integer_ratio(local, quot, rem, pos);
		}
		return;
	}
	/* otherwise */
	division2_bigdata_alloc(local, &denom2, denom);
	if (IsPlus(sign)) {
		if (compare_bb_real(temp, denom2) < 0)
			lisp_floor_integer_ratio(local, quot, rem, pos);
		else
			lisp_ceiling_integer_ratio(local, quot, rem, pos);
	}
	else {
		if (compare_bb_real(temp, denom2) < 0)
			lisp_ceiling_integer_ratio(local, quot, rem, pos);
		else
			lisp_floor_integer_ratio(local, quot, rem, pos);
	}
}

void lisp_round_integer_ratio(LocalRoot local, addr *quot, addr *rem, addr pos)
{
	LocalStack stack;

	CheckType(pos, LISPTYPE_RATIO);
	if (zerop_ratio(pos)) {
		fixnum_heap(quot, 0);
		fixnum_heap(rem, 0);
		return;
	}
	push_local(local, &stack);
	lisp_round_integer_r(local, quot, rem, pos);
	rollback_local(local, stack);
}

static void lisp_ffloor_integer_nosign_ratio(LocalRoot local,
		single_float *quot, addr *rem, addr pos)
{
	int check;
	addr numer, denom, temp;
	LocalStack stack;

	GetNumerRatio(pos, &numer);
	GetNumerRatio(pos, &denom);
	check = equal_nosign_bignum(numer, denom);
	Check(check == 0, "ratio error");
	if (check <= 0) {
		*quot = 0.0f;
		ratio_copy_nosign_alloc(NULL, rem, pos);
	}
	else {
		push_local(local, &stack);
		divrem_bigdata_local(local, &numer, &temp, numer, denom);
		Check(zerop_bignum(temp), "ratio zero error");
		*quot = single_float_bignum(numer);
		bignum_throw_heap(temp, &temp);
		bignum_throw_heap(denom, &denom);
		make_ratio_alloc_unsafe(NULL, rem, SignPlus, temp, denom);
		rollback_local(local, stack);
	}
}

static void lisp_fceiling_integer_nosign_ratio(LocalRoot local,
		single_float *quot, addr *rem, addr pos)
{
	int check;
	addr numer, denom, temp;
	LocalStack stack;

	GetNumerRatio(pos, &numer);
	GetNumerRatio(pos, &denom);
	check = equal_nosign_bignum(numer, denom);
	Check(check == 0, "ratio error");
	if (check <= 0) {
		*quot = 0.0f;
		minus_bigdata_alloc(NULL, denom, numer, &numer);
		bignum_throw_heap(denom, &denom);
		make_ratio_alloc_unsafe(NULL, rem, SignPlus, numer, denom);
	}
	else {
		push_local(local, &stack);
		divrem_bigdata_local(local, &numer, &temp, numer, denom);
		Check(zerop_bignum(temp), "ratio zero error");
		*quot = single_float_bignum(numer) + 1.0f;
		minus_bigdata_alloc(NULL, denom, temp, &temp);
		bignum_throw_heap(denom, &denom);
		make_ratio_alloc_unsafe(NULL, rem, SignPlus, numer, denom);
		rollback_local(local, stack);
	}
}

void lisp_ffloor_integer_ratio(LocalRoot local, addr *quot, addr *rem, addr pos)
{
	int sign;
	single_float v;

	CheckType(pos, LISPTYPE_RATIO);
	if (zerop_ratio(pos)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
	}
	else {
		GetSignRatio(pos, &sign);
		if (IsPlus(sign)) {
			lisp_ffloor_integer_nosign_ratio(local, &v, rem, pos);
			single_float_heap(quot, v);
		}
		else {
			lisp_fceiling_integer_nosign_ratio(local, &v, rem, pos);
			float_errorcheck1(CONSTANT_COMMON_FFLOOR, v, pos);
			single_float_heap(quot, -v);
		}
	}
}

void lisp_fceiling_integer_ratio(LocalRoot local, addr *quot, addr *rem, addr pos)
{
	int sign;
	single_float v;

	CheckType(pos, LISPTYPE_RATIO);
	if (zerop_ratio(pos)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
	}
	else {
		GetSignRatio(pos, &sign);
		if (IsPlus(sign)) {
			lisp_fceiling_integer_nosign_ratio(local, &v, rem, pos);
			float_errorcheck1(CONSTANT_COMMON_FCEILING, v, pos);
			single_float_heap(quot, -v);
		}
		else {
			lisp_ffloor_integer_nosign_ratio(local, &v, rem, pos);
			single_float_heap(quot, -v);
			SetSignRatio(*rem, SignMinus);
		}
	}
}

void lisp_ftruncate_integer_ratio(LocalRoot local, addr *quot, addr *rem, addr pos)
{
	int sign;
	single_float v;

	CheckType(pos, LISPTYPE_RATIO);
	if (zerop_ratio(pos)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
	}
	else {
		GetSignRatio(pos, &sign);
		lisp_ffloor_integer_nosign_ratio(local, &v, rem, pos);
		if (IsMinus(sign)) {
			single_float_heap(quot, -v);
			SetSignRatio(*rem, SignMinus);
		}
		else {
			single_float_heap(quot, v);
		}
	}
}

static void lisp_fround_integer_r(LocalRoot local,
		addr *quot, addr *rem, addr pos)
{
	int sign;
	addr numer, denom, denom2, temp;

	GetSignRatio(pos, &sign);
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	/* rem = 1/2 */
	divrem_bigdata_local(local, &numer, &temp, numer, denom);
	if (equal_value_bignum(temp, SignPlus, 1)
			&& equal_value_bignum(denom, SignPlus, 2)) {
		if (IsPlus(sign)) {
			if (evenp_bignum(numer))
				lisp_ffloor_integer_ratio(local, quot, rem, pos);
			else
				lisp_fceiling_integer_ratio(local, quot, rem, pos);
		}
		else {
			if (evenp_bignum(numer))
				lisp_fceiling_integer_ratio(local, quot, rem, pos);
			else
				lisp_ffloor_integer_ratio(local, quot, rem, pos);
		}
		return;
	}
	/* otherwise */
	division2_bigdata_alloc(local, &denom2, denom);
	if (IsPlus(sign)) {
		if (compare_bb_real(temp, denom2) < 0)
			lisp_ffloor_integer_ratio(local, quot, rem, pos);
		else
			lisp_fceiling_integer_ratio(local, quot, rem, pos);
	}
	else {
		if (compare_bb_real(temp, denom2) < 0)
			lisp_fceiling_integer_ratio(local, quot, rem, pos);
		else
			lisp_ffloor_integer_ratio(local, quot, rem, pos);
	}
}

void lisp_fround_integer_ratio(LocalRoot local, addr *quot, addr *rem, addr pos)
{
	LocalStack stack;

	CheckType(pos, LISPTYPE_RATIO);
	if (zerop_ratio(pos)) {
		single_float_heap(quot, 0.0f);
		fixnum_heap(rem, 0);
		return;
	}
	push_local(local, &stack);
	lisp_fround_integer_r(local, quot, rem, pos);
	rollback_local(local, stack);
}


/*
 *  ratio-ratio
 */
void lisp_floor_rr_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	fmte("TODO", NULL);
}

#if 0
static void lisp_floor_rr_quotient(LocalRoot local, addr *ret, addr pos)
{
	int check;
	addr numer, denom;

	GetNumerRatio(pos, &numer);
	GetNumerRatio(pos, &denom);
	check = equal_nosign_bignum(numer, denom);
	Check(check == 0, "ratio error");
	if (check <= 0)
		bignum_value_alloc(local, ret, SignPlus, 0);
	else
		divrem_bigdata_local(local, ret, &denom, numer, denom);
}

void lisp_floor_rr_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	int sign1, sign2, sign;
	addr q, r;
	LocalStack stack;

	CheckType(a, LISPTYPE_RATIO);
	CheckType(b, LISPTYPE_RATIO);
	Check(zerop_bignum(a), "ratio numer error");
	Check(zerop_bignum(b), "ratio denom error");
	GetSignRatio(a, &sign1);
	GetSignRatio(b, &sign2);
	sign = SignMulti(sign1, sign2);
	push_local(local, &stack);
	/* (floor a b) -> quot */
	lisp_floor_rr_quotient(local, &q, a, b);
	SetSignBignum(q, sign);
	/* remainder = a - quotient*b */
	multi_bigdata_alloc(local, q, b, &r);
	sign = SignMulti(sign, sign2);
	SetSignBignum
	if (minuscheck_bigdata_alloc(local, a, r, &r)) {
		SetSignRatio(r, SignMinus);
	}
	bignum_result_heap(q, quot);
	rollback_local(local, stack);
}
#endif

void lisp_ceiling_rr_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	fmte("TODO", NULL);
}

void lisp_truncate_rr_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	fmte("TODO", NULL);
}

void lisp_round_rr_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	fmte("TODO", NULL);
}

void lisp_ffloor_rr_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	fmte("TODO", NULL);
}

void lisp_fceiling_rr_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	fmte("TODO", NULL);
}

void lisp_ftruncate_rr_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	fmte("TODO", NULL);
}

void lisp_fround_rr_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	fmte("TODO", NULL);
}



/*
 *  ratio-bignum
 */
void lisp_floor_rb_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	fmte("TODO", NULL);
}

void lisp_ceiling_rb_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	fmte("TODO", NULL);
}

void lisp_truncate_rb_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	fmte("TODO", NULL);
}

void lisp_round_rb_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	fmte("TODO", NULL);
}

void lisp_ffloor_rb_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	fmte("TODO", NULL);
}

void lisp_fceiling_rb_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	fmte("TODO", NULL);
}

void lisp_ftruncate_rb_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	fmte("TODO", NULL);
}

void lisp_fround_rb_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	fmte("TODO", NULL);
}



/*
 *  bignum-ratio
 */
void lisp_floor_br_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	fmte("TODO", NULL);
}

void lisp_ceiling_br_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	fmte("TODO", NULL);
}

void lisp_truncate_br_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	fmte("TODO", NULL);
}

void lisp_round_br_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	fmte("TODO", NULL);
}

void lisp_ffloor_br_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	fmte("TODO", NULL);
}

void lisp_fceiling_br_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	fmte("TODO", NULL);
}

void lisp_ftruncate_br_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	fmte("TODO", NULL);
}

void lisp_fround_br_ratio(LocalRoot local, addr *quot, addr *rem, addr a, addr b)
{
	fmte("TODO", NULL);
}


