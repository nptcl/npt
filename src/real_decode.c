#include <math.h>
#include <float.h>
#include "bigdata.h"
#include "bignum.h"
#include "condition.h"
#include "integer.h"
#include "local.h"
#include "ratio.h"
#include "rational.h"
#include "real_ceiling.h"
#include "real_decode.h"
#include "real_float.h"

#define real_decode_inexact(x,y) \
	floating_point_inexact_stdarg(CONSTANT_COMMON_##x, (y), NULL)

/*
 *  decode-float
 */
static void decode_single_float(addr pos, addr *ret, addr *rexp, addr *rsign)
{
	int exp;
	single_float v;

	GetSingleFloat(pos, &v);
	v = frexpf(v, &exp);
	single_float_heap(ret, fabsf(v));
	fixnum_heap(rexp, (fixnum)exp);
	single_float_heap(rsign, signbit(v)? -1.0f: 1.0f);
}

static void decode_double_float(addr pos, addr *ret, addr *rexp, addr *rsign)
{
	int exp;
	double_float v;

	GetDoubleFloat(pos, &v);
	v = frexp(v, &exp);
	double_float_heap(ret, fabs(v));
	fixnum_heap(rexp, (fixnum)exp);
	double_float_heap(rsign, signbit(v)? -1.0: 1.0);
}

static void decode_long_float(addr pos, addr *ret, addr *rexp, addr *rsign)
{
	int exp;
	long_float v;

	GetLongFloat(pos, &v);
	v = frexpl(v, &exp);
	long_float_heap(ret, fabsl(v));
	fixnum_heap(rexp, (fixnum)exp);
	long_float_heap(rsign, signbit(v)? -1.0L: 1.0L);
}

void decode_float_common(addr pos, addr *ret, addr *rexp, addr *rsign)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			decode_single_float(pos, ret, rexp, rsign);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			decode_double_float(pos, ret, rexp, rsign);
			break;

		case LISPTYPE_LONG_FLOAT:
			decode_long_float(pos, ret, rexp, rsign);
			break;

		default:
			TypeError(pos, FLOAT);
			*ret = *rexp = *rsign = 0;
			return;
	}
}


/*
 *  scale-float
 */
void scale_float_common(addr pos, addr scale, addr *ret)
{
	fixnum fixnum_value;
	long n;
	single_float vs;
	double_float vd;
	long_float vl;

	/* scale */
	switch (GetType(scale)) {
		case LISPTYPE_FIXNUM:
			GetFixnum(scale, &fixnum_value);
			if (fixnum_value < LONG_MIN || LONG_MAX < fixnum_value)
				fmte("Scaling factor is too large ~A.", scale, NULL);
			n = (long)fixnum_value;
			break;

		case LISPTYPE_BIGNUM:
			fmte("Scaling factor ~A must be a fixnum type.", scale, NULL);
			return;

		default:
			TypeError(scale, INTEGER);
			*ret = 0;
			return;
	}

	/* scale-float */
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			GetSingleFloat(pos, &vs);
			single_float_check_heap(ret, scalblnf(vs, n));
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			GetDoubleFloat(pos, &vd);
			double_float_check_heap(ret, scalbln(vd, n));
			break;

		case LISPTYPE_LONG_FLOAT:
			GetLongFloat(pos, &vl);
			long_float_check_heap(ret, scalblnl(vl, n));
			break;

		default:
			TypeError(pos, FLOAT);
			*ret = 0;
			return;
	}
}


/*
 *  float-radix
 */
void float_radix_common(addr pos, addr *ret)
{
	fixnum_heap(ret, FLT_RADIX);
}


/*
 *  float-sign
 */
static void float_sign1_common(addr pos, addr *ret)
{
	single_float vs;
	double_float vd;
	long_float vl;

	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			GetSingleFloat(pos, &vs);
			single_float_heap(ret, signbit(vs)? -1.0f: 1.0f);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			GetDoubleFloat(pos, &vd);
			double_float_heap(ret, signbit(vd)? -1.0: 1.0);
			break;

		case LISPTYPE_LONG_FLOAT:
			GetLongFloat(pos, &vl);
			long_float_heap(ret, signbit(vl)? -1.0L: 1.0L);
			break;

		default:
			TypeError(pos, FLOAT);
			*ret = 0;
			return;
	}
}

#define copysign_ss(x,y)  copysignf((y), (x))
#define copysign_dd(x,y)  copysign((y), (x))
#define copysign_ll(x,y)  copysignl((y), (x))
#define copysign_code(a, b) { \
	if (signbit(a)) { \
		return signbit(b)? (b): -(b); \
	} \
	else { \
		return signbit(b)? -(b): (b); \
	} \
}

static inline double_float copysign_sd(single_float a, double_float b)
{
	copysign_code(a, b);
}
static inline long_float copysign_sl(single_float a, long_float b)
{
	copysign_code(a, b);
}
static inline single_float copysign_ds(double_float a, single_float b)
{
	copysign_code(a, b);
}
static inline long_float copysign_dl(double_float a, long_float b)
{
	copysign_code(a, b);
}
static inline single_float copysign_ls(long_float a, single_float b)
{
	copysign_code(a, b);
}
static inline double_float copysign_ld(long_float a, double_float b)
{
	copysign_code(a, b);
}

static void float_sign2_single(single_float left, addr opt, addr *ret)
{
	single_float vs;
	double_float vd;
	long_float vl;

	switch (GetType(opt)) {
		case LISPTYPE_SINGLE_FLOAT:
			GetSingleFloat(opt, &vs);
			single_float_check_heap(ret, copysign_ss(left, vs));
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			GetDoubleFloat(opt, &vd);
			double_float_check_heap(ret, copysign_sd(left, vd));
			break;

		case LISPTYPE_LONG_FLOAT:
			GetLongFloat(opt, &vl);
			long_float_check_heap(ret, copysign_sl(left, vl));
			break;

		default:
			TypeError(opt, FLOAT);
			*ret = 0;
			return;
	}
}

static void float_sign2_double(double_float left, addr opt, addr *ret)
{
	single_float vs;
	double_float vd;
	long_float vl;

	switch (GetType(opt)) {
		case LISPTYPE_SINGLE_FLOAT:
			GetSingleFloat(opt, &vs);
			single_float_check_heap(ret, copysign_ds(left, vs));
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			GetDoubleFloat(opt, &vd);
			double_float_check_heap(ret, copysign_dd(left, vd));
			break;

		case LISPTYPE_LONG_FLOAT:
			GetLongFloat(opt, &vl);
			long_float_check_heap(ret, copysign_dl(left, vl));
			break;

		default:
			TypeError(opt, FLOAT);
			*ret = 0;
			return;
	}
}

static void float_sign2_long(long_float left, addr opt, addr *ret)
{
	single_float vs;
	double_float vd;
	long_float vl;

	switch (GetType(opt)) {
		case LISPTYPE_SINGLE_FLOAT:
			GetSingleFloat(opt, &vs);
			single_float_check_heap(ret, copysign_ls(left, vs));
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			GetDoubleFloat(opt, &vd);
			double_float_check_heap(ret, copysign_ld(left, vd));
			break;

		case LISPTYPE_LONG_FLOAT:
			GetLongFloat(opt, &vl);
			long_float_check_heap(ret, copysign_ll(left, vl));
			break;

		default:
			TypeError(opt, FLOAT);
			*ret = 0;
			return;
	}
}

static void float_sign2_common(addr pos, addr opt, addr *ret)
{
	single_float vs;
	double_float vd;
	long_float vl;

	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			GetSingleFloat(pos, &vs);
			float_sign2_single(vs, opt, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			GetDoubleFloat(pos, &vd);
			float_sign2_double(vd, opt, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			GetLongFloat(pos, &vl);
			float_sign2_long(vl, opt, ret);
			break;

		default:
			TypeError(pos, FLOAT);
			*ret = 0;
			return;
	}
}

void float_sign_common(addr pos, addr opt, addr *ret)
{
	if (opt == Unbound)
		float_sign1_common(pos, ret);
	else
		float_sign2_common(pos, opt, ret);
}


/*
 *  float-digits
 */
void float_digits_common(addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			fixnum_heap(ret, FLT_MANT_DIG);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			fixnum_heap(ret, DBL_MANT_DIG);
			break;

		case LISPTYPE_LONG_FLOAT:
			fixnum_heap(ret, LDBL_MANT_DIG);
			break;

		default:
			TypeError(pos, FLOAT);
			*ret = 0;
			return;
	}
}


/*
 *  float-precision
 */
static int float_precision_single(single_float v, int *ret)
{
	switch (fpclassify(v)) {
		case FP_NORMAL:
			*ret = FLT_MANT_DIG;
			return 0;

		case FP_ZERO:
			*ret = 0;
			return 0;

		case FP_SUBNORMAL:
			*ret = FLT_MANT_DIG - (FLT_MIN_EXP - 1) + ilogbf(v);
			return 0;

		default:
			return 1;
	}
}

static int float_precision_double(double_float v, int *ret)
{
	switch (fpclassify(v)) {
		case FP_NORMAL:
			*ret = DBL_MANT_DIG;
			return 0;

		case FP_ZERO:
			*ret = 0;
			return 0;

		case FP_SUBNORMAL:
			*ret = DBL_MANT_DIG - (DBL_MIN_EXP - 1) + ilogb(v);
			return 0;

		default:
			return 1;
	}
}

static int float_precision_long(long_float v, int *ret)
{
	switch (fpclassify(v)) {
		case FP_NORMAL:
			*ret = LDBL_MANT_DIG;
			return 0;

		case FP_ZERO:
			*ret = 0;
			return 0;

		case FP_SUBNORMAL:
			*ret = LDBL_MANT_DIG - (LDBL_MIN_EXP - 1) + ilogbl(v);
			return 0;

		default:
			return 1;
	}
}

static int float_precision_float(addr pos, int *ret)
{
	single_float vs;
	double_float vd;
	long_float vl;

	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			GetSingleFloat(pos, &vs);
			return float_precision_single(vs, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			GetDoubleFloat(pos, &vd);
			return float_precision_double(vd, ret);

		case LISPTYPE_LONG_FLOAT:
			GetLongFloat(pos, &vl);
			return float_precision_long(vl, ret);

		default:
			TypeError(pos, FLOAT);
			return 1;
	}
}

void float_precision_common(addr pos, addr *ret)
{
	int size;

	if (float_precision_float(pos, &size)) {
		real_decode_inexact(FLOAT_PRECISION, pos);
		return;
	}
	fixnum_heap(ret, (fixnum)size);
}


/*
 *  integer-decode-float
 */
static int integer_decode_float_single_value(LocalRoot local,
		single_float v, addr *ret, int *rexp, int *rsign)
{
	int e, p;

	*rsign = signbit(v)? -1: 1;
	v = frexpf(fabsf(v), &e);
	if (float_precision_single(v, &p))
		return 1;
	v = ldexpf(v, p);
	*rexp = e - p;
	if (bignum_single_float_local(local, ret, v))
		return 1;

	return 0;
}

static int integer_decode_float_double_value(LocalRoot local,
		double_float v, addr *ret, int *rexp, int *rsign)
{
	int e, p;

	*rsign = signbit(v)? -1: 1;
	v = frexp(fabs(v), &e);
	if (float_precision_double(v, &p))
		return 1;
	v = ldexp(v, p);
	*rexp = e - p;
	if (bignum_double_float_local(local, ret, v))
		return 1;

	return 0;
}

static int integer_decode_float_long_value(LocalRoot local,
		long_float v, addr *ret, int *rexp, int *rsign)
{
	int e, p;

	*rsign = signbit(v)? -1: 1;
	v = frexpl(fabsl(v), &e);
	if (float_precision_long(v, &p))
		return 1;
	v = ldexpl(v, p);
	*rexp = e - p;
	if (bignum_long_float_local(local, ret, v))
		return 1;

	return 0;
}

static void integer_decode_float_single(LocalRoot local,
		addr pos, addr *ret, addr *rexp, addr *rsign)
{
	int e, sign;
	single_float v;
	addr temp;
	LocalStack stack;

	GetSingleFloat(pos, &v);
	if (v == 0.0f) {
		fixnum_heap(ret, 0);
		fixnum_heap(rexp, 0);
		fixnum_heap(rsign, signbit(v)? -1: 1);
		return;
	}

	push_local(local, &stack);
	if (integer_decode_float_single_value(local, v, &temp, &e, &sign)) {
		real_decode_inexact(INTEGER_DECODE_FLOAT, pos);
		*ret = *rexp = *rsign = 0;
		return;
	}
	bignum_result_heap(temp, ret);
	rollback_local(local, stack);
	fixnum_heap(rexp, (fixnum)e);
	fixnum_heap(rsign, (fixnum)sign);
}

static void integer_decode_float_double(LocalRoot local,
		addr pos, addr *ret, addr *rexp, addr *rsign)
{
	int e, sign;
	double_float v;
	addr temp;
	LocalStack stack;

	GetDoubleFloat(pos, &v);
	if (v == 0.0) {
		fixnum_heap(ret, 0);
		fixnum_heap(rexp, 0);
		fixnum_heap(rsign, signbit(v)? -1: 1);
		return;
	}

	push_local(local, &stack);
	if (integer_decode_float_double_value(local, v, &temp, &e, &sign)) {
		real_decode_inexact(INTEGER_DECODE_FLOAT, pos);
		*ret = *rexp = *rsign = 0;
		return;
	}
	bignum_result_heap(temp, ret);
	rollback_local(local, stack);
	fixnum_heap(rexp, (fixnum)e);
	fixnum_heap(rsign, (fixnum)sign);
}

static void integer_decode_float_long(LocalRoot local,
		addr pos, addr *ret, addr *rexp, addr *rsign)
{
	int e, sign;
	long_float v;
	addr temp;
	LocalStack stack;

	GetLongFloat(pos, &v);
	if (v == 0.0L) {
		fixnum_heap(ret, 0);
		fixnum_heap(rexp, 0);
		fixnum_heap(rsign, signbit(v)? -1: 1);
		return;
	}

	push_local(local, &stack);
	if (integer_decode_float_long_value(local, v, &temp, &e, &sign)) {
		real_decode_inexact(INTEGER_DECODE_FLOAT, pos);
		*ret = *rexp = *rsign = 0;
		return;
	}
	bignum_result_heap(temp, ret);
	rollback_local(local, stack);
	fixnum_heap(rexp, (fixnum)e);
	fixnum_heap(rsign, (fixnum)sign);
}

void integer_decode_float_common(LocalRoot local,
		addr pos, addr *ret, addr *rexp, addr *rsign)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			integer_decode_float_single(local, pos, ret, rexp, rsign);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			integer_decode_float_double(local, pos, ret, rexp, rsign);
			break;

		case LISPTYPE_LONG_FLOAT:
			integer_decode_float_long(local, pos, ret, rexp, rsign);
			break;

		default:
			TypeError(pos, FLOAT);
			*ret = *rexp = *rsign = 0;
			return;
	}
}


/*
 *  rational
 */
static void rational_float_common(LocalRoot local,
		addr *ret, addr pos, int e, int sign)
{
	addr denom;

	sign = (sign < 0)? SignMinus: SignPlus;
	if (e < 0) {
		/* ratio: (/ pos (ash 1 (1 e))) */
		power2_bigdata_alloc(local, &denom, (size_t)-e);
		ratio_reduction_heap(local, ret, sign, pos, denom);
	}
	else {
		/* integer: (ash pos e) */
		ash_bignum_common(local, pos, sign, (size_t)e, ret);
	}
}

static void rational_single_common(LocalRoot local, addr pos, addr *ret)
{
	int e, sign;
	single_float v;
	LocalStack stack;

	GetSingleFloat(pos, &v);
	if (v == 0.0f) {
		fixnum_heap(ret, 0);
		return;
	}

	push_local(local, &stack);
	if (integer_decode_float_single_value(local, v, &pos, &e, &sign)) {
		real_decode_inexact(INTEGER_DECODE_FLOAT, pos);
		*ret = 0;
		return;
	}
	rational_float_common(local, ret, pos, e, sign);
	rollback_local(local, stack);
}

static void rational_double_common(LocalRoot local, addr pos, addr *ret)
{
	int e, sign;
	double_float v;
	LocalStack stack;

	GetDoubleFloat(pos, &v);
	if (v == 0.0) {
		fixnum_heap(ret, 0);
		return;
	}

	push_local(local, &stack);
	if (integer_decode_float_double_value(local, v, &pos, &e, &sign)) {
		real_decode_inexact(INTEGER_DECODE_FLOAT, pos);
		*ret = 0;
		return;
	}
	rational_float_common(local, ret, pos, e, sign);
	rollback_local(local, stack);
}

static void rational_long_common(LocalRoot local, addr pos, addr *ret)
{
	int e, sign;
	long_float v;
	LocalStack stack;

	GetLongFloat(pos, &v);
	if (v == 0.0L) {
		fixnum_heap(ret, 0);
		return;
	}

	push_local(local, &stack);
	if (integer_decode_float_long_value(local, v, &pos, &e, &sign)) {
		real_decode_inexact(INTEGER_DECODE_FLOAT, pos);
		*ret = 0;
		return;
	}
	rational_float_common(local, ret, pos, e, sign);
	rollback_local(local, stack);
}

void rational_common(LocalRoot local, addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			fixnum_throw_heap(pos, ret);
			break;

		case LISPTYPE_BIGNUM:
			bignum_throw_heap(pos, ret);
			break;

		case LISPTYPE_RATIO:
			ratio_throw_heap(pos, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			rational_single_common(local, pos, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			rational_double_common(local, pos, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			rational_long_common(local, pos, ret);
			break;

		default:
			TypeError(pos, REAL);
			*ret = 0;
			return;
	}
}


/*
 *  rationalize
 *
 *  The algorithm described in CLISP.
 *    CLISP, Bruno Haible.
 *      http://clisp.org/
 *      https://sourceforge.net/p/clisp/clisp/ci/default/tree/src/realelem.d
 *
 *  Lisp code.
 *    CMUCL  [src/code/float.lisp]
 *      https://www.cons.org/cmucl/
 *      https://gitlab.common-lisp.net/cmucl/cmucl/blob/21d/src/code/float.lisp
 *    SBCL  [src/code/float.lisp]
 *      http://www.sbcl.org/
 *      https://sourceforge.net/p/sbcl/sbcl/ci/sbcl-1.5.0/tree/src/code/float.lisp
 */
static void rationalize_copy(addr *ret, addr pos)
{
	int sign;
	fixed value;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			castfixed_fixnum(pos, &sign, &value);
			bignum_value_heap(ret, SignPlus, value);
			break;

		case LISPTYPE_BIGNUM:
			bignum_copy_nosign_heap(ret, pos);
			break;

		default:
			TypeError(pos, INTEGER);
			*ret = 0;
			return;
	}
}

static void rationalize_build_ratio(addr *ret, addr numer, addr denom)
{
	int sign1, sign2;

	getsign_integer(numer, &sign1);
	getsign_integer(denom, &sign2);
	sign1 = SignMulti(sign1, sign2);
	rationalize_copy(&numer, numer);
	rationalize_copy(&denom, denom);
	make_ratio_alloc_unsafe(NULL, ret, sign1, numer, denom);
}

static void rationalize_multi2(LocalRoot local, addr *ret, addr frac)
{
	addr value;

	/* (* 2 frac) */
	fixnum_heap(&value, 2);
	multi_ii_real_common(local, frac, value, ret);
}

static void rationalize_letdenom(LocalRoot local, addr expo, addr *ret)
{
	addr one;

	/* (ash 1 (- 1 expo)) */
	fixnum_heap(&one, 1);
	minus_ii_real_common(local, one, expo, &expo);
	ash_integer_common(local, one, expo, ret);
}

static void rationalize_ab(LocalRoot local, addr *ra, addr *fb, addr frac, addr expo)
{
	addr a, b;

	rationalize_multi2(local, &frac, frac);
	oneminus_integer_common(local, frac, &a);
	oneplus_integer_common(local, frac, &b);
	rationalize_letdenom(local, expo, &expo);
	rationalize_build_ratio(ra, a, expo);
	rationalize_build_ratio(fb, b, expo);
}

static void rationalize_let(LocalRoot local, addr z, addr x1, addr x0, addr *ret)
{
	multi_ii_real_common(local, z, x1, &z);
	plus_ii_real_common(local, z, x0, ret);
}

static void rationalize_minus1(LocalRoot local, addr x, addr *ret)
{
	/* (- x 1) */
	addr one;
	fixnum_heap(&one, 1);
	minus_ii_real_common(local, x, one, ret);
}

static void rationalize_psetf(LocalRoot local, addr x, addr k, addr *ret)
{
	/* (/ (- x k)) */
	minus_rational_common(local, x, k, &x);
	inverse_rational_common(local, x, ret);
}

static void rationalize_float(LocalRoot local, addr x, addr *ret)
{
	addr frac, expo, sign, v1, v2;
	addr a, b, c, p0, q0, p1, q1, top, bot, k, p2, q2;

	/* multiple-value-bind */
	integer_decode_float_common(local, x, &frac, &expo, &sign);
	/* cond */
	if (zerop_integer(frac) || zerop_or_plusp_integer(expo)) {
		ash_integer_common(local, frac, expo, ret);
		if (minusp_integer(sign))
			sign_reverse_integer_common(*ret, ret);
		return;
	}
	/* a, b */
	rationalize_ab(local, &a, &b, frac, expo);
	/* p0, q0, p1, q1 */
	fixnum_heap(&p0, 0);
	fixnum_heap(&q0, 1);
	fixnum_heap(&p1, 1);
	fixnum_heap(&q1, 0);
	/* do */
	for (;;) {
		ceiling1_common(local, &c, &v1, a);
		/* result */
		if (less_rational(local, c, b)) {
			rationalize_let(local, c, p1, p0, &top);
			rationalize_let(local, c, q1, q0, &bot);
			if (minusp_integer(sign))
				sign_reverse_integer_common(top, &top);
			rationalize_build_ratio(ret, top, bot);
			return;
		}
		/* body */
		rationalize_minus1(local, c, &k);
		rationalize_let(local, k, p1, p0, &p2);
		rationalize_let(local, k, q1, q0, &q2);
		rationalize_psetf(local, b, k, &v1);
		rationalize_psetf(local, a, k, &v2);
		a = v1;
		b = v2;
		p0 = p1;
		q0 = q1;
		p1 = p2;
		q1 = q2;
	}
}

void rationalize_common(LocalRoot local, addr pos, addr *ret)
{
	if (rationalp(pos)) {
		rational_throw_heap(pos, ret);
		return;
	}
	if (floatp(pos)) {
		rationalize_float(local, pos, ret);
		return;
	}

	TypeError(pos, REAL);
	*ret = 0;
}


/*
 *  CMUCL  [src/code/float.lisp]
 *    https://www.cons.org/cmucl/
 *    https://gitlab.common-lisp.net/cmucl/cmucl/blob/21d/src/code/float.lisp
 */
/*
 * ;;; -*- Mode: Lisp; Package: KERNEL; Log: code.log -*-
 * ;;;
 * ;;; **********************************************************************
 * ;;; This code was written as part of the CMU Common Lisp project at
 * ;;; Carnegie Mellon University, and has been placed in the public domain.
 * ;;;
 */
/* ;;; RATIONALIZE  --  Public
 * ;;;
 * ;;; The algorithm here is the method described in CLISP.  Bruno Haible has
 * ;;; graciously given permission to use this algorithm.  He says, "You can use
 * ;;; it, if you present the following explanation of the algorithm."
 * ;;;
 * ;;; Algorithm (recursively presented):
 * ;;;   If x is a rational number, return x.
 * ;;;   If x = 0.0, return 0.
 * ;;;   If x < 0.0, return (- (rationalize (- x))).
 * ;;;   If x > 0.0:
 * ;;;     Call (integer-decode-float x). It returns a m,e,s=1 (mantissa,
 * ;;;     exponent, sign).
 * ;;;     If m = 0 or e >= 0: return x = m*2^e.
 * ;;;     Search a rational number between a = (m-1/2)*2^e and b = (m+1/2)*2^e
 * ;;;     with smallest possible numerator and denominator.
 * ;;;     Note 1: If m is a power of 2, we ought to take a = (m-1/4)*2^e.
 * ;;;       But in this case the result will be x itself anyway, regardless of
 * ;;;       the choice of a. Therefore we can simply ignore this case.
 * ;;;     Note 2: At first, we need to consider the closed interval [a,b].
 * ;;;       but since a and b have the denominator 2^(|e|+1) whereas x itself
 * ;;;       has a denominator <= 2^|e|, we can restrict the seach to the open
 * ;;;       interval (a,b).
 * ;;;     So, for given a and b (0 < a < b) we are searching a rational number
 * ;;;     y with a <= y <= b.
 * ;;;     Recursive algorithm fraction_between(a,b):
 * ;;;       c := (ceiling a)
 * ;;;       if c < b
 * ;;;         then return c       ; because a <= c < b, c integer
 * ;;;         else
 * ;;;           ; a is not integer (otherwise we would have had c = a < b)
 * ;;;           k := c-1          ; k = floor(a), k < a < b <= k+1
 * ;;;           return y = k + 1/fraction_between(1/(b-k), 1/(a-k))
 * ;;;                             ; note 1 <= 1/(b-k) < 1/(a-k)
 * ;;;
 * ;;; You can see that we are actually computing a continued fraction expansion.
 * ;;;
 * ;;; Algorithm (iterative):
 * ;;;   If x is rational, return x.
 * ;;;   Call (integer-decode-float x). It returns a m,e,s (mantissa,
 * ;;;     exponent, sign).
 * ;;;   If m = 0 or e >= 0, return m*2^e*s. (This includes the case x = 0.0.)
 * ;;;   Create rational numbers a := (2*m-1)*2^(e-1) and b := (2*m+1)*2^(e-1)
 * ;;;   (positive and already in lowest terms because the denominator is a
 * ;;;   power of two and the numerator is odd).
 * ;;;   Start a continued fraction expansion
 * ;;;     p[-1] := 0, p[0] := 1, q[-1] := 1, q[0] := 0, i := 0.
 * ;;;   Loop
 * ;;;     c := (ceiling a)
 * ;;;     if c >= b
 * ;;;       then k := c-1, partial_quotient(k), (a,b) := (1/(b-k),1/(a-k)),
 * ;;;            goto Loop
 * ;;;   finally partial_quotient(c).
 * ;;;   Here partial_quotient(c) denotes the iteration
 * ;;;     i := i+1, p[i] := c*p[i-1]+p[i-2], q[i] := c*q[i-1]+q[i-2].
 * ;;;   At the end, return s * (p[i]/q[i]).
 * ;;;   This rational number is already in lowest terms because
 * ;;;   p[i]*q[i-1]-p[i-1]*q[i] = (-1)^i.
 * ;;;
 */
/* (defun rationalize (x)
 *   "Converts any REAL to a RATIONAL.  Floats are converted to a simple rational
 *   representation exploiting the assumption that floats are only accurate to
 *   their precision.  RATIONALIZE (and also RATIONAL) preserve the invariant:
 *       (= x (float (rationalize x) x))"
 *   (number-dispatch ((x real))
 *     (((foreach single-float double-float #+long-float long-float
 *                #+double-double double-double-float))
 *      ;; This is a fairly straigtforward implementation of the iterative
 *      ;; algorithm above.
 *      (multiple-value-bind (frac expo sign)
 *          (integer-decode-float x)
 *        (cond ((or (zerop frac) (>= expo 0))
 *               (if (minusp sign)
 *                   (- (ash frac expo))
 *                   (ash frac expo)))
 *              (t
 *               ;; expo < 0 and (2*m-1) and (2*m+1) are coprime to 2^(1-e),
 *               ;; so build the fraction up immediately, without having to do
 *               ;; a gcd.
 *               (let ((a (build-ratio (- (* 2 frac) 1) (ash 1 (- 1 expo))))
 *                     (b (build-ratio (+ (* 2 frac) 1) (ash 1 (- 1 expo))))
 *                     (p0 0)
 *                     (q0 1)
 *                     (p1 1)
 *                     (q1 0))
 *                 (do ((c (ceiling a) (ceiling a)))
 *                     ((< c b)
 *                      (let ((top (+ (* c p1) p0))
 *                            (bot (+ (* c q1) q0)))
 *                        (build-ratio (if (minusp sign)
 *                                         (- top)
 *                                         top)
 *                                     bot)))
 *                   (let* ((k (- c 1))
 *                          (p2 (+ (* k p1) p0))
 *                          (q2 (+ (* k q1) q0)))
 *                     (psetf a (/ (- b k))
 *                            b (/ (- a k)))
 *                     (setf p0 p1
 *                           q0 q1
 *                           p1 p2
 *                           q1 q2))))))))
 *     ((rational) x)))
 *
 */

