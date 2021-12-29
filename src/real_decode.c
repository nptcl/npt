#include <math.h>
#include <float.h>
#include "bignum.h"
#include "bignum_data.h"
#include "bignum_object.h"
#include "condition.h"
#include "float_object.h"
#include "integer.h"
#include "integer_calc.h"
#include "integer_common.h"
#include "local.h"
#include "ratio.h"
#include "rational.h"
#include "rational_equal.h"
#include "rational_multi.h"
#include "rational_plus.h"
#include "real.h"
#include "real_ceiling.h"
#include "real_decode.h"
#include "real_equal.h"

#define real_decode_inexact_(ptr, x,y) \
	call_float_inexact_va_(ptr, CONSTANT_COMMON_##x, (y), NULL)

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

int decode_float_common_(addr pos, addr *ret, addr *rexp, addr *rsign)
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
			*ret = *rexp = *rsign = 0;
			return TypeError_(pos, FLOAT);
	}

	return 0;
}


/*
 *  scale-float
 */
int scale_float_common_(addr pos, addr scale, addr *ret)
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
				return fmte_("Scaling factor is too large ~A.", scale, NULL);
			n = (long)fixnum_value;
			break;

		case LISPTYPE_BIGNUM:
			return fmte_("Scaling factor ~A must be a fixnum type.", scale, NULL);

		default:
			*ret = 0;
			return TypeError_(scale, INTEGER);
	}

	/* scale-float */
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			GetSingleFloat(pos, &vs);
			return single_float_check_heap_(ret, scalblnf(vs, n));

		case LISPTYPE_DOUBLE_FLOAT:
			GetDoubleFloat(pos, &vd);
			return double_float_check_heap_(ret, scalbln(vd, n));

		case LISPTYPE_LONG_FLOAT:
			GetLongFloat(pos, &vl);
			return long_float_check_heap_(ret, scalblnl(vl, n));

		default:
			*ret = 0;
			return TypeError_(pos, FLOAT);
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
static int float_sign1_common_(addr pos, addr *ret)
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
			*ret = 0;
			return TypeError_(pos, FLOAT);
	}

	return 0;
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

static int float_sign2_single_(single_float left, addr opt, addr *ret)
{
	single_float vs;
	double_float vd;
	long_float vl;

	switch (GetType(opt)) {
		case LISPTYPE_SINGLE_FLOAT:
			GetSingleFloat(opt, &vs);
			return single_float_check_heap_(ret, copysign_ss(left, vs));

		case LISPTYPE_DOUBLE_FLOAT:
			GetDoubleFloat(opt, &vd);
			return double_float_check_heap_(ret, copysign_sd(left, vd));

		case LISPTYPE_LONG_FLOAT:
			GetLongFloat(opt, &vl);
			return long_float_check_heap_(ret, copysign_sl(left, vl));

		default:
			*ret = 0;
			return TypeError_(opt, FLOAT);
	}
}

static int float_sign2_double_(double_float left, addr opt, addr *ret)
{
	single_float vs;
	double_float vd;
	long_float vl;

	switch (GetType(opt)) {
		case LISPTYPE_SINGLE_FLOAT:
			GetSingleFloat(opt, &vs);
			return single_float_check_heap_(ret, copysign_ds(left, vs));

		case LISPTYPE_DOUBLE_FLOAT:
			GetDoubleFloat(opt, &vd);
			return double_float_check_heap_(ret, copysign_dd(left, vd));

		case LISPTYPE_LONG_FLOAT:
			GetLongFloat(opt, &vl);
			return long_float_check_heap_(ret, copysign_dl(left, vl));

		default:
			*ret = 0;
			return TypeError_(opt, FLOAT);
	}
}

static int float_sign2_long_(long_float left, addr opt, addr *ret)
{
	single_float vs;
	double_float vd;
	long_float vl;

	switch (GetType(opt)) {
		case LISPTYPE_SINGLE_FLOAT:
			GetSingleFloat(opt, &vs);
			return single_float_check_heap_(ret, copysign_ls(left, vs));

		case LISPTYPE_DOUBLE_FLOAT:
			GetDoubleFloat(opt, &vd);
			return double_float_check_heap_(ret, copysign_ld(left, vd));

		case LISPTYPE_LONG_FLOAT:
			GetLongFloat(opt, &vl);
			return long_float_check_heap_(ret, copysign_ll(left, vl));

		default:
			*ret = 0;
			return TypeError_(opt, FLOAT);
	}
}

static int float_sign2_common_(addr pos, addr opt, addr *ret)
{
	single_float vs;
	double_float vd;
	long_float vl;

	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			GetSingleFloat(pos, &vs);
			return float_sign2_single_(vs, opt, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			GetDoubleFloat(pos, &vd);
			return float_sign2_double_(vd, opt, ret);

		case LISPTYPE_LONG_FLOAT:
			GetLongFloat(pos, &vl);
			return float_sign2_long_(vl, opt, ret);

		default:
			*ret = 0;
			return TypeError_(pos, FLOAT);
	}
}

int float_sign_common_(addr pos, addr opt, addr *ret)
{
	if (opt == Unbound)
		return float_sign1_common_(pos, ret);
	else
		return float_sign2_common_(pos, opt, ret);
}


/*
 *  float-digits
 */
int float_digits_common_(addr pos, addr *ret)
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
			*ret = 0;
			return TypeError_(pos, FLOAT);
	}

	return 0;
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
			return -1;
	}
}

int float_precision_common_(Execute ptr, addr pos, addr *ret)
{
	int size, check;

	check = float_precision_float(pos, &size);
	if (check < 0)
		return TypeError_(pos, FLOAT);
	if (check)
		return real_decode_inexact_(ptr, FLOAT_PRECISION, pos);

	fixnum_heap(ret, (fixnum)size);
	return 0;
}


/*
 *  integer-decode-float
 */
static int integer_decode_float_single_value_(LocalRoot local,
		single_float v, addr *value, int *rexp, int *rsign, int *ret)
{
	int e, p, check;

	*rsign = signbit(v)? -1: 1;
	v = frexpf(fabsf(v), &e);
	if (float_precision_single(v, &p))
		return Result(ret, 1);
	v = ldexpf(v, p);
	*rexp = e - p;
	Return(bignum_single_float_local_(local, v, value, &check));
	if (check)
		return Result(ret, 1);

	return Result(ret, 0);
}

static int integer_decode_float_double_value_(LocalRoot local,
		double_float v, addr *value, int *rexp, int *rsign, int *ret)
{
	int e, p, check;

	*rsign = signbit(v)? -1: 1;
	v = frexp(fabs(v), &e);
	if (float_precision_double(v, &p))
		return Result(ret, 1);
	v = ldexp(v, p);
	*rexp = e - p;
	Return(bignum_double_float_local_(local, v, value, &check));
	if (check)
		return Result(ret, 1);

	return Result(ret, 0);
}

static int integer_decode_float_long_value_(LocalRoot local,
		long_float v, addr *value, int *rexp, int *rsign, int *ret)
{
	int e, p, check;

	*rsign = signbit(v)? -1: 1;
	v = frexpl(fabsl(v), &e);
	if (float_precision_long(v, &p))
		return Result(ret, 1);
	v = ldexpl(v, p);
	*rexp = e - p;
	Return(bignum_long_float_local_(local, v, value, &check));
	if (check)
		return Result(ret, 1);

	return Result(ret, 0);
}

static int integer_decode_float_single_(Execute ptr,
		addr pos, addr *ret, addr *rexp, addr *rsign)
{
	int e, sign, check;
	single_float v;
	addr temp;
	LocalRoot local;
	LocalStack stack;

	GetSingleFloat(pos, &v);
	if (v == 0.0f) {
		fixnum_heap(ret, 0);
		fixnum_heap(rexp, 0);
		fixnum_heap(rsign, signbit(v)? -1: 1);
		return 0;
	}

	local = ptr->local;
	push_local(local, &stack);
	Return(integer_decode_float_single_value_(local, v, &temp, &e, &sign, &check));
	if (check) {
		*ret = *rexp = *rsign = 0;
		return real_decode_inexact_(ptr, INTEGER_DECODE_FLOAT, pos);
	}
	bignum_result_heap(temp, ret);
	rollback_local(local, stack);
	fixnum_heap(rexp, (fixnum)e);
	fixnum_heap(rsign, (fixnum)sign);

	return 0;
}

static int integer_decode_float_double_(Execute ptr,
		addr pos, addr *ret, addr *rexp, addr *rsign)
{
	int e, sign, check;
	double_float v;
	addr temp;
	LocalRoot local;
	LocalStack stack;

	GetDoubleFloat(pos, &v);
	if (v == 0.0) {
		fixnum_heap(ret, 0);
		fixnum_heap(rexp, 0);
		fixnum_heap(rsign, signbit(v)? -1: 1);
		return 0;
	}

	local = ptr->local;
	push_local(local, &stack);
	Return(integer_decode_float_double_value_(local, v, &temp, &e, &sign, &check));
	if (check) {
		*ret = *rexp = *rsign = 0;
		return real_decode_inexact_(ptr, INTEGER_DECODE_FLOAT, pos);
	}
	bignum_result_heap(temp, ret);
	rollback_local(local, stack);
	fixnum_heap(rexp, (fixnum)e);
	fixnum_heap(rsign, (fixnum)sign);

	return 0;
}

static int integer_decode_float_long_(Execute ptr,
		addr pos, addr *ret, addr *rexp, addr *rsign)
{
	int e, sign, check;
	long_float v;
	addr temp;
	LocalRoot local;
	LocalStack stack;

	GetLongFloat(pos, &v);
	if (v == 0.0L) {
		fixnum_heap(ret, 0);
		fixnum_heap(rexp, 0);
		fixnum_heap(rsign, signbit(v)? -1: 1);
		return 0;
	}

	local = ptr->local;
	push_local(local, &stack);
	Return(integer_decode_float_long_value_(local, v, &temp, &e, &sign, &check));
	if (check) {
		*ret = *rexp = *rsign = 0;
		return real_decode_inexact_(ptr, INTEGER_DECODE_FLOAT, pos);
	}
	bignum_result_heap(temp, ret);
	rollback_local(local, stack);
	fixnum_heap(rexp, (fixnum)e);
	fixnum_heap(rsign, (fixnum)sign);

	return 0;
}

int integer_decode_float_common_(Execute ptr,
		addr pos, addr *ret, addr *rexp, addr *rsign)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			return integer_decode_float_single_(ptr, pos, ret, rexp, rsign);

		case LISPTYPE_DOUBLE_FLOAT:
			return integer_decode_float_double_(ptr, pos, ret, rexp, rsign);

		case LISPTYPE_LONG_FLOAT:
			return integer_decode_float_long_(ptr, pos, ret, rexp, rsign);

		default:
			*ret = *rexp = *rsign = 0;
			return TypeError_(pos, FLOAT);
	}
}


/*
 *  rational
 */
static int rational_ash_common_(LocalRoot local,
		addr pos, int sign2, size_t size, addr *ret)
{
	int sign1;

	CheckType(pos, LISPTYPE_BIGNUM);
	GetSignBignum(pos, &sign1);
	if (IsPlus(sign2))
		shiftup_bigdata_alloc(local, &pos, pos, size);
	else
		shiftdown_bigdata_alloc(local, &pos, pos, size);
	SetSignBignum(pos, sign1);
	bignum_result_heap(pos, ret);

	return 0;
}

static int rational_float_common_(LocalRoot local, addr *ret, addr pos, int e, int sign)
{
	addr denom;

	sign = (sign < 0)? SignMinus: SignPlus;
	if (e < 0) {
		/* ratio: (/ pos (ash 1 (- e))) */
		power2_bigdata_alloc(local, &denom, (size_t)-e);
		ratio_reduction_heap(local, ret, sign, pos, denom);
		return 0;
	}

	/* integer: (ash pos e) */
	return rational_ash_common_(local, pos, sign, (size_t)e, ret);
}

static int rational_single_common_(Execute ptr, addr pos, addr *ret)
{
	int e, sign, check;
	single_float v;
	LocalRoot local;
	LocalStack stack;

	GetSingleFloat(pos, &v);
	if (v == 0.0f) {
		fixnum_heap(ret, 0);
		return 0;
	}

	local = ptr->local;
	push_local(local, &stack);
	Return(integer_decode_float_single_value_(local, v, &pos, &e, &sign, &check));
	if (check) {
		*ret = 0;
		return real_decode_inexact_(ptr, INTEGER_DECODE_FLOAT, pos);
	}
	Return(rational_float_common_(local, ret, pos, e, sign));
	rollback_local(local, stack);

	return 0;
}

static int rational_double_common_(Execute ptr, addr pos, addr *ret)
{
	int e, sign, check;
	double_float v;
	LocalRoot local;
	LocalStack stack;

	GetDoubleFloat(pos, &v);
	if (v == 0.0) {
		fixnum_heap(ret, 0);
		return 0;
	}

	local = ptr->local;
	push_local(local, &stack);
	Return(integer_decode_float_double_value_(local, v, &pos, &e, &sign, &check));
	if (check) {
		*ret = 0;
		return real_decode_inexact_(ptr, INTEGER_DECODE_FLOAT, pos);
	}
	Return(rational_float_common_(local, ret, pos, e, sign));
	rollback_local(local, stack);

	return 0;
}

static int rational_long_common_(Execute ptr, addr pos, addr *ret)
{
	int e, sign, check;
	long_float v;
	LocalRoot local;
	LocalStack stack;

	GetLongFloat(pos, &v);
	if (v == 0.0L) {
		fixnum_heap(ret, 0);
		return 0;
	}

	local = ptr->local;
	push_local(local, &stack);
	Return(integer_decode_float_long_value_(local, v, &pos, &e, &sign, &check));
	if (check) {
		*ret = 0;
		return real_decode_inexact_(ptr, INTEGER_DECODE_FLOAT, pos);
	}
	Return(rational_float_common_(local, ret, pos, e, sign));
	rollback_local(local, stack);

	return 0;
}

int rational_common_(Execute ptr, addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			fixnum_throw_heap(pos, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			bignum_throw_heap(pos, ret);
			return 0;

		case LISPTYPE_RATIO:
			ratio_throw_heap(pos, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return rational_single_common_(ptr, pos, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return rational_double_common_(ptr, pos, ret);

		case LISPTYPE_LONG_FLOAT:
			return rational_long_common_(ptr, pos, ret);

		default:
			*ret = 0;
			return TypeError_(pos, REAL);
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
static int rationalize_copy_(addr *ret, addr pos)
{
	int sign;
	fixed value;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			castfixed_fixnum(pos, &sign, &value);
			bignum_value_heap(ret, SignPlus, value);
			return 0;

		case LISPTYPE_BIGNUM:
			bignum_copy_nosign_heap(ret, pos);
			return 0;

		default:
			*ret = 0;
			return TypeError_(pos, INTEGER);
	}
}

static int rationalize_build_ratio_(addr *ret, addr numer, addr denom)
{
	int sign1, sign2;

	Return(getsign_integer_(numer, &sign1));
	Return(getsign_integer_(denom, &sign2));
	sign1 = SignMulti(sign1, sign2);
	Return(rationalize_copy_(&numer, numer));
	Return(rationalize_copy_(&denom, denom));
	make_ratio_alloc_unsafe(NULL, ret, sign1, numer, denom);

	return 0;
}

static int rationalize_multi2_(LocalRoot local, addr *ret, addr frac)
{
	addr value;

	/* (* 2 frac) */
	fixnum_heap(&value, 2);
	return multi_ii_real_common_(local, frac, value, ret);
}

static int rationalize_letdenom_(LocalRoot local, addr expo, addr *ret)
{
	addr one;

	/* (ash 1 (- 1 expo)) */
	fixnum_heap(&one, 1);
	Return(minus_ii_real_common_(local, one, expo, &expo));
	return ash_integer_common_(local, one, expo, ret);
}

static int rationalize_ab_(LocalRoot local, addr *ra, addr *fb, addr frac, addr expo)
{
	addr a, b;

	Return(rationalize_multi2_(local, &frac, frac));
	Return(oneminus_integer_common_(local, frac, &a));
	Return(oneplus_integer_common_(local, frac, &b));
	Return(rationalize_letdenom_(local, expo, &expo));
	Return(rationalize_build_ratio_(ra, a, expo));
	Return(rationalize_build_ratio_(fb, b, expo));

	return 0;
}

static int rationalize_let_(LocalRoot local, addr z, addr x1, addr x0, addr *ret)
{
	Return(multi_ii_real_common_(local, z, x1, &z));
	Return(plus_ii_real_common_(local, z, x0, ret));
	return 0;
}

static int rationalize_minus1_(LocalRoot local, addr x, addr *ret)
{
	/* (- x 1) */
	addr one;
	fixnum_heap(&one, 1);
	return minus_ii_real_common_(local, x, one, ret);
}

static int rationalize_psetf_(LocalRoot local, addr x, addr k, addr *ret)
{
	/* (/ (- x k)) */
	Return(minus_rational_common_(local, x, k, &x));
	Return(inverse_rational_common_(local, x, ret));
	return 0;
}

static int rationalize_zero_check_(addr frac, addr expo, int *ret)
{
	Return(zerop_integer_(frac, ret));
	if (*ret)
		return 0;

	return zerop_or_plusp_integer_(expo, ret);
}

static int rationalize_float_(Execute ptr, addr x, addr *ret)
{
	int check;
	addr frac, expo, sign, v1, v2;
	addr a, b, c, p0, q0, p1, q1, top, bot, k, p2, q2;
	LocalRoot local;

	local = ptr->local;
	/* multiple-value-bind */
	Return(integer_decode_float_common_(ptr, x, &frac, &expo, &sign));
	/* cond */
	Return(rationalize_zero_check_(frac, expo, &check));
	if (check) {
		Return(ash_integer_common_(local, frac, expo, ret));
		Return(minusp_integer_(sign, &check));
		if (check) {
			Return(sign_reverse_integer_common_(*ret, ret));
		}
		return 0;
	}
	/* a, b */
	Return(rationalize_ab_(local, &a, &b, frac, expo));
	/* p0, q0, p1, q1 */
	fixnum_heap(&p0, 0);
	fixnum_heap(&q0, 1);
	fixnum_heap(&p1, 1);
	fixnum_heap(&q1, 0);
	/* do */
	for (;;) {
		Return(ceiling1_common_(local, &c, &v1, a));
		/* result */
		Return(less_rational_(local, c, b, &check));
		if (check) {
			Return(rationalize_let_(local, c, p1, p0, &top));
			Return(rationalize_let_(local, c, q1, q0, &bot));
			Return(minusp_integer_(sign, &check));
			if (check) {
				Return(sign_reverse_integer_common_(top, &top));
			}
			return rationalize_build_ratio_(ret, top, bot);
		}
		/* body */
		Return(rationalize_minus1_(local, c, &k));
		Return(rationalize_let_(local, k, p1, p0, &p2));
		Return(rationalize_let_(local, k, q1, q0, &q2));
		Return(rationalize_psetf_(local, b, k, &v1));
		Return(rationalize_psetf_(local, a, k, &v2));
		a = v1;
		b = v2;
		p0 = p1;
		q0 = q1;
		p1 = p2;
		q1 = q2;
	}
}

int rationalize_common_(Execute ptr, addr pos, addr *ret)
{
	int check;

	/* zerop */
	Return(zerop_real_(pos, &check));
	if (check) {
		fixnum_heap(ret, 0);
		return 0;
	}

	/* rational */
	if (rationalp(pos))
		return rational_throw_heap_(pos, ret);

	/* float */
	if (floatp(pos)) {
		Return(rationalize_float_(ptr, pos, &pos));
		ratio_result_noreduction_heap(ptr->local, pos, ret);
		return 0;
	}

	*ret = Nil;
	return TypeError_(pos, REAL);
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
 */

