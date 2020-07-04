#include "bignum_data.h"
#include "bignum_object.h"
#include "cmpl.h"
#include "cmpl_math.h"
#include "cmpl_multi.h"
#include "condition.h"
#include "integer.h"
#include "math_exp.h"
#include "math_power.h"
#include "math_type.h"
#include "ratio.h"
#include "rational.h"

/*
 *  expt
 */
static void expr_multi_integer_heap(addr a, addr b, addr *ret)
{
	if (a == NULL)
		*ret = b;
	else
		multi_bigdata_alloc(NULL, a, b, ret);
}

static void expt_integer_heap(addr *ret, addr base, size_t power)
{
	unsigned i, size;
	addr v;
	size_t check;

	CheckType(base, LISPTYPE_BIGNUM);
	/* high bit */
	check = power;
	for (size = 0; check; size++)
		check >>= 1;
	Check(size == 0, "power error");
	/* power */
	v = NULL;
	for (i = 0; i < size; i++) {
		expr_multi_integer_heap(v, v, &v);
		if ((power >> (size - i - 1)) & 1)
			expr_multi_integer_heap(v, base, &v);
	}
	if (v == NULL)
		bignum_value_heap(ret, SignPlus, 1);
	else
		bignum_throw_heap(v, ret);
}

static void bignum_if_fixnum_local(LocalRoot local, addr *ret, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			bignum_fixnum_local(local, ret, pos);
			break;

		case LISPTYPE_BIGNUM:
			*ret = pos;
			break;

		default:
			TypeError(pos, INTEGER);
			*ret = 0;
			break;
	}
}

static void expr_inverse_bignum_heap(int sign, addr denom, addr *ret)
{
	addr numer;

	CheckType(denom, LISPTYPE_BIGNUM);
	bignum_value_heap(&numer, SignPlus, 1);
	make_ratio_alloc_unsafe(NULL, ret, sign, numer, denom);
}

static void expt_integer_common(LocalRoot local, addr *ret, addr base, addr power)
{
	int sign, inverse;
	size_t size;

	CheckLocal(local);
	bignum_if_fixnum_local(local, &base, base);
	if (getindex_sign_integer(power, &inverse, &size))
		fmte("Too large expt power ~A.", power, NULL);

	/* sign, inverse */
	GetSignBignum(base, &sign);
	if (IsMinus(sign))
		sign = (size & 1)? SignMinus: SignPlus;

	/* result */
	expt_integer_heap(&base, base, size);
	if (inverse) {
		expr_inverse_bignum_heap(sign, base, &base);
		ratio_result_noreduction_heap(local, base, ret);
	}
	else {
		SetSignBignum(base, sign);
		bignum_result_heap(base, ret);
	}
}

static void expt_ratio_common(LocalRoot local, addr *ret, addr base, addr power)
{
	int sign, inverse;
	addr numer, denom;
	size_t size;

	CheckLocalType(local, base, LISPTYPE_RATIO);
	if (getindex_sign_integer(power, &inverse, &size))
		fmte("Too large expt power ~A.", power, NULL);

	/* sign, inverse */
	GetSignRatio(base, &sign);
	if (IsMinus(sign))
		sign = (size & 1)? SignMinus: SignPlus;

	/* result */
	GetNumerRatio(base, &numer);
	GetDenomRatio(base, &denom);
	expt_integer_heap(&numer, numer, size);
	expt_integer_heap(&denom, denom, size);
	if (inverse)
		make_ratio_alloc_unsafe(NULL, &base, sign, denom, numer);
	else
		make_ratio_alloc_unsafe(NULL, &base, sign, numer, denom);
	ratio_result_noreduction_heap(local, base, ret);
}

static void expt_float_common(addr *ret, addr base, addr power)
{
	single_float v1 = single_float_rational(base);
	single_float v2 = single_float_rational(power);
	expt_f(v1, 0.0f, v2, 0.0f, &v1, &v2);
	complex_single_heap(ret, v1, v2);
}

static void expt_rr_common(LocalRoot local, addr *ret, addr base, addr power)
{
	/* ff, fb, bf, bb -> integer
	 * rf, rb         -> ratio
	 * fr, br, rr     -> float
	 */
	LocalStack stack;

	if (zerop_rational(power)) {
		fixnum_heap(ret, 1);
		return;
	}
	if (zerop_rational(base)) {
		if (minusp_rational(power))
			division_by_zero_real2(CONSTANT_COMMON_EXPT, base, power);
		fixnum_heap(ret, 0);
		return;
	}

	push_local(local, &stack);
	if (ratiop(power))
		expt_float_common(ret, base, power);
	else if (ratiop(base))
		expt_ratio_common(local, ret, base, power);
	else
		expt_integer_common(local, ret, base, power);
	rollback_local(local, stack);
}

static void expt_single_common(struct mathcomplex2_struct *ptr, addr *ret)
{
	single_float real, imag;
	expt_f(ptr->v.s.a, ptr->v.s.b, ptr->v.s.c, ptr->v.s.d, &real, &imag);
	complex_single_heap(ret, real, imag);
}

static void expt_double_common(struct mathcomplex2_struct *ptr, addr *ret)
{
	double_float real, imag;
	expt_d(ptr->v.d.a, ptr->v.d.b, ptr->v.d.c, ptr->v.d.d, &real, &imag);
	complex_double_heap(ret, real, imag);
}

static void expt_long_common(struct mathcomplex2_struct *ptr, addr *ret)
{
	long_float real, imag;
	expt_l(ptr->v.l.a, ptr->v.l.b, ptr->v.l.c, ptr->v.l.d, &real, &imag);
	complex_long_heap(ret, real, imag);
}

static void expt_force_single(LocalRoot local, addr *ret, addr base, addr power)
{
	struct mathcomplex2_struct str;

	switch (getmathcomplex2_float(&str, base, power)) {
		case MathType_single:
			expt_single_common(&str, ret);
			break;

		default:
			fmte("Type error", NULL);
			*ret = 0;
			break;
	}
}

static void expr_multi_complex_heap(LocalRoot local, addr a, addr b, addr *ret)
{
	if (a == NULL)
		*ret = b;
	else
		multi_number_heap(local, a, b, ret);
}

static void expt_complex_heap(LocalRoot local, addr *ret, addr base, size_t power)
{
	unsigned i, size;
	addr v;
	size_t check;

	CheckLocalType(local, base, LISPTYPE_COMPLEX);
	/* high bit */
	check = power;
	for (size = 0; check; size++)
		check >>= 1;
	Check(size == 0, "power error");
	/* power */
	v = NULL;
	for (i = 0; i < size; i++) {
		expr_multi_complex_heap(local, v, v, &v);
		if ((power >> (size - i - 1)) & 1)
			expr_multi_complex_heap(local, v, base, &v);
	}
	if (v == NULL)
		fixnum_heap(ret, 1);
	else
		*ret = v;
}

static void expt_complex_integer(LocalRoot local, addr *ret, addr base, addr power)
{
	int inverse;
	size_t size;

	CheckLocal(local);
	CheckType(base, LISPTYPE_COMPLEX);
	Check(! integerp(power), "type error");
	if (getindex_sign_integer(power, &inverse, &size))
		fmte("Too large expt power ~A.", power, NULL);

	expt_complex_heap(local, ret, base, size);
	if (inverse)
		inverse_complex_common(local, *ret, ret);
}

static void expt_rational_common(LocalRoot local, addr *ret, addr base, addr power)
{
	if (complexp(power) || ratiop(power)) {
		/* float */
		expt_force_single(local, ret, base, power);
	}
	else if (complexp(base)) {
		/* complex - integer */
		expt_complex_integer(local, ret, base, power);
	}
	else {
		/* rational */
		expt_rr_common(local, ret, base, power);
	}
}

_g void expt_common(LocalRoot local, addr *ret, addr base, addr power)
{
	struct mathcomplex2_struct str;

	switch (getmathcomplex2_addr(&str, base, power)) {
		case MathType_single:
			expt_single_common(&str, ret);
			break;

		case MathType_double:
			expt_double_common(&str, ret);
			break;

		case MathType_long:
			expt_long_common(&str, ret);
			break;

		case MathType_rational:
			expt_rational_common(local, ret, base, power);
			break;

		case MathType_complex:
		case MathType_error:
		default:
			fmte("type error", NULL);
			*ret = 0;
			return;
	}
}

