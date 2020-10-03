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
#include "number_multi.h"
#include "ratio.h"
#include "rational.h"
#include "rational_equal.h"
#include "rational_multi.h"

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

static int bignum_if_fixnum_local_(LocalRoot local, addr *ret, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			bignum_fixnum_local(local, ret, pos);
			return 0;

		case LISPTYPE_BIGNUM:
			return Result(ret, pos);

		default:
			*ret = 0;
			return TypeError_(pos, INTEGER);
	}
}

static void expr_inverse_bignum_heap(int sign, addr denom, addr *ret)
{
	addr numer;

	CheckType(denom, LISPTYPE_BIGNUM);
	bignum_value_heap(&numer, SignPlus, 1);
	make_ratio_alloc_unsafe(NULL, ret, sign, numer, denom);
}

static int expt_integer_common_(LocalRoot local, addr *ret, addr base, addr power)
{
	int sign, inverse, check;
	size_t size;

	CheckLocal(local);
	Return(bignum_if_fixnum_local_(local, &base, base));
	Return(getindex_sign_integer_(power, &inverse, &size, &check));
	if (check) {
		*ret = Nil;
		return fmte_("Too large expt power ~A.", power, NULL);
	}

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

	return 0;
}

static int expt_ratio_common_(LocalRoot local, addr *ret, addr base, addr power)
{
	int check, sign, inverse;
	addr numer, denom;
	size_t size;

	CheckLocalType(local, base, LISPTYPE_RATIO);
    Return(getindex_sign_integer_(power, &inverse, &size, &check));
	if (check) {
		*ret = Nil;
		return fmte_("Too large expt power ~A.", power, NULL);
	}

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

	return 0;
}

static int expt_float_common_(addr *ret, addr base, addr power)
{
	single_float v1, v2;

	Return(single_float_rational_(base, &v1));
	Return(single_float_rational_(power, &v2));
	expt_f(v1, 0.0f, v2, 0.0f, &v1, &v2);
	return complex_single_heap_(ret, v1, v2);
}

static int expt_rr_common_(LocalRoot local, addr *ret, addr base, addr power)
{
	/* ff, fb, bf, bb -> integer
	 * rf, rb         -> ratio
	 * fr, br, rr     -> float
	 */
	int check;
	LocalStack stack;

	Return(zerop_rational_(power, &check));
	if (check) {
		fixnum_heap(ret, 1);
		return 0;
	}
	Return(zerop_rational_(base, &check));
	if (check) {
		Return(minusp_rational_(power, &check));
		if (check) {
			*ret = Nil;
			return call_division_by_zero_real2_(NULL,
					CONSTANT_COMMON_EXPT, base, power);
		}
		fixnum_heap(ret, 0);
		return 0;
	}

	push_local(local, &stack);
	if (ratiop(power)) {
		Return(expt_float_common_(ret, base, power));
	}
	else if (ratiop(base)) {
		Return(expt_ratio_common_(local, ret, base, power));
	}
	else {
		Return(expt_integer_common_(local, ret, base, power));
	}
	rollback_local(local, stack);

	return 0;
}

static int expt_single_common_(struct mathcomplex2_struct *ptr, addr *ret)
{
	single_float real, imag;
	expt_f(ptr->v.s.a, ptr->v.s.b, ptr->v.s.c, ptr->v.s.d, &real, &imag);
	return complex_single_heap_(ret, real, imag);
}

static int expt_double_common_(struct mathcomplex2_struct *ptr, addr *ret)
{
	double_float real, imag;
	expt_d(ptr->v.d.a, ptr->v.d.b, ptr->v.d.c, ptr->v.d.d, &real, &imag);
	return complex_double_heap_(ret, real, imag);
}

static int expt_long_common_(struct mathcomplex2_struct *ptr, addr *ret)
{
	long_float real, imag;
	expt_l(ptr->v.l.a, ptr->v.l.b, ptr->v.l.c, ptr->v.l.d, &real, &imag);
	return complex_long_heap_(ret, real, imag);
}

static int expt_force_single_(LocalRoot local, addr *ret, addr base, addr power)
{
	enum MathType type;
	struct mathcomplex2_struct str;

	Return(getmathcomplex2_float_(&str, base, power, &type));
	switch (type) {
		case MathType_single:
			return expt_single_common_(&str, ret);

		default:
			*ret = 0;
			return fmte_("Type error", NULL);
	}
}

static int expr_multi_complex_heap_(LocalRoot local, addr a, addr b, addr *ret)
{
	if (a == NULL)
		return Result(ret, b);
	else
		return multi_number_heap_(local, a, b, ret);
}

static int expt_complex_heap_(LocalRoot local, addr *ret, addr base, size_t power)
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
		Return(expr_multi_complex_heap_(local, v, v, &v));
		if ((power >> (size - i - 1)) & 1) {
			Return(expr_multi_complex_heap_(local, v, base, &v));
		}
	}
	if (v == NULL)
		fixnum_heap(ret, 1);
	else
		*ret = v;

	return 0;
}

static int expt_complex_integer_(LocalRoot local, addr *ret, addr base, addr power)
{
	int inverse, check;
	size_t size;

	CheckLocal(local);
	CheckType(base, LISPTYPE_COMPLEX);
	Check(! integerp(power), "type error");
	Return(getindex_sign_integer_(power, &inverse, &size, &check));
	if (check) {
		*ret = Nil;
		return fmte_("Too large expt power ~A.", power, NULL);
	}

	Return(expt_complex_heap_(local, ret, base, size));
	if (inverse)
		return inverse_complex_common_(local, *ret, ret);

	return 0;
}

static int expt_rational_common_(LocalRoot local, addr *ret, addr base, addr power)
{
	if (complexp(power) || ratiop(power)) {
		/* float */
		return expt_force_single_(local, ret, base, power);
	}
	else if (complexp(base)) {
		/* complex - integer */
		return expt_complex_integer_(local, ret, base, power);
	}
	else {
		/* rational */
		return expt_rr_common_(local, ret, base, power);
	}
}

_g int expt_common_(LocalRoot local, addr *ret, addr base, addr power)
{
	enum MathType type;
	struct mathcomplex2_struct str;

	Return(getmathcomplex2_addr_(&str, base, power, &type));
	switch (type) {
		case MathType_single:
			return expt_single_common_(&str, ret);

		case MathType_double:
			return expt_double_common_(&str, ret);

		case MathType_long:
			return expt_long_common_(&str, ret);

		case MathType_rational:
			return expt_rational_common_(local, ret, base, power);

		case MathType_complex:
		case MathType_error:
		default:
			*ret = 0;
			return fmte_("type error", NULL);
	}
}

