#include "cmpl.h"
#include "cmpl_plus.h"
#include "condition.h"
#include "float_plus.h"
#include "integer.h"
#include "math_type.h"
#include "ratio.h"
#include "rational.h"
#include "rational_plus.h"
#include "real.h"

/*
 *  oneplus
 */
_g int oneplus_complex_heap_(LocalRoot local, addr pos, addr *ret)
{
	enum ComplexType type;
	addr real, imag;

	Check(! complexp(pos), "type error");
	switch (getcomplex(pos, &type, &real, &imag)) {
		case ComplexType_single:
			Return(plus_float_sv_heap_(real, 1.0f, &real));
			break;

		case ComplexType_double:
			Return(plus_float_dv_heap_(real, 1.0, &real));
			break;

		case ComplexType_long:
			Return(plus_float_lv_heap_(real, 1.0L, &real));
			break;

		case ComplexType_rational:
			Return(oneplus_rational_common_(local, real, &real));
			break;

		default:
			*ret = Nil;
			return TypeError_(pos, COMPLEX);
	}

	return complex_heap_(ret, real, imag);
}

_g int oneminus_complex_heap_(LocalRoot local, addr pos, addr *ret)
{
	enum ComplexType type;
	addr real, imag;

	Check(! complexp(pos), "type error");
	switch (getcomplex(pos, &type, &real, &imag)) {
		case ComplexType_single:
			Return(plus_float_sv_heap_(real, -1.0f, &real));
			break;

		case ComplexType_double:
			Return(plus_float_dv_heap_(real, -1.0, &real));
			break;

		case ComplexType_long:
			Return(plus_float_lv_heap_(real, -1.0L, &real));
			break;

		case ComplexType_rational:
			Return(oneminus_rational_common_(local, real, &real));
			break;

		default:
			*ret = Nil;
			return TypeError_(pos, COMPLEX);
	}

	return complex_heap_(ret, real, imag);
}


/*
 *  plus
 */
static int plus_real_complex_(addr left, addr right, addr *value, int *ret)
{
	enum MathType type;
	struct mathcomplex2_struct str;

	Return(getmathcomplex2_addr_(&str, left, right, &type));
	switch (type) {
		case MathType_single:
			Return(complex_single_heap_(value, str.v.s.a+str.v.s.c, str.v.s.d));
			return Result(ret, 0);

		case MathType_double:
			Return(complex_double_heap_(value, str.v.d.a+str.v.d.c, str.v.d.d));
			return Result(ret, 0);

		case MathType_long:
			Return(complex_long_heap_(value, str.v.l.a+str.v.l.c, str.v.l.d));
			return Result(ret, 0);

		case MathType_rational:
			return Result(ret, 1);

		case MathType_complex:
		case MathType_error:
		default:
			*value = Nil;
			*ret = 0;
			return fmte_("type error", NULL);
	}
}

_g int plus_rational_complex_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	int check;
	addr real, imag;

	CheckLocal(local);
	Check(! rationalp(left), "type error");
	CheckType(right, LISPTYPE_COMPLEX);
	Return(plus_real_complex_(left, right, ret, &check));
	if (check) {
		GetRealComplex(right, &real);
		GetImagComplex(right, &imag);
		Return(plus_rational_common_(local, left, real, &real));
		return complex_heap_(ret, real, imag);
	}

	return 0;
}

_g int plus_fc_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
	return plus_rational_complex_common_(local, left, right, ret);
}

_g int plus_bc_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
	return plus_rational_complex_common_(local, left, right, ret);
}

_g int plus_rc_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_RATIO);
	return plus_rational_complex_common_(local, left, right, ret);
}

_g int plus_sc_number_common_(addr left, addr right, addr *ret)
{
	int check;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_COMPLEX);
	Return(plus_real_complex_(left, right, ret, &check));
	if (check)
		return fmte_("Type error", NULL);

	return 0;
}

_g int plus_dc_number_common_(addr left, addr right, addr *ret)
{
	int check;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_COMPLEX);
	Return(plus_real_complex_(left, right, ret, &check));
	if (check)
		return fmte_("Type error", NULL);

	return 0;
}

_g int plus_lc_number_common_(addr left, addr right, addr *ret)
{
	int check;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_COMPLEX);
	Return(plus_real_complex_(left, right, ret, &check));
	if (check)
		return fmte_("Type error", NULL);

	return 0;
}

static int plus_cc_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	LocalStack stack;
	addr a, b, c, d;

	/* (a+bi) + (c+di) = a+c + (b+d)i */
	Check(GetTypeComplex(left) != ComplexType_rational, "type error");
	Check(GetTypeComplex(right) != ComplexType_rational, "type error");
	push_local(local, &stack);
	GetRealComplex(left, &a);
	GetImagComplex(left, &b);
	GetRealComplex(right, &c);
	GetImagComplex(right, &d);
	Return(plus_rational_local_(local, a, c, &a));
	Return(plus_rational_local_(local, b, d, &b));
	Return(complex_heap_(ret, a, b));
	rollback_local(local, stack);

	return 0;
}

_g int plus_cc_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	enum MathType type;
	struct mathcomplex2_struct str;

	/* (a+bi) + (c+di) = a+c + (b+d)i */
	CheckLocal(local);
	CheckType(left, LISPTYPE_COMPLEX);
	CheckType(right, LISPTYPE_COMPLEX);
	Return(getmathcomplex2_addr_(&str, left, right, &type));
	switch (type) {
		case MathType_single:
			return complex_single_heap_(ret, str.v.s.a+str.v.s.c, str.v.s.b+str.v.s.d);

		case MathType_double:
			return complex_double_heap_(ret, str.v.d.a+str.v.d.c, str.v.d.b+str.v.d.d);

		case MathType_long:
			return complex_long_heap_(ret, str.v.l.a+str.v.l.c, str.v.l.b+str.v.l.d);

		case MathType_rational:
			return plus_cc_rational_common_(local, left, right, ret);

		case MathType_complex:
		case MathType_error:
		default:
			*ret = Nil;
			return fmte_("type error", NULL);
	}
}


/*
 *  minus
 */
static int minus_real_complex_(addr left, addr right, addr *value, int *ret)
{
	enum MathType type;
	struct mathcomplex2_struct str;

	/* n-(a+bi) = n-a + (-b)i */
	Return(getmathcomplex2_addr_(&str, left, right, &type));
	switch (type) {
		case MathType_single:
			Return(complex_single_heap_(value, str.v.s.a-str.v.s.c, -str.v.s.d));
			return Result(ret, 0);

		case MathType_double:
			Return(complex_double_heap_(value, str.v.d.a-str.v.d.c, -str.v.d.d));
			return Result(ret, 0);

		case MathType_long:
			Return(complex_long_heap_(value, str.v.l.a-str.v.l.c, -str.v.l.d));
			return Result(ret, 0);

		case MathType_rational:
			return Result(ret, 1);

		case MathType_complex:
		case MathType_error:
		default:
			*value = NULL;
			*ret = 0;
			return fmte_("type error", NULL);
	}
}

static int minus_complex_real_(addr left, addr right, addr *value, int *ret)
{
	enum MathType type;
	struct mathcomplex2_struct str;

	/* (a+bi)-n = a-n + bi */
	Return(getmathcomplex2_addr_(&str, left, right, &type));
	switch (type) {
		case MathType_single:
			Return(complex_single_heap_(value, str.v.s.a-str.v.s.c, str.v.s.b));
			return Result(ret, 0);

		case MathType_double:
			Return(complex_double_heap_(value, str.v.d.a-str.v.d.c, str.v.d.b));
			return Result(ret, 0);

		case MathType_long:
			Return(complex_long_heap_(value, str.v.l.a-str.v.l.c, str.v.l.b));
			return Result(ret, 0);

		case MathType_rational:
			return Result(ret, 1);

		case MathType_complex:
		case MathType_error:
		default:
			*value = NULL;
			*ret = 0;
			return fmte_("type error", NULL);
	}
}

_g int minus_rational_complex_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	int check;
	LocalStack stack;
	addr a, b;

	/* n-(a+bi) = n-a + (-b)i */
	CheckLocal(local);
	Check(! rationalp(left), "type error");
	CheckType(right, LISPTYPE_COMPLEX);
	Return(minus_real_complex_(left, right, ret, &check));
	if (check) {
		GetRealComplex(right, &a);
		GetImagComplex(right, &b);
		push_local(local, &stack);
		Return(minus_rational_local_(local, left, a, &a));
		Return(sign_reverse_rational_local_(local, b, &b));
		Return(complex_heap_(ret, a, b));
		rollback_local(local, stack);
	}

	return 0;
}

_g int minus_complex_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	int check;
	LocalStack stack;
	addr a, b;

	/* (a+bi)-n = a-n + bi */
	CheckLocal(local);
	CheckType(left, LISPTYPE_COMPLEX);
	Check(! rationalp(right), "type right error");
	Return(minus_complex_real_(left, right, ret, &check));
	if (check) {
		GetRealComplex(left, &a);
		GetImagComplex(left, &b);
		push_local(local, &stack);
		Return(minus_rational_local_(local, a, right, &a));
		Return(complex_heap_(ret, a, b));
		rollback_local(local, stack);
	}

	return 0;
}

_g int minus_fc_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
	return minus_rational_complex_common_(local, left, right, ret);
}

_g int minus_cf_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_FIXNUM);
	return minus_complex_rational_common_(local, left, right, ret);
}

_g int minus_bc_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
	return minus_rational_complex_common_(local, left, right, ret);
}

_g int minus_cb_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_BIGNUM);
	return minus_complex_rational_common_(local, left, right, ret);
}

_g int minus_rc_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_RATIO);
	return minus_rational_complex_common_(local, left, right, ret);
}

_g int minus_cr_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_RATIO);
	return minus_complex_rational_common_(local, left, right, ret);
}

_g int minus_sc_number_common_(addr left, addr right, addr *ret)
{
	int check;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_COMPLEX);
	Return(minus_real_complex_(left, right, ret, &check));
	if (check)
		return fmte_("Type error", NULL);

	return 0;
}

_g int minus_cs_number_common_(addr left, addr right, addr *ret)
{
	int check;

	CheckType(left, LISPTYPE_COMPLEX);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	Return(minus_complex_real_(left, right, ret, &check));
	if (check)
		return fmte_("Type error", NULL);

	return 0;
}

_g int minus_dc_number_common_(addr left, addr right, addr *ret)
{
	int check;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_COMPLEX);
	Return(minus_real_complex_(left, right, ret, &check));
	if (check)
		return fmte_("Type error", NULL);

	return 0;
}

_g int minus_cd_number_common_(addr left, addr right, addr *ret)
{
	int check;

	CheckType(left, LISPTYPE_COMPLEX);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	Return(minus_complex_real_(left, right, ret, &check));
	if (check)
		return fmte_("Type error", NULL);

	return 0;
}

_g int minus_lc_number_common_(addr left, addr right, addr *ret)
{
	int check;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_COMPLEX);
	Return(minus_real_complex_(left, right, ret, &check));
	if (check)
		return fmte_("Type error", NULL);

	return 0;
}

_g int minus_cl_number_common_(addr left, addr right, addr *ret)
{
	int check;

	CheckType(left, LISPTYPE_COMPLEX);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	Return(minus_complex_real_(left, right, ret, &check));
	if (check)
		return fmte_("Type error", NULL);

	return 0;
}

static int minus_cc_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	LocalStack stack;
	addr a, b, c, d;

	/* (a+bi) - (c+di) = a-c + (b-d)i */
	CheckLocal(local);
	CheckType(left, LISPTYPE_COMPLEX);
	CheckType(right, LISPTYPE_COMPLEX);
	GetRealComplex(left, &a);
	GetImagComplex(left, &b);
	GetRealComplex(right, &c);
	GetImagComplex(right, &d);
	push_local(local, &stack);
	Return(minus_rational_local_(local, a, c, &a));
	Return(minus_rational_local_(local, b, d, &b));
	Return(complex_heap_(ret, a, b));
	rollback_local(local, stack);

	return 0;
}

_g int minus_cc_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	enum MathType type;
	struct mathcomplex2_struct str;

	/* (a+bi) - (c+di) = a-c + (b-d)i */
	CheckLocal(local);
	CheckType(left, LISPTYPE_COMPLEX);
	CheckType(right, LISPTYPE_COMPLEX);
	Return(getmathcomplex2_addr_(&str, left, right, &type));
	switch (type) {
		case MathType_single:
			return complex_single_heap_(ret, str.v.s.a-str.v.s.c, str.v.s.b-str.v.s.d);

		case MathType_double:
			return complex_double_heap_(ret, str.v.d.a-str.v.d.c, str.v.d.b-str.v.d.d);

		case MathType_long:
			return complex_long_heap_(ret, str.v.l.a-str.v.l.c, str.v.l.b-str.v.l.d);

		case MathType_rational:
			return minus_cc_rational_common_(local, left, right, ret);

		case MathType_complex:
		case MathType_error:
		default:
			*ret = Nil;
			return fmte_("type error", NULL);
	}
}

