#include "cmpl.h"
#include "cmpl_plus.h"
#include "condition.h"
#include "integer.h"
#include "math_type.h"
#include "ratio.h"
#include "rational.h"
#include "real.h"
#include "real_float.h"


/*
 *  oneplus
 */
void oneplus_complex_heap(LocalRoot local, addr pos, addr *ret)
{
	enum ComplexType type;
	addr real, imag;

	Check(! complexp(pos), "type error");
	switch (getcomplex(pos, &type, &real, &imag)) {
		case ComplexType_single:
			plus_float_sv_heap(real, 1.0f, &real);
			break;

		case ComplexType_double:
			plus_float_dv_heap(real, 1.0, &real);
			break;

		case ComplexType_long:
			plus_float_lv_heap(real, 1.0L, &real);
			break;

		case ComplexType_rational:
			oneplus_rational_common(local, real, &real);
			break;

		default:
			TypeError(pos, COMPLEX);
			return;
	}
	complex_unsafe_heap(ret, real, imag, type);
}

void oneminus_complex_heap(LocalRoot local, addr pos, addr *ret)
{
	enum ComplexType type;
	addr real, imag;

	Check(! complexp(pos), "type error");
	switch (getcomplex(pos, &type, &real, &imag)) {
		case ComplexType_single:
			plus_float_sv_heap(real, -1.0f, &real);
			break;

		case ComplexType_double:
			plus_float_dv_heap(real, -1.0, &real);
			break;

		case ComplexType_long:
			plus_float_lv_heap(real, -1.0L, &real);
			break;

		case ComplexType_rational:
			oneminus_rational_common(local, real, &real);
			break;

		default:
			TypeError(pos, COMPLEX);
			return;
	}
	complex_unsafe_heap(ret, real, imag, type);
}


/*
 *  plus
 */
static int plus_real_complex(addr left, addr right, addr *ret)
{
	struct mathcomplex2_struct str;

	switch (getmathcomplex2_addr(&str, left, right)) {
		case MathType_single:
			complex_single_heap(ret, str.v.s.a+str.v.s.c, str.v.s.d);
			return 0;

		case MathType_double:
			complex_double_heap(ret, str.v.d.a+str.v.d.c, str.v.d.d);
			return 0;

		case MathType_long:
			complex_long_heap(ret, str.v.l.a+str.v.l.c, str.v.l.d);
			return 0;

		case MathType_rational:
			return 1;

		case MathType_complex:
		case MathType_error:
		default:
			fmte("type error", NULL);
			return 0;
	}
}

void plus_rational_complex_common(LocalRoot local, addr left, addr right, addr *ret)
{
	addr real, imag;

	CheckLocal(local);
	Check(! rationalp(left), "type error");
	CheckType(right, LISPTYPE_COMPLEX);
	if (plus_real_complex(left, right, ret)) {
		GetRealComplex(right, &real);
		GetImagComplex(right, &imag);
		plus_rational_common(local, left, real, &real);
		complex_unsafe_heap(ret, real, imag, ComplexType_rational);
	}
}

void plus_fc_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
	plus_rational_complex_common(local, left, right, ret);
}

void plus_bc_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
	plus_rational_complex_common(local, left, right, ret);
}

void plus_rc_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_RATIO);
	plus_rational_complex_common(local, left, right, ret);
}

void plus_sc_number_common(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_COMPLEX);
	if (plus_real_complex(left, right, ret))
		fmte("Type error", NULL);
}

void plus_dc_number_common(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_COMPLEX);
	if (plus_real_complex(left, right, ret))
		fmte("Type error", NULL);
}

void plus_lc_number_common(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_COMPLEX);
	if (plus_real_complex(left, right, ret))
		fmte("Type error", NULL);
}

static void plus_cc_rational_common(LocalRoot local, addr left, addr right, addr *ret)
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
	plus_rational_local(local, a, c, &a);
	plus_rational_local(local, b, d, &b);
	complex_unsafe_heap(ret, a, b, ComplexType_rational);
	rollback_local(local, stack);
}

void plus_cc_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	struct mathcomplex2_struct str;

	/* (a+bi) + (c+di) = a+c + (b+d)i */
	CheckLocal(local);
	CheckType(left, LISPTYPE_COMPLEX);
	CheckType(right, LISPTYPE_COMPLEX);
	switch (getmathcomplex2_addr(&str, left, right)) {
		case MathType_single:
			complex_single_heap(ret, str.v.s.a+str.v.s.c, str.v.s.b+str.v.s.d);
			break;

		case MathType_double:
			complex_double_heap(ret, str.v.d.a+str.v.d.c, str.v.d.b+str.v.d.d);
			break;

		case MathType_long:
			complex_long_heap(ret, str.v.l.a+str.v.l.c, str.v.l.b+str.v.l.d);
			break;

		case MathType_rational:
			plus_cc_rational_common(local, left, right, ret);
			break;

		case MathType_complex:
		case MathType_error:
		default:
			fmte("type error", NULL);
			return;
	}
}


/*
 *  minus
 */
static int minus_real_complex(addr left, addr right, addr *ret)
{
	struct mathcomplex2_struct str;

	/* n-(a+bi) = n-a + (-b)i */
	switch (getmathcomplex2_addr(&str, left, right)) {
		case MathType_single:
			complex_single_heap(ret, str.v.s.a-str.v.s.c, -str.v.s.d);
			return 0;

		case MathType_double:
			complex_double_heap(ret, str.v.d.a-str.v.d.c, -str.v.d.d);
			return 0;

		case MathType_long:
			complex_long_heap(ret, str.v.l.a-str.v.l.c, -str.v.l.d);
			return 0;

		case MathType_rational:
			return 1;

		case MathType_complex:
		case MathType_error:
		default:
			fmte("type error", NULL);
			return 0;
	}
}

static int minus_complex_real(addr left, addr right, addr *ret)
{
	struct mathcomplex2_struct str;

	/* (a+bi)-n = a-n + bi */
	switch (getmathcomplex2_addr(&str, left, right)) {
		case MathType_single:
			complex_single_heap(ret, str.v.s.a-str.v.s.c, str.v.s.b);
			return 0;

		case MathType_double:
			complex_double_heap(ret, str.v.d.a-str.v.d.c, str.v.d.b);
			return 0;

		case MathType_long:
			complex_long_heap(ret, str.v.l.a-str.v.l.c, str.v.l.b);
			return 0;

		case MathType_rational:
			return 1;

		case MathType_complex:
		case MathType_error:
		default:
			fmte("type error", NULL);
			return 0;
	}
}

void minus_rational_complex_common(LocalRoot local, addr left, addr right, addr *ret)
{
	LocalStack stack;
	addr a, b;

	/* n-(a+bi) = n-a + (-b)i */
	CheckLocal(local);
	Check(! rationalp(left), "type error");
	CheckType(right, LISPTYPE_COMPLEX);
	if (minus_real_complex(left, right, ret)) {
		GetRealComplex(right, &a);
		GetImagComplex(right, &b);
		push_local(local, &stack);
		minus_rational_local(local, left, a, &a);
		sign_reverse_rational_local(local, b, &b);
		complex_heap(ret, a, b);
		rollback_local(local, stack);
	}
}

void minus_complex_rational_common(LocalRoot local, addr left, addr right, addr *ret)
{
	LocalStack stack;
	addr a, b;

	/* (a+bi)-n = a-n + bi */
	CheckLocal(local);
	CheckType(left, LISPTYPE_COMPLEX);
	Check(! rationalp(right), "type right error");
	if (minus_complex_real(left, right, ret)) {
		GetRealComplex(left, &a);
		GetImagComplex(left, &b);
		push_local(local, &stack);
		minus_rational_local(local, a, left, &a);
		complex_heap(ret, a, b);
		rollback_local(local, stack);
	}
}

void minus_fc_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
	minus_rational_complex_common(local, left, right, ret);
}

void minus_cf_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_FIXNUM);
	minus_complex_rational_common(local, left, right, ret);
}

void minus_bc_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
	minus_rational_complex_common(local, left, right, ret);
}

void minus_cb_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_BIGNUM);
	minus_complex_rational_common(local, left, right, ret);
}

void minus_rc_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_RATIO);
	minus_rational_complex_common(local, left, right, ret);
}

void minus_cr_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_RATIO);
	minus_complex_rational_common(local, left, right, ret);
}

void minus_sc_number_common(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_COMPLEX);
	if (minus_real_complex(left, right, ret))
		fmte("Type error", NULL);
}

void minus_cs_number_common(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_COMPLEX);
	if (minus_complex_real(left, right, ret))
		fmte("Type error", NULL);
}

void minus_dc_number_common(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_COMPLEX);
	if (minus_real_complex(left, right, ret))
		fmte("Type error", NULL);
}

void minus_cd_number_common(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_COMPLEX);
	if (minus_complex_real(left, right, ret))
		fmte("Type error", NULL);
}

void minus_lc_number_common(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_COMPLEX);
	if (minus_real_complex(left, right, ret))
		fmte("Type error", NULL);
}

void minus_cl_number_common(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_COMPLEX);
	if (minus_complex_real(left, right, ret))
		fmte("Type error", NULL);
}

static void minus_cc_rational_common(LocalRoot local, addr left, addr right, addr *ret)
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
	minus_rational_local(local, a, c, &a);
	minus_rational_local(local, b, d, &b);
	complex_heap(ret, a, b);
	rollback_local(local, stack);
}

void minus_cc_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	struct mathcomplex2_struct str;

	/* (a+bi) - (c+di) = a-c + (b-d)i */
	CheckLocal(local);
	CheckType(left, LISPTYPE_COMPLEX);
	CheckType(right, LISPTYPE_COMPLEX);
	switch (getmathcomplex2_addr(&str, left, right)) {
		case MathType_single:
			complex_single_heap(ret, str.v.s.a-str.v.s.c, str.v.s.b-str.v.s.d);
			break;

		case MathType_double:
			complex_double_heap(ret, str.v.d.a-str.v.d.c, str.v.d.b-str.v.d.d);
			break;

		case MathType_long:
			complex_long_heap(ret, str.v.l.a-str.v.l.c, str.v.l.b-str.v.l.d);
			break;

		case MathType_rational:
			minus_cc_rational_common(local, left, right, ret);
			break;

		case MathType_complex:
		case MathType_error:
		default:
			fmte("type error", NULL);
			return;
	}
}

