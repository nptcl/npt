#include "bignum.h"
#include "cmpl.h"
#include "cmpl_multi.h"
#include "condition.h"
#include "integer.h"
#include "math_type.h"
#include "ratio.h"
#include "rational.h"
#include "rational_equal.h"
#include "rational_multi.h"
#include "rational_plus.h"
#include "real.h"

/*
 *  multiple
 */
static int multi_real_complex_(addr left, addr right, addr *value, int *ret)
{
	enum MathType type;
	single_float vs;
	double_float vd;
	long_float vl;
	struct mathcomplex2_struct str;

	Return(getmathcomplex2_addr_(&str, left, right, &type));
	switch (type) {
		case MathType_single:
			vs = str.v.s.a;
			complex_single_heap(value, vs*str.v.s.c, vs*str.v.s.d);
			return Result(ret, 0);

		case MathType_double:
			vd = str.v.d.a;
			complex_double_heap(value, vd*str.v.d.c, vd*str.v.d.d);
			return Result(ret, 0);

		case MathType_long:
			vl = str.v.l.a;
			complex_long_heap(value, vl*str.v.l.c, vl*str.v.l.d);
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

_g int multi_rational_complex_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	int check;
	addr real, imag;

	CheckLocal(local);
	Check(! rationalp(left), "type error");
	CheckType(right, LISPTYPE_COMPLEX);
	Return(multi_real_complex_(left, right, ret, &check));
	if (check) {
		GetRealComplex(right, &real);
		GetImagComplex(right, &imag);
		multi_rational_common(local, left, real, &real);
		multi_rational_common(local, left, imag, &imag);
		return complex_heap_(ret, real, imag);
	}

	return 0;
}

_g int multi_fc_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
	return multi_rational_complex_common_(local, left, right, ret);
}

_g int multi_bc_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
	return multi_rational_complex_common_(local, left, right, ret);
}

_g int multi_rc_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_RATIO);
	return multi_rational_complex_common_(local, left, right, ret);
}

_g int multi_sc_number_common_(addr left, addr right, addr *ret)
{
	int check;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_COMPLEX);
	Return(multi_real_complex_(left, right, ret, &check));
	if (check)
		return fmte_("Type error", NULL);
	
	return 0;
}

_g int multi_dc_number_common_(addr left, addr right, addr *ret)
{
	int check;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_COMPLEX);
	Return(multi_real_complex_(left, right, ret, &check));
	if (check)
		return fmte_("Type error", NULL);

	return 0;
}

_g int multi_lc_number_common_(addr left, addr right, addr *ret)
{
	int check;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_COMPLEX);
	Return(multi_real_complex_(left, right, ret, &check));
	if (check)
		return fmte_("Type error", NULL);

	return 0;
}

static int multi_cc_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	LocalStack stack;
	addr a, b, c, d, ac, bd, ad, bc;

	/* (a+bi)(c+di) = ac-bd + (ad+bc)i */
	Check(GetTypeComplex(left) != ComplexType_rational, "type error");
	Check(GetTypeComplex(right) != ComplexType_rational, "type error");
	push_local(local, &stack);
	GetRealComplex(left, &a);
	GetImagComplex(left, &b);
	GetRealComplex(right, &c);
	GetImagComplex(right, &d);
	multi_rational_local(local, a, c, &ac);
	multi_rational_local(local, b, d, &bd);
	multi_rational_local(local, a, d, &ad);
	multi_rational_local(local, b, c, &bc);
	minus_rational_local(local, ac, bd, &a);
	plus_rational_local(local, ad, bc, &b);
	Return(complex_heap_(ret, a, b));
	rollback_local(local, stack);

	return 0;
}

_g int multi_cc_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	enum MathType type;
	single_float as, bs, cs, ds;
	double_float ad, bd, cd, dd;
	long_float al, bl, cl, dl;
	struct mathcomplex2_struct str;

	/* (a+bi)(c+di) = ac-bd + (ad+bc)i */
	CheckLocal(local);
	CheckType(left, LISPTYPE_COMPLEX);
	CheckType(right, LISPTYPE_COMPLEX);
	Return(getmathcomplex2_addr_(&str, left, right, &type));
	switch (type) {
		case MathType_single:
			as = str.v.s.a;
			bs = str.v.s.b;
			cs = str.v.s.c;
			ds = str.v.s.d;
			complex_single_heap(ret, as*cs - bs*ds, as*ds + bs*cs);
			break;

		case MathType_double:
			ad = str.v.d.a;
			bd = str.v.d.b;
			cd = str.v.d.c;
			dd = str.v.d.d;
			complex_double_heap(ret, ad*cd - bd*dd, ad*dd + bd*cd);
			break;

		case MathType_long:
			al = str.v.l.a;
			bl = str.v.l.b;
			cl = str.v.l.c;
			dl = str.v.l.d;
			complex_long_heap(ret, al*cl - bl*dl, al*dl + bl*cl);
			break;

		case MathType_rational:
			return multi_cc_rational_common_(local, left, right, ret);

		case MathType_complex:
		case MathType_error:
		default:
			*ret = Nil;
			return fmte_("type error", NULL);
	}

	return 0;
}


/*
 *  inverse
 */
static int inverse_complex_rational_(LocalRoot local, addr pos, addr *ret)
{
	int check;
	LocalStack stack;
	addr a, b, a2, b2, ab;

	/* Re:  a/(a*a + b*b)
	 * Im: -b/(a*a + b*b)
	 */
	Return(zerop_complex_(pos, &check));
	if (check)
		return call_division_by_zero1_(NULL, pos);
	GetRealComplex(pos, &a);
	GetImagComplex(pos, &b);
	push_local(local, &stack);
	multi_rational_local(local, a, a, &a2);
	multi_rational_local(local, b, b, &b2);
	plus_rational_local(local, a2, b2, &ab);
	div_rational_local(local, a, ab, &a);
	div_rational_local(local, b, ab, &b);
	sign_reverse_rational_local(local, b, &b);
	Return(complex_heap_(ret, a, b));
	rollback_local(local, stack);

	return 0;
}

_g int inverse_complex_common_(LocalRoot local, addr pos, addr *ret)
{
	enum MathType type;
	single_float as, bs, ds;
	double_float ad, bd, dd;
	long_float al, bl, dl;
	struct mathreal2_struct str;

	CheckLocalType(local, pos, LISPTYPE_COMPLEX);
	Return(getmathcomplex1_inverse_(&str, pos, &type));
	switch (type) {
		case MathType_single:
			as = str.v.s.a;
			bs = str.v.s.b;
			ds = as*as + bs+bs;
			complex_single_heap(ret, as/ds, -bs/ds);
			break;

		case MathType_double:
			ad = str.v.d.a;
			bd = str.v.d.b;
			dd = ad*ad + bd+bd;
			complex_double_heap(ret, ad/dd, -bd/dd);
			break;

		case MathType_long:
			al = str.v.l.a;
			bl = str.v.l.b;
			dl = al*al + bl+bl;
			complex_long_heap(ret, al/dl, -bl/dl);
			break;

		case MathType_rational:
			return inverse_complex_rational_(local, pos, ret);

		case MathType_complex:
		case MathType_error:
		default:
			*ret = 0;
			return fmte_("Type error", NULL);
	}

	return 0;
}


/*
 *  division
 */
static int div_real_complex_(addr left, addr right, addr *value, int *ret)
{
	/* Re:  n*a/(a*a + b*b)
	 * Im: -n*b/(a*a + b*b)
	 */
	enum MathType type;
	single_float ns, as, bs, ds;
	double_float nd, ad, bd, dd;
	long_float nl, al, bl, dl;
	struct mathcomplex2_struct str;

	Return(getmathcomplex2_addr_(&str, left, right, &type));
	switch (type) {
		case MathType_single:
			ns = str.v.s.a;
			as = str.v.s.c;
			bs = str.v.s.d;
			ds = as*as + bs*bs;
			complex_single_heap(value, ns*as/ds, -ns*bs/ds);
			return Result(ret, 0);

		case MathType_double:
			nd = str.v.d.a;
			ad = str.v.d.c;
			bd = str.v.d.d;
			dd = ad*ad + bd*bd;
			complex_double_heap(value, nd*ad/dd, -nd*bd/dd);
			return Result(ret, 0);

		case MathType_long:
			nl = str.v.l.a;
			al = str.v.l.c;
			bl = str.v.l.d;
			dl = al*al + bl*bl;
			complex_long_heap(value, nl*al/dl, -nl*bl/dl);
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

static int div_complex_real_(addr left, addr right, addr *value, int *ret)
{
	/* Re: a/n
	 * Im: b/n
	 */
	enum MathType type;
	single_float ns, as, bs;
	double_float nd, ad, bd;
	long_float nl, al, bl;
	struct mathcomplex2_struct str;

	Return(getmathcomplex2_addr_(&str, left, right, &type));
	switch (type) {
		case MathType_single:
			as = str.v.s.a;
			bs = str.v.s.b;
			ns = str.v.s.c;
			complex_single_heap(value, as/ns, bs/ns);
			return Result(ret, 0);

		case MathType_double:
			ad = str.v.d.a;
			bd = str.v.d.b;
			nd = str.v.d.c;
			complex_double_heap(value, ad/nd, bd/nd);
			return Result(ret, 0);

		case MathType_long:
			al = str.v.l.a;
			bl = str.v.l.b;
			nl = str.v.l.c;
			complex_long_heap(value, al/nl, bl/nl);
			return Result(ret, 0);

		case MathType_rational:
			return Result(ret, 1);

		case MathType_complex:
		case MathType_error:
		default:
			*value = 0;
			*ret = 0;
			return fmte_("type error", NULL);
	}
}

_g int div_rational_complex_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	int check;
	LocalStack stack;
	addr a, b, a2, b2, ab;

	/* Re:  n*a/(a*a + b*b)
	 * Im: -n*b/(a*a + b*b)
	 */
	CheckLocal(local);
	Check(! rationalp(left), "type error");
	CheckType(right, LISPTYPE_COMPLEX);
	Return(zerop_complex_(right, &check));
	if (check)
		return call_division_by_zero2_(NULL, left, right);
	Return(div_real_complex_(left, right, ret, &check));
	if (check) {
		GetRealComplex(right, &a);
		GetImagComplex(right, &b);
		push_local(local, &stack);
		multi_rational_local(local, a, a, &a2);
		multi_rational_local(local, b, b, &b2);
		plus_rational_local(local, a2, b2, &ab);
		multi_rational_local(local, left, a, &a);
		multi_rational_local(local, left, b, &b);
		div_rational_local(local, a, ab, &a);
		div_rational_local(local, b, ab, &b);
		sign_reverse_rational_local(local, b, &b);
		Return(complex_heap_(ret, a, b));
		rollback_local(local, stack);
	}

	return 0;
}

_g int div_complex_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	int check;
	LocalStack stack;
	addr a, b;

	/* Re: a/n
	 * Im: b/n
	 */
	CheckLocal(local);
	CheckType(left, LISPTYPE_COMPLEX);
	Check(! rationalp(right), "type right error");
	if (zerop_rational(right))
		return call_division_by_zero2_(NULL, left, right);
	Return(div_complex_real_(left, right, ret, &check));
	if (check) {
		GetRealComplex(left, &a);
		GetImagComplex(left, &b);
		push_local(local, &stack);
		div_rational_local(local, a, right, &a);
		div_rational_local(local, b, right, &b);
		Return(complex_heap_(ret, a, b));
		rollback_local(local, stack);
	}

	return 0;
}

_g int div_fc_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
	return div_rational_complex_common_(local, left, right, ret);
}

_g int div_cf_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_FIXNUM);
	return div_complex_rational_common_(local, left, right, ret);
}

_g int div_bc_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
	return div_rational_complex_common_(local, left, right, ret);
}

_g int div_cb_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_BIGNUM);
	return div_complex_rational_common_(local, left, right, ret);
}

_g int div_rc_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_RATIO);
	return div_rational_complex_common_(local, left, right, ret);
}

_g int div_cr_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_RATIO);
	return div_complex_rational_common_(local, left, right, ret);
}

_g int div_sc_number_common_(addr left, addr right, addr *ret)
{
	int check;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_COMPLEX);
	Return(div_real_complex_(left, right, ret, &check));
	if (check)
		return fmte_("Type error", NULL);

	return 0;
}

_g int div_cs_number_common_(addr left, addr right, addr *ret)
{
	int check;

	CheckType(left, LISPTYPE_COMPLEX);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	Return(div_complex_real_(left, right, ret, &check));
	if (check)
		return fmte_("Type error", NULL);

	return 0;
}

_g int div_dc_number_common_(addr left, addr right, addr *ret)
{
	int check;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_COMPLEX);
	Return(div_real_complex_(left, right, ret, &check));
	if (check)
		return fmte_("Type error", NULL);

	return 0;
}

_g int div_cd_number_common_(addr left, addr right, addr *ret)
{
	int check;

	CheckType(left, LISPTYPE_COMPLEX);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	Return(div_complex_real_(left, right, ret, &check));
	if (check)
		return fmte_("Type error", NULL);

	return 0;
}

_g int div_lc_number_common_(addr left, addr right, addr *ret)
{
	int check;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_COMPLEX);
	Return(div_real_complex_(left, right, ret, &check));
	if (check)
		return fmte_("Type error", NULL);

	return 0;
}

_g int div_cl_number_common_(addr left, addr right, addr *ret)
{
	int check;

	CheckType(left, LISPTYPE_COMPLEX);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	Return(div_complex_real_(left, right, ret, &check));
	if (check)
		return fmte_("Type error", NULL);

	return 0;
}

static int div_cc_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	LocalStack stack;
	addr a, b, c, d, ac, bd, bc, ad, c2, d2;

	/* (a+bi)(c+di) =
	 *   Re: (ac + bd)/(c*c + d*d)
	 *   Im: (bc - ad)/(c*c + d*d)
	 */
	CheckLocal(local);
	CheckType(left, LISPTYPE_COMPLEX);
	CheckType(right, LISPTYPE_COMPLEX);
	GetRealComplex(left, &a);
	GetImagComplex(left, &b);
	GetRealComplex(right, &c);
	GetImagComplex(right, &d);
	push_local(local, &stack);
	multi_rational_local(local, c, c, &c2);
	multi_rational_local(local, d, d, &d2);
	multi_rational_local(local, a, c, &ac);
	multi_rational_local(local, b, d, &bd);
	multi_rational_local(local, b, c, &bc);
	multi_rational_local(local, a, d, &ad);
	plus_rational_local(local, ac, bd, &ac);
	minus_rational_local(local, bc, ad, &bc);
	plus_rational_local(local, c2, d2, &c2);
	div_rational_local(local, ac, c2, &ac);
	div_rational_local(local, bc, c2, &bc);
	Return(complex_heap_(ret, ac, bc));
	rollback_local(local, stack);

	return 0;
}

_g int div_cc_number_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	enum MathType type;
	single_float as, bs, cs, ds, xs, ys, zs;
	double_float ad, bd, cd, dd, xd, yd, zd;
	long_float al, bl, cl, dl, xl, yl, zl;
	struct mathcomplex2_struct str;

	/* (a+bi)(c+di) =
	 *   Re: (ac + bd)/(c*c + d*d)
	 *   Im: (bc - ad)/(c*c + d*d)
	 */
	CheckLocal(local);
	CheckType(left, LISPTYPE_COMPLEX);
	CheckType(right, LISPTYPE_COMPLEX);
	Return(getmathcomplex2_addr_(&str, left, right, &type));
	switch (type) {
		case MathType_single:
			as = str.v.s.a;
			bs = str.v.s.b;
			cs = str.v.s.c;
			ds = str.v.s.d;
			xs = as*cs + bs*ds;
			ys = bs*cs - as*ds;
			zs = cs*cs + ds*ds;
			complex_single_heap(ret, xs/zs, ys/zs);
			break;

		case MathType_double:
			ad = str.v.d.a;
			bd = str.v.d.b;
			cd = str.v.d.c;
			dd = str.v.d.d;
			xd = ad*cd + bd*dd;
			yd = bd*cd - ad*dd;
			zd = cd*cd + dd*dd;
			complex_double_heap(ret, xd/zd, yd/zd);
			break;

		case MathType_long:
			al = str.v.l.a;
			bl = str.v.l.b;
			cl = str.v.l.c;
			dl = str.v.l.d;
			xl = al*cl + bl*dl;
			yl = bl*cl - al*dl;
			zl = cl*cl + dl*dl;
			complex_long_heap(ret, xl/zl, yl/zl);
			break;

		case MathType_rational:
			return div_cc_rational_common_(local, left, right, ret);

		case MathType_complex:
		case MathType_error:
		default:
			*ret = Nil;
			return fmte_("type error", NULL);
	}

	return 0;
}

