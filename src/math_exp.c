#include "bignum.h"
#include "cmpl.h"
#include "cmpl_arch.h"
#include "cmpl_math.h"
#include "condition.h"
#include "float_object.h"
#include "ratio.h"
#include "real.h"
#include "rational.h"
#include "math_exp.h"
#include "math_type.h"
#include <math.h>

/*
 *  exp
 */
struct mathcall_struct {
	void (*complex_f)(single_float, single_float, single_float *, single_float *);
	void (*complex_d)(double_float, double_float, double_float *, double_float *);
	void (*complex_l)(long_float, long_float, long_float *, long_float *);
	single_float (*call_s)(single_float);
	double_float (*call_d)(double_float);
	long_float (*call_l)(long_float);
	int (*range_f)(single_float);
	int (*range_d)(double_float);
	int (*range_l)(long_float);
};

static int call_complex_common_(struct mathcall_struct *ptr, addr pos, addr *ret)
{
	single_float af, bf;
	double_float ad, bd;
	long_float al, bl;
	addr real, imag;

	CheckType(pos, LISPTYPE_COMPLEX);
	GetRealComplex(pos, &real);
	GetImagComplex(pos, &imag);
	switch (GetTypeComplex(pos)) {
		case ComplexType_single:
			af = RefSingleFloat(real);
			bf = RefSingleFloat(imag);
			(ptr->complex_f)(af, bf, &af, &bf);
			return complex_single_heap_(ret, af, bf);

		case ComplexType_double:
			ad = RefDoubleFloat(real);
			bd = RefDoubleFloat(imag);
			(ptr->complex_d)(ad, bd, &ad, &bd);
			return complex_double_heap_(ret, ad, bd);

		case ComplexType_long:
			al = RefLongFloat(real);
			bl = RefLongFloat(imag);
			(ptr->complex_l)(al, bl, &al, &bl);
			return complex_long_heap_(ret, al, bl);

		case ComplexType_rational:
			Return(single_float_rational_(real, &af));
			Return(single_float_rational_(imag, &bf));
			(ptr->complex_f)(af, bf, &af, &bf);
			return complex_single_heap_(ret, af, bf);

		default:
			*ret = Nil;
			return TypeError_(pos, COMPLEX);
	}

	return 0;
}

static int forcef_complex_common_(struct mathcall_struct *ptr,
		single_float a, addr *ret)
{
	single_float real, imag;
	(ptr->complex_f)(a, 0.0f, &real, &imag);
	return complex_single_heap_(ret, real, imag);
}

static int forced_complex_common_(struct mathcall_struct *ptr,
		double_float a, addr *ret)
{
	double_float real, imag;
	(ptr->complex_d)(a, 0.0, &real, &imag);
	return complex_double_heap_(ret, real, imag);
}

static int forcel_complex_common_(struct mathcall_struct *ptr,
		long_float a, addr *ret)
{
	long_float real, imag;
	(ptr->complex_l)(a, 0.0L, &real, &imag);
	return complex_long_heap_(ret, real, imag);
}

_g int call_common_(struct mathcall_struct *ptr, addr pos, addr *ret)
{
	enum MathType type;
	struct mathtype_struct str;

	Return(getmathtype_float_(&str, pos, &type));
	switch (type) {
		case MathType_single:
			return single_float_check_heap_(ret, (ptr->call_s)(str.v.s));

		case MathType_double:
			return double_float_check_heap_(ret, (ptr->call_d)(str.v.d));

		case MathType_long:
			return long_float_check_heap_(ret, (ptr->call_l)(str.v.l));

		case MathType_complex:
			return call_complex_common_(ptr, pos, ret);

		case MathType_error:
		default:
			*ret = Nil;
			return TypeError_(pos, NUMBER);
	}
}

_g int call_range_common_(struct mathcall_struct *ptr, addr pos, addr *ret)
{
	enum MathType type;
	single_float vs;
	double_float vd;
	long_float vl;
	struct mathtype_struct str;

	Return(getmathtype_float_(&str, pos, &type));
	switch (type) {
		case MathType_single:
			vs = str.v.s;
			if ((ptr->range_f)(vs))
				return single_float_check_heap_(ret, (ptr->call_s)(vs));
			else
				return forcef_complex_common_(ptr, vs, ret);
			break;

		case MathType_double:
			vd = str.v.d;
			if ((ptr->range_d)(vd))
				return double_float_check_heap_(ret, (ptr->call_d)(vd));
			else
				return forced_complex_common_(ptr, vd, ret);
			break;

		case MathType_long:
			vl = str.v.l;
			if ((ptr->range_l)(vl))
				return long_float_check_heap_(ret, (ptr->call_l)(vl));
			else
				return forcel_complex_common_(ptr, vl, ret);
			break;

		case MathType_complex:
			return call_complex_common_(ptr, pos, ret);

		case MathType_error:
		default:
			*ret = Nil;
			return TypeError_(pos, NUMBER);
	}

	return 0;
}

_g int exp_common_(addr pos, addr *ret)
{
	struct mathcall_struct str;

	str.complex_f = cexp_f;
	str.complex_d = cexp_d;
	str.complex_l = cexp_l;
	str.call_s = expf;
	str.call_d = exp;
	str.call_l = expl;
	return call_common_(&str, pos, ret);
}

_g int sin_common_(addr pos, addr *ret)
{
	struct mathcall_struct str;

	str.complex_f = csin_f;
	str.complex_d = csin_d;
	str.complex_l = csin_l;
	str.call_s = sinf;
	str.call_d = sin;
	str.call_l = sinl;
	return call_common_(&str, pos, ret);
}

_g int cos_common_(addr pos, addr *ret)
{
	struct mathcall_struct str;

	str.complex_f = ccos_f;
	str.complex_d = ccos_d;
	str.complex_l = ccos_l;
	str.call_s = cosf;
	str.call_d = cos;
	str.call_l = cosl;
	return call_common_(&str, pos, ret);
}

_g int tan_common_(addr pos, addr *ret)
{
	struct mathcall_struct str;

	str.complex_f = ctan_f;
	str.complex_d = ctan_d;
	str.complex_l = ctan_l;
	str.call_s = tanf;
	str.call_d = tan;
	str.call_l = tanl;
	return call_common_(&str, pos, ret);
}

_g int sinh_common_(addr pos, addr *ret)
{
	struct mathcall_struct str;

	str.complex_f = csinh_f;
	str.complex_d = csinh_d;
	str.complex_l = csinh_l;
	str.call_s = sinhf;
	str.call_d = sinh;
	str.call_l = sinhl;
	return call_common_(&str, pos, ret);
}

_g int cosh_common_(addr pos, addr *ret)
{
	struct mathcall_struct str;

	str.complex_f = ccosh_f;
	str.complex_d = ccosh_d;
	str.complex_l = ccosh_l;
	str.call_s = coshf;
	str.call_d = cosh;
	str.call_l = coshl;
	return call_common_(&str, pos, ret);
}

_g int tanh_common_(addr pos, addr *ret)
{
	struct mathcall_struct str;

	str.complex_f = ctanh_f;
	str.complex_d = ctanh_d;
	str.complex_l = ctanh_l;
	str.call_s = tanhf;
	str.call_d = tanh;
	str.call_l = tanhl;
	return call_common_(&str, pos, ret);
}

static int asinf_range(single_float v)  { return -1.0f <= v && v <= 1.0f; }
static int asind_range(double_float v)  { return -1.0  <= v && v <= 1.0;  }
static int asinl_range(long_float v)    { return -1.0L <= v && v <= 1.0L; }

_g int asin_common_(addr pos, addr *ret)
{
	struct mathcall_struct str;

	str.complex_f = casin_f;
	str.complex_d = casin_d;
	str.complex_l = casin_l;
	str.call_s = asinf;
	str.call_d = asin;
	str.call_l = asinl;
	str.range_f = asinf_range;
	str.range_d = asind_range;
	str.range_l = asinl_range;
	return call_range_common_(&str, pos, ret);
}

_g int acos_common_(addr pos, addr *ret)
{
	struct mathcall_struct str;

	str.complex_f = cacos_f;
	str.complex_d = cacos_d;
	str.complex_l = cacos_l;
	str.call_s = acosf;
	str.call_d = acos;
	str.call_l = acosl;
	str.range_f = asinf_range; /* asin */
	str.range_d = asind_range; /* asin */
	str.range_l = asinl_range; /* asin */
	return call_range_common_(&str, pos, ret);
}

_g int atan_common_(addr pos, addr *ret)
{
	struct mathcall_struct str;

	str.complex_f = catan_f;
	str.complex_d = catan_d;
	str.complex_l = catan_l;
	str.call_s = atanf;
	str.call_d = atan;
	str.call_l = atanl;
	return call_common_(&str, pos, ret);
}

_g int asinh_common_(addr pos, addr *ret)
{
	struct mathcall_struct str;

	str.complex_f = casinh_f;
	str.complex_d = casinh_d;
	str.complex_l = casinh_l;
	str.call_s = asinhf;
	str.call_d = asinh;
	str.call_l = asinhl;
	return call_common_(&str, pos, ret);
}

static int acoshf_range(single_float v)  { return 1.0f <= v; }
static int acoshd_range(double_float v)  { return 1.0  <= v; }
static int acoshl_range(long_float v)    { return 1.0L <= v; }

_g int acosh_common_(addr pos, addr *ret)
{
	struct mathcall_struct str;

	str.complex_f = cacosh_f;
	str.complex_d = cacosh_d;
	str.complex_l = cacosh_l;
	str.call_s = acoshf;
	str.call_d = acosh;
	str.call_l = acoshl;
	str.range_f = acoshf_range;
	str.range_d = acoshd_range;
	str.range_l = acoshl_range;
	return call_range_common_(&str, pos, ret);
}

_g int atanh_common_(addr pos, addr *ret)
{
	struct mathcall_struct str;

	str.complex_f = catanh_f;
	str.complex_d = catanh_d;
	str.complex_l = catanh_l;
	str.call_s = atanhf;
	str.call_d = atanh;
	str.call_l = atanhl;
	str.range_f = asinf_range; /* asin */
	str.range_d = asind_range; /* asin */
	str.range_l = asinl_range; /* asin */
	return call_range_common_(&str, pos, ret);
}


/*
 *  cis
 */
static inline void cis_f(single_float x, single_float *Re, single_float *Im)
{
	*Re = cosf(x);
	*Im = sinf(x);
}

static inline void cis_d(double_float x, double_float *Re, double_float *Im)
{
	*Re = cos(x);
	*Im = sin(x);
}

static inline void cis_l(long_float x, long_float *Re, long_float *Im)
{
	*Re = cosl(x);
	*Im = sinl(x);
}

_g int cis_common_(addr pos, addr *ret)
{
	enum MathType type;
	single_float single1, single2;
	double_float double1, double2;
	long_float long1, long2;
	struct mathtype_struct str;

	Return(getmathtype_float_(&str, pos, &type));
	switch (type) {
		case MathType_single:
			cis_f(str.v.s, &single1, &single2);
			return complex_single_heap_(ret, single1, single2);

		case MathType_double:
			cis_d(str.v.d, &double1, &double2);
			return complex_double_heap_(ret, double1, double2);

		case MathType_long:
			cis_l(str.v.l, &long1, &long2);
			return complex_long_heap_(ret, long1, long2);

		case MathType_complex:
		case MathType_error:
		default:
			*ret = Nil;
			return TypeError_(pos, REAL);
	}
}


/*
 *  tan2
 */
_g int atan2_common_(addr left, addr right, addr *ret)
{
	enum MathType type;
	struct mathreal2_struct str;

	Return(getmathreal2_float_(&str, left, right, &type));
	switch (type) {
		case MathType_single:
			return single_float_check_heap_(ret, atan2f(str.v.s.a, str.v.s.b));

		case MathType_double:
			return double_float_check_heap_(ret, atan2(str.v.d.a, str.v.d.b));

		case MathType_long:
			return long_float_check_heap_(ret, atan2l(str.v.l.a, str.v.l.b));

		case MathType_complex:
		case MathType_error:
		default:
			*ret = Nil;
			return fmte_("type error", NULL);
	}
}

_g int atan_optional_common_(addr var, addr opt, addr *ret)
{
	if (opt == Unbound)
		return atan_common_(var, ret);
	else
		return atan2_common_(var, opt, ret);
}


/*
 *  log
 */
static int log_natural_complex_(addr value, addr *ret)
{
	single_float reals, imags;
	double_float reald, imagd;
	long_float reall, imagl;

	CheckType(value, LISPTYPE_COMPLEX);
	switch (GetTypeComplex(value)) {
		case ComplexType_rational:
		case ComplexType_single:
			Return(single_float_complex_(value, &reals, &imags));
			clog_f(reals, imags, &reals, &imags);
			return complex_single_heap_(ret, reals, imags);

		case ComplexType_double:
			Return(double_float_complex_(value, &reald, &imagd));
			clog_d(reald, imagd, &reald, &imagd);
			return complex_double_heap_(ret, reald, imagd);

		case ComplexType_long:
			Return(long_float_complex_(value, &reall, &imagl));
			clog_l(reall, imagl, &reall, &imagl);
			return complex_long_heap_(ret, reall, imagl);

		case ComplexType_error:
		default:
			*ret = Nil;
			return TypeError_(value, COMPLEX);
	}
}

_g int log_natural_common_(addr value, addr *ret)
{
	enum MathType type;
	struct mathreal2_struct str;
	single_float reals, imags;
	double_float reald, imagd;
	long_float reall, imagl;

	Return(getmathcomplex1_log_(&str, value, &type));
	switch (type) {
		case MathType_single:
			clog_f(str.v.s.a, str.v.s.b, &reals, &imags);
			return complex_single_heap_(ret, reals, imags);

		case MathType_double:
			clog_d(str.v.d.a, str.v.d.b, &reald, &imagd);
			return complex_double_heap_(ret, reald, imagd);

		case MathType_long:
			clog_l(str.v.l.a, str.v.l.b, &reall, &imagl);
			return complex_long_heap_(ret, reall, imagl);

		case MathType_complex:
			return log_natural_complex_(value, ret);

		case MathType_rational:
		case MathType_error:
		default:
			*ret = Nil;
			return fmte_("type error", NULL);
	}
}

_g int log_base_common_(addr value, addr base, addr *ret)
{
	enum MathType type;
	single_float reals, imags;
	double_float reald, imagd;
	long_float reall, imagl;
	struct mathcomplex2_struct str;

	Return(getmathcomplex2_float_(&str, value, base, &type));
	switch (type) {
		case MathType_single:
			Return(clogb_f_(str.v.s.a, str.v.s.b, str.v.s.c, str.v.s.d, &reals, &imags));
			return complex_single_heap_(ret, reals, imags);

		case MathType_double:
			Return(clogb_d_(str.v.d.a, str.v.d.b, str.v.d.c, str.v.d.d, &reald, &imagd));
			return complex_double_heap_(ret, reald, imagd);

		case MathType_long:
			Return(clogb_l_(str.v.l.a, str.v.l.b, str.v.l.c, str.v.l.d, &reall, &imagl));
			return complex_long_heap_(ret, reall, imagl);

		case MathType_complex:
		case MathType_rational:
		case MathType_error:
		default:
			*ret = Nil;
			return fmte_("type error", NULL);
	}
}

_g int log_common_(addr value, addr base, addr *ret)
{
	if (base == Unbound)
		return log_natural_common_(value, ret);
	else
		return log_base_common_(value, base, ret);
}


/*
 *  phase
 */
static int phase_complex_common_(addr pos, addr *ret)
{
	single_float sr, si;
	double_float dr, di;
	long_float lr, li;

	CheckType(pos, LISPTYPE_COMPLEX);
	switch (GetTypeComplex(pos)) {
		case ComplexType_rational:
		case ComplexType_single:
			Return(single_float_complex_(pos, &sr, &si));
			single_float_heap(ret, atan2f(si, sr));
			break;

		case ComplexType_double:
			Return(double_float_complex_(pos, &dr, &di));
			double_float_heap(ret, atan2(di, dr));
			break;

		case ComplexType_long:
			Return(long_float_complex_(pos, &lr, &li));
			long_float_heap(ret, atan2l(li, lr));
			break;

		case ComplexType_error:
		default:
			*ret = Nil;
			return TypeError_(pos, COMPLEX);
	}

	return 0;
}

_g int phase_common_(addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_COMPLEX:
			return phase_complex_common_(pos, ret);

		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
		case LISPTYPE_RATIO:
		case LISPTYPE_SINGLE_FLOAT:
			single_float_heap(ret, 0.0f);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			double_float_heap(ret, 0.0);
			break;

		case LISPTYPE_LONG_FLOAT:
			long_float_heap(ret, 0.0L);
			break;

		default:
			*ret = Nil;
			return TypeError_(pos, NUMBER);
	}

	return 0;
}

