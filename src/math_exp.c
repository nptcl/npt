#include "bignum.h"
#include "cmpl.h"
#include "cmpl_math.h"
#include "condition.h"
#include "ratio.h"
#include "real.h"
#include "rational.h"
#include "real_float.h"
#include "math_exp.h"
#include "math_type.h"
#include <math.h>

/*
 *  exp
 */
struct mathcall_struct {
	void (*call)(addr pos, addr *ret);
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

static void call_complex_common(struct mathcall_struct *ptr, addr pos, addr *ret)
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
			complex_single_heap(ret, af, bf);
			break;

		case ComplexType_double:
			ad = RefDoubleFloat(real);
			bd = RefDoubleFloat(imag);
			(ptr->complex_d)(ad, bd, &ad, &bd);
			complex_double_heap(ret, ad, bd);
			break;

		case ComplexType_long:
			al = RefLongFloat(real);
			bl = RefLongFloat(imag);
			(ptr->complex_l)(al, bl, &al, &bl);
			complex_long_heap(ret, al, bl);
			break;

		case ComplexType_rational:
			af = single_float_rational(real);
			bf = single_float_rational(imag);
			(ptr->complex_f)(af, bf, &af, &bf);
			complex_single_heap(ret, af, bf);
			break;

		default:
			TypeError(pos, COMPLEX);
			return;
	}
}

static void forcef_complex_common(struct mathcall_struct *ptr,
		single_float a, addr *ret)
{
	single_float real, imag;
	(ptr->complex_f)(a, 0.0f, &real, &imag);
	complex_single_heap(ret, real, imag);
}

static void forced_complex_common(struct mathcall_struct *ptr,
		double_float a, addr *ret)
{
	double_float real, imag;
	(ptr->complex_d)(a, 0.0, &real, &imag);
	complex_double_heap(ret, real, imag);
}

static void forcel_complex_common(struct mathcall_struct *ptr,
		long_float a, addr *ret)
{
	long_float real, imag;
	(ptr->complex_l)(a, 0.0L, &real, &imag);
	complex_long_heap(ret, real, imag);
}

void call_common(struct mathcall_struct *ptr, addr pos, addr *ret)
{
	struct mathtype_struct str;

	switch (getmathtype_float(&str, pos)) {
		case MathType_single:
			single_float_check_heap(ret, (ptr->call_s)(str.v.s));
			break;

		case MathType_double:
			double_float_check_heap(ret, (ptr->call_d)(str.v.d));
			break;

		case MathType_long:
			long_float_check_heap(ret, (ptr->call_l)(str.v.l));
			break;

		case MathType_complex:
			call_complex_common(ptr, pos, ret);
			break;

		case MathType_error:
		default:
			TypeError(pos, NUMBER);
			return;
	}
}

void call_range_common(struct mathcall_struct *ptr, addr pos, addr *ret)
{
	single_float vs;
	double_float vd;
	long_float vl;
	struct mathtype_struct str;

	switch (getmathtype_float(&str, pos)) {
		case MathType_single:
			vs = str.v.s;
			if ((ptr->range_f)(vs))
				single_float_check_heap(ret, (ptr->call_s)(vs));
			else
				forcef_complex_common(ptr, vs, ret);
			break;

		case MathType_double:
			vd = str.v.d;
			if ((ptr->range_d)(vd))
				double_float_check_heap(ret, (ptr->call_d)(vd));
			else
				forced_complex_common(ptr, vd, ret);
			break;

		case MathType_long:
			vl = str.v.l;
			if ((ptr->range_l)(vl))
				long_float_check_heap(ret, (ptr->call_l)(vl));
			else
				forcel_complex_common(ptr, vl, ret);
			break;

		case MathType_complex:
			call_complex_common(ptr, pos, ret);
			break;

		case MathType_error:
		default:
			TypeError(pos, NUMBER);
			return;
	}
}

void exp_common(addr pos, addr *ret)
{
	struct mathcall_struct str;

	str.call = exp_common;
	str.complex_f = cexp_f;
	str.complex_d = cexp_d;
	str.complex_l = cexp_l;
	str.call_s = expf;
	str.call_d = exp;
	str.call_l = expl;
	call_common(&str, pos, ret);
}

void sin_common(addr pos, addr *ret)
{
	struct mathcall_struct str;

	str.call = sin_common;
	str.complex_f = csin_f;
	str.complex_d = csin_d;
	str.complex_l = csin_l;
	str.call_s = sinf;
	str.call_d = sin;
	str.call_l = sinl;
	call_common(&str, pos, ret);
}

void cos_common(addr pos, addr *ret)
{
	struct mathcall_struct str;

	str.call = cos_common;
	str.complex_f = ccos_f;
	str.complex_d = ccos_d;
	str.complex_l = ccos_l;
	str.call_s = cosf;
	str.call_d = cos;
	str.call_l = cosl;
	call_common(&str, pos, ret);
}

void tan_common(addr pos, addr *ret)
{
	struct mathcall_struct str;

	str.call = tan_common;
	str.complex_f = ctan_f;
	str.complex_d = ctan_d;
	str.complex_l = ctan_l;
	str.call_s = tanf;
	str.call_d = tan;
	str.call_l = tanl;
	call_common(&str, pos, ret);
}

void sinh_common(addr pos, addr *ret)
{
	struct mathcall_struct str;

	str.call = sinh_common;
	str.complex_f = csinh_f;
	str.complex_d = csinh_d;
	str.complex_l = csinh_l;
	str.call_s = sinhf;
	str.call_d = sinh;
	str.call_l = sinhl;
	call_common(&str, pos, ret);
}

void cosh_common(addr pos, addr *ret)
{
	struct mathcall_struct str;

	str.call = cosh_common;
	str.complex_f = ccosh_f;
	str.complex_d = ccosh_d;
	str.complex_l = ccosh_l;
	str.call_s = coshf;
	str.call_d = cosh;
	str.call_l = coshl;
	call_common(&str, pos, ret);
}

void tanh_common(addr pos, addr *ret)
{
	struct mathcall_struct str;

	str.call = tanh_common;
	str.complex_f = ctanh_f;
	str.complex_d = ctanh_d;
	str.complex_l = ctanh_l;
	str.call_s = tanhf;
	str.call_d = tanh;
	str.call_l = tanhl;
	call_common(&str, pos, ret);
}

static int asinf_range(single_float v)  { return -1.0f <= v && v <= 1.0f; }
static int asind_range(double_float v)  { return -1.0  <= v && v <= 1.0;  }
static int asinl_range(long_float v)    { return -1.0L <= v && v <= 1.0L; }

void asin_common(addr pos, addr *ret)
{
	struct mathcall_struct str;

	str.call = asin_common;
	str.complex_f = casin_f;
	str.complex_d = casin_d;
	str.complex_l = casin_l;
	str.call_s = asinf;
	str.call_d = asin;
	str.call_l = asinl;
	str.range_f = asinf_range;
	str.range_d = asind_range;
	str.range_l = asinl_range;
	call_range_common(&str, pos, ret);
}

void acos_common(addr pos, addr *ret)
{
	struct mathcall_struct str;

	str.call = acos_common;
	str.complex_f = cacos_f;
	str.complex_d = cacos_d;
	str.complex_l = cacos_l;
	str.call_s = acosf;
	str.call_d = acos;
	str.call_l = acosl;
	str.range_f = asinf_range; /* asin */
	str.range_d = asind_range; /* asin */
	str.range_l = asinl_range; /* asin */
	call_range_common(&str, pos, ret);
}

void atan_common(addr pos, addr *ret)
{
	struct mathcall_struct str;

	str.call = atan_common;
	str.complex_f = catan_f;
	str.complex_d = catan_d;
	str.complex_l = catan_l;
	str.call_s = atanf;
	str.call_d = atan;
	str.call_l = atanl;
	call_common(&str, pos, ret);
}

void asinh_common(addr pos, addr *ret)
{
	struct mathcall_struct str;

	str.call = asinh_common;
	str.complex_f = casinh_f;
	str.complex_d = casinh_d;
	str.complex_l = casinh_l;
	str.call_s = asinhf;
	str.call_d = asinh;
	str.call_l = asinhl;
	call_common(&str, pos, ret);
}

static int acoshf_range(single_float v)  { return 1.0f <= v; }
static int acoshd_range(double_float v)  { return 1.0  <= v; }
static int acoshl_range(long_float v)    { return 1.0L <= v; }

void acosh_common(addr pos, addr *ret)
{
	struct mathcall_struct str;

	str.call = acosh_common;
	str.complex_f = cacosh_f;
	str.complex_d = cacosh_d;
	str.complex_l = cacosh_l;
	str.call_s = acoshf;
	str.call_d = acosh;
	str.call_l = acoshl;
	str.range_f = acoshf_range;
	str.range_d = acoshd_range;
	str.range_l = acoshl_range;
	call_range_common(&str, pos, ret);
}

void atanh_common(addr pos, addr *ret)
{
	struct mathcall_struct str;

	str.call = atanh_common;
	str.complex_f = catanh_f;
	str.complex_d = catanh_d;
	str.complex_l = catanh_l;
	str.call_s = atanhf;
	str.call_d = atanh;
	str.call_l = atanhl;
	str.range_f = asinf_range; /* asin */
	str.range_d = asind_range; /* asin */
	str.range_l = asinl_range; /* asin */
	call_range_common(&str, pos, ret);
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

void cis_common(addr pos, addr *ret)
{
	single_float single1, single2;
	double_float double1, double2;
	long_float long1, long2;
	struct mathtype_struct str;

	switch (getmathtype_float(&str, pos)) {
		case MathType_single:
			cis_f(str.v.s, &single1, &single2);
			complex_single_heap(ret, single1, single2);
			break;

		case MathType_double:
			cis_d(str.v.d, &double1, &double2);
			complex_double_heap(ret, double1, double2);
			break;

		case MathType_long:
			cis_l(str.v.l, &long1, &long2);
			complex_long_heap(ret, long1, long2);
			break;

		case MathType_complex:
		case MathType_error:
		default:
			TypeError(pos, REAL);
			return;
	}
}


/*
 *  tan2
 */
void atan2_common(addr left, addr right, addr *ret)
{
	struct mathreal2_struct str;

	switch (getmathreal2_float(&str, left, right)) {
		case MathType_single:
			single_float_check_heap(ret, atan2f(str.v.s.a, str.v.s.b));
			break;

		case MathType_double:
			double_float_check_heap(ret, atan2(str.v.d.a, str.v.d.b));
			break;

		case MathType_long:
			long_float_check_heap(ret, atan2l(str.v.l.a, str.v.l.b));
			break;

		case MathType_complex:
		case MathType_error:
		default:
			fmte("type error", NULL);
			return;
	}
}


/*
 *  log
 */
static void log_natural_complex(addr value, addr *ret)
{
	single_float reals, imags;
	double_float reald, imagd;
	long_float reall, imagl;

	CheckType(value, LISPTYPE_COMPLEX);
	switch (GetTypeComplex(value)) {
		case ComplexType_rational:
		case ComplexType_single:
			single_float_complex(value, &reals, &imags);
			clog_f(reals, imags, &reals, &imags);
			complex_single_heap(ret, reals, imags);
			break;

		case ComplexType_double:
			double_float_complex(value, &reald, &imagd);
			clog_d(reald, imagd, &reald, &imagd);
			complex_double_heap(ret, reald, imagd);
			break;

		case ComplexType_long:
			long_float_complex(value, &reall, &imagl);
			clog_l(reall, imagl, &reall, &imagl);
			complex_long_heap(ret, reall, imagl);
			break;

		case ComplexType_error:
		default:
			TypeError(value, COMPLEX);
			return;
	}
}

void log_natural_common(addr value, addr *ret)
{
	struct mathreal2_struct str;
	single_float reals, imags;
	double_float reald, imagd;
	long_float reall, imagl;

	switch (getmathcomplex1_log(&str, value)) {
		case MathType_single:
			clog_f(str.v.s.a, str.v.s.b, &reals, &imags);
			complex_single_heap(ret, reals, imags);
			break;

		case MathType_double:
			clog_d(str.v.d.a, str.v.d.b, &reald, &imagd);
			complex_double_heap(ret, reald, imagd);
			break;

		case MathType_long:
			clog_l(str.v.l.a, str.v.l.b, &reall, &imagl);
			complex_long_heap(ret, reall, imagl);
			break;

		case MathType_complex:
			log_natural_complex(value, ret);
			break;

		case MathType_rational:
		case MathType_error:
		default:
			fmte("type error", NULL);
			return;
	}
}

void log_base_common(addr value, addr base, addr *ret)
{
	single_float reals, imags;
	double_float reald, imagd;
	long_float reall, imagl;
	struct mathcomplex2_struct str;

	switch (getmathcomplex2_float(&str, value, base)) {
		case MathType_single:
			clogb_f(str.v.s.a, str.v.s.b, str.v.s.c, str.v.s.d, &reals, &imags);
			complex_single_heap(ret, reals, imags);
			break;

		case MathType_double:
			clogb_d(str.v.d.a, str.v.d.b, str.v.d.c, str.v.d.d, &reald, &imagd);
			complex_double_heap(ret, reald, imagd);
			break;

		case MathType_long:
			clogb_l(str.v.l.a, str.v.l.b, str.v.l.c, str.v.l.d, &reall, &imagl);
			complex_long_heap(ret, reall, imagl);
			break;

		case MathType_complex:
		case MathType_rational:
		case MathType_error:
		default:
			fmte("type error", NULL);
			*ret = 0;
			return;
	}
}


/*
 *  phase
 */
void phase_common(addr pos, addr *ret)
{
	single_float sr, si;
	double_float dr, di;
	long_float lr, li;

	/* real */
	switch (GetType(pos)) {
		case LISPTYPE_COMPLEX:
			break;

		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
		case LISPTYPE_RATIO:
		case LISPTYPE_SINGLE_FLOAT:
			single_float_heap(ret, 0.0f);
			return;

		case LISPTYPE_DOUBLE_FLOAT:
			double_float_heap(ret, 0.0);
			return;

		case LISPTYPE_LONG_FLOAT:
			long_float_heap(ret, 0.0L);
			return;

		default:
			TypeError(pos, NUMBER);
			*ret = 0;
			return;
	}

	/* complex */
	switch (GetTypeComplex(pos)) {
		case ComplexType_rational:
		case ComplexType_single:
			single_float_complex(pos, &sr, &si);
			single_float_heap(ret, atan2f(si, sr));
			break;

		case ComplexType_double:
			double_float_complex(pos, &dr, &di);
			double_float_heap(ret, atan2(di, dr));
			break;

		case ComplexType_long:
			long_float_complex(pos, &lr, &li);
			long_float_heap(ret, atan2l(li, lr));
			break;

		case ComplexType_error:
		default:
			TypeError(pos, COMPLEX);
			break;
	}
}

