#include <complex.h>
#include <math.h>
#include "typedef.h"

typedef float complex single_complex;
typedef double complex double_complex;
typedef long double complex long_complex;

#ifdef __clang__
#define cexpl cexp
#define csinl csin
#define ccosl ccos
#define ctanl ctan
#define csinhl csinh
#define ccoshl ccosh
#define ctanhl ctanh
#define casinl casin
#define cacosl cacos
#define catanl catan
#define casinhl casinh
#define cacoshl cacosh
#define catanhl catanh

static single_complex clogf_define(single_complex z)
{
	return logf(cabsf(z)) + I * cargf(z);
}
static double_complex clogd_define(double_complex z)
{
	return log(cabs(z)) + I * carg(z);
}
static long_complex clogl_define(long_complex z)
{
	return logl(cabsl(z)) + I * cargl(z);
}
#else
#define clogf_define clogf
#define clogd_define clog
#define clogl_define clogl
#endif

_g void cexp_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c = cexpf(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void cexp_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c = cexp(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void cexp_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c = cexpl(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void clog_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c = clogf_define(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void clog_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c = clogd_define(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void clog_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c = clogl_define(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void csin_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c = csinf(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void csin_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c = csin(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void csin_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c = csinl(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void ccos_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c = ccosf(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void ccos_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c = ccos(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void ccos_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c = ccosl(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void ctan_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c = ctanf(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void ctan_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c = ctan(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void ctan_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c = ctanl(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void csinh_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c = csinhf(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void csinh_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c = csinh(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void csinh_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c = csinhl(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void ccosh_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c = ccoshf(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void ccosh_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c = ccosh(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void ccosh_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c = ccoshl(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void ctanh_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c = ctanhf(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void ctanh_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c = ctanh(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void ctanh_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c = ctanhl(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void casin_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c = casinf(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void casin_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c = casin(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void casin_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c = casinl(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void cacos_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c = cacosf(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void cacos_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c = cacos(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void cacos_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c = cacosl(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void catan_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c = catanf(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void catan_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c = catan(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void catan_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c = catanl(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void casinh_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c = casinhf(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void casinh_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c = casinh(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void casinh_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c = casinhl(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void cacosh_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c = cacoshf(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void cacosh_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c = cacosh(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void cacosh_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c = cacoshl(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void catanh_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c = catanhf(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void catanh_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c = catanh(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void catanh_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c = catanhl(real + imag * I);
	*re = creal(c);
	*im = cimag(c);
}

_g void cabs_f(single_float real, single_float imag, single_float *ret)
{
	single_complex c = real + imag * I;
	*ret = cabsf(c);
}

_g void cabs_d(double_float real, double_float imag, double_float *ret)
{
	double_complex c = real + imag * I;
	*ret = cabs(c);
}

_g void cabs_l(long_float real, long_float imag, long_float *ret)
{
	long_complex c = real + imag * I;
	*ret = cabsl(c);
}

