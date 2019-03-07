/*
 *  Complex math library for Windows
 *    WARNING: This library is a very inaccurasy.
 *
 *  Common Lisp the Language, 2nd Edition
 *  https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node128.html
 *  12.5.2. Trigonometric and Related Functions
 *    Implementation note: These formulae are mathematically correct, assuming
 *    completely accurate computation. They may be terrible methods for
 *    floating-point computation. Implementors should consult a good text
 *    on numerical analysis. The formulae given above are not necessarily
 *    the simplest ones for real-valued computations, either; they are chosen
 *    to define the branch cuts in desirable ways for the complex case.
 */
#include <math.h>
#include "typedef.h"

/* exp(z) = exp(x) * (cos(y) + i*sin(y)) */
void cexp_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_float e = expf(real);
	*re = e * cosf(imag);
	*im = e * sinf(imag);
}

void cexp_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_float e = exp(real);
	*re = e * cos(imag);
	*im = e * sin(imag);
}

void cexp_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_float e = expl(real);
	*re = e * cosl(imag);
	*im = e * sinl(imag);
}

/* log(z) = log|z| + i*phase(z) */
void clog_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	*re = logf(sqrtf(real*real + imag*imag));
	*im = atan2f(imag, real);
}

void clog_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	*re = log(sqrt(real*real + imag*imag));
	*im = atan2(imag, real);
}

void clog_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	*re = logl(sqrtl(real*real + imag*imag));
	*im = atan2l(imag, real);
}

/* sin(z) = sin(x)*cosh(y) + i*cos(x)*sinh(y) */
void csin_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	*re = sinf(real) * coshf(imag);
	*im = cosf(real) * sinhf(imag);
}

void csin_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	*re = sin(real) * cosh(imag);
	*im = cos(real) * sinh(imag);
}

void csin_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	*re = sinl(real) * coshl(imag);
	*im = cosl(real) * sinhl(imag);
}

/* cos(z) = cos(x)*cosh(y) - i*sin(x)*sinh(y) */
void ccos_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	*re =  cosf(real) * coshf(imag);
	*im = -sinf(real) * sinhf(imag);
}

void ccos_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	*re =  cos(real) * cosh(imag);
	*im = -sin(real) * sinh(imag);
}

void ccos_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	*re =  cosl(real) * coshl(imag);
	*im = -sinl(real) * sinhl(imag);
}

/* tan(z) = (sin(2*x) + i*sinh(2*y)) / (cos(2*x) + cosh(2*y)) */
void ctan_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_float x2, y2, denom;

	x2 = 2.0f * real;
	y2 = 2.0f * imag;
	denom = cosf(x2) + coshf(y2);
	*re = sinf(x2) / denom;
	*im = sinhf(y2) / denom;
}

void ctan_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_float x2, y2, denom;

	x2 = 2.0 * real;
	y2 = 2.0 * imag;
	denom = cos(x2) + cosh(y2);
	*re = sin(x2) / denom;
	*im = sinh(y2) / denom;
}

void ctan_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_float x2, y2, denom;

	x2 = 2.0L * real;
	y2 = 2.0L * imag;
	denom = cosl(x2) + coshl(y2);
	*re = sinl(x2) / denom;
	*im = sinhl(y2) / denom;
}

/* sinh(z) = sinh(x)*cos(y) + i*cosh(x)*sin(y) */
void csinh_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	*re = sinhf(real) * cosf(imag);
	*im = coshf(real) * sinf(imag);
}

void csinh_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	*re = sinh(real) * cos(imag);
	*im = cosh(real) * sin(imag);
}

void csinh_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	*re = sinhl(real) * cosl(imag);
	*im = coshl(real) * sinl(imag);
}

/* cosh(z) = cosh(x)*cos(y) i*sinh(x)*sin(y) */
void ccosh_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	*re = coshf(real) * cosf(imag);
	*im = sinhf(real) * sinf(imag);
}

void ccosh_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	*re = cosh(real) * cos(imag);
	*im = sinh(real) * sin(imag);
}

void ccosh_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	*re = coshl(real) * cosl(imag);
	*im = sinhl(real) * sinl(imag);
}

/* tan(z) = (sinh(2*x) + i*sin(2*y)) / (cosh(2*x) + cos(2*y)) */
void ctanh_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_float x2, y2, denom;

	x2 = 2.0f * real;
	y2 = 2.0f * imag;
	denom = coshf(x2) + cosf(y2);
	*re = sinhf(x2) / denom;
	*im = sinf(y2) / denom;
}

void ctanh_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_float x2, y2, denom;

	x2 = 2.0 * real;
	y2 = 2.0 * imag;
	denom = cosh(x2) + cos(y2);
	*re = sinh(x2) / denom;
	*im = sin(y2) / denom;
}

void ctanh_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_float x2, y2, denom;

	x2 = 2.0L * real;
	y2 = 2.0L * imag;
	denom = coshl(x2) + cosl(y2);
	*re = sinhl(x2) / denom;
	*im = sinl(y2) / denom;
}

/* asin(z) = -i*log(i*z + sqrt(1-z*z)) */
void casin_f(single_float a, single_float b, single_float *re, single_float *im)
{
	single_float c, d;

	csqrt_f(1.0f-(a*a-b*b), -2.0f*a*b, &c, &d);
	clog_f(-b+c, a+d, &c, &d);
	*re = d;
	*im = -c;
}

void casin_d(double_float a, double_float b, double_float *re, double_float *im)
{
	double_float c, d;

	csqrt_d(1.0-(a*a-b*b), -2.0*a*b, &c, &d);
	clog_d(-b+c, a+d, &c, &d);
	*re = d;
	*im = -c;
}

void casin_l(long_float a, long_float b, long_float *re, long_float *im)
{
	long_float c, d;

	csqrt_l(1.0L-(a*a-b*b), -2.0L*a*b, &c, &d);
	clog_l(-b+c, a+d, &c, &d);
	*re = d;
	*im = -c;
}

/* acos(z) = -i*log(z + i*sqrt(1-z*z)) */
void cacos_f(single_float a, single_float b, single_float *re, single_float *im)
{
	single_float c, d;

	csqrt_f(1.0f-(a*a-b*b), -2.0f*a*b, &c, &d);
	clog_f(a-d, b+c, &c, &d);
	*re = d;
	*im = -c;
}

void cacos_d(double_float a, double_float b, double_float *re, double_float *im)
{
	double_float c, d;

	csqrt_d(1.0-(a*a-b*b), -2.0*a*b, &c, &d);
	clog_d(a-d, b+c, &c, &d);
	*re = d;
	*im = -c;
}

void cacos_l(long_float a, long_float b, long_float *re, long_float *im)
{
	long_float c, d;

	csqrt_l(1.0L-(a*a-b*b), -2.0L*a*b, &c, &d);
	clog_l(a-d, b+c, &c, &d);
	*re = d;
	*im = -c;
}

/* atan(z) = (log(1+i*z) - log(1-i*z))/(2*i) */
void catan_f(single_float a, single_float b, single_float *re, single_float *im)
{
	single_float p1, p2, m1, m2;

	clog_f(1.0f-b,  a, &p1, &p2);
	clog_f(1.0f+b, -a, &m1, &m2);
	*re =  0.5f * (p2 - m2);
	*im = -0.5f * (p1 - m1);
}

void catan_d(double_float a, double_float b, double_float *re, double_float *im)
{
	double_float p1, p2, m1, m2;

	clog_d(1.0-b,  a, &p1, &p2);
	clog_d(1.0+b, -a, &m1, &m2);
	*re =  0.5 * (p2 - m2);
	*im = -0.5 * (p1 - m1);
}

void catan_l(long_float a, long_float b, long_float *re, long_float *im)
{
	long_float p1, p2, m1, m2;

	clog_l(1.0L-b,  a, &p1, &p2);
	clog_l(1.0L+b, -a, &m1, &m2);
	*re =  0.5L * (p2 - m2);
	*im = -0.5L * (p1 - m1);
}

/* asinh(z) = -i*asin(i*z) */
void casinh_f(single_float a, single_float b, single_float *re, single_float *im)
{
	casin_f(-b, a, &a, &b);
	*re = b;
	*im = -a;
}

void casinh_d(double_float a, double_float b, double_float *re, double_float *im)
{
	casin_d(-b, a, &a, &b);
	*re = b;
	*im = -a;
}

void casinh_l(long_float a, long_float b, long_float *re, long_float *im)
{
	casin_l(-b, a, &a, &b);
	*re = b;
	*im = -a;
}

/* acosh(z) = 2*log(sqrt((z+1)/2) + sqrt((z-1)/2)) */
void cacosh_f(single_float a, single_float b, single_float *re, single_float *im)
{
	single_float p1, p2, m1, m2;

	csqrt_f(0.5f*(a+1.0f), 0.5f*b, &p1, &p2);
	csqrt_f(0.5f*(a-1.0f), 0.5f*b, &m1, &m2);
	clog_f(p1+m1, p2+m2, &a, &b);
	*re = 2.0f*a;
	*im = 2.0f*b;
}

void cacosh_d(double_float a, double_float b, double_float *re, double_float *im)
{
	double_float p1, p2, m1, m2;

	csqrt_d(0.5*(a+1.0), 0.5*b, &p1, &p2);
	csqrt_d(0.5*(a-1.0), 0.5*b, &m1, &m2);
	clog_d(p1+m1, p2+m2, &a, &b);
	*re = 2.0*a;
	*im = 2.0*b;
}

void cacosh_l(long_float a, long_float b, long_float *re, long_float *im)
{
	long_float p1, p2, m1, m2;

	csqrt_l(0.5L*(a+1.0L), 0.5L*b, &p1, &p2);
	csqrt_l(0.5L*(a-1.0L), 0.5L*b, &m1, &m2);
	clog_l(p1+m1, p2+m2, &a, &b);
	*re = 2.0L*a;
	*im = 2.0L*b;
}

/* atanh(z) = atan(iz)/i */
void catanh_f(single_float a, single_float b, single_float *re, single_float *im)
{
	catan_f(-b, a, &a, &b);
	*re = b;
	*im = -a;
}

void catanh_d(double_float a, double_float b, double_float *re, double_float *im)
{
	catan_d(-b, a, &a, &b);
	*re = b;
	*im = -a;
}

void catanh_l(long_float a, long_float b, long_float *re, long_float *im)
{
	catan_l(-b, a, &a, &b);
	*re = b;
	*im = -a;
}

void cabs_f(single_float real, single_float imag, single_float *ret)
{
	*ret = sqrtf(real*real + imag*imag);
}

void cabs_d(double_float real, double_float imag, double_float *ret)
{
	*ret = sqrt(real*real + imag*imag);
}

void cabs_l(long_float real, long_float imag, long_float *ret)
{
	*ret = sqrtl(real*real + imag*imag);
}

