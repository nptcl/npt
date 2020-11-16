#include <cmath>
#include <complex>
#include "typedef.h"

typedef std::complex<float> single_complex;
typedef std::complex<double> double_complex;
typedef std::complex<long double> long_complex;

void cexp_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::exp(c);
	*re = c.real();
	*im = c.imag();
}

void cexp_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::exp(c);
	*re = c.real();
	*im = c.imag();
}

void cexp_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::exp(c);
	*re = c.real();
	*im = c.imag();
}

void clog_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::log(c);
	*re = c.real();
	*im = c.imag();
}

void clog_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::log(c);
	*re = c.real();
	*im = c.imag();
}

void clog_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::log(c);
	*re = c.real();
	*im = c.imag();
}

void csin_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::sin(c);
	*re = c.real();
	*im = c.imag();
}

void csin_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::sin(c);
	*re = c.real();
	*im = c.imag();
}

void csin_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::sin(c);
	*re = c.real();
	*im = c.imag();
}

void ccos_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::cos(c);
	*re = c.real();
	*im = c.imag();
}

void ccos_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::cos(c);
	*re = c.real();
	*im = c.imag();
}

void ccos_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::cos(c);
	*re = c.real();
	*im = c.imag();
}

void ctan_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::tan(c);
	*re = c.real();
	*im = c.imag();
}

void ctan_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::tan(c);
	*re = c.real();
	*im = c.imag();
}

void ctan_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::tan(c);
	*re = c.real();
	*im = c.imag();
}

void csinh_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::sinh(c);
	*re = c.real();
	*im = c.imag();
}

void csinh_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::sinh(c);
	*re = c.real();
	*im = c.imag();
}

void csinh_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::sinh(c);
	*re = c.real();
	*im = c.imag();
}

void ccosh_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::cosh(c);
	*re = c.real();
	*im = c.imag();
}

void ccosh_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::cosh(c);
	*re = c.real();
	*im = c.imag();
}

void ccosh_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::cosh(c);
	*re = c.real();
	*im = c.imag();
}

void ctanh_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::tanh(c);
	*re = c.real();
	*im = c.imag();
}

void ctanh_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::tanh(c);
	*re = c.real();
	*im = c.imag();
}

void ctanh_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::tanh(c);
	*re = c.real();
	*im = c.imag();
}

void casin_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::asin(c);
	*re = c.real();
	*im = c.imag();
}

void casin_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::asin(c);
	*re = c.real();
	*im = c.imag();
}

void casin_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::asin(c);
	*re = c.real();
	*im = c.imag();
}

void cacos_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::acos(c);
	*re = c.real();
	*im = c.imag();
}

void cacos_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::acos(c);
	*re = c.real();
	*im = c.imag();
}

void cacos_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::acos(c);
	*re = c.real();
	*im = c.imag();
}

void catan_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::atan(c);
	*re = c.real();
	*im = c.imag();
}

void catan_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::atan(c);
	*re = c.real();
	*im = c.imag();
}

void catan_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::atan(c);
	*re = c.real();
	*im = c.imag();
}

void casinh_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::asinh(c);
	*re = c.real();
	*im = c.imag();
}

void casinh_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::asinh(c);
	*re = c.real();
	*im = c.imag();
}

void casinh_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::asinh(c);
	*re = c.real();
	*im = c.imag();
}

void cacosh_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::acosh(c);
	*re = c.real();
	*im = c.imag();
}

void cacosh_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::acosh(c);
	*re = c.real();
	*im = c.imag();
}

void cacosh_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::acosh(c);
	*re = c.real();
	*im = c.imag();
}

void catanh_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::atanh(c);
	*re = c.real();
	*im = c.imag();
}

void catanh_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::atanh(c);
	*re = c.real();
	*im = c.imag();
}

void catanh_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::atanh(c);
	*re = c.real();
	*im = c.imag();
}

void cabs_f(single_float real, single_float imag, single_float *ret)
{
	single_complex c(real, imag);
	*ret = abs(c);
}

void cabs_d(double_float real, double_float imag, double_float *ret)
{
	double_complex c(real, imag);
	*ret = abs(c);
}

void cabs_l(long_float real, long_float imag, long_float *ret)
{
	long_complex c(real, imag);
	*ret = abs(c);
}

