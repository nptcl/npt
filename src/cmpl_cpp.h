#include <cmath>
#include <complex>
#include "typedef.h"

typedef std::complex<float> single_complex;
typedef std::complex<double> double_complex;
typedef std::complex<long double> long_complex;

_g void cexp_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::exp(c);
	*re = c.real();
	*im = c.imag();
}

_g void cexp_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::exp(c);
	*re = c.real();
	*im = c.imag();
}

_g void cexp_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::exp(c);
	*re = c.real();
	*im = c.imag();
}

_g void clog_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::log(c);
	*re = c.real();
	*im = c.imag();
}

_g void clog_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::log(c);
	*re = c.real();
	*im = c.imag();
}

_g void clog_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::log(c);
	*re = c.real();
	*im = c.imag();
}

_g void csin_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::sin(c);
	*re = c.real();
	*im = c.imag();
}

_g void csin_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::sin(c);
	*re = c.real();
	*im = c.imag();
}

_g void csin_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::sin(c);
	*re = c.real();
	*im = c.imag();
}

_g void ccos_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::cos(c);
	*re = c.real();
	*im = c.imag();
}

_g void ccos_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::cos(c);
	*re = c.real();
	*im = c.imag();
}

_g void ccos_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::cos(c);
	*re = c.real();
	*im = c.imag();
}

_g void ctan_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::tan(c);
	*re = c.real();
	*im = c.imag();
}

_g void ctan_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::tan(c);
	*re = c.real();
	*im = c.imag();
}

_g void ctan_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::tan(c);
	*re = c.real();
	*im = c.imag();
}

_g void csinh_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::sinh(c);
	*re = c.real();
	*im = c.imag();
}

_g void csinh_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::sinh(c);
	*re = c.real();
	*im = c.imag();
}

_g void csinh_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::sinh(c);
	*re = c.real();
	*im = c.imag();
}

_g void ccosh_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::cosh(c);
	*re = c.real();
	*im = c.imag();
}

_g void ccosh_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::cosh(c);
	*re = c.real();
	*im = c.imag();
}

_g void ccosh_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::cosh(c);
	*re = c.real();
	*im = c.imag();
}

_g void ctanh_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::tanh(c);
	*re = c.real();
	*im = c.imag();
}

_g void ctanh_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::tanh(c);
	*re = c.real();
	*im = c.imag();
}

_g void ctanh_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::tanh(c);
	*re = c.real();
	*im = c.imag();
}

_g void casin_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::asin(c);
	*re = c.real();
	*im = c.imag();
}

_g void casin_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::asin(c);
	*re = c.real();
	*im = c.imag();
}

_g void casin_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::asin(c);
	*re = c.real();
	*im = c.imag();
}

_g void cacos_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::acos(c);
	*re = c.real();
	*im = c.imag();
}

_g void cacos_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::acos(c);
	*re = c.real();
	*im = c.imag();
}

_g void cacos_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::acos(c);
	*re = c.real();
	*im = c.imag();
}

_g void catan_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::atan(c);
	*re = c.real();
	*im = c.imag();
}

_g void catan_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::atan(c);
	*re = c.real();
	*im = c.imag();
}

_g void catan_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::atan(c);
	*re = c.real();
	*im = c.imag();
}

_g void casinh_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::asinh(c);
	*re = c.real();
	*im = c.imag();
}

_g void casinh_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::asinh(c);
	*re = c.real();
	*im = c.imag();
}

_g void casinh_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::asinh(c);
	*re = c.real();
	*im = c.imag();
}

_g void cacosh_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::acosh(c);
	*re = c.real();
	*im = c.imag();
}

_g void cacosh_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::acosh(c);
	*re = c.real();
	*im = c.imag();
}

_g void cacosh_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::acosh(c);
	*re = c.real();
	*im = c.imag();
}

_g void catanh_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	single_complex c(real, imag);
	c = std::atanh(c);
	*re = c.real();
	*im = c.imag();
}

_g void catanh_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	double_complex c(real, imag);
	c = std::atanh(c);
	*re = c.real();
	*im = c.imag();
}

_g void catanh_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	long_complex c(real, imag);
	c = std::atanh(c);
	*re = c.real();
	*im = c.imag();
}

_g void cabs_f(single_float real, single_float imag, single_float *ret)
{
	single_complex c(real, imag);
	*ret = abs(c);
}

_g void cabs_d(double_float real, double_float imag, double_float *ret)
{
	double_complex c(real, imag);
	*ret = abs(c);
}

_g void cabs_l(long_float real, long_float imag, long_float *ret)
{
	long_complex c(real, imag);
	*ret = abs(c);
}

