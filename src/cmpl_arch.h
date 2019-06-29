#ifndef __CMPL_ARCH_HEADER__
#define __CMPL_ARCH_HEADER__

#include "typedef.h"

_g void cexp_f(single_float real, single_float imag, single_float *re, single_float *im);
_g void cexp_d(double_float real, double_float imag, double_float *re, double_float *im);
_g void cexp_l(long_float real, long_float imag, long_float *re, long_float *im);
_g void clog_f(single_float real, single_float imag, single_float *re, single_float *im);
_g void clog_d(double_float real, double_float imag, double_float *re, double_float *im);
_g void clog_l(long_float real, long_float imag, long_float *re, long_float *im);

_g void csin_f(single_float real, single_float imag, single_float *re, single_float *im);
_g void csin_d(double_float real, double_float imag, double_float *re, double_float *im);
_g void csin_l(long_float real, long_float imag, long_float *re, long_float *im);
_g void ccos_f(single_float real, single_float imag, single_float *re, single_float *im);
_g void ccos_d(double_float real, double_float imag, double_float *re, double_float *im);
_g void ccos_l(long_float real, long_float imag, long_float *re, long_float *im);
_g void ctan_f(single_float real, single_float imag, single_float *re, single_float *im);
_g void ctan_d(double_float real, double_float imag, double_float *re, double_float *im);
_g void ctan_l(long_float real, long_float imag, long_float *re, long_float *im);

_g void csinh_f(single_float real, single_float imag, single_float *re, single_float *im);
_g void csinh_d(double_float real, double_float imag, double_float *re, double_float *im);
_g void csinh_l(long_float real, long_float imag, long_float *re, long_float *im);
_g void ccosh_f(single_float real, single_float imag, single_float *re, single_float *im);
_g void ccosh_d(double_float real, double_float imag, double_float *re, double_float *im);
_g void ccosh_l(long_float real, long_float imag, long_float *re, long_float *im);
_g void ctanh_f(single_float real, single_float imag, single_float *re, single_float *im);
_g void ctanh_d(double_float real, double_float imag, double_float *re, double_float *im);
_g void ctanh_l(long_float real, long_float imag, long_float *re, long_float *im);

_g void casin_f(single_float real, single_float imag, single_float *re, single_float *im);
_g void casin_d(double_float real, double_float imag, double_float *re, double_float *im);
_g void casin_l(long_float real, long_float imag, long_float *re, long_float *im);
_g void cacos_f(single_float real, single_float imag, single_float *re, single_float *im);
_g void cacos_d(double_float real, double_float imag, double_float *re, double_float *im);
_g void cacos_l(long_float real, long_float imag, long_float *re, long_float *im);
_g void catan_f(single_float real, single_float imag, single_float *re, single_float *im);
_g void catan_d(double_float real, double_float imag, double_float *re, double_float *im);
_g void catan_l(long_float real, long_float imag, long_float *re, long_float *im);

_g void casinh_f(single_float real, single_float imag, single_float *re, single_float *im);
_g void casinh_d(double_float real, double_float imag, double_float *re, double_float *im);
_g void casinh_l(long_float real, long_float imag, long_float *re, long_float *im);
_g void cacosh_f(single_float real, single_float imag, single_float *re, single_float *im);
_g void cacosh_d(double_float real, double_float imag, double_float *re, double_float *im);
_g void cacosh_l(long_float real, long_float imag, long_float *re, long_float *im);
_g void catanh_f(single_float real, single_float imag, single_float *re, single_float *im);
_g void catanh_d(double_float real, double_float imag, double_float *re, double_float *im);
_g void catanh_l(long_float real, long_float imag, long_float *re, long_float *im);

_g void cabs_f(single_float real, single_float imag, single_float *ret);
_g void cabs_d(double_float real, double_float imag, double_float *ret);
_g void cabs_l(long_float real, long_float imag, long_float *ret);

#endif

