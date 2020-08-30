#ifndef __CMPL_ARCH_HEADER__
#define __CMPL_ARCH_HEADER__

#include "typedef.h"

#define cexp_f _n(cexp_f)
#define cexp_d _n(cexp_d)
#define cexp_l _n(cexp_l)
#define clog_f _n(clog_f)
#define clog_d _n(clog_d)
#define clog_l _n(clog_l)
#define csin_f _n(csin_f)
#define csin_d _n(csin_d)
#define csin_l _n(csin_l)
#define ccos_f _n(ccos_f)
#define ccos_d _n(ccos_d)
#define ccos_l _n(ccos_l)
#define ctan_f _n(ctan_f)
#define ctan_d _n(ctan_d)
#define ctan_l _n(ctan_l)
#define csinh_f _n(csinh_f)
#define csinh_d _n(csinh_d)
#define csinh_l _n(csinh_l)
#define ccosh_f _n(ccosh_f)
#define ccosh_d _n(ccosh_d)
#define ccosh_l _n(ccosh_l)
#define ctanh_f _n(ctanh_f)
#define ctanh_d _n(ctanh_d)
#define ctanh_l _n(ctanh_l)
#define casin_f _n(casin_f)
#define casin_d _n(casin_d)
#define casin_l _n(casin_l)
#define cacos_f _n(cacos_f)
#define cacos_d _n(cacos_d)
#define cacos_l _n(cacos_l)
#define catan_f _n(catan_f)
#define catan_d _n(catan_d)
#define catan_l _n(catan_l)
#define casinh_f _n(casinh_f)
#define casinh_d _n(casinh_d)
#define casinh_l _n(casinh_l)
#define cacosh_f _n(cacosh_f)
#define cacosh_d _n(cacosh_d)
#define cacosh_l _n(cacosh_l)
#define catanh_f _n(catanh_f)
#define catanh_d _n(catanh_d)
#define catanh_l _n(catanh_l)
#define cabs_f _n(cabs_f)
#define cabs_d _n(cabs_d)
#define cabs_l _n(cabs_l)

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

