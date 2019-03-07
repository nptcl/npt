#ifndef __MATH_COMPLEX_HEADER__
#define __MATH_COMPLEX_HEADER__

#include "typedef.h"

void cexp_f(single_float real, single_float imag, single_float *re, single_float *im);
void cexp_d(double_float real, double_float imag, double_float *re, double_float *im);
void cexp_l(long_float real, long_float imag, long_float *re, long_float *im);
void clog_f(single_float real, single_float imag, single_float *re, single_float *im);
void clog_d(double_float real, double_float imag, double_float *re, double_float *im);
void clog_l(long_float real, long_float imag, long_float *re, long_float *im);

void csin_f(single_float real, single_float imag, single_float *re, single_float *im);
void csin_d(double_float real, double_float imag, double_float *re, double_float *im);
void csin_l(long_float real, long_float imag, long_float *re, long_float *im);
void ccos_f(single_float real, single_float imag, single_float *re, single_float *im);
void ccos_d(double_float real, double_float imag, double_float *re, double_float *im);
void ccos_l(long_float real, long_float imag, long_float *re, long_float *im);
void ctan_f(single_float real, single_float imag, single_float *re, single_float *im);
void ctan_d(double_float real, double_float imag, double_float *re, double_float *im);
void ctan_l(long_float real, long_float imag, long_float *re, long_float *im);

void csinh_f(single_float real, single_float imag, single_float *re, single_float *im);
void csinh_d(double_float real, double_float imag, double_float *re, double_float *im);
void csinh_l(long_float real, long_float imag, long_float *re, long_float *im);
void ccosh_f(single_float real, single_float imag, single_float *re, single_float *im);
void ccosh_d(double_float real, double_float imag, double_float *re, double_float *im);
void ccosh_l(long_float real, long_float imag, long_float *re, long_float *im);
void ctanh_f(single_float real, single_float imag, single_float *re, single_float *im);
void ctanh_d(double_float real, double_float imag, double_float *re, double_float *im);
void ctanh_l(long_float real, long_float imag, long_float *re, long_float *im);

void casin_f(single_float real, single_float imag, single_float *re, single_float *im);
void casin_d(double_float real, double_float imag, double_float *re, double_float *im);
void casin_l(long_float real, long_float imag, long_float *re, long_float *im);
void cacos_f(single_float real, single_float imag, single_float *re, single_float *im);
void cacos_d(double_float real, double_float imag, double_float *re, double_float *im);
void cacos_l(long_float real, long_float imag, long_float *re, long_float *im);
void catan_f(single_float real, single_float imag, single_float *re, single_float *im);
void catan_d(double_float real, double_float imag, double_float *re, double_float *im);
void catan_l(long_float real, long_float imag, long_float *re, long_float *im);

void casinh_f(single_float real, single_float imag, single_float *re, single_float *im);
void casinh_d(double_float real, double_float imag, double_float *re, double_float *im);
void casinh_l(long_float real, long_float imag, long_float *re, long_float *im);
void cacosh_f(single_float real, single_float imag, single_float *re, single_float *im);
void cacosh_d(double_float real, double_float imag, double_float *re, double_float *im);
void cacosh_l(long_float real, long_float imag, long_float *re, long_float *im);
void catanh_f(single_float real, single_float imag, single_float *re, single_float *im);
void catanh_d(double_float real, double_float imag, double_float *re, double_float *im);
void catanh_l(long_float real, long_float imag, long_float *re, long_float *im);

void cabs_f(single_float real, single_float imag, single_float *ret);
void cabs_d(double_float real, double_float imag, double_float *ret);
void cabs_l(long_float real, long_float imag, long_float *ret);

void expt_f(single_float a, single_float b, single_float c, single_float d,
		single_float *re, single_float *im);
void expt_d(double_float a, double_float b, double_float c, double_float d,
		double_float *re, double_float *im);
void expt_l(long_float a, long_float b, long_float c, long_float d,
		long_float *re, long_float *im);
void clogb_f(single_float a, single_float b, single_float c, single_float d,
		single_float *re, single_float *im);
void clogb_d(double_float a, double_float b, double_float c, double_float d,
		double_float *re, double_float *im);
void clogb_l(long_float a, long_float b, long_float c, long_float d,
		long_float *re, long_float *im);

void csqrt_f(single_float real, single_float imag, single_float *re, single_float *im);
void csqrt_d(double_float real, double_float imag, double_float *re, double_float *im);
void csqrt_l(long_float real, long_float imag, long_float *re, long_float *im);

#endif

