#ifndef __CMPL_MATH_HEADER__
#define __CMPL_MATH_HEADER__

#include "typedef.h"

_g void expt_f(single_float a, single_float b, single_float c, single_float d,
		single_float *re, single_float *im);
_g void expt_d(double_float a, double_float b, double_float c, double_float d,
		double_float *re, double_float *im);
_g void expt_l(long_float a, long_float b, long_float c, long_float d,
		long_float *re, long_float *im);
_g int clogb_f_(single_float a, single_float b, single_float c, single_float d,
		single_float *re, single_float *im);
_g int clogb_d_(double_float a, double_float b, double_float c, double_float d,
		double_float *re, double_float *im);
_g int clogb_l_(long_float a, long_float b, long_float c, long_float d,
		long_float *re, long_float *im);

_g void csqrt_f(single_float real, single_float imag, single_float *re, single_float *im);
_g void csqrt_d(double_float real, double_float imag, double_float *re, double_float *im);
_g void csqrt_l(long_float real, long_float imag, long_float *re, long_float *im);

#endif

