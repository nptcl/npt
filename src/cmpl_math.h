#ifndef __CMPL_MATH_HEADER__
#define __CMPL_MATH_HEADER__

#include "typedef.h"

#define expt_f _n(expt_f)
#define expt_d _n(expt_d)
#define expt_l _n(expt_l)
#define clogb_f_ _n(clogb_f_)
#define clogb_d_ _n(clogb_d_)
#define clogb_l_ _n(clogb_l_)
#define csqrt_f _n(csqrt_f)
#define csqrt_d _n(csqrt_d)
#define csqrt_l _n(csqrt_l)

void expt_f(single_float a, single_float b, single_float c, single_float d,
		single_float *re, single_float *im);
void expt_d(double_float a, double_float b, double_float c, double_float d,
		double_float *re, double_float *im);
void expt_l(long_float a, long_float b, long_float c, long_float d,
		long_float *re, long_float *im);
int clogb_f_(single_float a, single_float b, single_float c, single_float d,
		single_float *re, single_float *im);
int clogb_d_(double_float a, double_float b, double_float c, double_float d,
		double_float *re, double_float *im);
int clogb_l_(long_float a, long_float b, long_float c, long_float d,
		long_float *re, long_float *im);

void csqrt_f(single_float real, single_float imag, single_float *re, single_float *im);
void csqrt_d(double_float real, double_float imag, double_float *re, double_float *im);
void csqrt_l(long_float real, long_float imag, long_float *re, long_float *im);

#endif

