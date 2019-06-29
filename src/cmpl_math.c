#include <math.h>
#include "condition.h"
#include "cmpl.h"
#include "cmpl_arch.h"
#include "cmpl_math.h"
#include "define.h"

#ifdef __clang__
#define powl_define pow
#else
#define powl_define powl
#endif

/*
 *  expt
 */
_g void expt_f(single_float a, single_float b, single_float c, single_float d,
		single_float *re, single_float *im)
{
	single_float x, y;

	/* a**b = exp{b*log(a)} */
	if (0.0f <= a && b == 0.0f && d == 0.0f) {
		*re = powf(a, c);
		*im = 0.0f;
	}
	else {
		/* log(a) */
		clog_f(a, b, &a, &b);
		/* b*log(a) */
		x = a*c - b*d;
		y = b*c + a*d;
		/* exp{...} */
		cexp_f(x, y, re, im);
	}
}

_g void expt_d(double_float a, double_float b, double_float c, double_float d,
		double_float *re, double_float *im)
{
	double_float x, y;

	/* a**b = exp{b*log(a)} */
	if (0.0 <= a && b == 0.0 && d == 0.0) {
		*re = pow(a, c);
		*im = 0.0;
	}
	else {
		/* log(a) */
		clog_d(a, b, &a, &b);
		/* b*log(a) */
		x = a*c - b*d;
		y = b*c + a*d;
		/* exp{...} */
		cexp_d(x, y, re, im);
	}
}

_g void expt_l(long_float a, long_float b, long_float c, long_float d,
		long_float *re, long_float *im)
{
	long_float x, y;

	/* a**b = exp{b*log(a)} */
	if (0.0L <= a && b == 0.0L && d == 0.0L) {
		*re = powl_define(a, c);
		*im = 0.0L;
	}
	else {
		/* log(a) */
		clog_l(a, b, &a, &b);
		/* b*log(a) */
		x = a*c - b*d;
		y = b*c + a*d;
		/* exp{...} */
		cexp_l(x, y, re, im);
	}
}


/*
 *  clogb
 */
_g void clogb_f(single_float a, single_float b, single_float c, single_float d,
		single_float *re, single_float *im)
{
	addr real, imag;
	single_float denom;

	/* clogb(number, base) -> log(a+ib)/log(c+id) */
	if (a == 0.0f && b == 0.0f) {
		complex_single_heap(&real, a, b);
		complex_single_heap(&imag, c, d);
		division_by_zero_real2(CONSTANT_COMMON_LOG, real, imag);
		return;
	}
	if (c == 0.0f && d == 0.0f) {
		*re = *im = 0.0f;
		return;
	}
	/* log(a+ib) */
	clog_f(a, b, &a, &b);
	/* log(c+id) */
	clog_f(c, d, &c, &d);
	/* log(a+ib) / log(c+id) */
	denom = c*c + d*d;
	*re = (a*c + b*d) / denom;
	*im = (b*c - a*d) / denom;
}

_g void clogb_d(double_float a, double_float b, double_float c, double_float d,
		double_float *re, double_float *im)
{
	addr real, imag;
	double_float denom;

	/* clogb(number, base) -> log(a+ib)/log(c+id) */
	if (a == 0.0 && b == 0.0) {
		complex_double_heap(&real, a, b);
		complex_double_heap(&imag, c, d);
		division_by_zero_real2(CONSTANT_COMMON_LOG, real, imag);
		return;
	}
	if (c == 0.0 && d == 0.0) {
		*re = *im = 0.0;
		return;
	}
	/* log(a+ib) */
	clog_d(a, b, &a, &b);
	/* log(c+id) */
	clog_d(c, d, &c, &d);
	/* log(a+ib) / log(c+id) */
	denom = c*c + d*d;
	*re = (a*c + b*d) / denom;
	*im = (b*c - a*d) / denom;
}

_g void clogb_l(long_float a, long_float b, long_float c, long_float d,
		long_float *re, long_float *im)
{
	addr real, imag;
	long_float denom;

	/* clogb(number, base) -> log(a+ib)/log(c+id) */
	if (a == 0.0L && b == 0.0L) {
		complex_long_heap(&real, a, b);
		complex_long_heap(&imag, c, d);
		division_by_zero_real2(CONSTANT_COMMON_LOG, real, imag);
		return;
	}
	if (c == 0.0L && d == 0.0L) {
		*re = *im = 0.0L;
		return;
	}
	/* log(a+ib) */
	clog_l(a, b, &a, &b);
	/* log(c+id) */
	clog_l(c, d, &c, &d);
	/* log(a+ib) / log(c+id) */
	denom = c*c + d*d;
	*re = (a*c + b*d) / denom;
	*im = (b*c - a*d) / denom;
}


/*
 *  csqrt
 */
_g void csqrt_f(single_float real, single_float imag, single_float *re, single_float *im)
{
	expt_f(real, imag, 0.5f, 0.0f, re, im);
}

_g void csqrt_d(double_float real, double_float imag, double_float *re, double_float *im)
{
	expt_d(real, imag, 0.5, 0.0, re, im);
}

_g void csqrt_l(long_float real, long_float imag, long_float *re, long_float *im)
{
	expt_l(real, imag, 0.5L, 0.0L, re, im);
}

