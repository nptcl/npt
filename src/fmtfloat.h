/*
 *  Common-Lisp Floating-point fomatter
 *      ~F  Fixed floating-point
 *      ~E  Exponential floating-point
 *      ~G  General floating-point
 */
#ifndef __FMTFLOAT_HEADER__
#define __FMTFLOAT_HEADER__

#include <stddef.h>
#include "typedef.h"

#define FMTFLOAT_ROUND_PARAMETER -1
/* float (32bit) fraction: 23+1 bit */
#define FMTFLOAT_ROUND_SINGLE   (8 + FMTFLOAT_ROUND_PARAMETER)
/* double (64bit) fraction: 52+1 bit */
#define FMTFLOAT_ROUND_DOUBLE   (16 + FMTFLOAT_ROUND_PARAMETER)
/* long double (Intel 80bit) fraction: 63+0 bit */
#if defined(LISP_FLOAT_LONG_80)
#define FMTFLOAT_ROUND_LONG     (19 + FMTFLOAT_ROUND_PARAMETER)
/* long double (IEEE-754 binary128) fraction: 112+1 bit */
#elif defined(LISP_FLOAT_LONG_128)
#define FMTFLOAT_ROUND_LONG     (35 + FMTFLOAT_ROUND_PARAMETER)
/* long double (Visual C++ 64bit) */
#else
#define FMTFLOAT_ROUND_LONG     FMTFLOAT_ROUND_DOUBLE
#endif


/*****************************************************************************
 *  fmtdecimal
 *****************************************************************************/
/*
 *    32bit: sX.[fraction]es[exponent] 1+1+1+ 8+1+1+2 + null  = 16
 *    64bit: sX.[fraction]es[exponent] 1+1+1+16+1+1+3 + null  = 25
 *   128bit: sX.[fraction]es[exponent] 1+1+1+35+1+1+4 + null  = 45
 *     -> 64byte
 */
#define FMTDECIMAL_FRACTION      64
#define FMTDECIMAL_EXPONENT      8

struct fmtdecimal_struct {
	unsigned sign : 1;    /* minus sign */
	unsigned size;
	fixnum exponent;
	byte fraction[FMTDECIMAL_FRACTION];
};

typedef struct fmtdecimal_struct *fmtdecimal;

int fmtdecimal_zerop(fmtdecimal str);
int fmtdecimal_round(fmtdecimal str, unsigned i);
int fmtdecimal_single_float(fmtdecimal str, single_float value, int round);
int fmtdecimal_double_float(fmtdecimal str, double_float value, int round);
int fmtdecimal_long_float(fmtdecimal str, long_float value, int round);
void fmtdecimal_dump(FILE *file, fmtdecimal str);


/*****************************************************************************
 *  fmtfloat
 *****************************************************************************/
struct fmtfloat_struct {
	unsigned sign : 1;
	unsigned signbit : 1;
	unsigned sign_exponent : 1;
	unsigned wp : 1;
	unsigned dp : 1;
	unsigned ep : 1;
	unsigned np : 1;
	unsigned overflowp : 1;
	unsigned markerp : 1;
	fixnum k, k_bias;
	unicode overflow, pad, marker;
	size_t w, d, e, n;
	union fmtfloat_union {
		single_float value_single;
		double_float value_double;
		long_float value_long;
	} u;
};

typedef struct fmtfloat_struct *fmtfloat;

void fmtfloat_fixed(addr stream, fmtfloat fmt, fmtdecimal dec);
void fmtfloat_exponent(addr stream, fmtfloat fmt, fmtdecimal dec);
void fmtfloat_general(addr stream, fmtfloat fmt, fmtdecimal dec);
void fmtfloat_dollars(addr stream, fmtfloat fmt, fmtdecimal dec);


/*****************************************************************************
 *  princ / prin1
 *****************************************************************************/
int fmtfloat_princ_single_float(addr stream,
		single_float value, int markerp, unicode marker);
int fmtfloat_princ_double_float(addr stream,
		double_float value, int markerp, unicode marker);
int fmtfloat_princ_long_float(addr stream,
		long_float value, int markerp, unicode marker);

#endif

