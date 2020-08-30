/*
 *  Common-Lisp Floating-point fomatter
 *      ~F  Fixed floating-point
 *      ~E  Exponential floating-point
 *      ~G  General floating-point
 */
#ifndef __FMTFLOAT_HEADER__
#define __FMTFLOAT_HEADER__

#define fmtfloat_write_char_ _n(fmtfloat_write_char_)
#define fmtfloat_print_ascii_ _n(fmtfloat_print_ascii_)
#define fmtdecimal_zerop _n(fmtdecimal_zerop)
#define fmtdecimal_round _n(fmtdecimal_round)
#define fmtdecimal_single_float _n(fmtdecimal_single_float)
#define fmtdecimal_double_float _n(fmtdecimal_double_float)
#define fmtdecimal_long_float _n(fmtdecimal_long_float)
#define fmtdecimal_dump _n(fmtdecimal_dump)
#define fmtfloat_fixed_ _n(fmtfloat_fixed_)
#define fmtfloat_exponent_ _n(fmtfloat_exponent_)
#define fmtfloat_general_ _n(fmtfloat_general_)
#define fmtfloat_monetary_ _n(fmtfloat_monetary_)
#define fmtfloat_princ_single_float_ _n(fmtfloat_princ_single_float_)
#define fmtfloat_princ_double_float_ _n(fmtfloat_princ_double_float_)
#define fmtfloat_princ_long_float_ _n(fmtfloat_princ_long_float_)

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

_g int fmtdecimal_zerop(fmtdecimal str);
_g int fmtdecimal_round(fmtdecimal str, unsigned i);
_g int fmtdecimal_single_float(fmtdecimal str, single_float value, int round);
_g int fmtdecimal_double_float(fmtdecimal str, double_float value, int round);
_g int fmtdecimal_long_float(fmtdecimal str, long_float value, int round);
_g void fmtdecimal_dump(FILE *file, fmtdecimal str);


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

_g int fmtfloat_fixed_(addr stream, fmtfloat fmt, fmtdecimal dec);
_g int fmtfloat_exponent_(addr stream, fmtfloat fmt, fmtdecimal dec);
_g int fmtfloat_general_(addr stream, fmtfloat fmt, fmtdecimal dec);
_g int fmtfloat_monetary_(addr stream, fmtfloat fmt, fmtdecimal dec);


/*****************************************************************************
 *  princ / prin1
 *****************************************************************************/
_g int fmtfloat_princ_single_float_(addr stream,
		single_float value, int markerp, unicode marker, int *ret);
_g int fmtfloat_princ_double_float_(addr stream,
		double_float value, int markerp, unicode marker, int *ret);
_g int fmtfloat_princ_long_float_(addr stream,
		long_float value, int markerp, unicode marker, int *ret);

/* debug */
#define fmtfloat_fixed_float_ _n(fmtfloat_fixed_float_)
#define fmtfloat_fixed_double_ _n(fmtfloat_fixed_double_)
#define fmtfloat_exponent_float_ _n(fmtfloat_exponent_float_)
#define fmtfloat_exponent_double_ _n(fmtfloat_exponent_double_)

_g int fmtfloat_fixed_float_(addr stream, single_float value,
		int sign,
		fixnum w, fixnum d, fixnum k,
		unicode overflow, unicode pad);
_g int fmtfloat_fixed_double_(addr stream, double_float value,
		int sign,
		fixnum w, fixnum d, fixnum k,
		unicode overflow, unicode pad);
_g int fmtfloat_exponent_float_(addr stream, single_float value,
		int sign, fixnum w,
		fixnum d, fixnum e, fixnum k,
		unicode overflow, unicode pad, unicode exponent);
_g int fmtfloat_exponent_double_(addr stream, double_float value,
		int sign, fixnum w,
		fixnum d, fixnum e, fixnum k,
		unicode overflow, unicode pad, unicode exponent);

#endif

