#ifndef __MATH_TYPE_HEADER__
#define __MATH_TYPE_HEADER__

#include "typedef.h"

enum MathType {
	MathType_single,
	MathType_double,
	MathType_long,
	MathType_complex,
	MathType_rational,
	MathType_error
};

struct mathtype_struct {
	enum MathType type;
	union {
		single_float s;
		double_float d;
		long_float l;
	} v;
};

struct mathreal2_single {
	single_float a, b;
};
struct mathreal2_double {
	double_float a, b;
};
struct mathreal2_long {
	long_float a, b;
};
struct mathtype_addr {
	addr x, y;
};
struct mathreal2_struct {
	enum MathType type;
	union {
		struct mathreal2_single s;
		struct mathreal2_double d;
		struct mathreal2_long l;
		struct mathtype_addr a;
	} v;
};

struct mathcomplex2_single {
	single_float a, b, c, d;
};
struct mathcomplex2_double {
	double_float a, b, c, d;
};
struct mathcomplex2_long {
	long_float a, b, c, d;
};
struct mathcomplex2_struct {
	enum MathType type;
	union {
		struct mathcomplex2_single s;
		struct mathcomplex2_double d;
		struct mathcomplex2_long l;
		struct mathtype_addr a;
	} v;
};

_g void getmathtype_float(struct mathtype_struct *ptr,
		addr pos, enum MathType *ret);
_g int getmathreal2_float_(struct mathreal2_struct *ptr,
		addr x, addr y, enum MathType *ret);
_g int getmathreal2_addr_(struct mathreal2_struct *ptr,
		addr x, addr y, enum MathType *ret);
_g int getmathcomplex1_log_(struct mathreal2_struct *ptr,
		addr pos, enum MathType *ret);
_g int getmathcomplex1_inverse_(struct mathreal2_struct *ptr,
		addr pos, enum MathType *ret);
_g int getmathcomplex1_sqrt_(struct mathreal2_struct *ptr,
		addr pos, enum MathType *ret);
_g int getmathcomplex2_float_(struct mathcomplex2_struct *ptr,
		addr x, addr y, enum MathType *ret);
_g int getmathcomplex2_addr_(struct mathcomplex2_struct *ptr,
		addr x, addr y, enum MathType *ret);

#endif

