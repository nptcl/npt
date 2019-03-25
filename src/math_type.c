#include "bignum.h"
#include "build.h"
#include "cmpl.h"
#include "condition.h"
#include "math_type.h"
#include "object.h"
#include "ratio.h"
#include "rational.h"
#include "real_float.h"

/*
 *  getmathtype
 */
enum MathType getmathtype_float(struct mathtype_struct *ptr, addr pos)
{
	enum MathType type;

	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			type = MathType_single;
			ptr->v.s = RefSingleFloat(pos);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			type = MathType_double;
			ptr->v.d = RefDoubleFloat(pos);
			break;

		case LISPTYPE_LONG_FLOAT:
			type = MathType_long;
			ptr->v.l = RefLongFloat(pos);
			break;

		case LISPTYPE_COMPLEX:
			type = MathType_complex;
			break;

		case LISPTYPE_FIXNUM:
			type = MathType_single;
			ptr->v.s = single_float_fixnum(pos);
			break;

		case LISPTYPE_BIGNUM:
			type = MathType_single;
			ptr->v.s = single_float_bignum(pos);
			break;

		case LISPTYPE_RATIO:
			type = MathType_single;
			ptr->v.s = single_float_ratio(pos);
			break;

		default:
			type = MathType_error;
			break;
	}
	ptr->type = type;

	return type;
}


/*
 *  getmathreal2
 */
static enum MathType getmathreal2_type_value(addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			return MathType_single;

		case LISPTYPE_DOUBLE_FLOAT:
			return MathType_double;

		case LISPTYPE_LONG_FLOAT:
			return MathType_long;

		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
		case LISPTYPE_RATIO:
			return MathType_rational;

		case LISPTYPE_COMPLEX:
		default:
			return MathType_error;
	}
}

static enum MathType getmathreal2_type_single(addr y)
{
	enum MathType type;

	type = getmathreal2_type_value(y);
	switch (type) {
		case MathType_single:
		case MathType_double:
		case MathType_long:
			return type;

		case MathType_rational:
			return MathType_single;

		case MathType_complex:
		case MathType_error:
		default:
			TypeError(y, REAL);
			return MathType_error;
	}
}

static enum MathType getmathreal2_type_double(addr y)
{
	switch (getmathreal2_type_value(y)) {
		case MathType_single:
		case MathType_double:
		case MathType_rational:
			return MathType_double;

		case MathType_long:
			return MathType_long;

		case MathType_complex:
		case MathType_error:
		default:
			TypeError(y, REAL);
			return MathType_error;
	}
}

static enum MathType getmathreal2_type_long(addr y)
{
	switch (getmathreal2_type_value(y)) {
		case MathType_single:
		case MathType_double:
		case MathType_long:
		case MathType_rational:
			return MathType_long;

		case MathType_complex:
		case MathType_error:
		default:
			TypeError(y, REAL);
			return MathType_error;
	}
}

static enum MathType getmathreal2_type_rational(addr y)
{
	enum MathType type;

	type = getmathreal2_type_value(y);
	switch (type) {
		case MathType_single:
		case MathType_double:
		case MathType_long:
		case MathType_rational:
			return type;

		case MathType_complex:
		case MathType_error:
		default:
			TypeError(y, REAL);
			return MathType_error;
	}
}

static enum MathType getmathreal2_type(addr x, addr y)
{
	switch (getmathreal2_type_value(x)) {
		case MathType_single:
			return getmathreal2_type_single(y);

		case MathType_double:
			return getmathreal2_type_double(y);

		case MathType_long:
			return getmathreal2_type_long(y);

		case MathType_rational:
			return getmathreal2_type_rational(y);

		case MathType_complex:
		case MathType_error:
		default:
			TypeError(x, REAL);
			return MathType_error;
	}
}

static void getmathreal2_single1(addr pos, single_float *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			*ret = cast_ss_value(pos);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = cast_ds_value(pos);
			break;

		case LISPTYPE_LONG_FLOAT:
			*ret = cast_ls_value(pos);
			break;

		case LISPTYPE_FIXNUM:
			*ret = single_float_fixnum(pos);
			break;

		case LISPTYPE_BIGNUM:
			*ret = single_float_bignum(pos);
			break;

		case LISPTYPE_RATIO:
			*ret = single_float_ratio(pos);
			break;

		default:
			TypeError(pos, REAL);
			*ret = 0;
			return;
	}
}

static void getmathreal2_double1(addr pos, double_float *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			*ret = cast_sd_value(pos);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = cast_dd_value(pos);
			break;

		case LISPTYPE_LONG_FLOAT:
			*ret = cast_ld_value(pos);
			break;

		case LISPTYPE_FIXNUM:
			*ret = double_float_fixnum(pos);
			break;

		case LISPTYPE_BIGNUM:
			*ret = double_float_bignum(pos);
			break;

		case LISPTYPE_RATIO:
			*ret = double_float_ratio(pos);
			break;

		default:
			TypeError(pos, REAL);
			*ret = 0;
			return;
	}
}

static void getmathreal2_long1(addr pos, long_float *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			*ret = cast_sl_value(pos);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = cast_dl_value(pos);
			break;

		case LISPTYPE_LONG_FLOAT:
			*ret = cast_ll_value(pos);
			break;

		case LISPTYPE_FIXNUM:
			*ret = long_float_fixnum(pos);
			break;

		case LISPTYPE_BIGNUM:
			*ret = long_float_bignum(pos);
			break;

		case LISPTYPE_RATIO:
			*ret = long_float_ratio(pos);
			break;

		default:
			TypeError(pos, REAL);
			*ret = 0;
			return;
	}
}

static void getmathreal2_single(struct mathreal2_struct *ptr, addr x, addr y)
{
	single_float value;

	getmathreal2_single1(x, &value);
	ptr->v.s.a = value;
	getmathreal2_single1(y, &value);
	ptr->v.s.b = value;
}

static void getmathreal2_double(struct mathreal2_struct *ptr, addr x, addr y)
{
	double_float value;

	getmathreal2_double1(x, &value);
	ptr->v.d.a = value;
	getmathreal2_double1(y, &value);
	ptr->v.d.b = value;
}

static void getmathreal2_long(struct mathreal2_struct *ptr, addr x, addr y)
{
	long_float value;

	getmathreal2_long1(x, &value);
	ptr->v.l.a = value;
	getmathreal2_long1(y, &value);
	ptr->v.l.b = value;
}

enum MathType getmathreal2_float(struct mathreal2_struct *ptr, addr x, addr y)
{
	enum MathType type;

	type = getmathreal2_type(x, y);
	switch (type) {
		case MathType_single:
			getmathreal2_single(ptr, x, y);
			break;

		case MathType_double:
			getmathreal2_double(ptr, x, y);
			break;

		case MathType_long:
			getmathreal2_long(ptr, x, y);
			break;

		case MathType_rational:
			getmathreal2_single(ptr, x, y);
			type = MathType_single;
			break;

		case MathType_complex:
		case MathType_error:
		default:
			fmte("type error", NULL);
			break;
	}
	ptr->type = type;

	return type;
}

static void getmathreal2_rational(struct mathreal2_struct *ptr, addr x, addr y)
{
	ptr->v.a.x = x;
	ptr->v.a.y = y;
}

enum MathType getmathreal2_addr(struct mathreal2_struct *ptr, addr x, addr y)
{
	enum MathType type;

	type = getmathreal2_type(x, y);
	switch (type) {
		case MathType_single:
			getmathreal2_single(ptr, x, y);
			break;

		case MathType_double:
			getmathreal2_double(ptr, x, y);
			break;

		case MathType_long:
			getmathreal2_long(ptr, x, y);
			break;

		case MathType_rational:
			getmathreal2_rational(ptr, x, y);
			break;

		case MathType_complex:
		case MathType_error:
		default:
			fmte("type error", NULL);
			break;
	}
	ptr->type = type;

	return type;
}


/*
 *  getmathcomplex1
 */
static enum MathType getmathcomplex_type_complex(addr pos)
{
	switch (GetTypeComplex(pos)) {
		case ComplexType_single:
			return MathType_single;

		case ComplexType_double:
			return MathType_double;

		case ComplexType_long:
			return MathType_long;

		case ComplexType_rational:
			return MathType_rational;

		case ComplexType_error:
		default:
			return MathType_error;
	}
}

static enum MathType getmathcomplex1_log_type(addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			return MathType_single;

		case LISPTYPE_DOUBLE_FLOAT:
			return MathType_double;

		case LISPTYPE_LONG_FLOAT:
			return MathType_long;

		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
		case LISPTYPE_RATIO:
			return MathType_rational;

		case LISPTYPE_COMPLEX:
			return MathType_complex;

		default:
			return MathType_error;
	}
}

static enum MathType getmathcomplex1_inverse_type(addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			return MathType_single;

		case LISPTYPE_DOUBLE_FLOAT:
			return MathType_double;

		case LISPTYPE_LONG_FLOAT:
			return MathType_long;

		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
		case LISPTYPE_RATIO:
			return MathType_rational;

		case LISPTYPE_COMPLEX:
			return getmathcomplex_type_complex(pos);

		default:
			return MathType_error;
	}
}

enum MathType getmathcomplex1_complex(struct mathreal2_struct *ptr, addr pos)
{
	switch (GetTypeComplex(pos)) {
		case ComplexType_single:
			ptr->type = MathType_single;
			single_float_complex(pos, &(ptr->v.s.a), &(ptr->v.s.b));
			break;

		case ComplexType_double:
			ptr->type = MathType_double;
			double_float_complex(pos, &(ptr->v.d.a), &(ptr->v.d.b));
			break;

		case ComplexType_long:
			ptr->type = MathType_long;
			long_float_complex(pos, &(ptr->v.l.a), &(ptr->v.l.b));
			break;

		case ComplexType_rational:
			ptr->type = MathType_single;
			single_float_complex(pos, &(ptr->v.s.a), &(ptr->v.s.b));
			break;

		case ComplexType_error:
		default:
			TypeError(pos, COMPLEX);
			return MathType_error;
	}

	return ptr->type;
}

enum MathType getmathcomplex1_log(struct mathreal2_struct *ptr, addr pos)
{
	enum MathType type;

	type = getmathcomplex1_log_type(pos);
	switch (type) {
		case MathType_single:
			ptr->v.s.a = cast_ss_value(pos);
			ptr->v.s.b = 0.0f;
			break;

		case MathType_double:
			ptr->v.d.a = cast_dd_value(pos);
			ptr->v.d.b = 0.0;
			break;

		case MathType_long:
			ptr->v.l.a = cast_ll_value(pos);
			ptr->v.l.b = 0.0L;
			break;

		case MathType_rational:
			ptr->v.s.a = single_float_rational(pos);
			ptr->v.s.b = 0.0f;
			type = MathType_single;
			break;

		case MathType_complex:
			break;

		case MathType_error:
		default:
			fmte("type error", NULL);
			break;
	}
	ptr->type = type;

	return type;
}

enum MathType getmathcomplex1_inverse(struct mathreal2_struct *ptr, addr pos)
{
	enum MathType type;

	type = getmathcomplex1_inverse_type(pos);
	switch (type) {
		case MathType_single:
			ptr->v.s.a = cast_ss_value(pos);
			ptr->v.s.b = 0.0f;
			break;

		case MathType_double:
			ptr->v.d.a = cast_dd_value(pos);
			ptr->v.d.b = 0.0;
			break;

		case MathType_long:
			ptr->v.l.a = cast_ll_value(pos);
			ptr->v.l.b = 0.0L;
			break;

		case MathType_rational:
			ptr->v.a.x = pos;
			break;

		case MathType_complex:
			type = getmathcomplex1_complex(ptr, pos);
			break;

		case MathType_error:
		default:
			fmte("type error", NULL);
			break;
	}
	ptr->type = type;

	return type;
}

enum MathType getmathcomplex1_sqrt(struct mathreal2_struct *ptr, addr pos)
{
	enum MathType type;

	type = getmathcomplex1_log_type(pos);
	switch (type) {
		case MathType_single:
			ptr->v.s.a = cast_ss_value(pos);
			ptr->v.s.b = 0.0f;
			break;

		case MathType_double:
			ptr->v.d.a = cast_dd_value(pos);
			ptr->v.d.b = 0.0;
			break;

		case MathType_long:
			ptr->v.l.a = cast_ll_value(pos);
			ptr->v.l.b = 0.0L;
			break;

		case MathType_rational:
			ptr->v.s.a = single_float_rational(pos);
			ptr->v.s.b = 0.0f;
			type = MathType_single;
			break;

		case MathType_complex:
			type = getmathcomplex1_complex(ptr, pos);
			break;

		case MathType_error:
		default:
			fmte("type error", NULL);
			break;
	}
	ptr->type = type;

	return type;
}


/*
 *  getmathcomplex2
 */
static enum MathType getmathcomplex2_type_value(addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			return MathType_single;

		case LISPTYPE_DOUBLE_FLOAT:
			return MathType_double;

		case LISPTYPE_LONG_FLOAT:
			return MathType_long;

		case LISPTYPE_COMPLEX:
			return getmathcomplex_type_complex(pos);

		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
		case LISPTYPE_RATIO:
			return MathType_rational;

		default:
			return MathType_error;
	}
}

static enum MathType getmathcomplex2_type_single(addr y)
{
	enum MathType type;

	type = getmathcomplex2_type_value(y);
	switch (type) {
		case MathType_single:
		case MathType_double:
		case MathType_long:
			return type;

		case MathType_rational:
			return MathType_single;

		case MathType_complex:
		case MathType_error:
		default:
			TypeError(y, NUMBER);
			return MathType_error;
	}
}

static enum MathType getmathcomplex2_type_double(addr y)
{
	switch (getmathcomplex2_type_value(y)) {
		case MathType_single:
		case MathType_double:
		case MathType_rational:
			return MathType_double;

		case MathType_long:
			return MathType_long;

		case MathType_complex:
		case MathType_error:
		default:
			TypeError(y, NUMBER);
			return MathType_error;
	}
}

static enum MathType getmathcomplex2_type_long(addr y)
{
	switch (getmathcomplex2_type_value(y)) {
		case MathType_single:
		case MathType_double:
		case MathType_long:
		case MathType_rational:
			return MathType_long;

		case MathType_complex:
		case MathType_error:
		default:
			TypeError(y, NUMBER);
			return MathType_error;
	}
}

static enum MathType getmathcomplex2_type_rational(addr y)
{
	enum MathType type;

	type = getmathcomplex2_type_value(y);
	switch (type) {
		case MathType_single:
		case MathType_double:
		case MathType_long:
		case MathType_rational:
			return type;

		case MathType_complex:
		case MathType_error:
		default:
			TypeError(y, NUMBER);
			return MathType_error;
	}
}

static enum MathType getmathcomplex2_type(addr x, addr y)
{
	switch (getmathcomplex2_type_value(x)) {
		case MathType_single:
			return getmathcomplex2_type_single(y);

		case MathType_double:
			return getmathcomplex2_type_double(y);

		case MathType_long:
			return getmathcomplex2_type_long(y);

		case MathType_rational:
			return getmathcomplex2_type_rational(y);

		case MathType_complex:
		case MathType_error:
		default:
			TypeError(x, NUMBER);
			return MathType_error;
	}
}

static void getmathcomplex2_single1(addr pos, single_float *re, single_float *im)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			*re = cast_ss_value(pos);
			*im = 0.0f;
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*re = cast_ds_value(pos);
			*im = 0.0f;
			break;

		case LISPTYPE_LONG_FLOAT:
			*re = cast_ls_value(pos);
			*im = 0.0f;
			break;

		case LISPTYPE_COMPLEX:
			single_float_complex(pos, re, im);
			break;

		case LISPTYPE_FIXNUM:
			*re = single_float_fixnum(pos);
			*im = 0.0f;
			break;

		case LISPTYPE_BIGNUM:
			*re = single_float_bignum(pos);
			*im = 0.0f;
			break;

		case LISPTYPE_RATIO:
			*re = single_float_ratio(pos);
			*im = 0.0f;
			break;

		default:
			TypeError(pos, NUMBER);
			return;
	}
}

static void getmathcomplex2_double1(addr pos, double_float *re, double_float *im)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			*re = cast_sd_value(pos);
			*im = 0.0;
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*re = cast_dd_value(pos);
			*im = 0.0;
			break;

		case LISPTYPE_LONG_FLOAT:
			*re = cast_ld_value(pos);
			*im = 0.0;
			break;

		case LISPTYPE_COMPLEX:
			double_float_complex(pos, re, im);
			break;

		case LISPTYPE_FIXNUM:
			*re = double_float_fixnum(pos);
			*im = 0.0;
			break;

		case LISPTYPE_BIGNUM:
			*re = double_float_bignum(pos);
			*im = 0.0;
			break;

		case LISPTYPE_RATIO:
			*re = double_float_ratio(pos);
			*im = 0.0;
			break;

		default:
			TypeError(pos, NUMBER);
			return;
	}
}

static void getmathcomplex2_long1(addr pos, long_float *re, long_float *im)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			*re = cast_sl_value(pos);
			*im = 0.0L;
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*re = cast_dl_value(pos);
			*im = 0.0L;
			break;

		case LISPTYPE_LONG_FLOAT:
			*re = cast_ll_value(pos);
			*im = 0.0L;
			break;

		case LISPTYPE_COMPLEX:
			long_float_complex(pos, re, im);
			break;

		case LISPTYPE_FIXNUM:
			*re = long_float_fixnum(pos);
			*im = 0.0L;
			break;

		case LISPTYPE_BIGNUM:
			*re = long_float_bignum(pos);
			*im = 0.0L;
			break;

		case LISPTYPE_RATIO:
			*re = long_float_ratio(pos);
			*im = 0.0L;
			break;

		default:
			TypeError(pos, NUMBER);
			return;
	}
}

static void getmathcomplex2_single(struct mathcomplex2_struct *ptr, addr x, addr y)
{
	single_float real, imag;

	getmathcomplex2_single1(x, &real, &imag);
	ptr->v.s.a = real;
	ptr->v.s.b = imag;
	getmathcomplex2_single1(y, &real, &imag);
	ptr->v.s.c = real;
	ptr->v.s.d = imag;
}

static void getmathcomplex2_double(struct mathcomplex2_struct *ptr, addr x, addr y)
{
	double_float real, imag;

	getmathcomplex2_double1(x, &real, &imag);
	ptr->v.d.a = real;
	ptr->v.d.b = imag;
	getmathcomplex2_double1(y, &real, &imag);
	ptr->v.d.c = real;
	ptr->v.d.d = imag;
}

static void getmathcomplex2_long(struct mathcomplex2_struct *ptr, addr x, addr y)
{
	long_float real, imag;

	getmathcomplex2_long1(x, &real, &imag);
	ptr->v.l.a = real;
	ptr->v.l.b = imag;
	getmathcomplex2_long1(y, &real, &imag);
	ptr->v.l.c = real;
	ptr->v.l.d = imag;
}

enum MathType getmathcomplex2_float(struct mathcomplex2_struct *ptr, addr x, addr y)
{
	enum MathType type;

	type = getmathcomplex2_type(x, y);
	switch (type) {
		case MathType_single:
			getmathcomplex2_single(ptr, x, y);
			break;

		case MathType_double:
			getmathcomplex2_double(ptr, x, y);
			break;

		case MathType_long:
			getmathcomplex2_long(ptr, x, y);
			break;

		case MathType_rational:
			getmathcomplex2_single(ptr, x, y);
			type = MathType_single;
			break;

		case MathType_complex:
		case MathType_error:
		default:
			fmte("type error", NULL);
			break;
	}
	ptr->type = type;

	return type;
}


/*
 *  getmathcomplex2_rational
 */
static void getmathcomplex2_rational(struct mathcomplex2_struct *ptr, addr x, addr y)
{
	ptr->v.a.x = x;
	ptr->v.a.y = y;
}

enum MathType getmathcomplex2_addr(struct mathcomplex2_struct *ptr, addr x, addr y)
{
	enum MathType type;

	type = getmathcomplex2_type(x, y);
	switch (type) {
		case MathType_single:
			getmathcomplex2_single(ptr, x, y);
			break;

		case MathType_double:
			getmathcomplex2_double(ptr, x, y);
			break;

		case MathType_long:
			getmathcomplex2_long(ptr, x, y);
			break;

		case MathType_rational:
			getmathcomplex2_rational(ptr, x, y);
			break;

		case MathType_complex:
		case MathType_error:
		default:
			fmte("type error", NULL);
			break;
	}
	ptr->type = type;

	return type;
}

