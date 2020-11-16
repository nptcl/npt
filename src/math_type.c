#include "bignum.h"
#include "build.h"
#include "cmpl.h"
#include "condition.h"
#include "float_object.h"
#include "math_type.h"
#include "object.h"
#include "ratio.h"
#include "rational.h"

/*
 *  getmathtype
 */
int getmathtype_float_(struct mathtype_struct *ptr, addr pos, enum MathType *ret)
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
			Return(single_float_bignum_(pos, &(ptr->v.s)));
			break;

		case LISPTYPE_RATIO:
			type = MathType_single;
			Return(single_float_ratio_(pos, &(ptr->v.s)));
			break;

		default:
			type = MathType_error;
			break;
	}
	ptr->type = type;
	return Result(ret, type);
}


/*
 *  getmathreal2
 */
static void getmathreal2_type_value(addr pos, enum MathType *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			*ret = MathType_single;
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = MathType_double;
			break;

		case LISPTYPE_LONG_FLOAT:
			*ret = MathType_long;
			break;

		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
		case LISPTYPE_RATIO:
			*ret = MathType_rational;
			break;

		case LISPTYPE_COMPLEX:
		default:
			*ret = MathType_error;
			break;
	}
}

static int getmathreal2_type_single_(addr y, enum MathType *ret)
{
	enum MathType type;

	getmathreal2_type_value(y, &type);
	switch (type) {
		case MathType_single:
		case MathType_double:
		case MathType_long:
			return Result(ret, type);

		case MathType_rational:
			return Result(ret, MathType_single);

		case MathType_complex:
		case MathType_error:
		default:
			*ret = MathType_error;
			return TypeError_(y, REAL);
	}
}

static int getmathreal2_type_double_(addr y, enum MathType *ret)
{
	enum MathType type;

	getmathreal2_type_value(y, &type);
	switch (type) {
		case MathType_single:
		case MathType_double:
		case MathType_rational:
			return Result(ret, MathType_double);

		case MathType_long:
			return Result(ret, MathType_long);

		case MathType_complex:
		case MathType_error:
		default:
			*ret = MathType_error;
			return TypeError_(y, REAL);
	}
}

static int getmathreal2_type_long_(addr y, enum MathType *ret)
{
	enum MathType type;

	getmathreal2_type_value(y, &type);
	switch (type) {
		case MathType_single:
		case MathType_double:
		case MathType_long:
		case MathType_rational:
			return Result(ret, MathType_long);

		case MathType_complex:
		case MathType_error:
		default:
			*ret = MathType_error;
			return TypeError_(y, REAL);
	}
}

static int getmathreal2_type_rational_(addr y, enum MathType *ret)
{
	enum MathType type;

	getmathreal2_type_value(y, &type);
	switch (type) {
		case MathType_single:
		case MathType_double:
		case MathType_long:
		case MathType_rational:
			return Result(ret, type);

		case MathType_complex:
		case MathType_error:
		default:
			*ret = MathType_error;
			return TypeError_(y, REAL);
	}
}

static int getmathreal2_type_(addr x, addr y, enum MathType *ret)
{
	enum MathType type;

	getmathreal2_type_value(x, &type);
	switch (type) {
		case MathType_single:
			return getmathreal2_type_single_(y, ret);

		case MathType_double:
			return getmathreal2_type_double_(y, ret);

		case MathType_long:
			return getmathreal2_type_long_(y, ret);

		case MathType_rational:
			return getmathreal2_type_rational_(y, ret);

		case MathType_complex:
		case MathType_error:
		default:
			*ret = MathType_error;
			return TypeError_(x, REAL);
	}
}

static int getmathreal2_single1_(addr pos, single_float *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			return cast_ss_value_(pos, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return cast_ds_value_(pos, ret);

		case LISPTYPE_LONG_FLOAT:
			return cast_ls_value_(pos, ret);

		case LISPTYPE_FIXNUM:
			*ret = single_float_fixnum(pos);
			return 0;

		case LISPTYPE_BIGNUM:
			return single_float_bignum_(pos, ret);

		case LISPTYPE_RATIO:
			return single_float_ratio_(pos, ret);

		default:
			*ret = 0;
			return TypeError_(pos, REAL);
	}
}

static int getmathreal2_double1_(addr pos, double_float *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			return cast_sd_value_(pos, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return cast_dd_value_(pos, ret);

		case LISPTYPE_LONG_FLOAT:
			return cast_ld_value_(pos, ret);

		case LISPTYPE_FIXNUM:
			*ret = double_float_fixnum(pos);
			return 0;

		case LISPTYPE_BIGNUM:
			return double_float_bignum_(pos, ret);

		case LISPTYPE_RATIO:
			return double_float_ratio_(pos, ret);

		default:
			*ret = 0;
			return TypeError_(pos, REAL);
	}
}

static int getmathreal2_long1_(addr pos, long_float *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			return cast_sl_value_(pos, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return cast_dl_value_(pos, ret);

		case LISPTYPE_LONG_FLOAT:
			return cast_ll_value_(pos, ret);

		case LISPTYPE_FIXNUM:
			*ret = long_float_fixnum(pos);
			return 0;

		case LISPTYPE_BIGNUM:
			return long_float_bignum_(pos, ret);

		case LISPTYPE_RATIO:
			return long_float_ratio_(pos, ret);

		default:
			*ret = 0;
			return TypeError_(pos, REAL);
	}
}

static int getmathreal2_single_(struct mathreal2_struct *ptr, addr x, addr y)
{
	single_float value;

	Return(getmathreal2_single1_(x, &value));
	ptr->v.s.a = value;
	Return(getmathreal2_single1_(y, &value));
	ptr->v.s.b = value;

	return 0;
}

static int getmathreal2_double_(struct mathreal2_struct *ptr, addr x, addr y)
{
	double_float value;

	Return(getmathreal2_double1_(x, &value));
	ptr->v.d.a = value;
	Return(getmathreal2_double1_(y, &value));
	ptr->v.d.b = value;

	return 0;
}

static int getmathreal2_long_(struct mathreal2_struct *ptr, addr x, addr y)
{
	long_float value;

	Return(getmathreal2_long1_(x, &value));
	ptr->v.l.a = value;
	Return(getmathreal2_long1_(y, &value));
	ptr->v.l.b = value;

	return 0;
}

int getmathreal2_float_(struct mathreal2_struct *ptr,
		addr x, addr y, enum MathType *ret)
{
	enum MathType type;

	Return(getmathreal2_type_(x, y, &type));
	switch (type) {
		case MathType_single:
			Return(getmathreal2_single_(ptr, x, y));
			break;

		case MathType_double:
			Return(getmathreal2_double_(ptr, x, y));
			break;

		case MathType_long:
			Return(getmathreal2_long_(ptr, x, y));
			break;

		case MathType_rational:
			Return(getmathreal2_single_(ptr, x, y));
			type = MathType_single;
			break;

		case MathType_complex:
		case MathType_error:
		default:
			*ret = MathType_error;
			return fmte_("type error", NULL);
	}
	ptr->type = type;

	return Result(ret, type);
}

static void getmathreal2_rational(struct mathreal2_struct *ptr, addr x, addr y)
{
	ptr->v.a.x = x;
	ptr->v.a.y = y;
}

int getmathreal2_addr_(struct mathreal2_struct *ptr,
		addr x, addr y, enum MathType *ret)
{
	enum MathType type;

	Return(getmathreal2_type_(x, y, &type));
	switch (type) {
		case MathType_single:
			Return(getmathreal2_single_(ptr, x, y));
			break;

		case MathType_double:
			Return(getmathreal2_double_(ptr, x, y));
			break;

		case MathType_long:
			Return(getmathreal2_long_(ptr, x, y));
			break;

		case MathType_rational:
			getmathreal2_rational(ptr, x, y);
			break;

		case MathType_complex:
		case MathType_error:
		default:
			*ret = MathType_error;
			return fmte_("type error", NULL);
	}
	ptr->type = type;

	return Result(ret, type);
}


/*
 *  getmathcomplex1
 */
static void getmathcomplex_type_complex(addr pos, enum MathType *ret)
{
	switch (GetTypeComplex(pos)) {
		case ComplexType_single:
			*ret = MathType_single;
			break;

		case ComplexType_double:
			*ret = MathType_double;
			break;

		case ComplexType_long:
			*ret = MathType_long;
			break;

		case ComplexType_rational:
			*ret = MathType_rational;
			break;

		case ComplexType_error:
		default:
			*ret = MathType_error;
			break;
	}
}

static void getmathcomplex1_log_type(addr pos, enum MathType *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			*ret = MathType_single;
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = MathType_double;
			break;

		case LISPTYPE_LONG_FLOAT:
			*ret = MathType_long;
			break;

		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
		case LISPTYPE_RATIO:
			*ret = MathType_rational;
			break;

		case LISPTYPE_COMPLEX:
			*ret = MathType_complex;
			break;

		default:
			*ret = MathType_error;
			break;
	}
}

static void getmathcomplex1_inverse_type(addr pos, enum MathType *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			*ret = MathType_single;
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = MathType_double;
			break;

		case LISPTYPE_LONG_FLOAT:
			*ret = MathType_long;
			break;

		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
		case LISPTYPE_RATIO:
			*ret = MathType_rational;
			break;

		case LISPTYPE_COMPLEX:
			getmathcomplex_type_complex(pos, ret);
			break;

		default:
			*ret = MathType_error;
			break;
	}
}

static int getmathcomplex1_complex_(struct mathreal2_struct *ptr,
		addr pos, enum MathType *ret)
{
	switch (GetTypeComplex(pos)) {
		case ComplexType_single:
			ptr->type = MathType_single;
			Return(single_float_complex_(pos, &(ptr->v.s.a), &(ptr->v.s.b)));
			break;

		case ComplexType_double:
			ptr->type = MathType_double;
			Return(double_float_complex_(pos, &(ptr->v.d.a), &(ptr->v.d.b)));
			break;

		case ComplexType_long:
			ptr->type = MathType_long;
			Return(long_float_complex_(pos, &(ptr->v.l.a), &(ptr->v.l.b)));
			break;

		case ComplexType_rational:
			ptr->type = MathType_single;
			Return(single_float_complex_(pos, &(ptr->v.s.a), &(ptr->v.s.b)));
			break;

		case ComplexType_error:
		default:
			*ret = MathType_error;
			return TypeError_(pos, COMPLEX);
	}

	return Result(ret, ptr->type);
}

int getmathcomplex1_log_(struct mathreal2_struct *ptr,
		addr pos, enum MathType *ret)
{
	enum MathType type;

	getmathcomplex1_log_type(pos, &type);
	switch (type) {
		case MathType_single:
			Return(cast_ss_value_(pos, &(ptr->v.s.a)));
			ptr->v.s.b = 0.0f;
			break;

		case MathType_double:
			Return(cast_dd_value_(pos, &(ptr->v.d.a)));
			ptr->v.d.b = 0.0;
			break;

		case MathType_long:
			Return(cast_ll_value_(pos, &(ptr->v.l.a)));
			ptr->v.l.b = 0.0L;
			break;

		case MathType_rational:
			Return(single_float_rational_(pos, &(ptr->v.s.a)));
			ptr->v.s.b = 0.0f;
			type = MathType_single;
			break;

		case MathType_complex:
			break;

		case MathType_error:
		default:
			*ret = MathType_error;
			return fmte_("type error", NULL);
	}
	ptr->type = type;

	return Result(ret, type);
}

int getmathcomplex1_inverse_(struct mathreal2_struct *ptr,
		addr pos, enum MathType *ret)
{
	enum MathType type;

	getmathcomplex1_inverse_type(pos, &type);
	switch (type) {
		case MathType_single:
			Return(cast_ss_value_(pos, &(ptr->v.s.a)));
			ptr->v.s.b = 0.0f;
			break;

		case MathType_double:
			Return(cast_dd_value_(pos, &(ptr->v.d.a)));
			ptr->v.d.b = 0.0;
			break;

		case MathType_long:
			Return(cast_ll_value_(pos, &(ptr->v.l.a)));
			ptr->v.l.b = 0.0L;
			break;

		case MathType_rational:
			ptr->v.a.x = pos;
			break;

		case MathType_complex:
			Return(getmathcomplex1_complex_(ptr, pos, &type));
			break;

		case MathType_error:
		default:
			*ret = MathType_error;
			return fmte_("type error", NULL);
	}
	ptr->type = type;

	return Result(ret, type);
}

int getmathcomplex1_sqrt_(struct mathreal2_struct *ptr,
		addr pos, enum MathType *ret)
{
	enum MathType type;

	getmathcomplex1_log_type(pos, &type);
	switch (type) {
		case MathType_single:
			Return(cast_ss_value_(pos, &(ptr->v.s.a)));
			ptr->v.s.b = 0.0f;
			break;

		case MathType_double:
			Return(cast_dd_value_(pos, &(ptr->v.d.a)));
			ptr->v.d.b = 0.0;
			break;

		case MathType_long:
			Return(cast_ll_value_(pos, &(ptr->v.l.a)));
			ptr->v.l.b = 0.0L;
			break;

		case MathType_rational:
			Return(single_float_rational_(pos, &(ptr->v.s.a)));
			ptr->v.s.b = 0.0f;
			type = MathType_single;
			break;

		case MathType_complex:
			Return(getmathcomplex1_complex_(ptr, pos, &type));
			break;

		case MathType_error:
		default:
			*ret = MathType_error;
			return fmte_("type error", NULL);
	}
	ptr->type = type;

	return Result(ret, type);
}


/*
 *  getmathcomplex2
 */
static void getmathcomplex2_type_value(addr pos, enum MathType *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			*ret = MathType_single;
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = MathType_double;
			break;

		case LISPTYPE_LONG_FLOAT:
			*ret = MathType_long;
			break;

		case LISPTYPE_COMPLEX:
			getmathcomplex_type_complex(pos, ret);
			break;

		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
		case LISPTYPE_RATIO:
			*ret = MathType_rational;
			break;

		default:
			*ret = MathType_error;
			break;
	}
}

static int getmathcomplex2_type_single_(addr y, enum MathType *ret)
{
	enum MathType type;

	getmathcomplex2_type_value(y, &type);
	switch (type) {
		case MathType_single:
		case MathType_double:
		case MathType_long:
			return Result(ret, type);

		case MathType_rational:
			return Result(ret, MathType_single);

		case MathType_complex:
		case MathType_error:
		default:
			*ret = MathType_error;
			return TypeError_(y, NUMBER);
	}
}

static int getmathcomplex2_type_double_(addr y, enum MathType *ret)
{
	enum MathType type;

	getmathcomplex2_type_value(y, &type);
	switch (type) {
		case MathType_single:
		case MathType_double:
		case MathType_rational:
			return Result(ret, MathType_double);

		case MathType_long:
			return Result(ret, MathType_long);

		case MathType_complex:
		case MathType_error:
		default:
			*ret = MathType_error;
			return TypeError_(y, NUMBER);
	}
}

static int getmathcomplex2_type_long_(addr y, enum MathType *ret)
{
	enum MathType type;

	getmathcomplex2_type_value(y, &type);
	switch (type) {
		case MathType_single:
		case MathType_double:
		case MathType_long:
		case MathType_rational:
			return Result(ret, MathType_long);

		case MathType_complex:
		case MathType_error:
		default:
			*ret = MathType_error;
			return TypeError_(y, NUMBER);
	}
}

static int getmathcomplex2_type_rational_(addr y, enum MathType *ret)
{
	enum MathType type;

	getmathcomplex2_type_value(y, &type);
	switch (type) {
		case MathType_single:
		case MathType_double:
		case MathType_long:
		case MathType_rational:
			return Result(ret, type);

		case MathType_complex:
		case MathType_error:
		default:
			*ret = MathType_error;
			return TypeError_(y, NUMBER);
	}
}

static int getmathcomplex2_type_(addr x, addr y, enum MathType *ret)
{
	enum MathType type;

	getmathcomplex2_type_value(x, &type);
	switch (type) {
		case MathType_single:
			return getmathcomplex2_type_single_(y, ret);

		case MathType_double:
			return getmathcomplex2_type_double_(y, ret);

		case MathType_long:
			return getmathcomplex2_type_long_(y, ret);

		case MathType_rational:
			return getmathcomplex2_type_rational_(y, ret);

		case MathType_complex:
		case MathType_error:
		default:
			*ret = MathType_error;
			return TypeError_(x, NUMBER);
	}
}

static int getmathcomplex2_single1_(addr pos, single_float *re, single_float *im)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			Return(cast_ss_value_(pos, re));
			*im = 0.0f;
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			Return(cast_ds_value_(pos, re));
			*im = 0.0f;
			break;

		case LISPTYPE_LONG_FLOAT:
			Return(cast_ls_value_(pos, re));
			*im = 0.0f;
			break;

		case LISPTYPE_COMPLEX:
			return single_float_complex_(pos, re, im);

		case LISPTYPE_FIXNUM:
			*re = single_float_fixnum(pos);
			*im = 0.0f;
			break;

		case LISPTYPE_BIGNUM:
			Return(single_float_bignum_(pos, re));
			*im = 0.0f;
			return 0;

		case LISPTYPE_RATIO:
			Return(single_float_ratio_(pos, re));
			*im = 0.0f;
			return 0;

		default:
			*re = 0.0f;
			*im = 0.0f;
			return TypeError_(pos, NUMBER);
	}

	return 0;
}

static int getmathcomplex2_double1_(addr pos, double_float *re, double_float *im)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			Return(cast_sd_value_(pos, re));
			*im = 0.0;
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			Return(cast_dd_value_(pos, re));
			*im = 0.0;
			break;

		case LISPTYPE_LONG_FLOAT:
			Return(cast_ld_value_(pos, re));
			*im = 0.0;
			break;

		case LISPTYPE_COMPLEX:
			return double_float_complex_(pos, re, im);

		case LISPTYPE_FIXNUM:
			*re = double_float_fixnum(pos);
			*im = 0.0;
			break;

		case LISPTYPE_BIGNUM:
			Return(double_float_bignum_(pos, re));
			*im = 0.0;
			break;

		case LISPTYPE_RATIO:
			Return(double_float_ratio_(pos, re));
			*im = 0.0;
			break;

		default:
			*re = 0.0;
			*im = 0.0;
			return TypeError_(pos, NUMBER);
	}

	return 0;
}

static int getmathcomplex2_long1_(addr pos, long_float *re, long_float *im)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			Return(cast_sl_value_(pos, re));
			*im = 0.0L;
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			Return(cast_dl_value_(pos, re));
			*im = 0.0L;
			break;

		case LISPTYPE_LONG_FLOAT:
			Return(cast_ll_value_(pos, re));
			*im = 0.0L;
			break;

		case LISPTYPE_COMPLEX:
			return long_float_complex_(pos, re, im);

		case LISPTYPE_FIXNUM:
			*re = long_float_fixnum(pos);
			*im = 0.0L;
			break;

		case LISPTYPE_BIGNUM:
			Return(long_float_bignum_(pos, re));
			*im = 0.0L;
			break;

		case LISPTYPE_RATIO:
			Return(long_float_ratio_(pos, re));
			*im = 0.0L;
			break;

		default:
			*re = 0.0L;
			*im = 0.0L;
			return TypeError_(pos, NUMBER);
	}

	return 0;
}

static int getmathcomplex2_single_(struct mathcomplex2_struct *ptr, addr x, addr y)
{
	single_float real, imag;

	Return(getmathcomplex2_single1_(x, &real, &imag));
	ptr->v.s.a = real;
	ptr->v.s.b = imag;
	Return(getmathcomplex2_single1_(y, &real, &imag));
	ptr->v.s.c = real;
	ptr->v.s.d = imag;

	return 0;
}

static int getmathcomplex2_double_(struct mathcomplex2_struct *ptr, addr x, addr y)
{
	double_float real, imag;

	Return(getmathcomplex2_double1_(x, &real, &imag));
	ptr->v.d.a = real;
	ptr->v.d.b = imag;
	Return(getmathcomplex2_double1_(y, &real, &imag));
	ptr->v.d.c = real;
	ptr->v.d.d = imag;

	return 0;
}

static int getmathcomplex2_long_(struct mathcomplex2_struct *ptr, addr x, addr y)
{
	long_float real, imag;

	Return(getmathcomplex2_long1_(x, &real, &imag));
	ptr->v.l.a = real;
	ptr->v.l.b = imag;
	Return(getmathcomplex2_long1_(y, &real, &imag));
	ptr->v.l.c = real;
	ptr->v.l.d = imag;

	return 0;
}

int getmathcomplex2_float_(struct mathcomplex2_struct *ptr,
		addr x, addr y, enum MathType *ret)
{
	enum MathType type;

	Return(getmathcomplex2_type_(x, y, &type));
	switch (type) {
		case MathType_single:
			Return(getmathcomplex2_single_(ptr, x, y));
			break;

		case MathType_double:
			Return(getmathcomplex2_double_(ptr, x, y));
			break;

		case MathType_long:
			Return(getmathcomplex2_long_(ptr, x, y));
			break;

		case MathType_rational:
			Return(getmathcomplex2_single_(ptr, x, y));
			type = MathType_single;
			break;

		case MathType_complex:
		case MathType_error:
		default:
			*ret = MathType_error;
			return fmte_("type error", NULL);
	}
	ptr->type = type;

	return Result(ret, type);
}


/*
 *  getmathcomplex2_rational
 */
static void getmathcomplex2_rational(struct mathcomplex2_struct *ptr, addr x, addr y)
{
	ptr->v.a.x = x;
	ptr->v.a.y = y;
}

int getmathcomplex2_addr_(struct mathcomplex2_struct *ptr,
		addr x, addr y, enum MathType *ret)
{
	enum MathType type;

	Return(getmathcomplex2_type_(x, y, &type));
	switch (type) {
		case MathType_single:
			Return(getmathcomplex2_single_(ptr, x, y));
			break;

		case MathType_double:
			Return(getmathcomplex2_double_(ptr, x, y));
			break;

		case MathType_long:
			Return(getmathcomplex2_long_(ptr, x, y));
			break;

		case MathType_rational:
			getmathcomplex2_rational(ptr, x, y);
			break;

		case MathType_complex:
		case MathType_error:
		default:
			*ret = MathType_error;
			return fmte_("type error", NULL);
	}
	ptr->type = type;

	return Result(ret, type);
}

