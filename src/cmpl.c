#include "bignum.h"
#include "bignum_equal.h"
#include "bignum_object.h"
#include "build.h"
#include "cmpl.h"
#include "cmpl_arch.h"
#include "cmpl_math.h"
#include "condition.h"
#include "equal.h"
#include "float_equal.h"
#include "float_object.h"
#include "float_plus.h"
#include "heap.h"
#include "integer.h"
#include "number.h"
#include "ratio.h"
#include "rational.h"
#include "rational_equal.h"
#include "rational_plus.h"
#include "real_equal.h"
#include "type.h"

_g int complexp(addr pos)
{
	return GetType(pos) == LISPTYPE_COMPLEX;
}

_g void setreal_complex(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_COMPLEX);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetRealComplex_Low(pos, value);
}

_g void getreal_complex(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_COMPLEX);
	GetRealComplex_Low(pos, ret);
}

_g void setimag_complex(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_COMPLEX);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetImagComplex_Low(pos, value);
}

_g void getimag_complex(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_COMPLEX);
	GetImagComplex_Low(pos, ret);
}

_g void settype_complex(addr pos, int value)
{
	CheckType(pos, LISPTYPE_COMPLEX);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetTypeComplex_Low(pos, value);
}

_g enum ComplexType gettype_complex(addr pos)
{
	CheckType(pos, LISPTYPE_COMPLEX);
	return GetTypeComplex_Low(pos);
}

_g enum ComplexType getcomplex(addr pos, enum ComplexType *type, addr *real, addr *imag)
{
	CheckType(pos, LISPTYPE_COMPLEX);
	*type = GetTypeComplex_Low(pos);
	GetRealComplex_Low(pos, real);
	GetImagComplex_Low(pos, imag);
	return *type;
}

_g enum ComplexType getcomplexr(addr pos, addr *real, addr *imag)
{
	CheckType(pos, LISPTYPE_COMPLEX);
	GetRealComplex_Low(pos, real);
	GetImagComplex_Low(pos, imag);
	return GetTypeComplex_Low(pos);
}


/*
 *  rational complex  -> #c(1/2 3)
 *  float complex     -> #c(0.5 3.0)
 */
_g void make_complex_unsafe(LocalRoot local, addr *ret, enum ComplexType type)
{
	alloc_array2(local, ret, LISPTYPE_COMPLEX, 2);
	SetTypeComplex(*ret, type);
}

static void complex_unsafe_alloc(LocalRoot local, addr *ret,
		addr real, addr imag, enum ComplexType type)
{
	addr pos;

	real_throw_alloc(local, real, &real);
	real_throw_alloc(local, imag, &imag);
	make_complex_unsafe(local, &pos, type);
	SetRealComplex(pos, real);
	SetImagComplex(pos, imag);
	*ret = pos;
}

static void complex_unsafe_heap(addr *ret,
		addr real, addr imag, enum ComplexType type)
{
	complex_unsafe_alloc(NULL, ret, real, imag, type);
}

static int complex_fixnum_(LocalRoot local, addr *ret, addr real, addr imag)
{
	switch (GetType(imag)) {
		case LISPTYPE_SINGLE_FLOAT:
			single_float_alloc(local, &real, (single_float)RefFixnum(real));
			single_float_throw_alloc(local, imag, &imag);
			complex_unsafe_alloc(local, ret, real, imag, ComplexType_single);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			double_float_fixnum_alloc(local, &real, real);
			double_float_throw_alloc(local, imag, &imag);
			complex_unsafe_alloc(local, ret, real, imag, ComplexType_double);
			break;

		case LISPTYPE_LONG_FLOAT:
			long_float_fixnum_alloc(local, &real, real);
			long_float_throw_alloc(local, imag, &imag);
			complex_unsafe_alloc(local, ret, real, imag, ComplexType_long);
			break;

		case LISPTYPE_FIXNUM:
			if (zerop_fixnum(imag)) {
				fixnum_throw_alloc(local, real, ret);
			}
			else {
				fixnum_throw_alloc(local, real, &real);
				fixnum_throw_alloc(local, imag, &imag);
				complex_unsafe_alloc(local, ret, real, imag, ComplexType_rational);
			}
			break;

		case LISPTYPE_BIGNUM:
			if (zerop_bignum(imag)) {
				fixnum_throw_alloc(local, real, ret);
			}
			else {
				fixnum_throw_alloc(local, real, &real);
				bignum_throw_alloc(local, imag, &imag);
				complex_unsafe_alloc(local, ret, real, imag, ComplexType_rational);
			}
			break;

		case LISPTYPE_RATIO:
			if (zerop_ratio(imag)) {
				fixnum_throw_alloc(local, real, ret);
			}
			else {
				fixnum_throw_alloc(local, real, &real);
				ratio_throw_alloc(local, imag, &imag);
				complex_unsafe_alloc(local, ret, real, imag, ComplexType_rational);
			}
			break;

		default:
			*ret = Nil;
			return TypeError_(imag, REAL);
	}

	return 0;
}

static int complex_bignum_(LocalRoot local, addr *ret, addr real, addr imag)
{
	switch (GetType(imag)) {
		case LISPTYPE_SINGLE_FLOAT:
			single_float_bignum_alloc(local, &real, real);
			single_float_throw_alloc(local, imag, &imag);
			complex_unsafe_alloc(local, ret, real, imag, ComplexType_single);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			double_float_bignum_alloc(local, &real, real);
			double_float_throw_alloc(local, imag, &imag);
			complex_unsafe_alloc(local, ret, real, imag, ComplexType_double);
			break;

		case LISPTYPE_LONG_FLOAT:
			long_float_bignum_alloc(local, &real, real);
			long_float_throw_alloc(local, imag, &imag);
			complex_unsafe_alloc(local, ret, real, imag, ComplexType_long);
			break;

		case LISPTYPE_FIXNUM:
			if (zerop_fixnum(imag)) {
				bignum_throw_alloc(local, real, ret);
			}
			else {
				bignum_throw_alloc(local, real, &real);
				fixnum_throw_alloc(local, imag, &imag);
				complex_unsafe_alloc(local, ret, real, imag, ComplexType_rational);
			}
			break;

		case LISPTYPE_BIGNUM:
			if (zerop_bignum(imag)) {
				bignum_throw_alloc(local, real, ret);
			}
			else {
				bignum_throw_alloc(local, real, &real);
				bignum_throw_alloc(local, imag, &imag);
				complex_unsafe_alloc(local, ret, real, imag, ComplexType_rational);
			}
			break;

		case LISPTYPE_RATIO:
			if (zerop_ratio(imag)) {
				bignum_throw_alloc(local, real, ret);
			}
			else {
				bignum_throw_alloc(local, real, &real);
				ratio_throw_alloc(local, imag, &imag);
				complex_unsafe_alloc(local, ret, real, imag, ComplexType_rational);
			}
			break;

		default:
			*ret = Nil;
			return TypeError_(imag, REAL);
	}

	return 0;
}

static int complex_ratio_(LocalRoot local, addr *ret, addr real, addr imag)
{
	switch (GetType(imag)) {
		case LISPTYPE_SINGLE_FLOAT:
			single_float_alloc(local, &real, single_float_ratio(real));
			single_float_throw_alloc(local, imag, &imag);
			complex_unsafe_alloc(local, ret, real, imag, ComplexType_single);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			double_float_alloc(local, &real, double_float_ratio(real));
			double_float_throw_alloc(local, imag, &imag);
			complex_unsafe_alloc(local, ret, real, imag, ComplexType_double);
			break;

		case LISPTYPE_LONG_FLOAT:
			long_float_alloc(local, &real, long_float_ratio(real));
			long_float_throw_alloc(local, imag, &imag);
			complex_unsafe_alloc(local, ret, real, imag, ComplexType_long);
			break;

		case LISPTYPE_FIXNUM:
			if (zerop_fixnum(imag)) {
				ratio_throw_alloc(local, real, ret);
			}
			else {
				ratio_throw_alloc(local, real, &real);
				fixnum_throw_alloc(local, imag, &imag);
				complex_unsafe_alloc(local, ret, real, imag, ComplexType_rational);
			}
			break;

		case LISPTYPE_BIGNUM:
			if (zerop_bignum(imag)) {
				ratio_throw_alloc(local, real, ret);
			}
			else {
				ratio_throw_alloc(local, real, &real);
				bignum_throw_alloc(local, imag, &imag);
				complex_unsafe_alloc(local, ret, real, imag, ComplexType_rational);
			}
			break;

		case LISPTYPE_RATIO:
			if (zerop_ratio(imag)) {
				ratio_throw_alloc(local, real, ret);
			}
			else {
				ratio_throw_alloc(local, real, &real);
				ratio_throw_alloc(local, imag, &imag);
				complex_unsafe_alloc(local, ret, real, imag, ComplexType_rational);
			}
			break;

		default:
			*ret = Nil;
			return TypeError_(imag, REAL);
	}

	return 0;
}

static int complex_single_(LocalRoot local, addr *ret, addr real, addr imag)
{
	switch (GetType(imag)) {
		case LISPTYPE_SINGLE_FLOAT:
			single_float_throw_alloc(local, real, &real);
			single_float_throw_alloc(local, imag, &imag);
			complex_unsafe_alloc(local, ret, real, imag, ComplexType_single);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			double_float_alloc(local, &real, (double_float)RefSingleFloat(real));
			double_float_throw_alloc(local, imag, &imag);
			complex_unsafe_alloc(local, ret, real, imag, ComplexType_double);
			break;

		case LISPTYPE_LONG_FLOAT:
			long_float_alloc(local, &real, (long_float)RefSingleFloat(real));
			long_float_throw_alloc(local, imag, &imag);
			complex_unsafe_alloc(local, ret, real, imag, ComplexType_long);
			break;

		case LISPTYPE_FIXNUM:
			single_float_throw_alloc(local, real, &real);
			single_float_alloc(local, &imag, (single_float)RefFixnum(imag));
			complex_unsafe_alloc(local, ret, real, imag, ComplexType_single);
			break;

		case LISPTYPE_BIGNUM:
			single_float_throw_alloc(local, real, &real);
			single_float_bignum_alloc(local, &imag, imag);
			complex_unsafe_alloc(local, ret, real, imag, ComplexType_single);
			break;

		case LISPTYPE_RATIO:
			single_float_throw_alloc(local, real, &real);
			single_float_alloc(local, &imag, single_float_ratio(imag));
			complex_unsafe_alloc(local, ret, real, imag, ComplexType_single);
			break;

		default:
			*ret = Nil;
			return TypeError_(imag, REAL);
	}

	return 0;
}

static int complex_double_(LocalRoot local, addr *ret, addr real, addr imag)
{
	switch (GetType(imag)) {
		case LISPTYPE_SINGLE_FLOAT:
			double_float_throw_alloc(local, real, &real);
			double_float_alloc(local, &imag, (double_float)RefSingleFloat(imag));
			complex_unsafe_alloc(local, ret, real, imag, ComplexType_double);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			double_float_throw_alloc(local, real, &real);
			double_float_throw_alloc(local, imag, &imag);
			complex_unsafe_alloc(local, ret, real, imag, ComplexType_double);
			break;

		case LISPTYPE_LONG_FLOAT:
			long_float_alloc(local, &real, (long_float)RefDoubleFloat(real));
			long_float_throw_alloc(local, imag, &imag);
			complex_unsafe_alloc(local, ret, real, imag, ComplexType_long);
			break;

		case LISPTYPE_FIXNUM:
			double_float_throw_alloc(local, real, &real);
			double_float_fixnum_alloc(local, &imag, imag);
			complex_unsafe_alloc(local, ret, real, imag, ComplexType_double);
			break;

		case LISPTYPE_BIGNUM:
			double_float_throw_alloc(local, real, &real);
			double_float_bignum_alloc(local, &imag, imag);
			complex_unsafe_alloc(local, ret, real, imag, ComplexType_double);
			break;

		case LISPTYPE_RATIO:
			double_float_throw_alloc(local, real, &real);
			double_float_alloc(local, &imag, double_float_ratio(imag));
			complex_unsafe_alloc(local, ret, real, imag, ComplexType_double);
			break;

		default:
			*ret = Nil;
			return TypeError_(imag, REAL);
	}

	return 0;
}

static int complex_long_(LocalRoot local, addr *ret, addr real, addr imag)
{
	switch (GetType(imag)) {
		case LISPTYPE_SINGLE_FLOAT:
			long_float_throw_alloc(local, real, &real);
			long_float_alloc(local, &imag, (long_float)RefSingleFloat(imag));
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			long_float_throw_alloc(local, real, &real);
			long_float_alloc(local, &imag, (long_float)RefDoubleFloat(imag));
			break;

		case LISPTYPE_LONG_FLOAT:
			long_float_throw_alloc(local, real, &real);
			long_float_throw_alloc(local, imag, &imag);
			break;

		case LISPTYPE_FIXNUM:
			long_float_throw_alloc(local, real, &real);
			long_float_fixnum_alloc(local, &imag, imag);
			break;

		case LISPTYPE_BIGNUM:
			long_float_throw_alloc(local, real, &real);
			long_float_bignum_alloc(local, &imag, imag);
			break;

		case LISPTYPE_RATIO:
			long_float_throw_alloc(local, real, &real);
			long_float_alloc(local, &imag, long_float_ratio(imag));
			break;

		default:
			*ret = Nil;
			return TypeError_(imag, REAL);
	}
	complex_unsafe_alloc(local, ret, real, imag, ComplexType_long);
	return 0;
}

static int complex2_alloc_(LocalRoot local, addr *ret, addr real, addr imag)
{
	switch (GetType(real)) {
		case LISPTYPE_SINGLE_FLOAT:
			return complex_single_(local, ret, real, imag);

		case LISPTYPE_DOUBLE_FLOAT:
			return complex_double_(local, ret, real, imag);

		case LISPTYPE_LONG_FLOAT:
			return complex_long_(local, ret, real, imag);

		case LISPTYPE_FIXNUM:
			return complex_fixnum_(local, ret, real, imag);

		case LISPTYPE_BIGNUM:
			return complex_bignum_(local, ret, real, imag);

		case LISPTYPE_RATIO:
			return complex_ratio_(local, ret, real, imag);

		default:
			*ret = Nil;
			return TypeError_(real, REAL);
	}
}

static int complex1_alloc_(LocalRoot local, addr *ret, addr real)
{
	addr imag;

	switch (GetType(real)) {
		case LISPTYPE_SINGLE_FLOAT:
			single_float_throw_alloc(local, real, &real);
			single_float_alloc(local, &imag, 0.0f);
			complex_unsafe_alloc(local, ret, real, imag, ComplexType_single);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			double_float_throw_alloc(local, real, &real);
			double_float_alloc(local, &imag, 0.0);
			complex_unsafe_alloc(local, ret, real, imag, ComplexType_double);
			break;

		case LISPTYPE_LONG_FLOAT:
			long_float_throw_alloc(local, real, &real);
			long_float_alloc(local, &imag, 0.0L);
			complex_unsafe_alloc(local, ret, real, imag, ComplexType_long);
			break;

		case LISPTYPE_FIXNUM:
			fixnum_throw_alloc(local, real, ret);
			break;

		case LISPTYPE_BIGNUM:
			bignum_throw_alloc(local, real, ret);
			break;

		case LISPTYPE_RATIO:
			ratio_throw_alloc(local, real, ret);
			break;

		default:
			*ret = Nil;
			return TypeError_(real, REAL);
	}

	return 0;
}

_g int complex_alloc_(LocalRoot local, addr *ret, addr real, addr imag)
{
	if (imag == Unbound)
		return complex1_alloc_(local, ret, real);
	else
		return complex2_alloc_(local, ret, real, imag);
}
_g int complex_local_(LocalRoot local, addr *ret, addr real, addr imag)
{
	Check(local == NULL, "local error");
	return complex_alloc_(local, ret, real, imag);
}
_g int complex_heap_(addr *ret, addr real, addr imag)
{
	return complex_alloc_(NULL, ret, real, imag);
}

_g void complex_single_alloc(LocalRoot local,
		addr *ret, single_float real, single_float imag)
{
	addr pos1, pos2;

	single_float_check_alloc(local, &pos1, real);
	single_float_check_alloc(local, &pos2, imag);
	complex_unsafe_alloc(local, ret, pos1, pos2, ComplexType_single);
}
_g void complex_single_local(LocalRoot local,
		addr *ret, single_float real, single_float imag)
{
	Check(local == NULL, "local error");
	complex_single_alloc(local, ret, real, imag);
}
_g void complex_single_heap(addr *ret,
		single_float real, single_float imag)
{
	complex_single_alloc(NULL, ret, real, imag);
}

_g void complex_double_alloc(LocalRoot local,
		addr *ret, double_float real, double_float imag)
{
	addr pos1, pos2;

	double_float_check_alloc(local, &pos1, real);
	double_float_check_alloc(local, &pos2, imag);
	complex_unsafe_alloc(local, ret, pos1, pos2, ComplexType_double);
}
_g void complex_double_local(LocalRoot local,
		addr *ret, double_float real, double_float imag)
{
	Check(local == NULL, "local error");
	complex_double_alloc(local, ret, real, imag);
}
_g void complex_double_heap(addr *ret,
		double_float real, double_float imag)
{
	complex_double_alloc(NULL, ret, real, imag);
}

_g void complex_long_alloc(LocalRoot local,
		addr *ret, long_float real, long_float imag)
{
	addr pos1, pos2;

	long_float_check_alloc(local, &pos1, real);
	long_float_check_alloc(local, &pos2, imag);
	complex_unsafe_alloc(local, ret, pos1, pos2, ComplexType_long);
}
_g void complex_long_local(LocalRoot local,
		addr *ret, long_float real, long_float imag)
{
	Check(local == NULL, "local error");
	complex_long_alloc(local, ret, real, imag);
}
_g void complex_long_heap(addr *ret,
		long_float real, long_float imag)
{
	complex_long_alloc(NULL, ret, real, imag);
}

_g void complex_copy_alloc(LocalRoot local, addr pos, addr *ret)
{
	enum ComplexType type;
	addr real, imag;

	Check(GetType(pos) != LISPTYPE_COMPLEX, "type error");
	getcomplex(pos, &type, &real, &imag);
	real_copy_alloc(local, real, &real);
	real_copy_alloc(local, imag, &imag);
	complex_unsafe_alloc(local, ret, real, imag, type);
}
_g void complex_copy_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	complex_copy_alloc(local, pos, ret);
}
_g void complex_copy_heap(addr pos, addr *ret)
{
	complex_copy_alloc(NULL, pos, ret);
}

_g int complex_result_local_(LocalRoot local, addr pos, addr *ret)
{
	addr real, imag;

	Check(local == NULL, "local error");
	CheckType(pos, LISPTYPE_COMPLEX);
	GetRealComplex(pos, &real);
	GetImagComplex(pos, &imag);
	if (zerop_real_inplace(imag)) {
		real_result_local(local, real, ret);
		return 0;
	}
	else {
		real_result_local(local, real, &real);
		real_result_local(local, imag, &imag);
		return complex_local_(local, ret, real, imag);
	}
}
_g int complex_result_heap_(LocalRoot local, addr pos, addr *ret)
{
	addr real, imag;

	Check(local == NULL, "local error");
	CheckType(pos, LISPTYPE_COMPLEX);
	GetRealComplex(pos, &real);
	GetImagComplex(pos, &imag);
	if (zerop_real_inplace(imag)) {
		real_result_heap(local, real, ret);
		return 0;
	}
	else {
		real_result_heap(local, real, &real);
		real_result_heap(local, imag, &imag);
		return complex_heap_(ret, real, imag);
	}
}

_g void complex_throw_alloc(LocalRoot local, addr pos, addr *ret)
{
	Check(GetType(pos) != LISPTYPE_COMPLEX, "type error");
	if (GetStatusDynamic(pos))
		complex_copy_heap(pos, ret);
	else
		*ret = pos;
}
_g void complex_throw_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	complex_throw_alloc(local, pos, ret);
}

_g void complex_throw_heap(addr pos, addr *ret)
{
	complex_throw_alloc(NULL, pos, ret);
}

static int complex_force_type_(addr pos, enum ComplexType *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
		case LISPTYPE_RATIO:
			return Result(ret, ComplexType_rational);

		case LISPTYPE_SINGLE_FLOAT:
			return Result(ret, ComplexType_single);

		case LISPTYPE_DOUBLE_FLOAT:
			return Result(ret, ComplexType_double);

		case LISPTYPE_LONG_FLOAT:
			return Result(ret, ComplexType_long);

		default:
			*ret = ComplexType_error;
			return TypeError_(pos, REAL);
	}
}

_g int complex_force_heap_(addr *ret, addr real, addr imag, enum ComplexType type)
{
	addr pos;

	real_throw_heap(real, &real);
	real_throw_heap(imag, &imag);
	switch (type) {
		case ComplexType_single:
		case ComplexType_double:
		case ComplexType_long:
		case ComplexType_rational:
			break;

		case ComplexType_error:
		default:
			Return(complex_force_type_(real, &type));
			break;
	}

	make_complex_unsafe(NULL, &pos, type);
	SetRealComplex(pos, real);
	SetImagComplex(pos, imag);
	return Result(ret, pos);
}

_g int single_float_complex_(addr pos, single_float *re, single_float *im)
{
	addr real, imag;

	CheckType(pos, LISPTYPE_COMPLEX);
	switch (getcomplexr(pos, &real, &imag)) {
		case ComplexType_single:
			*re = cast_ss_value(real);
			*im = cast_ss_value(imag);
			break;

		case ComplexType_double:
			*re = cast_ds_value(real);
			*im = cast_ds_value(imag);
			break;

		case ComplexType_long:
			*re = cast_ls_value(real);
			*im = cast_ls_value(imag);
			break;

		case ComplexType_rational:
			*re = single_float_rational(real);
			*im = single_float_rational(imag);
			break;

		case ComplexType_error:
		default:
			*re = 0.0f;
			*im = 0.0f;
			return TypeError_(pos, COMPLEX);
	}

	return 0;
}

_g int double_float_complex_(addr pos, double_float *re, double_float *im)
{
	addr real, imag;

	CheckType(pos, LISPTYPE_COMPLEX);
	switch (getcomplexr(pos, &real, &imag)) {
		case ComplexType_single:
			*re = cast_sd_value(real);
			*im = cast_sd_value(imag);
			break;

		case ComplexType_double:
			*re = cast_dd_value(real);
			*im = cast_dd_value(imag);
			break;

		case ComplexType_long:
			*re = cast_dl_value(real);
			*im = cast_dl_value(imag);
			break;

		case ComplexType_rational:
			*re = double_float_rational(real);
			*im = double_float_rational(imag);
			break;

		case ComplexType_error:
		default:
			*re = 0.0;
			*im = 0.0;
			return TypeError_(pos, COMPLEX);
	}

	return 0;
}

_g int long_float_complex_(addr pos, long_float *re, long_float *im)
{
	addr real, imag;

	CheckType(pos, LISPTYPE_COMPLEX);
	switch (getcomplexr(pos, &real, &imag)) {
		case ComplexType_single:
			*re = cast_sl_value(real);
			*im = cast_sl_value(imag);
			break;

		case ComplexType_double:
			*re = cast_dl_value(real);
			*im = cast_dl_value(imag);
			break;

		case ComplexType_long:
			*re = cast_ll_value(real);
			*im = cast_ll_value(imag);
			break;

		case ComplexType_rational:
			*re = long_float_rational(real);
			*im = long_float_rational(imag);
			break;

		case ComplexType_error:
		default:
			*re = 0.0L;
			*im = 0.0L;
			return TypeError_(pos, COMPLEX);
	}

	return 0;
}

static int zerop_call_complex_(addr pos, int *ret, int (*call)(addr))
{
	addr check;
	GetRealComplex(pos, &check);
	if (! (*call)(check))
		return Result(ret, 0);
	GetImagComplex(pos, &check);
	return Result(ret, (*call)(check));
}

_g int zerop_complex_(addr pos, int *ret)
{
	Check(GetType(pos) != LISPTYPE_COMPLEX, "type error");
	switch (GetTypeComplex(pos)) {
		case ComplexType_single:
			return zerop_call_complex_(pos, ret, zerop_single_float);

		case ComplexType_double:
			return zerop_call_complex_(pos, ret, zerop_double_float);

		case ComplexType_long:
			return zerop_call_complex_(pos, ret, zerop_long_float);

		case ComplexType_rational:
			return zerop_call_complex_(pos, ret, zerop_rational);

		default:
			*ret = 0;
			return TypeError_(pos, COMPLEX);
	}
}

_g int eql_complex(addr left, addr right)
{
	addr check1, check2;

	Check(GetType(left) != LISPTYPE_COMPLEX, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetRealComplex(left, &check1);
	GetRealComplex(right, &check2);
	if (! eql_function(check1, check2))
		return 0;
	GetImagComplex(left, &check1);
	GetImagComplex(right, &check2);

	return eql_function(check1, check2);
}

_g int equal_complex(LocalRoot local, addr left, addr right)
{
	addr check1, check2;

	Check(GetType(left) != LISPTYPE_COMPLEX, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetRealComplex(left, &check1);
	GetRealComplex(right, &check2);
	if (! equal_real_inplace(local, check1, check2))
		return 0;
	GetImagComplex(left, &check1);
	GetImagComplex(right, &check2);

	return equal_real_inplace(local, check1, check2);
}

_g int equal_fc_number(addr left, addr right)
{
	addr check;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetImagComplex(right, &check);
	if (! zerop_real_inplace(check))
		return 0;
	GetRealComplex(right, &check);

	return equal_fixnum_real_inplace(left, check);
}

_g int equal_bc_number(addr left, addr right)
{
	addr check;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetImagComplex(right, &check);
	if (! zerop_real_inplace(check))
		return 0;
	GetRealComplex(right, &check);

	return equal_bignum_real_inplace(left, check);
}

_g int equal_rc_number(LocalRoot local, addr left, addr right)
{
	addr check;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetImagComplex(right, &check);
	if (! zerop_real_inplace(check))
		return 0;
	GetRealComplex(right, &check);

	return equal_ratio_real_inplace(local, left, check);
}

_g int equal_sc_number(LocalRoot local, addr left, addr right)
{
	addr check;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetImagComplex(right, &check);
	if (! zerop_real_inplace(check))
		return 0;
	GetRealComplex(right, &check);

	return equal_single_float_real_inplace(local, left, check);
}

_g int equal_dc_number(LocalRoot local, addr left, addr right)
{
	addr check;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetImagComplex(right, &check);
	if (! zerop_real_inplace(check))
		return 0;
	GetRealComplex(right, &check);

	return equal_double_float_real_inplace(local, left, check);
}

_g int equal_lc_number(LocalRoot local, addr left, addr right)
{
	addr check;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetImagComplex(right, &check);
	if (! zerop_real_inplace(check))
		return 0;
	GetRealComplex(right, &check);

	return equal_long_float_real_inplace(local, left, check);
}

_g int sign_reverse_complex_common_(addr pos, addr *ret)
{
	enum ComplexType type;
	addr real, imag;

	Check(GetType(pos) != LISPTYPE_COMPLEX, "type error");
	switch (getcomplex(pos, &type, &real, &imag)) {
		case ComplexType_single:
			sign_reverse_floats_heap(real, &real);
			sign_reverse_floats_heap(imag, &imag);
			break;

		case ComplexType_double:
			sign_reverse_floatd_heap(real, &real);
			sign_reverse_floatd_heap(imag, &imag);
			break;

		case ComplexType_long:
			sign_reverse_floatl_heap(real, &real);
			sign_reverse_floatl_heap(imag, &imag);
			break;

		case ComplexType_rational:
			sign_reverse_rational_common(real, &real);
			sign_reverse_rational_common(imag, &imag);
			break;

		default:
			*ret = Nil;
			return TypeError_(pos, COMPLEX);
	}
	complex_unsafe_heap(ret, real, imag, type);
	return 0;
}


/*
 *  abs
 */
_g int abs_complex_common_(addr pos, addr *ret)
{
	addr real, imag;
	single_float vf, v2;
	double_float vd;
	long_float vl;

	CheckType(pos, LISPTYPE_COMPLEX);
	switch (getcomplexr(pos, &real, &imag)) {
		case ComplexType_single:
			cabs_f(RefSingleFloat(real), RefSingleFloat(imag), &vf);
			single_float_check_heap(ret, vf);
			break;

		case ComplexType_double:
			cabs_d(RefDoubleFloat(real), RefDoubleFloat(imag), &vd);
			double_float_check_heap(ret, vd);
			break;

		case ComplexType_long:
			cabs_l(RefLongFloat(real), RefLongFloat(imag), &vl);
			long_float_check_heap(ret, vl);
			break;

		case ComplexType_rational:
			vf = single_float_rational(real);
			v2 = single_float_rational(imag);
			single_float_check_heap(ret, sqrtf(vf*vf + v2+v2));
			break;

		default:
			*ret = Nil;
			return TypeError_(pos, COMPLEX);
	}

	return 0;
}


/*
 *  signum
 */
static int signum_complex_single_(addr pos, addr *ret)
{
	single_float real, imag, denom;

	Return(single_float_complex_(pos, &real, &imag));
	cabs_f(real, imag, &denom);
	if (denom == 0.0f)
		single_float_check_heap(ret, denom);
	else
		complex_single_heap(ret, real/denom, imag/denom);
	
	return 0;
}

static int signum_complex_double_(addr pos, addr *ret)
{
	double_float real, imag, denom;

	Return(double_float_complex_(pos, &real, &imag));
	cabs_d(real, imag, &denom);
	if (denom == 0.0f)
		double_float_check_heap(ret, denom);
	else
		complex_double_heap(ret, real/denom, imag/denom);

	return 0;
}

static int signum_complex_long_(addr pos, addr *ret)
{
	long_float real, imag, denom;

	Return(long_float_complex_(pos, &real, &imag));
	cabs_l(real, imag, &denom);
	if (denom == 0.0L)
		long_float_check_heap(ret, denom);
	else
		complex_long_heap(ret, real/denom, imag/denom);

	
	return 0;
}

_g int signum_complex_common_(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_COMPLEX);
	switch (GetTypeComplex(pos)) {
		case ComplexType_rational:
		case ComplexType_single:
			return signum_complex_single_(pos, ret);

		case ComplexType_double:
			return signum_complex_double_(pos, ret);

		case ComplexType_long:
			return signum_complex_long_(pos, ret);

		default:
			*ret = Nil;
			return TypeError_(pos, COMPLEX);
	}
}

