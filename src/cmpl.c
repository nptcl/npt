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

static int complex_unsafe_alloc_(LocalRoot local, addr *ret,
		addr real, addr imag, enum ComplexType type)
{
	addr pos;

	Return(real_throw_alloc_(local, real, &real));
	Return(real_throw_alloc_(local, imag, &imag));
	make_complex_unsafe(local, &pos, type);
	SetRealComplex(pos, real);
	SetImagComplex(pos, imag);

	return Result(ret, pos);
}

static int complex_unsafe_heap_(addr *ret,
		addr real, addr imag, enum ComplexType type)
{
	return complex_unsafe_alloc_(NULL, ret, real, imag, type);
}

static int complex_fixnum_(LocalRoot local, addr *ret, addr real, addr imag)
{
	switch (GetType(imag)) {
		case LISPTYPE_SINGLE_FLOAT:
			single_float_alloc(local, &real, (single_float)RefFixnum(real));
			single_float_throw_alloc(local, imag, &imag);
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_single);

		case LISPTYPE_DOUBLE_FLOAT:
			double_float_fixnum_alloc(local, &real, real);
			double_float_throw_alloc(local, imag, &imag);
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_double);

		case LISPTYPE_LONG_FLOAT:
			long_float_fixnum_alloc(local, &real, real);
			long_float_throw_alloc(local, imag, &imag);
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_long);

		case LISPTYPE_FIXNUM:
			if (zerop_fixnum(imag)) {
				fixnum_throw_alloc(local, real, ret);
			}
			else {
				fixnum_throw_alloc(local, real, &real);
				fixnum_throw_alloc(local, imag, &imag);
				return complex_unsafe_alloc_(local,
						ret, real, imag, ComplexType_rational);
			}
			return 0;

		case LISPTYPE_BIGNUM:
			if (zerop_bignum(imag)) {
				fixnum_throw_alloc(local, real, ret);
			}
			else {
				fixnum_throw_alloc(local, real, &real);
				bignum_throw_alloc(local, imag, &imag);
				return complex_unsafe_alloc_(local,
						ret, real, imag, ComplexType_rational);
			}
			return 0;

		case LISPTYPE_RATIO:
			if (zerop_ratio(imag)) {
				fixnum_throw_alloc(local, real, ret);
			}
			else {
				fixnum_throw_alloc(local, real, &real);
				ratio_throw_alloc(local, imag, &imag);
				return complex_unsafe_alloc_(local,
						ret, real, imag, ComplexType_rational);
			}
			return 0;

		default:
			*ret = Nil;
			return TypeError_(imag, REAL);
	}
}

static int complex_bignum_(LocalRoot local, addr *ret, addr real, addr imag)
{
	switch (GetType(imag)) {
		case LISPTYPE_SINGLE_FLOAT:
			Return(single_float_bignum_alloc_(local, &real, real));
			single_float_throw_alloc(local, imag, &imag);
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_single);

		case LISPTYPE_DOUBLE_FLOAT:
			Return(double_float_bignum_alloc_(local, &real, real));
			double_float_throw_alloc(local, imag, &imag);
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_double);

		case LISPTYPE_LONG_FLOAT:
			Return(long_float_bignum_alloc_(local, &real, real));
			long_float_throw_alloc(local, imag, &imag);
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_long);

		case LISPTYPE_FIXNUM:
			if (zerop_fixnum(imag)) {
				bignum_throw_alloc(local, real, ret);
			}
			else {
				bignum_throw_alloc(local, real, &real);
				fixnum_throw_alloc(local, imag, &imag);
				return complex_unsafe_alloc_(local,
						ret, real, imag, ComplexType_rational);
			}
			return 0;

		case LISPTYPE_BIGNUM:
			if (zerop_bignum(imag)) {
				bignum_throw_alloc(local, real, ret);
			}
			else {
				bignum_throw_alloc(local, real, &real);
				bignum_throw_alloc(local, imag, &imag);
				return complex_unsafe_alloc_(local,
						ret, real, imag, ComplexType_rational);
			}
			return 0;

		case LISPTYPE_RATIO:
			if (zerop_ratio(imag)) {
				bignum_throw_alloc(local, real, ret);
			}
			else {
				bignum_throw_alloc(local, real, &real);
				ratio_throw_alloc(local, imag, &imag);
				return complex_unsafe_alloc_(local,
						ret, real, imag, ComplexType_rational);
			}
			return 0;

		default:
			*ret = Nil;
			return TypeError_(imag, REAL);
	}
}

static int complex_ratio_(LocalRoot local, addr *ret, addr real, addr imag)
{
	switch (GetType(imag)) {
		case LISPTYPE_SINGLE_FLOAT:
			Return(single_float_ratio_alloc_(local, &real, real));
			single_float_throw_alloc(local, imag, &imag);
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_single);

		case LISPTYPE_DOUBLE_FLOAT:
			Return(double_float_ratio_alloc_(local, &real, real));
			double_float_throw_alloc(local, imag, &imag);
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_double);

		case LISPTYPE_LONG_FLOAT:
			Return(long_float_ratio_alloc_(local, &real, real));
			long_float_throw_alloc(local, imag, &imag);
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_long);

		case LISPTYPE_FIXNUM:
			if (zerop_fixnum(imag)) {
				ratio_throw_alloc(local, real, ret);
			}
			else {
				ratio_throw_alloc(local, real, &real);
				fixnum_throw_alloc(local, imag, &imag);
				return complex_unsafe_alloc_(local,
						ret, real, imag, ComplexType_rational);
			}
			return 0;

		case LISPTYPE_BIGNUM:
			if (zerop_bignum(imag)) {
				ratio_throw_alloc(local, real, ret);
			}
			else {
				ratio_throw_alloc(local, real, &real);
				bignum_throw_alloc(local, imag, &imag);
				return complex_unsafe_alloc_(local,
						ret, real, imag, ComplexType_rational);
			}
			return 0;

		case LISPTYPE_RATIO:
			if (zerop_ratio(imag)) {
				ratio_throw_alloc(local, real, ret);
			}
			else {
				ratio_throw_alloc(local, real, &real);
				ratio_throw_alloc(local, imag, &imag);
				return complex_unsafe_alloc_(local,
						ret, real, imag, ComplexType_rational);
			}
			return 0;

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
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_single);

		case LISPTYPE_DOUBLE_FLOAT:
			double_float_alloc(local, &real, (double_float)RefSingleFloat(real));
			double_float_throw_alloc(local, imag, &imag);
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_double);

		case LISPTYPE_LONG_FLOAT:
			long_float_alloc(local, &real, (long_float)RefSingleFloat(real));
			long_float_throw_alloc(local, imag, &imag);
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_long);

		case LISPTYPE_FIXNUM:
			single_float_throw_alloc(local, real, &real);
			single_float_alloc(local, &imag, (single_float)RefFixnum(imag));
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_single);

		case LISPTYPE_BIGNUM:
			single_float_throw_alloc(local, real, &real);
			Return(single_float_bignum_alloc_(local, &imag, imag));
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_single);

		case LISPTYPE_RATIO:
			single_float_throw_alloc(local, real, &real);
			Return(single_float_ratio_alloc_(local, &imag, imag));
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_single);

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
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_double);

		case LISPTYPE_DOUBLE_FLOAT:
			double_float_throw_alloc(local, real, &real);
			double_float_throw_alloc(local, imag, &imag);
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_double);

		case LISPTYPE_LONG_FLOAT:
			long_float_alloc(local, &real, (long_float)RefDoubleFloat(real));
			long_float_throw_alloc(local, imag, &imag);
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_long);

		case LISPTYPE_FIXNUM:
			double_float_throw_alloc(local, real, &real);
			double_float_fixnum_alloc(local, &imag, imag);
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_double);

		case LISPTYPE_BIGNUM:
			double_float_throw_alloc(local, real, &real);
			Return(double_float_bignum_alloc_(local, &imag, imag));
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_double);

		case LISPTYPE_RATIO:
			double_float_throw_alloc(local, real, &real);
			Return(double_float_ratio_alloc_(local, &imag, imag));
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_double);

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
			Return(long_float_bignum_alloc_(local, &imag, imag));
			break;

		case LISPTYPE_RATIO:
			long_float_throw_alloc(local, real, &real);
			Return(long_float_ratio_alloc_(local, &imag, imag));
			break;

		default:
			*ret = Nil;
			return TypeError_(imag, REAL);
	}

	return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_long);
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
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_single);

		case LISPTYPE_DOUBLE_FLOAT:
			double_float_throw_alloc(local, real, &real);
			double_float_alloc(local, &imag, 0.0);
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_double);

		case LISPTYPE_LONG_FLOAT:
			long_float_throw_alloc(local, real, &real);
			long_float_alloc(local, &imag, 0.0L);
			return complex_unsafe_alloc_(local, ret, real, imag, ComplexType_long);

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

_g int complex_single_alloc_(LocalRoot local,
		addr *ret, single_float real, single_float imag)
{
	addr pos1, pos2;

	Return(single_float_check_alloc_(local, &pos1, real));
	Return(single_float_check_alloc_(local, &pos2, imag));
	return complex_unsafe_alloc_(local, ret, pos1, pos2, ComplexType_single);
}
_g int complex_single_local_(LocalRoot local,
		addr *ret, single_float real, single_float imag)
{
	Check(local == NULL, "local error");
	return complex_single_alloc_(local, ret, real, imag);
}
_g int complex_single_heap_(addr *ret,
		single_float real, single_float imag)
{
	return complex_single_alloc_(NULL, ret, real, imag);
}

_g int complex_double_alloc_(LocalRoot local,
		addr *ret, double_float real, double_float imag)
{
	addr pos1, pos2;

	Return(double_float_check_alloc_(local, &pos1, real));
	Return(double_float_check_alloc_(local, &pos2, imag));
	return complex_unsafe_alloc_(local, ret, pos1, pos2, ComplexType_double);
}
_g int complex_double_local_(LocalRoot local,
		addr *ret, double_float real, double_float imag)
{
	Check(local == NULL, "local error");
	return complex_double_alloc_(local, ret, real, imag);
}
_g int complex_double_heap_(addr *ret,
		double_float real, double_float imag)
{
	return complex_double_alloc_(NULL, ret, real, imag);
}

_g int complex_long_alloc_(LocalRoot local,
		addr *ret, long_float real, long_float imag)
{
	addr pos1, pos2;

	Return(long_float_check_alloc_(local, &pos1, real));
	Return(long_float_check_alloc_(local, &pos2, imag));
	return complex_unsafe_alloc_(local, ret, pos1, pos2, ComplexType_long);
}
_g int complex_long_local_(LocalRoot local,
		addr *ret, long_float real, long_float imag)
{
	Check(local == NULL, "local error");
	return complex_long_alloc_(local, ret, real, imag);
}
_g int complex_long_heap_(addr *ret,
		long_float real, long_float imag)
{
	return complex_long_alloc_(NULL, ret, real, imag);
}

_g int complex_copy_alloc_(LocalRoot local, addr pos, addr *ret)
{
	enum ComplexType type;
	addr real, imag;

	Check(GetType(pos) != LISPTYPE_COMPLEX, "type error");
	getcomplex(pos, &type, &real, &imag);
	Return(real_copy_alloc_(local, real, &real));
	Return(real_copy_alloc_(local, imag, &imag));
	return complex_unsafe_alloc_(local, ret, real, imag, type);
}
_g int complex_copy_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	return complex_copy_alloc_(local, pos, ret);
}
_g int complex_copy_heap_(addr pos, addr *ret)
{
	return complex_copy_alloc_(NULL, pos, ret);
}

_g int complex_result_local_(LocalRoot local, addr pos, addr *ret)
{
	int check;
	addr real, imag;

	Check(local == NULL, "local error");
	CheckType(pos, LISPTYPE_COMPLEX);
	GetRealComplex(pos, &real);
	GetImagComplex(pos, &imag);
	Return(zerop_real_(imag, &check));
	if (check) {
		return real_result_local_(local, real, ret);
	}
	else {
		Return(real_result_local_(local, real, &real));
		Return(real_result_local_(local, imag, &imag));
		return complex_local_(local, ret, real, imag);
	}
}
_g int complex_result_heap_(LocalRoot local, addr pos, addr *ret)
{
	int check;
	addr real, imag;

	Check(local == NULL, "local error");
	CheckType(pos, LISPTYPE_COMPLEX);
	GetRealComplex(pos, &real);
	GetImagComplex(pos, &imag);
	Return(zerop_real_(imag, &check));
	if (check) {
		return real_result_heap_(local, real, ret);
	}
	else {
		Return(real_result_heap_(local, real, &real));
		Return(real_result_heap_(local, imag, &imag));
		return complex_heap_(ret, real, imag);
	}
}

_g int complex_throw_alloc_(LocalRoot local, addr pos, addr *ret)
{
	Check(GetType(pos) != LISPTYPE_COMPLEX, "type error");
	if (GetStatusDynamic(pos))
		return complex_copy_heap_(pos, ret);
	else
		return Result(ret, pos);
}
_g int complex_throw_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	return complex_throw_alloc_(local, pos, ret);
}

_g int complex_throw_heap_(addr pos, addr *ret)
{
	return complex_throw_alloc_(NULL, pos, ret);
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

	Return(real_throw_heap_(real, &real));
	Return(real_throw_heap_(imag, &imag));
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
			Return(cast_ss_value_(real, re));
			Return(cast_ss_value_(imag, im));
			return 0;

		case ComplexType_double:
			Return(cast_ds_value_(real, re));
			Return(cast_ds_value_(imag, im));
			return 0;

		case ComplexType_long:
			Return(cast_ls_value_(real, re));
			Return(cast_ls_value_(imag, im));
			return 0;

		case ComplexType_rational:
			Return(single_float_rational_(real, re));
			Return(single_float_rational_(imag, im));
			return 0;

		case ComplexType_error:
		default:
			*re = 0.0f;
			*im = 0.0f;
			return TypeError_(pos, COMPLEX);
	}
}

_g int double_float_complex_(addr pos, double_float *re, double_float *im)
{
	addr real, imag;

	CheckType(pos, LISPTYPE_COMPLEX);
	switch (getcomplexr(pos, &real, &imag)) {
		case ComplexType_single:
			Return(cast_sd_value_(real, re));
			Return(cast_sd_value_(imag, im));
			return 0;

		case ComplexType_double:
			Return(cast_dd_value_(real, re));
			Return(cast_dd_value_(imag, im));
			return 0;

		case ComplexType_long:
			Return(cast_ld_value_(real, re));
			Return(cast_ld_value_(imag, im));
			return 0;

		case ComplexType_rational:
			Return(double_float_rational_(real, re));
			Return(double_float_rational_(imag, im));
			return 0;

		case ComplexType_error:
		default:
			*re = 0.0;
			*im = 0.0;
			return TypeError_(pos, COMPLEX);
	}
}

_g int long_float_complex_(addr pos, long_float *re, long_float *im)
{
	addr real, imag;

	CheckType(pos, LISPTYPE_COMPLEX);
	switch (getcomplexr(pos, &real, &imag)) {
		case ComplexType_single:
			Return(cast_sl_value_(real, re));
			Return(cast_sl_value_(imag, im));
			return 0;

		case ComplexType_double:
			Return(cast_dl_value_(real, re));
			Return(cast_dl_value_(imag, im));
			return 0;

		case ComplexType_long:
			Return(cast_ll_value_(real, re));
			Return(cast_ll_value_(imag, im));
			return 0;

		case ComplexType_rational:
			Return(long_float_rational_(real, re));
			Return(long_float_rational_(imag, im));
			return 0;

		case ComplexType_error:
		default:
			*re = 0.0L;
			*im = 0.0L;
			return TypeError_(pos, COMPLEX);
	}
}

static int zerop_call_complex_(addr pos, int *ret, int (*call)(addr))
{
	addr value;

	GetRealComplex(pos, &value);
	if (! (*call)(value))
		return Result(ret, 0);

	GetImagComplex(pos, &value);
	return Result(ret, (*call)(value));
}

static int zerop_rational_complex_(addr pos, int *ret)
{
	addr value;

	GetRealComplex(pos, &value);
	Return(zerop_rational_(value, ret));
	if (*ret == 0)
		return 0;

	GetImagComplex(pos, &value);
	return zerop_rational_(value, ret);
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
			return zerop_rational_complex_(pos, ret);

		default:
			*ret = 0;
			return TypeError_(pos, COMPLEX);
	}
}

_g int eql_complex(addr left, addr right)
{
	addr value1, value2;

	Check(GetType(left) != LISPTYPE_COMPLEX, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetRealComplex(left, &value1);
	GetRealComplex(right, &value2);
	if (! eql_function(value1, value2))
		return 0;
	GetImagComplex(left, &value1);
	GetImagComplex(right, &value2);

	return eql_function(value1, value2);
}

_g int equal_complex_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	addr value1, value2;

	Check(GetType(left) != LISPTYPE_COMPLEX, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetRealComplex(left, &value1);
	GetRealComplex(right, &value2);
	Return(equal_real_(local, value1, value2, &check));
	if (! check)
		return Result(ret, 0);
	GetImagComplex(left, &value1);
	GetImagComplex(right, &value2);

	return equal_real_(local, value1, value2, ret);
}

_g int equal_fc_number_(addr left, addr right, int *ret)
{
	int check;
	addr value;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetImagComplex(right, &value);
	Return(zerop_real_(value, &check));
	if (! check)
		return Result(ret, 0);
	GetRealComplex(right, &value);

	return equal_fixnum_real_(left, value, ret);
}

_g int equal_bc_number_(addr left, addr right, int *ret)
{
	int check;
	addr value;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetImagComplex(right, &value);
	Return(zerop_real_(value, &check));
	if (! check)
		return Result(ret, 0);
	GetRealComplex(right, &value);

	return equal_bignum_real_(left, value, ret);
}

_g int equal_rc_number_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	addr value;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetImagComplex(right, &value);
	Return(zerop_real_(value, &check));
	if (! check)
		return Result(ret, 0);
	GetRealComplex(right, &value);

	return equal_ratio_real_(local, left, value, ret);
}

_g int equal_sc_number_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	addr value;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetImagComplex(right, &value);
	Return(zerop_real_(value, &check));
	if (! check)
		return Result(ret, 0);
	GetRealComplex(right, &value);

	return equal_single_float_real_(local, left, value, ret);
}

_g int equal_dc_number_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	addr value;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetImagComplex(right, &value);
	Return(zerop_real_(value, &check));
	if (! check)
		return Result(ret, 0);
	GetRealComplex(right, &value);

	return equal_double_float_real_(local, left, value, ret);
}

_g int equal_lc_number_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	addr value;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetImagComplex(right, &value);
	Return(zerop_real_(value, &check));
	if (! check)
		return Result(ret, 0);
	GetRealComplex(right, &value);

	return equal_long_float_real_(local, left, value, ret);
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
			Return(sign_reverse_rational_common_(real, &real));
			Return(sign_reverse_rational_common_(imag, &imag));
			break;

		default:
			*ret = Nil;
			return TypeError_(pos, COMPLEX);
	}

	return complex_unsafe_heap_(ret, real, imag, type);
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
			return single_float_check_heap_(ret, vf);

		case ComplexType_double:
			cabs_d(RefDoubleFloat(real), RefDoubleFloat(imag), &vd);
			return double_float_check_heap_(ret, vd);

		case ComplexType_long:
			cabs_l(RefLongFloat(real), RefLongFloat(imag), &vl);
			return long_float_check_heap_(ret, vl);

		case ComplexType_rational:
			Return(single_float_rational_(real, &vf));
			Return(single_float_rational_(imag, &v2));
			return single_float_check_heap_(ret, sqrtf(vf*vf + v2+v2));

		default:
			*ret = Nil;
			return TypeError_(pos, COMPLEX);
	}
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
		return single_float_check_heap_(ret, denom);
	else
		return complex_single_heap_(ret, real/denom, imag/denom);
}

static int signum_complex_double_(addr pos, addr *ret)
{
	double_float real, imag, denom;

	Return(double_float_complex_(pos, &real, &imag));
	cabs_d(real, imag, &denom);
	if (denom == 0.0f)
		return double_float_check_heap_(ret, denom);
	else
		return complex_double_heap_(ret, real/denom, imag/denom);
}

static int signum_complex_long_(addr pos, addr *ret)
{
	long_float real, imag, denom;

	Return(long_float_complex_(pos, &real, &imag));
	cabs_l(real, imag, &denom);
	if (denom == 0.0L)
		return long_float_check_heap_(ret, denom);
	else
		return complex_long_heap_(ret, real/denom, imag/denom);
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

