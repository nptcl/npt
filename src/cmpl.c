#include "bignum.h"
#include "cmpl.h"
#include "condition.h"
#include "equal.h"
#include "heap.h"
#include "integer.h"
#include "lisp.h"
#include "number.h"
#include "ratio.h"
#include "rational.h"
#include "real.h"
#include "real_float.h"
#include "type.h"

/*
 *  rational complex  -> #c(1/2 3)
 *  float complex     -> #c(0.5 3.0)
 */
#define real_complex_p(x) (GetUser(x))
#define rational_complex_p(x) (GetUser(x) == 0)

void make_complex_alloc(LocalRoot local, addr *ret)
{
	alloc_array2(local, ret, LISPTYPE_COMPLEX, 2);
}

static void complex_value(LocalRoot local,
		addr *ret, addr real, addr imag, int type)
{
	addr pos;

	if (zerop_real(imag)) {
		real_result_alloc(local, real, ret);
	}
	else {
		make_complex_alloc(local, &pos);
		SetUser(pos, type); /* 0:rational, 1:float */
		SetRealComplex(pos, real);
		SetImagComplex(pos, imag);
		*ret = pos;
	}
}

static void complex_fixnum(LocalRoot local, addr *ret, addr real, addr imag)
{
	switch (GetType(imag)) {
		case LISPTYPE_FIXNUM:
			fixnum_throw_alloc(local, real, &real);
			fixnum_throw_alloc(local, imag, &imag);
			complex_value(local, ret, real, imag, 0);
			break;

		case LISPTYPE_BIGNUM:
			fixnum_throw_alloc(local, real, &real);
			bignum_throw_alloc(local, imag, &imag);
			complex_value(local, ret, real, imag, 0);
			break;

		case LISPTYPE_RATIO:
			fixnum_throw_alloc(local, real, &real);
			ratio_throw_alloc(local, imag, &imag);
			complex_value(local, ret, real, imag, 0);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			single_float_alloc(local, &real, (single_float)RefFixnum(real));
			single_float_throw_alloc(local, imag, &imag);
			complex_value(local, ret, real, imag, 1);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			double_float_fixnum_alloc(local, &real, real);
			double_float_throw_alloc(local, imag, &imag);
			complex_value(local, ret, real, imag, 1);
			break;

		case LISPTYPE_LONG_FLOAT:
			long_float_fixnum_alloc(local, &real, real);
			long_float_throw_alloc(local, imag, &imag);
			complex_value(local, ret, real, imag, 1);
			break;

		default:
			TypeError(imag, REAL);
			break;
	}
}

static void complex_bignum(LocalRoot local, addr *ret, addr real, addr imag)
{
	switch (GetType(imag)) {
		case LISPTYPE_FIXNUM:
			bignum_throw_alloc(local, real, &real);
			fixnum_throw_alloc(local, imag, &imag);
			complex_value(local, ret, real, imag, 0);
			break;

		case LISPTYPE_BIGNUM:
			bignum_throw_alloc(local, real, &real);
			bignum_throw_alloc(local, imag, &imag);
			complex_value(local, ret, real, imag, 0);
			break;

		case LISPTYPE_RATIO:
			bignum_throw_alloc(local, real, &real);
			ratio_throw_alloc(local, imag, &imag);
			complex_value(local, ret, real, imag, 0);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			single_float_bignum_alloc(local, &real, real);
			single_float_throw_alloc(local, imag, &imag);
			complex_value(local, ret, real, imag, 1);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			double_float_bignum_alloc(local, &real, real);
			double_float_throw_alloc(local, imag, &imag);
			complex_value(local, ret, real, imag, 1);
			break;

		case LISPTYPE_LONG_FLOAT:
			long_float_bignum_alloc(local, &real, real);
			long_float_throw_alloc(local, imag, &imag);
			complex_value(local, ret, real, imag, 1);
			break;

		default:
			TypeError(imag, REAL);
			break;
	}
}

static void complex_ratio(LocalRoot local, addr *ret, addr real, addr imag)
{
	switch (GetType(imag)) {
		case LISPTYPE_FIXNUM:
			ratio_throw_alloc(local, real, &real);
			fixnum_throw_alloc(local, imag, &imag);
			complex_value(local, ret, real, imag, 0);
			break;

		case LISPTYPE_BIGNUM:
			ratio_throw_alloc(local, real, &real);
			bignum_throw_alloc(local, imag, &imag);
			complex_value(local, ret, real, imag, 0);
			break;

		case LISPTYPE_RATIO:
			ratio_throw_alloc(local, real, &real);
			ratio_throw_alloc(local, imag, &imag);
			complex_value(local, ret, real, imag, 0);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			single_float_alloc(local, &real, single_float_ratio(real));
			single_float_throw_alloc(local, imag, &imag);
			complex_value(local, ret, real, imag, 1);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			double_float_alloc(local, &real, double_float_ratio(real));
			double_float_throw_alloc(local, imag, &imag);
			complex_value(local, ret, real, imag, 1);
			break;

		case LISPTYPE_LONG_FLOAT:
			long_float_alloc(local, &real, long_float_ratio(real));
			long_float_throw_alloc(local, imag, &imag);
			complex_value(local, ret, real, imag, 1);
			break;

		default:
			TypeError(imag, REAL);
			break;
	}
}

static void complex_single(LocalRoot local, addr *ret, addr real, addr imag)
{
	switch (GetType(imag)) {
		case LISPTYPE_FIXNUM:
			single_float_throw_alloc(local, real, &real);
			single_float_alloc(local, &imag, (single_float)RefFixnum(imag));
			break;

		case LISPTYPE_BIGNUM:
			single_float_throw_alloc(local, real, &real);
			single_float_bignum_alloc(local, &imag, imag);
			break;

		case LISPTYPE_RATIO:
			single_float_throw_alloc(local, real, &real);
			single_float_alloc(local, &imag, single_float_ratio(imag));
			break;

		case LISPTYPE_SINGLE_FLOAT:
			single_float_throw_alloc(local, real, &real);
			single_float_throw_alloc(local, imag, &imag);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			double_float_alloc(local, &real, (double_float)RefSingleFloat(real));
			double_float_throw_alloc(local, imag, &imag);
			break;

		case LISPTYPE_LONG_FLOAT:
			long_float_alloc(local, &real, (long_float)RefSingleFloat(real));
			long_float_throw_alloc(local, imag, &imag);
			break;

		default:
			TypeError(imag, REAL);
			break;
	}
	complex_value(local, ret, real, imag, 1);
}

static void complex_double(LocalRoot local, addr *ret, addr real, addr imag)
{
	switch (GetType(imag)) {
		case LISPTYPE_FIXNUM:
			double_float_throw_alloc(local, real, &real);
			double_float_fixnum_alloc(local, &imag, imag);
			break;

		case LISPTYPE_BIGNUM:
			double_float_throw_alloc(local, real, &real);
			double_float_bignum_alloc(local, &imag, imag);
			break;

		case LISPTYPE_RATIO:
			double_float_throw_alloc(local, real, &real);
			double_float_alloc(local, &imag, double_float_ratio(imag));
			break;

		case LISPTYPE_SINGLE_FLOAT:
			double_float_throw_alloc(local, real, &real);
			double_float_alloc(local, &imag, (double_float)RefSingleFloat(imag));
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			double_float_throw_alloc(local, real, &real);
			double_float_throw_alloc(local, imag, &imag);
			break;

		case LISPTYPE_LONG_FLOAT:
			long_float_alloc(local, &real, (long_float)RefDoubleFloat(real));
			long_float_throw_alloc(local, imag, &imag);
			break;

		default:
			TypeError(imag, REAL);
			break;
	}
	complex_value(local, ret, real, imag, 1);
}

static void complex_long(LocalRoot local, addr *ret, addr real, addr imag)
{
	switch (GetType(imag)) {
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

		default:
			TypeError(imag, REAL);
			break;
	}
	complex_value(local, ret, real, imag, 1);
}

void complex_alloc(LocalRoot local, addr *ret, addr real, addr imag)
{
	switch (GetType(real)) {
		case LISPTYPE_FIXNUM:
			complex_fixnum(local, ret, real, imag);
			break;

		case LISPTYPE_BIGNUM:
			complex_bignum(local, ret, real, imag);
			break;

		case LISPTYPE_RATIO:
			complex_ratio(local, ret, real, imag);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			complex_single(local, ret, real, imag);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			complex_double(local, ret, real, imag);
			break;

		case LISPTYPE_LONG_FLOAT:
			complex_long(local, ret, real, imag);
			break;

		default:
			TypeError(real, REAL);
			break;
	}
}
void complex_local(LocalRoot local, addr *ret, addr real, addr imag)
{
	Check(local == NULL, "local error");
	complex_alloc(local, ret, real, imag);
}
void complex_heap(addr *ret, addr real, addr imag)
{
	complex_alloc(NULL, ret, real, imag);
}

void complex_copy_alloc(LocalRoot local, addr *ret, addr pos)
{
	addr real, imag;

	Check(GetType(pos) != LISPTYPE_COMPLEX, "type error");
	GetRealComplex(pos, &real);
	GetImagComplex(pos, &imag);
	complex_value(local, ret, real, imag, GetUser(pos));
}
void complex_copy_local(LocalRoot local, addr *ret, addr pos)
{
	Check(local == NULL, "local error");
	complex_copy_alloc(local, ret, pos);
}
void complex_copy_heap(addr *ret, addr pos)
{
	complex_copy_alloc(NULL, ret, pos);
}

int complexp(addr pos)
{
	return GetType(pos) == LISPTYPE_COMPLEX;
}

int complex_result_alloc(LocalRoot local, addr pos, addr *ret)
{
	addr check;

	if (GetType(pos) != LISPTYPE_COMPLEX)
		return real_result_alloc(local, pos, ret);
	GetImagComplex(pos, &check);
	if (zerop_real(check)) {
		GetRealComplex(pos, &check);
		return rational_result_alloc(local, check, ret);
	}
	*ret = pos;
	return 0;
}

void complex_throw_alloc(LocalRoot local, addr pos, addr *ret)
{
	Check(GetType(pos) != LISPTYPE_COMPLEX, "type error");
	if (GetStatusDynamic(pos))
		complex_copy_heap(ret, pos);
	else
		*ret = pos;
}

void complex_throw_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	complex_throw_alloc(local, pos, ret);
}

void complex_throw_heap(addr pos, addr *ret)
{
	complex_throw_alloc(NULL, pos, ret);
}

int zerop_complex(addr pos)
{
	addr check;

	Check(GetType(pos) != LISPTYPE_COMPLEX, "type error");
	GetRealComplex(pos, &check);
	if (! zerop_real(check)) return 0;
	GetImagComplex(pos, &check);

	return zerop_real(check);
}

int eql_complex(addr left, addr right)
{
	addr real1, imag1, real2, imag2;

	Check(GetType(left) != LISPTYPE_COMPLEX, "type left error");
	GetRealComplex(left, &real1);
	GetImagComplex(left, &imag1);
	if (rationalp(right) && rationalp(imag1) && zerop_rational(imag1))
		return eql_function(real1, right);
	if (GetType(right) != LISPTYPE_COMPLEX) return 0;
	GetRealComplex(right, &real2);
	GetImagComplex(right, &imag2);

	return eql_function(real1, real2) && eql_function(imag1, imag2);
}

int equal_complex(LocalRoot local, addr left, addr right)
{
	addr check1, check2;

	Check(GetType(left) != LISPTYPE_COMPLEX, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetRealComplex(left, &check1);
	GetRealComplex(right, &check2);
	if (! equal_number(local, check1, check2)) return 0;
	GetImagComplex(left, &check1);
	GetImagComplex(right, &check2);

	return equal_number(local, check1, check2);
}

int equal_fc_number(addr left, addr right)
{
	addr check;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetImagComplex(right, &check);
	if (! zerop_real(check)) return 0;
	GetRealComplex(right, &check);

	return equal_fixnum_real(left, right);
}

int equal_bc_number(addr left, addr right)
{
	addr check;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetImagComplex(right, &check);
	if (! zerop_real(check)) return 0;
	GetRealComplex(right, &check);

	return equal_bignum_real(left, right);
}

int equal_rc_number(LocalRoot local, addr left, addr right)
{
	addr check;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetImagComplex(right, &check);
	if (! zerop_real(check)) return 0;
	GetRealComplex(right, &check);

	return equal_ratio_real(local, left, right);
}

int equal_sc_number(LocalRoot local, addr left, addr right)
{
	addr check;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetImagComplex(right, &check);
	if (! zerop_real(check)) return 0;
	GetRealComplex(right, &check);

	return equal_single_float_real(local, left, right);
}

int equal_dc_number(LocalRoot local, addr left, addr right)
{
	addr check;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetImagComplex(right, &check);
	if (! zerop_real(check)) return 0;
	GetRealComplex(right, &check);

	return equal_double_float_real(local, left, right);
}

int equal_lc_number(LocalRoot local, addr left, addr right)
{
	addr check;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetImagComplex(right, &check);
	if (! zerop_real(check)) return 0;
	GetRealComplex(right, &check);

	return equal_long_float_real(local, left, right);
}

void oneplus_complex_heap(LocalRoot local, addr pos, addr *ret)
{
	addr real, imag;

	Check(GetType(pos) != LISPTYPE_COMPLEX, "type error");
	GetRealComplex(pos, &real);
	GetImagComplex(pos, &imag);
	oneplus_real_common(local, real, &real);
	complex_value(NULL, ret, real, imag, GetUser(pos));
}

void oneminus_complex_heap(LocalRoot local, addr pos, addr *ret)
{
	addr real, imag;

	Check(GetType(pos) != LISPTYPE_COMPLEX, "type error");
	GetRealComplex(pos, &real);
	GetImagComplex(pos, &imag);
	oneminus_real_common(local, real, &real);
	complex_value(NULL, ret, real, imag, GetUser(pos));
}

void sign_reverse_complex_common(addr pos, addr *ret)
{
	addr real, imag;

	Check(GetType(pos) != LISPTYPE_COMPLEX, "type error");
	GetRealComplex(pos, &real);
	GetImagComplex(pos, &imag);
	sign_reverse_real_common(real, &real);
	sign_reverse_real_common(imag, &imag);
	complex_value(NULL, ret, real, imag, GetUser(pos));
}

void sign_reverse_complex_local(LocalRoot local, addr pos, addr *ret)
{
	addr real, imag;

	Check(local == NULL, "local error");
	Check(GetType(pos) != LISPTYPE_COMPLEX, "type error");
	GetRealComplex(pos, &real);
	GetImagComplex(pos, &imag);
	sign_reverse_real_local(local, real, &real);
	sign_reverse_real_local(local, imag, &imag);
	complex_value(local, ret, real, imag, GetUser(pos));
}


/*
 *  plus
 */
void plus_fc_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	addr real, imag;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetRealComplex(right, &real);
	GetImagComplex(right, &imag);
	plus_fixnum_real_common(local, left, real, &real);
	complex_heap(ret, real, imag);
}

void plus_bc_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	addr real, imag;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetRealComplex(right, &real);
	GetImagComplex(right, &imag);
	plus_bignum_real_common(local, left, real, &real);
	complex_heap(ret, real, imag);
}

void plus_rc_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	addr real, imag;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetRealComplex(right, &real);
	GetImagComplex(right, &imag);
	plus_ratio_real_common(local, left, real, &real);
	complex_heap(ret, real, imag);
}

void plus_sc_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	addr real, imag;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetRealComplex(right, &real);
	GetImagComplex(right, &imag);
	plus_single_real_common(left, real, &real);
	complex_heap(ret, real, imag);
}

void plus_dc_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	addr real, imag;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetRealComplex(right, &real);
	GetImagComplex(right, &imag);
	plus_double_real_common(left, real, &real);
	complex_heap(ret, real, imag);
}

void plus_lc_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	addr real, imag;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetRealComplex(right, &real);
	GetImagComplex(right, &imag);
	plus_long_real_common(left, real, &real);
	complex_heap(ret, real, imag);
}

void plus_cc_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	addr real1, imag1, real2, imag2;

	Check(GetType(left) != LISPTYPE_COMPLEX, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetRealComplex(left, &real1);
	GetImagComplex(left, &imag1);
	GetRealComplex(right, &real2);
	GetImagComplex(right, &imag2);
	plus_real_common(local, real1, real2, &real1);
	plus_real_common(local, imag1, imag2, &imag1);
	complex_heap(ret, real1, imag1);
}


/*
 *  minus
 */
void minus_fc_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	addr real, imag;

	/* left - (real + i*imag) = left - real - i*imag */
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetRealComplex(right, &real);
	GetImagComplex(right, &imag);
	minus_fixnum_real_common(local, left, real, &real);
	sign_reverse_real_common(imag, &imag);
	complex_heap(ret, real, imag);
}

void minus_cf_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	addr real, imag;

	/* real + i*imag - right = real - right + i*imag */
	Check(GetType(left) != LISPTYPE_COMPLEX, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");
	GetRealComplex(left, &real);
	GetImagComplex(left, &imag);
	minus_real_fixnum_common(local, real, right, &real);
	complex_heap(ret, real, imag);
}

void minus_bc_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	addr real, imag;

	/* left - (real + i*imag) = left - real - i*imag */
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetRealComplex(right, &real);
	GetImagComplex(right, &imag);
	minus_bignum_real_common(local, left, real, &real);
	sign_reverse_real_common(imag, &imag);
	complex_heap(ret, real, imag);
}

void minus_cb_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	addr real, imag;

	/* real + i*imag - right = real - right + i*imag */
	Check(GetType(left) != LISPTYPE_COMPLEX, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	GetRealComplex(left, &real);
	GetImagComplex(left, &imag);
	minus_real_bignum_common(local, real, right, &real);
	complex_heap(ret, real, imag);
}

void minus_rc_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	addr real, imag;

	/* left - (real + i*imag) = left - real - i*imag */
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetRealComplex(right, &real);
	GetImagComplex(right, &imag);
	minus_ratio_real_common(local, left, real, &real);
	sign_reverse_real_common(imag, &imag);
	complex_heap(ret, real, imag);
}

void minus_cr_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	addr real, imag;

	/* real + i*imag - right = real - right + i*imag */
	Check(GetType(left) != LISPTYPE_COMPLEX, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	GetRealComplex(left, &real);
	GetImagComplex(left, &imag);
	minus_real_ratio_common(local, real, right, &real);
	complex_heap(ret, real, imag);
}

void minus_sc_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	addr real, imag;

	/* left - (real + i*imag) = left - real - i*imag */
	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetRealComplex(right, &real);
	GetImagComplex(right, &imag);
	minus_single_real_common(local, left, real, &real);
	sign_reverse_real_common(imag, &imag);
	complex_heap(ret, real, imag);
}

void minus_cs_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	addr real, imag;

	/* real + i*imag - right = real - right + i*imag */
	Check(GetType(left) != LISPTYPE_COMPLEX, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	GetRealComplex(left, &real);
	GetImagComplex(left, &imag);
	minus_real_single_common(local, real, right, &real);
	complex_heap(ret, real, imag);
}

void minus_dc_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	addr real, imag;

	/* left - (real + i*imag) = left - real - i*imag */
	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetRealComplex(right, &real);
	GetImagComplex(right, &imag);
	minus_double_real_common(local, left, real, &real);
	sign_reverse_real_common(imag, &imag);
	complex_heap(ret, real, imag);
}

void minus_cd_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	addr real, imag;

	/* real + i*imag - right = real - right + i*imag */
	Check(GetType(left) != LISPTYPE_COMPLEX, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	GetRealComplex(left, &real);
	GetImagComplex(left, &imag);
	minus_real_double_common(local, real, right, &real);
	complex_heap(ret, real, imag);
}

void minus_lc_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	addr real, imag;

	/* left - (real + i*imag) = left - real - i*imag */
	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetRealComplex(right, &real);
	GetImagComplex(right, &imag);
	minus_long_real_common(local, left, real, &real);
	sign_reverse_real_common(imag, &imag);
	complex_heap(ret, real, imag);
}

void minus_cl_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	addr real, imag;

	/* real + i*imag - right = real - right + i*imag */
	Check(GetType(left) != LISPTYPE_COMPLEX, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	GetRealComplex(left, &real);
	GetImagComplex(left, &imag);
	minus_real_long_common(local, real, right, &real);
	complex_heap(ret, real, imag);
}

void minus_cc_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	addr real1, imag1, real2, imag2;

	Check(GetType(left) != LISPTYPE_COMPLEX, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetRealComplex(left, &real1);
	GetImagComplex(left, &imag1);
	GetRealComplex(right, &real2);
	GetImagComplex(right, &imag2);
	minus_real_common(local, real1, real2, &real1);
	minus_real_common(local, imag1, imag2, &imag1);
	complex_heap(ret, real1, imag1);
}


/*
 *  multiple
 */
void multi_fc_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	addr real, imag;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetRealComplex(right, &real);
	GetImagComplex(right, &imag);
	multi_fixnum_real_common(local, left, real, &real);
	multi_fixnum_real_common(local, left, imag, &imag);
	complex_heap(ret, real, imag);
}

void multi_bc_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	addr real, imag;

	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetRealComplex(right, &real);
	GetImagComplex(right, &imag);
	multi_bignum_real_common(local, left, real, &real);
	multi_bignum_real_common(local, left, imag, &imag);
	complex_heap(ret, real, imag);
}

void multi_rc_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	addr real, imag;

	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetRealComplex(right, &real);
	GetImagComplex(right, &imag);
	multi_ratio_real_common(local, left, real, &real);
	multi_ratio_real_common(local, left, imag, &imag);
	complex_heap(ret, real, imag);
}

void multi_sc_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	addr real, imag;

	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetRealComplex(right, &real);
	GetImagComplex(right, &imag);
	multi_single_real_common(local, left, real, &real);
	complex_heap(ret, real, imag);
}

void multi_dc_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	addr real, imag;

	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetRealComplex(right, &real);
	GetImagComplex(right, &imag);
	multi_double_real_common(local, left, real, &real);
	complex_heap(ret, real, imag);
}

void multi_lc_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	addr real, imag;

	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetRealComplex(right, &real);
	GetImagComplex(right, &imag);
	multi_long_real_common(local, left, real, &real);
	complex_heap(ret, real, imag);
}

void multi_cc_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	LocalStack stack;
	addr a, b, c, d, ac, bd, ad, bc;

	/* (a + bi)(c + di) = ac - bd + (ad + bc)i */
	Check(GetType(left) != LISPTYPE_COMPLEX, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	push_local(local, &stack);
	GetRealComplex(left, &a);
	GetImagComplex(left, &b);
	GetRealComplex(right, &c);
	GetImagComplex(right, &d);
	multi_real_local(local, a, c, &ac);
	multi_real_local(local, b, d, &bd);
	multi_real_local(local, a, d, &ad);
	multi_real_local(local, b, c, &bc);
	minus_real_local(local, ac, bd, &a);
	plus_real_local(local, ad, bc, &b);
	complex_heap(ret, a, b);
	rollback_local(local, stack);
}

void inverse_complex_common(LocalRoot local, addr pos, addr *ret)
{
	LocalStack stack;
	addr a, b, a2, b2, ab;

	/* Re:  a/(a*a + b*b)
	 * Im: -b/(a*a + b*b)
	 */
	Check(local == NULL, "local error");
	CheckType(pos, LISPTYPE_COMPLEX);
	if (zerop_complex(pos))
		division_by_zero1(pos);
	GetRealComplex(pos, &a);
	GetImagComplex(pos, &b);
	push_local(local, &stack);
	multi_real_local(local, a, a, &a2);
	multi_real_local(local, b, b, &b2);
	plus_real_local(local, a2, b2, &ab);
	div_real_local(local, a, ab, &a);
	div_real_local(local, b, ab, &b);
	sign_reverse_real_local(local, b, &b);
	complex_heap(ret, a, b);
	rollback_local(local, stack);
}

void div_fc_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	LocalStack stack;
	addr a, b, a2, b2, ab;

	/* Re:  n*a/(a*a + b*b)
	 * Im: -n*b/(a*a + b*b)
	 */
	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	if (zerop_complex(right))
		division_by_zero2(left, right);
	GetRealComplex(right, &a);
	GetImagComplex(right, &b);
	push_local(local, &stack);
	multi_real_local(local, a, a, &a2);
	multi_real_local(local, b, b, &b2);
	plus_real_local(local, a2, b2, &ab);
	multi_fixnum_real_local(local, left, a, &a);
	multi_fixnum_real_local(local, left, b, &b);
	div_real_local(local, a, ab, &a);
	div_real_local(local, b, ab, &b);
	sign_reverse_real_local(local, b, &b);
	complex_heap(ret, a, b);
	rollback_local(local, stack);
}

void div_cf_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	LocalStack stack;
	addr a, b;

	/* Re: n*a
	 * Im: n*b
	 */
	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_COMPLEX, "type left error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");
	if (zerop_fixnum(right))
		division_by_zero2(left, right);
	GetRealComplex(left, &a);
	GetImagComplex(left, &b);
	push_local(local, &stack);
	div_real_local(local, a, right, &a);
	div_real_local(local, b, right, &b);
	complex_heap(ret, a, b);
	rollback_local(local, stack);
}

void div_bc_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	LocalStack stack;
	addr a, b, a2, b2, ab;

	/* Re:  n*a/(a*a + b*b)
	 * Im: -n*b/(a*a + b*b)
	 */
	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_BIGNUM, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	if (zerop_complex(right))
		division_by_zero2(left, right);
	GetRealComplex(right, &a);
	GetImagComplex(right, &b);
	push_local(local, &stack);
	multi_real_local(local, a, a, &a2);
	multi_real_local(local, b, b, &b2);
	plus_real_local(local, a2, b2, &ab);
	multi_bignum_real_local(local, left, a, &a);
	multi_bignum_real_local(local, left, b, &b);
	div_real_local(local, a, ab, &a);
	div_real_local(local, b, ab, &b);
	sign_reverse_real_local(local, b, &b);
	complex_heap(ret, a, b);
	rollback_local(local, stack);
}

void div_cb_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	LocalStack stack;
	addr a, b;

	/* Re: n*a
	 * Im: n*b
	 */
	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_COMPLEX, "type left error");
	Check(GetType(right) != LISPTYPE_BIGNUM, "type right error");
	if (zerop_bignum(right))
		division_by_zero2(left, right);
	GetRealComplex(left, &a);
	GetImagComplex(left, &b);
	push_local(local, &stack);
	div_real_local(local, a, right, &a);
	div_real_local(local, b, right, &b);
	complex_heap(ret, a, b);
	rollback_local(local, stack);
}

void div_rc_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	LocalStack stack;
	addr x, y, a, b, a2, b2, ab;

	/* (y/x) / (a+ib) =
	 *   Re:  y*a/[x*(a*a + b*b)]
	 *   Im: -y*b/[x*(a*a + b*b)]
	 */
	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_RATIO, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	if (zerop_complex(right))
		division_by_zero2(left, right);
	GetNumerRatio(left, &y);
	GetDenomRatio(left, &x);
	GetRealComplex(right, &a);
	GetImagComplex(right, &b);
	push_local(local, &stack);
	multi_real_local(local, a, a, &a2);
	multi_real_local(local, b, b, &b2);
	plus_real_local(local, a2, b2, &ab);
	multi_real_local(local, x, ab, &ab);
	multi_bignum_real_local(local, y, a, &a);
	multi_bignum_real_local(local, y, b, &b);
	div_real_local(local, a, ab, &a);
	div_real_local(local, b, ab, &b);
	sign_reverse_real_local(local, b, &b);
	complex_heap(ret, a, b);
	rollback_local(local, stack);
}

void div_cr_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	LocalStack stack;
	addr x, y, a, b;

	/* (a+ib) / (y/x) =
	 *   Re: a*x/y
	 *   Im: b*x/y
	 */
	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_COMPLEX, "type left error");
	Check(GetType(right) != LISPTYPE_RATIO, "type right error");
	if (zerop_ratio(right))
		division_by_zero2(left, right);
	GetRealComplex(left, &a);
	GetImagComplex(left, &b);
	GetNumerRatio(right, &y);
	GetDenomRatio(right, &x);
	push_local(local, &stack);
	multi_real_local(local, a, x, &a);
	multi_real_local(local, b, x, &b);
	div_real_local(local, a, y, &a);
	div_real_local(local, b, y, &b);
	complex_heap(ret, a, b);
	rollback_local(local, stack);
}

void div_sc_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	LocalStack stack;
	addr a, b, a2, b2, ab;

	/* Re:  n*a/(a*a + b*b)
	 * Im: -n*b/(a*a + b*b)
	 */
	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_SINGLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	if (zerop_complex(right))
		division_by_zero2(left, right);
	GetRealComplex(right, &a);
	GetImagComplex(right, &b);
	push_local(local, &stack);
	multi_real_local(local, a, a, &a2);
	multi_real_local(local, b, b, &b2);
	plus_real_local(local, a2, b2, &ab);
	multi_single_real_local(local, left, a, &a);
	multi_single_real_local(local, left, b, &b);
	div_real_local(local, a, ab, &a);
	div_real_local(local, b, ab, &b);
	sign_reverse_real_local(local, b, &b);
	complex_heap(ret, a, b);
	rollback_local(local, stack);
}

void div_cs_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	LocalStack stack;
	addr a, b;

	/* Re: n*a
	 * Im: n*b
	 */
	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_COMPLEX, "type left error");
	Check(GetType(right) != LISPTYPE_SINGLE_FLOAT, "type right error");
	if (zerop_single_float(right))
		division_by_zero2(left, right);
	GetRealComplex(left, &a);
	GetImagComplex(left, &b);
	push_local(local, &stack);
	multi_real_local(local, a, right, &a);
	multi_real_local(local, b, right, &b);
	complex_heap(ret, a, b);
	rollback_local(local, stack);
}

void div_dc_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	LocalStack stack;
	addr a, b, a2, b2, ab;

	/* Re:  n*a/(a*a + b*b)
	 * Im: -n*b/(a*a + b*b)
	 */
	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_DOUBLE_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	if (zerop_complex(right))
		division_by_zero2(left, right);
	GetRealComplex(right, &a);
	GetImagComplex(right, &b);
	push_local(local, &stack);
	multi_real_local(local, a, a, &a2);
	multi_real_local(local, b, b, &b2);
	plus_real_local(local, a2, b2, &ab);
	multi_double_real_local(local, left, a, &a);
	multi_double_real_local(local, left, b, &b);
	div_real_local(local, a, ab, &a);
	div_real_local(local, b, ab, &b);
	sign_reverse_real_local(local, b, &b);
	complex_heap(ret, a, b);
	rollback_local(local, stack);
}

void div_cd_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	LocalStack stack;
	addr a, b;

	/* Re: n*a
	 * Im: n*b
	 */
	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_COMPLEX, "type left error");
	Check(GetType(right) != LISPTYPE_DOUBLE_FLOAT, "type right error");
	if (zerop_double_float(right))
		division_by_zero2(left, right);
	GetRealComplex(right, &a);
	GetImagComplex(right, &b);
	push_local(local, &stack);
	multi_real_local(local, a, right, &a);
	multi_real_local(local, b, right, &b);
	complex_heap(ret, a, b);
	rollback_local(local, stack);
}

void div_lc_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	LocalStack stack;
	addr a, b, a2, b2, ab;

	/* Re:  n*a/(a*a + b*b)
	 * Im: -n*b/(a*a + b*b)
	 */
	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_LONG_FLOAT, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	if (zerop_complex(right))
		division_by_zero2(left, right);
	GetRealComplex(right, &a);
	GetImagComplex(right, &b);
	push_local(local, &stack);
	multi_real_local(local, a, a, &a2);
	multi_real_local(local, b, b, &b2);
	plus_real_local(local, a2, b2, &ab);
	multi_long_real_local(local, left, a, &a);
	multi_long_real_local(local, left, b, &b);
	div_real_local(local, a, ab, &a);
	div_real_local(local, b, ab, &b);
	sign_reverse_real_local(local, b, &b);
	complex_heap(ret, a, b);
	rollback_local(local, stack);
}

void div_cl_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	LocalStack stack;
	addr a, b;

	/* Re: n*a
	 * Im: n*b
	 */
	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_COMPLEX, "type left error");
	Check(GetType(right) != LISPTYPE_LONG_FLOAT, "type right error");
	if (zerop_long_float(right))
		division_by_zero2(left, right);
	GetRealComplex(right, &a);
	GetImagComplex(right, &b);
	push_local(local, &stack);
	multi_real_local(local, a, right, &a);
	multi_real_local(local, b, right, &b);
	complex_heap(ret, a, b);
	rollback_local(local, stack);
}

void div_cc_number_common(LocalRoot local, addr left, addr right, addr *ret)
{
	LocalStack stack;
	addr a, b, c, d, ac, bd, bc, ad, c2, d2;

	/* (a+bi)(c+di) =
	 *   Re: (ac + bd)/(c*c + d*d)
	 *   Im: (bc - ad)/(c*c + d*d)
	 */
	Check(local == NULL, "local error");
	Check(GetType(left) != LISPTYPE_COMPLEX, "type left error");
	Check(GetType(right) != LISPTYPE_COMPLEX, "type right error");
	GetRealComplex(left, &a);
	GetImagComplex(left, &b);
	GetRealComplex(right, &c);
	GetImagComplex(right, &d);
	push_local(local, &stack);
	multi_real_local(local, c, c, &c2);
	multi_real_local(local, d, d, &d2);
	multi_real_local(local, a, c, &ac);
	multi_real_local(local, b, d, &bd);
	multi_real_local(local, b, c, &bc);
	multi_real_local(local, a, d, &ad);
	plus_real_local(local, ac, bd, &ac);
	minus_real_local(local, bc, ad, &bc);
	plus_real_local(local, c2, d2, &c2);
	div_real_local(local, ac, c2, &ac);
	div_real_local(local, bc, c2, &bc);
	complex_heap(ret, ac, bc);
	rollback_local(local, stack);
}

void abs_complex_common(LocalRoot local, addr left, addr *ret)
{
	addr real, imag;
	LocalStack stack;

	CheckType(left, LISPTYPE_COMPLEX);
	GetRealComplex(left, &real);
	GetRealComplex(left, &imag);
	push_local(local, &stack);
	cast_float_local(local, real, &real);
	cast_float_local(local, imag, &imag);
	multi_float_local(local, real, real, &real);
	multi_float_local(local, imag, imag, &imag);
	plus_float_local(local, real, imag, &real);
	sqrt_float_heap(local, real, ret);
	rollback_local(local, stack);
}

