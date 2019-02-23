#include "bignum.h"
#include "condition.h"
#include "heap.h"
#include "integer.h"
#include "lisp.h"
#include "ratio.h"
#include "rational.h"

int rationalp(addr pos)
{
	enum LISPTYPE type = GetType(pos);
	return type == LISPTYPE_FIXNUM
		|| type == LISPTYPE_BIGNUM
		|| type == LISPTYPE_RATIO;
}

/*
 *  throw
 */
int rational_result_alloc(LocalRoot local, addr pos, addr *ret)
{
	if (GetType(pos) == LISPTYPE_RATIO) {
		*ret = pos;
		return 0;
	}
	return integer_result_alloc(local, pos, ret);
}

void rational_throw_alloc(LocalRoot local, addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			fixnum_throw_alloc(local, pos, ret);
			break;

		case LISPTYPE_BIGNUM:
			bignum_throw_alloc(local, pos, ret);
			break;

		case LISPTYPE_RATIO:
			ratio_throw_alloc(local, pos, ret);
			break;

		default:
			TypeError(pos, RATIONAL);
			break;
	}
}

void rational_throw_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	rational_throw_alloc(local, pos, ret);
}

void rational_throw_heap(addr pos, addr *ret)
{
	rational_throw_alloc(NULL, pos, ret);
}


/*
 *  rational
 */
int plusp_rational(addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return plusp_fixnum(pos);

		case LISPTYPE_BIGNUM:
			return plusp_bignum(pos);

		case LISPTYPE_RATIO:
			return plusp_ratio(pos);

		default:
			TypeError(pos, RATIONAL);
			return 0;
	}
}

int minusp_rational(addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return minusp_fixnum(pos);

		case LISPTYPE_BIGNUM:
			return minusp_bignum(pos);

		case LISPTYPE_RATIO:
			return minusp_ratio(pos);

		default:
			TypeError(pos, RATIONAL);
			return 0;
	}
}

int zerop_rational(addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return zerop_fixnum(pos);

		case LISPTYPE_BIGNUM:
			return zerop_bignum(pos);

		case LISPTYPE_RATIO:
			return zerop_ratio(pos);

		default:
			TypeError(pos, RATIONAL);
			return 0;
	}
}

static inline int equal_fixnum_rational(addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return equal_ff_real(left, right);

		case LISPTYPE_BIGNUM:
			return equal_fb_real(left, right);

		case LISPTYPE_RATIO:
			return equal_fr_real(left, right);

		default:
			TypeError(right, RATIONAL);
			return 0;
	}
}

static inline int equal_bignum_rational(addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return equal_bf_real(left, right);

		case LISPTYPE_BIGNUM:
			return equal_bb_real(left, right);

		case LISPTYPE_RATIO:
			return equal_br_real(left, right);

		default:
			TypeError(right, RATIONAL);
			return 0;
	}
}

static inline int equal_ratio_rational(addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return equal_rf_real(left, right);

		case LISPTYPE_BIGNUM:
			return equal_rb_real(left, right);

		case LISPTYPE_RATIO:
			return equal_rr_real(left, right);

		default:
			TypeError(right, RATIONAL);
			return 0;
	}
}

int equal_rational(addr left, addr right)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return equal_fixnum_rational(left, right);

		case LISPTYPE_BIGNUM:
			return equal_bignum_rational(left, right);

		case LISPTYPE_RATIO:
			return equal_ratio_rational(left, right);

		default:
			TypeError(left, RATIONAL);
			return 0;
	}
}

static inline int compare_fixnum_rational(LocalRoot local, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return compare_ff_real(left, right);

		case LISPTYPE_BIGNUM:
			return compare_fb_real(left, right);

		case LISPTYPE_RATIO:
			return compare_fr_real(local, left, right);

		default:
			TypeError(right, RATIONAL);
			return 0;
	}
}

static inline int compare_bignum_rational(LocalRoot local, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return compare_bf_real(left, right);

		case LISPTYPE_BIGNUM:
			return compare_bb_real(left, right);

		case LISPTYPE_RATIO:
			return compare_br_real(local, left, right);

		default:
			TypeError(right, RATIONAL);
			return 0;
	}
}

static inline int compare_ratio_rational(LocalRoot local, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return compare_rf_real(local, left, right);

		case LISPTYPE_BIGNUM:
			return compare_rb_real(local, left, right);

		case LISPTYPE_RATIO:
			return compare_rr_real(local, left, right);

		default:
			TypeError(right, RATIONAL);
			return 0;
	}
}

int compare_rational(LocalRoot local, addr left, addr right)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return compare_fixnum_rational(local, left, right);

		case LISPTYPE_BIGNUM:
			return compare_bignum_rational(local, left, right);

		case LISPTYPE_RATIO:
			return compare_ratio_rational(local, left, right);

		default:
			TypeError(left, RATIONAL);
			return 0;
	}
}

int less_rational_clang(LocalRoot local, addr left, addr right)
{
	return less_rational(local, left, right);
}

int less_equal_rational_clang(LocalRoot local, addr left, addr right)
{
	return less_equal_rational(local, left, right);
}

void plus_rational(LocalRoot local, addr left, addr right, addr *ret)
{
}

void minus_rational(LocalRoot local, addr left, addr right, addr *ret);
void multi_rational(LocalRoot local, addr left, addr right, addr *ret);
void division_rational(LocalRoot local, addr left, addr right, addr *ret);

