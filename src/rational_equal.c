#include "bignum_equal.h"
#include "condition.h"
#include "integer.h"
#include "ratio.h"
#include "ratio_equal.h"
#include "rational_equal.h"
#include "typedef.h"

/*
 *  rational
 */
int plusp_rational_(addr pos, int *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, plusp_fixnum(pos));

		case LISPTYPE_BIGNUM:
			return Result(ret, plusp_bignum(pos));

		case LISPTYPE_RATIO:
			return Result(ret, plusp_ratio(pos));

		default:
			*ret = 0;
			return TypeError_(pos, RATIONAL);
	}
}

int minusp_rational_(addr pos, int *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, minusp_fixnum(pos));

		case LISPTYPE_BIGNUM:
			return Result(ret, minusp_bignum(pos));

		case LISPTYPE_RATIO:
			return Result(ret, minusp_ratio(pos));

		default:
			*ret = 0;
			return TypeError_(pos, RATIONAL);
	}
}

int zerop_rational_(addr pos, int *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, zerop_fixnum(pos));

		case LISPTYPE_BIGNUM:
			return Result(ret, zerop_bignum(pos));

		case LISPTYPE_RATIO:
			return Result(ret, zerop_ratio(pos));

		default:
			*ret = 0;
			return TypeError_(pos, RATIONAL);
	}
}

static inline int equal_fixnum_rational_(addr left, addr right, int *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, equal_ff_real(left, right));

		case LISPTYPE_BIGNUM:
			return Result(ret, equal_fb_real(left, right));

		case LISPTYPE_RATIO:
			return Result(ret, equal_fr_real(left, right));

		default:
			*ret = 0;
			return TypeError_(right, RATIONAL);
	}
}

static inline int equal_bignum_rational_(addr left, addr right, int *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, equal_bf_real(left, right));

		case LISPTYPE_BIGNUM:
			return Result(ret, equal_bb_real(left, right));

		case LISPTYPE_RATIO:
			return Result(ret, equal_br_real(left, right));

		default:
			*ret = 0;
			return TypeError_(right, RATIONAL);
	}
}

static inline int equal_ratio_rational_(addr left, addr right, int *ret)
{
	CheckType(left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, equal_rf_real(left, right));

		case LISPTYPE_BIGNUM:
			return Result(ret, equal_rb_real(left, right));

		case LISPTYPE_RATIO:
			return Result(ret, equal_rr_real(left, right));

		default:
			*ret = 0;
			return TypeError_(right, RATIONAL);
	}
}

int equal_rational_(addr left, addr right, int *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return equal_fixnum_rational_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return equal_bignum_rational_(left, right, ret);

		case LISPTYPE_RATIO:
			return equal_ratio_rational_(left, right, ret);

		default:
			*ret = 0;
			return TypeError_(left, RATIONAL);
	}
}

int not_equal_rational_(addr left, addr right, int *ret)
{
	int check;
	Return(equal_rational_(left, right, &check));
	return Result(ret, ! check);
}

static inline int compare_fixnum_rational_(LocalRoot local,
		addr left, addr right, int *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, compare_ff_real(left, right));

		case LISPTYPE_BIGNUM:
			return Result(ret, compare_fb_real(left, right));

		case LISPTYPE_RATIO:
			return Result(ret, compare_fr_real(local, left, right));

		default:
			*ret = 0;
			return TypeError_(right, RATIONAL);
	}
}

static inline int compare_bignum_rational_(LocalRoot local,
		addr left, addr right, int *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, compare_bf_real(left, right));

		case LISPTYPE_BIGNUM:
			return Result(ret, compare_bb_real(left, right));

		case LISPTYPE_RATIO:
			return Result(ret, compare_br_real(local, left, right));

		default:
			*ret = 0;
			return TypeError_(right, RATIONAL);
	}
}

static inline int compare_ratio_rational_(LocalRoot local,
		addr left, addr right, int *ret)
{
	CheckType(left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return Result(ret, compare_rf_real(local, left, right));

		case LISPTYPE_BIGNUM:
			return Result(ret, compare_rb_real(local, left, right));

		case LISPTYPE_RATIO:
			return Result(ret, compare_rr_real(local, left, right));

		default:
			*ret = 0;
			return TypeError_(right, RATIONAL);
	}
}

int compare_rational_(LocalRoot local, addr left, addr right, int *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return compare_fixnum_rational_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return compare_bignum_rational_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return compare_ratio_rational_(local, left, right, ret);

		default:
			*ret = 0;
			return TypeError_(left, RATIONAL);
	}
}

int less_rational_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	Return(compare_rational_(local, left, right, &check));
	return Result(ret, check < 0);
}

int less_equal_rational_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	Return(compare_rational_(local, left, right, &check));
	return Result(ret, check <= 0);
}

int greater_rational_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	Return(compare_rational_(local, left, right, &check));
	return Result(ret, check > 0);
}

int greater_equal_rational_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	Return(compare_rational_(local, left, right, &check));
	return Result(ret, check >= 0);
}

int less_rational_debug(LocalRoot local, addr left, addr right)
{
	int check;
	check = 0;
	Error(less_rational_(local, left, right, &check));
	return check;
}

int less_equal_rational_debug(LocalRoot local, addr left, addr right)
{
	int check;
	check = 0;
	Error(less_equal_rational_(local, left, right, &check));
	return check;
}

