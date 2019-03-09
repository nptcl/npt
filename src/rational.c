#include "bignum.h"
#include "condition.h"
#include "heap.h"
#include "integer.h"
#include "lisp.h"
#include "ratio.h"
#include "rational.h"
#include "real_float.h"

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
void rational_result_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	if (rationalp(pos))
		ratio_result_noreduction_local(local, pos, ret);
	else
		integer_result_local(local, pos, ret);
}

void rational_result_heap(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	if (rationalp(pos))
		ratio_result_noreduction_heap(local, pos, ret);
	else
		integer_result_heap(pos, ret);
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

void rational_copy_alloc(LocalRoot local, addr pos, addr *ret)
{
	if (ratiop(pos))
		ratio_copy_alloc(local, ret, pos);
	else
		integer_copy_alloc(local, pos, ret);
}

void rational_copy_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	rational_copy_alloc(local, pos, ret);
}

void rational_copy_heap(addr pos, addr *ret)
{
	rational_copy_alloc(NULL, pos, ret);
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
	CheckType(left, LISPTYPE_FIXNUM);
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
	CheckType(left, LISPTYPE_BIGNUM);
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
	CheckType(left, LISPTYPE_RATIO);
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
	CheckType(left, LISPTYPE_FIXNUM);
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
	CheckType(left, LISPTYPE_BIGNUM);
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
	CheckType(left, LISPTYPE_RATIO);
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


/*
 *  float
 */
single_float single_float_rational(addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return single_float_fixnum(pos);

		case LISPTYPE_BIGNUM:
			return single_float_bignum(pos);

		case LISPTYPE_RATIO:
			return single_float_ratio(pos);

		default:
			TypeError(pos, RATIONAL);
			return 0.0f;
	}
}

double_float double_float_rational(addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return double_float_fixnum(pos);

		case LISPTYPE_BIGNUM:
			return double_float_bignum(pos);

		case LISPTYPE_RATIO:
			return double_float_ratio(pos);

		default:
			TypeError(pos, RATIONAL);
			return 0.0;
	}
}

long_float long_float_rational(addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return long_float_fixnum(pos);

		case LISPTYPE_BIGNUM:
			return long_float_bignum(pos);

		case LISPTYPE_RATIO:
			return long_float_ratio(pos);

		default:
			TypeError(pos, RATIONAL);
			return 0.0L;
	}
}


/*
 *  sign-reverse
 */
void sign_reverse_rational_common(addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			sigrev_fixnum_integer_common(pos, ret);
			break;

		case LISPTYPE_BIGNUM:
			sigrev_bignum_integer_common(pos, ret);
			break;

		case LISPTYPE_RATIO:
			sign_reverse_ratio_common(pos, ret);
			break;

		default:
			TypeError(pos, RATIONAL);
			break;
	}
}

void sign_reverse_rational_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			sigrev_fixnum_integer_local(local, pos, ret);
			break;

		case LISPTYPE_BIGNUM:
			sigrev_bignum_integer_local(local, pos, ret);
			break;

		case LISPTYPE_RATIO:
			sign_reverse_ratio_local(local, pos, ret);
			break;

		default:
			TypeError(pos, RATIONAL);
			break;
	}
}


/*
 *  oneplus
 */
void oneplus_rational_common(LocalRoot local, addr value, addr *ret)
{
	switch (GetType(value)) {
		case LISPTYPE_FIXNUM:
			plus_fv_real_common(value, 1, ret);
			break;

		case LISPTYPE_BIGNUM:
			plus_bv_real_common(local, value, 1, ret);
			break;

		case LISPTYPE_RATIO:
			plus_rv_real_common(local, value, 1, ret);
			break;

		default:
			TypeError(value, RATIONAL);
			break;
	}
}

void oneminus_rational_common(LocalRoot local, addr value, addr *ret)
{
	switch (GetType(value)) {
		case LISPTYPE_FIXNUM:
			plus_fv_real_common(value, -1, ret);
			break;

		case LISPTYPE_BIGNUM:
			plus_bv_real_common(local, value, -1, ret);
			break;

		case LISPTYPE_RATIO:
			plus_rv_real_common(local, value, -1, ret);
			break;

		default:
			TypeError(value, RATIONAL);
			break;
	}
}


/*
 *  plus
 */
void plus_fixnum_rational_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_ff_real_common(left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			plus_fb_real_common(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			plus_fr_real_common(local, left, right, ret);
			break;

		default:
			TypeError(right, RATIONAL);
			break;
	}
}

void plus_bignum_rational_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_bf_real_common(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			plus_bb_real_common(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			plus_br_real_common(local, left, right, ret);
			break;

		default:
			TypeError(right, RATIONAL);
			break;
	}
}

void plus_ratio_rational_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_rf_real_common(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			plus_rb_real_common(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			plus_rr_real_common(local, left, right, ret);
			break;

		default:
			TypeError(right, RATIONAL);
			break;
	}
}

void plus_single_rational_common(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_float_sf_heap(left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			plus_float_sb_heap(left, right, ret);
			break;

		case LISPTYPE_RATIO:
			plus_float_sr_heap(left, right, ret);
			break;

		default:
			TypeError(right, RATIONAL);
			break;
	}
}

void plus_double_rational_common(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_float_df_heap(left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			plus_float_db_heap(left, right, ret);
			break;

		case LISPTYPE_RATIO:
			plus_float_dr_heap(left, right, ret);
			break;

		default:
			TypeError(right, RATIONAL);
			break;
	}
}

void plus_long_rational_common(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_LONG_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_float_lf_heap(left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			plus_float_lb_heap(left, right, ret);
			break;

		case LISPTYPE_RATIO:
			plus_float_lr_heap(left, right, ret);
			break;

		default:
			TypeError(right, RATIONAL);
			break;
	}
}

void plus_rational_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocal(local);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			plus_fixnum_rational_common(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			plus_bignum_rational_common(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			plus_ratio_rational_common(local, left, right, ret);
			break;

		default:
			TypeError(left, RATIONAL);
			break;
	}
}

static void plus_fixnum_rational_local(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_ff_real_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			plus_fb_real_local(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			plus_fr_real_local(local, left, right, ret);
			break;

		default:
			TypeError(right, RATIONAL);
			break;
	}
}

static void plus_bignum_rational_local(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_bf_real_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			plus_bb_real_local(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			plus_br_real_local(local, left, right, ret);
			break;

		default:
			TypeError(right, RATIONAL);
			break;
	}
}

static void plus_ratio_rational_local(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_rf_real_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			plus_rb_real_local(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			plus_rr_real_local(local, left, right, ret);
			break;

		default:
			TypeError(right, RATIONAL);
			break;
	}
}

void plus_rational_local(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocal(local);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			plus_fixnum_rational_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			plus_bignum_rational_local(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			plus_ratio_rational_local(local, left, right, ret);
			break;

		default:
			TypeError(left, RATIONAL);
			break;
	}
}

/*
 *  minus
 */
void minus_fixnum_rational_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_ff_real_common(left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			minus_fb_real_common(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			minus_fr_real_common(local, left, right, ret);
			break;

		default:
			TypeError(right, RATIONAL);
			break;
	}
}
void minus_rational_fixnum_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, right, LISPTYPE_FIXNUM);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			minus_ff_real_common(left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			minus_bf_real_common(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			minus_rf_real_common(local, left, right, ret);
			break;

		default:
			TypeError(left, RATIONAL);
			break;
	}
}

void minus_bignum_rational_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_bf_real_common(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			minus_bb_real_common(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			minus_br_real_common(local, left, right, ret);
			break;

		default:
			TypeError(right, RATIONAL);
			break;
	}
}

void minus_rational_bignum_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, right, LISPTYPE_BIGNUM);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			minus_fb_real_common(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			minus_bb_real_common(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			minus_rb_real_common(local, left, right, ret);
			break;

		default:
			TypeError(left, RATIONAL);
			break;
	}
}

void minus_ratio_rational_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_rf_real_common(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			minus_rb_real_common(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			minus_rr_real_common(local, left, right, ret);
			break;

		default:
			TypeError(right, RATIONAL);
			break;
	}
}

void minus_rational_ratio_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, right, LISPTYPE_RATIO);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			minus_fr_real_common(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			minus_br_real_common(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			minus_rr_real_common(local, left, right, ret);
			break;

		default:
			TypeError(left, RATIONAL);
			break;
	}
}

void minus_single_rational_common(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_float_sf_heap(left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			minus_float_sb_heap(left, right, ret);
			break;

		case LISPTYPE_RATIO:
			minus_float_sr_heap(left, right, ret);
			break;

		default:
			TypeError(right, RATIONAL);
			break;
	}
}

void minus_rational_single_common(addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			minus_float_fs_heap(left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			minus_float_bs_heap(left, right, ret);
			break;

		case LISPTYPE_RATIO:
			minus_float_rs_heap(left, right, ret);
			break;

		default:
			TypeError(left, RATIONAL);
			break;
	}
}

void minus_double_rational_common(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_float_df_heap(left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			minus_float_db_heap(left, right, ret);
			break;

		case LISPTYPE_RATIO:
			minus_float_dr_heap(left, right, ret);
			break;

		default:
			TypeError(right, RATIONAL);
			break;
	}
}

void minus_rational_double_common(addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			minus_float_fd_heap(left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			minus_float_bd_heap(left, right, ret);
			break;

		case LISPTYPE_RATIO:
			minus_float_rd_heap(left, right, ret);
			break;

		default:
			TypeError(left, RATIONAL);
			break;
	}
}

void minus_long_rational_common(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_LONG_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_float_lf_heap(left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			minus_float_lb_heap(left, right, ret);
			break;

		case LISPTYPE_RATIO:
			minus_float_lr_heap(left, right, ret);
			break;

		default:
			TypeError(right, RATIONAL);
			break;
	}
}

void minus_rational_long_common(addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_LONG_FLOAT);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			minus_float_fl_heap(left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			minus_float_bl_heap(left, right, ret);
			break;

		case LISPTYPE_RATIO:
			minus_float_rl_heap(left, right, ret);
			break;

		default:
			TypeError(left, RATIONAL);
			break;
	}
}

static void minus_fixnum_rational_local(LocalRoot local,
		addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_ff_real_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			minus_fb_real_local(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			minus_fr_real_local(local, left, right, ret);
			break;

		default:
			TypeError(right, RATIONAL);
			break;
	}
}

static void minus_bignum_rational_local(LocalRoot local,
		addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_bf_real_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			minus_bb_real_local(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			minus_br_real_local(local, left, right, ret);
			break;

		default:
			TypeError(right, RATIONAL);
			break;
	}
}

static void minus_ratio_rational_local(LocalRoot local,
		addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_rf_real_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			minus_rb_real_local(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			minus_rr_real_local(local, left, right, ret);
			break;

		default:
			TypeError(right, RATIONAL);
			break;
	}
}

void minus_rational_local(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocal(local);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			minus_fixnum_rational_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			minus_bignum_rational_local(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			minus_ratio_rational_local(local, left, right, ret);
			break;

		default:
			TypeError(left, RATIONAL);
			break;
	}
}


/*
 *  multi
 */
void multi_fixnum_rational_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			multi_ff_real_common(left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			multi_fb_real_common(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			multi_fr_real_common(local, left, right, ret);
			break;

		default:
			TypeError(right, RATIONAL);
			break;
	}
}

void multi_bignum_rational_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			multi_bf_real_common(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			multi_bb_real_common(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			multi_br_real_common(local, left, right, ret);
			break;

		default:
			TypeError(right, RATIONAL);
			break;
	}
}

void multi_ratio_rational_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			multi_rf_real_common(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			multi_rb_real_common(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			multi_rr_real_common(local, left, right, ret);
			break;

		default:
			TypeError(right, RATIONAL);
			break;
	}
}

void multi_single_rational_common(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			multi_float_sf_heap(left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			multi_float_sb_heap(left, right, ret);
			break;

		case LISPTYPE_RATIO:
			multi_float_sr_heap(left, right, ret);
			break;

		default:
			TypeError(right, RATIONAL);
			break;
	}
}

void multi_double_rational_common(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			multi_float_df_heap(left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			multi_float_db_heap(left, right, ret);
			break;

		case LISPTYPE_RATIO:
			multi_float_dr_heap(left, right, ret);
			break;

		default:
			TypeError(right, RATIONAL);
			break;
	}
}

void multi_long_rational_common(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_LONG_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			multi_float_lf_heap(left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			multi_float_lb_heap(left, right, ret);
			break;

		case LISPTYPE_RATIO:
			multi_float_lr_heap(left, right, ret);
			break;

		default:
			TypeError(right, RATIONAL);
			break;
	}
}

void multi_rational_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocal(local);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			multi_fixnum_rational_common(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			multi_bignum_rational_common(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			multi_ratio_rational_common(local, left, right, ret);
			break;

		default:
			TypeError(left, RATIONAL);
			break;
	}
}

void multi_fixnum_rational_local(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			multi_ff_real_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			multi_fb_real_local(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			multi_fr_real_local(local, left, right, ret);
			break;

		default:
			TypeError(right, RATIONAL);
			break;
	}
}

void multi_bignum_rational_local(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			multi_bf_real_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			multi_bb_real_local(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			multi_br_real_local(local, left, right, ret);
			break;

		default:
			TypeError(right, RATIONAL);
			break;
	}
}

void multi_ratio_rational_local(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			multi_rf_real_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			multi_rb_real_local(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			multi_rr_real_local(local, left, right, ret);
			break;

		default:
			TypeError(right, RATIONAL);
			break;
	}
}

void multi_rational_local(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocal(local);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			multi_fixnum_rational_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			multi_bignum_rational_local(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			multi_ratio_rational_local(local, left, right, ret);
			break;

		default:
			TypeError(left, RATIONAL);
			break;
	}
}


/*
 *  div
 */
void div_fixnum_rational_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			div_ff_real_common(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			div_fb_real_common(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			div_fr_real_common(local, left, right, ret);
			break;

		default:
			TypeError(right, RATIONAL);
			break;
	}
}
void div_rational_fixnum_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, right, LISPTYPE_FIXNUM);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			div_ff_real_common(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			div_bf_real_common(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			div_rf_real_common(local, left, right, ret);
			break;

		default:
			TypeError(left, RATIONAL);
			break;
	}
}

void div_bignum_rational_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			div_bf_real_common(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			div_bb_real_common(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			div_br_real_common(local, left, right, ret);
			break;

		default:
			TypeError(right, RATIONAL);
			break;
	}
}

void div_rational_bignum_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, right, LISPTYPE_BIGNUM);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			div_fb_real_common(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			div_bb_real_common(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			div_rb_real_common(local, left, right, ret);
			break;

		default:
			TypeError(left, RATIONAL);
			break;
	}
}

void div_ratio_rational_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			div_rf_real_common(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			div_rb_real_common(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			div_rr_real_common(local, left, right, ret);
			break;

		default:
			TypeError(right, RATIONAL);
			break;
	}
}

void div_rational_ratio_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, right, LISPTYPE_RATIO);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			div_fr_real_common(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			div_br_real_common(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			div_rr_real_common(local, left, right, ret);
			break;

		default:
			TypeError(left, RATIONAL);
			break;
	}
}

void div_single_rational_common(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			div_float_sf_heap(left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			div_float_sb_heap(left, right, ret);
			break;

		case LISPTYPE_RATIO:
			div_float_sr_heap(left, right, ret);
			break;

		default:
			TypeError(right, RATIONAL);
			break;
	}
}

void div_rational_single_common(addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			div_float_fs_heap(left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			div_float_bs_heap(left, right, ret);
			break;

		case LISPTYPE_RATIO:
			div_float_rs_heap(left, right, ret);
			break;

		default:
			TypeError(left, RATIONAL);
			break;
	}
}

void div_double_rational_common(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			div_float_df_heap(left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			div_float_db_heap(left, right, ret);
			break;

		case LISPTYPE_RATIO:
			div_float_dr_heap(left, right, ret);
			break;

		default:
			TypeError(right, RATIONAL);
			break;
	}
}

void div_rational_double_common(addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			div_float_fd_heap(left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			div_float_bd_heap(left, right, ret);
			break;

		case LISPTYPE_RATIO:
			div_float_rd_heap(left, right, ret);
			break;

		default:
			TypeError(left, RATIONAL);
			break;
	}
}

void div_long_rational_common(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_LONG_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			div_float_lf_heap(left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			div_float_lb_heap(left, right, ret);
			break;

		case LISPTYPE_RATIO:
			div_float_lr_heap(left, right, ret);
			break;

		default:
			TypeError(right, RATIONAL);
			break;
	}
}

void div_rational_long_common(addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_LONG_FLOAT);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			div_float_fl_heap(left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			div_float_bl_heap(left, right, ret);
			break;

		case LISPTYPE_RATIO:
			div_float_rl_heap(left, right, ret);
			break;

		default:
			TypeError(left, RATIONAL);
			break;
	}
}

static void div_fixnum_rational_local(LocalRoot local,
		addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			div_ff_real_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			div_fb_real_local(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			div_fr_real_local(local, left, right, ret);
			break;

		default:
			TypeError(right, RATIONAL);
			break;
	}
}

static void div_bignum_rational_local(LocalRoot local,
		addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			div_bf_real_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			div_bb_real_local(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			div_br_real_local(local, left, right, ret);
			break;

		default:
			TypeError(right, RATIONAL);
			break;
	}
}

static void div_ratio_rational_local(LocalRoot local,
		addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			div_rf_real_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			div_rb_real_local(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			div_rr_real_local(local, left, right, ret);
			break;

		default:
			TypeError(right, RATIONAL);
			break;
	}
}

void div_rational_local(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocal(local);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			div_fixnum_rational_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			div_bignum_rational_local(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			div_ratio_rational_local(local, left, right, ret);
			break;

		default:
			TypeError(left, RATIONAL);
			break;
	}
}


/*
 *  numerator
 */
void numerator_common(addr pos, addr *ret)
{
	int sign;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			fixnum_throw_heap(pos, ret);
			break;

		case LISPTYPE_BIGNUM:
			bignum_throw_heap(pos, ret);
			break;

		case LISPTYPE_RATIO:
			GetSignRatio(pos, &sign);
			GetNumerRatio(pos, &pos);
			bignum_copy_heap(&pos, pos);
			SetSignBignum(pos, sign);
			*ret = pos;
			break;

		default:
			TypeError(pos, RATIONAL);
			*ret = 0;
			return;
	}
}


/*
 *  denominator
 */
void denominator_common(addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
			fixnum_heap(ret, 1);
			break;

		case LISPTYPE_RATIO:
			GetDenomRatio(pos, &pos);
			bignum_copy_heap(ret, pos);
			break;

		default:
			TypeError(pos, RATIONAL);
			*ret = 0;
			return;
	}
}

