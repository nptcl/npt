#include "bignum_plus.h"
#include "condition.h"
#include "float_plus.h"
#include "ratio_plus.h"
#include "typedef.h"

/*
 *  sign-reverse
 */
_g void sign_reverse_rational_common(addr pos, addr *ret)
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

_g void sign_reverse_rational_local(LocalRoot local, addr pos, addr *ret)
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
_g void oneplus_rational_common(LocalRoot local, addr value, addr *ret)
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

_g void oneminus_rational_common(LocalRoot local, addr value, addr *ret)
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
_g void plus_fixnum_rational_common(LocalRoot local, addr left, addr right, addr *ret)
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

_g void plus_bignum_rational_common(LocalRoot local, addr left, addr right, addr *ret)
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

_g void plus_ratio_rational_common(LocalRoot local, addr left, addr right, addr *ret)
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

_g void plus_single_rational_common(addr left, addr right, addr *ret)
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

_g void plus_double_rational_common(addr left, addr right, addr *ret)
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

_g void plus_long_rational_common(addr left, addr right, addr *ret)
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

_g void plus_rational_common(LocalRoot local, addr left, addr right, addr *ret)
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

_g void plus_rational_local(LocalRoot local, addr left, addr right, addr *ret)
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
_g void minus_fixnum_rational_common(LocalRoot local, addr left, addr right, addr *ret)
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
_g void minus_rational_fixnum_common(LocalRoot local, addr left, addr right, addr *ret)
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

_g void minus_bignum_rational_common(LocalRoot local, addr left, addr right, addr *ret)
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

_g void minus_rational_bignum_common(LocalRoot local, addr left, addr right, addr *ret)
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

_g void minus_ratio_rational_common(LocalRoot local, addr left, addr right, addr *ret)
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

_g void minus_rational_ratio_common(LocalRoot local, addr left, addr right, addr *ret)
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

_g void minus_single_rational_common(addr left, addr right, addr *ret)
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

_g void minus_rational_single_common(addr left, addr right, addr *ret)
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

_g void minus_double_rational_common(addr left, addr right, addr *ret)
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

_g void minus_rational_double_common(addr left, addr right, addr *ret)
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

_g void minus_long_rational_common(addr left, addr right, addr *ret)
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

_g void minus_rational_long_common(addr left, addr right, addr *ret)
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

_g void minus_rational_local(LocalRoot local, addr left, addr right, addr *ret)
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

_g void minus_rational_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocal(local);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			minus_fixnum_rational_common(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			minus_bignum_rational_common(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			minus_ratio_rational_common(local, left, right, ret);
			break;

		default:
			TypeError(left, RATIONAL);
			break;
	}
}

