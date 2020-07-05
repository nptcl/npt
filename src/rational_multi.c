#include "bignum_multi.h"
#include "condition.h"
#include "float_multi.h"
#include "ratio_multi.h"
#include "typedef.h"

/*
 *  multi
 */
_g void multi_fixnum_rational_common(LocalRoot local, addr left, addr right, addr *ret)
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

_g void multi_bignum_rational_common(LocalRoot local, addr left, addr right, addr *ret)
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

_g void multi_ratio_rational_common(LocalRoot local, addr left, addr right, addr *ret)
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

_g void multi_single_rational_common(addr left, addr right, addr *ret)
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

_g void multi_double_rational_common(addr left, addr right, addr *ret)
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

_g void multi_long_rational_common(addr left, addr right, addr *ret)
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

_g void multi_rational_common(LocalRoot local, addr left, addr right, addr *ret)
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

_g void multi_fixnum_rational_local(LocalRoot local, addr left, addr right, addr *ret)
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

_g void multi_bignum_rational_local(LocalRoot local, addr left, addr right, addr *ret)
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

_g void multi_ratio_rational_local(LocalRoot local, addr left, addr right, addr *ret)
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

_g void multi_rational_local(LocalRoot local, addr left, addr right, addr *ret)
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
_g void div_fixnum_rational_common(LocalRoot local, addr left, addr right, addr *ret)
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
_g void div_rational_fixnum_common(LocalRoot local, addr left, addr right, addr *ret)
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

_g void div_bignum_rational_common(LocalRoot local, addr left, addr right, addr *ret)
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

_g void div_rational_bignum_common(LocalRoot local, addr left, addr right, addr *ret)
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

_g void div_ratio_rational_common(LocalRoot local, addr left, addr right, addr *ret)
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

_g void div_rational_ratio_common(LocalRoot local, addr left, addr right, addr *ret)
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

_g void div_single_rational_common(addr left, addr right, addr *ret)
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

_g void div_rational_single_common(addr left, addr right, addr *ret)
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

_g void div_double_rational_common(addr left, addr right, addr *ret)
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

_g void div_rational_double_common(addr left, addr right, addr *ret)
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

_g void div_long_rational_common(addr left, addr right, addr *ret)
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

_g void div_rational_long_common(addr left, addr right, addr *ret)
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

_g void div_rational_local(LocalRoot local, addr left, addr right, addr *ret)
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
 *  inverse
 */
_g void inverse_rational_common(LocalRoot local, addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			inverse_fixnum_common(pos, ret);
			break;

		case LISPTYPE_BIGNUM:
			inverse_bignum_common(pos, ret);
			break;

		case LISPTYPE_RATIO:
			inverse_ratio_common(local, pos, ret);
			break;

		default:
			TypeError(pos, RATIONAL);
			*ret = 0;
			return;
	}
}

