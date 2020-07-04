#include "bignum_plus.h"
#include "condition.h"
#include "float_plus.h"
#include "ratio_plus.h"
#include "typedef.h"

/*
 *  sign_reverse
 */
_g void sign_reverse_real_common(addr pos, addr *ret)
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

		case LISPTYPE_SINGLE_FLOAT:
			sign_reverse_floats_heap(pos, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			sign_reverse_floatd_heap(pos, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			sign_reverse_floatl_heap(pos, ret);
			break;

		default:
			TypeError(pos, REAL);
			break;
	}
}

_g void sign_reverse_real_local(LocalRoot local, addr pos, addr *ret)
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

		case LISPTYPE_SINGLE_FLOAT:
			sign_reverse_floats_local(local, pos, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			sign_reverse_floatd_local(local, pos, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			sign_reverse_floatl_local(local, pos, ret);
			break;

		default:
			TypeError(pos, REAL);
			break;
	}
}


/*
 *  1+, 1-
 */
_g void oneplus_real_common(LocalRoot local, addr value, addr *ret)
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

		case LISPTYPE_SINGLE_FLOAT:
			plus_float_sv_heap(value, 1.0f, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			plus_float_dv_heap(value, 1.0, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			plus_float_lv_heap(value, 1.0L, ret);
			break;

		default:
			TypeError(value, REAL);
			break;
	}
}

_g void oneminus_real_common(LocalRoot local, addr value, addr *ret)
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

		case LISPTYPE_SINGLE_FLOAT:
			plus_float_sv_heap(value, -1.0f, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			plus_float_dv_heap(value, -1.0, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			plus_float_lv_heap(value, -1.0L, ret);
			break;

		default:
			TypeError(value, REAL);
			break;
	}
}


/*
 *  plus
 */
_g void plus_fixnum_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
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

		case LISPTYPE_SINGLE_FLOAT:
			plus_float_fs_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			plus_float_fd_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			plus_float_fl_heap(left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void plus_bignum_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
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

		case LISPTYPE_SINGLE_FLOAT:
			plus_float_bs_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			plus_float_bd_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			plus_float_bl_heap(left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void plus_ratio_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_RATIO);
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

		case LISPTYPE_SINGLE_FLOAT:
			plus_float_rs_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			plus_float_rd_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			plus_float_rl_heap(left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void plus_single_real_common(addr left, addr right, addr *ret)
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

		case LISPTYPE_SINGLE_FLOAT:
			plus_float_ss_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			plus_float_sd_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			plus_float_sl_heap(left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void plus_double_real_common(addr left, addr right, addr *ret)
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

		case LISPTYPE_SINGLE_FLOAT:
			plus_float_ds_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			plus_float_dd_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			plus_float_dl_heap(left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void plus_long_real_common(addr left, addr right, addr *ret)
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

		case LISPTYPE_SINGLE_FLOAT:
			plus_float_ls_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			plus_float_ld_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			plus_float_ll_heap(left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void plus_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			plus_fixnum_real_common(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			plus_bignum_real_common(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			plus_ratio_real_common(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			plus_single_real_common(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			plus_double_real_common(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			plus_long_real_common(left, right, ret);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

static void plus_fixnum_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	CheckType(left, LISPTYPE_FIXNUM);
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

		case LISPTYPE_SINGLE_FLOAT:
			plus_float_fs_alloc(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			plus_float_fd_alloc(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			plus_float_fl_alloc(local, left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void plus_bignum_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	CheckType(left, LISPTYPE_BIGNUM);
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

		case LISPTYPE_SINGLE_FLOAT:
			plus_float_bs_alloc(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			plus_float_bd_alloc(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			plus_float_bl_alloc(local, left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void plus_ratio_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	CheckType(left, LISPTYPE_RATIO);
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

		case LISPTYPE_SINGLE_FLOAT:
			plus_float_rs_alloc(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			plus_float_rd_alloc(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			plus_float_rl_alloc(local, left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void plus_single_float_real_local(LocalRoot local,
		addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_float_sf_alloc(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			plus_float_sb_alloc(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			plus_float_sr_alloc(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			plus_float_ss_alloc(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			plus_float_sd_alloc(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			plus_float_sl_alloc(local, left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void plus_double_float_real_local(LocalRoot local,
		addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_float_df_alloc(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			plus_float_db_alloc(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			plus_float_dr_alloc(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			plus_float_ds_alloc(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			plus_float_dd_alloc(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			plus_float_dl_alloc(local, left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void plus_long_float_real_local(LocalRoot local,
		addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	CheckType(left, LISPTYPE_LONG_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_float_lf_alloc(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			plus_float_lb_alloc(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			plus_float_lr_alloc(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			plus_float_ls_alloc(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			plus_float_ld_alloc(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			plus_float_ll_alloc(local, left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void plus_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			plus_fixnum_real_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			plus_bignum_real_local(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			plus_ratio_real_local(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			plus_single_float_real_local(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			plus_double_float_real_local(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			plus_long_float_real_local(local, left, right, ret);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}


/*
 *  minus
 */
_g void minus_fixnum_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
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

		case LISPTYPE_SINGLE_FLOAT:
			minus_float_fs_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			minus_float_fd_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			minus_float_fl_heap(left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void minus_real_fixnum_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_FIXNUM);
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

		case LISPTYPE_SINGLE_FLOAT:
			minus_float_sf_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			minus_float_df_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			minus_float_lf_heap(left, right, ret);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

_g void minus_bignum_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
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

		case LISPTYPE_SINGLE_FLOAT:
			minus_float_bs_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			minus_float_bd_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			minus_float_bl_heap(left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void minus_real_bignum_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_BIGNUM);
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

		case LISPTYPE_SINGLE_FLOAT:
			minus_float_sb_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			minus_float_db_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			minus_float_lb_heap(left, right, ret);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

_g void minus_ratio_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_RATIO);
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

		case LISPTYPE_SINGLE_FLOAT:
			minus_float_rs_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			minus_float_rd_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			minus_float_rl_heap(left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void minus_real_ratio_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_RATIO);
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

		case LISPTYPE_SINGLE_FLOAT:
			minus_float_sr_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			minus_float_dr_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			minus_float_lr_heap(left, right, ret);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

_g void minus_single_real_common(LocalRoot local, addr left, addr right, addr *ret)
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

		case LISPTYPE_SINGLE_FLOAT:
			minus_float_ss_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			minus_float_sd_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			minus_float_sl_heap(left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void minus_real_single_common(LocalRoot local, addr left, addr right, addr *ret)
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

		case LISPTYPE_SINGLE_FLOAT:
			minus_float_ss_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			minus_float_ds_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			minus_float_ls_heap(left, right, ret);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

_g void minus_double_real_common(LocalRoot local, addr left, addr right, addr *ret)
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

		case LISPTYPE_SINGLE_FLOAT:
			minus_float_ds_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			minus_float_dd_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			minus_float_dl_heap(left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void minus_real_double_common(LocalRoot local, addr left, addr right, addr *ret)
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

		case LISPTYPE_SINGLE_FLOAT:
			minus_float_sd_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			minus_float_dd_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			minus_float_ld_heap(left, right, ret);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

_g void minus_long_real_common(LocalRoot local, addr left, addr right, addr *ret)
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

		case LISPTYPE_SINGLE_FLOAT:
			minus_float_ls_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			minus_float_ld_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			minus_float_ll_heap(left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void minus_real_long_common(LocalRoot local, addr left, addr right, addr *ret)
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

		case LISPTYPE_SINGLE_FLOAT:
			minus_float_sl_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			minus_float_dl_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			minus_float_ll_heap(left, right, ret);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

_g void minus_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			minus_fixnum_real_common(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			minus_bignum_real_common(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			minus_ratio_real_common(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			minus_single_real_common(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			minus_double_real_common(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			minus_long_real_common(local, left, right, ret);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

static void minus_fixnum_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	CheckType(left, LISPTYPE_FIXNUM);
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

		case LISPTYPE_SINGLE_FLOAT:
			minus_float_fs_local(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			minus_float_fd_local(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			minus_float_fl_local(local, left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void minus_bignum_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	CheckType(left, LISPTYPE_BIGNUM);
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

		case LISPTYPE_SINGLE_FLOAT:
			minus_float_bs_local(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			minus_float_bd_local(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			minus_float_bl_local(local, left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void minus_ratio_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	CheckType(left, LISPTYPE_RATIO);
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

		case LISPTYPE_SINGLE_FLOAT:
			minus_float_rs_local(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			minus_float_rd_local(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			minus_float_rl_local(local, left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void minus_single_real_local(LocalRoot local,
		addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_float_sf_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			minus_float_sb_local(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			minus_float_sr_local(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			minus_float_ss_local(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			minus_float_sd_local(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			minus_float_sl_local(local, left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void minus_double_real_local(LocalRoot local,
		addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_float_df_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			minus_float_db_local(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			minus_float_dr_local(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			minus_float_ds_local(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			minus_float_dd_local(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			minus_float_dl_local(local, left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void minus_long_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	CheckType(left, LISPTYPE_LONG_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_float_lf_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			minus_float_lb_local(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			minus_float_lr_local(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			minus_float_ls_local(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			minus_float_ld_local(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			minus_float_ll_local(local, left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void minus_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			minus_fixnum_real_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			minus_bignum_real_local(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			minus_ratio_real_local(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			minus_single_real_local(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			minus_double_real_local(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			minus_long_real_local(local, left, right, ret);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

