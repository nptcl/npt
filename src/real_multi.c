#include "bignum_multi.h"
#include "condition.h"
#include "float_multi.h"
#include "ratio_multi.h"
#include "typedef.h"

/*
 *  multiple
 */
_g void multi_fixnum_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
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

		case LISPTYPE_SINGLE_FLOAT:
			multi_float_fs_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			multi_float_fd_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			multi_float_fl_heap(left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void multi_bignum_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
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

		case LISPTYPE_SINGLE_FLOAT:
			multi_float_bs_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			multi_float_bd_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			multi_float_bl_heap(left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void multi_ratio_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_RATIO);
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

		case LISPTYPE_SINGLE_FLOAT:
			multi_float_rs_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			multi_float_rd_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			multi_float_rl_heap(left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void multi_single_real_common(LocalRoot local, addr left, addr right, addr *ret)
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

		case LISPTYPE_SINGLE_FLOAT:
			multi_float_ss_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			multi_float_sd_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			multi_float_sl_heap(left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void multi_double_real_common(LocalRoot local, addr left, addr right, addr *ret)
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

		case LISPTYPE_SINGLE_FLOAT:
			multi_float_ds_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			multi_float_dd_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			multi_float_dl_heap(left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void multi_long_real_common(LocalRoot local, addr left, addr right, addr *ret)
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

		case LISPTYPE_SINGLE_FLOAT:
			multi_float_ls_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			multi_float_ld_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			multi_float_ll_heap(left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void multi_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			multi_fixnum_real_common(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			multi_bignum_real_common(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			multi_ratio_real_common(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			multi_single_real_common(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			multi_double_real_common(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			multi_long_real_common(local, left, right, ret);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

_g void multi_fixnum_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
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

		case LISPTYPE_SINGLE_FLOAT:
			multi_float_fs_local(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			multi_float_fd_local(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			multi_float_fl_local(local, left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void multi_bignum_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
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

		case LISPTYPE_SINGLE_FLOAT:
			multi_float_bs_local(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			multi_float_bd_local(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			multi_float_bl_local(local, left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void multi_ratio_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_RATIO);
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

		case LISPTYPE_SINGLE_FLOAT:
			multi_float_rs_local(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			multi_float_rd_local(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			multi_float_rl_local(local, left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void multi_single_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			multi_float_sf_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			multi_float_sb_local(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			multi_float_sr_local(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			multi_float_ss_local(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			multi_float_sd_local(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			multi_float_sl_local(local, left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void multi_double_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			multi_float_df_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			multi_float_db_local(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			multi_float_dr_local(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			multi_float_ds_local(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			multi_float_dd_local(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			multi_float_dl_local(local, left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void multi_long_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_LONG_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			multi_float_lf_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			multi_float_lb_local(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			multi_float_lr_local(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			multi_float_ls_local(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			multi_float_ld_local(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			multi_float_ll_local(local, left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void multi_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			multi_fixnum_real_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			multi_bignum_real_local(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			multi_ratio_real_local(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			multi_single_real_local(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			multi_double_real_local(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			multi_long_real_local(local, left, right, ret);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}


/*
 *  division
 */
_g void div_fixnum_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
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

		case LISPTYPE_SINGLE_FLOAT:
			div_float_fs_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			div_float_fd_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			div_float_fl_heap(left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void div_real_fixnum_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_FIXNUM);
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

		case LISPTYPE_SINGLE_FLOAT:
			div_float_sf_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			div_float_df_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			div_float_lf_heap(left, right, ret);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

_g void div_bignum_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
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

		case LISPTYPE_SINGLE_FLOAT:
			div_float_bs_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			div_float_bd_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			div_float_bl_heap(left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void div_real_bignum_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_BIGNUM);
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

		case LISPTYPE_SINGLE_FLOAT:
			div_float_sb_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			div_float_db_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			div_float_lb_heap(left, right, ret);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

_g void div_ratio_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_RATIO);
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

		case LISPTYPE_SINGLE_FLOAT:
			div_float_rs_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			div_float_rd_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			div_float_rl_heap(left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void div_real_ratio_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_RATIO);
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

		case LISPTYPE_SINGLE_FLOAT:
			div_float_sr_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			div_float_dr_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			div_float_lr_heap(left, right, ret);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

_g void div_single_real_common(addr left, addr right, addr *ret)
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

		case LISPTYPE_SINGLE_FLOAT:
			div_float_ss_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			div_float_sd_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			div_float_sl_heap(left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void div_real_single_common(addr left, addr right, addr *ret)
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

		case LISPTYPE_SINGLE_FLOAT:
			div_float_ss_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			div_float_ds_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			div_float_ls_heap(left, right, ret);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

_g void div_double_real_common(addr left, addr right, addr *ret)
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

		case LISPTYPE_SINGLE_FLOAT:
			div_float_ds_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			div_float_dd_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			div_float_dl_heap(left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void div_real_double_common(addr left, addr right, addr *ret)
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

		case LISPTYPE_SINGLE_FLOAT:
			div_float_sd_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			div_float_dd_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			div_float_ld_heap(left, right, ret);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

_g void div_long_real_common(addr left, addr right, addr *ret)
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

		case LISPTYPE_SINGLE_FLOAT:
			div_float_ls_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			div_float_ld_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			div_float_ll_heap(left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void div_real_long_common(addr left, addr right, addr *ret)
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

		case LISPTYPE_SINGLE_FLOAT:
			div_float_sl_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			div_float_dl_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			div_float_ll_heap(left, right, ret);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

_g void div_real_common(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			div_fixnum_real_common(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			div_bignum_real_common(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			div_ratio_real_common(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			div_single_real_common(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			div_double_real_common(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			div_long_real_common(left, right, ret);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

_g void div_fixnum_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
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

		case LISPTYPE_SINGLE_FLOAT:
			div_float_fs_local(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			div_float_fd_local(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			div_float_fl_local(local, left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void div_real_fixnum_local(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_FIXNUM);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			div_ff_real_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			div_bf_real_local(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			div_rf_real_local(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			div_float_sf_local(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			div_float_df_local(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			div_float_lf_local(local, left, right, ret);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

_g void div_bignum_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
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

		case LISPTYPE_SINGLE_FLOAT:
			div_float_bs_local(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			div_float_bd_local(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			div_float_bl_local(local, left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void div_real_bignum_local(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_BIGNUM);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			div_fb_real_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			div_bb_real_local(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			div_rb_real_local(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			div_float_sb_local(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			div_float_db_local(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			div_float_lb_local(local, left, right, ret);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

_g void div_ratio_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_RATIO);
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

		case LISPTYPE_SINGLE_FLOAT:
			div_float_rs_local(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			div_float_rd_local(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			div_float_rl_local(local, left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void div_real_ratio_local(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_RATIO);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			div_fr_real_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			div_br_real_local(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			div_rr_real_local(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			div_float_sr_local(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			div_float_dr_local(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			div_float_lr_local(local, left, right, ret);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

_g void div_single_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			div_float_sf_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			div_float_sb_local(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			div_float_sr_local(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			div_float_ss_local(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			div_float_sd_local(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			div_float_sl_local(local, left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void div_real_single_local(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			div_float_fs_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			div_float_bs_local(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			div_float_rs_local(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			div_float_ss_local(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			div_float_ds_local(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			div_float_ls_local(local, left, right, ret);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

_g void div_double_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			div_float_df_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			div_float_db_local(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			div_float_dr_local(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			div_float_ds_local(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			div_float_dd_local(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			div_float_dl_local(local, left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void div_real_double_local(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			div_float_fd_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			div_float_bd_local(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			div_float_rd_local(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			div_float_sd_local(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			div_float_dd_local(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			div_float_ld_local(local, left, right, ret);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

_g void div_long_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_LONG_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			div_float_lf_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			div_float_lb_local(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			div_float_lr_local(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			div_float_ls_local(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			div_float_ld_local(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			div_float_ll_local(local, left, right, ret);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void div_real_long_local(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_LONG_FLOAT);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			div_float_fl_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			div_float_bl_local(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			div_float_rl_local(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			div_float_sl_local(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			div_float_dl_local(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			div_float_ll_local(local, left, right, ret);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

_g void div_real_local(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			div_fixnum_real_local(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			div_bignum_real_local(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			div_ratio_real_local(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			div_single_real_local(local, left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			div_double_real_local(local, left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			div_long_real_local(local, left, right, ret);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

