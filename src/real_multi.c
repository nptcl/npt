#include "bignum_multi.h"
#include "condition.h"
#include "float_multi.h"
#include "ratio_multi.h"
#include "real_multi.h"
#include "typedef.h"

/*
 *  multiple
 */
int multi_fixnum_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			multi_ff_real_common(left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			multi_fb_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			multi_fr_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return multi_float_fs_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return multi_float_fd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return multi_float_fl_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int multi_bignum_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			multi_bf_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			multi_bb_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			multi_br_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return multi_float_bs_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return multi_float_bd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return multi_float_bl_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int multi_ratio_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			multi_rf_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			multi_rb_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			multi_rr_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return multi_float_rs_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return multi_float_rd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return multi_float_rl_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int multi_single_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return multi_float_sf_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return multi_float_sb_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return multi_float_sr_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return multi_float_ss_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return multi_float_sd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return multi_float_sl_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int multi_double_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return multi_float_df_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return multi_float_db_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return multi_float_dr_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return multi_float_ds_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return multi_float_dd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return multi_float_dl_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int multi_long_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_LONG_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return multi_float_lf_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return multi_float_lb_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return multi_float_lr_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return multi_float_ls_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return multi_float_ld_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return multi_float_ll_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int multi_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return multi_fixnum_real_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return multi_bignum_real_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return multi_ratio_real_common_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return multi_single_real_common_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return multi_double_real_common_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return multi_long_real_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

int multi_fixnum_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			multi_ff_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			multi_fb_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			multi_fr_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return multi_float_fs_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return multi_float_fd_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return multi_float_fl_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int multi_bignum_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			multi_bf_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			multi_bb_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			multi_br_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return multi_float_bs_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return multi_float_bd_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return multi_float_bl_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int multi_ratio_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			multi_rf_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			multi_rb_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			multi_rr_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return multi_float_rs_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return multi_float_rd_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return multi_float_rl_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int multi_single_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return multi_float_sf_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return multi_float_sb_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return multi_float_sr_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return multi_float_ss_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return multi_float_sd_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return multi_float_sl_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int multi_double_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return multi_float_df_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return multi_float_db_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return multi_float_dr_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return multi_float_ds_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return multi_float_dd_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return multi_float_dl_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int multi_long_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_LONG_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return multi_float_lf_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return multi_float_lb_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return multi_float_lr_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return multi_float_ls_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return multi_float_ld_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return multi_float_ll_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int multi_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return multi_fixnum_real_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return multi_bignum_real_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return multi_ratio_real_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return multi_single_real_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return multi_double_real_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return multi_long_real_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}


/*
 *  division
 */
int div_fixnum_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_ff_real_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_fb_real_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_fr_real_common_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_fs_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_fd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_fl_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int div_real_fixnum_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_FIXNUM);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_ff_real_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_bf_real_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_rf_real_common_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_sf_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_df_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_lf_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

int div_bignum_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_bf_real_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_bb_real_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_br_real_common_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_bs_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_bd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_bl_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int div_real_bignum_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_BIGNUM);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_fb_real_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_bb_real_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_rb_real_common_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_sb_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_db_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_lb_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

int div_ratio_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_rf_real_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_rb_real_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_rr_real_common_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_rs_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_rd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_rl_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int div_real_ratio_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_RATIO);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_fr_real_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_br_real_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_rr_real_common_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_sr_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_dr_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_lr_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

int div_single_real_common_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_float_sf_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_float_sb_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return div_float_sr_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_ss_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_sd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_sl_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int div_real_single_common_(addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_float_fs_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_float_bs_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return div_float_rs_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_ss_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_ds_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_ls_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

int div_double_real_common_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_float_df_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_float_db_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return div_float_dr_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_ds_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_dd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_dl_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int div_real_double_common_(addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_float_fd_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_float_bd_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return div_float_rd_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_sd_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_dd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_ld_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

int div_long_real_common_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_LONG_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_float_lf_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_float_lb_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return div_float_lr_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_ls_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_ld_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_ll_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int div_real_long_common_(addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_LONG_FLOAT);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_float_fl_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_float_bl_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return div_float_rl_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_sl_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_dl_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_ll_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

int div_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_fixnum_real_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_bignum_real_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_ratio_real_common_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_single_real_common_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_double_real_common_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_long_real_common_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

int div_fixnum_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_ff_real_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_fb_real_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_fr_real_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_fs_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_fd_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_fl_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int div_real_fixnum_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_FIXNUM);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_ff_real_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_bf_real_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_rf_real_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_sf_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_df_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_lf_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

int div_bignum_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_bf_real_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_bb_real_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_br_real_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_bs_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_bd_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_bl_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int div_real_bignum_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_BIGNUM);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_fb_real_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_bb_real_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_rb_real_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_sb_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_db_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_lb_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

int div_ratio_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_rf_real_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_rb_real_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_rr_real_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_rs_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_rd_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_rl_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int div_real_ratio_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_RATIO);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_fr_real_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_br_real_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_rr_real_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_sr_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_dr_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_lr_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

int div_single_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_float_sf_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_float_sb_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_float_sr_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_ss_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_sd_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_sl_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int div_real_single_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_float_fs_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_float_bs_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_float_rs_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_ss_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_ds_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_ls_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

int div_double_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_float_df_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_float_db_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_float_dr_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_ds_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_dd_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_dl_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int div_real_double_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_float_fd_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_float_bd_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_float_rd_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_sd_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_dd_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_ld_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

int div_long_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_LONG_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_float_lf_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_float_lb_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_float_lr_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_ls_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_ld_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_ll_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

int div_real_long_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_LONG_FLOAT);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_float_fl_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_float_bl_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_float_rl_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_float_sl_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_float_dl_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_float_ll_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

int div_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_fixnum_real_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_bignum_real_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_ratio_real_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_single_real_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_double_real_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_long_real_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

