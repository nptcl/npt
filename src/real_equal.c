#include "bignum.h"
#include "bignum_equal.h"
#include "condition.h"
#include "float_equal.h"
#include "integer.h"
#include "object.h"
#include "ratio.h"
#include "ratio_equal.h"
#include "real_equal.h"
#include "typedef.h"

_g int plusp_real(addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return plusp_fixnum(pos);

		case LISPTYPE_BIGNUM:
			return plusp_bignum(pos);

		case LISPTYPE_RATIO:
			return plusp_ratio(pos);

		case LISPTYPE_SINGLE_FLOAT:
			return plusp_single_float(pos);

		case LISPTYPE_DOUBLE_FLOAT:
			return plusp_double_float(pos);

		case LISPTYPE_LONG_FLOAT:
			return plusp_long_float(pos);

		default:
			TypeError(pos, REAL);
			break;
	}
	return 0;
}

_g int minusp_real(addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return minusp_fixnum(pos);

		case LISPTYPE_BIGNUM:
			return minusp_bignum(pos);

		case LISPTYPE_RATIO:
			return minusp_ratio(pos);

		case LISPTYPE_SINGLE_FLOAT:
			return minusp_single_float(pos);

		case LISPTYPE_DOUBLE_FLOAT:
			return minusp_double_float(pos);

		case LISPTYPE_LONG_FLOAT:
			return minusp_long_float(pos);

		default:
			TypeError(pos, REAL);
			break;
	}
	return 0;
}

_g int zerop_real(addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return zerop_fixnum(pos);

		case LISPTYPE_BIGNUM:
			return zerop_bignum(pos);

		case LISPTYPE_RATIO:
			return zerop_ratio(pos);

		case LISPTYPE_SINGLE_FLOAT:
			return zerop_single_float(pos);

		case LISPTYPE_DOUBLE_FLOAT:
			return zerop_double_float(pos);

		case LISPTYPE_LONG_FLOAT:
			return zerop_long_float(pos);

		default:
			TypeError(pos, REAL);
			break;
	}
	return 0;
}

_g int equal_fixnum_real(addr left, addr right)
{
	CheckType(left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return equal_ff_real(left, right);

		case LISPTYPE_BIGNUM:
			return equal_fb_real(left, right);

		case LISPTYPE_RATIO:
			return equal_fr_real(left, right);

		case LISPTYPE_SINGLE_FLOAT:
			return equal_fs_real(left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return equal_fd_real(left, right);

		case LISPTYPE_LONG_FLOAT:
			return equal_fl_real(left, right);

		default:
			TypeError(right, REAL);
			break;
	}
	return 0;
}

_g int equal_bignum_real(addr left, addr right)
{
	CheckType(left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return equal_bf_real(left, right);

		case LISPTYPE_BIGNUM:
			return equal_bb_real(left, right);

		case LISPTYPE_RATIO:
			return equal_br_real(left, right);

		case LISPTYPE_SINGLE_FLOAT:
			return equal_bs_real(left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return equal_bd_real(left, right);

		case LISPTYPE_LONG_FLOAT:
			return equal_bl_real(left, right);

		default:
			TypeError(right, REAL);
			break;
	}
	return 0;
}

_g int equal_ratio_real(LocalRoot local, addr left, addr right)
{
	CheckType(left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return equal_rf_real(left, right);

		case LISPTYPE_BIGNUM:
			return equal_rb_real(left, right);

		case LISPTYPE_RATIO:
			return equal_rr_real(left, right);

		case LISPTYPE_SINGLE_FLOAT:
			return equal_rs_real(local, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return equal_rd_real(local, left, right);

		case LISPTYPE_LONG_FLOAT:
			return equal_rl_real(local, left, right);

		default:
			TypeError(right, REAL);
			break;
	}
	return 0;
}

_g int equal_single_float_real(LocalRoot local, addr left, addr right)
{
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return equal_sf_real(left, right);

		case LISPTYPE_BIGNUM:
			return equal_sb_real(left, right);

		case LISPTYPE_RATIO:
			return equal_sr_real(local, left, right);

		case LISPTYPE_SINGLE_FLOAT:
			return equal_ss_real(left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return equal_sd_real(left, right);

		case LISPTYPE_LONG_FLOAT:
			return equal_sl_real(left, right);

		default:
			TypeError(right, REAL);
			break;
	}
	return 0;
}

_g int equal_double_float_real(LocalRoot local, addr left, addr right)
{
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return equal_df_real(left, right);

		case LISPTYPE_BIGNUM:
			return equal_db_real(left, right);

		case LISPTYPE_RATIO:
			return equal_dr_real(local, left, right);

		case LISPTYPE_SINGLE_FLOAT:
			return equal_ds_real(left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return equal_dd_real(left, right);

		case LISPTYPE_LONG_FLOAT:
			return equal_dl_real(left, right);

		default:
			TypeError(right, REAL);
			break;
	}
	return 0;
}

_g int equal_long_float_real(LocalRoot local, addr left, addr right)
{
	CheckType(left, LISPTYPE_LONG_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return equal_lf_real(left, right);

		case LISPTYPE_BIGNUM:
			return equal_lb_real(left, right);

		case LISPTYPE_RATIO:
			return equal_lr_real(local, left, right);

		case LISPTYPE_SINGLE_FLOAT:
			return equal_ls_real(left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return equal_ld_real(left, right);

		case LISPTYPE_LONG_FLOAT:
			return equal_ll_real(left, right);

		default:
			TypeError(right, REAL);
			break;
	}
	return 0;
}

_g int equal_real(LocalRoot local, addr left, addr right)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return equal_fixnum_real(left, right);

		case LISPTYPE_BIGNUM:
			return equal_bignum_real(left, right);

		case LISPTYPE_RATIO:
			return equal_ratio_real(local, left, right);

		case LISPTYPE_SINGLE_FLOAT:
			return equal_single_float_real(local, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return equal_double_float_real(local, left, right);

		case LISPTYPE_LONG_FLOAT:
			return equal_long_float_real(local, left, right);

		default:
			TypeError(left, REAL);
			break;
	}
	return 0;
}

static inline int compare_fixnum_real(LocalRoot local, addr left, addr right)
{
	CheckType(left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return compare_ff_real(left, right);

		case LISPTYPE_BIGNUM:
			return compare_fb_real(left, right);

		case LISPTYPE_RATIO:
			return compare_fr_real(local, left, right);

		case LISPTYPE_SINGLE_FLOAT:
			return compare_fs_real(left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return compare_fd_real(left, right);

		case LISPTYPE_LONG_FLOAT:
			return compare_fl_real(left, right);

		default:
			TypeError(right, REAL);
			break;
	}
	return 0;
}

static inline int compare_bignum_real(LocalRoot local, addr left, addr right)
{
	CheckType(left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return compare_bf_real(left, right);

		case LISPTYPE_BIGNUM:
			return compare_bb_real(left, right);

		case LISPTYPE_RATIO:
			return compare_br_real(local, left, right);

		case LISPTYPE_SINGLE_FLOAT:
			return compare_bs_real(left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return compare_bd_real(left, right);

		case LISPTYPE_LONG_FLOAT:
			return compare_bl_real(left, right);

		default:
			TypeError(right, REAL);
			break;
	}
	return 0;
}

_g int compare_ratio_real(LocalRoot local, addr left, addr right)
{
	CheckType(left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return compare_rf_real(local, left, right);

		case LISPTYPE_BIGNUM:
			return compare_rb_real(local, left, right);

		case LISPTYPE_RATIO:
			return compare_rr_real(local, left, right);

		case LISPTYPE_SINGLE_FLOAT:
			return compare_rs_real(local, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return compare_rd_real(local, left, right);

		case LISPTYPE_LONG_FLOAT:
			return compare_rl_real(local, left, right);

		default:
			TypeError(right, REAL);
			break;
	}
	return 0;
}

static inline int compare_single_float_real(LocalRoot local, addr left, addr right)
{
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return compare_sf_real(left, right);

		case LISPTYPE_BIGNUM:
			return compare_sb_real(left, right);

		case LISPTYPE_RATIO:
			return compare_sr_real(local, left, right);

		case LISPTYPE_SINGLE_FLOAT:
			return compare_ss_real(left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return compare_sd_real(left, right);

		case LISPTYPE_LONG_FLOAT:
			return compare_sl_real(left, right);

		default:
			TypeError(right, REAL);
			break;
	}
	return 0;
}

static int compare_double_float_real(LocalRoot local, addr left, addr right)
{
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return compare_df_real(left, right);

		case LISPTYPE_BIGNUM:
			return compare_db_real(left, right);

		case LISPTYPE_RATIO:
			return compare_dr_real(local, left, right);

		case LISPTYPE_SINGLE_FLOAT:
			return compare_ds_real(left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return compare_dd_real(left, right);

		case LISPTYPE_LONG_FLOAT:
			return compare_dl_real(left, right);

		default:
			TypeError(right, REAL);
			break;
	}
	return 0;
}

static int compare_long_float_real(LocalRoot local, addr left, addr right)
{
	CheckType(left, LISPTYPE_LONG_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return compare_lf_real(left, right);

		case LISPTYPE_BIGNUM:
			return compare_lb_real(left, right);

		case LISPTYPE_RATIO:
			return compare_lr_real(local, left, right);

		case LISPTYPE_SINGLE_FLOAT:
			return compare_ls_real(left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return compare_ld_real(left, right);

		case LISPTYPE_LONG_FLOAT:
			return compare_ll_real(left, right);

		default:
			TypeError(right, REAL);
			break;
	}
	return 0;
}

_g int compare_real(LocalRoot local, addr left, addr right)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return compare_fixnum_real(local, left, right);

		case LISPTYPE_BIGNUM:
			return compare_bignum_real(local, left, right);

		case LISPTYPE_RATIO:
			return compare_ratio_real(local, left, right);

		case LISPTYPE_SINGLE_FLOAT:
			return compare_single_float_real(local, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return compare_double_float_real(local, left, right);

		case LISPTYPE_LONG_FLOAT:
			return compare_long_float_real(local, left, right);

		default:
			TypeError(left, REAL);
			break;
	}
	return 0;
}

_g int less_real_clang(LocalRoot local, addr left, addr right)
{
	return less_real(local, left, right);
}

_g int less_equal_real_clang(LocalRoot local, addr left, addr right)
{
	return less_equal_real(local, left, right);
}

_g int greater_real_clang(LocalRoot local, addr left, addr right)
{
	return greater_real(local, left, right);
}

_g int greater_equal_real_clang(LocalRoot local, addr left, addr right)
{
	return greater_equal_real(local, left, right);
}

