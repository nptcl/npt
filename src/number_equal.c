#include "bignum_equal.h"
#include "cmpl.h"
#include "condition.h"
#include "float_equal.h"
#include "integer.h"
#include "number_equal.h"
#include "ratio.h"
#include "ratio_equal.h"
#include "typedef.h"

_g int zerop_number(addr pos)
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

		case LISPTYPE_COMPLEX:
			return zerop_complex(pos);

		default:
			TypeError(pos, NUMBER);
			break;
	}
	return 0;
}

static int equal_fixnum_number(addr left, addr right)
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

		case LISPTYPE_COMPLEX:
			return equal_fc_number(left, right);

		default:
			TypeError(right, NUMBER);
			break;
	}
	return 0;
}

static int equal_bignum_number(addr left, addr right)
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

		case LISPTYPE_COMPLEX:
			return equal_bc_number(left, right);

		default:
			TypeError(right, NUMBER);
			break;
	}
	return 0;
}

_g int equal_ratio_number(LocalRoot local, addr left, addr right)
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

		case LISPTYPE_COMPLEX:
			return equal_rc_number(local, left, right);

		default:
			TypeError(right, NUMBER);
			break;
	}
	return 0;
}

static int equal_single_float_number(LocalRoot local, addr left, addr right)
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

		case LISPTYPE_COMPLEX:
			return equal_sc_number(local, left, right);

		default:
			TypeError(right, NUMBER);
			break;
	}
	return 0;
}

static int equal_double_float_number(LocalRoot local, addr left, addr right)
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

		case LISPTYPE_COMPLEX:
			return equal_dc_number(local, left, right);

		default:
			TypeError(right, NUMBER);
			break;
	}
	return 0;
}

static int equal_long_float_number(LocalRoot local, addr left, addr right)
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

		case LISPTYPE_COMPLEX:
			return equal_lc_number(local, left, right);

		default:
			TypeError(right, NUMBER);
			break;
	}
	return 0;
}

static int equal_complex_number(LocalRoot local, addr left, addr right)
{
	CheckType(left, LISPTYPE_COMPLEX);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return equal_cf_number(left, right);

		case LISPTYPE_BIGNUM:
			return equal_cb_number(left, right);

		case LISPTYPE_RATIO:
			return equal_cr_number(local, left, right);

		case LISPTYPE_SINGLE_FLOAT:
			return equal_cs_number(local, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return equal_cd_number(local, left, right);

		case LISPTYPE_LONG_FLOAT:
			return equal_cl_number(local, left, right);

		case LISPTYPE_COMPLEX:
			return equal_complex(local, left, right);

		default:
			TypeError(right, NUMBER);
			break;
	}
	return 0;
}

_g int equal_number(LocalRoot local, addr left, addr right)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return equal_fixnum_number(left, right);

		case LISPTYPE_BIGNUM:
			return equal_bignum_number(left, right);

		case LISPTYPE_RATIO:
			return equal_ratio_number(local, left, right);

		case LISPTYPE_SINGLE_FLOAT:
			return equal_single_float_number(local, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return equal_double_float_number(local, left, right);

		case LISPTYPE_LONG_FLOAT:
			return equal_long_float_number(local, left, right);

		case LISPTYPE_COMPLEX:
			return equal_complex_number(local, left, right);

		default:
			TypeError(left, NUMBER);
			break;
	}
	return 0;
}

