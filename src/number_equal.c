#include "bignum_equal.h"
#include "cmpl.h"
#include "condition.h"
#include "float_equal.h"
#include "integer.h"
#include "number_equal.h"
#include "ratio.h"
#include "ratio_equal.h"
#include "typedef.h"

_g int zerop_numberp(addr pos, int *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			*ret = zerop_fixnum(pos);
			break;

		case LISPTYPE_BIGNUM:
			*ret = zerop_bignum(pos);
			break;

		case LISPTYPE_RATIO:
			*ret = zerop_ratio(pos);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			*ret = zerop_single_float(pos);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = zerop_double_float(pos);
			break;

		case LISPTYPE_LONG_FLOAT:
			*ret = zerop_long_float(pos);
			break;

		case LISPTYPE_COMPLEX:
			return zerop_complex_(pos, ret);

		default:
			*ret = 0;
			return 1;
	}

	return 0;
}

_g int zerop_number_(addr pos, int *ret)
{
	if (zerop_numberp(pos, ret))
		return TypeError_(pos, NUMBER);

	return 0;
}

static int equal_fixnum_number_(addr left, addr right, int *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			*ret = equal_ff_real(left, right);
			break;

		case LISPTYPE_BIGNUM:
			*ret = equal_fb_real(left, right);
			break;

		case LISPTYPE_RATIO:
			*ret = equal_fr_real(left, right);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			*ret = equal_fs_real(left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = equal_fd_real(left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			*ret = equal_fl_real(left, right);
			break;

		case LISPTYPE_COMPLEX:
			return equal_fc_number_(left, right, ret);

		default:
			*ret = 0;
			return TypeError_(right, NUMBER);
	}

	return 0;
}

static int equal_bignum_number_(addr left, addr right, int *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			*ret = equal_bf_real(left, right);
			break;

		case LISPTYPE_BIGNUM:
			*ret = equal_bb_real(left, right);
			break;

		case LISPTYPE_RATIO:
			*ret = equal_br_real(left, right);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			return equal_bs_real_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return equal_bd_real_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return equal_bl_real_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return equal_bc_number_(left, right, ret);

		default:
			*ret = 0;
			return TypeError_(right, NUMBER);
	}

	return 0;
}

static int equal_ratio_number_(LocalRoot local, addr left, addr right, int *ret)
{
	CheckType(left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			*ret = equal_rf_real(left, right);
			break;

		case LISPTYPE_BIGNUM:
			*ret = equal_rb_real(left, right);
			break;

		case LISPTYPE_RATIO:
			*ret = equal_rr_real(left, right);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			return equal_rs_real_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return equal_rd_real_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return equal_rl_real_(local, left, right, ret);

		case LISPTYPE_COMPLEX:
			return equal_rc_number_(local, left, right, ret);

		default:
			*ret = 0;
			return TypeError_(right, NUMBER);
	}

	return 0;
}

static int equal_single_float_number_(LocalRoot local, addr left, addr right, int *ret)
{
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			*ret = equal_sf_real(left, right);
			break;

		case LISPTYPE_BIGNUM:
			return equal_sb_real_(left, right, ret);

		case LISPTYPE_RATIO:
			return equal_sr_real_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			*ret = equal_ss_real(left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = equal_sd_real(left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			*ret = equal_sl_real(left, right);
			break;

		case LISPTYPE_COMPLEX:
			return equal_sc_number_(local, left, right, ret);

		default:
			*ret = 0;
			return TypeError_(right, NUMBER);
	}

	return 0;
}

static int equal_double_float_number_(LocalRoot local, addr left, addr right, int *ret)
{
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			*ret = equal_df_real(left, right);
			break;

		case LISPTYPE_BIGNUM:
			return equal_db_real_(left, right, ret);

		case LISPTYPE_RATIO:
			return equal_dr_real_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			*ret = equal_ds_real(left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = equal_dd_real(left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			*ret = equal_dl_real(left, right);
			break;

		case LISPTYPE_COMPLEX:
			return equal_dc_number_(local, left, right, ret);

		default:
			*ret = 0;
			return TypeError_(right, NUMBER);
	}

	return 0;
}

static int equal_long_float_number_(LocalRoot local, addr left, addr right, int *ret)
{
	CheckType(left, LISPTYPE_LONG_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			*ret = equal_lf_real(left, right);
			break;

		case LISPTYPE_BIGNUM:
			return equal_lb_real_(left, right, ret);

		case LISPTYPE_RATIO:
			return equal_lr_real_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			*ret = equal_ls_real(left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = equal_ld_real(left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			*ret = equal_ll_real(left, right);
			break;

		case LISPTYPE_COMPLEX:
			return equal_lc_number_(local, left, right, ret);

		default:
			*ret = 0;
			return TypeError_(right, NUMBER);
	}

	return 0;
}

static int equal_complex_number_(LocalRoot local, addr left, addr right, int *ret)
{
	CheckType(left, LISPTYPE_COMPLEX);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return equal_cf_number_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return equal_cb_number_(left, right, ret);

		case LISPTYPE_RATIO:
			return equal_cr_number_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return equal_cs_number_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return equal_cd_number_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return equal_cl_number_(local, left, right, ret);

		case LISPTYPE_COMPLEX:
			return equal_complex_(local, left, right, ret);

		default:
			*ret = 0;
			return TypeError_(right, NUMBER);
	}
}

_g int equal_number_(LocalRoot local, addr left, addr right, int *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return equal_fixnum_number_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return equal_bignum_number_(left, right, ret);

		case LISPTYPE_RATIO:
			return equal_ratio_number_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return equal_single_float_number_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return equal_double_float_number_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return equal_long_float_number_(local, left, right, ret);

		case LISPTYPE_COMPLEX:
			return equal_complex_number_(local, left, right, ret);

		default:
			*ret = 0;
			return TypeError_(left, NUMBER);
	}
}

_g int not_equal_number_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	Return(equal_number_(local, left, right, &check));
	return Result(ret, ! check);
}

