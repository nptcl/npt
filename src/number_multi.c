#include "bignum_multi.h"
#include "cmpl_multi.h"
#include "condition.h"
#include "float_multi.h"
#include "number_multi.h"
#include "ratio_multi.h"
#include "typedef.h"

/*
 *  multiple
 */
static int multi_fixnum_number_heap_(LocalRoot local,
		addr left, addr right, addr *ret)
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
			return multi_float_fs_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return multi_float_fd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return multi_float_fl_heap_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return multi_fc_number_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}

	return 0;
}

static int multi_bignum_number_heap_(LocalRoot local,
		addr left, addr right, addr *ret)
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
			return multi_float_bs_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return multi_float_bd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return multi_float_bl_heap_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return multi_bc_number_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}

	return 0;
}

static int multi_ratio_number_heap_(LocalRoot local,
		addr left, addr right, addr *ret)
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
			return multi_float_rs_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return multi_float_rd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return multi_float_rl_heap_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return multi_rc_number_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}

	return 0;
}

static int multi_single_float_number_heap_(addr left, addr right, addr *ret)
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

		case LISPTYPE_COMPLEX:
			return multi_sc_number_common_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}
}

static int multi_double_float_number_heap_(addr left, addr right, addr *ret)
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

		case LISPTYPE_COMPLEX:
			return multi_dc_number_common_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}
}

static int multi_long_float_number_heap_(addr left, addr right, addr *ret)
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

		case LISPTYPE_COMPLEX:
			return multi_lc_number_common_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}
}

static int multi_complex_number_heap_(LocalRoot local,
		addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_COMPLEX);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return multi_cf_number_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return multi_cb_number_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return multi_cr_number_common_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return multi_cs_number_common_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return multi_cd_number_common_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return multi_cl_number_common_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return multi_cc_number_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}
}

_g int multi_number_heap_(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return multi_fixnum_number_heap_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return multi_bignum_number_heap_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return multi_ratio_number_heap_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return multi_single_float_number_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return multi_double_float_number_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return multi_long_float_number_heap_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return multi_complex_number_heap_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, NUMBER);
	}
}


/*
 *  inverse
 */
_g int inverse_number_heap_(LocalRoot local, addr left, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return inverse_fixnum_common_(left, ret);

		case LISPTYPE_BIGNUM:
			return inverse_bignum_common_(left, ret);

		case LISPTYPE_RATIO:
			return inverse_ratio_common_(local, left, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return inverse_single_float_heap_(left, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return inverse_double_float_heap_(left, ret);

		case LISPTYPE_LONG_FLOAT:
			return inverse_long_float_heap_(left, ret);

		case LISPTYPE_COMPLEX:
			return inverse_complex_common_(local, left, ret);

		default:
			*ret = Nil;
			return TypeError_(left, NUMBER);
	}
}


/*
 *  division
 */
static int div_fixnum_number_heap_(LocalRoot local,
		addr left, addr right, addr *ret)
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

		case LISPTYPE_COMPLEX:
			return div_fc_number_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}
}

static int div_bignum_number_heap_(LocalRoot local,
		addr left, addr right, addr *ret)
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

		case LISPTYPE_COMPLEX:
			return div_bc_number_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}
}

static int div_ratio_number_heap_(LocalRoot local,
		addr left, addr right, addr *ret)
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

		case LISPTYPE_COMPLEX:
			return div_rc_number_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}
}

static int div_single_float_number_heap_(addr left, addr right, addr *ret)
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

		case LISPTYPE_COMPLEX:
			return div_sc_number_common_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}
}

static int div_double_float_number_heap_(addr left, addr right, addr *ret)
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

		case LISPTYPE_COMPLEX:
			return div_dc_number_common_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}
}

static int div_long_float_number_heap_(addr left, addr right, addr *ret)
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

		case LISPTYPE_COMPLEX:
			return div_lc_number_common_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}
}

static int div_complex_number_heap_(LocalRoot local,
		addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_COMPLEX);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_cf_number_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_cb_number_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_cr_number_common_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_cs_number_common_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_cd_number_common_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return div_cl_number_common_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return div_cc_number_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}
}

_g int div_number_heap_(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_fixnum_number_heap_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_bignum_number_heap_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_ratio_number_heap_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return div_single_float_number_heap_(left, right ,ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return div_double_float_number_heap_(left, right ,ret);

		case LISPTYPE_LONG_FLOAT:
			return div_long_float_number_heap_(left, right ,ret);

		case LISPTYPE_COMPLEX:
			return div_complex_number_heap_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, NUMBER);
	}
}

