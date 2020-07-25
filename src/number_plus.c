#include "bignum_plus.h"
#include "cmpl.h"
#include "cmpl_plus.h"
#include "condition.h"
#include "float_plus.h"
#include "ratio_plus.h"
#include "typedef.h"

/*
 *  1+, 1-
 */
_g int oneplus_number_common_(LocalRoot local, addr value, addr *ret)
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

		case LISPTYPE_COMPLEX:
			return oneplus_complex_heap_(local, value, ret);

		default:
			*ret = Nil;
			return TypeError_(value, NUMBER);
	}

	return 0;
}

_g int oneminus_number_common_(LocalRoot local, addr value, addr *ret)
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

		case LISPTYPE_COMPLEX:
			return oneminus_complex_heap_(local, value, ret);

		default:
			*ret = Nil;
			return TypeError_(value, NUMBER);
	}

	return 0;
}

_g int sign_reverse_number_common_(addr left, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			sigrev_fixnum_integer_common(left, ret);
			break;

		case LISPTYPE_BIGNUM:
			sigrev_bignum_integer_common(left, ret);
			break;

		case LISPTYPE_RATIO:
			sign_reverse_ratio_common(left, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			sign_reverse_floats_heap(left, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			sign_reverse_floatd_heap(left, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			sign_reverse_floatl_heap(left, ret);
			break;

		case LISPTYPE_COMPLEX:
			sign_reverse_complex_common(left, ret);
			break;

		default:
			*ret = Nil;
			return TypeError_(left, NUMBER);
	}

	return 0;
}


/*
 *  plus
 */
static int plus_fixnum_number_heap_(LocalRoot local, addr left, addr right, addr *ret)
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

		case LISPTYPE_COMPLEX:
			return plus_fc_number_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}

	return 0;
}

static int plus_bignum_number_heap_(LocalRoot local, addr left, addr right, addr *ret)
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

		case LISPTYPE_COMPLEX:
			return plus_bc_number_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}

	return 0;
}

static int plus_ratio_number_heap_(LocalRoot local, addr left, addr right, addr *ret)
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

		case LISPTYPE_COMPLEX:
			return plus_rc_number_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}

	return 0;
}

static int plus_single_float_number_heap_(addr left, addr right, addr *ret)
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

		case LISPTYPE_COMPLEX:
			return plus_sc_number_common_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}

	return 0;
}

static int plus_double_float_number_heap_(addr left, addr right, addr *ret)
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

		case LISPTYPE_COMPLEX:
			return plus_dc_number_common_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}

	return 0;
}

static int plus_long_float_number_heap_(addr left, addr right, addr *ret)
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

		case LISPTYPE_COMPLEX:
			return plus_lc_number_common_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}

	return 0;
}

static int plus_complex_number_heap_(LocalRoot local,
		addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_COMPLEX);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return plus_cf_number_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return plus_cb_number_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return plus_cr_number_common_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return plus_cs_number_common_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_cd_number_common_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_cl_number_common_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return plus_cc_number_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}
}

_g int plus_number_heap_(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return plus_fixnum_number_heap_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return plus_bignum_number_heap_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return plus_ratio_number_heap_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return plus_single_float_number_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_double_float_number_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_long_float_number_heap_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return plus_complex_number_heap_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, NUMBER);
	}
}


/*
 *  minus
 */
static int minus_fixnum_number_heap_(LocalRoot local,
		addr left, addr right, addr *ret)
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

		case LISPTYPE_COMPLEX:
			return minus_fc_number_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}

	return 0;
}

static int minus_bignum_number_heap_(LocalRoot local,
		addr left, addr right, addr *ret)
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

		case LISPTYPE_COMPLEX:
			return minus_bc_number_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}

	return 0;
}

static int minus_ratio_number_heap_(LocalRoot local,
		addr left, addr right, addr *ret)
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

		case LISPTYPE_COMPLEX:
			return minus_rc_number_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}

	return 0;
}

static int minus_single_float_number_heap_(addr left, addr right, addr *ret)
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

		case LISPTYPE_COMPLEX:
			return minus_sc_number_common_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}

	return 0;
}

static int minus_double_float_number_heap_(addr left, addr right, addr *ret)
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

		case LISPTYPE_COMPLEX:
			return minus_dc_number_common_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}

	return 0;
}

static int minus_long_float_number_heap_(addr left, addr right, addr *ret)
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

		case LISPTYPE_COMPLEX:
			return minus_lc_number_common_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}

	return 0;
}

static int minus_complex_number_heap_(LocalRoot local,
		addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_COMPLEX);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return minus_cf_number_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_cb_number_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return minus_cr_number_common_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return minus_cs_number_common_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_cd_number_common_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_cl_number_common_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return minus_cc_number_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, NUMBER);
	}
}

_g int minus_number_heap_(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return minus_fixnum_number_heap_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_bignum_number_heap_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return minus_ratio_number_heap_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return minus_single_float_number_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_double_float_number_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_long_float_number_heap_(left, right, ret);

		case LISPTYPE_COMPLEX:
			return minus_complex_number_heap_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, NUMBER);
	}
}

