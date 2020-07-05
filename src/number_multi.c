#include "bignum_multi.h"
#include "cmpl_multi.h"
#include "condition.h"
#include "float_multi.h"
#include "ratio_multi.h"
#include "typedef.h"

/*
 *  multiple
 */
static inline void multi_fixnum_number_heap(LocalRoot local,
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
			multi_float_fs_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			multi_float_fd_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			multi_float_fl_heap(left, right, ret);
			break;

		case LISPTYPE_COMPLEX:
			multi_fc_number_common(local, left, right, ret);
			break;

		default:
			TypeError(right, NUMBER);
			break;
	}
}

static inline void multi_bignum_number_heap(LocalRoot local,
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
			multi_float_bs_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			multi_float_bd_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			multi_float_bl_heap(left, right, ret);
			break;

		case LISPTYPE_COMPLEX:
			multi_bc_number_common(local, left, right, ret);
			break;

		default:
			TypeError(right, NUMBER);
			break;
	}
}

static inline void multi_ratio_number_heap(LocalRoot local,
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
			multi_float_rs_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			multi_float_rd_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			multi_float_rl_heap(left, right, ret);
			break;

		case LISPTYPE_COMPLEX:
			multi_rc_number_common(local, left, right, ret);
			break;

		default:
			TypeError(right, NUMBER);
			break;
	}
}

static inline void multi_single_float_number_heap(addr left, addr right, addr *ret)
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

		case LISPTYPE_COMPLEX:
			multi_sc_number_common(left, right, ret);
			break;

		default:
			TypeError(right, NUMBER);
			break;
	}
}

static inline void multi_double_float_number_heap(addr left, addr right, addr *ret)
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

		case LISPTYPE_COMPLEX:
			multi_dc_number_common(left, right, ret);
			break;

		default:
			TypeError(right, NUMBER);
			break;
	}
}

static inline void multi_long_float_number_heap(addr left, addr right, addr *ret)
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

		case LISPTYPE_COMPLEX:
			multi_lc_number_common(left, right, ret);
			break;

		default:
			TypeError(right, NUMBER);
			break;
	}
}

static inline void multi_complex_number_heap(LocalRoot local,
		addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_COMPLEX);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			multi_cf_number_common(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			multi_cb_number_common(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			multi_cr_number_common(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			multi_cs_number_common(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			multi_cd_number_common(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			multi_cl_number_common(left, right, ret);
			break;

		case LISPTYPE_COMPLEX:
			multi_cc_number_common(local, left, right, ret);
			break;

		default:
			TypeError(right, NUMBER);
			break;
	}
}

_g void multi_number_heap(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			multi_fixnum_number_heap(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			multi_bignum_number_heap(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			multi_ratio_number_heap(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			multi_single_float_number_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			multi_double_float_number_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			multi_long_float_number_heap(left, right, ret);
			break;

		case LISPTYPE_COMPLEX:
			multi_complex_number_heap(local, left, right, ret);
			break;

		default:
			TypeError(left, NUMBER);
			break;
	}
}


/*
 *  inverse
 */
_g void inverse_number_heap(LocalRoot local, addr left, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			inverse_fixnum_common(left, ret);
			break;

		case LISPTYPE_BIGNUM:
			inverse_bignum_common(left, ret);
			break;

		case LISPTYPE_RATIO:
			inverse_ratio_common(local, left, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			inverse_single_float_heap(left, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			inverse_double_float_heap(left, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			inverse_long_float_heap(left, ret);
			break;

		case LISPTYPE_COMPLEX:
			inverse_complex_common(local, left, ret);
			break;

		default:
			TypeError(left, NUMBER);
			break;
	}
}


/*
 *  division
 */
static inline void div_fixnum_number_heap(LocalRoot local,
		addr left, addr right, addr *ret)
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

		case LISPTYPE_COMPLEX:
			div_fc_number_common(local, left, right, ret);
			break;

		default:
			TypeError(right, NUMBER);
			break;
	}
}

static inline void div_bignum_number_heap(LocalRoot local,
		addr left, addr right, addr *ret)
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

		case LISPTYPE_COMPLEX:
			div_bc_number_common(local, left, right, ret);
			break;

		default:
			TypeError(right, NUMBER);
			break;
	}
}

static inline void div_ratio_number_heap(LocalRoot local,
		addr left, addr right, addr *ret)
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

		case LISPTYPE_COMPLEX:
			div_rc_number_common(local, left, right, ret);
			break;

		default:
			TypeError(right, NUMBER);
			break;
	}
}

static inline void div_single_float_number_heap(addr left, addr right, addr *ret)
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

		case LISPTYPE_COMPLEX:
			div_sc_number_common(left, right, ret);
			break;

		default:
			TypeError(right, NUMBER);
			break;
	}
}

static inline void div_double_float_number_heap(addr left, addr right, addr *ret)
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

		case LISPTYPE_COMPLEX:
			div_dc_number_common(left, right, ret);
			break;

		default:
			TypeError(right, NUMBER);
			break;
	}
}

static inline void div_long_float_number_heap(addr left, addr right, addr *ret)
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

		case LISPTYPE_COMPLEX:
			div_lc_number_common(left, right, ret);
			break;

		default:
			TypeError(right, NUMBER);
			break;
	}
}

static inline void div_complex_number_heap(LocalRoot local,
		addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_COMPLEX);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			div_cf_number_common(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			div_cb_number_common(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			div_cr_number_common(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			div_cs_number_common(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			div_cd_number_common(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			div_cl_number_common(left, right, ret);
			break;

		case LISPTYPE_COMPLEX:
			div_cc_number_common(local, left, right, ret);
			break;

		default:
			TypeError(right, NUMBER);
			break;
	}
}

_g void div_number_heap(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			div_fixnum_number_heap(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			div_bignum_number_heap(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			div_ratio_number_heap(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			div_single_float_number_heap(left, right ,ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			div_double_float_number_heap(left, right ,ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			div_long_float_number_heap(left, right ,ret);
			break;

		case LISPTYPE_COMPLEX:
			div_complex_number_heap(local, left, right, ret);
			break;

		default:
			TypeError(left, NUMBER);
			break;
	}
}

