#include "bignum.h"
#include "cmpl.h"
#include "cmpl_multi.h"
#include "cmpl_plus.h"
#include "condition.h"
#include "integer.h"
#include "memory.h"
#include "number.h"
#include "object.h"
#include "ratio.h"
#include "rational.h"
#include "real_float.h"

int numberp(addr pos)
{
	enum LISPTYPE type = GetType(pos);
	return type == LISPTYPE_FIXNUM
		|| type == LISPTYPE_BIGNUM
		|| type == LISPTYPE_RATIO
		|| type == LISPTYPE_SINGLE_FLOAT
		|| type == LISPTYPE_DOUBLE_FLOAT
		|| type == LISPTYPE_LONG_FLOAT
		|| type == LISPTYPE_COMPLEX
		|| type == LISPTYPE_SHORT_FLOAT;
}

void number_result_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	if (complexp(pos))
		complex_result_local(local, pos, ret);
	else
		rational_result_local(local, pos, ret);
}

void number_result_heap(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	if (complexp(pos))
		complex_result_heap(local, pos, ret);
	else
		rational_result_heap(local, pos, ret);
}

void number_throw_alloc(LocalRoot local, addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			fixnum_throw_alloc(local, pos, ret);
			break;

		case LISPTYPE_BIGNUM:
			bignum_throw_alloc(local, pos, ret);
			break;

		case LISPTYPE_RATIO:
			ratio_throw_alloc(local, pos, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			single_float_throw_alloc(local, pos, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			double_float_throw_alloc(local, pos, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			long_float_throw_alloc(local, pos, ret);
			break;

		case LISPTYPE_COMPLEX:
			complex_throw_alloc(local, pos, ret);
			break;

		default:
			TypeError(pos, REAL);
			break;
	}
}

void number_throw_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	number_throw_alloc(local, pos, ret);
}

void number_throw_heap(addr pos, addr *ret)
{
	number_throw_alloc(NULL, pos, ret);
}

void number_copy_alloc(LocalRoot local, addr pos, addr *ret)
{
	if (complexp(pos))
		complex_copy_alloc(local, pos, ret);
	else
		real_copy_alloc(local, pos, ret);
}

void number_copy_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	number_copy_alloc(local, pos, ret);
}

void number_copy_heap(addr pos, addr *ret)
{
	number_copy_alloc(NULL, pos, ret);
}


/*
 *  number function
 */
int zerop_number(addr pos)
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

int equal_ratio_number(LocalRoot local, addr left, addr right)
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

int equal_number(LocalRoot local, addr left, addr right)
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


/*
 *  operator
 */
void oneplus_number_heap(LocalRoot local, addr value, addr *ret)
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
			oneplus_complex_heap(local, value, ret);
			break;

		default:
			TypeError(value, NUMBER);
			break;
	}
}

void oneminus_number_heap(LocalRoot local, addr value, addr *ret)
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
			oneminus_complex_heap(local, value, ret);
			break;

		default:
			TypeError(value, NUMBER);
			break;
	}
}

void sign_reverse_number_common(addr left, addr *ret)
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
			TypeError(left, NUMBER);
			break;
	}
}

void sign_reverse_number_local(LocalRoot local, addr left, addr *ret)
{
	Check(local == NULL, "local error");
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			sigrev_fixnum_integer_local(local, left, ret);
			break;

		case LISPTYPE_BIGNUM:
			sigrev_bignum_integer_local(local, left, ret);
			break;

		case LISPTYPE_RATIO:
			sign_reverse_ratio_local(local, left, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			sign_reverse_floats_local(local, left, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			sign_reverse_floatd_local(local, left, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			sign_reverse_floatl_local(local, left, ret);
			break;

		case LISPTYPE_COMPLEX:
			sign_reverse_complex_local(local, left, ret);
			break;

		default:
			TypeError(left, NUMBER);
			break;
	}
}


/*
 *  plus
 */
static void plus_fixnum_number_heap(LocalRoot local, addr left, addr right, addr *ret)
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
			plus_fc_number_common(local, left, right, ret);
			break;

		default:
			TypeError(right, NUMBER);
			break;
	}
}

static void plus_bignum_number_heap(LocalRoot local, addr left, addr right, addr *ret)
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
			plus_bc_number_common(local, left, right, ret);
			break;

		default:
			TypeError(right, NUMBER);
			break;
	}
}

static void plus_ratio_number_heap(LocalRoot local, addr left, addr right, addr *ret)
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
			plus_rc_number_common(local, left, right, ret);
			break;

		default:
			TypeError(right, NUMBER);
			break;
	}
}

static void plus_single_float_number_heap(addr left, addr right, addr *ret)
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
			plus_sc_number_common(left, right, ret);
			break;

		default:
			TypeError(right, NUMBER);
			break;
	}
}

static void plus_double_float_number_heap(addr left, addr right, addr *ret)
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
			plus_dc_number_common(left, right, ret);
			break;

		default:
			TypeError(right, NUMBER);
			break;
	}
}

static void plus_long_float_number_heap(addr left, addr right, addr *ret)
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
			plus_lc_number_common(left, right, ret);
			break;

		default:
			TypeError(right, NUMBER);
			break;
	}
}

static inline void plus_complex_number_heap(LocalRoot local,
		addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_COMPLEX);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_cf_number_common(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			plus_cb_number_common(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			plus_cr_number_common(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			plus_cs_number_common(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			plus_cd_number_common(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			plus_cl_number_common(left, right, ret);
			break;

		case LISPTYPE_COMPLEX:
			plus_cc_number_common(local, left, right, ret);
			break;

		default:
			TypeError(right, NUMBER);
			break;
	}
}

void plus_number_heap(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			plus_fixnum_number_heap(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			plus_bignum_number_heap(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			plus_ratio_number_heap(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			plus_single_float_number_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			plus_double_float_number_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			plus_long_float_number_heap(left, right, ret);
			break;

		case LISPTYPE_COMPLEX:
			plus_complex_number_heap(local, left, right, ret);
			break;

		default:
			TypeError(left, NUMBER);
			break;
	}
}


/*
 *  minus
 */
static inline void minus_fixnum_number_heap(LocalRoot local,
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
			minus_fc_number_common(local, left, right, ret);
			break;

		default:
			TypeError(right, NUMBER);
			break;
	}
}

static inline void minus_bignum_number_heap(LocalRoot local,
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
			minus_bc_number_common(local, left, right, ret);
			break;

		default:
			TypeError(right, NUMBER);
			break;
	}
}

static inline void minus_ratio_number_heap(LocalRoot local,
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
			minus_rc_number_common(local, left, right, ret);
			break;

		default:
			TypeError(right, NUMBER);
			break;
	}
}

static inline void minus_single_float_number_heap(addr left, addr right, addr *ret)
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
			minus_sc_number_common(left, right, ret);
			break;

		default:
			TypeError(right, NUMBER);
			break;
	}
}

static inline void minus_double_float_number_heap(addr left, addr right, addr *ret)
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
			minus_dc_number_common(left, right, ret);
			break;

		default:
			TypeError(right, NUMBER);
			break;
	}
}

static inline void minus_long_float_number_heap(addr left, addr right, addr *ret)
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
			minus_lc_number_common(left, right, ret);
			break;

		default:
			TypeError(right, NUMBER);
			break;
	}
}

static inline void minus_complex_number_heap(LocalRoot local,
		addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_COMPLEX);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_cf_number_common(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			minus_cb_number_common(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			minus_cr_number_common(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			minus_cs_number_common(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			minus_cd_number_common(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			minus_cl_number_common(left, right, ret);
			break;

		case LISPTYPE_COMPLEX:
			minus_cc_number_common(local, left, right, ret);
			break;

		default:
			TypeError(right, NUMBER);
			break;
	}
}

void minus_number_heap(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			minus_fixnum_number_heap(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			minus_bignum_number_heap(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			minus_ratio_number_heap(local, left, right, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			minus_single_float_number_heap(left, right, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			minus_double_float_number_heap(left, right, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			minus_long_float_number_heap(left, right, ret);
			break;

		case LISPTYPE_COMPLEX:
			minus_complex_number_heap(local, left, right, ret);
			break;

		default:
			TypeError(left, NUMBER);
			break;
	}
}

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

void multi_number_heap(LocalRoot local, addr left, addr right, addr *ret)
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
void inverse_number_heap(LocalRoot local, addr left, addr *ret)
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

void div_number_heap(LocalRoot local, addr left, addr right, addr *ret)
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


/*
 *  abs
 */
void abs_number_common(LocalRoot local, addr left, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			abs_fixnum_integer_common(left, ret);
			break;

		case LISPTYPE_BIGNUM:
			abs_bignum_integer_common(left, ret);
			break;

		case LISPTYPE_RATIO:
			abs_ratio_heap(left, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			abs_floats_heap(left, ret);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			abs_floatd_heap(left, ret);
			break;

		case LISPTYPE_LONG_FLOAT:
			abs_floatl_heap(left, ret);
			break;

		case LISPTYPE_COMPLEX:
			abs_complex_common(local, left, ret);
			break;

		default:
			TypeError(left, NUMBER);
			break;
	}
}

void random_number_heap(addr pos, addr state, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			fmte("TODO", NULL);
			break;

		case LISPTYPE_BIGNUM:
			fmte("TODO", NULL);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			fmte("TODO", NULL);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			fmte("TODO", NULL);
			break;

		case LISPTYPE_LONG_FLOAT:
			fmte("TODO", NULL);
			break;

		default:
			fmte("The random number ~S must be an integer or float.", pos, NULL);
			break;
	}
}

