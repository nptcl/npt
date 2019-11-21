#include "condition.h"
#include "bignum.h"
#include "ratio.h"
#include "rational.h"
#include "integer.h"
#include "object.h"
#include "real.h"
#include "real_float.h"

_g int floatp(addr pos)
{
	enum LISPTYPE type = GetType(pos);
	return type == LISPTYPE_SINGLE_FLOAT
		|| type == LISPTYPE_DOUBLE_FLOAT
		|| type == LISPTYPE_LONG_FLOAT
		|| type == LISPTYPE_SHORT_FLOAT;
}

_g int realp(addr pos)
{
	enum LISPTYPE type = GetType(pos);
	return type == LISPTYPE_FIXNUM
		|| type == LISPTYPE_BIGNUM
		|| type == LISPTYPE_RATIO
		|| type == LISPTYPE_SINGLE_FLOAT
		|| type == LISPTYPE_DOUBLE_FLOAT
		|| type == LISPTYPE_LONG_FLOAT
		|| type == LISPTYPE_SHORT_FLOAT;
}

_g void real_result_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	if (floatp(pos))
		float_result_local(local, pos, ret);
	else
		rational_result_local(local, pos, ret);
}

_g void real_result_heap(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	if (floatp(pos))
		float_result_heap(pos, ret);
	else
		rational_result_heap(local, pos, ret);
}

_g void real_throw_alloc(LocalRoot local, addr pos, addr *ret)
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

		default:
			TypeError(pos, REAL);
			break;
	}
}

_g void real_throw_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	real_throw_alloc(local, pos, ret);
}

_g void real_throw_heap(addr pos, addr *ret)
{
	real_throw_alloc(NULL, pos, ret);
}

_g void real_copy_alloc(LocalRoot local, addr pos, addr *ret)
{
	if (floatp(pos))
		float_copy_alloc(local, pos, ret);
	else
		rational_copy_alloc(local, pos, ret);
}

_g void real_copy_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	real_copy_alloc(local, pos, ret);
}

_g void real_copy_heap(addr pos, addr *ret)
{
	real_copy_alloc(NULL, pos, ret);
}

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

_g double_float cast_double_float_unsafe(addr value)
{
	switch (GetType(value)) {
		case LISPTYPE_FIXNUM:
			return (double_float)RefFixnum(value);

		case LISPTYPE_BIGNUM:
			return double_float_bignum(value);

		case LISPTYPE_RATIO:
			return double_float_ratio(value);

		case LISPTYPE_SINGLE_FLOAT:
			return (double_float)RefSingleFloat(value);

		case LISPTYPE_DOUBLE_FLOAT:
			return RefDoubleFloat(value);

		case LISPTYPE_LONG_FLOAT:
			return (double_float)RefLongFloat(value);

		default:
			TypeError(value, REAL);
			return 0.0;
	}
}

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

