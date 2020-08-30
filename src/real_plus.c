#include "bignum_plus.h"
#include "condition.h"
#include "float_plus.h"
#include "ratio_plus.h"
#include "real_plus.h"
#include "typedef.h"

/*
 *  sign_reverse
 */
_g int sign_reverse_real_common_(addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			sigrev_fixnum_integer_common(pos, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			sigrev_bignum_integer_common(pos, ret);
			return 0;

		case LISPTYPE_RATIO:
			sign_reverse_ratio_common(pos, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			sign_reverse_floats_heap(pos, ret);
			return 0;

		case LISPTYPE_DOUBLE_FLOAT:
			sign_reverse_floatd_heap(pos, ret);
			return 0;

		case LISPTYPE_LONG_FLOAT:
			sign_reverse_floatl_heap(pos, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(pos, REAL);
	}
}

_g int sign_reverse_real_local_(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			sigrev_fixnum_integer_local(local, pos, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			sigrev_bignum_integer_local(local, pos, ret);
			return 0;

		case LISPTYPE_RATIO:
			sign_reverse_ratio_local(local, pos, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			sign_reverse_floats_local(local, pos, ret);
			return 0;

		case LISPTYPE_DOUBLE_FLOAT:
			sign_reverse_floatd_local(local, pos, ret);
			return 0;

		case LISPTYPE_LONG_FLOAT:
			sign_reverse_floatl_local(local, pos, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(pos, REAL);
	}
}


/*
 *  1+, 1-
 */
_g int oneplus_real_common_(LocalRoot local, addr value, addr *ret)
{
	switch (GetType(value)) {
		case LISPTYPE_FIXNUM:
			plus_fv_real_common(value, 1, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			plus_bv_real_common(local, value, 1, ret);
			return 0;

		case LISPTYPE_RATIO:
			plus_rv_real_common(local, value, 1, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return plus_float_sv_heap_(value, 1.0f, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_float_dv_heap_(value, 1.0, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_float_lv_heap_(value, 1.0L, ret);

		default:
			*ret = Nil;
			return TypeError_(value, REAL);
	}
}

_g int oneminus_real_common_(LocalRoot local, addr value, addr *ret)
{
	switch (GetType(value)) {
		case LISPTYPE_FIXNUM:
			plus_fv_real_common(value, -1, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			plus_bv_real_common(local, value, -1, ret);
			return 0;

		case LISPTYPE_RATIO:
			plus_rv_real_common(local, value, -1, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return plus_float_sv_heap_(value, -1.0f, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_float_dv_heap_(value, -1.0, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_float_lv_heap_(value, -1.0L, ret);

		default:
			*ret = Nil;
			return TypeError_(value, REAL);
	}
}


/*
 *  plus
 */
_g int plus_fixnum_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_ff_real_common(left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			plus_fb_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			plus_fr_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return plus_float_fs_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_float_fd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_float_fl_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

_g int plus_bignum_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_bf_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			plus_bb_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			plus_br_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return plus_float_bs_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_float_bd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_float_bl_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

_g int plus_ratio_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_rf_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			plus_rb_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			plus_rr_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return plus_float_rs_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_float_rd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_float_rl_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

_g int plus_single_real_common_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return plus_float_sf_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return plus_float_sb_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return plus_float_sr_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return plus_float_ss_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_float_sd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_float_sl_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

_g int plus_double_real_common_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return plus_float_df_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return plus_float_db_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return plus_float_dr_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return plus_float_ds_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_float_dd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_float_dl_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

_g int plus_long_real_common_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_LONG_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return plus_float_lf_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return plus_float_lb_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return plus_float_lr_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return plus_float_ls_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_float_ld_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_float_ll_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

_g int plus_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return plus_fixnum_real_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return plus_bignum_real_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return plus_ratio_real_common_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return plus_single_real_common_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_double_real_common_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_long_real_common_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

static int plus_fixnum_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	CheckType(left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_ff_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			plus_fb_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			plus_fr_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return plus_float_fs_alloc_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_float_fd_alloc_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_float_fl_alloc_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

static int plus_bignum_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	CheckType(left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_bf_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			plus_bb_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			plus_br_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return plus_float_bs_alloc_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_float_bd_alloc_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_float_bl_alloc_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

static int plus_ratio_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	CheckType(left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			plus_rf_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			plus_rb_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			plus_rr_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return plus_float_rs_alloc_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_float_rd_alloc_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_float_rl_alloc_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

static int plus_single_float_real_local_(LocalRoot local,
		addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return plus_float_sf_alloc_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return plus_float_sb_alloc_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return plus_float_sr_alloc_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return plus_float_ss_alloc_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_float_sd_alloc_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_float_sl_alloc_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

static int plus_double_float_real_local_(LocalRoot local,
		addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return plus_float_df_alloc_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return plus_float_db_alloc_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return plus_float_dr_alloc_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return plus_float_ds_alloc_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_float_dd_alloc_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_float_dl_alloc_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

static int plus_long_float_real_local_(LocalRoot local,
		addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	CheckType(left, LISPTYPE_LONG_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return plus_float_lf_alloc_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return plus_float_lb_alloc_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return plus_float_lr_alloc_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return plus_float_ls_alloc_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_float_ld_alloc_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_float_ll_alloc_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

_g int plus_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return plus_fixnum_real_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return plus_bignum_real_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return plus_ratio_real_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return plus_single_float_real_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return plus_double_float_real_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return plus_long_float_real_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}


/*
 *  minus
 */
_g int minus_fixnum_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_ff_real_common(left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			minus_fb_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			minus_fr_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_fs_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_fd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_fl_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

_g int minus_real_fixnum_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_FIXNUM);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			minus_ff_real_common(left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			minus_bf_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			minus_rf_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_sf_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_df_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_lf_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

_g int minus_bignum_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_bf_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			minus_bb_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			minus_br_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_bs_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_bd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_bl_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

_g int minus_real_bignum_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_BIGNUM);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			minus_fb_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			minus_bb_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			minus_rb_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_sb_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_db_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_lb_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

_g int minus_ratio_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_rf_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			minus_rb_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			minus_rr_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_rs_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_rd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_rl_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

_g int minus_real_ratio_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_RATIO);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			minus_fr_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			minus_br_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			minus_rr_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_sr_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_dr_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_lr_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

_g int minus_single_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return minus_float_sf_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_float_sb_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return minus_float_sr_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_ss_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_sd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_sl_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

_g int minus_real_single_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return minus_float_fs_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_float_bs_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return minus_float_rs_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_ss_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_ds_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_ls_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

_g int minus_double_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return minus_float_df_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_float_db_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return minus_float_dr_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_ds_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_dd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_dl_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

_g int minus_real_double_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return minus_float_fd_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_float_bd_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return minus_float_rd_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_sd_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_dd_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_ld_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

_g int minus_long_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_LONG_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return minus_float_lf_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_float_lb_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return minus_float_lr_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_ls_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_ld_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_ll_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

_g int minus_real_long_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_LONG_FLOAT);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return minus_float_fl_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_float_bl_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return minus_float_rl_heap_(left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_sl_heap_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_dl_heap_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_ll_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

_g int minus_real_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return minus_fixnum_real_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_bignum_real_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return minus_ratio_real_common_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return minus_single_real_common_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_double_real_common_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_long_real_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

static int minus_fixnum_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	CheckType(left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_ff_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			minus_fb_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			minus_fr_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_fs_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_fd_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_fl_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

static int minus_bignum_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	CheckType(left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_bf_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			minus_bb_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			minus_br_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_bs_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_bd_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_bl_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

static int minus_ratio_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	CheckType(left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_rf_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			minus_rb_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			minus_rr_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_rs_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_rd_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_rl_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

static int minus_single_real_local_(LocalRoot local,
		addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return minus_float_sf_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_float_sb_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return minus_float_sr_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_ss_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_sd_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_sl_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

static int minus_double_real_local_(LocalRoot local,
		addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return minus_float_df_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_float_db_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return minus_float_dr_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_ds_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_dd_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_dl_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

static int minus_long_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	CheckType(left, LISPTYPE_LONG_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return minus_float_lf_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_float_lb_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return minus_float_lr_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return minus_float_ls_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_float_ld_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_float_ll_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, REAL);
	}
}

_g int minus_real_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	Check(local == NULL, "local error");
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return minus_fixnum_real_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_bignum_real_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return minus_ratio_real_local_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return minus_single_real_local_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return minus_double_real_local_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return minus_long_real_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, REAL);
	}
}

