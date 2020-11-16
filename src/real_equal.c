#include "bignum.h"
#include "bignum_equal.h"
#include "condition.h"
#include "float_equal.h"
#include "integer.h"
#include "object.h"
#include "ratio.h"
#include "ratio_equal.h"
#include "real.h"
#include "real_equal.h"
#include "typedef.h"

int plusp_realp(addr pos, int *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			*ret = plusp_fixnum(pos);
			break;

		case LISPTYPE_BIGNUM:
			*ret = plusp_bignum(pos);
			break;

		case LISPTYPE_RATIO:
			*ret = plusp_ratio(pos);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			*ret = plusp_single_float(pos);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = plusp_double_float(pos);
			break;

		case LISPTYPE_LONG_FLOAT:
			*ret = plusp_long_float(pos);
			break;

		default:
			*ret = 0;
			return 1;
	}

	return 0;
}

int plusp_real_(addr pos, int *ret)
{
	if (plusp_realp(pos, ret))
		return TypeError_(pos, REAL);

	return 0;
}

int minusp_realp(addr pos, int *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			*ret = minusp_fixnum(pos);
			break;

		case LISPTYPE_BIGNUM:
			*ret = minusp_bignum(pos);
			break;

		case LISPTYPE_RATIO:
			*ret = minusp_ratio(pos);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			*ret = minusp_single_float(pos);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = minusp_double_float(pos);
			break;

		case LISPTYPE_LONG_FLOAT:
			*ret = minusp_long_float(pos);
			break;

		default:
			*ret = 0;
			return 1;
	}

	return 0;
}

int minusp_real_(addr pos, int *ret)
{
	if (minusp_realp(pos, ret))
		return TypeError_(pos, REAL);

	return 0;
}

int zerop_real_(addr pos, int *ret)
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

		default:
			*ret = 0;
			return TypeError_(pos, REAL);
	}

	return 0;
}

int equal_fixnum_real_(addr left, addr right, int *ret)
{
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

		default:
			*ret = 0;
			return TypeError_(right, REAL);
	}

	return 0;
}

int equal_bignum_real_(addr left, addr right, int *ret)
{
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

		default:
			*ret = 0;
			return TypeError_(right, REAL);
	}

	return 0;
}

int equal_ratio_real_(LocalRoot local, addr left, addr right, int *ret)
{
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

		default:
			*ret = 0;
			return TypeError_(right, REAL);
	}

	return 0;
}

int equal_single_float_real_(LocalRoot local, addr left, addr right, int *ret)
{
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

		default:
			*ret = 0;
			return TypeError_(right, REAL);
	}

	return 0;
}

int equal_double_float_real_(LocalRoot local, addr left, addr right, int *ret)
{
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

		default:
			*ret = 0;
			return TypeError_(right, REAL);
	}

	return 0;
}

int equal_long_float_real_(LocalRoot local, addr left, addr right, int *ret)
{
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

		default:
			*ret = 0;
			return TypeError_(right, REAL);
	}

	return 0;
}

int equal_real_(LocalRoot local, addr left, addr right, int *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return equal_fixnum_real_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return equal_bignum_real_(left, right, ret);

		case LISPTYPE_RATIO:
			return equal_ratio_real_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return equal_single_float_real_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return equal_double_float_real_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return equal_long_float_real_(local, left, right, ret);

		default:
			*ret = 0;
			return TypeError_(left, REAL);
	}
}

int not_equal_real_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	Return(equal_real_(local, left, right, &check));
	return Result(ret, !check);
}

static int compare_fixnum_real_(LocalRoot local, addr left, addr right, int *ret)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			*ret = compare_ff_real(left, right);
			break;

		case LISPTYPE_BIGNUM:
			*ret = compare_fb_real(left, right);
			break;

		case LISPTYPE_RATIO:
			*ret = compare_fr_real(local, left, right);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			*ret = compare_fs_real(left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = compare_fd_real(left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			*ret = compare_fl_real(left, right);
			break;

		default:
			*ret = 0;
			return TypeError_(right, REAL);
	}

	return 0;
}

static int compare_bignum_real_(LocalRoot local, addr left, addr right, int *ret)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			*ret = compare_bf_real(left, right);
			break;

		case LISPTYPE_BIGNUM:
			*ret = compare_bb_real(left, right);
			break;

		case LISPTYPE_RATIO:
			*ret = compare_br_real(local, left, right);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			return compare_bs_real_(left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return compare_bd_real_(left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return compare_bl_real_(left, right, ret);

		default:
			*ret = 0;
			return TypeError_(right, REAL);
	}

	return 0;
}

int compare_ratio_real_(LocalRoot local, addr left, addr right, int *ret)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			*ret = compare_rf_real(local, left, right);
			break;

		case LISPTYPE_BIGNUM:
			*ret = compare_rb_real(local, left, right);
			break;

		case LISPTYPE_RATIO:
			*ret = compare_rr_real(local, left, right);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			return compare_rs_real_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return compare_rd_real_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return compare_rl_real_(local, left, right, ret);

		default:
			*ret = 0;
			return TypeError_(right, REAL);
	}

	return 0;
}

static int compare_single_float_real_(LocalRoot local, addr left, addr right, int *ret)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			*ret = compare_sf_real(left, right);
			break;

		case LISPTYPE_BIGNUM:
			return compare_sb_real_(left, right, ret);

		case LISPTYPE_RATIO:
			return compare_sr_real_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			*ret = compare_ss_real(left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = compare_sd_real(left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			*ret = compare_sl_real(left, right);
			break;

		default:
			*ret = 0;
			return TypeError_(right, REAL);
	}

	return 0;
}

static int compare_double_float_real_(LocalRoot local, addr left, addr right, int *ret)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			*ret = compare_df_real(left, right);
			break;

		case LISPTYPE_BIGNUM:
			return compare_db_real_(left, right, ret);

		case LISPTYPE_RATIO:
			return compare_dr_real_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			*ret = compare_ds_real(left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = compare_dd_real(left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			*ret = compare_dl_real(left, right);
			break;

		default:
			*ret = 0;
			return TypeError_(right, REAL);
	}

	return 0;
}

static int compare_long_float_real_(LocalRoot local, addr left, addr right, int *ret)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			*ret = compare_lf_real(left, right);
			break;

		case LISPTYPE_BIGNUM:
			return compare_lb_real_(left, right, ret);

		case LISPTYPE_RATIO:
			return compare_lr_real_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			*ret = compare_ls_real(left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = compare_ld_real(left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			*ret = compare_ll_real(left, right);
			break;

		default:
			*ret = 0;
			return TypeError_(right, REAL);
	}

	return 0;
}

int compare_real_(LocalRoot local, addr left, addr right, int *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return compare_fixnum_real_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return compare_bignum_real_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return compare_ratio_real_(local, left, right, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return compare_single_float_real_(local, left, right, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return compare_double_float_real_(local, left, right, ret);

		case LISPTYPE_LONG_FLOAT:
			return compare_long_float_real_(local, left, right, ret);

		default:
			*ret = 0;
			return TypeError_(left, REAL);
	}
}

int less_real_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	Return(compare_real_(local, left, right, &check));
	return Result(ret, check < 0);
}

int less_equal_real_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	Return(compare_real_(local, left, right, &check));
	return Result(ret, check <= 0);
}

int greater_real_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	Return(compare_real_(local, left, right, &check));
	return Result(ret, check > 0);
}

int greater_equal_real_(LocalRoot local, addr left, addr right, int *ret)
{
	int check;
	Return(compare_real_(local, left, right, &check));
	return Result(ret, check >= 0);
}


/*
 *  debug
 */
int plusp_real_debug(addr pos)
{
	int check;

	Check(! realp(pos), "type error");
	check = 0;
	Error(plusp_real_(pos, &check));

	return check;
}

int minusp_real_debug(addr pos)
{
	int check;

	Check(! realp(pos), "type error");
	check = 0;
	Error(minusp_real_(pos, &check));

	return check;
}

int zerop_real_debug(addr pos)
{
	int check;

	Check(! realp(pos), "type error");
	check = 0;
	Error(zerop_real_(pos, &check));

	return check;
}

int equal_fixnum_real_debug(addr left, addr right)
{
	int check;

	Check(! realp(left), "type error");
	Check(! realp(right), "type error");
	check = 0;
	Error(equal_fixnum_real_(left, right, &check));

	return check;
}

int equal_bignum_real_debug(addr left, addr right)
{
	int check;

	Check(! realp(left), "type error");
	Check(! realp(right), "type error");
	check = 0;
	Error(equal_bignum_real_(left, right, &check));

	return check;
}

int equal_ratio_real_debug(LocalRoot local, addr left, addr right)
{
	int check;

	Check(! realp(left), "type error");
	Check(! realp(right), "type error");
	check = 0;
	Error(equal_ratio_real_(local, left, right, &check));

	return check;
}

int equal_single_float_real_debug(LocalRoot local, addr left, addr right)
{
	int check;

	Check(! realp(left), "type error");
	Check(! realp(right), "type error");
	check = 0;
	Error(equal_single_float_real_(local, left, right, &check));

	return check;
}

int equal_double_float_real_debug(LocalRoot local, addr left, addr right)
{
	int check;

	Check(! realp(left), "type error");
	Check(! realp(right), "type error");
	check = 0;
	Error(equal_double_float_real_(local, left, right, &check));

	return check;
}

int equal_long_float_real_debug(LocalRoot local, addr left, addr right)
{
	int check;

	Check(! realp(left), "type error");
	Check(! realp(right), "type error");
	check = 0;
	Error(equal_long_float_real_(local, left, right, &check));

	return check;
}

int equal_real_debug(LocalRoot local, addr left, addr right)
{
	int check;

	Check(! realp(left), "type error");
	Check(! realp(right), "type error");
	check = 0;
	Error(equal_real_(local, left, right, &check));

	return check;
}

int less_real_debug(LocalRoot local, addr left, addr right)
{
	int check;

	Check(! realp(left), "type error");
	Check(! realp(right), "type error");
	check = 0;
	Error(less_real_(local, left, right, &check));

	return check;
}

int less_equal_real_debug(LocalRoot local, addr left, addr right)
{
	int check;

	Check(! realp(left), "type error");
	Check(! realp(right), "type error");
	check = 0;
	Error(less_equal_real_(local, left, right, &check));

	return check;
}

int greater_real_debug(LocalRoot local, addr left, addr right)
{
	int check;

	Check(! realp(left), "type error");
	Check(! realp(right), "type error");
	check = 0;
	Error(greater_real_(local, left, right, &check));

	return check;
}

int greater_equal_real_debug(LocalRoot local, addr left, addr right)
{
	int check;

	Check(! realp(left), "type error");
	Check(! realp(right), "type error");
	check = 0;
	Error(greater_equal_real_(local, left, right, &check));

	return check;
}

