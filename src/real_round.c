#include "bignum.h"
#include "bignum_data.h"
#include "bignum_object.h"
#include "condition.h"
#include "ratio.h"
#include "real_division.h"
#include "real_round.h"

/*
 *  common
 */
static void round1_float(LocalRoot local, addr *quot, addr *rem, addr left)
{
	single_float v, r;

	GetSingleFloat(left, &v);
	v = lisp_round1_s(v, &r);
	single_float_integer_heap(local, quot, v);
	single_float_heap(rem, r);
}

static void round1_double(LocalRoot local, addr *quot, addr *rem, addr left)
{
	double_float v, r;

	GetDoubleFloat(left, &v);
	v = lisp_round1_d(v, &r);
	double_float_integer_heap(local, quot, v);
	double_float_heap(rem, r);
}

static void round1_long(LocalRoot local, addr *quot, addr *rem, addr left)
{
	long_float v, r;

	GetLongFloat(left, &v);
	v = lisp_round1_l(v, &r);
	long_float_integer_heap(local, quot, v);
	long_float_heap(rem, r);
}

_g void round1_common(LocalRoot local, addr *quot, addr *rem, addr left)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			fixnum_throw_heap(left, quot);
			fixnum_heap(rem, 0);
			break;

		case LISPTYPE_BIGNUM:
			bignum_throw_heap(left, quot);
			fixnum_heap(rem, 0);
			break;

		case LISPTYPE_RATIO:
			lisp_round1_ratio(local, quot, rem, left);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			round1_float(local, quot, rem, left);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			round1_double(local, quot, rem, left);
			break;

		case LISPTYPE_LONG_FLOAT:
			round1_long(local, quot, rem, left);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

static void fround1_float(LocalRoot local, addr *quot, addr *rem, addr left)
{
	single_float v, r;

	GetSingleFloat(left, &v);
	v = lisp_round1_s(v, &r);
	single_float_heap(quot, v);
	single_float_heap(rem, r);
}

static void fround1_double(LocalRoot local, addr *quot, addr *rem, addr left)
{
	double_float v, r;

	GetDoubleFloat(left, &v);
	v = lisp_round1_d(v, &r);
	double_float_heap(quot, v);
	double_float_heap(rem, r);
}

static void fround1_long(LocalRoot local, addr *quot, addr *rem, addr left)
{
	long_float v, r;

	GetLongFloat(left, &v);
	v = lisp_round1_l(v, &r);
	long_float_heap(quot, v);
	long_float_heap(rem, r);
}

_g void fround1_common(LocalRoot local, addr *quot, addr *rem, addr left)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			single_float_fixnum_heap(quot, left);
			fixnum_heap(rem, 0);
			break;

		case LISPTYPE_BIGNUM:
			single_float_bignum_heap(quot, left);
			fixnum_heap(rem, 0);
			break;

		case LISPTYPE_RATIO:
			lisp_fround1_ratio(local, quot, rem, left);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			fround1_float(local, quot, rem, left);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			fround1_double(local, quot, rem, left);
			break;

		case LISPTYPE_LONG_FLOAT:
			fround1_long(local, quot, rem, left);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

static void round_ff_common(addr *quot, addr *rem, addr left, addr right)
{
	fixnum a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	GetFixnum(left, &a);
	GetFixnum(right, &b);
	lisp_round_fixnum(quot, rem, a, b);
}

static void round_fb_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_BIGNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	lisp_round_bignum(local, quot, rem, left, right);
	rollback_local(local, stack);
}

static void round_fr_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_RATIO);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	lisp_round_br_ratio(local, quot, rem, left, right);
	rollback_local(local, stack);
}

static void round_fs_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = single_float_fixnum(left);
	b = RefSingleFloat(right);
	a = lisp_round_s(a, b, &b);
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);
}

static void round_fd_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = double_float_fixnum(left);
	b = RefDoubleFloat(right);
	a = lisp_round_d(a, b, &b);
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);
}

static void round_fl_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = long_float_fixnum(left);
	b = RefLongFloat(right);
	a = lisp_round_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void round_fixnum_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			round_ff_common(quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			round_fb_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			round_fr_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			round_fs_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			round_fd_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			round_fl_common(local, quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void round_bf_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	lisp_round_bignum(local, quot, rem, left, right);
	rollback_local(local, stack);
}

static void round_bs_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = single_float_bignum(left);
	b = RefSingleFloat(right);
	a = lisp_round_s(a, b, &b);
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);
}

static void round_bd_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = double_float_bignum(left);
	b = RefDoubleFloat(right);
	a = lisp_round_d(a, b, &b);
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);
}

static void round_bl_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = long_float_bignum(left);
	b = RefLongFloat(right);
	a = lisp_round_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void round_bignum_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			round_bf_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			lisp_round_bignum(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			lisp_round_br_ratio(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			round_bs_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			round_bd_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			round_bl_common(local, quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void round_rf_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	lisp_round_rb_ratio(local, quot, rem, left, right);
	rollback_local(local, stack);
}

static void round_rs_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = single_float_ratio(left);
	b = RefSingleFloat(right);
	a = lisp_round_s(a, b, &b);
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);
}

static void round_rd_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = double_float_ratio(left);
	b = RefDoubleFloat(right);
	a = lisp_round_d(a, b, &b);
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);
}

static void round_rl_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = long_float_ratio(left);
	b = RefLongFloat(right);
	a = lisp_round_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void round_ratio_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			round_rf_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			lisp_round_rb_ratio(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			lisp_round_rr_ratio(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			round_rs_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			round_rd_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			round_rl_common(local, quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void round_sf_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefSingleFloat(left);
	b = single_float_fixnum(right);
	a = lisp_round_s(a, b, &b);
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);
}

static void round_sb_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefSingleFloat(left);
	b = single_float_bignum(right);
	a = lisp_round_s(a, b, &b);
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);
}

static void round_sr_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefSingleFloat(left);
	b = single_float_ratio(right);
	a = lisp_round_s(a, b, &b);
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);
}

static void round_ss_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefSingleFloat(left);
	b = RefSingleFloat(right);
	a = lisp_round_s(a, b, &b);
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);
}

static void round_sd_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = (double_float)RefSingleFloat(left);
	b = RefDoubleFloat(right);
	a = lisp_round_d(a, b, &b);
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);
}

static void round_sl_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefSingleFloat(left);
	b = RefLongFloat(right);
	a = lisp_round_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void round_single_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			round_sf_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			round_sb_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			round_sr_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			round_ss_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			round_sd_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			round_sl_common(local, quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void round_df_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefDoubleFloat(left);
	b = double_float_fixnum(right);
	a = lisp_round_d(a, b, &b);
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);
}

static void round_db_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefDoubleFloat(left);
	b = double_float_bignum(right);
	a = lisp_round_d(a, b, &b);
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);
}

static void round_dr_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefDoubleFloat(left);
	b = double_float_ratio(right);
	a = lisp_round_d(a, b, &b);
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);
}

static void round_ds_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefDoubleFloat(left);
	b = (double_float)RefSingleFloat(right);
	a = lisp_round_d(a, b, &b);
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);
}

static void round_dd_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefDoubleFloat(left);
	b = RefDoubleFloat(right);
	a = lisp_round_d(a, b, &b);
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);
}

static void round_dl_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefDoubleFloat(left);
	b = RefLongFloat(right);
	a = lisp_round_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void round_double_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			round_df_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			round_db_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			round_dr_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			round_ds_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			round_dd_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			round_dl_common(local, quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void round_lf_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefLongFloat(left);
	b = long_float_fixnum(right);
	a = lisp_round_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void round_lb_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefLongFloat(left);
	b = long_float_bignum(right);
	a = lisp_round_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void round_lr_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefLongFloat(left);
	b = long_float_ratio(right);
	a = lisp_round_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void round_ls_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefSingleFloat(right);
	a = lisp_round_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void round_ld_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefDoubleFloat(right);
	a = lisp_round_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void round_ll_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = RefLongFloat(left);
	b = RefLongFloat(right);
	a = lisp_round_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void round_long_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			round_lf_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			round_lb_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			round_lr_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			round_ls_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			round_ld_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			round_ll_common(local, quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void round2_common(LocalRoot local, addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			round_fixnum_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			round_bignum_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			round_ratio_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			round_single_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			round_double_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			round_long_common(local, quot, rem, left, right);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

static void fround_ff_common(addr *quot, addr *rem, addr left, addr right)
{
	fixnum a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	GetFixnum(left, &a);
	GetFixnum(right, &b);
	lisp_fround_fixnum(quot, rem, a, b);
}

static void fround_fb_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_BIGNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	lisp_fround_bignum(local, quot, rem, left, right);
	rollback_local(local, stack);
}

static void fround_fr_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_RATIO);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	lisp_fround_br_ratio(local, quot, rem, left, right);
	rollback_local(local, stack);
}

static void fround_fs_common(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = single_float_fixnum(left);
	b = RefSingleFloat(right);
	a = lisp_round_s(a, b, &b);
	single_float_heap(quot, a);
	single_float_heap(rem, b);
}

static void fround_fd_common(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = double_float_fixnum(left);
	b = RefDoubleFloat(right);
	a = lisp_round_d(a, b, &b);
	double_float_heap(quot, a);
	double_float_heap(rem, b);
}

static void fround_fl_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = long_float_fixnum(left);
	b = RefLongFloat(right);
	a = lisp_round_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void fround_fixnum_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			fround_ff_common(quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			fround_fb_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			fround_fr_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			fround_fs_common(quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			fround_fd_common(quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			fround_fl_common(quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void fround_bf_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	lisp_fround_bignum(local, quot, rem, left, right);
	rollback_local(local, stack);
}

static void fround_bs_common(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = single_float_bignum(left);
	b = RefSingleFloat(right);
	a = lisp_round_s(a, b, &b);
	single_float_heap(quot, a);
	single_float_heap(rem, b);
}

static void fround_bd_common(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = double_float_bignum(left);
	b = RefDoubleFloat(right);
	a = lisp_round_d(a, b, &b);
	double_float_heap(quot, a);
	double_float_heap(rem, b);
}

static void fround_bl_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = long_float_bignum(left);
	b = RefLongFloat(right);
	a = lisp_round_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void fround_bignum_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			fround_bf_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			lisp_fround_bignum(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			lisp_fround_br_ratio(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			fround_bs_common(quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			fround_bd_common(quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			fround_bl_common(quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void fround_rf_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	lisp_fround_rb_ratio(local, quot, rem, left, right);
	rollback_local(local, stack);
}

static void fround_rs_common(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = single_float_ratio(left);
	b = RefSingleFloat(right);
	a = lisp_round_s(a, b, &b);
	single_float_heap(quot, a);
	single_float_heap(rem, b);
}

static void fround_rd_common(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = double_float_ratio(left);
	b = RefDoubleFloat(right);
	a = lisp_round_d(a, b, &b);
	double_float_heap(quot, a);
	double_float_heap(rem, b);
}

static void fround_rl_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = long_float_ratio(left);
	b = RefLongFloat(right);
	a = lisp_round_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void fround_ratio_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			fround_rf_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			lisp_fround_rb_ratio(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			lisp_fround_rr_ratio(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			fround_rs_common(quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			fround_rd_common(quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			fround_rl_common(quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void fround_sf_common(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefSingleFloat(left);
	b = single_float_fixnum(right);
	a = lisp_round_s(a, b, &b);
	single_float_heap(quot, a);
	single_float_heap(rem, b);
}

static void fround_sb_common(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefSingleFloat(left);
	b = single_float_bignum(right);
	a = lisp_round_s(a, b, &b);
	single_float_heap(quot, a);
	single_float_heap(rem, b);
}

static void fround_sr_common(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefSingleFloat(left);
	b = single_float_ratio(right);
	a = lisp_round_s(a, b, &b);
	single_float_heap(quot, a);
	single_float_heap(rem, b);
}

static void fround_ss_common(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefSingleFloat(left);
	b = RefSingleFloat(right);
	a = lisp_round_s(a, b, &b);
	single_float_heap(quot, a);
	single_float_heap(rem, b);
}

static void fround_sd_common(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = (double_float)RefSingleFloat(left);
	b = RefDoubleFloat(right);
	a = lisp_round_d(a, b, &b);
	double_float_heap(quot, a);
	double_float_heap(rem, b);
}

static void fround_sl_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefSingleFloat(left);
	b = RefLongFloat(right);
	a = lisp_round_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void fround_single_common(addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			fround_sf_common(quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			fround_sb_common(quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			fround_sr_common(quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			fround_ss_common(quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			fround_sd_common(quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			fround_sl_common(quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void fround_df_common(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefDoubleFloat(left);
	b = double_float_fixnum(right);
	a = lisp_round_d(a, b, &b);
	double_float_heap(quot, a);
	double_float_heap(rem, b);
}

static void fround_db_common(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefDoubleFloat(left);
	b = double_float_bignum(right);
	a = lisp_round_d(a, b, &b);
	double_float_heap(quot, a);
	double_float_heap(rem, b);
}

static void fround_dr_common(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefDoubleFloat(left);
	b = double_float_ratio(right);
	a = lisp_round_d(a, b, &b);
	double_float_heap(quot, a);
	double_float_heap(rem, b);
}

static void fround_ds_common(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefDoubleFloat(left);
	b = (double_float)RefSingleFloat(right);
	a = lisp_round_d(a, b, &b);
	double_float_heap(quot, a);
	double_float_heap(rem, b);
}

static void fround_dd_common(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefDoubleFloat(left);
	b = RefDoubleFloat(right);
	a = lisp_round_d(a, b, &b);
	double_float_heap(quot, a);
	double_float_heap(rem, b);
}

static void fround_dl_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefDoubleFloat(left);
	b = RefLongFloat(right);
	a = lisp_round_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void fround_double_common(addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			fround_df_common(quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			fround_db_common(quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			fround_dr_common(quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			fround_ds_common(quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			fround_dd_common(quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			fround_dl_common(quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void fround_lf_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefLongFloat(left);
	b = long_float_fixnum(right);
	a = lisp_round_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void fround_lb_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefLongFloat(left);
	b = long_float_bignum(right);
	a = lisp_round_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void fround_lr_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefLongFloat(left);
	b = long_float_ratio(right);
	a = lisp_round_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void fround_ls_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefSingleFloat(right);
	a = lisp_round_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void fround_ld_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefDoubleFloat(right);
	a = lisp_round_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void fround_ll_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = RefLongFloat(left);
	b = RefLongFloat(right);
	a = lisp_round_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void fround_long_common(addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			fround_lf_common(quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			fround_lb_common(quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			fround_lr_common(quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			fround_ls_common(quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			fround_ld_common(quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			fround_ll_common(quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void fround2_common(LocalRoot local, addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			fround_fixnum_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			fround_bignum_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			fround_ratio_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			fround_single_common(quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			fround_double_common(quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			fround_long_common(quot, rem, left, right);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

_g void round_common(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2)
{
	if (div == Unbound)
		round1_common(local, ret1, ret2, var);
	else
		round2_common(local, ret1, ret2, var, div);
}

_g void fround_common(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2)
{
	if (div == Unbound)
		fround1_common(local, ret1, ret2, var);
	else
		fround2_common(local, ret1, ret2, var, div);
}

