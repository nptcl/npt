#include "bignum.h"
#include "bignum_data.h"
#include "bignum_object.h"
#include "condition.h"
#include "ratio.h"
#include "real_division.h"
#include "real_truncate.h"

/*
 *  common
 */
static void truncate1_float(LocalRoot local, addr *quot, addr *rem, addr left)
{
	single_float v, r;

	GetSingleFloat(left, &v);
	v = lisp_truncate1_s(v, &r);
	single_float_integer_heap(local, quot, v);
	single_float_heap(rem, r);
}

static void truncate1_double(LocalRoot local, addr *quot, addr *rem, addr left)
{
	double_float v, r;

	GetDoubleFloat(left, &v);
	v = lisp_truncate1_d(v, &r);
	double_float_integer_heap(local, quot, v);
	double_float_heap(rem, r);
}

static void truncate1_long(LocalRoot local, addr *quot, addr *rem, addr left)
{
	long_float v, r;

	GetLongFloat(left, &v);
	v = lisp_truncate1_l(v, &r);
	long_float_integer_heap(local, quot, v);
	long_float_heap(rem, r);
}

_g void truncate1_common(LocalRoot local, addr *quot, addr *rem, addr left)
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
			lisp_truncate1_ratio(local, quot, rem, left);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			truncate1_float(local, quot, rem, left);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			truncate1_double(local, quot, rem, left);
			break;

		case LISPTYPE_LONG_FLOAT:
			truncate1_long(local, quot, rem, left);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

static void ftruncate1_float(LocalRoot local, addr *quot, addr *rem, addr left)
{
	single_float v, r;

	GetSingleFloat(left, &v);
	v = lisp_truncate1_s(v, &r);
	single_float_heap(quot, v);
	single_float_heap(rem, r);
}

static void ftruncate1_double(LocalRoot local, addr *quot, addr *rem, addr left)
{
	double_float v, r;

	GetDoubleFloat(left, &v);
	v = lisp_truncate1_d(v, &r);
	double_float_heap(quot, v);
	double_float_heap(rem, r);
}

static void ftruncate1_long(LocalRoot local, addr *quot, addr *rem, addr left)
{
	long_float v, r;

	GetLongFloat(left, &v);
	v = lisp_truncate1_l(v, &r);
	long_float_heap(quot, v);
	long_float_heap(rem, r);
}

_g void ftruncate1_common(LocalRoot local, addr *quot, addr *rem, addr left)
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
			lisp_ftruncate1_ratio(local, quot, rem, left);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			ftruncate1_float(local, quot, rem, left);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			ftruncate1_double(local, quot, rem, left);
			break;

		case LISPTYPE_LONG_FLOAT:
			ftruncate1_long(local, quot, rem, left);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

static void truncate_ff_common(addr *quot, addr *rem, addr left, addr right)
{
	fixnum a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	GetFixnum(left, &a);
	GetFixnum(right, &b);
	lisp_truncate_fixnum(quot, rem, a, b);
}

static void truncate_fb_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_BIGNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	lisp_truncate_bignum(local, quot, rem, left, right);
	rollback_local(local, stack);
}

static void truncate_fr_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_RATIO);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	lisp_truncate_br_ratio(local, quot, rem, left, right);
	rollback_local(local, stack);
}

static void truncate_fs_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = single_float_fixnum(left);
	b = RefSingleFloat(right);
	a = lisp_truncate_s(a, b, &b);
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);
}

static void truncate_fd_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = double_float_fixnum(left);
	b = RefDoubleFloat(right);
	a = lisp_truncate_d(a, b, &b);
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);
}

static void truncate_fl_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = long_float_fixnum(left);
	b = RefLongFloat(right);
	a = lisp_truncate_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void truncate_fixnum_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			truncate_ff_common(quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			truncate_fb_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			truncate_fr_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			truncate_fs_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			truncate_fd_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			truncate_fl_common(local, quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void truncate_bf_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	lisp_truncate_bignum(local, quot, rem, left, right);
	rollback_local(local, stack);
}

static void truncate_bs_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = single_float_bignum(left);
	b = RefSingleFloat(right);
	a = lisp_truncate_s(a, b, &b);
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);
}

static void truncate_bd_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = double_float_bignum(left);
	b = RefDoubleFloat(right);
	a = lisp_truncate_d(a, b, &b);
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);
}

static void truncate_bl_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = long_float_bignum(left);
	b = RefLongFloat(right);
	a = lisp_truncate_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void truncate_bignum_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			truncate_bf_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			lisp_truncate_bignum(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			lisp_truncate_br_ratio(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			truncate_bs_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			truncate_bd_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			truncate_bl_common(local, quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void truncate_rf_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	lisp_truncate_rb_ratio(local, quot, rem, left, right);
	rollback_local(local, stack);
}

static void truncate_rs_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = single_float_ratio(left);
	b = RefSingleFloat(right);
	a = lisp_truncate_s(a, b, &b);
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);
}

static void truncate_rd_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = double_float_ratio(left);
	b = RefDoubleFloat(right);
	a = lisp_truncate_d(a, b, &b);
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);
}

static void truncate_rl_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = long_float_ratio(left);
	b = RefLongFloat(right);
	a = lisp_truncate_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void truncate_ratio_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			truncate_rf_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			lisp_truncate_rb_ratio(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			lisp_truncate_rr_ratio(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			truncate_rs_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			truncate_rd_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			truncate_rl_common(local, quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void truncate_sf_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefSingleFloat(left);
	b = single_float_fixnum(right);
	a = lisp_truncate_s(a, b, &b);
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);
}

static void truncate_sb_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefSingleFloat(left);
	b = single_float_bignum(right);
	a = lisp_truncate_s(a, b, &b);
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);
}

static void truncate_sr_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefSingleFloat(left);
	b = single_float_ratio(right);
	a = lisp_truncate_s(a, b, &b);
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);
}

static void truncate_ss_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefSingleFloat(left);
	b = RefSingleFloat(right);
	a = lisp_truncate_s(a, b, &b);
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);
}

static void truncate_sd_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = (double_float)RefSingleFloat(left);
	b = RefDoubleFloat(right);
	a = lisp_truncate_d(a, b, &b);
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);
}

static void truncate_sl_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefSingleFloat(left);
	b = RefLongFloat(right);
	a = lisp_truncate_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void truncate_single_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			truncate_sf_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			truncate_sb_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			truncate_sr_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			truncate_ss_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			truncate_sd_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			truncate_sl_common(local, quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void truncate_df_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefDoubleFloat(left);
	b = double_float_fixnum(right);
	a = lisp_truncate_d(a, b, &b);
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);
}

static void truncate_db_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefDoubleFloat(left);
	b = double_float_bignum(right);
	a = lisp_truncate_d(a, b, &b);
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);
}

static void truncate_dr_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefDoubleFloat(left);
	b = double_float_ratio(right);
	a = lisp_truncate_d(a, b, &b);
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);
}

static void truncate_ds_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefDoubleFloat(left);
	b = (double_float)RefSingleFloat(right);
	a = lisp_truncate_d(a, b, &b);
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);
}

static void truncate_dd_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefDoubleFloat(left);
	b = RefDoubleFloat(right);
	a = lisp_truncate_d(a, b, &b);
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);
}

static void truncate_dl_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefDoubleFloat(left);
	b = RefLongFloat(right);
	a = lisp_truncate_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void truncate_double_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			truncate_df_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			truncate_db_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			truncate_dr_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			truncate_ds_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			truncate_dd_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			truncate_dl_common(local, quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void truncate_lf_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefLongFloat(left);
	b = long_float_fixnum(right);
	a = lisp_truncate_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void truncate_lb_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefLongFloat(left);
	b = long_float_bignum(right);
	a = lisp_truncate_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void truncate_lr_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefLongFloat(left);
	b = long_float_ratio(right);
	a = lisp_truncate_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void truncate_ls_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefSingleFloat(right);
	a = lisp_truncate_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void truncate_ld_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefDoubleFloat(right);
	a = lisp_truncate_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void truncate_ll_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = RefLongFloat(left);
	b = RefLongFloat(right);
	a = lisp_truncate_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void truncate_long_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			truncate_lf_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			truncate_lb_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			truncate_lr_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			truncate_ls_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			truncate_ld_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			truncate_ll_common(local, quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void truncate2_common(LocalRoot local, addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			truncate_fixnum_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			truncate_bignum_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			truncate_ratio_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			truncate_single_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			truncate_double_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			truncate_long_common(local, quot, rem, left, right);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

static void ftruncate_ff_common(addr *quot, addr *rem, addr left, addr right)
{
	fixnum a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	GetFixnum(left, &a);
	GetFixnum(right, &b);
	lisp_ftruncate_fixnum(quot, rem, a, b);
}

static void ftruncate_fb_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_BIGNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	lisp_ftruncate_bignum(local, quot, rem, left, right);
	rollback_local(local, stack);
}

static void ftruncate_fr_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_RATIO);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	lisp_ftruncate_br_ratio(local, quot, rem, left, right);
	rollback_local(local, stack);
}

static void ftruncate_fs_common(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = single_float_fixnum(left);
	b = RefSingleFloat(right);
	a = lisp_truncate_s(a, b, &b);
	single_float_heap(quot, a);
	single_float_heap(rem, b);
}

static void ftruncate_fd_common(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = double_float_fixnum(left);
	b = RefDoubleFloat(right);
	a = lisp_truncate_d(a, b, &b);
	double_float_heap(quot, a);
	double_float_heap(rem, b);
}

static void ftruncate_fl_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = long_float_fixnum(left);
	b = RefLongFloat(right);
	a = lisp_truncate_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void ftruncate_fixnum_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			ftruncate_ff_common(quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			ftruncate_fb_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			ftruncate_fr_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			ftruncate_fs_common(quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			ftruncate_fd_common(quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			ftruncate_fl_common(quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void ftruncate_bf_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	lisp_ftruncate_bignum(local, quot, rem, left, right);
	rollback_local(local, stack);
}

static void ftruncate_bs_common(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = single_float_bignum(left);
	b = RefSingleFloat(right);
	a = lisp_truncate_s(a, b, &b);
	single_float_heap(quot, a);
	single_float_heap(rem, b);
}

static void ftruncate_bd_common(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = double_float_bignum(left);
	b = RefDoubleFloat(right);
	a = lisp_truncate_d(a, b, &b);
	double_float_heap(quot, a);
	double_float_heap(rem, b);
}

static void ftruncate_bl_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = long_float_bignum(left);
	b = RefLongFloat(right);
	a = lisp_truncate_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void ftruncate_bignum_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			ftruncate_bf_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			lisp_ftruncate_bignum(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			lisp_ftruncate_br_ratio(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			ftruncate_bs_common(quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			ftruncate_bd_common(quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			ftruncate_bl_common(quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void ftruncate_rf_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	lisp_ftruncate_rb_ratio(local, quot, rem, left, right);
	rollback_local(local, stack);
}

static void ftruncate_rs_common(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = single_float_ratio(left);
	b = RefSingleFloat(right);
	a = lisp_truncate_s(a, b, &b);
	single_float_heap(quot, a);
	single_float_heap(rem, b);
}

static void ftruncate_rd_common(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = double_float_ratio(left);
	b = RefDoubleFloat(right);
	a = lisp_truncate_d(a, b, &b);
	double_float_heap(quot, a);
	double_float_heap(rem, b);
}

static void ftruncate_rl_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = long_float_ratio(left);
	b = RefLongFloat(right);
	a = lisp_truncate_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void ftruncate_ratio_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			ftruncate_rf_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			lisp_ftruncate_rb_ratio(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			lisp_ftruncate_rr_ratio(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			ftruncate_rs_common(quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			ftruncate_rd_common(quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			ftruncate_rl_common(quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void ftruncate_sf_common(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefSingleFloat(left);
	b = single_float_fixnum(right);
	a = lisp_truncate_s(a, b, &b);
	single_float_heap(quot, a);
	single_float_heap(rem, b);
}

static void ftruncate_sb_common(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefSingleFloat(left);
	b = single_float_bignum(right);
	a = lisp_truncate_s(a, b, &b);
	single_float_heap(quot, a);
	single_float_heap(rem, b);
}

static void ftruncate_sr_common(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefSingleFloat(left);
	b = single_float_ratio(right);
	a = lisp_truncate_s(a, b, &b);
	single_float_heap(quot, a);
	single_float_heap(rem, b);
}

static void ftruncate_ss_common(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefSingleFloat(left);
	b = RefSingleFloat(right);
	a = lisp_truncate_s(a, b, &b);
	single_float_heap(quot, a);
	single_float_heap(rem, b);
}

static void ftruncate_sd_common(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = (double_float)RefSingleFloat(left);
	b = RefDoubleFloat(right);
	a = lisp_truncate_d(a, b, &b);
	double_float_heap(quot, a);
	double_float_heap(rem, b);
}

static void ftruncate_sl_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefSingleFloat(left);
	b = RefLongFloat(right);
	a = lisp_truncate_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void ftruncate_single_common(addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			ftruncate_sf_common(quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			ftruncate_sb_common(quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			ftruncate_sr_common(quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			ftruncate_ss_common(quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			ftruncate_sd_common(quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			ftruncate_sl_common(quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void ftruncate_df_common(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefDoubleFloat(left);
	b = double_float_fixnum(right);
	a = lisp_truncate_d(a, b, &b);
	double_float_heap(quot, a);
	double_float_heap(rem, b);
}

static void ftruncate_db_common(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefDoubleFloat(left);
	b = double_float_bignum(right);
	a = lisp_truncate_d(a, b, &b);
	double_float_heap(quot, a);
	double_float_heap(rem, b);
}

static void ftruncate_dr_common(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefDoubleFloat(left);
	b = double_float_ratio(right);
	a = lisp_truncate_d(a, b, &b);
	double_float_heap(quot, a);
	double_float_heap(rem, b);
}

static void ftruncate_ds_common(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefDoubleFloat(left);
	b = (double_float)RefSingleFloat(right);
	a = lisp_truncate_d(a, b, &b);
	double_float_heap(quot, a);
	double_float_heap(rem, b);
}

static void ftruncate_dd_common(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefDoubleFloat(left);
	b = RefDoubleFloat(right);
	a = lisp_truncate_d(a, b, &b);
	double_float_heap(quot, a);
	double_float_heap(rem, b);
}

static void ftruncate_dl_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefDoubleFloat(left);
	b = RefLongFloat(right);
	a = lisp_truncate_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void ftruncate_double_common(addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			ftruncate_df_common(quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			ftruncate_db_common(quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			ftruncate_dr_common(quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			ftruncate_ds_common(quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			ftruncate_dd_common(quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			ftruncate_dl_common(quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void ftruncate_lf_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefLongFloat(left);
	b = long_float_fixnum(right);
	a = lisp_truncate_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void ftruncate_lb_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefLongFloat(left);
	b = long_float_bignum(right);
	a = lisp_truncate_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void ftruncate_lr_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefLongFloat(left);
	b = long_float_ratio(right);
	a = lisp_truncate_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void ftruncate_ls_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefSingleFloat(right);
	a = lisp_truncate_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void ftruncate_ld_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefDoubleFloat(right);
	a = lisp_truncate_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void ftruncate_ll_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = RefLongFloat(left);
	b = RefLongFloat(right);
	a = lisp_truncate_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void ftruncate_long_common(addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			ftruncate_lf_common(quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			ftruncate_lb_common(quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			ftruncate_lr_common(quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			ftruncate_ls_common(quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			ftruncate_ld_common(quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			ftruncate_ll_common(quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void ftruncate2_common(LocalRoot local, addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			ftruncate_fixnum_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			ftruncate_bignum_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			ftruncate_ratio_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			ftruncate_single_common(quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			ftruncate_double_common(quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			ftruncate_long_common(quot, rem, left, right);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

_g void truncate_common(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2)
{
	if (div == Unbound)
		truncate1_common(local, ret1, ret2, var);
	else
		truncate2_common(local, ret1, ret2, var, div);
}

_g void ftruncate_common(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2)
{
	if (div == Unbound)
		ftruncate1_common(local, ret1, ret2, var);
	else
		ftruncate2_common(local, ret1, ret2, var, div);
}


/*
 *  rem
 */
static void rem_ff_common(addr *ret, addr left, addr right)
{
	fixnum a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	GetFixnum(left, &a);
	GetFixnum(right, &b);
	lisp_rem_fixnum(ret, a, b);
}

static void rem_fb_common(LocalRoot local, addr *ret, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_BIGNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	lisp_rem_bignum(local, ret, left, right);
	rollback_local(local, stack);
}

static void rem_fr_common(LocalRoot local, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_RATIO);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	lisp_rem_br_ratio(local, rem, left, right);
	rollback_local(local, stack);
}

static void rem_bf_common(LocalRoot local, addr *ret, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	lisp_rem_bignum(local, ret, left, right);
	rollback_local(local, stack);
}

static void rem_rf_common(LocalRoot local, addr *ret, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	lisp_rem_rb_ratio(local, ret, left, right);
	rollback_local(local, stack);
}

static void rem_fixnum_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			rem_ff_common(ret, left, right);
			break;

		case LISPTYPE_BIGNUM:
			rem_fb_common(local, ret, left, right);
			break;

		case LISPTYPE_RATIO:
			rem_fr_common(local, ret, left, right);
			break;

		default:
			TypeError(right, RATIONAL);
			break;
	}
}

static void rem_bignum_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			rem_bf_common(local, ret, left, right);
			break;

		case LISPTYPE_BIGNUM:
			lisp_rem_bignum(local, ret, left, right);
			break;

		case LISPTYPE_RATIO:
			lisp_rem_br_ratio(local, ret, left, right);
			break;

		default:
			TypeError(right, RATIONAL);
			break;
	}
}

static void rem_ratio_common(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			rem_rf_common(local, ret, left, right);
			break;

		case LISPTYPE_BIGNUM:
			lisp_rem_rb_ratio(local, ret, left, right);
			break;

		case LISPTYPE_RATIO:
			lisp_rem_rr_ratio(local, ret, left, right);
			break;

		default:
			TypeError(right, RATIONAL);
			break;
	}
}

_g void rem_rational_common(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			rem_fixnum_common(local, left, right, ret);
			break;

		case LISPTYPE_BIGNUM:
			rem_bignum_common(local, left, right, ret);
			break;

		case LISPTYPE_RATIO:
			rem_ratio_common(local, left, right, ret);
			break;

		default:
			TypeError(left, RATIONAL);
			break;
	}
}

