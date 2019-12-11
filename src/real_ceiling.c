#include "bigdata.h"
#include "bignum.h"
#include "condition.h"
#include "ratio.h"
#include "real_division.h"
#include "real_ceiling.h"

/*
 *  common
 */
static void ceiling1_float(LocalRoot local, addr *quot, addr *rem, addr left)
{
	single_float v, r;

	GetSingleFloat(left, &v);
	v = lisp_ceiling1_s(v, &r);
	single_float_integer_heap(local, quot, v);
	single_float_heap(rem, r);
}

static void ceiling1_double(LocalRoot local, addr *quot, addr *rem, addr left)
{
	double_float v, r;

	GetDoubleFloat(left, &v);
	v = lisp_ceiling1_d(v, &r);
	double_float_integer_heap(local, quot, v);
	double_float_heap(rem, r);
}

static void ceiling1_long(LocalRoot local, addr *quot, addr *rem, addr left)
{
	long_float v, r;

	GetLongFloat(left, &v);
	v = lisp_ceiling1_l(v, &r);
	long_float_integer_heap(local, quot, v);
	long_float_heap(rem, r);
}

_g void ceiling1_common(LocalRoot local, addr *quot, addr *rem, addr left)
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
			lisp_ceiling1_ratio(local, quot, rem, left);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			ceiling1_float(local, quot, rem, left);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			ceiling1_double(local, quot, rem, left);
			break;

		case LISPTYPE_LONG_FLOAT:
			ceiling1_long(local, quot, rem, left);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

static void fceiling1_float(LocalRoot local, addr *quot, addr *rem, addr left)
{
	single_float v, r;

	GetSingleFloat(left, &v);
	v = lisp_ceiling1_s(v, &r);
	single_float_heap(quot, v);
	single_float_heap(rem, r);
}

static void fceiling1_double(LocalRoot local, addr *quot, addr *rem, addr left)
{
	double_float v, r;

	GetDoubleFloat(left, &v);
	v = lisp_ceiling1_d(v, &r);
	double_float_heap(quot, v);
	double_float_heap(rem, r);
}

static void fceiling1_long(LocalRoot local, addr *quot, addr *rem, addr left)
{
	long_float v, r;

	GetLongFloat(left, &v);
	v = lisp_ceiling1_l(v, &r);
	long_float_heap(quot, v);
	long_float_heap(rem, r);
}

_g void fceiling1_common(LocalRoot local, addr *quot, addr *rem, addr left)
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
			lisp_fceiling1_ratio(local, quot, rem, left);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			fceiling1_float(local, quot, rem, left);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			fceiling1_double(local, quot, rem, left);
			break;

		case LISPTYPE_LONG_FLOAT:
			fceiling1_long(local, quot, rem, left);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

static void ceiling_ff_common(addr *quot, addr *rem, addr left, addr right)
{
	fixnum a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	GetFixnum(left, &a);
	GetFixnum(right, &b);
	lisp_ceiling_fixnum(quot, rem, a, b);
}

static void ceiling_fb_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_BIGNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	lisp_ceiling_bignum(local, quot, rem, left, right);
	rollback_local(local, stack);
}

static void ceiling_fr_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_RATIO);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	lisp_ceiling_br_ratio(local, quot, rem, left, right);
	rollback_local(local, stack);
}

static void ceiling_fs_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = single_float_fixnum(left);
	b = RefSingleFloat(right);
	a = lisp_ceiling_s(a, b, &b);
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);
}

static void ceiling_fd_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = double_float_fixnum(left);
	b = RefDoubleFloat(right);
	a = lisp_ceiling_d(a, b, &b);
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);
}

static void ceiling_fl_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = long_float_fixnum(left);
	b = RefLongFloat(right);
	a = lisp_ceiling_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void ceiling_fixnum_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			ceiling_ff_common(quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			ceiling_fb_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			ceiling_fr_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			ceiling_fs_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			ceiling_fd_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			ceiling_fl_common(local, quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void ceiling_bf_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	lisp_ceiling_bignum(local, quot, rem, left, right);
	rollback_local(local, stack);
}

static void ceiling_bs_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = single_float_bignum(left);
	b = RefSingleFloat(right);
	a = lisp_ceiling_s(a, b, &b);
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);
}

static void ceiling_bd_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = double_float_bignum(left);
	b = RefDoubleFloat(right);
	a = lisp_ceiling_d(a, b, &b);
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);
}

static void ceiling_bl_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = long_float_bignum(left);
	b = RefLongFloat(right);
	a = lisp_ceiling_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void ceiling_bignum_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			ceiling_bf_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			lisp_ceiling_bignum(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			lisp_ceiling_br_ratio(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			ceiling_bs_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			ceiling_bd_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			ceiling_bl_common(local, quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void ceiling_rf_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	lisp_ceiling_rb_ratio(local, quot, rem, left, right);
	rollback_local(local, stack);
}

static void ceiling_rs_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = single_float_ratio(left);
	b = RefSingleFloat(right);
	a = lisp_ceiling_s(a, b, &b);
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);
}

static void ceiling_rd_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = double_float_ratio(left);
	b = RefDoubleFloat(right);
	a = lisp_ceiling_d(a, b, &b);
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);
}

static void ceiling_rl_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = long_float_ratio(left);
	b = RefLongFloat(right);
	a = lisp_ceiling_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void ceiling_ratio_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			ceiling_rf_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			lisp_ceiling_rb_ratio(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			lisp_ceiling_rr_ratio(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			ceiling_rs_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			ceiling_rd_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			ceiling_rl_common(local, quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void ceiling_sf_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefSingleFloat(left);
	b = single_float_fixnum(right);
	a = lisp_ceiling_s(a, b, &b);
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);
}

static void ceiling_sb_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefSingleFloat(left);
	b = single_float_bignum(right);
	a = lisp_ceiling_s(a, b, &b);
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);
}

static void ceiling_sr_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefSingleFloat(left);
	b = single_float_ratio(right);
	a = lisp_ceiling_s(a, b, &b);
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);
}

static void ceiling_ss_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefSingleFloat(left);
	b = RefSingleFloat(right);
	a = lisp_ceiling_s(a, b, &b);
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);
}

static void ceiling_sd_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = (double_float)RefSingleFloat(left);
	b = RefDoubleFloat(right);
	a = lisp_ceiling_d(a, b, &b);
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);
}

static void ceiling_sl_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefSingleFloat(left);
	b = RefLongFloat(right);
	a = lisp_ceiling_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void ceiling_single_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			ceiling_sf_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			ceiling_sb_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			ceiling_sr_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			ceiling_ss_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			ceiling_sd_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			ceiling_sl_common(local, quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void ceiling_df_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefDoubleFloat(left);
	b = double_float_fixnum(right);
	a = lisp_ceiling_d(a, b, &b);
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);
}

static void ceiling_db_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefDoubleFloat(left);
	b = double_float_bignum(right);
	a = lisp_ceiling_d(a, b, &b);
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);
}

static void ceiling_dr_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefDoubleFloat(left);
	b = double_float_ratio(right);
	a = lisp_ceiling_d(a, b, &b);
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);
}

static void ceiling_ds_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefDoubleFloat(left);
	b = (double_float)RefSingleFloat(right);
	a = lisp_ceiling_d(a, b, &b);
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);
}

static void ceiling_dd_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefDoubleFloat(left);
	b = RefDoubleFloat(right);
	a = lisp_ceiling_d(a, b, &b);
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);
}

static void ceiling_dl_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefDoubleFloat(left);
	b = RefLongFloat(right);
	a = lisp_ceiling_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void ceiling_double_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			ceiling_df_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			ceiling_db_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			ceiling_dr_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			ceiling_ds_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			ceiling_dd_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			ceiling_dl_common(local, quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void ceiling_lf_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefLongFloat(left);
	b = long_float_fixnum(right);
	a = lisp_ceiling_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void ceiling_lb_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefLongFloat(left);
	b = long_float_bignum(right);
	a = lisp_ceiling_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void ceiling_lr_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefLongFloat(left);
	b = long_float_ratio(right);
	a = lisp_ceiling_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void ceiling_ls_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefSingleFloat(right);
	a = lisp_ceiling_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void ceiling_ld_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefDoubleFloat(right);
	a = lisp_ceiling_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void ceiling_ll_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = RefLongFloat(left);
	b = RefLongFloat(right);
	a = lisp_ceiling_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void ceiling_long_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			ceiling_lf_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			ceiling_lb_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			ceiling_lr_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			ceiling_ls_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			ceiling_ld_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			ceiling_ll_common(local, quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void ceiling2_common(LocalRoot local, addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			ceiling_fixnum_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			ceiling_bignum_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			ceiling_ratio_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			ceiling_single_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			ceiling_double_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			ceiling_long_common(local, quot, rem, left, right);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

static void fceiling_ff_common(addr *quot, addr *rem, addr left, addr right)
{
	fixnum a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	GetFixnum(left, &a);
	GetFixnum(right, &b);
	lisp_fceiling_fixnum(quot, rem, a, b);
}

static void fceiling_fb_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_BIGNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	lisp_fceiling_bignum(local, quot, rem, left, right);
	rollback_local(local, stack);
}

static void fceiling_fr_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_RATIO);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	lisp_fceiling_br_ratio(local, quot, rem, left, right);
	rollback_local(local, stack);
}

static void fceiling_fs_common(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = single_float_fixnum(left);
	b = RefSingleFloat(right);
	a = lisp_ceiling_s(a, b, &b);
	single_float_heap(quot, a);
	single_float_heap(rem, b);
}

static void fceiling_fd_common(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = double_float_fixnum(left);
	b = RefDoubleFloat(right);
	a = lisp_ceiling_d(a, b, &b);
	double_float_heap(quot, a);
	double_float_heap(rem, b);
}

static void fceiling_fl_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = long_float_fixnum(left);
	b = RefLongFloat(right);
	a = lisp_ceiling_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void fceiling_fixnum_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			fceiling_ff_common(quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			fceiling_fb_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			fceiling_fr_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			fceiling_fs_common(quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			fceiling_fd_common(quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			fceiling_fl_common(quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void fceiling_bf_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	lisp_fceiling_bignum(local, quot, rem, left, right);
	rollback_local(local, stack);
}

static void fceiling_bs_common(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = single_float_bignum(left);
	b = RefSingleFloat(right);
	a = lisp_ceiling_s(a, b, &b);
	single_float_heap(quot, a);
	single_float_heap(rem, b);
}

static void fceiling_bd_common(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = double_float_bignum(left);
	b = RefDoubleFloat(right);
	a = lisp_ceiling_d(a, b, &b);
	double_float_heap(quot, a);
	double_float_heap(rem, b);
}

static void fceiling_bl_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = long_float_bignum(left);
	b = RefLongFloat(right);
	a = lisp_ceiling_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void fceiling_bignum_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			fceiling_bf_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			lisp_fceiling_bignum(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			lisp_fceiling_br_ratio(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			fceiling_bs_common(quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			fceiling_bd_common(quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			fceiling_bl_common(quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void fceiling_rf_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	lisp_fceiling_rb_ratio(local, quot, rem, left, right);
	rollback_local(local, stack);
}

static void fceiling_rs_common(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = single_float_ratio(left);
	b = RefSingleFloat(right);
	a = lisp_ceiling_s(a, b, &b);
	single_float_heap(quot, a);
	single_float_heap(rem, b);
}

static void fceiling_rd_common(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = double_float_ratio(left);
	b = RefDoubleFloat(right);
	a = lisp_ceiling_d(a, b, &b);
	double_float_heap(quot, a);
	double_float_heap(rem, b);
}

static void fceiling_rl_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = long_float_ratio(left);
	b = RefLongFloat(right);
	a = lisp_ceiling_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void fceiling_ratio_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			fceiling_rf_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			lisp_fceiling_rb_ratio(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			lisp_fceiling_rr_ratio(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			fceiling_rs_common(quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			fceiling_rd_common(quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			fceiling_rl_common(quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void fceiling_sf_common(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefSingleFloat(left);
	b = single_float_fixnum(right);
	a = lisp_ceiling_s(a, b, &b);
	single_float_heap(quot, a);
	single_float_heap(rem, b);
}

static void fceiling_sb_common(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefSingleFloat(left);
	b = single_float_bignum(right);
	a = lisp_ceiling_s(a, b, &b);
	single_float_heap(quot, a);
	single_float_heap(rem, b);
}

static void fceiling_sr_common(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefSingleFloat(left);
	b = single_float_ratio(right);
	a = lisp_ceiling_s(a, b, &b);
	single_float_heap(quot, a);
	single_float_heap(rem, b);
}

static void fceiling_ss_common(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefSingleFloat(left);
	b = RefSingleFloat(right);
	a = lisp_ceiling_s(a, b, &b);
	single_float_heap(quot, a);
	single_float_heap(rem, b);
}

static void fceiling_sd_common(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = (double_float)RefSingleFloat(left);
	b = RefDoubleFloat(right);
	a = lisp_ceiling_d(a, b, &b);
	double_float_heap(quot, a);
	double_float_heap(rem, b);
}

static void fceiling_sl_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefSingleFloat(left);
	b = RefLongFloat(right);
	a = lisp_ceiling_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void fceiling_single_common(addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			fceiling_sf_common(quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			fceiling_sb_common(quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			fceiling_sr_common(quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			fceiling_ss_common(quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			fceiling_sd_common(quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			fceiling_sl_common(quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void fceiling_df_common(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefDoubleFloat(left);
	b = double_float_fixnum(right);
	a = lisp_ceiling_d(a, b, &b);
	double_float_heap(quot, a);
	double_float_heap(rem, b);
}

static void fceiling_db_common(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefDoubleFloat(left);
	b = double_float_bignum(right);
	a = lisp_ceiling_d(a, b, &b);
	double_float_heap(quot, a);
	double_float_heap(rem, b);
}

static void fceiling_dr_common(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefDoubleFloat(left);
	b = double_float_ratio(right);
	a = lisp_ceiling_d(a, b, &b);
	double_float_heap(quot, a);
	double_float_heap(rem, b);
}

static void fceiling_ds_common(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefDoubleFloat(left);
	b = (double_float)RefSingleFloat(right);
	a = lisp_ceiling_d(a, b, &b);
	double_float_heap(quot, a);
	double_float_heap(rem, b);
}

static void fceiling_dd_common(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefDoubleFloat(left);
	b = RefDoubleFloat(right);
	a = lisp_ceiling_d(a, b, &b);
	double_float_heap(quot, a);
	double_float_heap(rem, b);
}

static void fceiling_dl_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefDoubleFloat(left);
	b = RefLongFloat(right);
	a = lisp_ceiling_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void fceiling_double_common(addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			fceiling_df_common(quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			fceiling_db_common(quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			fceiling_dr_common(quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			fceiling_ds_common(quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			fceiling_dd_common(quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			fceiling_dl_common(quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void fceiling_lf_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefLongFloat(left);
	b = long_float_fixnum(right);
	a = lisp_ceiling_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void fceiling_lb_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefLongFloat(left);
	b = long_float_bignum(right);
	a = lisp_ceiling_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void fceiling_lr_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefLongFloat(left);
	b = long_float_ratio(right);
	a = lisp_ceiling_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void fceiling_ls_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefSingleFloat(right);
	a = lisp_ceiling_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void fceiling_ld_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefDoubleFloat(right);
	a = lisp_ceiling_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void fceiling_ll_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = RefLongFloat(left);
	b = RefLongFloat(right);
	a = lisp_ceiling_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void fceiling_long_common(addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			fceiling_lf_common(quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			fceiling_lb_common(quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			fceiling_lr_common(quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			fceiling_ls_common(quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			fceiling_ld_common(quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			fceiling_ll_common(quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

_g void fceiling2_common(LocalRoot local, addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			fceiling_fixnum_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			fceiling_bignum_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			fceiling_ratio_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			fceiling_single_common(quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			fceiling_double_common(quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			fceiling_long_common(quot, rem, left, right);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

_g void ceiling_common(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2)
{
	if (div == Unbound)
		ceiling1_common(local, ret1, ret2, var);
	else
		ceiling2_common(local, ret1, ret2, var, div);
}

_g void fceiling_common(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2)
{
	if (div == Unbound)
		fceiling1_common(local, ret1, ret2, var);
	else
		fceiling2_common(local, ret1, ret2, var, div);
}

