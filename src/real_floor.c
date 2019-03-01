#include "bigdata.h"
#include "bignum.h"
#include "condition.h"
#include "ratio.h"
#include "real_division.h"
#include "real_floor.h"

/*
 *  common
 */
static void single_float_integer_heap(LocalRoot local, addr *ret, single_float v)
{
	addr pos;
	LocalStack stack;

	push_local(local, &stack);
	if (bignum_single_float_local(local, &pos, v))
		fmte("bignum_single_float_local error.", NULL);
	bignum_result_heap(pos, ret);
	rollback_local(local, stack);
}

static void double_float_integer_heap(LocalRoot local, addr *ret, double_float v)
{
	addr pos;
	LocalStack stack;

	push_local(local, &stack);
	if (bignum_double_float_local(local, &pos, v))
		fmte("bignum_double_float_local error.", NULL);
	bignum_result_heap(pos, ret);
	rollback_local(local, stack);
}

static void long_float_integer_heap(LocalRoot local, addr *ret, long_float v)
{
	addr pos;
	LocalStack stack;

	push_local(local, &stack);
	if (bignum_long_float_local(local, &pos, v))
		fmte("bignum_long_float_local error.", NULL);
	bignum_result_heap(pos, ret);
	rollback_local(local, stack);
}

static void floor1_float(LocalRoot local, addr *quot, addr *rem, addr left)
{
	single_float v, r;

	GetSingleFloat(left, &v);
	v = lisp_floor1_s(v, &r);
	single_float_integer_heap(local, quot, v);
	single_float_heap(rem, r);
}

static void floor1_double(LocalRoot local, addr *quot, addr *rem, addr left)
{
	double_float v, r;

	GetDoubleFloat(left, &v);
	v = lisp_floor1_d(v, &r);
	double_float_integer_heap(local, quot, v);
	double_float_heap(rem, r);
}

static void floor1_long(LocalRoot local, addr *quot, addr *rem, addr left)
{
	long_float v, r;

	GetLongFloat(left, &v);
	v = lisp_floor1_l(v, &r);
	long_float_integer_heap(local, quot, v);
	long_float_heap(rem, r);
}

void floor1_common(LocalRoot local, addr *quot, addr *rem, addr left)
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
			lisp_floor1_ratio(local, quot, rem, left);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			floor1_float(local, quot, rem, left);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			floor1_double(local, quot, rem, left);
			break;

		case LISPTYPE_LONG_FLOAT:
			floor1_long(local, quot, rem, left);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

static void ffloor1_float(LocalRoot local, addr *quot, addr *rem, addr left)
{
	single_float v, r;

	GetSingleFloat(left, &v);
	v = lisp_floor1_s(v, &r);
	single_float_heap(quot, v);
	single_float_heap(rem, r);
}

static void ffloor1_double(LocalRoot local, addr *quot, addr *rem, addr left)
{
	double_float v, r;

	GetDoubleFloat(left, &v);
	v = lisp_floor1_d(v, &r);
	double_float_heap(quot, v);
	double_float_heap(rem, r);
}

static void ffloor1_long(LocalRoot local, addr *quot, addr *rem, addr left)
{
	long_float v, r;

	GetLongFloat(left, &v);
	v = lisp_floor1_l(v, &r);
	long_float_heap(quot, v);
	long_float_heap(rem, r);
}

void ffloor1_common(LocalRoot local, addr *quot, addr *rem, addr left)
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
			lisp_ffloor1_ratio(local, quot, rem, left);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			ffloor1_float(local, quot, rem, left);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			ffloor1_double(local, quot, rem, left);
			break;

		case LISPTYPE_LONG_FLOAT:
			ffloor1_long(local, quot, rem, left);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

static void floor_ff_common(addr *quot, addr *rem, addr left, addr right)
{
	fixnum a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	GetFixnum(left, &a);
	GetFixnum(right, &b);
	lisp_floor_fixnum(quot, rem, a, b);
}

static void floor_fb_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_BIGNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	lisp_floor_bignum(local, quot, rem, left, right);
	rollback_local(local, stack);
}

static void floor_fr_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_RATIO);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	lisp_floor_br_ratio(local, quot, rem, left, right);
	rollback_local(local, stack);
}

static void floor_fs_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = single_float_fixnum(left);
	b = RefSingleFloat(right);
	a = lisp_floor_s(a, b, &b);
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);
}

static void floor_fd_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = double_float_fixnum(left);
	b = RefDoubleFloat(right);
	a = lisp_floor_d(a, b, &b);
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);
}

static void floor_fl_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = long_float_fixnum(left);
	b = RefLongFloat(right);
	a = lisp_floor_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void floor_fixnum_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			floor_ff_common(quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			floor_fb_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			floor_fr_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			floor_fs_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			floor_fd_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			floor_fl_common(local, quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void floor_bf_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	lisp_floor_bignum(local, quot, rem, left, right);
	rollback_local(local, stack);
}

static void floor_bs_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = single_float_bignum(left);
	b = RefSingleFloat(right);
	a = lisp_floor_s(a, b, &b);
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);
}

static void floor_bd_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = double_float_bignum(left);
	b = RefDoubleFloat(right);
	a = lisp_floor_d(a, b, &b);
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);
}

static void floor_bl_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = long_float_bignum(left);
	b = RefLongFloat(right);
	a = lisp_floor_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void floor_bignum_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			floor_bf_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			lisp_floor_bignum(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			lisp_floor_br_ratio(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			floor_bs_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			floor_bd_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			floor_bl_common(local, quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void floor_rf_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	lisp_floor_rb_ratio(local, quot, rem, left, right);
	rollback_local(local, stack);
}

static void floor_rs_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = single_float_ratio(left);
	b = RefSingleFloat(right);
	a = lisp_floor_s(a, b, &b);
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);
}

static void floor_rd_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = double_float_ratio(left);
	b = RefDoubleFloat(right);
	a = lisp_floor_d(a, b, &b);
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);
}

static void floor_rl_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = long_float_ratio(left);
	b = RefLongFloat(right);
	a = lisp_floor_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void floor_ratio_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			floor_rf_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			lisp_floor_rb_ratio(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			lisp_floor_rr_ratio(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			floor_rs_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			floor_rd_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			floor_rl_common(local, quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void floor_sf_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefSingleFloat(left);
	b = single_float_fixnum(right);
	a = lisp_floor_s(a, b, &b);
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);
}

static void floor_sb_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefSingleFloat(left);
	b = single_float_bignum(right);
	a = lisp_floor_s(a, b, &b);
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);
}

static void floor_sr_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefSingleFloat(left);
	b = single_float_ratio(right);
	a = lisp_floor_s(a, b, &b);
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);
}

static void floor_ss_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefSingleFloat(left);
	b = RefSingleFloat(right);
	a = lisp_floor_s(a, b, &b);
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);
}

static void floor_sd_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = (double_float)RefSingleFloat(left);
	b = RefDoubleFloat(right);
	a = lisp_floor_d(a, b, &b);
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);
}

static void floor_sl_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefSingleFloat(left);
	b = RefLongFloat(right);
	a = lisp_floor_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void floor_single_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			floor_sf_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			floor_sb_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			floor_sr_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			floor_ss_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			floor_sd_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			floor_sl_common(local, quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void floor_df_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefDoubleFloat(left);
	b = double_float_fixnum(right);
	a = lisp_floor_d(a, b, &b);
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);
}

static void floor_db_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefDoubleFloat(left);
	b = double_float_bignum(right);
	a = lisp_floor_d(a, b, &b);
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);
}

static void floor_dr_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefDoubleFloat(left);
	b = double_float_ratio(right);
	a = lisp_floor_d(a, b, &b);
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);
}

static void floor_ds_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefDoubleFloat(left);
	b = (double_float)RefSingleFloat(right);
	a = lisp_floor_d(a, b, &b);
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);
}

static void floor_dd_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefDoubleFloat(left);
	b = RefDoubleFloat(right);
	a = lisp_floor_d(a, b, &b);
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);
}

static void floor_dl_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefDoubleFloat(left);
	b = RefLongFloat(right);
	a = lisp_floor_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void floor_double_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			floor_df_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			floor_db_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			floor_dr_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			floor_ds_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			floor_dd_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			floor_dl_common(local, quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void floor_lf_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefLongFloat(left);
	b = long_float_fixnum(right);
	a = lisp_floor_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void floor_lb_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefLongFloat(left);
	b = long_float_bignum(right);
	a = lisp_floor_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void floor_lr_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefLongFloat(left);
	b = long_float_ratio(right);
	a = lisp_floor_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void floor_ls_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefSingleFloat(right);
	a = lisp_floor_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void floor_ld_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefDoubleFloat(right);
	a = lisp_floor_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void floor_ll_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = RefLongFloat(left);
	b = RefLongFloat(right);
	a = lisp_floor_l(a, b, &b);
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);
}

static void floor_long_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			floor_lf_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			floor_lb_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			floor_lr_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			floor_ls_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			floor_ld_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			floor_ll_common(local, quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

void floor_common(LocalRoot local, addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			floor_fixnum_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			floor_bignum_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			floor_ratio_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			floor_single_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			floor_double_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			floor_long_common(local, quot, rem, left, right);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

static void ffloor_ff_common(addr *quot, addr *rem, addr left, addr right)
{
	fixnum a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	GetFixnum(left, &a);
	GetFixnum(right, &b);
	lisp_ffloor_fixnum(quot, rem, a, b);
}

static void ffloor_fb_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_BIGNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	lisp_ffloor_bignum(local, quot, rem, left, right);
	rollback_local(local, stack);
}

static void ffloor_fr_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_RATIO);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	lisp_ffloor_br_ratio(local, quot, rem, left, right);
	rollback_local(local, stack);
}

static void ffloor_fs_common(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = single_float_fixnum(left);
	b = RefSingleFloat(right);
	a = lisp_floor_s(a, b, &b);
	single_float_heap(quot, a);
	single_float_heap(rem, b);
}

static void ffloor_fd_common(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = double_float_fixnum(left);
	b = RefDoubleFloat(right);
	a = lisp_floor_d(a, b, &b);
	double_float_heap(quot, a);
	double_float_heap(rem, b);
}

static void ffloor_fl_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = long_float_fixnum(left);
	b = RefLongFloat(right);
	a = lisp_floor_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void ffloor_fixnum_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			ffloor_ff_common(quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			ffloor_fb_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			ffloor_fr_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			ffloor_fs_common(quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			ffloor_fd_common(quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			ffloor_fl_common(quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void ffloor_bf_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	lisp_ffloor_bignum(local, quot, rem, left, right);
	rollback_local(local, stack);
}

static void ffloor_bs_common(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = single_float_bignum(left);
	b = RefSingleFloat(right);
	a = lisp_floor_s(a, b, &b);
	single_float_heap(quot, a);
	single_float_heap(rem, b);
}

static void ffloor_bd_common(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = double_float_bignum(left);
	b = RefDoubleFloat(right);
	a = lisp_floor_d(a, b, &b);
	double_float_heap(quot, a);
	double_float_heap(rem, b);
}

static void ffloor_bl_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = long_float_bignum(left);
	b = RefLongFloat(right);
	a = lisp_floor_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void ffloor_bignum_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			ffloor_bf_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			lisp_ffloor_bignum(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			lisp_ffloor_br_ratio(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			ffloor_bs_common(quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			ffloor_bd_common(quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			ffloor_bl_common(quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void ffloor_rf_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	lisp_ffloor_rb_ratio(local, quot, rem, left, right);
	rollback_local(local, stack);
}

static void ffloor_rs_common(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = single_float_ratio(left);
	b = RefSingleFloat(right);
	a = lisp_floor_s(a, b, &b);
	single_float_heap(quot, a);
	single_float_heap(rem, b);
}

static void ffloor_rd_common(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = double_float_ratio(left);
	b = RefDoubleFloat(right);
	a = lisp_floor_d(a, b, &b);
	double_float_heap(quot, a);
	double_float_heap(rem, b);
}

static void ffloor_rl_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = long_float_ratio(left);
	b = RefLongFloat(right);
	a = lisp_floor_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void ffloor_ratio_common(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			ffloor_rf_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			lisp_ffloor_rb_ratio(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			lisp_ffloor_rr_ratio(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			ffloor_rs_common(quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			ffloor_rd_common(quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			ffloor_rl_common(quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void ffloor_sf_common(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefSingleFloat(left);
	b = single_float_fixnum(right);
	a = lisp_floor_s(a, b, &b);
	single_float_heap(quot, a);
	single_float_heap(rem, b);
}

static void ffloor_sb_common(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefSingleFloat(left);
	b = single_float_bignum(right);
	a = lisp_floor_s(a, b, &b);
	single_float_heap(quot, a);
	single_float_heap(rem, b);
}

static void ffloor_sr_common(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefSingleFloat(left);
	b = single_float_ratio(right);
	a = lisp_floor_s(a, b, &b);
	single_float_heap(quot, a);
	single_float_heap(rem, b);
}

static void ffloor_ss_common(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefSingleFloat(left);
	b = RefSingleFloat(right);
	a = lisp_floor_s(a, b, &b);
	single_float_heap(quot, a);
	single_float_heap(rem, b);
}

static void ffloor_sd_common(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = (double_float)RefSingleFloat(left);
	b = RefDoubleFloat(right);
	a = lisp_floor_d(a, b, &b);
	double_float_heap(quot, a);
	double_float_heap(rem, b);
}

static void ffloor_sl_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefSingleFloat(left);
	b = RefLongFloat(right);
	a = lisp_floor_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void ffloor_single_common(addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			ffloor_sf_common(quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			ffloor_sb_common(quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			ffloor_sr_common(quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			ffloor_ss_common(quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			ffloor_sd_common(quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			ffloor_sl_common(quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void ffloor_df_common(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefDoubleFloat(left);
	b = double_float_fixnum(right);
	a = lisp_floor_d(a, b, &b);
	double_float_heap(quot, a);
	double_float_heap(rem, b);
}

static void ffloor_db_common(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefDoubleFloat(left);
	b = double_float_bignum(right);
	a = lisp_floor_d(a, b, &b);
	double_float_heap(quot, a);
	double_float_heap(rem, b);
}

static void ffloor_dr_common(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefDoubleFloat(left);
	b = double_float_ratio(right);
	a = lisp_floor_d(a, b, &b);
	double_float_heap(quot, a);
	double_float_heap(rem, b);
}

static void ffloor_ds_common(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefDoubleFloat(left);
	b = (double_float)RefSingleFloat(right);
	a = lisp_floor_d(a, b, &b);
	double_float_heap(quot, a);
	double_float_heap(rem, b);
}

static void ffloor_dd_common(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefDoubleFloat(left);
	b = RefDoubleFloat(right);
	a = lisp_floor_d(a, b, &b);
	double_float_heap(quot, a);
	double_float_heap(rem, b);
}

static void ffloor_dl_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefDoubleFloat(left);
	b = RefLongFloat(right);
	a = lisp_floor_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void ffloor_double_common(addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			ffloor_df_common(quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			ffloor_db_common(quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			ffloor_dr_common(quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			ffloor_ds_common(quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			ffloor_dd_common(quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			ffloor_dl_common(quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

static void ffloor_lf_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefLongFloat(left);
	b = long_float_fixnum(right);
	a = lisp_floor_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void ffloor_lb_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefLongFloat(left);
	b = long_float_bignum(right);
	a = lisp_floor_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void ffloor_lr_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefLongFloat(left);
	b = long_float_ratio(right);
	a = lisp_floor_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void ffloor_ls_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefSingleFloat(right);
	a = lisp_floor_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void ffloor_ld_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefDoubleFloat(right);
	a = lisp_floor_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void ffloor_ll_common(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = RefLongFloat(left);
	b = RefLongFloat(right);
	a = lisp_floor_l(a, b, &b);
	long_float_heap(quot, a);
	long_float_heap(rem, b);
}

static void ffloor_long_common(addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			ffloor_lf_common(quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			ffloor_lb_common(quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			ffloor_lr_common(quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			ffloor_ls_common(quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			ffloor_ld_common(quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			ffloor_ll_common(quot, rem, left, right);
			break;

		default:
			TypeError(right, REAL);
			break;
	}
}

void ffloor_common(LocalRoot local, addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			ffloor_fixnum_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_BIGNUM:
			ffloor_bignum_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_RATIO:
			ffloor_ratio_common(local, quot, rem, left, right);
			break;

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			ffloor_single_common(quot, rem, left, right);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			ffloor_double_common(quot, rem, left, right);
			break;

		case LISPTYPE_LONG_FLOAT:
			ffloor_long_common(quot, rem, left, right);
			break;

		default:
			TypeError(left, REAL);
			break;
	}
}

