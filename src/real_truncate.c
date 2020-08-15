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
static int truncate1_float_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	single_float v, r;

	GetSingleFloat(left, &v);
	Return(lisp_truncate1_s_(v, &v, &r));
	single_float_integer_heap(local, quot, v);
	single_float_heap(rem, r);

	return 0;
}

static int truncate1_double_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	double_float v, r;

	GetDoubleFloat(left, &v);
	Return(lisp_truncate1_d_(v, &v, &r));
	double_float_integer_heap(local, quot, v);
	double_float_heap(rem, r);

	return 0;
}

static int truncate1_long_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	long_float v, r;

	GetLongFloat(left, &v);
	Return(lisp_truncate1_l_(v, &v, &r));
	long_float_integer_heap(local, quot, v);
	long_float_heap(rem, r);

	return 0;
}

_g int truncate1_common_(LocalRoot local, addr *quot, addr *rem, addr left)
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
			return lisp_truncate1_ratio_(local, quot, rem, left);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return truncate1_float_(local, quot, rem, left);

		case LISPTYPE_DOUBLE_FLOAT:
			return truncate1_double_(local, quot, rem, left);

		case LISPTYPE_LONG_FLOAT:
			return truncate1_long_(local, quot, rem, left);

		default:
			*quot = *rem = Nil;
			return TypeError_(left, REAL);
	}

	return 0;
}

static int ftruncate1_float_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	single_float v, r;

	GetSingleFloat(left, &v);
	Return(lisp_truncate1_s_(v, &v, &r));
	single_float_heap(quot, v);
	single_float_heap(rem, r);

	return 0;
}

static int ftruncate1_double_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	double_float v, r;

	GetDoubleFloat(left, &v);
	Return(lisp_truncate1_d_(v, &v, &r));
	double_float_heap(quot, v);
	double_float_heap(rem, r);

	return 0;
}

static int ftruncate1_long_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	long_float v, r;

	GetLongFloat(left, &v);
	Return(lisp_truncate1_l_(v, &v, &r));
	long_float_heap(quot, v);
	long_float_heap(rem, r);

	return 0;
}

_g int ftruncate1_common_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			single_float_fixnum_heap(quot, left);
			fixnum_heap(rem, 0);
			break;

		case LISPTYPE_BIGNUM:
			Return(single_float_bignum_heap_(quot, left));
			fixnum_heap(rem, 0);
			break;

		case LISPTYPE_RATIO:
			return lisp_ftruncate1_ratio_(local, quot, rem, left);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return ftruncate1_float_(local, quot, rem, left);

		case LISPTYPE_DOUBLE_FLOAT:
			return ftruncate1_double_(local, quot, rem, left);

		case LISPTYPE_LONG_FLOAT:
			return ftruncate1_long_(local, quot, rem, left);

		default:
			*quot = *rem = Nil;
			return TypeError_(left, REAL);
	}

	return 0;
}

static int truncate_ff_common_(addr *quot, addr *rem, addr left, addr right)
{
	fixnum a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	GetFixnum(left, &a);
	GetFixnum(right, &b);
	return lisp_truncate_fixnum_(quot, rem, a, b);
}

static int truncate_fb_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_BIGNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	Return(lisp_truncate_bignum_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int truncate_fr_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_RATIO);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	Return(lisp_truncate_br_ratio_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int truncate_fs_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = single_float_fixnum(left);
	b = RefSingleFloat(right);
	Return(lisp_truncate_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int truncate_fd_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = double_float_fixnum(left);
	b = RefDoubleFloat(right);
	Return(lisp_truncate_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int truncate_fl_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = long_float_fixnum(left);
	b = RefLongFloat(right);
	Return(lisp_truncate_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int truncate_fixnum_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return truncate_ff_common_(quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return truncate_fb_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return truncate_fr_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return truncate_fs_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return truncate_fd_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return truncate_fl_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int truncate_bf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	Return(lisp_truncate_bignum_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int truncate_bs_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	Return(single_float_bignum_(left, &a));
	b = RefSingleFloat(right);
	Return(lisp_truncate_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int truncate_bd_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	Return(double_float_bignum_(left, &a));
	b = RefDoubleFloat(right);
	Return(lisp_truncate_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int truncate_bl_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	Return(long_float_bignum_(left, &a));
	b = RefLongFloat(right);
	Return(lisp_truncate_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int truncate_bignum_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return truncate_bf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return lisp_truncate_bignum_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return lisp_truncate_br_ratio_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return truncate_bs_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return truncate_bd_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return truncate_bl_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int truncate_rf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	Return(lisp_truncate_rb_ratio_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int truncate_rs_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	Return(single_float_ratio_(left, &a));
	b = RefSingleFloat(right);
	Return(lisp_truncate_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int truncate_rd_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	Return(double_float_ratio_(left, &a));
	b = RefDoubleFloat(right);
	Return(lisp_truncate_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int truncate_rl_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	Return(long_float_ratio_(left, &a));
	b = RefLongFloat(right);
	Return(lisp_truncate_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int truncate_ratio_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return truncate_rf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return lisp_truncate_rb_ratio_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return lisp_truncate_rr_ratio_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return truncate_rs_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return truncate_rd_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return truncate_rl_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int truncate_sf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefSingleFloat(left);
	b = single_float_fixnum(right);
	Return(lisp_truncate_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int truncate_sb_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefSingleFloat(left);
	Return(single_float_bignum_(right, &b));
	Return(lisp_truncate_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int truncate_sr_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefSingleFloat(left);
	Return(single_float_ratio_(right, &b));
	Return(lisp_truncate_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int truncate_ss_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefSingleFloat(left);
	b = RefSingleFloat(right);
	Return(lisp_truncate_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int truncate_sd_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = (double_float)RefSingleFloat(left);
	b = RefDoubleFloat(right);
	Return(lisp_truncate_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int truncate_sl_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefSingleFloat(left);
	b = RefLongFloat(right);
	Return(lisp_truncate_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int truncate_single_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return truncate_sf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return truncate_sb_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return truncate_sr_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return truncate_ss_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return truncate_sd_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return truncate_sl_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int truncate_df_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefDoubleFloat(left);
	b = double_float_fixnum(right);
	Return(lisp_truncate_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int truncate_db_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefDoubleFloat(left);
	Return(double_float_bignum_(right, &b));
	Return(lisp_truncate_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int truncate_dr_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefDoubleFloat(left);
	Return(double_float_ratio_(right, &b));
	Return(lisp_truncate_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int truncate_ds_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefDoubleFloat(left);
	b = (double_float)RefSingleFloat(right);
	Return(lisp_truncate_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int truncate_dd_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefDoubleFloat(left);
	b = RefDoubleFloat(right);
	Return(lisp_truncate_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int truncate_dl_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefDoubleFloat(left);
	b = RefLongFloat(right);
	Return(lisp_truncate_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int truncate_double_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return truncate_df_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return truncate_db_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return truncate_dr_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return truncate_ds_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return truncate_dd_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return truncate_dl_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int truncate_lf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefLongFloat(left);
	b = long_float_fixnum(right);
	Return(lisp_truncate_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int truncate_lb_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefLongFloat(left);
	Return(long_float_bignum_(right, &b));
	Return(lisp_truncate_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int truncate_lr_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefLongFloat(left);
	Return(long_float_ratio_(right, &b));
	Return(lisp_truncate_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int truncate_ls_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefSingleFloat(right);
	Return(lisp_truncate_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int truncate_ld_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefDoubleFloat(right);
	Return(lisp_truncate_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int truncate_ll_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = RefLongFloat(left);
	b = RefLongFloat(right);
	Return(lisp_truncate_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int truncate_long_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return truncate_lf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return truncate_lb_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return truncate_lr_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return truncate_ls_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return truncate_ld_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return truncate_ll_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

_g int truncate2_common_(LocalRoot local, addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return truncate_fixnum_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return truncate_bignum_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return truncate_ratio_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return truncate_single_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return truncate_double_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return truncate_long_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(left, REAL);
	}
}

static int ftruncate_ff_common_(addr *quot, addr *rem, addr left, addr right)
{
	fixnum a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	GetFixnum(left, &a);
	GetFixnum(right, &b);
	return lisp_ftruncate_fixnum_(quot, rem, a, b);
}

static int ftruncate_fb_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_BIGNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	Return(lisp_ftruncate_bignum_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int ftruncate_fr_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_RATIO);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	Return(lisp_ftruncate_br_ratio_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int ftruncate_fs_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = single_float_fixnum(left);
	b = RefSingleFloat(right);
	Return(lisp_truncate_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int ftruncate_fd_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = double_float_fixnum(left);
	b = RefDoubleFloat(right);
	Return(lisp_truncate_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ftruncate_fl_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = long_float_fixnum(left);
	b = RefLongFloat(right);
	Return(lisp_truncate_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ftruncate_fixnum_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return ftruncate_ff_common_(quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return ftruncate_fb_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return ftruncate_fr_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return ftruncate_fs_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return ftruncate_fd_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return ftruncate_fl_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int ftruncate_bf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	Return(lisp_ftruncate_bignum_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int ftruncate_bs_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	Return(single_float_bignum_(left, &a));
	b = RefSingleFloat(right);
	Return(lisp_truncate_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int ftruncate_bd_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	Return(double_float_bignum_(left, &a));
	b = RefDoubleFloat(right);
	Return(lisp_truncate_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ftruncate_bl_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	Return(long_float_bignum_(left, &a));
	b = RefLongFloat(right);
	Return(lisp_truncate_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ftruncate_bignum_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return ftruncate_bf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return lisp_ftruncate_bignum_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return lisp_ftruncate_br_ratio_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return ftruncate_bs_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return ftruncate_bd_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return ftruncate_bl_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int ftruncate_rf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	Return(lisp_ftruncate_rb_ratio_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int ftruncate_rs_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	Return(single_float_ratio_(left, &a));
	b = RefSingleFloat(right);
	Return(lisp_truncate_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int ftruncate_rd_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	Return(double_float_ratio_(left, &a));
	b = RefDoubleFloat(right);
	Return(lisp_truncate_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ftruncate_rl_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	Return(long_float_ratio_(left, &a));
	b = RefLongFloat(right);
	Return(lisp_truncate_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ftruncate_ratio_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return ftruncate_rf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return lisp_ftruncate_rb_ratio_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return lisp_ftruncate_rr_ratio_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return ftruncate_rs_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return ftruncate_rd_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return ftruncate_rl_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int ftruncate_sf_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefSingleFloat(left);
	b = single_float_fixnum(right);
	Return(lisp_truncate_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int ftruncate_sb_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefSingleFloat(left);
	Return(single_float_bignum_(right, &b));
	Return(lisp_truncate_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int ftruncate_sr_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefSingleFloat(left);
	Return(single_float_ratio_(right, &b));
	Return(lisp_truncate_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int ftruncate_ss_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefSingleFloat(left);
	b = RefSingleFloat(right);
	Return(lisp_truncate_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int ftruncate_sd_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = (double_float)RefSingleFloat(left);
	b = RefDoubleFloat(right);
	Return(lisp_truncate_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ftruncate_sl_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefSingleFloat(left);
	b = RefLongFloat(right);
	Return(lisp_truncate_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ftruncate_single_common_(addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return ftruncate_sf_common_(quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return ftruncate_sb_common_(quot, rem, left, right);

		case LISPTYPE_RATIO:
			return ftruncate_sr_common_(quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return ftruncate_ss_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return ftruncate_sd_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return ftruncate_sl_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int ftruncate_df_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefDoubleFloat(left);
	b = double_float_fixnum(right);
	Return(lisp_truncate_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ftruncate_db_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefDoubleFloat(left);
	Return(double_float_bignum_(right, &b));
	Return(lisp_truncate_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ftruncate_dr_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefDoubleFloat(left);
	Return(double_float_ratio_(right, &b));
	Return(lisp_truncate_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ftruncate_ds_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefDoubleFloat(left);
	b = (double_float)RefSingleFloat(right);
	Return(lisp_truncate_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ftruncate_dd_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefDoubleFloat(left);
	b = RefDoubleFloat(right);
	Return(lisp_truncate_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ftruncate_dl_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefDoubleFloat(left);
	b = RefLongFloat(right);
	Return(lisp_truncate_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ftruncate_double_common_(addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return ftruncate_df_common_(quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return ftruncate_db_common_(quot, rem, left, right);

		case LISPTYPE_RATIO:
			return ftruncate_dr_common_(quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return ftruncate_ds_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return ftruncate_dd_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return ftruncate_dl_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int ftruncate_lf_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefLongFloat(left);
	b = long_float_fixnum(right);
	Return(lisp_truncate_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ftruncate_lb_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefLongFloat(left);
	Return(long_float_bignum_(right, &b));
	Return(lisp_truncate_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ftruncate_lr_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefLongFloat(left);
	Return(long_float_ratio_(right, &b));
	Return(lisp_truncate_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ftruncate_ls_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefSingleFloat(right);
	Return(lisp_truncate_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ftruncate_ld_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefDoubleFloat(right);
	Return(lisp_truncate_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ftruncate_ll_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = RefLongFloat(left);
	b = RefLongFloat(right);
	Return(lisp_truncate_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ftruncate_long_common_(addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return ftruncate_lf_common_(quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return ftruncate_lb_common_(quot, rem, left, right);

		case LISPTYPE_RATIO:
			return ftruncate_lr_common_(quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return ftruncate_ls_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return ftruncate_ld_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return ftruncate_ll_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

_g int ftruncate2_common_(LocalRoot local, addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return ftruncate_fixnum_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return ftruncate_bignum_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return ftruncate_ratio_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return ftruncate_single_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return ftruncate_double_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return ftruncate_long_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(left, REAL);
	}
}

_g int truncate_common_(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2)
{
	if (div == Unbound)
		return truncate1_common_(local, ret1, ret2, var);
	else
		return truncate2_common_(local, ret1, ret2, var, div);
}

_g int ftruncate_common_(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2)
{
	if (div == Unbound)
		return ftruncate1_common_(local, ret1, ret2, var);
	else
		return ftruncate2_common_(local, ret1, ret2, var, div);
}


/*
 *  rem
 */
static int rem_ff_common_(addr *ret, addr left, addr right)
{
	fixnum a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	GetFixnum(left, &a);
	GetFixnum(right, &b);
	return lisp_rem_fixnum_(ret, a, b);
}

static int rem_fb_common_(LocalRoot local, addr *ret, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_BIGNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	Return(lisp_rem_bignum_(local, ret, left, right));
	rollback_local(local, stack);

	return 0;
}

static int rem_fr_common_(LocalRoot local, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_RATIO);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	Return(lisp_rem_br_ratio_(local, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int rem_bf_common_(LocalRoot local, addr *ret, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	Return(lisp_rem_bignum_(local, ret, left, right));
	rollback_local(local, stack);

	return 0;
}

static int rem_rf_common_(LocalRoot local, addr *ret, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	Return(lisp_rem_rb_ratio_(local, ret, left, right));
	rollback_local(local, stack);

	return 0;
}

static int rem_fixnum_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return rem_ff_common_(ret, left, right);

		case LISPTYPE_BIGNUM:
			return rem_fb_common_(local, ret, left, right);

		case LISPTYPE_RATIO:
			return rem_fr_common_(local, ret, left, right);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

static int rem_bignum_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return rem_bf_common_(local, ret, left, right);

		case LISPTYPE_BIGNUM:
			return lisp_rem_bignum_(local, ret, left, right);

		case LISPTYPE_RATIO:
			return lisp_rem_br_ratio_(local, ret, left, right);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

static int rem_ratio_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return rem_rf_common_(local, ret, left, right);

		case LISPTYPE_BIGNUM:
			return lisp_rem_rb_ratio_(local, ret, left, right);

		case LISPTYPE_RATIO:
			return lisp_rem_rr_ratio_(local, ret, left, right);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

_g int rem_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return rem_fixnum_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return rem_bignum_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return rem_ratio_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}

