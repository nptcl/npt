#include "bignum.h"
#include "bignum_data.h"
#include "bignum_object.h"
#include "condition.h"
#include "ratio.h"
#include "real_division.h"
#include "real_ceiling.h"

/*
 *  common
 */
static int ceiling1_float_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	single_float v, r;

	GetSingleFloat(left, &v);
	Return(float_ceiling1_s_(v, &v, &r));
	single_float_integer_heap(local, quot, v);
	single_float_heap(rem, r);

	return 0;
}

static int ceiling1_double_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	double_float v, r;

	GetDoubleFloat(left, &v);
	Return(float_ceiling1_d_(v, &v, &r));
	double_float_integer_heap(local, quot, v);
	double_float_heap(rem, r);

	return 0;
}

static int ceiling1_long_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	long_float v, r;

	GetLongFloat(left, &v);
	Return(float_ceiling1_l_(v, &v, &r));
	long_float_integer_heap(local, quot, v);
	long_float_heap(rem, r);

	return 0;
}

int ceiling1_common_(LocalRoot local, addr *quot, addr *rem, addr left)
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
			return float_ceiling1_ratio_(local, quot, rem, left);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return ceiling1_float_(local, quot, rem, left);

		case LISPTYPE_DOUBLE_FLOAT:
			return ceiling1_double_(local, quot, rem, left);

		case LISPTYPE_LONG_FLOAT:
			return ceiling1_long_(local, quot, rem, left);

		default:
			*quot = *rem = Nil;
			return TypeError_(left, REAL);
	}

	return 0;
}

static int fceiling1_float_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	single_float v, r;

	GetSingleFloat(left, &v);
	Return(float_ceiling1_s_(v, &v, &r));
	single_float_heap(quot, v);
	single_float_heap(rem, r);

	return 0;
}

static int fceiling1_double_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	double_float v, r;

	GetDoubleFloat(left, &v);
	Return(float_ceiling1_d_(v, &v, &r));
	double_float_heap(quot, v);
	double_float_heap(rem, r);

	return 0;
}

static int fceiling1_long_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	long_float v, r;

	GetLongFloat(left, &v);
	Return(float_ceiling1_l_(v, &v, &r));
	long_float_heap(quot, v);
	long_float_heap(rem, r);

	return 0;
}

int fceiling1_common_(LocalRoot local, addr *quot, addr *rem, addr left)
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
			return float_fceiling1_ratio_(local, quot, rem, left);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return fceiling1_float_(local, quot, rem, left);

		case LISPTYPE_DOUBLE_FLOAT:
			return fceiling1_double_(local, quot, rem, left);

		case LISPTYPE_LONG_FLOAT:
			return fceiling1_long_(local, quot, rem, left);

		default:
			*quot = *rem = Nil;
			return TypeError_(left, REAL);
	}

	return 0;
}

static int ceiling_ff_common_(addr *quot, addr *rem, addr left, addr right)
{
	fixnum a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	GetFixnum(left, &a);
	GetFixnum(right, &b);
	return float_ceiling_fixnum_(quot, rem, a, b);
}

static int ceiling_fb_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_BIGNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	Return(float_ceiling_bignum_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int ceiling_fr_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_RATIO);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	Return(float_ceiling_br_ratio_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int ceiling_fs_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = single_float_fixnum(left);
	b = RefSingleFloat(right);
	Return(float_ceiling_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int ceiling_fd_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = double_float_fixnum(left);
	b = RefDoubleFloat(right);
	Return(float_ceiling_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ceiling_fl_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = long_float_fixnum(left);
	b = RefLongFloat(right);
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ceiling_fixnum_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return ceiling_ff_common_(quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return ceiling_fb_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return ceiling_fr_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return ceiling_fs_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return ceiling_fd_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return ceiling_fl_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int ceiling_bf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	Return(float_ceiling_bignum_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int ceiling_bs_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	Return(single_float_bignum_(left, &a));
	b = RefSingleFloat(right);
	Return(float_ceiling_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int ceiling_bd_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	Return(double_float_bignum_(left, &a));
	b = RefDoubleFloat(right);
	Return(float_ceiling_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ceiling_bl_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	Return(long_float_bignum_(left, &a));
	b = RefLongFloat(right);
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ceiling_bignum_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return ceiling_bf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return float_ceiling_bignum_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return float_ceiling_br_ratio_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return ceiling_bs_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return ceiling_bd_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return ceiling_bl_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int ceiling_rf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	Return(float_ceiling_rb_ratio_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int ceiling_rs_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	Return(single_float_ratio_(left, &a));
	b = RefSingleFloat(right);
	Return(float_ceiling_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int ceiling_rd_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	Return(double_float_ratio_(left, &a));
	b = RefDoubleFloat(right);
	Return(float_ceiling_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ceiling_rl_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	Return(long_float_ratio_(left, &a));
	b = RefLongFloat(right);
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ceiling_ratio_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return ceiling_rf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return float_ceiling_rb_ratio_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return float_ceiling_rr_ratio_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return ceiling_rs_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return ceiling_rd_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return ceiling_rl_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int ceiling_sf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefSingleFloat(left);
	b = single_float_fixnum(right);
	Return(float_ceiling_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int ceiling_sb_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefSingleFloat(left);
	Return(single_float_bignum_(right, &b));
	Return(float_ceiling_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int ceiling_sr_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefSingleFloat(left);
	Return(single_float_ratio_(right, &b));
	Return(float_ceiling_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int ceiling_ss_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefSingleFloat(left);
	b = RefSingleFloat(right);
	Return(float_ceiling_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int ceiling_sd_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = (double_float)RefSingleFloat(left);
	b = RefDoubleFloat(right);
	Return(float_ceiling_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ceiling_sl_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefSingleFloat(left);
	b = RefLongFloat(right);
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ceiling_single_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return ceiling_sf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return ceiling_sb_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return ceiling_sr_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return ceiling_ss_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return ceiling_sd_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return ceiling_sl_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int ceiling_df_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefDoubleFloat(left);
	b = double_float_fixnum(right);
	Return(float_ceiling_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ceiling_db_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefDoubleFloat(left);
	Return(double_float_bignum_(right, &b));
	Return(float_ceiling_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ceiling_dr_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefDoubleFloat(left);
	Return(double_float_ratio_(right, &b));
	Return(float_ceiling_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ceiling_ds_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefDoubleFloat(left);
	b = (double_float)RefSingleFloat(right);
	Return(float_ceiling_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ceiling_dd_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefDoubleFloat(left);
	b = RefDoubleFloat(right);
	Return(float_ceiling_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int ceiling_dl_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefDoubleFloat(left);
	b = RefLongFloat(right);
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ceiling_double_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return ceiling_df_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return ceiling_db_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return ceiling_dr_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return ceiling_ds_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return ceiling_dd_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return ceiling_dl_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int ceiling_lf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefLongFloat(left);
	b = long_float_fixnum(right);
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ceiling_lb_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefLongFloat(left);
	Return(long_float_bignum_(right, &b));
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ceiling_lr_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefLongFloat(left);
	Return(long_float_ratio_(right, &b));
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ceiling_ls_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefSingleFloat(right);
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ceiling_ld_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefDoubleFloat(right);
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ceiling_ll_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = RefLongFloat(left);
	b = RefLongFloat(right);
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int ceiling_long_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return ceiling_lf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return ceiling_lb_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return ceiling_lr_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return ceiling_ls_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return ceiling_ld_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return ceiling_ll_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

int ceiling2_common_(LocalRoot local, addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return ceiling_fixnum_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return ceiling_bignum_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return ceiling_ratio_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return ceiling_single_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return ceiling_double_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return ceiling_long_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(left, REAL);
	}
}

static int fceiling_ff_common_(addr *quot, addr *rem, addr left, addr right)
{
	fixnum a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	GetFixnum(left, &a);
	GetFixnum(right, &b);
	return float_fceiling_fixnum_(quot, rem, a, b);
}

static int fceiling_fb_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_BIGNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	Return(float_fceiling_bignum_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int fceiling_fr_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_RATIO);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	Return(float_fceiling_br_ratio_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int fceiling_fs_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = single_float_fixnum(left);
	b = RefSingleFloat(right);
	Return(float_ceiling_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int fceiling_fd_common_(addr *quot, addr *rem, addr left, addr right)

{
	double_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = double_float_fixnum(left);
	b = RefDoubleFloat(right);
	Return(float_ceiling_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int fceiling_fl_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = long_float_fixnum(left);
	b = RefLongFloat(right);
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fceiling_fixnum_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return fceiling_ff_common_(quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return fceiling_fb_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return fceiling_fr_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return fceiling_fs_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return fceiling_fd_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return fceiling_fl_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int fceiling_bf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	Return(float_fceiling_bignum_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int fceiling_bs_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	Return(single_float_bignum_(left, &a));
	b = RefSingleFloat(right);
	Return(float_ceiling_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int fceiling_bd_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	Return(double_float_bignum_(left, &a));
	b = RefDoubleFloat(right);
	Return(float_ceiling_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int fceiling_bl_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	Return(long_float_bignum_(left, &a));
	b = RefLongFloat(right);
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fceiling_bignum_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return fceiling_bf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return float_fceiling_bignum_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return float_fceiling_br_ratio_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return fceiling_bs_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return fceiling_bd_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return fceiling_bl_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int fceiling_rf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	Return(float_fceiling_rb_ratio_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int fceiling_rs_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	Return(single_float_ratio_(left, &a));
	b = RefSingleFloat(right);
	Return(float_ceiling_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int fceiling_rd_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	Return(double_float_ratio_(left, &a));
	b = RefDoubleFloat(right);
	Return(float_ceiling_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int fceiling_rl_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	Return(long_float_ratio_(left, &a));
	b = RefLongFloat(right);
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fceiling_ratio_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return fceiling_rf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return float_fceiling_rb_ratio_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return float_fceiling_rr_ratio_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return fceiling_rs_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return fceiling_rd_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return fceiling_rl_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int fceiling_sf_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefSingleFloat(left);
	b = single_float_fixnum(right);
	Return(float_ceiling_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int fceiling_sb_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefSingleFloat(left);
	Return(single_float_bignum_(right, &b));
	Return(float_ceiling_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int fceiling_sr_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefSingleFloat(left);
	Return(single_float_ratio_(right, &b));
	Return(float_ceiling_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int fceiling_ss_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefSingleFloat(left);
	b = RefSingleFloat(right);
	Return(float_ceiling_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int fceiling_sd_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = (double_float)RefSingleFloat(left);
	b = RefDoubleFloat(right);
	Return(float_ceiling_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int fceiling_sl_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefSingleFloat(left);
	b = RefLongFloat(right);
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fceiling_single_common_(addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return fceiling_sf_common_(quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return fceiling_sb_common_(quot, rem, left, right);

		case LISPTYPE_RATIO:
			return fceiling_sr_common_(quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return fceiling_ss_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return fceiling_sd_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return fceiling_sl_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int fceiling_df_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefDoubleFloat(left);
	b = double_float_fixnum(right);
	Return(float_ceiling_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int fceiling_db_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefDoubleFloat(left);
	Return(double_float_bignum_(right, &b));
	Return(float_ceiling_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int fceiling_dr_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefDoubleFloat(left);
	Return(double_float_ratio_(right, &b));
	Return(float_ceiling_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int fceiling_ds_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefDoubleFloat(left);
	b = (double_float)RefSingleFloat(right);
	Return(float_ceiling_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int fceiling_dd_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefDoubleFloat(left);
	b = RefDoubleFloat(right);
	Return(float_ceiling_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int fceiling_dl_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefDoubleFloat(left);
	b = RefLongFloat(right);
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fceiling_double_common_(addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return fceiling_df_common_(quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return fceiling_db_common_(quot, rem, left, right);

		case LISPTYPE_RATIO:
			return fceiling_dr_common_(quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return fceiling_ds_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return fceiling_dd_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return fceiling_dl_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int fceiling_lf_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefLongFloat(left);
	b = long_float_fixnum(right);
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fceiling_lb_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefLongFloat(left);
	Return(long_float_bignum_(right, &b));
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fceiling_lr_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefLongFloat(left);
	Return(long_float_ratio_(right, &b));
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fceiling_ls_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefSingleFloat(right);
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fceiling_ld_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefDoubleFloat(right);
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fceiling_ll_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = RefLongFloat(left);
	b = RefLongFloat(right);
	Return(float_ceiling_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fceiling_long_common_(addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return fceiling_lf_common_(quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return fceiling_lb_common_(quot, rem, left, right);

		case LISPTYPE_RATIO:
			return fceiling_lr_common_(quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return fceiling_ls_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return fceiling_ld_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return fceiling_ll_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

int fceiling2_common_(LocalRoot local, addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return fceiling_fixnum_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return fceiling_bignum_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return fceiling_ratio_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return fceiling_single_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return fceiling_double_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return fceiling_long_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(left, REAL);
	}
}

int ceiling_common_(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2)
{
	if (div == Unbound)
		return ceiling1_common_(local, ret1, ret2, var);
	else
		return ceiling2_common_(local, ret1, ret2, var, div);
}

int fceiling_common_(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2)
{
	if (div == Unbound)
		return fceiling1_common_(local, ret1, ret2, var);
	else
		return fceiling2_common_(local, ret1, ret2, var, div);
}

