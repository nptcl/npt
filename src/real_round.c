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
static int round1_float_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	single_float v, r;

	GetSingleFloat(left, &v);
	Return(float_round1_s_(v, &v, &r));
	single_float_integer_heap(local, quot, v);
	single_float_heap(rem, r);

	return 0;
}

static int round1_double_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	double_float v, r;

	GetDoubleFloat(left, &v);
	Return(float_round1_d_(v, &v, &r));
	double_float_integer_heap(local, quot, v);
	double_float_heap(rem, r);

	return 0;
}

static int round1_long_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	long_float v, r;

	GetLongFloat(left, &v);
	Return(float_round1_l_(v, &v, &r));
	long_float_integer_heap(local, quot, v);
	long_float_heap(rem, r);

	return 0;
}

_g int round1_common_(LocalRoot local, addr *quot, addr *rem, addr left)
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
			return float_round1_ratio_(local, quot, rem, left);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return round1_float_(local, quot, rem, left);

		case LISPTYPE_DOUBLE_FLOAT:
			return round1_double_(local, quot, rem, left);

		case LISPTYPE_LONG_FLOAT:
			return round1_long_(local, quot, rem, left);

		default:
			*quot = *rem = Nil;
			return TypeError_(left, REAL);
	}

	return 0;
}

static int fround1_float_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	single_float v, r;

	GetSingleFloat(left, &v);
	Return(float_round1_s_(v, &v, &r));
	single_float_heap(quot, v);
	single_float_heap(rem, r);

	return 0;
}

static int fround1_double_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	double_float v, r;

	GetDoubleFloat(left, &v);
	Return(float_round1_d_(v, &v, &r));
	double_float_heap(quot, v);
	double_float_heap(rem, r);

	return 0;
}

static int fround1_long_(LocalRoot local, addr *quot, addr *rem, addr left)
{
	long_float v, r;

	GetLongFloat(left, &v);
	Return(float_round1_l_(v, &v, &r));
	long_float_heap(quot, v);
	long_float_heap(rem, r);

	return 0;
}

_g int fround1_common_(LocalRoot local, addr *quot, addr *rem, addr left)
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
			return float_fround1_ratio_(local, quot, rem, left);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return fround1_float_(local, quot, rem, left);

		case LISPTYPE_DOUBLE_FLOAT:
			return fround1_double_(local, quot, rem, left);

		case LISPTYPE_LONG_FLOAT:
			return fround1_long_(local, quot, rem, left);

		default:
			*quot = *rem = Nil;
			return TypeError_(left, REAL);
	}

	return 0;
}

static int round_ff_common_(addr *quot, addr *rem, addr left, addr right)
{
	fixnum a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	GetFixnum(left, &a);
	GetFixnum(right, &b);
	return float_round_fixnum_(quot, rem, a, b);
}

static int round_fb_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_BIGNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	Return(float_round_bignum_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int round_fr_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_RATIO);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	Return(float_round_br_ratio_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int round_fs_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = single_float_fixnum(left);
	b = RefSingleFloat(right);
	Return(float_round_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int round_fd_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = double_float_fixnum(left);
	b = RefDoubleFloat(right);
	Return(float_round_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int round_fl_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = long_float_fixnum(left);
	b = RefLongFloat(right);
	Return(float_round_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int round_fixnum_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return round_ff_common_(quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return round_fb_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return round_fr_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return round_fs_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return round_fd_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return round_fl_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int round_bf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	Return(float_round_bignum_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int round_bs_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	Return(single_float_bignum_(left, &a));
	b = RefSingleFloat(right);
	Return(float_round_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int round_bd_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	Return(double_float_bignum_(left, &a));
	b = RefDoubleFloat(right);
	Return(float_round_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int round_bl_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	Return(long_float_bignum_(left, &a));
	b = RefLongFloat(right);
	Return(float_round_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int round_bignum_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return round_bf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return float_round_bignum_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return float_round_br_ratio_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return round_bs_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return round_bd_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return round_bl_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int round_rf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	Return(float_round_rb_ratio_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int round_rs_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	Return(single_float_ratio_(left, &a));
	b = RefSingleFloat(right);
	Return(float_round_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int round_rd_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	Return(double_float_ratio_(left, &a));
	b = RefDoubleFloat(right);
	Return(float_round_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int round_rl_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	Return(long_float_ratio_(left, &a));
	b = RefLongFloat(right);
	Return(float_round_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int round_ratio_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return round_rf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return float_round_rb_ratio_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return float_round_rr_ratio_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return round_rs_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return round_rd_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return round_rl_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int round_sf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefSingleFloat(left);
	b = single_float_fixnum(right);
	Return(float_round_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int round_sb_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefSingleFloat(left);
	Return(single_float_bignum_(right, &b));
	Return(float_round_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int round_sr_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefSingleFloat(left);
	Return(single_float_ratio_(right, &b));
	Return(float_round_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int round_ss_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefSingleFloat(left);
	b = RefSingleFloat(right);
	Return(float_round_s_(a, b, &a, &b));
	single_float_integer_heap(local, quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int round_sd_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = (double_float)RefSingleFloat(left);
	b = RefDoubleFloat(right);
	Return(float_round_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int round_sl_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefSingleFloat(left);
	b = RefLongFloat(right);
	Return(float_round_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int round_single_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return round_sf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return round_sb_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return round_sr_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return round_ss_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return round_sd_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return round_sl_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int round_df_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefDoubleFloat(left);
	b = double_float_fixnum(right);
	Return(float_round_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int round_db_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefDoubleFloat(left);
	Return(double_float_bignum_(right, &b));
	Return(float_round_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int round_dr_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefDoubleFloat(left);
	Return(double_float_ratio_(right, &b));
	Return(float_round_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int round_ds_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefDoubleFloat(left);
	b = (double_float)RefSingleFloat(right);
	Return(float_round_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int round_dd_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefDoubleFloat(left);
	b = RefDoubleFloat(right);
	Return(float_round_d_(a, b, &a, &b));
	double_float_integer_heap(local, quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int round_dl_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefDoubleFloat(left);
	b = RefLongFloat(right);
	Return(float_round_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int round_double_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return round_df_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return round_db_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return round_dr_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return round_ds_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return round_dd_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return round_dl_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int round_lf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefLongFloat(left);
	b = long_float_fixnum(right);
	Return(float_round_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int round_lb_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefLongFloat(left);
	Return(long_float_bignum_(right, &b));
	Return(float_round_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int round_lr_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefLongFloat(left);
	Return(long_float_ratio_(right, &b));
	Return(float_round_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int round_ls_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefSingleFloat(right);
	Return(float_round_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int round_ld_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefDoubleFloat(right);
	Return(float_round_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int round_ll_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = RefLongFloat(left);
	b = RefLongFloat(right);
	Return(float_round_l_(a, b, &a, &b));
	long_float_integer_heap(local, quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int round_long_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return round_lf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return round_lb_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return round_lr_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return round_ls_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return round_ld_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return round_ll_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

_g int round2_common_(LocalRoot local, addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return round_fixnum_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return round_bignum_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return round_ratio_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return round_single_common_(local, quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return round_double_common_(local, quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return round_long_common_(local, quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(left, REAL);
	}
}

static int fround_ff_common_(addr *quot, addr *rem, addr left, addr right)
{
	fixnum a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	GetFixnum(left, &a);
	GetFixnum(right, &b);
	return float_fround_fixnum_(quot, rem, a, b);
}

static int fround_fb_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_BIGNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	Return(float_fround_bignum_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int fround_fr_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_RATIO);
	push_local(local, &stack);
	bignum_fixnum_local(local, &left, left);
	Return(float_fround_br_ratio_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int fround_fs_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = single_float_fixnum(left);
	b = RefSingleFloat(right);
	Return(float_round_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int fround_fd_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = double_float_fixnum(left);
	b = RefDoubleFloat(right);
	Return(float_round_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int fround_fl_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_FIXNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = long_float_fixnum(left);
	b = RefLongFloat(right);
	Return(float_round_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fround_fixnum_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return fround_ff_common_(quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return fround_fb_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return fround_fr_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return fround_fs_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return fround_fd_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return fround_fl_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int fround_bf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	Return(float_fround_bignum_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int fround_bs_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	Return(single_float_bignum_(left, &a));
	b = RefSingleFloat(right);
	Return(float_round_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int fround_bd_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	Return(double_float_bignum_(left, &a));
	b = RefDoubleFloat(right);
	Return(float_round_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int fround_bl_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_BIGNUM);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	Return(long_float_bignum_(left, &a));
	b = RefLongFloat(right);
	Return(float_round_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fround_bignum_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return fround_bf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return float_fround_bignum_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return float_fround_br_ratio_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return fround_bs_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return fround_bd_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return fround_bl_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int fround_rf_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	LocalStack stack;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_FIXNUM);
	push_local(local, &stack);
	bignum_fixnum_local(local, &right, right);
	Return(float_fround_rb_ratio_(local, quot, rem, left, right));
	rollback_local(local, stack);

	return 0;
}

static int fround_rs_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	Return(single_float_ratio_(left, &a));
	b = RefSingleFloat(right);
	Return(float_round_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int fround_rd_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	Return(double_float_ratio_(left, &a));
	b = RefDoubleFloat(right);
	Return(float_round_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int fround_rl_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_RATIO);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	Return(long_float_ratio_(left, &a));
	b = RefLongFloat(right);
	Return(float_round_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fround_ratio_common_(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return fround_rf_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return float_fround_rb_ratio_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return float_fround_rr_ratio_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return fround_rs_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return fround_rd_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return fround_rl_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int fround_sf_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefSingleFloat(left);
	b = single_float_fixnum(right);
	Return(float_round_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int fround_sb_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefSingleFloat(left);
	Return(single_float_bignum_(right, &b));
	Return(float_round_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int fround_sr_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefSingleFloat(left);
	Return(single_float_ratio_(right, &b));
	Return(float_round_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int fround_ss_common_(addr *quot, addr *rem, addr left, addr right)
{
	single_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefSingleFloat(left);
	b = RefSingleFloat(right);
	Return(float_round_s_(a, b, &a, &b));
	single_float_heap(quot, a);
	single_float_heap(rem, b);

	return 0;
}

static int fround_sd_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = (double_float)RefSingleFloat(left);
	b = RefDoubleFloat(right);
	Return(float_round_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int fround_sl_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefSingleFloat(left);
	b = RefLongFloat(right);
	Return(float_round_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fround_single_common_(addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return fround_sf_common_(quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return fround_sb_common_(quot, rem, left, right);

		case LISPTYPE_RATIO:
			return fround_sr_common_(quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return fround_ss_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return fround_sd_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return fround_sl_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int fround_df_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefDoubleFloat(left);
	b = double_float_fixnum(right);
	Return(float_round_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int fround_db_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefDoubleFloat(left);
	Return(double_float_bignum_(right, &b));
	Return(float_round_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int fround_dr_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefDoubleFloat(left);
	Return(double_float_ratio_(right, &b));
	Return(float_round_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int fround_ds_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefDoubleFloat(left);
	b = (double_float)RefSingleFloat(right);
	Return(float_round_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int fround_dd_common_(addr *quot, addr *rem, addr left, addr right)
{
	double_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefDoubleFloat(left);
	b = RefDoubleFloat(right);
	Return(float_round_d_(a, b, &a, &b));
	double_float_heap(quot, a);
	double_float_heap(rem, b);

	return 0;
}

static int fround_dl_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = (long_float)RefDoubleFloat(left);
	b = RefLongFloat(right);
	Return(float_round_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fround_double_common_(addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return fround_df_common_(quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return fround_db_common_(quot, rem, left, right);

		case LISPTYPE_RATIO:
			return fround_dr_common_(quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return fround_ds_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return fround_dd_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return fround_dl_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

static int fround_lf_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_FIXNUM);
	a = RefLongFloat(left);
	b = long_float_fixnum(right);
	Return(float_round_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fround_lb_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_BIGNUM);
	a = RefLongFloat(left);
	Return(long_float_bignum_(right, &b));
	Return(float_round_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fround_lr_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_RATIO);
	a = RefLongFloat(left);
	Return(long_float_ratio_(right, &b));
	Return(float_round_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fround_ls_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefSingleFloat(right);
	Return(float_round_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fround_ld_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	a = RefLongFloat(left);
	b = (long_float)RefDoubleFloat(right);
	Return(float_round_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fround_ll_common_(addr *quot, addr *rem, addr left, addr right)
{
	long_float a, b;

	CheckType(left, LISPTYPE_LONG_FLOAT);
	CheckType(right, LISPTYPE_LONG_FLOAT);
	a = RefLongFloat(left);
	b = RefLongFloat(right);
	Return(float_round_l_(a, b, &a, &b));
	long_float_heap(quot, a);
	long_float_heap(rem, b);

	return 0;
}

static int fround_long_common_(addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return fround_lf_common_(quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return fround_lb_common_(quot, rem, left, right);

		case LISPTYPE_RATIO:
			return fround_lr_common_(quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return fround_ls_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return fround_ld_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return fround_ll_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(right, REAL);
	}
}

_g int fround2_common_(LocalRoot local, addr *quot, addr *rem, addr left, addr right)
{
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return fround_fixnum_common_(local, quot, rem, left, right);

		case LISPTYPE_BIGNUM:
			return fround_bignum_common_(local, quot, rem, left, right);

		case LISPTYPE_RATIO:
			return fround_ratio_common_(local, quot, rem, left, right);

		case LISPTYPE_SHORT_FLOAT:
		case LISPTYPE_SINGLE_FLOAT:
			return fround_single_common_(quot, rem, left, right);

		case LISPTYPE_DOUBLE_FLOAT:
			return fround_double_common_(quot, rem, left, right);

		case LISPTYPE_LONG_FLOAT:
			return fround_long_common_(quot, rem, left, right);

		default:
			*quot = *rem = Nil;
			return TypeError_(left, REAL);
			break;
	}
}

_g int round_common_(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2)
{
	if (div == Unbound)
		return round1_common_(local, ret1, ret2, var);
	else
		return round2_common_(local, ret1, ret2, var, div);
}

_g int fround_common_(LocalRoot local, addr var, addr div, addr *ret1, addr *ret2)
{
	if (div == Unbound)
		return fround1_common_(local, ret1, ret2, var);
	else
		return fround2_common_(local, ret1, ret2, var, div);
}

