#include "bignum_plus.h"
#include "condition.h"
#include "float_plus.h"
#include "ratio_plus.h"
#include "typedef.h"

/*
 *  sign-reverse
 */
_g int sign_reverse_rational_common_(addr pos, addr *ret)
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

		default:
			*ret = Nil;
			return TypeError_(pos, RATIONAL);
	}

	return 0;
}

_g int sign_reverse_rational_local_(LocalRoot local, addr pos, addr *ret)
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

		default:
			*ret = Nil;
			return TypeError_(pos, RATIONAL);
	}

	return 0;
}


/*
 *  oneplus
 */
_g int oneplus_rational_common_(LocalRoot local, addr value, addr *ret)
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

		default:
			*ret = Nil;
			return TypeError_(value, RATIONAL);
	}
}

_g int oneminus_rational_common_(LocalRoot local, addr value, addr *ret)
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

		default:
			*ret = 0;
			return TypeError_(value, RATIONAL);
	}
}


/*
 *  plus
 */
_g int plus_fixnum_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
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

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

_g int plus_bignum_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
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

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

_g int plus_ratio_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_RATIO);
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

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

_g int plus_single_rational_common_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return plus_float_sf_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return plus_float_sb_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return plus_float_sr_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

_g int plus_double_rational_common_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return plus_float_df_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return plus_float_db_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return plus_float_dr_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

_g int plus_long_rational_common_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_LONG_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return plus_float_lf_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return plus_float_lb_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return plus_float_lr_heap_(left, right, ret);

		default:
			return TypeError_(right, RATIONAL);
	}
}

_g int plus_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocal(local);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return plus_fixnum_rational_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return plus_bignum_rational_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return plus_ratio_rational_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}

static int plus_fixnum_rational_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
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

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

static int plus_bignum_rational_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
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

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

static int plus_ratio_rational_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_RATIO);
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

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

_g int plus_rational_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocal(local);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return plus_fixnum_rational_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return plus_bignum_rational_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return plus_ratio_rational_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}

/*
 *  minus
 */
_g int minus_fixnum_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
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

		default:
			*ret = 0;
			return TypeError_(right, RATIONAL);
	}
}

_g int minus_rational_fixnum_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, right, LISPTYPE_FIXNUM);
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

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}

_g int minus_bignum_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
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

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

_g int minus_rational_bignum_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, right, LISPTYPE_BIGNUM);
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

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}

_g int minus_ratio_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			minus_rf_real_common(local, left, right, ret);
			return 0;;

		case LISPTYPE_BIGNUM:
			minus_rb_real_common(local, left, right, ret);
			return 0;;

		case LISPTYPE_RATIO:
			minus_rr_real_common(local, left, right, ret);
			return 0;;

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

_g int minus_rational_ratio_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, right, LISPTYPE_RATIO);
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

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}

_g int minus_single_rational_common_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return minus_float_sf_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_float_sb_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return minus_float_sr_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

_g int minus_rational_single_common_(addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return minus_float_fs_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_float_bs_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return minus_float_rs_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}

_g int minus_double_rational_common_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return minus_float_df_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_float_db_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return minus_float_dr_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

_g int minus_rational_double_common_(addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return minus_float_fd_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_float_bd_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return minus_float_rd_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}

_g int minus_long_rational_common_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_LONG_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return minus_float_lf_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_float_lb_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return minus_float_lr_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

_g int minus_rational_long_common_(addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_LONG_FLOAT);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return minus_float_fl_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_float_bl_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return minus_float_rl_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}

static int minus_fixnum_rational_local_(LocalRoot local,
		addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
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

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

static int minus_bignum_rational_local_(LocalRoot local,
		addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
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

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

static int minus_ratio_rational_local_(LocalRoot local,
		addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_RATIO);
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

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

_g int minus_rational_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocal(local);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return minus_fixnum_rational_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_bignum_rational_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return minus_ratio_rational_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}

_g int minus_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocal(local);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return minus_fixnum_rational_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return minus_bignum_rational_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return minus_ratio_rational_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}

