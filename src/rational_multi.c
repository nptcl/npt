#include "bignum_multi.h"
#include "condition.h"
#include "float_multi.h"
#include "ratio_multi.h"
#include "rational_multi.h"
#include "typedef.h"

/*
 *  multi
 */
_g int multi_fixnum_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			multi_ff_real_common(left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			multi_fb_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			multi_fr_real_common(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

_g int multi_bignum_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			multi_bf_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			multi_bb_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			multi_br_real_common(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

_g int multi_ratio_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			multi_rf_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			multi_rb_real_common(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			multi_rr_real_common(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

_g int multi_single_rational_common_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return multi_float_sf_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return multi_float_sb_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return multi_float_sr_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

_g int multi_double_rational_common_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return multi_float_df_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return multi_float_db_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return multi_float_dr_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

_g int multi_long_rational_common_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_LONG_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return multi_float_lf_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return multi_float_lb_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return multi_float_lr_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

_g int multi_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocal(local);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return multi_fixnum_rational_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return multi_bignum_rational_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return multi_ratio_rational_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}

_g int multi_fixnum_rational_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			multi_ff_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			multi_fb_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			multi_fr_real_local(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

_g int multi_bignum_rational_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			multi_bf_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			multi_bb_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			multi_br_real_local(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

_g int multi_ratio_rational_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			multi_rf_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			multi_rb_real_local(local, left, right, ret);
			return 0;

		case LISPTYPE_RATIO:
			multi_rr_real_local(local, left, right, ret);
			return 0;

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

_g int multi_rational_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocal(local);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return multi_fixnum_rational_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return multi_bignum_rational_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return multi_ratio_rational_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}


/*
 *  div
 */
_g int div_fixnum_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_ff_real_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_fb_real_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_fr_real_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

_g int div_rational_fixnum_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, right, LISPTYPE_FIXNUM);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_ff_real_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_bf_real_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_rf_real_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}

_g int div_bignum_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_bf_real_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_bb_real_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_br_real_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

_g int div_rational_bignum_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, right, LISPTYPE_BIGNUM);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_fb_real_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_bb_real_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_rb_real_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}

_g int div_ratio_rational_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_rf_real_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_rb_real_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_rr_real_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

_g int div_rational_ratio_common_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocalType(local, right, LISPTYPE_RATIO);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_fr_real_common_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_br_real_common_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_rr_real_common_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}

_g int div_single_rational_common_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_float_sf_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_float_sb_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return div_float_sr_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

_g int div_rational_single_common_(addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_SINGLE_FLOAT);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_float_fs_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_float_bs_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return div_float_rs_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}

_g int div_double_rational_common_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_float_df_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_float_db_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return div_float_dr_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

_g int div_rational_double_common_(addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_DOUBLE_FLOAT);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_float_fd_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_float_bd_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return div_float_rd_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}

_g int div_long_rational_common_(addr left, addr right, addr *ret)
{
	CheckType(left, LISPTYPE_LONG_FLOAT);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_float_lf_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_float_lb_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return div_float_lr_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

_g int div_rational_long_common_(addr left, addr right, addr *ret)
{
	CheckType(right, LISPTYPE_LONG_FLOAT);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_float_fl_heap_(left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_float_bl_heap_(left, right, ret);

		case LISPTYPE_RATIO:
			return div_float_rl_heap_(left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}

static int div_fixnum_rational_local_(LocalRoot local,
		addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_FIXNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_ff_real_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_fb_real_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_fr_real_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

static int div_bignum_rational_local_(LocalRoot local,
		addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_BIGNUM);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_bf_real_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_bb_real_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_br_real_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

static int div_ratio_rational_local_(LocalRoot local,
		addr left, addr right, addr *ret)
{
	CheckLocalType(local, left, LISPTYPE_RATIO);
	switch (GetType(right)) {
		case LISPTYPE_FIXNUM:
			return div_rf_real_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_rb_real_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_rr_real_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(right, RATIONAL);
	}
}

_g int div_rational_local_(LocalRoot local, addr left, addr right, addr *ret)
{
	CheckLocal(local);
	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return div_fixnum_rational_local_(local, left, right, ret);

		case LISPTYPE_BIGNUM:
			return div_bignum_rational_local_(local, left, right, ret);

		case LISPTYPE_RATIO:
			return div_ratio_rational_local_(local, left, right, ret);

		default:
			*ret = Nil;
			return TypeError_(left, RATIONAL);
	}
}


/*
 *  inverse
 */
_g int inverse_rational_common_(LocalRoot local, addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return inverse_fixnum_common_(pos, ret);

		case LISPTYPE_BIGNUM:
			return inverse_bignum_common_(pos, ret);

		case LISPTYPE_RATIO:
			return inverse_ratio_common_(local, pos, ret);

		default:
			*ret = Nil;
			return TypeError_(pos, RATIONAL);
	}
}

